# Predeclared numerical audit: draws 1, 50, 199, default versus point-covariance start.
# Same model/objective and optimizer/tolerances, no selection by allocation.
reconstruction_library_mode<-TRUE;source('2. Code/paper_A_reconstruction/reconstruct.R',local=TRUE)
ss<-if(length(commandArgs(TRUE)))strsplit(commandArgs(TRUE)[1],',',fixed=TRUE)[[1]]else c('A','B','C','D')
ans<-list()
for(s in ss){o<-geto(s);t<-o$t;l<-o$long[,c('id','card','y',attrs)];idx<-match(l$id,t$id);ci<-as.integer(factor(l$card));ii<-split(seq_len(nrow(l)),idx);theta<-getME(readRDS(file.path(local,paste0('point_',s,'.rds')))$model,'theta')
 for(b in c(1L,50L,199L)){
  set.seed(1004000+b);draw<-sample.int(nrow(t),replace=TRUE);w<-tabulate(draw,nbins=nrow(t));lw<-w[idx];sm<-rowsum(l$y*lw,ci);ct<-rowsum(lw,ci);sm<-setNames(as.numeric(sm),rownames(sm));ct<-setNames(as.numeric(ct),rownames(ct));h<-tapply(l$y-(sm[as.character(ci)]-l$y)/(ct[as.character(ci)]-1),idx,mean)
  tb<-t[draw,];tb$id<-as.character(seq_along(draw));tb$hb<-as.numeric(h[as.character(draw)]);lb<-l[unlist(ii[as.character(draw)],use.names=FALSE),];lb$id<-rep(tb$id,lengths(ii[as.character(draw)]));z<-prepare_model(lb,tb)
  fits<-lapply(c('point','automatic'),function(st)suppressWarnings(lmer(form,z,REML=TRUE,start=if(st=='point')theta else NULL,control=lmerControl(optimizer='nloptwrap',calc.derivs=FALSE,optCtrl=list(maxeval=30000,xtol_abs=1e-8,ftol_abs=1e-9)))))
  p1<-allocate(fits[[1]],z);p2<-allocate(fits[[2]],z);ans[[length(ans)+1]]<-data.frame(scenario=s,draw=b,logLik_point=as.numeric(logLik(fits[[1]])),logLik_automatic=as.numeric(logLik(fits[[2]])),logLik_delta=as.numeric(logLik(fits[[2]])-logLik(fits[[1]])),max_share_delta=max(abs(p2-p1)),singular_point=isSingular(fits[[1]]),singular_automatic=isSingular(fits[[2]]),optimizer_point=as.numeric(fits[[1]]@optinfo$conv$opt),optimizer_automatic=as.numeric(fits[[2]]@optinfo$conv$opt))
  csv(bind_rows(ans),'bootstrap_start_audit');message('Start audit ',s,' draw ',b)
 }
}
