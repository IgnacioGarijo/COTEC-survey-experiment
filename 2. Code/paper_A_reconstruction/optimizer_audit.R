# Numerical controls audit on predeclared seeds; no estimator/specification change.
reconstruction_library_mode<-TRUE;source('2. Code/paper_A_reconstruction/reconstruct.R',local=TRUE)
ans<-list()
for(s in c('B','C','D')){
 o<-geto(s);t<-o$t;l<-o$long[,c('id','card','y',attrs)];idx<-match(l$id,t$id);ci<-as.integer(factor(l$card));ii<-split(seq_len(nrow(l)),idx)
 for(b in c(1L,50L,199L)){
  set.seed(1004000+b);draw<-sample.int(nrow(t),replace=TRUE);w<-tabulate(draw,nbins=nrow(t));lw<-w[idx];sm<-rowsum(l$y*lw,ci);ct<-rowsum(lw,ci);sm<-setNames(as.numeric(sm),rownames(sm));ct<-setNames(as.numeric(ct),rownames(ct));h<-tapply(l$y-(sm[as.character(ci)]-l$y)/(ct[as.character(ci)]-1),idx,mean)
  tb<-t[draw,];tb$id<-as.character(seq_along(draw));tb$hb<-as.numeric(h[as.character(draw)]);lb<-l[unlist(ii[as.character(draw)],use.names=FALSE),];lb$id<-rep(tb$id,lengths(ii[as.character(draw)]));z<-prepare_model(lb,tb)
  started<-Sys.time();warn<-character()
  m<-withCallingHandlers(lmer(form,z,REML=TRUE,control=lmerControl(optimizer='nloptwrap',calc.derivs=TRUE,optCtrl=list(maxeval=30000))),warning=function(w){warn<<-c(warn,conditionMessage(w));invokeRestart('muffleWarning')})
  p<-allocate(m,z);di<-diagnose(m);ref<-read.csv(file.path(out,'tables','bootstrap_start_audit.csv'));ref<-ref[ref$scenario==s&ref$draw==b,]
  ans[[length(ans)+1]]<-data.frame(scenario=s,draw=b,logLik=as.numeric(logLik(m)),delta_vs_tight_automatic=as.numeric(logLik(m))-ref$logLik_automatic,as.list(p),as.list(di),seconds=as.numeric(difftime(Sys.time(),started,units='secs')),warnings=paste(warn,collapse=' | '))
  csv(bind_rows(ans),'optimizer_control_audit');saveRDS(list(model=m,shares=p),file.path(local,paste0('optimizer_audit_',s,'_',b,'.rds')))
  message('Numerical audit ',s,' ',b,' deltaLL ',ans[[length(ans)]]$delta_vs_tight_automatic)
 }
}
