# Covariance candidates fixed before inspecting their allocations. Same fixed effects/data.
options(stringsAsFactors=FALSE);invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
reconstruction_library_mode<-TRUE;source('2. Code/paper_A_reconstruction/reconstruct.R')
out<-'3. Output/paper_A_final_revision';local<-file.path(out,'local')
fixed<-paste(unlist(blocks),collapse='+')
forms<-list(full=as.formula(paste('y~',fixed,'+(0+male_c+background_c+failed_c+competence_c+absent_c+disruptive_c|id)')),
 diagonal=as.formula(paste('y~',fixed,'+(0+male_c+background_c+failed_c+competence_c+absent_c+disruptive_c||id)')),
 domains=as.formula(paste('y~',fixed,'+(0+male_c+background_c|id)+(0+failed_c+competence_c|id)+(0+absent_c+disruptive_c|id)')))
variance_random<-function(m,z){sum(vapply(VarCorr(m),function(S){S<-as.matrix(S);Z<-as.matrix(z[,colnames(S),drop=FALSE]);mean(rowSums((Z%*%S)*Z))},numeric(1)))}
allocate<-function(m,z){
 eta<-as.numeric(predict(m,re.form=NA));ff<-nobars(formula(m));X<-model.matrix(ff,z);labs<-attr(terms(ff),'term.labels');groups<-lapply(blocks,function(a)which(attr(X,'assign')%in%match(a,labs)))
 K<-length(blocks);val<-numeric(2^K);shap<-numeric(K)
 for(mask in 0:(2^K-1)){sel<-which(as.logical(intToBits(mask)[seq_len(K)]));cols<-unique(c(1,unlist(groups[sel])));val[mask+1]<-var(lm.fit(X[,cols,drop=FALSE],eta)$fitted.values)}
 for(j in seq_len(K))for(mask in 0:(2^K-1)){sel<-which(as.logical(intToBits(mask)[seq_len(K)]));if(j%in%sel)next;s<-length(sel);shap[j]<-shap[j]+factorial(s)*factorial(K-s-1)/factorial(K)*(val[bitwOr(mask,bitwShiftL(1,j-1))+1]-val[mask+1])}
 names(shap)<-names(blocks);v<-c(shap,Sensitivities=variance_random(m,z),Residual=sigma(m)^2)
 stopifnot(abs(sum(shap)-var(eta))<1e-8,all(v>=-1e-10));v/sum(v)
}
diagnose<-function(m){g<-m@optinfo$derivs$gradient;H<-m@optinfo$derivs$Hessian;sc<-tryCatch(solve(chol(H),g),error=function(e)rep(Inf,length(g)));ev<-unlist(lapply(VarCorr(m),function(S)eigen(as.matrix(S),symmetric=TRUE)$values));c(singular=as.numeric(isSingular(m)),gradient=max(pmin(abs(sc),abs(g))),min_hessian=min(eigen(H,symmetric=TRUE)$values),optimizer=as.numeric(m@optinfo$conv$opt),min_covariance=min(ev))}
fitone<-function(s,kind,opt='nloptwrap',derivs=TRUE){
 o<-geto(s);z<-prepare_model(o$long,o$t);form<-forms[[kind]];path<-file.path(local,paste0('candidate_',s,'_',kind,'_',opt,'.rds'))
 fp<-unname(tools::md5sum(object_path(s)));if(file.exists(path)){c<-readRDS(path);stopifnot(identical(c$fingerprint,fp));return(c)}
 warn<-character();m<-withCallingHandlers(lmer(form,z,REML=TRUE,control=lmerControl(optimizer=opt,calc.derivs=derivs,optCtrl=if(opt=='nloptwrap')list(maxeval=50000,xtol_abs=1e-9,ftol_abs=1e-10)else list(maxfun=150000,rhoend=1e-8))),warning=function(w){warn<<-c(warn,conditionMessage(w));invokeRestart('muffleWarning')})
 dg<-diagnose(m);p<-allocate(m,z);c<-list(model=m,fingerprint=fp,diagnostics=dg,shares=p,warnings=warn,scenario=s,kind=kind,optimizer=opt);saveRDS(c,path);c
}
args<-commandArgs(TRUE);phase<-if(length(args))args[1]else'candidates'
if(phase=='candidates'){
 ans<-list()
 for(kind in names(forms))for(opt in c('nloptwrap','bobyqa')){
  c<-fitone('B',kind,opt);m<-c$model;dg<-c$diagnostics
  ans[[length(ans)+1]]<-data.frame(scenario='B',model=kind,optimizer=opt,logLik=as.numeric(logLik(m)),AIC=AIC(m),n=nobs(m),theta=length(getME(m,'theta')),as.list(dg),as.list(c$shares),outside01=mean(predict(m)<0|predict(m)>1),hb=unname(fixef(m)['hb']))
  csv(bind_rows(ans),'covariance_candidates');message(kind,' ',opt,': ',paste(names(dg),round(dg,7),collapse='; '))
 }
 quit(save='no')
}
if(phase=='points'){
 kind<-args[2]
 for(s in c('B','C','NoScreen','B_full8','ByID')){
  c<-fitone(s,kind);if(c$diagnostics['gradient']>.002||c$diagnostics['min_hessian']<=0)c<-fitone(s,kind,'bobyqa')
  stopifnot(c$diagnostics['optimizer']==0,c$diagnostics['gradient']<.002,c$diagnostics['min_hessian']>0)
  csv(data.frame(component=names(c$shares),share=as.numeric(c$shares)),'decomposition_point',s)
  m<-c$model;csv(data.frame(term=names(fixef(m)),estimate=fixef(m)),'decomposition_fixed_effects',s)
  js(list(scenario=s,model=kind,n=nobs(m),teachers=nrow(geto(s)$common),fixed_rank=qr(getME(m,'X'))$rank,covariance_parameters=length(getME(m,'theta')),diagnostics=as.list(c$diagnostics),outside01=mean(predict(m)<0|predict(m)>1),hb=unname(fixef(m)['hb']),optimizer=c$optimizer),'decomposition_diagnostics',s)
  saveRDS(c,file.path(local,paste0('selected_',s,'.rds')))
 }
 quit(save='no')
}
if(phase=='bootstrap'){
 s<-args[2];B<-as.integer(args[3]);workers<-as.integer(args[4]);o<-geto(s);point<-readRDS(file.path(local,paste0('selected_',s,'.rds')));form<-forms[[point$kind]]
 t<-o$t;l<-o$long[,c('id','card','y',attrs)];idx<-match(l$id,t$id);cidx<-as.integer(factor(l$card));by_teacher<-split(seq_len(nrow(l)),idx)
 run_one<-function(b){
  set.seed(2410000+b);draw<-sample.int(nrow(t),replace=TRUE);w<-tabulate(draw,nbins=nrow(t));lw<-w[idx];sm<-rowsum(l$y*lw,cidx);ct<-rowsum(lw,cidx);sm<-setNames(as.numeric(sm),rownames(sm));ct<-setNames(as.numeric(ct),rownames(ct));ref<-(sm[as.character(cidx)]-l$y)/(ct[as.character(cidx)]-1);h<-tapply(l$y-ref,idx,mean)
  tb<-t[draw,];tb$id<-as.character(seq_along(draw));tb$hb<-as.numeric(h[as.character(draw)]);rows<-unlist(by_teacher[as.character(draw)],use.names=FALSE);lb<-l[rows,];lb$id<-rep(tb$id,lengths(by_teacher[as.character(draw)]));stopifnot(abs(sum(tb$hb*tb$n_cards))<1e-8)
  z<-prepare_model(lb,tb)
  tryCatch({
   m<-suppressWarnings(lmer(form,z,REML=TRUE,control=lmerControl(optimizer='nloptwrap',calc.derivs=TRUE,optCtrl=list(maxeval=30000,xtol_abs=1e-8,ftol_abs=1e-9))));dg<-diagnose(m);opt<-'nloptwrap'
   if(dg['optimizer']!=0||dg['gradient']>=.002||dg['min_hessian']<=0){m<-suppressWarnings(lmer(form,z,REML=TRUE,control=lmerControl(optimizer='bobyqa',calc.derivs=TRUE,optCtrl=list(maxfun=100000))));dg<-diagnose(m);opt<-'bobyqa'}
   p<-allocate(m,z);data.frame(draw=b,as.list(p),as.list(dg),opt_used=opt,accepted=all(is.finite(p))&&dg['optimizer']==0&&dg['gradient']<.002&&dg['min_hessian']>0,error='')
  },error=function(e)data.frame(draw=b,accepted=FALSE,error=conditionMessage(e)))
 }
 path<-file.path(out,s,'decomposition_bootstrap_draws.csv')
 fingerprint<-list(scenario_md5=point$fingerprint,model=point$kind,method='clone-LOO-all-six-blocks-full-derivatives-recenter-v1',seed='2410000 + draw',lme4=as.character(packageVersion('lme4')))
 fingerprint_path<-file.path(out,s,'bootstrap_fingerprint.json')
 if(file.exists(fingerprint_path))stopifnot(identical(read_json(fingerprint_path,simplifyVector=TRUE),fingerprint))else write_json(fingerprint,fingerprint_path,pretty=TRUE,auto_unbox=TRUE)
 prior<-if(file.exists(path))read.csv(path)else data.frame(draw=integer());stopifnot(!anyDuplicated(prior$draw));todo<-setdiff(seq_len(B),prior$draw)
 cl<-parallel::makeCluster(workers,rscript_args='--vanilla');parallel::clusterCall(cl,function(p).libPaths(p),.libPaths());env<-parallel::clusterEvalQ(cl,{suppressPackageStartupMessages({library(dplyr);library(lme4)});as.character(packageVersion('lme4'))});stopifnot(all(unlist(env)==as.character(packageVersion('lme4'))))
 parallel::clusterExport(cl,c('t','l','idx','cidx','by_teacher','attrs','x3','blocks','form','prepare_model','allocate','variance_random','diagnose','run_one'),envir=environment())
 for(first in seq.int(1,max(1,length(todo)),by=workers*2)){if(!length(todo))break;ids<-todo[first:min(first+workers*2-1,length(todo))];prior<-bind_rows(prior,bind_rows(parallel::parLapply(cl,ids,run_one)));write.csv(prior,path,row.names=FALSE,na='');message('Bootstrap ',s,' ',nrow(prior),'/',B)}
 parallel::stopCluster(cl);prior<-prior[prior$draw<=B,];good<-prior$accepted%in%TRUE
 p<-data.frame(component=names(point$shares),share=as.numeric(point$shares));p$se<-sapply(prior[good,p$component],sd);p$successful<-sum(good);p$requested<-B;csv(p,'decomposition',s)
 js(list(requested=B,accepted=sum(good),singular=sum(prior$singular[good]==1),seed='2410000 + draw',all_derivatives_checked=TRUE,model=point$kind,lme4=as.character(packageVersion('lme4'))),'bootstrap_status',s)
 stopifnot(sum(good)>=.95*B)
}
