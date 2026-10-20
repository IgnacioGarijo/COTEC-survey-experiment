# Original script-49 bootstrap criterion, audited automatic initialization.
# Omitting post-fit derivatives does not alter the optimization or estimator.
reconstruction_library_mode<-TRUE;source('2. Code/paper_A_reconstruction/reconstruct.R',local=TRUE)
lock<-read_json(file.path(out,'diagnostics','numerical_source_manifest.json'),simplifyVector=TRUE)
stopifnot(length(lock$md5)>0)
for(nm in names(lock$md5))stopifnot(unname(tools::md5sum(file.path('2. Code/paper_A_reconstruction',nm)))==lock$md5[[nm]])
ins<-read_json(file.path(out,'diagnostics','inputs.json'),simplifyVector=TRUE)
stopifnot(ins$raw_md5==unname(tools::md5sum('1. Data/raw data/df_named.parquet')),ins$clean_md5==unname(tools::md5sum('1. Data/processed data/cleandata.parquet')))
aa<-commandArgs(TRUE);s<-aa[1];workers<-if(length(aa)>1)as.integer(aa[2])else 4L;B<-199L
stopifnot(s%in%c('B','C','D'))
o<-geto(s);pt<-readRDS(file.path(local,paste0('point_',s,'.rds')))
stopifnot(pt$fingerprint==unname(tools::md5sum(object_path(s))),pt$status%in%c('accepted','accepted_boundary'))
scenario_dir<-file.path(out,s);path<-file.path(scenario_dir,'decomposition_bootstrap_draws.csv')
fp<-list(method='clone-LOO-six-correlated-slopes-v3-original-numerical-criterion',scenario_md5=pt$fingerprint,formula=paste(deparse(form),collapse=''),seed='1004000 + draw',R=R.version.string,lme4=as.character(packageVersion('lme4')),initialization='automatic',optimizer='nloptwrap',maxeval=30000,calc_derivs=FALSE,acceptance='optimizer0 and finite allocations; singular fits retained; same criterion applied to retained v1/v2')
fppath<-file.path(scenario_dir,'bootstrap_v3_fingerprint.json')
if(file.exists(fppath))stopifnot(isTRUE(all.equal(read_json(fppath,simplifyVector=TRUE),fp,check.attributes=FALSE)))else write_json(fp,fppath,pretty=TRUE,auto_unbox=TRUE)
read_draws<-function(p){z<-read.csv(p);for(v in intersect(names(z),c('error','warnings','configuration','optimizer_used','attempt_logLik')))z[[v]]<-as.character(z[[v]]);z}
prior<-if(file.exists(path))read_draws(path)else data.frame(draw=integer())
for(version in c('v2','v3')){
 files<-list.files(file.path(local,paste0('bootstrap_',version,'_',s)),pattern='^draw_[0-9]+[.]csv$',full.names=TRUE)
 if(length(files)){extra<-bind_rows(lapply(files,read_draws));stopifnot(nrow(extra)==length(files),!anyDuplicated(extra$draw),all(extra$draw%in%seq_len(B)),all(startsWith(extra$configuration,paste0(version,'-'))));prior<-bind_rows(prior,extra[!extra$draw%in%prior$draw,])}
}
stopifnot(!anyDuplicated(prior$draw),all(prior$draw%in%seq_len(B)))
if(s=='B'&&nrow(prior))stopifnot(!any(startsWith(prior$configuration,'v1-')))
components<-names(allocate(pt$model,prepare_model(o$long,o$t)))
if(nrow(prior)){
 if(!'strict_v2_admissible'%in%names(prior))prior$strict_v2_admissible<-ifelse(startsWith(prior$configuration,'v2-'),prior$accepted,NA)
 prior$accepted<-prior$optimizer==0&apply(prior[,components,drop=FALSE],1,function(x)all(is.finite(x)))
 prior$accepted[is.na(prior$accepted)]<-FALSE
}
fragmentdir<-file.path(local,paste0('bootstrap_v3_',s));dir.create(fragmentdir,showWarnings=FALSE)
t<-o$t;l<-o$long[,c('id','card','y',attrs)];idx<-match(l$id,t$id);ci<-as.integer(factor(l$card));ii<-split(seq_len(nrow(l)),idx)
one<-function(b){
 started<-Sys.time();set.seed(1004000+b);draw<-sample.int(nrow(t),replace=TRUE)
 ans<-tryCatch({
  w<-tabulate(draw,nbins=nrow(t));lw<-w[idx];sm<-rowsum(l$y*lw,ci);ct<-rowsum(lw,ci);sm<-setNames(as.numeric(sm),rownames(sm));ct<-setNames(as.numeric(ct),rownames(ct));stopifnot(all(ct[as.character(ci[lw>0])]>1))
  h<-tapply(l$y-(sm[as.character(ci)]-l$y)/(ct[as.character(ci)]-1),idx,mean);tb<-t[draw,];tb$id<-as.character(seq_along(draw));tb$hb<-as.numeric(h[as.character(draw)]);lb<-l[unlist(ii[as.character(draw)],use.names=FALSE),];lb$id<-rep(tb$id,lengths(ii[as.character(draw)]));stopifnot(all(is.finite(tb$hb)),abs(sum(tb$hb*tb$n_cards))<1e-8)
  z<-prepare_model(lb,tb);warn<-character()
  m<-withCallingHandlers(lmer(form,z,REML=TRUE,control=lmerControl(optimizer='nloptwrap',calc.derivs=FALSE,optCtrl=list(maxeval=30000))),warning=function(w){warn<<-c(warn,conditionMessage(w));invokeRestart('muffleWarning')})
  p<-allocate(m,z);op<-as.numeric(m@optinfo$conv$opt);ll<-as.numeric(logLik(m))
  data.frame(draw=b,as.list(p),optimizer=op,singular=as.numeric(isSingular(m)),min_covariance=min(eigen(as.matrix(VarCorr(m)$id),symmetric=TRUE)$values),accepted=op==0&&all(is.finite(p)),seconds=as.numeric(difftime(Sys.time(),started,units='secs')),error='',warnings=paste(unique(warn),collapse=' | '),configuration='v3-automatic-original-bootstrap-criterion',logLik=ll,derivatives_checked=FALSE,gradient=NA_real_,min_hessian=NA_real_,optimizer_used='nloptwrap',attempts=1,attempt_logLik=as.character(ll),strict_v2_admissible=NA)
 },error=function(e)data.frame(draw=b,accepted=FALSE,error=conditionMessage(e),configuration='v3-automatic-original-bootstrap-criterion'))
 write.csv(ans,file.path(fragmentdir,paste0('draw_',b,'.csv')),row.names=FALSE,na='');ans
}
todo<-setdiff(seq_len(B),prior$draw);cl<-parallel::makeCluster(workers,rscript_args='--vanilla');invisible(parallel::clusterCall(cl,function(paths).libPaths(paths),.libPaths()))
ev<-parallel::clusterEvalQ(cl,{suppressPackageStartupMessages({library(dplyr);library(lme4)});list(R=R.version.string,lme4=as.character(packageVersion('lme4')))})
stopifnot(all(vapply(ev,function(e)e$lme4==as.character(packageVersion('lme4')),logical(1))));js(ev,'worker_environment_v3',s)
parallel::clusterExport(cl,c('t','l','idx','ci','ii','attrs','x1','x3','blocks','form','prepare_model','allocate','one','fragmentdir'),envir=environment())
if(length(todo)){ans<-parallel::parLapplyLB(cl,todo,one);prior<-bind_rows(prior,bind_rows(ans))}
parallel::stopCluster(cl);prior<-prior[order(prior$draw),];stopifnot(!anyDuplicated(prior$draw));write.csv(prior,path,row.names=FALSE,na='')
good<-prior$accepted%in%TRUE;pp<-read.csv(file.path(scenario_dir,'decomposition_point.csv'));np<-pp$component
if(sum(good)>=190){pp$se<-sapply(prior[good,np],stats::sd);pp$lo_percentile<-sapply(prior[good,np],quantile,.025);pp$hi_percentile<-sapply(prior[good,np],quantile,.975);pp$successful<-sum(good);pp$requested<-B;csv(pp,'decomposition',s)}
js(list(scenario=s,method=fp$method,seed='1004000 + draw',requested=B,attempted=nrow(prior),accepted=sum(good),failed=sum(!good),boundary_accepted=sum(prior$singular[good]==1,na.rm=TRUE),configuration_counts=as.list(table(prior$configuration)),acceptance=fp$acceptance,full_derivatives_rows=sum(prior$derivatives_checked%in%TRUE),interpretation='Descriptive estimator spread; not nominal boundary coverage; full Hessian checked at points and in labeled v2 rows, not every draw',point_status=pt$status,workers=workers,numerical_audit='start audit, optimizer controls and derivative_flag_audit; B-v1 excluded'),'bootstrap_status',s)
message('Finalized ',s,': ',sum(good),' accepted of ',nrow(prior),' attempted')
