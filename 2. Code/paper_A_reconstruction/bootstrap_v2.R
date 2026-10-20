# Same clustered bootstrap/REML estimator; audited automatic initialization.
# B-v1 is excluded: warm start trapped the optimizer. C/D-v1 are retained as
# numerically equivalent configurations and explicitly identified by draw.
reconstruction_library_mode<-TRUE;source('2. Code/paper_A_reconstruction/reconstruct.R',local=TRUE)
source_lock<-read_json(file.path(out,'diagnostics','numerical_source_manifest.json'),simplifyVector=TRUE)
stopifnot(length(source_lock$md5)>0)
for(nm in names(source_lock$md5))stopifnot(unname(tools::md5sum(file.path('2. Code/paper_A_reconstruction',nm)))==source_lock$md5[[nm]])
input_lock<-read_json(file.path(out,'diagnostics','inputs.json'),simplifyVector=TRUE)
stopifnot(input_lock$raw_md5==unname(tools::md5sum('1. Data/raw data/df_named.parquet')),input_lock$clean_md5==unname(tools::md5sum('1. Data/processed data/cleandata.parquet')))
aa<-commandArgs(TRUE);s<-aa[1];workers<-if(length(aa)>1)as.integer(aa[2])else 4L;B<-199L
stopifnot(s%in%c('B','C','D'))
o<-geto(s);pt<-readRDS(file.path(local,paste0('point_',s,'.rds')))
stopifnot(pt$fingerprint==unname(tools::md5sum(object_path(s))),pt$status%in%c('accepted','accepted_boundary'))
sd<-file.path(out,s);archive<-file.path(sd,'numerical_audit_v1');dir.create(archive,showWarnings=FALSE)
read_draws<-function(p){z<-read.csv(p);for(v in intersect(names(z),c('error','warnings','configuration','optimizer_used','attempt_logLik')))z[[v]]<-as.character(z[[v]]);z}
path<-file.path(sd,'decomposition_bootstrap_draws.csv');statepath<-file.path(sd,'bootstrap_v2_fingerprint.json')
fp<-list(method='clone-LOO-six-correlated-slopes-v2-numerical',scenario_md5=pt$fingerprint,formula=paste(deparse(form),collapse=''),seed='1004000 + draw',R=R.version.string,lme4=as.character(packageVersion('lme4')),initialization='automatic; nloptwrap defaults, full derivatives; bobyqa fallback for numerical diagnostics',retained_v1=s!='B')
if(!file.exists(statepath)){
 for(f in c('decomposition_bootstrap_draws.csv','bootstrap_status.json','decomposition.csv','bootstrap_fingerprint.json'))if(file.exists(file.path(sd,f)))stopifnot(file.copy(file.path(sd,f),file.path(archive,f),overwrite=FALSE))
 prior<-if(s!='B'&&file.exists(path))read.csv(path)else data.frame(draw=integer())
 if(nrow(prior)){prior$configuration<-'v1-point-start-tight';prior$logLik<-NA_real_;prior$derivatives_checked<-FALSE}
 write.csv(prior,path,row.names=FALSE,na='');write_json(fp,statepath,pretty=TRUE,auto_unbox=TRUE)
 if(file.exists(file.path(sd,'decomposition.csv')))unlink(file.path(sd,'decomposition.csv'))
 if(file.exists(file.path(sd,'bootstrap_status.json')))unlink(file.path(sd,'bootstrap_status.json'))
}else{stopifnot(isTRUE(all.equal(read_json(statepath,simplifyVector=TRUE),fp,check.attributes=FALSE)));prior<-read_draws(path)}
fragmentdir<-file.path(local,paste0('bootstrap_v2_',s));dir.create(fragmentdir,showWarnings=FALSE)
fragments<-list.files(fragmentdir,pattern='^draw_[0-9]+[.]csv$',full.names=TRUE)
if(length(fragments)){extra<-bind_rows(lapply(fragments,read_draws));stopifnot(nrow(extra)==length(fragments),!anyDuplicated(extra$draw),all(extra$draw%in%seq_len(B)),all(extra$configuration=='v2-automatic-full-derivatives'));prior<-bind_rows(prior,extra[!extra$draw%in%prior$draw,])}
stopifnot(!anyDuplicated(prior$draw),all(prior$draw%in%seq_len(B)))
t<-o$t;l<-o$long[,c('id','card','y',attrs)];idx<-match(l$id,t$id);ci<-as.integer(factor(l$card));ii<-split(seq_len(nrow(l)),idx)
one<-function(b){
 started<-Sys.time();set.seed(1004000+b);draw<-sample.int(nrow(t),replace=TRUE)
 ans<-tryCatch({
  w<-tabulate(draw,nbins=nrow(t));lw<-w[idx];sm<-rowsum(l$y*lw,ci);ct<-rowsum(lw,ci);sm<-setNames(as.numeric(sm),rownames(sm));ct<-setNames(as.numeric(ct),rownames(ct));stopifnot(all(ct[as.character(ci[lw>0])]>1))
  h<-tapply(l$y-(sm[as.character(ci)]-l$y)/(ct[as.character(ci)]-1),idx,mean);tb<-t[draw,];tb$id<-as.character(seq_along(draw));tb$hb<-as.numeric(h[as.character(draw)]);lb<-l[unlist(ii[as.character(draw)],use.names=FALSE),];lb$id<-rep(tb$id,lengths(ii[as.character(draw)]));stopifnot(all(is.finite(tb$hb)),abs(sum(tb$hb*tb$n_cards))<1e-8)
  z<-prepare_model(lb,tb);warn<-character();attempts<-list()
  admissible<-function(m){d<-diagnose(m);all(is.finite(d))&&d['optimizer']==0&&d['gradient']<.002&&d['min_hessian']>0}
  fit<-function(opt){withCallingHandlers(lmer(form,z,REML=TRUE,control=lmerControl(optimizer=opt,calc.derivs=TRUE,optCtrl=if(opt=='nloptwrap')list(maxeval=30000)else list(maxfun=100000))),warning=function(w){warn<<-c(warn,conditionMessage(w));invokeRestart('muffleWarning')})}
  m<-fit('nloptwrap');chosen<-'nloptwrap';attempts[[1]]<-c(logLik=as.numeric(logLik(m)),diagnose(m))
  if(!admissible(m)){m2<-fit('bobyqa');attempts[[2]]<-c(logLik=as.numeric(logLik(m2)),diagnose(m2));if(admissible(m2)&&(!admissible(m)||logLik(m2)>logLik(m))&&as.numeric(logLik(m2)-logLik(m))>=-.0005){m<-m2;chosen<-'bobyqa'}}
  p<-allocate(m,z);d<-diagnose(m)
  data.frame(draw=b,as.list(p),optimizer=unname(d['optimizer']),singular=unname(d['singular']),min_covariance=unname(d['min_covariance']),accepted=admissible(m)&&all(is.finite(p)),seconds=as.numeric(difftime(Sys.time(),started,units='secs')),error='',warnings=paste(unique(warn),collapse=' | '),configuration='v2-automatic-full-derivatives',logLik=as.numeric(logLik(m)),derivatives_checked=TRUE,gradient=unname(d['gradient']),min_hessian=unname(d['min_hessian']),optimizer_used=chosen,attempts=length(attempts),attempt_logLik=paste(vapply(attempts,function(x)x['logLik'],numeric(1)),collapse=' | '))
 },error=function(e)data.frame(draw=b,accepted=FALSE,error=conditionMessage(e),configuration='v2-automatic-full-derivatives'))
 write.csv(ans,file.path(fragmentdir,paste0('draw_',b,'.csv')),row.names=FALSE,na='');ans
}
todo<-setdiff(seq_len(B),prior$draw);cl<-parallel::makeCluster(workers,rscript_args='--vanilla');parallel::clusterCall(cl,function(paths).libPaths(paths),.libPaths())
ev<-parallel::clusterEvalQ(cl,{suppressPackageStartupMessages({library(dplyr);library(lme4)});list(R=R.version.string,lme4=as.character(packageVersion('lme4')))})
stopifnot(all(vapply(ev,function(e)e$lme4==as.character(packageVersion('lme4')),logical(1))));js(ev,'worker_environment_v2',s)
parallel::clusterExport(cl,c('t','l','idx','ci','ii','attrs','x1','x3','blocks','form','prepare_model','allocate','diagnose','one','fragmentdir'),envir=environment())
batch<-max(workers,length(todo))
for(first in seq.int(1,max(1,length(todo)),by=batch)){
 if(!length(todo))break;ids<-todo[first:min(first+batch-1,length(todo))];ans<-parallel::parLapplyLB(cl,ids,one);prior<-bind_rows(prior,bind_rows(ans));prior<-prior[order(prior$draw),];stopifnot(!anyDuplicated(prior$draw));write.csv(prior,path,row.names=FALSE,na='');message('Bootstrap v2 ',s,': ',nrow(prior),'/199')
}
parallel::stopCluster(cl);good<-prior$accepted%in%TRUE;pp<-read.csv(file.path(sd,'decomposition_point.csv'));np<-pp$component
if(sum(good)>=190){pp$se<-sapply(prior[good,np],stats::sd);pp$lo_percentile<-sapply(prior[good,np],quantile,.025);pp$hi_percentile<-sapply(prior[good,np],quantile,.975);pp$successful<-sum(good);pp$requested<-B;csv(pp,'decomposition',s)}
js(list(scenario=s,seed='1004000 + draw',requested=B,attempted=nrow(prior),accepted=sum(good),failed=sum(!good),boundary_accepted=sum(prior$singular[good]==1,na.rm=TRUE),configuration_counts=as.list(table(prior$configuration)),acceptance='v1: optimizer0 finite shares; v2: optimizer0, combined gradient<.002, positive Hessian, finite shares',interpretation='Descriptive estimator spread; not nominal boundary coverage',point_status=pt$status,workers=workers,numerical_audit='tables/bootstrap_start_audit.csv and optimizer_control_audit.csv; B-v1 excluded'),'bootstrap_status',s)
