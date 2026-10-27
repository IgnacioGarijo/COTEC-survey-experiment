# Adopted reviewed masked-card algorithm; canonical C files remain read-only.
options(stringsAsFactors=FALSE,width=140)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(arrow);library(jsonlite)})
out<-'3. Output/paper_A_author_revision';local<-file.path(out,'local');source_root<-'3. Output/paper_A_final_revision/local'
csv<-function(d,n)write.csv(d,file.path(out,'tables',paste0(n,'.csv')),row.names=FALSE,na='')
js<-function(d,n)write_json(d,file.path(out,'diagnostics',paste0(n,'.json')),pretty=TRUE,auto_unbox=TRUE)
raw_assignment<-as.data.frame(read_parquet('1. Data/raw data/df_named.parquet',col_select=c('ResponseId','Treatment')))
pr<-readRDS(file.path(source_root,'provenance.rds'));design<-unique(pr$ordered[,c('profile')]);if(!is.data.frame(design))design<-data.frame(profile=design)
stopifnot(nrow(design)==56,!anyDuplicated(raw_assignment$ResponseId))
prepare_pred<-function(z,s,extended=FALSE){
 n<-table(z$id);eligible<-if(s=='C')names(n[n>=4])else names(n[n==8]);z<-z[z$id%in%eligible,]
 z<-z[order(z$id,z$position),];ids<-sort(unique(z$id));z$i<-match(z$id,ids);z$p<-match(z$profile,sort(unique(design$profile)))
 z$treatment<-factor(as.character(raw_assignment$Treatment[match(z$id,raw_assignment$ResponseId)]),levels=as.character(1:10))
 z$stage<-factor(ifelse(is.na(z$level),'Unknown',as.character(z$level)))
 z$set_factor<-factor(z$set);stopifnot(!anyNA(z$treatment))
 X<-if(extended)model.matrix(~treatment+set_factor+stage+experiencia+female+edad+public+public_permanent+grupos_docencia+impact+empathy+meritocracia+resources_excess,z)else model.matrix(~treatment+set_factor+stage,z)
 set.seed(261025);fold_teacher<-sample(rep(1:5,length.out=length(ids)));z$fold<-fold_teacher[z$i]
 list(z=z,ids=ids,X=X,n=length(ids),by=split(seq_len(nrow(z)),z$i),profiles=length(unique(design$profile)),extended=extended)
}
split_history<-function(o,seed,ordered=FALSE){
 set.seed(seed);hist<-logical(nrow(o$z))
 for(rows in o$by){k<-floor(length(rows)/2);if(ordered)hist[rows[seq_len(k)]]<-TRUE else hist[sample(rows,k)]<-TRUE}
 hist
}
prob_clip<-function(p)pmin(1-1e-8,pmax(1e-8,p))
wtab<-function(b,w,n){a<-numeric(n);v<-rowsum(w,b,reorder=FALSE);a[as.integer(rownames(v))]<-v[,1];a}
pipeline<-function(o,hist,w=rep(1,o$n),save_predictions=FALSE){
 z<-o$z;lr<-matrix(NA_real_,o$n,4);preds<-list();fit_warnings<-character();rank_drop<-0L
 for(f in 1:5){
  train<-z$fold!=f;test<-!train;rw<-w[z$i];used<-train&rw>0
  np<-wtab(z$p[used],rw[used],o$profiles)
  sy<-wtab(z$p[used],rw[used]*z$y[used],o$profiles)
  # Each teacher sees a profile at most once. Remove all copies of its response.
  denom<-np[z$p]-ifelse(train,rw,0);numer<-sy[z$p]-ifelse(train,rw*z$y,0)
  pref<-(numer+1)/(denom+2);stopifnot(all(pref>0&pref<1))
  off<-qlogis(pref);hh<-tapply((z$y-pref)[hist],z$i[hist],mean)
  H<-as.numeric(hh[as.character(z$i)]);stopifnot(!anyNA(H))
  tr<-train&!hist&rw>0;te<-test&!hist;ntarget<-tabulate(z$i[!hist],nbins=o$n)
  fit<-function(aug){
   X<-o$X
   if(isTRUE(o$extended))for(v in c('experiencia','edad','grupos_docencia','impact','empathy','meritocracia','resources_excess')){
    wv<-rw[tr]/ntarget[z$i[tr]];mu<-weighted.mean(X[tr,v],wv);ss<-sqrt(weighted.mean((X[tr,v]-mu)^2,wv));stopifnot(is.finite(ss),ss>0);X[,v]<-(X[,v]-mu)/ss
   }
   if(aug)X<-cbind(X,H=H)
   m<-withCallingHandlers(glm.fit(X[tr,,drop=FALSE],z$y[tr],weights=rw[tr]/ntarget[z$i[tr]],offset=off[tr],family=binomial(),control=glm.control(maxit=50,epsilon=1e-8)),warning=function(e){fit_warnings<<-c(fit_warnings,conditionMessage(e));invokeRestart('muffleWarning')})
   if(!isTRUE(m$converged)||any(!is.finite(m$coefficients[!is.na(m$coefficients)])))stop('Calibration failed')
   b<-m$coefficients;rank_drop<<-rank_drop+sum(is.na(b));b[is.na(b)]<-0
   prob_clip(plogis(off[te]+as.numeric(X[te,,drop=FALSE]%*%b)))
  }
  p0<-fit(FALSE);p1<-fit(TRUE);y<-z$y[te]
  loss<-cbind(brier_base=(y-p0)^2,brier_history=(y-p1)^2,log_base=-(y*log(p0)+(1-y)*log1p(-p0)),log_history=-(y*log(p1)+(1-y)*log1p(-p1)))
  per<-rowsum(loss,z$i[te]);ct<-table(z$i[te]);lr[as.integer(rownames(per)),]<-per/as.numeric(ct[rownames(per)])
  if(save_predictions)preds[[f]]<-data.frame(i=z$i[te],profile=z$profile[te],position=z$position[te],y=y,p0=p0,p1=p1,H=H[te],fold=f)
 }
 stopifnot(!anyNA(lr));means<-colSums(lr*w)/sum(w)
 names(means)<-c('brier_base','brier_history','log_base','log_history')
 stats<-c(means,brier_gain=means[1]-means[2],log_gain=means[3]-means[4]);names(stats)[5:6]<-c('brier_gain','log_gain')
 list(stats=stats,loss=lr,predictions=if(save_predictions)do.call(rbind,preds)else NULL,rank_drop=rank_drop,
      weight_warnings=sum(grepl('non-integer',fit_warnings,fixed=TRUE)),other_warnings=sum(!grepl('non-integer',fit_warnings,fixed=TRUE)),warnings=unique(fit_warnings))
}

make_config<-function(label){
 if(label=='Matched_random'){
  o<-readRDS(file.path(local,'scenario_Common.rds'));z<-o$long;vars<-c('experiencia','female','edad','public','public_permanent','grupos_docencia','impact','empathy','meritocracia','resources_excess');cov<-o$t[,c('id',vars)];z<-merge(z,cov,by='id',sort=FALSE);s<-'Matched';extended<-TRUE
 }else{s<-sub('_.*','',label);z<-if(s=='B')readRDS(file.path(local,'scenario_Main.rds'))$long else readRDS(file.path(source_root,paste0('scenario_',s,'.rds')))$long;extended<-FALSE}
 o<-prepare_pred(as.data.frame(z),s,extended);hist<-split_history(o,261026,grepl('ordered',label));list(o=o,hist=hist)
}
export_results<-function(label,o,hist,point,draws){
 M<-do.call(rbind,lapply(draws,`[[`,'stats'));stopifnot(nrow(M)==399,all(is.finite(M)))
 audit<-do.call(rbind,lapply(seq_along(draws),function(b)data.frame(scenario=label,draw=b,rank_drop=draws[[b]]$rank_drop,weight_warnings=draws[[b]]$weight_warnings,other_warnings=draws[[b]]$other_warnings)))
 stopifnot(all(audit$rank_drop==0),all(audit$other_warnings==0))
 csv(audit,paste0('prediction_fit_audit_',label));csv(data.frame(scenario=label,metric=names(point$stats),estimate=point$stats,teachers=o$n,history_decisions=sum(hist),test_decisions=sum(!hist)),paste0('prediction_point_',label))
 csv(data.frame(scenario=label,metric=names(point$stats),estimate=point$stats,se=apply(M,2,sd),lo=apply(M,2,quantile,.025),hi=apply(M,2,quantile,.975),draws=399,teachers=o$n),paste0('prediction_intervals_',label))
 reps<-lapply(if(grepl('ordered',label))1 else 1:20,function(k){a<-if(k==1)point else pipeline(o,split_history(o,261025+k));data.frame(scenario=label,partition=k,seed=261025+k,as.list(a$stats),rank_drop=a$rank_drop)})
 csv(do.call(rbind,reps),paste0('partition_sensitivity_',label))
 js(list(label=label,teachers=o$n,history=sum(hist),test=sum(!hist),folds=5,bootstrap=399,rank_drop=point$rank_drop,bootstrap_rank_drops=sum(audit$rank_drop),other_warnings=sum(audit$other_warnings),extended=isTRUE(o$extended),transformations='Extended continuous inputs centered/scaled within training fold and bootstrap fit',interpretation='within-encounter masked-card or ordered-card prediction; fixed folds/partition for bootstrap'),paste0('prediction_design_',label))
 saveRDS(list(o=o,hist=hist,point=point),file.path(local,paste0('prediction_',label,'.rds')))
}
phase<-if(length(commandArgs(TRUE)))commandArgs(TRUE)[1]else'adopt'
if(phase=='adopt'){
 checks<-list()
 for(label in c('B_random','B_ordered','C_random','NoScreen_random')){
  cfg<-make_config(label);o<-cfg$o;hist<-cfg$hist;point<-pipeline(o,hist,save_predictions=TRUE)
  old<-readRDS(file.path('3. Output/paper_C/local',paste0('prediction_',label,'.rds')))
  stopifnot(all(vapply(c('id','profile','position','y'),function(v)identical(o$z[[v]],old$o$z[[v]]),logical(1))),identical(hist,old$hist),max(abs(point$stats-old$point$stats))<1e-11)
  fp<-read_json(file.path('3. Output/paper_C/local',paste0('prediction_bootstrap_audited_',label,'.rds.json')),simplifyVector=TRUE)
  stopifnot(fp$code_md5==unname(tools::md5sum('2. Code/paper_C/analysis.R')),fp$data_md5==unname(tools::md5sum(file.path(source_root,paste0('scenario_',sub('_.*','',label),'.rds')))),fp$assignment_md5==unname(tools::md5sum('1. Data/raw data/df_named.parquet')),fp$plan_md5==unname(tools::md5sum('3. Output/paper_C/analysis_plan.md')))
  draws<-readRDS(file.path('3. Output/paper_C/local',paste0('prediction_bootstrap_audited_',label,'.rds')));export_results(label,o,hist,point,draws)
  saveRDS(draws,file.path(local,paste0('prediction_bootstrap_',label,'.rds')));checks[[label]]<-list(max_point_error=max(abs(point$stats-old$point$stats)),teachers=o$n,source_fingerprint=fp,review='3. Output/paper_C/independent_review.txt');message('Verified/adopted ',label)
 }
 js(checks,'prediction_adoption');quit(save='no')
}
if(phase=='matched'){
 label<-'Matched_random';cfg<-make_config(label);o<-cfg$o;hist<-cfg$hist;stopifnot(o$n==2164,sum(hist)==8656,sum(!hist)==8656,!anyNA(o$X))
 point<-pipeline(o,hist,save_predictions=TRUE);path<-file.path(local,'prediction_bootstrap_Matched_random.rds')
 fp<-list(script_md5=unname(tools::md5sum('2. Code/paper_A_author_revision/prediction.R')),input_md5=unname(tools::md5sum(file.path(local,'scenario_Common.rds'))),raw_md5=unname(tools::md5sum('1. Data/raw data/df_named.parquet')),draws=399L,seed='262000+b')
 fpp<-file.path(local,'prediction_bootstrap_Matched_random.json');if(file.exists(fpp))stopifnot(identical(read_json(fpp,simplifyVector=TRUE),fp))else write_json(fp,fpp,auto_unbox=TRUE,pretty=TRUE)
 draws<-if(file.exists(path))readRDS(path)else list();todo<-setdiff(1:399,seq_along(draws))
 run_one<-function(b){set.seed(262000+b);w<-tabulate(sample.int(o$n,replace=TRUE),nbins=o$n);a<-pipeline(o,hist,w);list(stats=a$stats,rank_drop=a$rank_drop,weight_warnings=a$weight_warnings,other_warnings=a$other_warnings)}
 cl<-parallel::makeCluster(4,rscript_args='--vanilla');parallel::clusterExport(cl,c('o','hist','pipeline','wtab','prob_clip','run_one'),envir=environment())
 for(first in seq.int(1,max(1,length(todo)),by=20)){if(!length(todo))break;ix<-todo[first:min(first+19,length(todo))];a<-parallel::parLapply(cl,ix,run_one);for(j in seq_along(ix))draws[[ix[j]]]<-a[[j]];saveRDS(draws,path);message('Matched prediction ',length(draws),'/399')}
 parallel::stopCluster(cl);export_results(label,o,hist,point,draws);print(point$stats);quit(save='no')
}
if(phase=='recompute'){
 # Full reproduction from owned algorithm and source data; no Paper C dependency.
 for(label in c('B_random','B_ordered','C_random','NoScreen_random')){
  cfg<-make_config(label);o<-cfg$o;hist<-cfg$hist;point<-pipeline(o,hist,save_predictions=TRUE)
  draws<-lapply(1:399,function(b){set.seed(262000+b);w<-tabulate(sample.int(o$n,replace=TRUE),nbins=o$n);a<-pipeline(o,hist,w);list(stats=a$stats,rank_drop=a$rank_drop,weight_warnings=a$weight_warnings,other_warnings=a$other_warnings)})
  export_results(label,o,hist,point,draws);saveRDS(draws,file.path(local,paste0('prediction_bootstrap_',label,'.rds')))
 }
}
