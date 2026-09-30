# Paired repeated validation of random-intercept vs four-random-slope LMMs.
# No response in a held-out teacher/profile enters either training set.
# Rscript "2. Code/42. A_within_teacher_validation.R" [--inspect]
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(lme4)})
out <- '3. Output/paper_A/reports'
o <- readRDS(file.path(out,'analysis_objects.rds'))
saved <- readRDS(file.path(out,'variance_models.rds'))
attrs <- c('male','background','failed','competence','absent','disruptive')
slopes <- c('failed','competence','absent','disruptive')
z <- left_join(o$long[,c('id','y',attrs)],o$common[,unique(c('id',o$x3,'cell'))],by='id')
z <- z[complete.cases(z),];z$id <- factor(z$id)
stopifnot(nrow(z)==nobs(saved$linear),identical(as.character(z$id),as.character(model.frame(saved$linear)$id)))
z$row <- seq_len(nrow(z));z$profile <- do.call(paste,c(z[,attrs],sep=':'))
by_teacher <- split(seq_len(nrow(z)),z$id)
profiles <- lapply(by_teacher,function(ii)unique(z$profile[ii]))
support <- data.frame(n_teachers=length(profiles),n_decisions=nrow(z),
 min_cards=min(lengths(by_teacher)),max_cards=max(lengths(by_teacher)),
 min_unique_profiles=min(lengths(profiles)),max_unique_profiles=max(lengths(profiles)),
 teachers_with_repeated_profiles=sum(lengths(by_teacher)>lengths(profiles)))
print(support);stopifnot(all(lengths(profiles)>=2))
if('--inspect'%in%commandArgs(TRUE))quit(save='no')
R <- 30L;workers <- 4L
# Balanced random schedules: every recorded profile is held out approximately
# equally often for each teacher. The schedule never examines response values.
set.seed(930420)
schedule <- lapply(profiles,function(p)unlist(replicate(ceiling(R/length(p)),sample(p),simplify=FALSE),use.names=FALSE)[seq_len(R)])
fixed <- paste(unlist(saved$blocks),collapse='+')
forms <- list(Intercept=as.formula(paste0('y~',fixed,'+(1|id)')),
 Four=as.formula(paste0('y~',fixed,'+(1+failed_c+competence_c+absent_c+disruptive_c|id)')))
fit_model <- function(form,train){
 w<-character()
 m<-withCallingHandlers(lmer(form,train,REML=TRUE,control=lmerControl(optimizer='nloptwrap',calc.derivs=TRUE,optCtrl=list(maxeval=20000))),warning=function(e){w<<-c(w,conditionMessage(e));invokeRestart('muffleWarning')})
 msg<-paste(unlist(m@optinfo$conv$lme4$messages),collapse=';')
 # Boundary fits are retained; convergence problems trigger another optimizer.
 if(any(m@optinfo$conv$opt!=0)||any(grepl('converg|Hessian|identif',c(msg,w),ignore.case=TRUE))){
  m<-withCallingHandlers(lmer(form,train,REML=TRUE,control=lmerControl(optimizer='bobyqa',calc.derivs=TRUE,optCtrl=list(maxfun=100000))),warning=function(e){w<<-c(w,conditionMessage(e));invokeRestart('muffleWarning')})
  msg<-paste(unlist(m@optinfo$conv$lme4$messages),collapse=';')
 }
 list(m=m,warnings=paste(w,collapse=';'),message=msg)
}
one <- function(r){
 start<-proc.time()[3]
 chosen<-vapply(schedule,function(x)x[r],character(1))
 held<-z$profile==chosen[as.character(z$id)]
 train<-z[!held,];test<-z[held,]
 stopifnot(length(unique(train$id))==length(profiles),length(unique(test$id))==length(profiles),
           length(intersect(train$row,test$row))==0,
           length(intersect(paste(train$id,train$profile),paste(test$id,test$profile)))==0)
 for(v in slopes){a<-mean(train[[v]]);train[[paste0(v,'_c')]]<-train[[v]]-a;test[[paste0(v,'_c')]]<-test[[v]]-a}
 pred<-test[,c('id','row','y')];diags<-list()
 for(n in names(forms)){
  ans<-fit_model(forms[[n]],train);m<-ans$m
  pred[[n]]<-as.numeric(predict(m,newdata=test,re.form=NULL,allow.new.levels=FALSE))
  g<-m@optinfo$derivs$gradient;H<-m@optinfo$derivs$Hessian
  scaled<-tryCatch(max(abs(solve(chol(H),g))),error=function(e)NA_real_)
  diags[[n]]<-data.frame(round=r,model=n,n_train=nrow(train),n_test=nrow(test),
   singular=isSingular(m),optimizer_status=paste(m@optinfo$conv$opt,collapse=';'),scaled_gradient=scaled,
   message=ans$message,warnings=ans$warnings,outside_01=mean(pred[[n]]<0|pred[[n]]>1))
 }
 pred$round<-r
 # Equal teacher weights; multiple copies of a held-out profile do not give
 # a teacher extra weight. Primary score uses raw linear predictions.
 score<-pred%>%group_by(id)%>%summarise(
  intercept=mean((y-Intercept)^2),four=mean((y-Four)^2),
  intercept_clipped=mean((y-pmin(pmax(Intercept,0),1))^2),
  four_clipped=mean((y-pmin(pmax(Four,0),1))^2),.groups='drop')
 list(round=r,pred=pred,score=score,diagnostics=bind_rows(diags),seconds=unname(proc.time()[3]-start))
}
cache<-file.path(out,'within_teacher_cv_objects.rds')
fingerprint<-tools::md5sum(file.path(out,'analysis_objects.rds'))
prior<-if(file.exists(cache))readRDS(cache) else NULL
if(!is.null(prior))stopifnot(identical(prior$fingerprint,fingerprint),identical(prior$rounds,R),identical(prior$schedule,schedule))
done<-if(is.null(prior))list() else prior$results
todo<-setdiff(seq_len(R),vapply(done,function(a)a$round,integer(1)))
if(length(todo)){
 cl<-parallel::makePSOCKcluster(workers)
 parallel::clusterEvalQ(cl,{suppressPackageStartupMessages({library(lme4);library(dplyr)});NULL})
 parallel::clusterExport(cl,c('z','profiles','schedule','slopes','forms','fit_model','one'),envir=environment())
 for(batch in split(todo,ceiling(seq_along(todo)/workers))){
  got<-parallel::parLapplyLB(cl,batch,one);done<-c(done,got)
  saveRDS(list(fingerprint=fingerprint,rounds=R,schedule=schedule,results=done),cache)
  message('Completed ',length(done),'/',R,' paired partitions; median seconds=',round(median(vapply(done,function(a)a$seconds,numeric(1))),1))
 }
 parallel::stopCluster(cl)
}
diag<-bind_rows(lapply(done,function(a)a$diagnostics))
write.csv(diag,file.path(out,'within_teacher_cv_diagnostics.csv'),row.names=FALSE)
stopifnot(nrow(diag)==2*R,all(diag$optimizer_status=='0'),!any(grepl('converg|Hessian|identif',diag$message,ignore.case=TRUE)))
folds<-bind_rows(lapply(done,function(a)data.frame(round=a$round,as.list(colMeans(as.data.frame(a$score[,-1]))),seconds=a$seconds)))
folds$improvement<-folds$intercept-folds$four
folds$relative_reduction<-folds$improvement/folds$intercept
write.csv(folds,file.path(out,'within_teacher_cv_partitions.csv'),row.names=FALSE)
loss<-bind_rows(lapply(done,function(a)a$score))%>%group_by(id)%>%summarise(across(everything(),mean),.groups='drop')
# Paired bootstrap of each teacher's mean held-out loss over all partitions.
# This estimates uncertainty in the held-out loss contrast conditional on the
# fitted CV models; it is not a second nested refitting of the training sets.
set.seed(930421);B<-3999L
boots<-replicate(B,{ii<-sample.int(nrow(loss),nrow(loss),replace=TRUE);v<-colMeans(as.data.frame(loss[ii,-1]));c(raw_difference=v['intercept']-v['four'],raw_relative=(v['intercept']-v['four'])/v['intercept'],clipped_difference=v['intercept_clipped']-v['four_clipped'])})
rownames(boots)<-c('raw_difference','raw_relative','clipped_difference')
v<-colMeans(as.data.frame(loss[,-1]));values<-c(v['intercept']-v['four'],(v['intercept']-v['four'])/v['intercept'],v['intercept_clipped']-v['four_clipped'])
summary<-data.frame(metric=rownames(boots),estimate=unname(values),se=apply(boots,1,sd),lo=apply(boots,1,quantile,.025),hi=apply(boots,1,quantile,.975),replicates=B,row.names=NULL)
write.csv(summary,file.path(out,'within_teacher_cv_uncertainty.csv'),row.names=FALSE)
overall<-data.frame(rounds=R,n_teachers=nrow(loss),n_original_decisions=nrow(z),
 mse_intercept=v['intercept'],mse_four=v['four'],mse_intercept_clipped=v['intercept_clipped'],mse_four_clipped=v['four_clipped'],
 rounds_improved=sum(folds$improvement>0),min_improvement=min(folds$improvement),max_improvement=max(folds$improvement),
 singular_intercept=sum(diag$singular[diag$model=='Intercept']),singular_four=sum(diag$singular[diag$model=='Four']))
write.csv(overall,file.path(out,'within_teacher_cv_summary.csv'),row.names=FALSE)
write.csv(support,file.path(out,'within_teacher_cv_support.csv'),row.names=FALSE)
writeLines(trimws(capture.output(sessionInfo()),which='right'),file.path(out,'R_session_within_teacher_cv.txt'))
print(overall);print(summary)
