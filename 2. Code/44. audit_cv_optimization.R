# Independently fit the three highest-gradient Gaussian CV models using glmmTMB.
# This audits numerical stability; does not select models or change held-out sets.
# Run with Rscript --vanilla to use the installed glmmTMB runtime.
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(lme4);library(glmmTMB)})
out<-'3. Output/paper_A/reports'
o<-readRDS(file.path(out,'analysis_objects.rds'));saved<-readRDS(file.path(out,'variance_models.rds'))
cv<-readRDS(file.path(out,'within_teacher_cv_objects.rds'))
attrs<-c('male','background','failed','competence','absent','disruptive');slopes<-c('failed','competence','absent','disruptive')
z<-left_join(o$long[,c('id','y',attrs)],o$common[,unique(c('id',o$x3,'cell'))],by='id')
z<-z[complete.cases(z),];z$id<-factor(z$id);z$row<-seq_len(nrow(z));z$profile<-do.call(paste,c(z[,attrs],sep=':'))
dd<-read.csv(file.path(out,'within_teacher_cv_diagnostics.csv'))
target<-head(dd$round[order(dd$scaled_gradient,decreasing=TRUE)],3)
f<-as.formula(paste0('y~',paste(unlist(saved$blocks),collapse='+'),'+(1+failed_c+competence_c+absent_c+disruptive_c|id)'))
results<-list()
for(r in target){
 message('Refining CV partition ',r)
 chosen<-vapply(cv$schedule,function(x)x[r],character(1));held<-z$profile==chosen[as.character(z$id)]
 train<-z[!held,];test<-z[held,]
 for(v in slopes){a<-mean(train[[v]]);train[[paste0(v,'_c')]]<-train[[v]]-a;test[[paste0(v,'_c')]]<-test[[v]]-a}
 for(v in o$x3)if(is.numeric(train[[v]])&&length(unique(train[[v]]))>5){a<-mean(train[[v]]);s<-sd(train[[v]]);train[[v]]<-(train[[v]]-a)/s;test[[v]]<-(test[[v]]-a)/s}
 initial<-lmer(f,train,REML=TRUE,control=lmerControl(optimizer='nloptwrap',calc.derivs=FALSE,optCtrl=list(maxeval=20000)))
 S<-as.matrix(lme4::VarCorr(initial)$id)
 start<-list(beta=unname(fixef(initial)),theta=c(log(sqrt(diag(S))),glmmTMB::put_cor(cov2cor(S))))
 setup<-glmmTMB(f,train,family=gaussian(),REML=TRUE,doFit=FALSE)
 dispersion_name<-intersect(c('betad','betadisp'),names(setup$parameters))
 stopifnot(length(dispersion_name)==1)
 start[[dispersion_name]]<-log(sigma(initial)^2)
 m<-glmmTMB(f,train,family=gaussian(),REML=TRUE,start=start,control=glmmTMBControl(optCtrl=list(iter.max=2000,eval.max=3000)))
 g<-m$obj$gr(m$fit$par);H<-solve(m$sdr$cov.fixed);sc<-solve(chol(H),g)
 p<-as.numeric(predict(m,newdata=test,re.form=NULL))
 old<-cv$results[[which(vapply(cv$results,function(a)a$round,integer(1))==r)]]$pred
 stopifnot(identical(test$row,old$row))
 results[[length(results)+1]]<-data.frame(round=r,old_mse=mean((test$y-old$Four)^2),refined_mse=mean((test$y-p)^2),
  max_abs_prediction_change=max(abs(p-old$Four)),max_scaled_gradient=max(abs(sc)),
  convergence_gradient=max(pmin(abs(sc),abs(g))),min_hessian_eigenvalue=min(eigen(H,symmetric=TRUE)$values),
  positive_hessian=m$sdr$pdHess,optimizer_status=paste(m$fit$convergence,collapse=';'),
  lmer_logLik=as.numeric(logLik(initial)),glmmTMB_logLik=as.numeric(logLik(m)))
}
ans<-bind_rows(results);write.csv(ans,file.path(out,'within_teacher_cv_optimizer_audit.csv'),row.names=FALSE)
stopifnot(all(ans$optimizer_status=='0'),all(ans$positive_hessian),all(ans$min_hessian_eigenvalue>0))
print(ans)
