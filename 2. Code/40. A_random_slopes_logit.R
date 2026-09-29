# Binomial-logit sensitivity for the same sample and fixed effects.
# Report both latent and observed probability-scale variance; never equate them.
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(glmmTMB)})
out <- '3. Output/paper_A/reports'
o <- readRDS(file.path(out,'analysis_objects.rds'));base <- readRDS(file.path(out,'variance_models.rds'))
attrs <- c('male','background','failed','competence','absent','disruptive');slopes <- c('failed','competence','absent','disruptive');sn <- paste0(slopes,'_c')
z <- left_join(o$long[,c('id','y',attrs)],o$common[,unique(c('id',o$x3,'cell'))],by='id')
z <- z[complete.cases(z),];z$id <- factor(z$id)
for(v in slopes)z[[paste0(v,'_c')]] <- z[[v]]-mean(z[[v]])
for(v in o$x3)if(is.numeric(z[[v]])&&length(unique(z[[v]]))>5)z[[v]] <- as.numeric(scale(z[[v]]))
fixed <- paste(unlist(base$blocks),collapse='+')
form <- as.formula(paste0('y~',fixed,'+(1+failed_c+competence_c+absent_c+disruptive_c|id)'))
cache <- file.path(out,'random_slopes_logit_models.rds')
models <- if('--reuse'%in%commandArgs(TRUE)&&file.exists(cache))readRDS(cache) else list(Current=base$logit)
if(is.null(models$Four)){
 message('Fitting full correlated binomial model')
 models$Four <- glmmTMB(form,z,family=binomial(),control=glmmTMBControl(optCtrl=list(iter.max=2000,eval.max=3000)))
 saveRDS(models,cache)
}
if('--audit'%in%commandArgs(TRUE)){
 if(is.null(models$Four_BFGS)){
  message('Auditing full logit with BFGS optimizer from default starting values')
  models$Four_BFGS <- glmmTMB(form,z,family=binomial(),control=glmmTMBControl(optimizer=optim,optArgs=list(method='BFGS'),optCtrl=list(maxit=2000)))
  saveRDS(models,cache)
 }
 if(is.null(models$Four_diagonal)){
  message('Auditing diagonal random-effects logit')
  fd <- as.formula(paste0('y~',fixed,'+diag(1+failed_c+competence_c+absent_c+disruptive_c|id)'))
  models$Four_diagonal <- glmmTMB(fd,z,family=binomial(),control=glmmTMBControl(optCtrl=list(iter.max=2000,eval.max=3000)))
  saveRDS(models,cache)
 }
}
diagnostic <- function(m,n){
 S <- as.matrix(VarCorr(m)$cond$id);ev <- eigen(S,symmetric=TRUE)$values
 data.frame(model=n,convergence_code=m$fit$convergence,positive_hessian=m$sdr$pdHess,
 message=m$fit$message,AIC=AIC(m),logLik=as.numeric(logLik(m)),min_cov_eigenvalue=min(ev),max_cov_eigenvalue=max(ev),
 max_abs_gradient=max(abs(m$obj$gr(m$fit$par))),min_conditional_probability=min(predict(m,type='response')),max_conditional_probability=max(predict(m,type='response')))
}
gh <- function(k){J<-matrix(0,k,k);J[cbind(1:(k-1),2:k)]<-sqrt(1:(k-1));J<-J+t(J);e<-eigen(J,symmetric=TRUE);list(nodes=e$values,weights=e$vectors[1,]^2)}
partition <- function(m,n,k){
 S <- as.matrix(VarCorr(m)$cond$id);eta <- as.numeric(predict(m,newdata=z,re.form=NA))
 Z <- if(ncol(S)==1)matrix(1,nrow(z),1) else cbind(1,as.matrix(z[,sn]))
 vr <- rowSums((Z%*%S)*Z);stopifnot(min(vr)> -1e-8)
 if(k==0){
  # Adaptive integration independently verifies large-variance fits for which
  # ordinary Gauss-Hermite nodes do not resolve the narrow logistic transition.
  val <- vapply(seq_along(eta),function(i){
   sd <- sqrt(max(vr[i],0));a <- eta[i]
   c(integrate(function(t)plogis(a+sd*t)*dnorm(t),-Inf,Inf,rel.tol=1e-8,subdivisions=300)$value,
     integrate(function(t)plogis(a+sd*t)^2*dnorm(t),-Inf,Inf,rel.tol=1e-8,subdivisions=300)$value)
  },numeric(2))
  mu <- val[1,];ep2 <- val[2,]
 }else{
  q <- gh(k);p <- plogis(eta+outer(sqrt(pmax(vr,0)),q$nodes))
  mu <- as.vector(p%*%q$weights);ep2 <- as.vector((p^2)%*%q$weights)
 }
 vf <- mean((mu-mean(mu))^2);vu <- mean(ep2-mu^2);ve <- mean(mu-ep2);den <- vf+vu+ve
 stopifnot(abs(den-mean(mu)*(1-mean(mu)))<1e-10)
 latent_den <- var(eta)+mean(vr)+pi^2/3
 criteria <- if(ncol(S)>1)mean(rowSums((Z[,-1]%*%S[-1,-1])*Z[,-1])) else 0
 data.frame(model=n,nodes=k,mean_probability=mean(mu),observed_fixed=vf/den,observed_teacher=vu/den,observed_residual=ve/den,
 observed_total=(vf+vu)/den,latent_fixed=var(eta)/latent_den,latent_general=S[1,1]/latent_den,
 latent_criteria=criteria/latent_den,latent_residual=(pi^2/3)/latent_den)
}
diag <- bind_rows(lapply(names(models),function(n)diagnostic(models[[n]],n)))
write.csv(diag,file.path(out,'random_slopes_logit_diagnostics.csv'),row.names=FALSE)
ans <- bind_rows(lapply(names(models),function(n){message('Integrating ',n);bind_rows(lapply(c(60,120,0),function(k)partition(models[[n]],n,k)))}))
write.csv(ans,file.path(out,'random_slopes_logit_partition.csv'),row.names=FALSE)
covs <- bind_rows(lapply(names(models),function(n){S<-as.matrix(VarCorr(models[[n]])$cond$id);as.data.frame(as.table(S))%>%mutate(model=n)}))
write.csv(covs,file.path(out,'random_slopes_logit_covariances.csv'),row.names=FALSE)
print(diag);print(ans)
writeLines(trimws(capture.output(sessionInfo()),which='right'),file.path(out,'R_session_random_slopes_logit.txt'))
