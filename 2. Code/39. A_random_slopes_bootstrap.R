# Nonparametric teacher bootstrap for the four-slope variance partition.
# Whole teacher clusters are resampled; copies receive distinct IDs.
# Rscript "2. Code/39. A_random_slopes_bootstrap.R" [replicates=399] [workers=3]
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(lme4)})
out <- '3. Output/paper_A/reports'
o <- readRDS(file.path(out,'analysis_objects.rds'))
main <- readRDS(file.path(out,'random_slopes_models.rds'))$models$Four
blocks <- readRDS(file.path(out,'variance_models.rds'))$blocks
attrs <- c('male','background','failed','competence','absent','disruptive')
slopes <- c('failed','competence','absent','disruptive'); sn <- paste0(slopes,'_c')
z <- left_join(o$long[,c('id','y',attrs)],o$common[,unique(c('id',o$x3,'cell'))],by='id')
z <- z[complete.cases(z),];z$id <- factor(z$id)
for(v in slopes)z[[paste0(v,'_c')]] <- z[[v]]-mean(z[[v]])
stopifnot(nrow(z)==nobs(main),identical(as.character(z$id),as.character(model.frame(main)$id)))
form <- formula(main);form_fixed <- nobars(form);initial <- getME(main,'theta')
idx <- split(seq_len(nrow(z)),z$id)
partition <- function(m,d){
 eta <- as.numeric(predict(m,re.form=NA));X <- model.matrix(form_fixed,d)
 labs <- attr(terms(form_fixed),'term.labels');groups <- lapply(blocks,function(a)which(attr(X,'assign')%in%match(a,labs)))
 K <- length(blocks);values <- numeric(2^K);shap <- numeric(K)
 for(mask in 0:(2^K-1)){
  sel <- which(as.logical(intToBits(mask)[seq_len(K)]));cols <- unique(c(1,unlist(groups[sel])))
  values[mask+1] <- var(lm.fit(X[,cols,drop=FALSE],eta)$fitted.values)
 }
 for(j in seq_len(K))for(mask in 0:(2^K-1)){
  sel <- which(as.logical(intToBits(mask)[seq_len(K)]));if(j%in%sel)next;s <- length(sel)
  shap[j] <- shap[j]+factorial(s)*factorial(K-s-1)/factorial(K)*(values[bitwOr(mask,bitwShiftL(1,j-1))+1]-values[mask+1])
 }
 S <- as.matrix(VarCorr(m)$id);Z <- as.matrix(d[,sn]);general <- S[1,1]
 criteria <- mean(rowSums((Z%*%S[-1,-1])*Z));cross <- 2*sum(colMeans(Z)*S[1,-1])
 vf <- var(eta);ve <- sigma(m)^2;den <- vf+general+criteria+cross+ve
 stopifnot(abs(cross)<1e-8,abs(sum(shap)-vf)<1e-8)
 p <- c(setNames(shap,names(blocks)),General=general,Criteria=criteria,Residual=ve)/den
 stopifnot(abs(sum(p)-1)<1e-8,all(p> -1e-10))
 c(p,Observed=vf/den,Teacher=(general+criteria)/den,Total=1-ve/den)
}
point <- partition(main,z)
one <- function(b){
 set.seed(930000+b);start <- proc.time()[3]
 draw <- sample(seq_along(idx),length(idx),replace=TRUE);rr <- unlist(idx[draw],use.names=FALSE)
 zz <- z[rr,];zz$id <- factor(rep(seq_along(draw),lengths(idx[draw])))
 for(v in slopes)zz[[paste0(v,'_c')]] <- zz[[v]]-mean(zz[[v]])
 w <- character()
 ans <- tryCatch(withCallingHandlers({
  fit <- lmer(form,zz,REML=TRUE,start=list(theta=initial),control=lmerControl(optimizer='nloptwrap',calc.derivs=FALSE,check.conv.singular='ignore',optCtrl=list(maxeval=20000)))
  msg <- paste(unlist(fit@optinfo$conv$lme4$messages),collapse=';')
  if(any(fit@optinfo$conv$opt!=0)||nzchar(msg)){
   fit <- lmer(form,zz,REML=TRUE,control=lmerControl(optimizer='bobyqa',calc.derivs=FALSE,check.conv.singular='ignore',optCtrl=list(maxfun=100000)))
   msg <- paste(unlist(fit@optinfo$conv$lme4$messages),collapse=';')
  }
  stopifnot(all(fit@optinfo$conv$opt==0),!nzchar(msg))
  list(p=partition(fit,zz),singular=isSingular(fit),message=msg)
 },warning=function(e){w<<-c(w,conditionMessage(e));invokeRestart('muffleWarning')}),error=function(e)list(p=setNames(rep(NA_real_,length(point)),names(point)),singular=NA,message=conditionMessage(e)))
 data.frame(draw=b,as.list(ans$p),singular=ans$singular,message=ans$message,warnings=paste(w,collapse=';'),seconds=unname(proc.time()[3]-start),check.names=FALSE)
}
args <- commandArgs(TRUE);B <- if(length(args))as.integer(args[1]) else 399L;workers <- if(length(args)>1)as.integer(args[2]) else 3L
path <- file.path(out,'random_slopes_bootstrap_draws.csv')
done <- if(file.exists(path))read.csv(path,check.names=FALSE) else data.frame()
todo <- setdiff(seq_len(B),if(nrow(done))done$draw else integer())
if(length(todo)){
 cl <- parallel::makePSOCKcluster(workers)
 parallel::clusterEvalQ(cl,{suppressPackageStartupMessages(library(lme4));NULL})
 parallel::clusterExport(cl,c('z','slopes','sn','form','form_fixed','initial','idx','partition','blocks','point','one'),envir=environment())
 for(batch in split(todo,ceiling(seq_along(todo)/(workers*4)))){
  got <- parallel::parLapply(cl,batch,one)
  done <- bind_rows(done,bind_rows(got));write.csv(done,path,row.names=FALSE,na='')
  message('Completed ',sum(done$draw<=B),'/',B,'; median seconds/fit=',round(median(done$seconds),1))
 }
 parallel::stopCluster(cl)
}
done <- done[done$draw<=B,];good <- complete.cases(done[,names(point)])
stopifnot(sum(good)>=.95*B)
results <- data.frame(component=names(point),share=as.numeric(point),
 se=sapply(done[good,names(point)],sd),lo=sapply(done[good,names(point)],quantile,.025),hi=sapply(done[good,names(point)],quantile,.975),
 replicates=B,successful=sum(good),singular=sum(done$singular[good],na.rm=TRUE),row.names=NULL)
write.csv(results,file.path(out,'random_slopes_uncertainty.csv'),row.names=FALSE)
writeLines(trimws(capture.output(sessionInfo()),which='right'),file.path(out,'R_session_random_slopes_validation.txt'))
print(results)
