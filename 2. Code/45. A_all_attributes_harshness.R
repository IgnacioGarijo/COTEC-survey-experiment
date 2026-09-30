# Internal specification comparison requested on 2026-10-01; no manuscript edits.
# Rscript "2. Code/45. A_all_attributes_harshness.R"
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(lme4)})
out <- '3. Output/paper_A/reports'
o <- readRDS(file.path(out,'analysis_objects.rds'))
saved <- readRDS(file.path(out,'variance_models.rds'))
prior <- readRDS(file.path(out,'random_slopes_models.rds'))
attrs <- c('male','background','failed','competence','absent','disruptive')
four <- attrs[3:6]
z <- left_join(o$long[,c('id','y',attrs)],o$common[,unique(c('id',o$x3,'cell','hb'))],by='id')
z <- z[complete.cases(z),]; z$id <- factor(z$id)
stopifnot(nrow(z)==nobs(prior$models$Four),
 identical(as.character(z$id),as.character(model.frame(prior$models$Four)$id)),
 isTRUE(all.equal(z$y,as.numeric(model.response(model.frame(prior$models$Four))))))
for(v in attrs) z[[paste0(v,'_c')]] <- z[[v]]-mean(z[[v]])
pairs <- apply(combn(attrs,2),2,paste,collapse=':')
base <- list(Student=c(attrs,pairs),Professional=saved$blocks$Professional,Beliefs=saved$blocks$Beliefs)
blocks <- list(Current=saved$blocks,Pairwise_context=c(base,list(Context='cell')),
 Pairwise_no_context=base,Six_no_context=base,Harshness_six=c(base,list(Harshness='hb')))
slopes <- list(Current=four,Pairwise_context=four,Pairwise_no_context=four,Six_no_context=attrs,Harshness_six=attrs)
has_intercept <- c(Current=TRUE,Pairwise_context=TRUE,Pairwise_no_context=TRUE,Six_no_context=TRUE,Harshness_six=FALSE)
models <- list(Current=prior$models$Four,Pairwise_context=prior$models$Four_pairwise)
warnings <- list()
cache <- file.path(out,'all_attributes_harshness_models.rds')
if('--reuse'%in%commandArgs(TRUE)&&file.exists(cache)) {
 cached <- readRDS(cache);models <- cached$models;warnings <- cached$warnings
}
support <- bind_rows(lapply(attrs,function(v) {
 a <- tapply(z[[v]],z$id,function(x)length(unique(x)))
 data.frame(attribute=v,teachers_both_values=sum(a==2),teachers=length(a))
}))
write.csv(support,file.path(out,'all_attributes_harshness_support.csv'),row.names=FALSE)
for(n in setdiff(names(blocks),names(models))) {
 message('Fitting ',n)
 ff <- paste(unlist(blocks[[n]]),collapse='+')
 rr <- paste(c(if(has_intercept[n]) '1' else '0',paste0(slopes[[n]],'_c')),collapse='+')
 f <- as.formula(paste0('y~',ff,'+(',rr,'|id)'))
 w <- character()
 m <- withCallingHandlers(lmer(f,z,REML=TRUE,
  control=lmerControl(optimizer='nloptwrap',calc.derivs=TRUE,optCtrl=list(maxeval=30000))),
  warning=function(e){w<<-c(w,conditionMessage(e));invokeRestart('muffleWarning')})
 models[[n]] <- m;warnings[[n]] <- w
 saveRDS(list(models=models,warnings=warnings),cache)
 message(n,': singular=',isSingular(m),'; optimizer=',paste(m@optinfo$conv$opt,collapse=';'))
}
allocate <- function(m,b) {
 eta <- as.numeric(predict(m,re.form=NA));f <- nobars(formula(m));X <- model.matrix(f,z)
 labs <- attr(terms(f),'term.labels');groups <- lapply(b,function(a)which(attr(X,'assign')%in%match(a,labs)))
 K <- length(b);val <- numeric(2^K);shap <- numeric(K)
 for(mask in 0:(2^K-1)) {
  sel <- which(as.logical(intToBits(mask)[seq_len(K)]));cols <- unique(c(1,unlist(groups[sel])))
  val[mask+1] <- var(lm.fit(X[,cols,drop=FALSE],eta)$fitted.values)
 }
 for(j in seq_len(K))for(mask in 0:(2^K-1)) {
  sel <- which(as.logical(intToBits(mask)[seq_len(K)]));if(j%in%sel)next;s <- length(sel)
  shap[j] <- shap[j]+factorial(s)*factorial(K-s-1)/factorial(K)*
   (val[bitwOr(mask,bitwShiftL(1,j-1))+1]-val[mask+1])
 }
 stopifnot(abs(sum(shap)-var(eta))<1e-8,all(shap> -1e-8))
 setNames(shap,names(b))
}
parts <- list();diagnostics <- list();covariances <- list();coef_tables <- list()
for(n in names(blocks)) {
 m <- models[[n]];S <- as.matrix(VarCorr(m)$id);sn <- paste0(slopes[[n]],'_c')
 Z <- as.matrix(z[,sn]);general <- if(has_intercept[n])S[1,1] else 0
 criteria <- mean(rowSums((Z%*%S[sn,sn])*Z))
 cross <- if(has_intercept[n])2*sum(colMeans(Z)*S[1,sn]) else 0
 stopifnot(abs(cross)<1e-8)
 sh <- allocate(m,blocks[[n]]);ve <- sigma(m)^2;den <- sum(sh)+general+criteria+ve
 p <- c(sh,General=general,Criteria=criteria,Residual=ve)/den
 stopifnot(abs(sum(p)-1)<1e-8)
 parts[[n]] <- data.frame(model=n,component=names(p),share=as.numeric(p),variance=as.numeric(p)*den,denominator=den)
 g <- m@optinfo$derivs$gradient;H <- m@optinfo$derivs$Hessian
 sc <- if(length(g)&&!is.null(H))tryCatch(solve(chol(H),g),error=function(e)NA_real_) else NA_real_
 diagnostics[[n]] <- data.frame(model=n,n_decisions=nrow(z),n_teachers=n_distinct(z$id),
  n_fixed=length(fixef(m)),n_covariance_parameters=length(getME(m,'theta')),
  fixed_rank=qr(model.matrix(nobars(formula(m)),z))$rank,
  dropped_fixed=paste(names(attr(getME(m,'X'),'col.dropped')),collapse=';'),
  singular=isSingular(m),min_cov_eigenvalue=min(eigen(S,symmetric=TRUE)$values),
  max_scaled_gradient=if(all(is.na(sc)))NA_real_ else max(abs(sc)),
  convergence_gradient=if(all(is.na(sc)))NA_real_ else max(pmin(abs(sc),abs(g))),
  min_hessian_eigenvalue=if(is.null(H))NA_real_ else min(eigen(H,symmetric=TRUE)$values),
  optimizer_status=paste(m@optinfo$conv$opt,collapse=';'),
  messages=paste(unlist(m@optinfo$conv$lme4$messages),collapse=';'),
  warnings=paste(warnings[[n]],collapse=';'),
  accounted=1-ve/den,conditional_in_sample_r2=1-sum(residuals(m)^2)/sum((z$y-mean(z$y))^2),
  outside_01=mean(predict(m)<0|predict(m)>1),
  harshness_coefficient=if('hb'%in%names(fixef(m)))unname(fixef(m)['hb']) else NA_real_)
 covariances[[n]] <- transform(as.data.frame(VarCorr(m)),model=n)
 coef_tables[[n]] <- data.frame(model=n,term=names(fixef(m)),estimate=unname(fixef(m)))
}
write.csv(bind_rows(parts),file.path(out,'all_attributes_harshness_partition.csv'),row.names=FALSE)
write.csv(bind_rows(diagnostics),file.path(out,'all_attributes_harshness_diagnostics.csv'),row.names=FALSE)
write.csv(bind_rows(covariances),file.path(out,'all_attributes_harshness_covariances.csv'),row.names=FALSE)
write.csv(bind_rows(coef_tables),file.path(out,'all_attributes_harshness_coefficients.csv'),row.names=FALSE)
writeLines(trimws(capture.output(sessionInfo()),which='right'),file.path(out,'R_session_all_attributes_harshness.txt'))
print(bind_rows(diagnostics));print(bind_rows(parts));print(support)
