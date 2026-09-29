# Internal comparison requested by the authors; does not change the manuscript.
# Run from repository root: Rscript "2. Code/37. A_harshness_partition_comparison.R"
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(lme4)})
out <- '3. Output/paper_A/reports'
o <- readRDS(file.path(out,'analysis_objects.rds'))
saved <- readRDS(file.path(out,'variance_models.rds'))
attrs <- c('male','background','failed','competence','absent','disruptive')
z <- left_join(o$long[,c('id','y',attrs)],
               o$common[,unique(c('id',o$x3,'cell','n_cards','hb'))],by='id')
z <- z[complete.cases(z),]; z$id <- factor(z$id)
blocks <- saved$blocks
base_formula <- reformulate(unlist(blocks,use.names=FALSE),'y')
base <- lm(base_formula,z)
full <- lm(update(base_formula,.~.+hb),z)
mixed <- saved$linear
stopifnot(nrow(z)==nobs(mixed),
          identical(as.character(z$id),as.character(model.frame(mixed)$id)),
          isTRUE(all.equal(as.numeric(z$y),as.numeric(model.response(model.frame(mixed))))))
# The full OLS predictor projected on a subset gives the same fitted values
# as regressing y on that subset. Thus this is an exact Shapley R-squared
# allocation, averaging each block's incremental fit over all block orders.
allocate <- function(m,dat,b,mixed_model=FALSE){
 f <- if(mixed_model)nobars(formula(m)) else formula(m)
 eta <- if(mixed_model)as.numeric(predict(m,re.form=NA)) else fitted(m)
 X <- model.matrix(f,dat); labels <- attr(terms(f),'term.labels')
 groups <- lapply(b,function(a)which(attr(X,'assign')%in%match(a,labels)))
 K <- length(b); values <- numeric(2^K)
 for(mask in 0:(2^K-1)){
  sel <- which(as.logical(intToBits(mask)[seq_len(K)]))
  cols <- unique(c(1,unlist(groups[sel])))
  values[mask+1] <- var(lm.fit(X[,cols,drop=FALSE],eta)$fitted.values)
 }
 shap <- numeric(K)
 for(j in seq_len(K))for(mask in 0:(2^K-1)){
  sel <- which(as.logical(intToBits(mask)[seq_len(K)]))
  if(j%in%sel)next
  s <- length(sel)
  shap[j] <- shap[j]+factorial(s)*factorial(K-s-1)/factorial(K)*
   (values[bitwOr(mask,bitwShiftL(1,j-1))+1]-values[mask+1])
 }
 stopifnot(abs(sum(shap)-var(eta))<1e-8,all(shap> -1e-10))
 if(mixed_model){
  vu <- as.numeric(VarCorr(m)$id[1]); ve <- sigma(m)^2
  den <- var(eta)+vu+ve
  parts <- c(setNames(shap,names(b)),Teacher_residual=vu,Decision_residual=ve)/den
 }else{
  den <- var(dat$y)
  parts <- c(setNames(shap,names(b)),Decision_residual=var(residuals(m)))/den
  stopifnot(abs(sum(shap)/den-summary(m)$r.squared)<1e-10)
 }
 stopifnot(abs(sum(parts)-1)<1e-8)
 data.frame(component=names(parts),share=as.numeric(parts),denominator=den)
}
parts <- bind_rows(
 transform(allocate(mixed,z,blocks,TRUE),model='Current random intercept'),
 transform(allocate(base,z,blocks),model='OLS without harshness'),
 transform(allocate(full,z,c(blocks,list(Harshness='hb'))),model='OLS with harshness'))
old <- read.csv('3. Output/paper_A/tables/variance_partition.csv')
stopifnot(max(abs(parts$share[parts$model=='Current random intercept']-old$share))<1e-8)
diag <- data.frame(n_decisions=nrow(z),n_teachers=n_distinct(z$id),
 r2_OLS_without_harshness=summary(base)$r.squared,
 r2_OLS_with_harshness=summary(full)$r.squared,
 incremental_r2_harshness=summary(full)$r.squared-summary(base)$r.squared,
 fraction_of_remaining_variance=(summary(full)$r.squared-summary(base)$r.squared)/(1-summary(base)$r.squared),
 harshness_coefficient=unname(coef(full)['hb']),
 OLS_adjusted_r2=summary(full)$adj.r.squared,
 OLS_outside_01=mean(fitted(full)<0|fitted(full)>1))
write.csv(parts,file.path(out,'harshness_partition_comparison.csv'),row.names=FALSE)
write.csv(diag,file.path(out,'harshness_partition_diagnostics.csv'),row.names=FALSE)
capture.output(sessionInfo(),file=file.path(out,'R_session_harshness_partition.txt'))
print(parts);print(diag)
