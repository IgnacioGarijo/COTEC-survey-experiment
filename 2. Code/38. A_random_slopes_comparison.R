# Complementary analysis: heterogeneous teacher responses to attributes.
# Run from repository root; manuscript and main analysis remain unchanged.
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(lme4)})
out <- '3. Output/paper_A/reports'
o <- readRDS(file.path(out,'analysis_objects.rds'))
saved <- readRDS(file.path(out,'variance_models.rds'))
attrs <- c('male','background','failed','competence','absent','disruptive')
slopes <- c('failed','competence','absent','disruptive')
z <- left_join(o$long[,c('id','y',attrs)],o$common[,unique(c('id',o$x3,'cell','n_cards','hb'))],by='id')
z <- z[complete.cases(z),];z$id <- factor(z$id)
stopifnot(nrow(z)==nobs(saved$linear),identical(as.character(z$id),as.character(model.frame(saved$linear)$id)),
          isTRUE(all.equal(as.numeric(z$y),as.numeric(model.response(model.frame(saved$linear))))))
for(v in slopes)z[[paste0(v,'_c')]] <- z[[v]]-mean(z[[v]])
support <- bind_rows(lapply(slopes,function(v){
 s <- tapply(z[[v]],z$id,function(x)length(unique(x)))
 data.frame(attribute=v,teachers_both_values=sum(s==2),teachers_total=length(s),share_both=mean(s==2))
}))
write.csv(support,file.path(out,'random_slopes_support.csv'),row.names=FALSE)
fixed <- paste(unlist(saved$blocks),collapse='+')
pair_fixed <- paste(c(paste0('(',paste(attrs,collapse='+'),')^2'),unlist(saved$blocks[-1])),collapse='+')
ctrl <- lmerControl(optimizer='bobyqa',calc.derivs=TRUE,optCtrl=list(maxfun=200000))
forms <- list(Academic=paste0('y~',fixed,'+(1+failed_c+competence_c|id)'),
 Four=paste0('y~',fixed,'+(1+failed_c+competence_c+absent_c+disruptive_c|id)'),
 Four_diagonal=paste0('y~',fixed,'+(1+failed_c+competence_c+absent_c+disruptive_c||id)'),
 Four_pairwise=paste0('y~',pair_fixed,'+(1+failed_c+competence_c+absent_c+disruptive_c|id)'))
cache <- file.path(out,'random_slopes_models.rds')
prior <- if('--reuse'%in%commandArgs(TRUE)&&file.exists(cache))readRDS(cache) else NULL
models <- list(Current=saved$linear);data <- list(Current=z);warnings <- list()
for(n in names(forms)){
 message('Fitting ',n)
 w <- character()
 models[[n]] <- if(!is.null(prior$models[[n]]))prior$models[[n]] else withCallingHandlers(lmer(as.formula(forms[[n]]),z,REML=TRUE,control=ctrl),warning=function(e){w<<-c(w,conditionMessage(e));invokeRestart('muffleWarning')})
 data[[n]] <- z;warnings[[n]] <- w
 saveRDS(list(models=models,warnings=warnings),file.path(out,'random_slopes_models.rds'))
 message(n,': singular=',isSingular(models[[n]]),'; warnings=',paste(w,collapse=';'))
}
for(n in c('Four_eight','Four_optimizer')){
 message('Checking ',n)
 zz <- if(n=='Four_eight')droplevels(z[z$n_cards>=8,]) else z
 for(v in slopes)zz[[paste0(v,'_c')]] <- zz[[v]]-mean(zz[[v]])
 cc <- if(n=='Four_optimizer')lmerControl(optimizer='nloptwrap',calc.derivs=TRUE,optCtrl=list(maxeval=200000)) else ctrl
 models[[n]] <- if(!is.null(prior$models[[n]]))prior$models[[n]] else lmer(as.formula(forms$Four),zz,REML=TRUE,control=cc)
 data[[n]] <- zz
}
saveRDS(list(models=models,warnings=warnings),cache)
# Evaluate covariance on observed profiles: mean(z_i' Sigma z_i).
# Global centering makes average intercept-slope covariance exactly zero.
# Slope-slope covariance is retained inside the aggregate criteria component.
summarize <- function(m,d,n){
 vc <- VarCorr(m);sn <- paste0(slopes,'_c');rn <- c('(Intercept)',sn)
 S <- matrix(0,length(rn),length(rn),dimnames=list(rn,rn))
 for(a in vc){q<-rownames(a);S[q,q]<-S[q,q]+as.matrix(a)}
 Z <- cbind(1,as.matrix(d[,sn]));colnames(Z)<-rn
 teacher <- mean(rowSums((Z%*%S)*Z))
 general <- S[1,1]
 criteria <- mean(rowSums((Z[,-1,drop=FALSE]%*%S[-1,-1])*Z[,-1,drop=FALSE]))
 cross <- teacher-general-criteria
 vf <- var(predict(m,re.form=NA));ve <- sigma(m)^2;tot <- vf+teacher+ve
 stopifnot(abs(cross)<1e-8,criteria> -1e-8,abs(sum(c(vf,general,criteria,cross,ve))/tot-1)<1e-8)
 g <- m@optinfo$derivs$gradient
 H <- m@optinfo$derivs$Hessian
 scaled <- if(length(g)&&!is.null(H))tryCatch(max(abs(solve(chol(H),g))),error=function(e)NA_real_) else NA_real_
 data.frame(model=n,n_decisions=nrow(d),n_teachers=n_distinct(d$id),
 fixed_share=vf/tot,general_share=general/tot,criteria_share=criteria/tot,
 residual_share=ve/tot,total_accounted=(vf+teacher)/tot,variance_denominator=tot,
 fixed_variance=vf,general_variance=general,criteria_variance=criteria,residual_variance=ve,
 singular=isSingular(m),min_cov_eigenvalue=min(unlist(lapply(vc,function(a)eigen(a,symmetric=TRUE)$values))),
 REML_AIC=AIC(m),REML_logLik=as.numeric(logLik(m)),
 max_abs_gradient=if(length(g))max(abs(g)) else NA,
 max_abs_scaled_gradient=scaled,optimizer_status=paste(m@optinfo$conv$opt,collapse=';'),
 convergence=paste(unlist(m@optinfo$conv$lme4$messages),collapse=';'),
 conditional_in_sample_r2=1-sum(residuals(m)^2)/sum((d$y-mean(d$y))^2),
 outside_01=mean(predict(m)<0|predict(m)>1))
}
ans <- bind_rows(lapply(names(models),function(n)summarize(models[[n]],data[[n]],n)))
write.csv(ans,file.path(out,'random_slopes_comparison.csv'),row.names=FALSE)
covs <- bind_rows(lapply(names(models),function(n)transform(as.data.frame(VarCorr(models[[n]])),model=n)))
write.csv(covs,file.path(out,'random_slopes_covariances.csv'),row.names=FALSE)
capture.output(sessionInfo(),file=file.path(out,'R_session_random_slopes.txt'))
print(ans);print(support)
