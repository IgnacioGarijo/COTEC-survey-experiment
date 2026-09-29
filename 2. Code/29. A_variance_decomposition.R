# Model-based variance partition. Shapley projection allocates fitted systematic variance.
options(stringsAsFactors=FALSE);invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(lme4);library(jsonlite)})
out<-'3. Output/paper_A';o<-readRDS(file.path(out,'reports/analysis_objects.rds'))
export<-function(x,n)write.csv(x,file.path(out,'tables',paste0(n,'.csv')),row.names=FALSE,na='')
attrs<-c('male','background','failed','competence','absent','disruptive')
z<-o$long[,c('id','y',attrs)];z<-left_join(z,o$common[,c('id',o$x3,'cell','n_cards')],by='id');z<-z[complete.cases(z),];z$id<-factor(z$id)
blocks<-list(Student=c(attrs,'failed:competence'),Professional=o$x1,Beliefs=setdiff(o$x3,o$x1),Context='cell')
fixed<-unlist(blocks,use.names=FALSE);f<-as.formula(paste('y~',paste(fixed,collapse='+'),'+(1|id)'))
ctrl<-lmerControl(optimizer='bobyqa',check.conv.singular='ignore')
fit<-lmer(f,z,REML=TRUE,control=ctrl)
# Shapley game v(S)=variance of projection of full fixed predictor onto design blocks S.
# All subsets use the SAME full fitted predictor, population, and total denominator.
# This is a descriptive allocation of fitted systematic variance, not reduced-model R2.
partition<-function(m,dat,b=blocks){
 eta<-as.numeric(predict(m,re.form=NA));X<-model.matrix(nobars(formula(m)),dat);terms<-attr(terms(nobars(formula(m))),'term.labels');assign<-attr(X,'assign')
 groups<-lapply(b,function(a)which(assign%in%match(a,terms)));K<-length(b);values<-numeric(2^K)
 for(mask in 0:(2^K-1)){sel<-which(as.logical(intToBits(mask)[seq_len(K)]));cols<-unique(c(1,unlist(groups[sel])));fitted<-lm.fit(X[,cols,drop=FALSE],eta)$fitted.values;values[mask+1]<-var(fitted)}
 shap<-numeric(K)
 for(j in seq_len(K))for(mask in 0:(2^K-1)){sel<-which(as.logical(intToBits(mask)[seq_len(K)]));if(j%in%sel)next;s<-length(sel);shap[j]<-shap[j]+factorial(s)*factorial(K-s-1)/factorial(K)*(values[bitwOr(mask,bitwShiftL(1,j-1))+1]-values[mask+1])}
 vf<-var(eta);vu<-if(inherits(m,'glmmTMB'))as.numeric(VarCorr(m)$cond$id[1]) else as.numeric(VarCorr(m)$id[1]);ve<-if(inherits(m,c('glmerMod','glmmTMB')))pi^2/3 else sigma(m)^2
 stopifnot(abs(sum(shap)-vf)<1e-8,all(shap> -1e-10));tot<-vf+vu+ve
 setNames(c(shap,vu,ve)/tot,c(names(b),'Teacher_residual','Decision_residual'))
}
main<-partition(fit,z)
diagnostic<-function(m,d,label){eta<-predict(m,re.form=NA);tmb<-inherits(m,'glmmTMB');v<-if(tmb)as.numeric(VarCorr(m)$cond$id[1]) else as.numeric(VarCorr(m)$id[1]);e<-if(tmb||inherits(m,'glmerMod'))pi^2/3 else sigma(m)^2;data.frame(model=label,n_decisions=nrow(d),n_teachers=n_distinct(d$id),fixed_variance=var(eta),teacher_variance=v,residual_variance=e,marginal_r2=var(eta)/(var(eta)+v+e),conditional_r2=(var(eta)+v)/(var(eta)+v+e),singular=if(tmb)v<1e-8 else isSingular(m),convergence=if(tmb){if(m$fit$convergence==0&&m$sdr$pdHess)'' else m$fit$message}else paste(unlist(m@optinfo$conv$lme4$messages),collapse=';'),outside_01=if(tmb||inherits(m,'glmerMod'))NA else mean(predict(m)<0|predict(m)>1))}
diagnostics<-list(diagnostic(fit,z,'Linear main'))
points<-list(data.frame(model='Linear main',component=names(main),share=main))
for(kind in c('At least eight','Additive')){
 zz<-if(kind=='At least eight')z[z$n_cards>=8,] else z;bb<-blocks;if(kind=='Additive')bb$Student<-attrs
 ff<-as.formula(paste('y~',paste(unlist(bb),collapse='+'),'+(1|id)'));mm<-lmer(ff,zz,REML=TRUE,control=ctrl)
 pp<-partition(mm,zz,bb);points[[length(points)+1]]<-data.frame(model=kind,component=names(pp),share=pp);diagnostics[[length(diagnostics)+1]]<-diagnostic(mm,zz,kind)
}
message('Linear partitions completed')
zg<-z
for(v in o$x3)if(is.numeric(zg[[v]])&&length(unique(zg[[v]]))>5)zg[[v]]<-as.numeric(scale(zg[[v]]))
suppressPackageStartupMessages(library(glmmTMB))
gm<-glmmTMB(f,zg,family=binomial())
stopifnot(gm$fit$convergence==0,gm$sdr$pdHess)
message('Logistic Laplace fit completed')
pg<-partition(gm,zg);points[[length(points)+1]]<-data.frame(model='Logit latent scale',component=names(pg),share=pg);diagnostics[[length(diagnostics)+1]]<-diagnostic(gm,zg,'Logit latent scale')
export(bind_rows(points),'variance_sensitivity');export(bind_rows(diagnostics),'variance_diagnostics')
saveRDS(list(linear=fit,logit=gm,blocks=blocks),file.path(out,'reports/variance_models.rds'))
# Nonparametric teacher bootstrap: duplicated teachers receive distinct cluster identifiers.
set.seed(92704);B<-399;idx<-split(seq_len(nrow(z)),z$id);boots<-matrix(NA_real_,B,length(main));colnames(boots)<-names(main);status<-list()
for(b in seq_len(B)){
 draw<-sample(seq_along(idx),length(idx),replace=TRUE);rr<-unlist(idx[draw],use.names=FALSE);zz<-z[rr,];zz$id<-factor(rep(seq_along(draw),lengths(idx[draw])))
 ans<-tryCatch({mm<-lmer(f,zz,REML=TRUE,control=ctrl);list(p=partition(mm,zz),singular=isSingular(mm),msg=paste(unlist(mm@optinfo$conv$lme4$messages),collapse=';'))},error=function(e)list(p=rep(NA_real_,length(main)),singular=NA,msg=conditionMessage(e)))
 boots[b,]<-ans$p;status[[b]]<-data.frame(draw=b,singular=ans$singular,message=ans$msg)
 if(b%%25==0)message('Variance bootstrap ',b,'/',B)
}
export(bind_rows(status),'variance_bootstrap_status');export(as.data.frame(boots),'variance_bootstrap_draws')
stopifnot(sum(complete.cases(boots))>=.95*B)
export(data.frame(component=names(main),share=main,lo=apply(boots,2,quantile,.025,na.rm=TRUE),hi=apply(boots,2,quantile,.975,na.rm=TRUE),successful=sum(complete.cases(boots)),replicates=B),'variance_partition')
capture.output(sessionInfo(),file=file.path(out,'reports/R_session_variance.txt'))
message('Variance partition finished')
