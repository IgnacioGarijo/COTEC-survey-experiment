# Read-only extraction from cached fits; no models are fitted or overwritten.
options(stringsAsFactors=FALSE,width=140)
suppressPackageStartupMessages({library(lme4);library(jsonlite)})
O<-'3. Output/sensitivity_presentation_review';L<-'3. Output/paper_A_final_revision/local'
attrs<-c('male','background','failed','competence','absent','disruptive')
write<-function(x,n)write.csv(x,file.path(O,paste0(n,'.csv')),row.names=FALSE,na='')
files<-c(Diagonal='candidate_B_diagonal_bobyqa.rds',Pairs='candidate_B_domains_bobyqa.rds',Full='candidate_B_full_bobyqa.rds',Eight='candidate_B_full8_diagonal_nloptwrap.rds',NoShared='candidate_C_diagonal_nloptwrap.rds',NoScreen='candidate_NoScreen_diagonal_nloptwrap.rds',ByID='candidate_ByID_diagonal_nloptwrap.rds')
fits<-lapply(files,function(f)readRDS(file.path(L,f))$model)
mat<-function(m){M<-matrix(0,6,6,dimnames=list(paste0(attrs,'_c'),paste0(attrs,'_c')));for(S in VarCorr(m)){n<-colnames(S);M[n,n]<-as.matrix(S)};M}
ss<-do.call(rbind,lapply(names(fits),function(n){m<-fits[[n]];M<-mat(m);stopifnot(all(is.finite(M)),all(diag(M)>=0));data.frame(specification=n,attribute=attrs,sd_pp=100*sqrt(diag(M)),variance=diag(M),observations=nobs(m),singular=isSingular(m))}))
write(ss,'sensitivity_sds')
cc<-do.call(rbind,lapply(c('Pairs','Full'),function(n){M<-mat(fits[[n]]);C<-cov2cor(M);ix<-which(upper.tri(M),arr.ind=TRUE);data.frame(specification=n,attribute_1=attrs[ix[,1]],attribute_2=attrs[ix[,2]],covariance=M[ix],correlation=C[ix])}))
write(cc,'sensitivity_correlations')
audit<-do.call(rbind,lapply(names(fits),function(n){m<-fits[[n]];M<-mat(m);data.frame(specification=n,n=nobs(m),AIC=AIC(m),covariance_parameters=length(getME(m,'theta')),singular=isSingular(m),min_eigen=min(eigen(M,symmetric=TRUE)$values),outside01=mean(predict(m)<0|predict(m)>1))}));write(audit,'fit_diagnostics')
# Read existing full-eight tercile coefficients, not new regressions.
tab<-read.csv('3. Output/paper_A_final_revision/B_full8/attributes_by_tercile.csv');stopifnot(nrow(tab)==18);write(tab,'tercile_coefficients_full8')
# The current bootstrap records shares and diagnostics, not per-attribute variances.
boot<-read.csv('3. Output/paper_A_final_revision/B/decomposition_bootstrap_draws.csv')
stopifnot(nrow(boot)==199,all(boot$accepted),!any(grepl('sd_|variance_',names(boot))))
manifest<-list(input_hashes=as.list(tools::md5sum(c(file.path(L,files),'3. Output/paper_A_final_revision/B_full8/attributes_by_tercile.csv','3. Output/paper_A_final_revision/B/decomposition_bootstrap_draws.csv'))),script_md5=unname(tools::md5sum('2. Code/sensitivity_presentation_review/extract.R')),fits_reestimated=FALSE,SD_unit='percentage-point change in sensitivity to a 0/1 student attribute; latent model-implied dispersion, not SE or raw teacher-estimate SD',uncertainty='Not estimated here. Existing bootstrap saves aggregate shares only; cannot reuse their SE as SD uncertainty.',comparison_scope='B full/pairs/diagonal on same17312 decisions; Eight is updated reference sensitivity on same2164 complete teachers; all subject to response-derived harshness working model.')
write_json(manifest,file.path(O,'audit.json'),pretty=TRUE,auto_unbox=TRUE)
print(reshape(ss[,1:3],idvar='attribute',timevar='specification',direction='wide'),row.names=FALSE)
print(cc[cc$attribute_1=='failed'&cc$attribute_2=='competence'|cc$attribute_1=='absent'&cc$attribute_2=='disruptive',],row.names=FALSE)
print(tab[tab$term%in%c('failed','competence'),c('model','term','estimate','se','teachers')],row.names=FALSE)
