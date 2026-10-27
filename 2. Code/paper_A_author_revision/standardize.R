# Uniform academic comparison on common support, updated Main2520 cohort.
options(stringsAsFactors=FALSE);suppressPackageStartupMessages(library(jsonlite))
O<-'3. Output/paper_A_author_revision';o<-readRDS(file.path(O,'local/scenario_Main.rds'));z<-as.data.frame(o$long)
attrs<-c('male','background','failed','competence','absent','disruptive');others<-c('male','background','absent','disruptive');pr<-readRDS('3. Output/paper_A_final_revision/local/provenance.rds');d<-unique(as.data.frame(pr$ordered)[,c('profile',attrs)]);d$u<-apply(d[,others],1,paste0,collapse='');d$fc<-paste0(d$failed,d$competence)
us<-sort(unique(d$u));support<-us[vapply(us,function(u)length(unique(d$fc[d$u==u]))==4,logical(1))];stopifnot(length(support)==12,sum(d$u%in%support)==48)
z$u<-apply(z[,others],1,paste0,collapse='');z$fc<-paste0(z$failed,z$competence);z<-z[z$u%in%support,];levels<-c('00','01','10','11');keys<-as.vector(outer(support,levels,paste,sep='_'))
ids<-sort(unique(z$id));idx<-match(z$id,ids);k<-match(paste(z$u,z$fc,sep='_'),keys);stopifnot(!anyNA(k),!anyDuplicated(z[,c('id','profile')]))
N<-Y<-matrix(0,length(ids),48);N[cbind(idx,k)]<-1;Y[cbind(idx,k)]<-z$y
stat<-function(w){n<-as.numeric(crossprod(w,N));r<-as.numeric(crossprod(w,Y));stopifnot(all(n>1));c(colMeans(matrix(r/n,nrow=12)),colMeans(matrix(2*r*(n-r)/(n*(n-1)),nrow=12)))}
point<-stat(rep(1,length(ids)));set.seed(2611001);B<-1999;draws<-t(replicate(B,stat(tabulate(sample.int(length(ids),replace=TRUE),nbins=length(ids)))))
contr<-rbind(rate_conflict=c(0,-1,1,0,0,0,0,0),disagreement_conflict=c(0,0,0,0,0,-1,1,0),rate_complementarity=c(1,-1,-1,1,0,0,0,0));delta<-as.numeric(contr%*%point);bd<-draws%*%t(contr);se<-apply(bd,2,sd);stopifnot(all(se>0));critical<-quantile(apply(abs(sweep(sweep(bd,2,delta),2,se,'/')),1,max),.95)
a<-data.frame(metric=rep(c('rate','disagreement'),each=4),fc=rep(levels,2),estimate=point,se=apply(draws,2,sd),lo=apply(draws,2,quantile,.025),hi=apply(draws,2,quantile,.975),teachers=length(ids),decisions=nrow(z))
c<-data.frame(contrast=rownames(contr),estimate=delta,se=se,sim_lo=delta-critical*se,sim_hi=delta+critical*se,draws=B)
write.csv(a,file.path(O,'tables/standardized_profiles.csv'),row.names=FALSE);write.csv(c,file.path(O,'tables/standardized_contrasts.csv'),row.names=FALSE);write.csv(d,file.path(O,'tables/standardized_support.csv'),row.names=FALSE)
saveRDS(draws,file.path(O,'local/standardized_draws.rds'));write_json(list(main_teachers=2520,supported_teachers=length(ids),decisions=nrow(z),support_cells=12,profiles=48,seed=2611001,draws=B,critical=critical,missing_combinations='absent=disruptive=1',input_md5=unname(tools::md5sum(file.path(O,'local/scenario_Main.rds')))),file.path(O,'diagnostics/standardization.json'),pretty=TRUE,auto_unbox=TRUE)
print(a);print(c)
