# Targeted exploratory check authorized 2026-09-25. Does not change paper outputs.
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(sandwich)})
out <- '3. Output/resource_measure_check_2026-09-25'
dir.create(out, recursive=TRUE, showWarnings=FALSE)
export <- function(x,n) write.csv(x,file.path(out,paste0(n,'.csv')),row.names=FALSE,na='')
o <- readRDS('3. Output/two_papers_2026-09-21/reports/analysis_objects.rds')
d <- o$t
suffix <- c('estudiantes','pasar_sin_competencias','preparados_nivel_sig','demasiados_recursos_repetidores','recursos_repetidores_ineficaces')
audit <- bind_rows(lapply(suffix,function(s){a<-d[[paste0('impacto_centro_',s)]];b<-d[[paste0('impacto_region_',s)]];data.frame(item=s,centre=sum(!is.na(a)),region=sum(!is.na(b)),both=sum(!is.na(a)&!is.na(b)),neither=sum(is.na(a)&is.na(b)))}))
export(audit,'routing_audit')
d$frame <- factor(ifelse(!is.na(d$impacto_centro_estudiantes),'Centre',ifelse(!is.na(d$impacto_region_estudiantes),'Region',NA)))
d$pass_unprepared <- coalesce(d$impacto_centro_pasar_sin_competencias,d$impacto_region_pasar_sin_competencias)
d$unprepared <- 10-coalesce(d$impacto_centro_preparados_nivel_sig,d$impacto_region_preparados_nivel_sig)
d$low_impact <- 10-d$impact
items <- c('resources_excess','resources_ineffective','pass_unprepared','unprepared','low_impact')
stopifnot(nrow(d)==2705,all(vapply(d[,items],function(x)all(is.na(x)|x>=0&x<=10),logical(1))))
export(bind_rows(lapply(items,function(v){x<-d[[v]];data.frame(item=v,n=sum(!is.na(x)),mean=mean(x,na.rm=TRUE),sd=sd(x,na.rm=TRUE),pct_zero=100*mean(x==0,na.rm=TRUE),pct_ten=100*mean(x==10,na.rm=TRUE))})),'item_descriptives')
scales <- list(two=items[1:2],three=items[1:3],four=items[1:4],five=items)
diagnostics<-list();loads<-list()
for(pop in c('All','Centre','Region')) for(s in names(scales)){
 z<-d[if(pop=='All')rep(TRUE,nrow(d))else !is.na(d$frame)&d$frame==pop,scales[[s]],drop=FALSE];z<-z[complete.cases(z),,drop=FALSE]
 R<-cor(z);k<-ncol(z);r<-mean(R[upper.tri(R)]);ev<-eigen(R,symmetric=TRUE)
 diagnostics[[length(diagnostics)+1]]<-data.frame(population=pop,scale=s,n=nrow(z),k=k,alpha_standardized=k*r/(1+(k-1)*r),alpha_raw=k/(k-1)*(1-sum(diag(cov(z)))/sum(cov(z))),mean_r=r,min_r=min(R[upper.tri(R)]),eigen1=ev$values[1],eigen2=ev$values[2],pc1_share=ev$values[1]/k)
 if(pop=='All'){
  export(data.frame(item=rownames(R),R,check.names=FALSE),paste0('correlation_',s))
  export(data.frame(item=colnames(z),cor(z,method='spearman'),check.names=FALSE),paste0('spearman_',s))
  if(k>=3){fa<-factanal(z,1);loads[[length(loads)+1]]<-data.frame(scale=s,item=rownames(fa$loadings),loading=as.vector(fa$loadings),uniqueness=fa$uniquenesses,model_df=fa$dof,fit_p=if(is.null(fa$PVAL))NA else fa$PVAL)}
 }
}
export(bind_rows(diagnostics),'scale_diagnostics');export(bind_rows(loads),'one_factor_loadings')
# Same standardization population as current composite, not model-specific scaling.
for(v in items)d[[paste0(v,'_z')]]<-as.numeric(scale(d[[v]]))
for(s in names(scales)) d[[paste0('scale_',s)]]<-as.numeric(scale(rowMeans(d[,paste0(scales[[s]],'_z'),drop=FALSE])))
stopifnot(max(abs(d$scale_two-d$resources),na.rm=TRUE)<1e-12)
specs <- list(X2=character(),Current_two='resources',Excess='resources_excess_z',Ineffective='resources_ineffective_z',Both=c('resources_excess_z','resources_ineffective_z'),Expanded_three='scale_three',Expanded_four='scale_four')
tidy <- function(m,pop,model,y){V<-vcovHC(m,type='HC3');b<-coef(m);se<-sqrt(diag(V));crit<-qt(.975,df.residual(m));data.frame(population=pop,model=model,outcome=y,term=names(b),estimate=b,se=se,lo=b-crit*se,hi=b+crit*se,p=2*pt(-abs(b/se),df.residual(m)),n=nobs(m),r2=summary(m)$r.squared,adjusted_r2=summary(m)$adj.r.squared,row.names=NULL)}
common<-d[complete.cases(d[,o$x3]),];wide<-d[complete.cases(d[,c(o$x2,items)]),]
stopifnot(nrow(common)==2156)
results<-list()
for(pop in c('Current_common','Expanded_common','Available','Current_common_frame_cell'))for(s in names(specs))for(y in c('hb','ha','hc')){
 z<-switch(pop,Current_common=common,Expanded_common=wide,Available=d,Current_common_frame_cell=common)
 x<-c(o$x2,specs[[s]],if(pop=='Current_common_frame_cell')c('frame','cell')else character())
 m<-lm(reformulate(x,y),z)
 results[[length(results)+1]]<-tidy(m,pop,s,y)
}
res<-bind_rows(results);export(res,'regressions_all_terms')
export(filter(res,term%in%c('resources',paste0(items,'_z'),'scale_three','scale_four')),'regressions_resource_terms')
old<-read.csv('3. Output/two_papers_2026-09-22/tables/characterization.csv')
check<-merge(filter(res,population=='Current_common',model=='Current_two',outcome=='hb'),filter(old,model=='Resources_common'),by='term')
stopifnot(nrow(check)==length(coef(lm(reformulate(o$x3,'hb'),common))),max(abs(check$estimate.x-check$estimate.y))<1e-10)
# Separate item estimates in original 0--10 units for interpretation.
export(bind_rows(lapply(c('resources_excess','resources_ineffective'),function(v)tidy(lm(reformulate(c(o$x2,v),'hb'),common),'Current_common',v,'hb'))),'regressions_raw_units')
# Paired repeated teacher folds. Rebuild card references and scaling inside training.
# No tuning is needed for these OLS models. This is exploratory model comparison.
pred<-list();counter<-1
for(pop in c('Current_common','Expanded_common')){
 z<-if(pop=='Current_common')common else wide
 use_specs<-if(pop=='Current_common')specs[1:5]else specs
 for(re in 1:5){set.seed(92230+re);fold<-sample(rep(1:5,length.out=nrow(z)))
  for(k in 1:5){tr<-z[fold!=k,];te<-z[fold==k,];lr<-o$long[o$long$id%in%tr$id,];le<-o$long[o$long$id%in%te$id,]
   ref<-lr%>%group_by(card)%>%summarise(s=sum(y),n=n(),.groups='drop')
   stopifnot(all(le$card%in%ref$card),min(ref$n)>1)
   ht<-left_join(lr,ref,by='card')%>%group_by(id)%>%summarise(target=mean(y-(s-y)/(n-1)),.groups='drop')
   he<-left_join(le,ref,by='card')%>%group_by(id)%>%summarise(target=mean(y-s/n),.groups='drop')
   tr<-left_join(tr,ht,by='id');te<-left_join(te,he,by='id')
   for(v in items){mu<-mean(tr[[v]],na.rm=TRUE);sig<-sd(tr[[v]],na.rm=TRUE);tr[[paste0(v,'_z')]]<-(tr[[v]]-mu)/sig;te[[paste0(v,'_z')]]<-(te[[v]]-mu)/sig}
   for(s in names(scales)){a<-rowMeans(tr[,paste0(scales[[s]],'_z'),drop=FALSE]);b<-rowMeans(te[,paste0(scales[[s]],'_z'),drop=FALSE]);mu<-mean(a,na.rm=TRUE);sig<-sd(a,na.rm=TRUE);tr[[paste0('scale_',s)]]<-(a-mu)/sig;te[[paste0('scale_',s)]]<-(b-mu)/sig}
   tr$resources<-tr$scale_two;te$resources<-te$scale_two
   for(s in names(use_specs)){m<-lm(reformulate(c(o$x2,use_specs[[s]]),'target'),tr);p<-predict(m,te);stopifnot(!anyNA(p));pred[[counter]]<-data.frame(population=pop,repeat_id=re,fold=k,model=s,n=nrow(te),sse=sum((te$target-p)^2),baseline_sse=sum((te$target-mean(tr$target))^2));counter<-counter+1}
  }
 }
}
pr<-bind_rows(pred);export(pr,'cv_fold_errors')
met<-pr%>%group_by(population,repeat_id,model)%>%summarise(n=sum(n),r2=1-sum(sse)/sum(baseline_sse),rmse=sqrt(sum(sse)/sum(n)),.groups='drop')
export(met,'cv_by_repeat');export(met%>%group_by(population,model)%>%summarise(n=first(n),r2_min=min(r2),r2_max=max(r2),r2=mean(r2),rmse=mean(rmse),.groups='drop'),'cv_summary')
stopifnot(nrow(met)==60,all(met$n==2156))
capture.output(warnings(),file=file.path(out,'warnings.txt'))
capture.output(sessionInfo(),file=file.path(out,'R_session.txt'))
export(data.frame(input=c('3. Output/two_papers_2026-09-21/reports/analysis_objects.rds','2. Code/23. resource_measure_check.R'),md5=unname(tools::md5sum(c('3. Output/two_papers_2026-09-21/reports/analysis_objects.rds','2. Code/23. resource_measure_check.R')))),'input_manifest')
cat('Completed. Current common:',nrow(common),' Expanded common:',nrow(wide),'\n')
print(filter(bind_rows(diagnostics),population=='All'))
print(filter(res,population=='Current_common',outcome=='hb',term%in%c('resources','resources_excess_z','resources_ineffective_z','scale_three','scale_four')))
