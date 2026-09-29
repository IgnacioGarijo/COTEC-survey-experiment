# Authorized integrated revision, 27 September 2026. Historical objects unchanged.
options(stringsAsFactors=FALSE);invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(fixest);library(sandwich);library(ggplot2);library(jsonlite)})
setFixest_nthreads(2)
out<-'3. Output/paper_A'
for(z in c('tables','figures','reports'))dir.create(file.path(out,z),recursive=TRUE,showWarnings=FALSE)
o<-readRDS('3. Output/shared/reports/analysis_objects.rds')
d<-readRDS('3. Output/paper_B/reports/deepened_objects.rds')
t<-d$t;l<-d$long
x1<-setdiff(o$x1,'antiguedad3');x2<-c(x1,'impact','empathy','meritocracia');x3<-c(x2,'resources_excess')
cc<-t[complete.cases(t[,x3]),];oldcc<-t[complete.cases(t[,o$x3]),]
export<-function(x,n)if (n %in% jsonlite::read_json('2. Code/artifact_manifest.json')$paper_A$tables) write.csv(x,file.path(out,'tables',paste0(n,'.csv')),row.names=FALSE,na='')
tidy<-function(m,label){V<-vcovHC(m,type='HC3');b<-coef(m);s<-sqrt(diag(V));data.frame(model=label,term=names(b),estimate=b,se=s,lo=b-qt(.975,df.residual(m))*s,hi=b+qt(.975,df.residual(m))*s,n=nobs(m),r2=summary(m)$r.squared,row.names=NULL)}
fits<-list(X1_common=lm(reformulate(x1,'hb'),cc),X2_common=lm(reformulate(x2,'hb'),cc),X3_common=lm(reformulate(x3,'hb'),cc),X1_available=lm(reformulate(x1,'hb'),t))
export(bind_rows(lapply(names(fits),function(n)tidy(fits[[n]],n))),'characterization')
alt<-cc[complete.cases(cc[,c('ha','hb','hc','hd')]),]
export(bind_rows(lapply(c('ha','hb','hc','hd'),function(y)tidy(lm(reformulate(x3,y),alt),y))),'four_outcomes')
export(data.frame(variable=x3,n_observed=sapply(t[,x3],function(z)sum(!is.na(z))),n_total=nrow(t)),'covariate_availability')
export(t%>%group_by(tercile)%>%summarise(n=n(),mean_hb=mean(hb),mean_ha=mean(ha),age=mean(edad,na.rm=TRUE),experience=mean(experiencia,na.rm=TRUE),secondary=mean(level=='Secondary',na.rm=TRUE),female=mean(female,na.rm=TRUE),effort=mean(meritocracia,na.rm=TRUE),resources_excess=mean(resources_excess,na.rm=TRUE),.groups='drop'),'tercile_descriptives')
# Reuse only unchanged analyses, preserving traceable filenames.
for(n in c('attributes','attributes_by_tercile','card_disagreement','academic_categories','academic_interactions','sequence_summary','sequence_lag_permutation')){
 src<-file.path('3. Output/paper_B/tables',paste0(n,'.csv'))
 if(!file.exists(src))src<-file.path('3. Output/shared/tables',paste0(n,'.csv'))
 stopifnot(file.copy(src,file.path(out,'tables'),overwrite=TRUE))
}
ggsave(file.path(out,'figures/harshness_distribution.png'),ggplot(t,aes(100*hb))+geom_histogram(binwidth=5,fill='#215e70',color='white')+labs(x='Harshness (percentage points)',y='Teachers')+theme_minimal(base_size=12),width=7.5,height=3.7,dpi=220)
summary<-list(n_teachers=nrow(t),n_decisions=nrow(l),n_common=nrow(cc),n_x1=sum(complete.cases(t[,x1])),n_alt=nrow(alt),hb_mean=mean(t$hb),hb_sd=sd(t$hb),hb_quantiles=quantile(t$hb,c(.1,.25,.5,.75,.9)),tercile_cutoffs=quantile(t$hb,c(1/3,2/3)),n_control=sum(t$arm=='Control'),old_common=nrow(oldcc))
write_json(summary,file.path(out,'reports/summary.json'),auto_unbox=TRUE,pretty=TRUE)
saveRDS(list(t=t,long=l,rawt=o$rawt,x1=x1,x2=x2,x3=x3,common=cc,summary=summary),file.path(out,'reports/analysis_objects.rds'))
# Cluster bootstrap: recalculate same-card references in every teacher resample.
set.seed(92701);B<-999;idx<-match(l$id,t$id);cidx<-as.integer(factor(l$card));bt<-matrix(NA_real_,B,length(coef(fits$X3_common)));colnames(bt)<-names(coef(fits$X3_common))
for(b in seq_len(B)){
 draw<-sample.int(nrow(t),replace=TRUE);w<-tabulate(draw,nbins=nrow(t));lw<-w[idx]
 sm<-rowsum(l$y*lw,cidx);ct<-rowsum(lw,cidx)
 ref<-(as.numeric(sm)[cidx]-l$y)/(as.numeric(ct)[cidx]-1)
 h<-tapply(l$y-ref,idx,mean);tb<-t[draw,];tb$hb<-as.numeric(h[as.character(draw)])
 z<-tb[complete.cases(tb[,x3]),];bt[b,]<-coef(lm(reformulate(x3,'hb'),z))
}
export(data.frame(term=colnames(bt),lo=apply(bt,2,quantile,.025),hi=apply(bt,2,quantile,.975),se=apply(bt,2,sd),replicates=B),'characterization_bootstrap')
message('Covariates, models and bootstrap completed; common sample: ',nrow(cc))
# Repeated splits of responses, never splits of teachers as substitute for response halves.
# Five teacher folds. For each evaluation fold, disjoint teacher reference pools for A/B.
# This prevents the same estimated card baseline from mechanically entering both halves.
set.seed(92702);fold<-sample(rep(1:5,length.out=nrow(t)));reference_group<-sample(rep(1:2,length.out=nrow(t)))
refA<-refB<-numeric(nrow(l));allref<-list()
for(k in 1:5){
 eval<-which(fold[idx]==k)
 for(g in 1:2){z<-l[fold[idx]!=k&reference_group[idx]==g,];rr<-tapply(z$y,z$card,mean);nn<-table(z$card);stopifnot(all(l$card[eval]%in%names(rr)),min(nn)>1)
  if(g==1)refA[eval]<-rr[l$card[eval]] else refB[eval]<-rr[l$card[eval]]
  allref[[length(allref)+1]]<-data.frame(fold=k,pool=g,min_per_card=min(nn),teachers=n_distinct(z$id))
 }
}
export(bind_rows(allref),'split_reference_support')
ii<-split(seq_len(nrow(l)),idx);eligible<-which(t$n_cards>=8);correlations<-numeric(1000)
set.seed(92703)
for(b in 1:1000){
 h1<-h2<-numeric(length(eligible))
 for(a in seq_along(eligible)){j<-eligible[a];rows<-sample(ii[[as.character(j)]]);n1<-floor(length(rows)/2);if(length(rows)%%2==1)n1<-n1+rbinom(1,1,.5);ia<-rows[seq_len(n1)];ib<-rows[-seq_len(n1)];h1[a]<-mean(l$y[ia]-refA[ia]);h2[a]<-mean(l$y[ib]-refB[ib])}
 correlations[b]<-cor(h1,h2)
}
export(data.frame(partition=1:1000,correlation=correlations),'split_half_partitions')
export(data.frame(n_teachers=length(eligible),min_responses=8,partitions=1000,mean=mean(correlations),median=median(correlations),p025=quantile(correlations,.025),p975=quantile(correlations,.975),reference_seed=92702,partition_seed=92703),'split_half_summary')
ggsave(file.path(out,'figures/split_half.png'),ggplot(data.frame(r=correlations),aes(r))+geom_histogram(bins=30,fill='#215e70',color='white')+geom_vline(xintercept=0,linetype=2)+labs(x='Correlation between independently referenced response halves',y='Random partitions')+theme_minimal(base_size=11),width=7.7,height=3.6,dpi=220)
capture.output(sessionInfo(),file=file.path(out,'reports/R_session_core.txt'))
message('Core revision finished')
