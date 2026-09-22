options(stringsAsFactors=FALSE);Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8')
suppressPackageStartupMessages({library(arrow);library(dplyr);library(tidyr);library(fixest);library(sandwich)})
setFixest_nthreads(2);out<-'3. Output/two_papers_2026-09-21';o<-readRDS(file.path(out,'reports/analysis_objects.rds'));t<-o$t;l<-o$long;r<-o$rawt
raw<-as.data.frame(read_parquet('1. Data/raw data/df_named.parquet'))
export<-function(x,n)write.csv(x,file.path(out,'tables',paste0(n,'.csv')),row.names=FALSE)
tidy<-function(m,label){V<-vcovHC(m,type='HC3');b<-coef(m)[rownames(V)];s<-sqrt(diag(V));data.frame(model=label,term=names(b),estimate=b,se=s,lo=b-qt(.975,df.residual(m))*s,hi=b+qt(.975,df.residual(m))*s,p=2*pt(-abs(b/s),df.residual(m)),n=nobs(m),row.names=NULL)}
ri<-raw[match(r$id,raw$ResponseId),];r$favorite<-factor(ifelse(ri$R1_1==1,1,ifelse(ri$R1_2==1,2,ifelse(ri$R1_3==1,3,NA))),levels=1:3)
r$least<-factor(ifelse(ri$R1_1==3,1,ifelse(ri$R1_2==3,2,ifelse(ri$R1_3==3,3,NA))),levels=1:3)
r$alignment<-factor(ifelse(r$policy==r$favorite,'Favorite',ifelse(r$policy==r$least,'Least','Middle')),levels=c('Middle','Favorite','Least'))
ar<-list()
for(a in c('Policy','Revelation','Awareness'))for(y in c('score_observed','task_complete','survey_complete')){
 z<-filter(r,arm==a);z[[y]]<-as.numeric(z[[y]]);ar[[paste(a,y)]]<-tidy(lm(reformulate('policy',y),z),paste(a,y))
 if(a!='Policy'){z<-filter(z,!is.na(alignment));ar[[paste(a,y,'align')]]<-tidy(lm(reformulate(c('alignment','favorite'),y),z),paste(a,y,'pre-policy alignment'))}
}
export(bind_rows(ar)%>%group_by(model)%>%mutate(p_holm=p.adjust(p,'holm'))%>%ungroup(),'attrition_policy_alignment')
export(filter(r,arm%in%c('Revelation','Awareness'),!is.na(alignment))%>%group_by(arm,alignment)%>%summarise(n=n(),score_rate=mean(score_observed),completed=mean(survey_complete),.groups='drop'),'attrition_alignment_rates')
# Causal alignment contrast only uses rankings elicited before policy assignment.
al<-list()
for(a in c('Revelation','Awareness')){z<-filter(t,arm==a);al[[a]]<-tidy(lm(hb~alignment+favorite,z),paste(a,'pre-assignment ranking'))}
z<-filter(t,arm%in%c('Revelation','Awareness'));z$arm<-droplevels(z$arm)
m<-lm(hb~arm*alignment+favorite,z);al[['interaction']]<-tidy(m,'Awareness versus Revelation by alignment')
export(bind_rows(al),'alignment_preassignment')
V<-vcovHC(m,type='HC3');nn<-grep(':',names(coef(m)),value=TRUE);b<-coef(m)[nn];w<-drop(t(b)%*%solve(V[nn,nn],b));export(data.frame(test='Arm by pre-assignment alignment',chi2=w,df=length(nn),p=pchisq(w,length(nn),lower.tail=FALSE)),'alignment_joint_test')
# Symmetric total-duration trimming is a sensitivity, not an exclusion imposed on the main sample.
t$duration<-as.numeric(raw$`Duration (in seconds)`[match(t$id,raw$ResponseId)])
q<-quantile(t$duration,c(.0175,.9825),na.rm=TRUE);tt<-filter(t,duration>=q[1],duration<=q[2]);broad<-list(c('Policy','Control'),c('Revelation','Policy'),c('Awareness','Revelation'))
export(bind_rows(lapply(broad,function(pair){z<-filter(tt,arm%in%pair);z$arm<-relevel(droplevels(z$arm),pair[2]);tidy(lm(hb~arm,z),paste(pair,collapse=' - '))})),'policy_duration_trim')
export(data.frame(lower_seconds=q[1],upper_seconds=q[2],n=nrow(tt)),'duration_trim_definition')
# A lagged outcome in a short panel can be negatively biased by teacher demeaning.
# Reordering intact response-profile pairs reproduces that mechanical benchmark.
cols<-grep('^FL_[0-9]+_DO_Grupo[1-8]\\.[1-8]$',names(raw),value=TRUE)
ord<-raw%>%select(id=ResponseId,all_of(cols))%>%pivot_longer(-id,names_to='field',values_to='position')%>%filter(!is.na(position))%>%mutate(position=as.numeric(position),card=sub('^.*_DO_Grupo([1-8])\\.([1-8])$','alumno_\\1_\\2',field))
z<-inner_join(l,select(ord,id,card,position),by=c('id','card'))%>%group_by(id)%>%filter(n()==8,n_distinct(position)==8)%>%ungroup()%>%arrange(id,position)
lagfit<-function(z){z<-z%>%arrange(id,position)%>%group_by(id)%>%mutate(previous=lag(y))%>%ungroup()%>%filter(!is.na(previous));as.numeric(coef(feols(y~previous|id+card,z,cluster=~id,notes=FALSE))['previous'])}
actual<-lagfit(z);set.seed(929);perm<-numeric(199)
for(b in 1:199){zp<-z%>%group_by(id)%>%mutate(position=sample(position))%>%ungroup();perm[b]<-lagfit(zp)}
export(data.frame(observed=actual,permutation_mean=mean(perm),lo=quantile(perm,.025),hi=quantile(perm,.975),p=(1+sum(abs(perm-mean(perm))>=abs(actual-mean(perm))))/200,n_teachers=n_distinct(z$id),replicates=199),'sequence_lag_permutation')
# Joint nulls for the three policy identities within each arm.
joints<-list()
for(a in c('Policy','Revelation','Awareness')){z<-filter(t,arm==a);m<-lm(hb~policy,z);nn<-grep('^policy',names(coef(m)),value=TRUE);b<-coef(m)[nn];V<-vcovHC(m,type='HC3')[nn,nn];w<-drop(t(b)%*%solve(V,b));joints[[a]]<-data.frame(arm=a,chi2=w,df=2,p=pchisq(w,2,lower.tail=FALSE))}
export(bind_rows(joints)%>%mutate(p_holm=p.adjust(p,'holm')),'policy_identity_joint_tests')
# Ranking outcomes use every retained teacher with a ranking, irrespective of score availability.
pref<-filter(r,retained,!is.na(favorite),!is.na(least));pf<-list()
for(k in 1:3)for(side in c('favorite','least')){z<-filter(pref,arm=='Policy');z$selected<-as.integer(z[[side]]==as.character(k));z$assigned_here<-as.integer(z$policy==as.character(k));pf[[paste(k,side)]]<-tidy(lm(selected~assigned_here,z),paste('Policy',k,side))%>%filter(term=='assigned_here')}
export(bind_rows(pf)%>%mutate(p_holm=p.adjust(p,'holm')),'preference_effects_all_rankings')
export(pref%>%group_by(arm,favorite)%>%summarise(n=n(),.groups='drop')%>%group_by(arm)%>%mutate(share=n/sum(n),total=sum(n))%>%ungroup(),'preference_shares_all_rankings')
pa<-list()
for(pair in broad)for(k in 1:3){z<-filter(pref,arm%in%pair);z$arm<-relevel(droplevels(z$arm),pair[2]);z$selected<-as.integer(z$favorite==as.character(k));pa[[paste(pair[1],k)]]<-tidy(lm(selected~arm,z),paste(paste(pair,collapse=' - '),'favorite',k))%>%filter(term!='(Intercept)')}
export(bind_rows(pa)%>%mutate(p_holm=p.adjust(p,'holm')),'preference_arm_contrasts')
message('Supplementary sensitivity analyses complete.')

