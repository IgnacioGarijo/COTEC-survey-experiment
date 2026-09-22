# New analyses for the two-manuscript revision. Originals are never overwritten.
# Run from the repository root with Rscript --vanilla.
options(stringsAsFactors=FALSE, width=140)
Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8')
suppressPackageStartupMessages({library(arrow);library(dplyr);library(tidyr);library(fixest);library(sandwich);library(ggplot2);library(jsonlite)})
set.seed(21092026)
setFixest_nthreads(2)
out <- '3. Output/two_papers_2026-09-21'
for (z in c('tables','figures','reports')) dir.create(file.path(out,z),recursive=TRUE,showWarnings=FALSE)
export <- function(x,name) write.csv(x,file.path(out,'tables',paste0(name,'.csv')),row.names=FALSE,na='')
num <- function(x) suppressWarnings(as.numeric(as.character(x)))
saveplot <- function(p,n,w=8,h=5) ggsave(file.path(out,'figures',paste0(n,'.png')),p,width=w,height=h,dpi=220,bg='white')
theme_set(theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),plot.title.position='plot'))
df <- as.data.frame(read_parquet('1. Data/processed data/cleandata.parquet'))
raw <- as.data.frame(read_parquet('1. Data/raw data/df_named.parquet'))
stopifnot(!anyDuplicated(df$id),!anyDuplicated(raw$ResponseId))
for (e in parse('2. Code/0. main.R',encoding='UTF-8')) {
  if(is.call(e) && identical(e[[1]],as.name('<-')) && grepl('^vector_',as.character(e[[2]]))) eval(e)
}
cards <- as.vector(outer(1:8,1:8,function(x,y) paste0('alumno_',x,'_',y)))
used <- setdiff(cards,c('alumno_1_1','alumno_2_1'))
attrs <- c('male','background','failed','competence','absent','disruptive')
design <- data.frame(card=cards,male=as.integer(cards %in% vector_niño),background=as.integer(cards %in% vector_extranjero),failed=as.integer(cards %in% vector_suspensos),competence=as.integer(cards %in% vector_carencias),absent=as.integer(cards %in% vector_absentista),disruptive=as.integer(cards %in% vector_expulsion))
long <- df %>% select(id,all_of(used)) %>% pivot_longer(-id,names_to='card',values_to='decision') %>% filter(!is.na(decision)) %>% mutate(y=as.integer(decision=='repite')) %>% left_join(design,by='card')
stopifnot(all(long$decision %in% c('pasa','repite')))
long <- long %>% group_by(card) %>% mutate(loo=(sum(y)-y)/(n()-1),deviation=y-loo) %>% ungroup()
t <- long %>% group_by(id) %>% summarise(hb=mean(deviation),ha=mean(y),n_cards=n(),.groups='drop') %>% left_join(df,by='id')
t <- t %>% mutate(
  arm=factor(case_when(num(treatment)==1~'Control',num(treatment)%in%2:4~'Policy',num(treatment)%in%5:7~'Revelation',num(treatment)%in%8:10~'Awareness'),levels=c('Control','Policy','Revelation','Awareness')),
  policy=factor(ifelse(arm=='Control',NA,num(politica)),levels=1:3),
  cell=factor(ifelse(arm=='Control','Control',paste(arm,policy,sep='_')),levels=c('Control',paste(rep(c('Policy','Revelation','Awareness'),each=3),rep(1:3,3),sep='_'))),
  level=factor(ifelse(nivel=='E. Primaria','Primary',ifelse(nivel=='E. Secundaria','Secondary',NA)),levels=c('Primary','Secondary')),
  female=num(female),public=ifelse(is.na(titularidad),NA,as.integer(titularidad=='Pública')),
  public_permanent=as.integer(sitlabpub %in% c('Contratada/o laboral indefinido','Funcionaria/o con destino definitivo')),
  impact=coalesce(impacto_centro_estudiantes,impacto_region_estudiantes),
  empathy=coalesce(empatia_escala_1a5,ceiling(empatia_escala_0a100_t1/20),ceiling(empatia_escala_0a100_t2/20),ceiling(empatia_escala_0a100_t3/20)),
  empathy=ifelse(empathy==0,1,empathy),
  resources_excess=coalesce(impacto_centro_demasiados_recursos_repetidores,impacto_region_demasiados_recursos_repetidores),
  resources_ineffective=coalesce(impacto_centro_recursos_repetidores_ineficaces,impacto_region_recursos_repetidores_ineficaces),
  resources=as.numeric(scale((as.numeric(scale(resources_excess))+as.numeric(scale(resources_ineffective)))/2)),
  favorite=factor(case_when(orden_pref_refuerzo==1~'1',orden_pref_criterios_promo==1~'2',orden_pref_formacion_prof==1~'3'),levels=1:3),
  least=factor(case_when(orden_pref_refuerzo==3~'1',orden_pref_criterios_promo==3~'2',orden_pref_formacion_prof==3~'3'),levels=1:3),
  alignment=factor(case_when(arm=='Control'~'Control',policy==favorite~'Favorite',policy==least~'Least',!is.na(favorite)&!is.na(least)~'Middle'),levels=c('Control','Middle','Favorite','Least')))
stopifnot(nrow(t)==2705,nrow(long)==22124,!anyNA(t$arm))
long <- long %>% left_join(t %>% select(id,arm,policy,cell,level,edad,experiencia,meritocracia,resources),by='id')
x1 <- c('level','experiencia','antiguedad3','female','edad','public','public_permanent','grupos_docencia')
x2 <- c(x1,'impact','empathy','meritocracia'); x3 <- c(x2,'resources')
fm <- function(y,x) reformulate(x,response=y)
tidy_lm <- function(m,label) {
  V<-vcovHC(m,type='HC3'); b<-coef(m)[rownames(V)]; se<-sqrt(diag(V)); d<-df.residual(m)
  data.frame(model=label,term=names(b),estimate=unname(b),se=se,p=2*pt(-abs(b/se),d),lo=b-qt(.975,d)*se,hi=b+qt(.975,d)*se,n=nobs(m),r2=summary(m)$r.squared,row.names=NULL)
}
tidy_fe <- function(m,label) {
  a<-as.data.frame(coeftable(m));ci<-confint(m)
  data.frame(model=label,term=rownames(a),estimate=a[[1]],se=a[[2]],p=a[[4]],lo=ci[,1],hi=ci[,2],n=nobs(m),row.names=NULL)
}
contrast <- function(m,weights,label,family='') {
  V<-vcovHC(m,type='HC3');b<-coef(m)[rownames(V)];L<-setNames(rep(0,length(b)),names(b)); stopifnot(all(names(weights)%in%names(b)));L[names(weights)]<-weights
  e<-sum(L*b);se<-sqrt(drop(t(L)%*%V%*%L));d<-df.residual(m)
  data.frame(family=family,contrast=label,estimate=e,se=se,p=2*pt(-abs(e/se),d),lo=e-qt(.975,d)*se,hi=e+qt(.975,d)*se,lo90=e-qt(.95,d)*se,hi90=e+qt(.95,d)*se,mde=(qt(.975,d)+qt(.8,d))*se,n=nobs(m),df=d)
}
joint <- function(m,terms,label) {
  b<-coef(m)[terms];V<-vcovHC(m,type='HC3')[terms,terms,drop=FALSE];W<-drop(t(b)%*%solve(V,b));data.frame(test=label,statistic=W,df=length(b),p=pchisq(W,length(b),lower.tail=FALSE))
}
message('A. Card attributes, disagreement, and characterization')
fits_attr <- list(All=feols(fm('y',paste0(paste(attrs,collapse='+'),'|id')),long,cluster=~id),Control=feols(fm('y',paste0(paste(attrs,collapse='+'),'|id')),filter(long,arm=='Control'),cluster=~id))
atr <- bind_rows(lapply(names(fits_attr),function(n) tidy_fe(fits_attr[[n]],n)))
export(atr,'attributes')
stopifnot(abs(atr$estimate[atr$model=='All'&atr$term=='failed']-.546241)<1e-5)
t$tercile<-factor(ifelse(t$hb<=quantile(t$hb,1/3),'Lower',ifelse(t$hb>=quantile(t$hb,2/3),'Upper','Middle')),levels=c('Lower','Middle','Upper'))
long<-left_join(long,t%>%select(id,tercile),by='id')
terc<-bind_rows(lapply(levels(t$tercile),function(g) tidy_fe(feols(fm('y',paste0(paste(attrs,collapse='+'),'|id')),filter(long,tercile==g),cluster=~id),g)))
export(terc,'attributes_by_tercile')
card_summary<-long%>%group_by(card)%>%summarise(n=n(),repeat_rate=mean(y),pair_disagreement=2*sum(y)*(n()-sum(y))/(n()*(n()-1)),.groups='drop')%>%left_join(design,by='card')
export(card_summary,'card_disagreement')
export(t%>%group_by(tercile)%>%summarise(n=n(),mean_hb=mean(hb),sd_hb=sd(hb),mean_ha=mean(ha),age=mean(edad,na.rm=TRUE),secondary=mean(level=='Secondary',na.rm=TRUE),female=mean(female,na.rm=TRUE),effort=mean(meritocracia,na.rm=TRUE),resources=mean(resources,na.rm=TRUE),.groups='drop'),'tercile_descriptives')
common<-t[complete.cases(t[,x3]),]
chmodels<-list(X1_available=lm(fm('hb',x1),t),X2_available=lm(fm('hb',x2),t),X1_common=lm(fm('hb',x1),common),X2_common=lm(fm('hb',x2),common),Resources_common=lm(fm('hb',x3),common))
export(bind_rows(lapply(names(chmodels),function(n)tidy_lm(chmodels[[n]],n))),'characterization')
missingness<-data.frame(variable=x3,n_observed=sapply(t[,x3],function(z)sum(!is.na(z))),n_total=nrow(t));export(missingness,'covariate_availability')
gm<-glm(fm('y',attrs),long,family=binomial());long$hc_dev<-long$y-predict(gm,type='response')
t<-left_join(t,long%>%group_by(id)%>%summarise(hc=mean(hc_dev),.groups='drop'),by='id')
alt<-bind_rows(lapply(c('hb','ha','hc'),function(y)tidy_lm(lm(fm(y,x3),t),y)));export(alt,'alternative_outcomes')
saveplot(ggplot(atr,aes(x=100*estimate,y=factor(term,levels=rev(attrs)),color=model))+geom_vline(xintercept=0,color='grey70')+geom_errorbar(aes(xmin=100*lo,xmax=100*hi),orientation='y',width=.15,position=position_dodge(.5))+geom_point(position=position_dodge(.5),size=2)+labs(x='Difference in recommendation probability (percentage points)',y=NULL,color=NULL),'attribute_effects',8,4.8)
saveplot(ggplot(t,aes(100*hb))+geom_histogram(binwidth=5,fill='#215e70',color='white')+labs(x='Relative recommendation propensity (percentage points)',y='Teachers'),'propensity_distribution',7.6,4.2)
saveplot(ggplot(card_summary,aes(x=reorder(card,repeat_rate),y=100*repeat_rate))+geom_point(color='#215e70')+coord_flip()+labs(x='Recorded profile identifier',y='Repetition recommendations (%)')+theme(axis.text.y=element_text(size=5)),'profile_rates',7,9)

message('B. Policy estimands with robust covariance and explicit contrast orientation')
broad<-list(c('Policy','Control'),c('Revelation','Policy'),c('Awareness','Revelation'))
policy_rows<-list();j<-1
for (pair in broad) {
  z<-t[t$arm%in%pair,];z$arm<-relevel(droplevels(z$arm),pair[2]);m<-lm(hb~arm,z)
  policy_rows[[j]]<-contrast(m,setNames(1,paste0('arm',pair[1])),paste(pair,collapse=' - '),'Broad arms');j<-j+1
  ma<-lm(fm('hb',c('arm',x1)),z)
  policy_rows[[j]]<-contrast(ma,setNames(1,paste0('arm',pair[1])),paste(pair,collapse=' - '),'Broad arms + X1');j<-j+1
}
cellmod<-lm(hb~0+cell,t)
policy_rows[[j]]<-bind_rows(lapply(levels(t$cell)[-1],function(z)contrast(cellmod,setNames(c(1,-1),paste0('cell',c(z,'Control'))),paste(z,'- Control'),'Cells vs control')));j<-j+1
for (a in c('Policy','Revelation','Awareness')) for(k in 2:3) {
  policy_rows[[j]]<-contrast(cellmod,setNames(c(1,-1),paste0('cell',a,'_',c(k,1))),paste(a,': policy',k,'- 1'),'Policies within arm');j<-j+1
}
for (p in 1:3) for(pair in broad[-1]) {
  policy_rows[[j]]<-contrast(cellmod,setNames(c(1,-1),paste0('cell',pair,'_',p)),paste(paste(pair,collapse=' - '),': policy',p),'Arms within policy');j<-j+1
}
pr<-bind_rows(policy_rows)%>%group_by(family)%>%mutate(p_holm=p.adjust(p,'holm'))%>%ungroup();export(pr,'policy_contrasts')
eq<-bind_rows(lapply(c(.02,.03,.05),function(delta)pr%>%mutate(margin=delta,p_tost=pmax(pt((estimate+delta)/se,df,lower.tail=FALSE),pt((estimate-delta)/se,df)),equivalent=p_tost<.05)))
export(eq,'policy_equivalence')
cellmeans<-tidy_lm(cellmod,'Ten cells')%>%mutate(cell=sub('^cell','',term));export(cellmeans,'policy_cell_means')
jointpol<-list()
for(pair in broad[-1]) {
  z<-t[t$arm%in%pair,];z$arm<-relevel(droplevels(z$arm),pair[2]);z$policy<-droplevels(z$policy)
  m<-lm(hb~arm*policy,z)
  jointpol[[paste(pair,collapse='_')]]<-joint(m,grep(':',names(coef(m)),value=TRUE),paste('Arm x policy:',paste(pair,collapse=' - ')))
}
export(bind_rows(jointpol),'policy_joint_tests')
saveplot(ggplot(cellmeans,aes(x=100*estimate,y=factor(cell,levels=rev(levels(t$cell)))))+geom_vline(xintercept=0,color='grey65')+geom_errorbar(aes(xmin=100*lo,xmax=100*hi),orientation='y',width=.15)+geom_point(size=2,color='#215e70')+labs(x='Mean relative propensity (percentage points)',y=NULL),'policy_cells',8,5)
saveplot(ggplot(filter(pr,family=='Broad arms'),aes(x=100*estimate,y=contrast))+annotate('rect',xmin=-3,xmax=3,ymin=-Inf,ymax=Inf,alpha=.12,fill='#2a8077')+geom_vline(xintercept=0,color='grey65')+geom_errorbar(aes(xmin=100*lo,xmax=100*hi),orientation='y',width=.1)+geom_point(size=2)+labs(x='Difference (percentage points); 95% intervals',y=NULL),'policy_broad',8,3.5)
# Direct decision-level sensitivity, with profile fixed effects and teacher clustering.
direct<-bind_rows(lapply(broad,function(pair){z<-filter(long,arm%in%pair);z$arm<-relevel(droplevels(z$arm),pair[2]);tidy_fe(feols(y~arm|card,z,cluster=~id),paste(pair,collapse=' - '))}));export(direct,'policy_card_level')
# Preserve the previous favorite-adjusted specifications as explicitly secondary.
adjrows<-list()
for(pair in broad[-1]) {
 z<-filter(t,arm%in%pair);z$arm<-relevel(droplevels(z$arm),pair[2]);m<-lm(hb~arm*policy+favorite,z)
 for(k in 1:3){w<-setNames(1,paste0('arm',pair[1]));if(k>1)w[paste0('arm',pair[1],':policy',k)]<-1;adjrows[[paste(pair[1],k)]]<-contrast(m,w,paste(paste(pair,collapse=' - '),': policy',k),'Favorite-adjusted')}
}
export(bind_rows(adjrows),'policy_favorite_adjusted')
alignrows<-list()
z<-filter(t,arm%in%c('Control','Policy'),alignment%in%c('Control','Favorite','Least'));z$alignment<-relevel(droplevels(z$alignment),'Control')
for(adj in c(FALSE,TRUE)) {
 m<-lm(if(adj)hb~alignment+favorite else hb~alignment,z)
 for(a in c('Favorite','Least'))alignrows[[paste('Study1',adj,a)]]<-contrast(m,setNames(1,paste0('alignment',a)),paste(a,'- Control'),if(adj)'Study I adjusted' else 'Study I unadjusted')
}
for(pair in broad[-1])for(a in c('Favorite','Middle','Least')) {
 z<-filter(t,arm%in%pair,alignment==a);z$arm<-relevel(droplevels(z$arm),pair[2]);m<-lm(hb~arm+favorite,z)
 alignrows[[paste(pair[1],a)]]<-contrast(m,setNames(1,paste0('arm',pair[1])),paste(paste(pair,collapse=' - '),':',a),'Alignment strata')
}
export(bind_rows(alignrows)%>%group_by(family)%>%mutate(p_holm=p.adjust(p,'holm'))%>%ungroup(),'alignment')
# Preference outcomes: linear probability differences, not log-odds.
prefs<-list()
for(k in 1:3)for(side in c('favorite','least')) {
 z<-filter(t,arm=='Policy',!is.na(.data[[side]]));z$selected<-as.integer(z[[side]]==as.character(k));z$assigned_here<-as.integer(z$policy==as.character(k));m<-lm(selected~assigned_here,z)
 prefs[[paste(k,side)]]<-contrast(m,c(assigned_here=1),paste('Policy',k,side), 'Assignment and later ranking')
}
export(bind_rows(prefs)%>%mutate(p_holm=p.adjust(p,'holm')),'preference_effects')
export(t%>%filter(!is.na(favorite))%>%group_by(arm,favorite)%>%summarise(n=n(),.groups='drop')%>%group_by(arm)%>%mutate(share=n/sum(n),total=sum(n))%>%ungroup(),'preference_shares')

message('C. Attrition and baseline balance (raw assignment denominators)')
rawt<-raw%>%filter(num(Treatment)%in%1:10)%>%transmute(id=ResponseId,treatment=num(Treatment),arm=factor(case_when(treatment==1~'Control',treatment%in%2:4~'Policy',treatment%in%5:7~'Revelation',TRUE~'Awareness'),levels=levels(t$arm)),policy=factor(ifelse(arm=='Control',NA,num(Politica)),levels=1:3),age=num(`6. Edad`),female=ifelse(`5. Género`=='Mujer',1,ifelse(`5. Género`=='Hombre',0,NA)),experience=num(`2.Antiguedad`),secondary=ifelse(nivel=='E. Secundaria',1,ifelse(nivel=='E. Primaria',0,NA)),public=ifelse(is.na(`9.Titularidad`),NA,as.integer(`9.Titularidad`=='Pública')),initial_done=!is.na(B1_end),task_complete=!is.na(B2_complet),ranking=!is.na(R1_1)&!is.na(R1_2)&!is.na(R1_3),survey_complete=Finished=='True',retained=id%in%df$id,score_observed=id%in%t$id)
rawt$age[rawt$age<20|rawt$age>80]<-NA;rawt$experience[rawt$experience>60]<-NA
rawt$task_started<-rowSums(!is.na(raw[match(rawt$id,raw$ResponseId),paste0('Grupo',1:8)]))>0
flow<-rawt%>%group_by(arm)%>%summarise(assigned=n(),retained=sum(retained),score_observed=sum(score_observed),task_started=sum(task_started),task_complete=sum(task_complete),ranking=sum(ranking),survey_complete=sum(survey_complete),.groups='drop');export(flow,'flow_by_arm')
export(rawt%>%filter(arm!='Control')%>%group_by(arm,policy)%>%summarise(assigned=n(),retained=sum(retained),score_observed=sum(score_observed),task_complete=sum(task_complete),ranking=sum(ranking),survey_complete=sum(survey_complete),.groups='drop'),'flow_by_policy')
attrrows<-list()
for(y in c('retained','score_observed','task_complete','ranking','survey_complete'))for(pair in broad) {
 z<-filter(rawt,arm%in%pair);z$arm<-relevel(droplevels(z$arm),pair[2]);z[[y]]<-as.numeric(z[[y]]);m<-lm(fm(y,'arm'),z)
 attrrows[[paste(y,pair[1])]]<-contrast(m,setNames(1,paste0('arm',pair[1])),paste(pair,collapse=' - '),y)
}
export(bind_rows(attrrows)%>%group_by(family)%>%mutate(p_holm=p.adjust(p,'holm'))%>%ungroup(),'attrition_contrasts')
balance<-list()
for(v in c('age','female','experience','secondary','public'))for(y in c('score_observed','survey_complete')){
 a<-rawt[[v]][rawt[[y]]];b<-rawt[[v]][!rawt[[y]]];balance[[paste(v,y)]]<-data.frame(variable=v,selection=y,n_yes=sum(!is.na(a)),n_no=sum(!is.na(b)),mean_yes=mean(a,na.rm=TRUE),mean_no=mean(b,na.rm=TRUE),smd=(mean(a,na.rm=TRUE)-mean(b,na.rm=TRUE))/sqrt((var(a,na.rm=TRUE)+var(b,na.rm=TRUE))/2))
}
export(bind_rows(balance),'attrition_balance')
baselinearms<-rawt%>%group_by(arm)%>%summarise(n=n(),across(c(age,female,experience,secondary,public),~mean(.x,na.rm=TRUE)),.groups='drop');export(baselinearms,'baseline_by_arm')
flong<-flow%>%pivot_longer(c(retained,score_observed,task_complete,ranking,survey_complete),names_to='stage',values_to='n')%>%mutate(rate=n/assigned,stage=factor(stage,levels=c('retained','score_observed','task_complete','ranking','survey_complete')))
saveplot(ggplot(flong,aes(stage,100*rate,color=arm,group=arm))+geom_point(size=2)+geom_line()+labs(x=NULL,y='Available / assigned (%)',color=NULL)+scale_x_discrete(labels=c('Pass retained\nexclusion rule','Recommendation\nscore','Task completion\nflag','Policy\nranking','Survey\nfinished'))+theme(legend.position='bottom'),'availability_by_arm',9,4.6)
# Outcome bounds on the [0,1] observed-decision mean; no MAR assumption.
rr<-rawt%>%left_join(t%>%select(id,ha,hb),by='id')%>%group_by(arm)%>%summarise(n=n(),observed=sum(!is.na(ha)),r=mean(!is.na(ha)),mean_ha=mean(ha,na.rm=TRUE),.groups='drop')%>%mutate(lower=r*mean_ha,upper=r*mean_ha+1-r)
export(rr,'selection_outcome_bounds')
export(bind_rows(lapply(broad,function(pair){a<-rr[rr$arm==pair[1],];b<-rr[rr$arm==pair[2],];data.frame(contrast=paste(pair,collapse=' - '),lower=a$lower-b$upper,upper=a$upper-b$lower)})),'selection_effect_bounds')
# Inverse-observation weighting, conditional-MAR sensitivity only.
ip<-rawt
for(v in c('age','female','experience','secondary','public')){ip[[paste0(v,'_missing')]]<-as.integer(is.na(ip[[v]]));ip[[v]][is.na(ip[[v]])]<-median(ip[[v]],na.rm=TRUE)}
selmod<-glm(score_observed~arm+age+female+experience+secondary+public+age_missing+female_missing+experience_missing+secondary_missing+public_missing,ip,family=binomial())
ip$pr<-predict(selmod,type='response');tw<-left_join(t,ip%>%select(id,pr),by='id');tw$w<-1/pmax(tw$pr,.05)
iprows<-bind_rows(lapply(broad,function(pair){z<-filter(tw,arm%in%pair);z$arm<-relevel(droplevels(z$arm),pair[2]);m<-lm(hb~arm,z,weights=w);contrast(m,setNames(1,paste0('arm',pair[1])),paste(pair,collapse=' - '),'IPW sensitivity')}));export(iprows,'policy_ipw')

message('D. Recover displayed order; descriptive sequential dependence')
ordercols<-grep('^FL_[0-9]+_DO_Grupo[1-8]\\.[1-8]$',names(raw),value=TRUE)
ord<-raw%>%select(id=ResponseId,all_of(ordercols))%>%pivot_longer(-id,names_to='field',values_to='position')%>%filter(!is.na(position))%>%mutate(position=num(position),card=sub('^.*_DO_Grupo([1-8])\\.([1-8])$','alumno_\\1_\\2',field))
orddiag<-ord%>%group_by(id)%>%summarise(n_rows=n(),n_cards=n_distinct(card),n_pos=n_distinct(position),min_pos=min(position),max_pos=max(position),.groups='drop')%>%mutate(valid=n_rows==8&n_cards==8&n_pos==8&min_pos==1&max_pos==8)
valid_ids<-orddiag$id[orddiag$valid]
sq<-inner_join(long,filter(ord,id%in%valid_ids)%>%select(id,card,position),by=c('id','card'))%>%arrange(id,position)%>%group_by(id)%>%mutate(previous=lag(y),previous_card=lag(card),lag_position=lag(position),switch=as.integer(y!=previous))%>%ungroup()%>%filter(position-lag_position==1)
seqmodel<-feols(y~previous+i(position)|id+card,sq,cluster=~id)
seqprofile<-feols(y~previous+i(position)|id+card+previous_card,sq,cluster=~id)
export(bind_rows(tidy_fe(seqmodel,'Teacher + current profile FE'),tidy_fe(seqprofile,'Teacher + current and previous profile FE')),'sequence_models')
export(sq%>%group_by(previous,y)%>%summarise(n=n(),.groups='drop')%>%group_by(previous)%>%mutate(share=n/sum(n))%>%ungroup(),'sequence_transitions')
fullsq<-inner_join(long,filter(ord,id%in%valid_ids)%>%select(id,card,position),by=c('id','card'))%>%group_by(id)%>%filter(n()==8)%>%ungroup()%>%select(id,position,y)%>%pivot_wider(names_from=position,values_from=y)
M<-as.matrix(fullsq[,as.character(1:8)]);obs_switch<-mean(M[,-1]!=M[,-8]);set.seed(921)
perm<-replicate(999,{P<-t(apply(M,1,sample));mean(P[,-1]!=P[,-8])})
seqsumm<-data.frame(raw_with_order=nrow(orddiag),valid_order=sum(orddiag$valid),included_with_valid_order=sum(t$id%in%valid_ids),adjacent_pairs=nrow(sq),teachers_with_pairs=n_distinct(sq$id),complete_eight=nrow(M),observed_switch=obs_switch,permutation_mean=mean(perm),permutation_lo=quantile(perm,.025),permutation_hi=quantile(perm,.975),permutation_two_sided=(1+sum(abs(perm-mean(perm))>=abs(obs_switch-mean(perm))))/1000)
export(seqsumm,'sequence_summary')
saveplot(ggplot(data.frame(switch=100*perm),aes(switch))+geom_histogram(bins=25,fill='#c2d8de',color='white')+geom_vline(xintercept=100*obs_switch,color='#ab4c14',linewidth=1)+labs(x='Switching between consecutive recommendations (%)',y='Permutations'),'sequence_permutation',7.5,4.4)

message('E. Bootstrap teachers and reconstruct the relative score')
set.seed(924)
ids<-t$id;idx<-match(long$id,ids);cidx<-as.integer(factor(long$card));nc<-length(unique(cidx));B<-399
bootpol<-matrix(NA_real_,B,3);bootchar<-matrix(NA_real_,B,length(coef(chmodels$Resources_common)));colnames(bootchar)<-names(coef(chmodels$Resources_common))
for(b in seq_len(B)){
 draw<-sample.int(length(ids),replace=TRUE);w<-tabulate(draw,nbins=length(ids));lw<-w[idx]
 sums<-rowsum(long$y*lw,cidx,reorder=FALSE); counts<-rowsum(lw,cidx,reorder=FALSE)
 # Named aggregation avoids relying on the first appearance order of card identifiers.
 sm<-setNames(as.numeric(sums),rownames(sums));ct<-setNames(as.numeric(counts),rownames(counts))
 ref<-(sm[as.character(cidx)]-long$y)/(ct[as.character(cidx)]-1)
 h<-tapply(long$y-ref,idx,mean);tb<-t[draw,];tb$hb<-as.numeric(h[as.character(draw)])
 for(k in 1:3){pair<-broad[[k]];bootpol[b,k]<-mean(tb$hb[tb$arm==pair[1]])-mean(tb$hb[tb$arm==pair[2]])}
 cc<-tb[complete.cases(tb[,x3]),];bootchar[b,]<-coef(lm(fm('hb',x3),cc))
}
bs<-function(M,labels) data.frame(term=labels,bootstrap_se=apply(M,2,sd),lo=apply(M,2,quantile,.025),hi=apply(M,2,quantile,.975),replicates=B)
export(bs(bootpol,sapply(broad,paste,collapse=' - ')),'policy_bootstrap');export(bs(bootchar,colnames(bootchar)),'characterization_bootstrap')
summary<-list(n_teachers=nrow(t),n_decisions=nrow(long),n_control=sum(t$arm=='Control'),n_common=nrow(common),hb_mean=mean(t$hb),hb_sd=sd(t$hb),ha_mean=mean(t$ha),decision_weighted_repeat=mean(long$y),weighted_disagreement=weighted.mean(card_summary$pair_disagreement,card_summary$n),uniform_identifier_disagreement=mean(card_summary$pair_disagreement),raw_assigned=nrow(rawt),resources_correlation=cor(t$resources_excess,t$resources_ineffective,use='complete.obs'),age_experience_correlation=cor(t$edad,t$experiencia,use='complete.obs'),zero_repetition_teachers=sum(t$ha==0),n_decisions_distribution=as.list(table(t$n_cards)))
write_json(summary,file.path(out,'reports','analysis_summary.json'),pretty=TRUE,auto_unbox=TRUE)
saveRDS(list(t=t,long=long,rawt=rawt,x1=x1,x2=x2,x3=x3,common=common,summary=summary),file.path(out,'reports','analysis_objects.rds'))
capture.output(sessionInfo(),file=file.path(out,'reports','R_session.txt'))
message('Core analysis completed. Predictive validation runs in 10. two_papers_prediction.R')
