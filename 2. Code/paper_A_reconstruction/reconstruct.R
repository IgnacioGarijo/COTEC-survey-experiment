# Isolated reconstruction. Never source the existing write-producing pipeline.
# Run from repository root: Rscript --vanilla .../reconstruct.R prepare|core|points|bootstrap [scenario] [draws] [workers]
options(stringsAsFactors=FALSE, width=140)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(arrow);library(dplyr);library(tidyr);library(fixest);library(sandwich);library(lme4);library(jsonlite)})
setFixest_nthreads(1)
args<-commandArgs(TRUE); mode<-if(exists('reconstruction_library_mode')&&isTRUE(reconstruction_library_mode))'library'else if(length(args))args[1]else'prepare'
out<-'3. Output/paper_A_reconstruction'; local<-file.path(out,'local')
for(p in c(out,local,file.path(out,'tables'),file.path(out,'diagnostics'),file.path(out,'logs')))dir.create(p,recursive=TRUE,showWarnings=FALSE)
stopifnot(file.exists(file.path(out,'.gitignore')),grepl('local/',paste(readLines(file.path(out,'.gitignore')),collapse='')))
attrs<-c('male','background','failed','competence','absent','disruptive')
x1<-c('level','experiencia','female','edad','public','public_permanent','grupos_docencia')
x2<-c(x1,'impact','empathy','meritocracia'); x3<-c(x2,'resources_excess')
scenarios<-c('A','B','C','D','B_full8','C_full8','C_inclusive')
num<-function(x)suppressWarnings(as.numeric(as.character(x)))
csv<-function(x,name,scenario=NULL){p<-if(is.null(scenario))file.path(out,'tables')else file.path(out,scenario);dir.create(p,recursive=TRUE,showWarnings=FALSE);write.csv(x,file.path(p,paste0(name,'.csv')),row.names=FALSE,na='')}
js<-function(x,name,scenario=NULL){p<-if(is.null(scenario))file.path(out,'diagnostics')else file.path(out,scenario);dir.create(p,recursive=TRUE,showWarnings=FALSE);write_json(x,file.path(p,paste0(name,'.json')),pretty=TRUE,auto_unbox=TRUE,na='null')}
object_path<-function(s)file.path(local,paste0('scenario_',s,'.rds'))
geto<-function(s)readRDS(object_path(s))
fe_tidy<-function(m,label){a<-as.data.frame(coeftable(m));ci<-confint(m);data.frame(model=label,term=rownames(a),estimate=a[[1]],se=a[[2]],p=a[[4]],lo=ci[,1],hi=ci[,2],n=nobs(m),teachers=length(unique(m$fixef_id[[1]])),row.names=NULL)}
lm_tidy<-function(m,label){V<-vcovHC(m,type='HC3');b<-coef(m);se<-sqrt(diag(V));df<-df.residual(m);data.frame(model=label,term=names(b),estimate=b,se=se,lo=b-qt(.975,df)*se,hi=b+qt(.975,df)*se,p=2*pt(-abs(b/se),df),n=nobs(m),r2=summary(m)$r.squared,row.names=NULL)}
rebuild<-function(l,cov){
 stopifnot(!anyDuplicated(l[,c('id','card')]),all(l$y%in%c(0L,1L)))
 l<-l%>%group_by(card)%>%mutate(reference_n=n(),reference_repeats=sum(y),loo=(sum(y)-y)/(n()-1),deviation=y-loo)%>%ungroup()
 stopifnot(all(l$reference_n>1),abs(sum(l$deviation))<1e-8)
 t<-l%>%group_by(id)%>%summarise(hb=mean(deviation),ha=mean(y),n_cards=n(),.groups='drop')%>%left_join(cov,by='id')
 cuts<-quantile(t$hb,c(1/3,2/3));t$tercile<-factor(ifelse(t$hb<=cuts[1],'Lower',ifelse(t$hb>=cuts[2],'Upper','Middle')),levels=c('Lower','Middle','Upper'))
 l<-left_join(l,t%>%select(id,level,titularidad,arm,tercile),by='id');l$ownership<-l$titularidad
 g<-glm(reformulate(attrs,'y'),l,family=binomial());l$hc_dev<-l$y-predict(g,type='response')
 gd<-glm(reformulate(c(attrs,'level','ownership'),'y'),l,family=binomial(),na.action=na.exclude)
 l$hd_dev<-l$y-predict(gd,newdata=l,type='response')
 h<-l%>%group_by(id)%>%summarise(hc=mean(hc_dev),hd=if(all(is.na(hd_dev)))NA_real_ else mean(hd_dev,na.rm=TRUE),.groups='drop')
 t<-left_join(t,h,by='id')
 list(t=t,long=l,common=t[complete.cases(t[,x3]),],cuts=cuts,x1=x1,x2=x2,x3=x3,glm_diagnostics=list(hc_converged=g$converged,hd_converged=gd$converged,hc_rank=g$rank,hd_rank=gd$rank))
}

if(mode=='prepare'){
 raw<-as.data.frame(read_parquet('1. Data/raw data/df_named.parquet'));clean<-as.data.frame(read_parquet('1. Data/processed data/cleandata.parquet'))
 stopifnot(!anyDuplicated(raw$ResponseId),!anyDuplicated(clean$id))
 for(e in parse('2. Code/0. main.R',encoding='UTF-8'))if(is.call(e)&&identical(e[[1]],as.name('<-'))&&grepl('^vector_',as.character(e[[2]])))eval(e)
 cards<-as.vector(outer(1:8,1:8,function(g,k)paste0('alumno_',g,'_',k)))
 response_field<-function(c){if(c=='alumno_3_5')return('Grupo 5_1');if(c=='alumno_6_1')return('Grupo 6_1');sub('alumno_([1-8])_([1-8])','Grupo \\1.\\2_1',c)}
 design<-data.frame(card=cards,male=as.integer(cards%in%get(ls(pattern='^vector_ni')[1])),background=as.integer(cards%in%vector_extranjero),failed=as.integer(cards%in%vector_suspensos),competence=as.integer(cards%in%vector_carencias),absent=as.integer(cards%in%vector_absentista),disruptive=as.integer(cards%in%vector_expulsion))
 design$response_field<-vapply(cards,response_field,character(1));design$signature<-apply(design[,attrs],1,paste0,collapse='')
 stopifnot(all(design$response_field%in%names(raw)),!anyDuplicated(design$response_field))
 csv(design,'card_mapping');csv(design%>%group_by(signature)%>%summarise(identifiers=n(),cards=paste(card,collapse=' | '),.groups='drop'),'coded_profile_support')
 all<-bind_rows(lapply(cards,function(c){v<-raw[[response_field(c)]];data.frame(id=raw$ResponseId,rawrow=seq_len(nrow(raw)),card=c,response_field=response_field(c),response=as.character(v))}))%>%left_join(design%>%select(card,all_of(attrs)),by='card')
 stopifnot(all(is.na(all$response)|all$response%in%c('pasa','repite')));all$y<-ifelse(is.na(all$response),NA_integer_,as.integer(all$response=='repite'))
 cols<-grep('^FL_[0-9]+_DO_Grupo[1-8]\\.[1-8]$',names(raw),value=TRUE)
 ord<-raw%>%select(id=ResponseId,all_of(cols))%>%pivot_longer(-id,names_to='order_field',values_to='position')%>%filter(!is.na(position))%>%mutate(position=num(position),card=sub('^.*_DO_Grupo([1-8])\\.([1-8])$','alumno_\\1_\\2',order_field))
 od<-ord%>%group_by(id)%>%summarise(entries=n(),distinct_cards=n_distinct(card),distinct_positions=n_distinct(position),valid=entries==8&&distinct_cards==8&&setequal(position,1:8),.groups='drop')
 csv(od%>%count(entries,distinct_cards,distinct_positions,valid),'order_diagnostics')
 ord<-ord%>%left_join(od%>%select(id,valid),by='id')%>%left_join(all,by=c('id','card'))
 ord$example<-as.character(raw$Ejemplo[ord$rawrow]);ord$example_known<-ord$example%in%as.character(1:4)
 ord$collision<-ifelse(ord$example%in%c('1','2'),ord$card%in%c('alumno_1_1','alumno_3_8'),ifelse(ord$example%in%c('3','4'),ord$card%in%c('alumno_2_1','alumno_2_8'),NA))
 stopifnot(all(ord$example_known[ord$valid]),!anyNA(ord$collision[ord$valid]))
 ord$time_field<-sub('alumno_([1-8])_([1-8])','time_grupo\\1.\\2_Page Submit',ord$card)
 ord$seconds<-mapply(function(i,n)if(n%in%names(raw))num(raw[[n]][i])else NA_real_,ord$rawrow,ord$time_field)
 ord$click_field<-sub('Page Submit$','Click Count',ord$time_field)
 ord$clicks<-mapply(function(i,n)if(n%in%names(raw))num(raw[[n]][i])else NA_real_,ord$rawrow,ord$click_field)
 ord$screen_retained<-ord$id%in%clean$id
 ord$b2_flag<-!is.na(raw$B2_complet[ord$rawrow]);ord$group_end_flag<-rowSums(!is.na(raw[,paste0('Grupo',1:8)]))[ord$rawrow]>0
 ord$finished<-raw$Finished[ord$rawrow]=='True';ord$finished[is.na(ord$finished)]<-FALSE
 # These are evidence fields, never a claim that a collision response was saved after the example.
 ord$provenance<-ifelse(ord$collision,'shared_example_task_unresolved','ordered_not_shared')
 vv<-filter(ord,valid)
 td<-vv%>%group_by(id)%>%summarise(rawrow=first(rawrow),stored=sum(!is.na(response)),collisions=sum(collision),screen_retained=first(screen_retained),group_end_flag=first(group_end_flag),b2_flag=first(b2_flag),finished=first(finished),.groups='drop')
 valid_ids<-td$id
 extra<-all%>%filter(!is.na(response),id%in%valid_ids)%>%anti_join(vv%>%select(id,card),by=c('id','card'))
 extra$example<-as.character(raw$Ejemplo[extra$rawrow]);csv(extra%>%count(example,card)%>%left_join(design%>%select(card,all_of(attrs)),by='card'),'outside_order_by_example')
 csv(td%>%count(collisions),'raw_collision_counts');csv(td%>%count(stored,group_end_flag,b2_flag),'stored_vs_completion')
 csv(vv%>%group_by(collision)%>%summarise(assigned=n(),stored=sum(!is.na(response)),positive_timing=sum(is.finite(seconds)&seconds>0),positive_clicks=sum(is.finite(clicks)&clicks>0),group_end=sum(group_end_flag),.groups='drop'),'collision_evidence')
 filt<-c('Grupo 1.1_1','Grupo 2.1_1','Grupo 5.1_1','Grupo 8.1_1');ex<-rowSums(raw[,filt]=='repite',na.rm=TRUE)>0
 stopifnot(setequal(raw$ResponseId[!ex],clean$id))
 filter_audit<-bind_rows(lapply(filt,function(f){c<-design$card[match(f,design$response_field)];is_example<-ifelse(as.character(raw$Ejemplo)%in%c('1','2'),c%in%c('alumno_1_1','alumno_3_8'),ifelse(as.character(raw$Ejemplo)%in%c('3','4'),c%in%c('alumno_2_1','alumno_2_8'),FALSE));assigned<-raw$ResponseId%in%vv$id[vv$card==c];data.frame(field=f,card=c,observed=sum(!is.na(raw[[f]])),repeat_count=sum(raw[[f]]=='repite',na.rm=TRUE),repeat_and_assigned=sum(raw[[f]]=='repite'&assigned,na.rm=TRUE),repeat_and_example=sum(raw[[f]]=='repite'&is_example,na.rm=TRUE),repeat_assigned_not_example=sum(raw[[f]]=='repite'&assigned&!is_example,na.rm=TRUE))}))
 csv(filter_audit,'screening_fields');csv(data.frame(excluded=ex,valid_order=raw$ResponseId%in%valid_ids)%>%count(excluded,valid_order),'screening_counts')
 # Recreate exactly the current covariate transformations, independently of scores/cases.
 cov<-data.frame(id=raw$ResponseId,level=factor(ifelse(raw$nivel=='E. Primaria','Primary',ifelse(raw$nivel=='E. Secundaria','Secondary',NA)),levels=c('Primary','Secondary')),
 experiencia=ifelse(num(raw[['2.Antiguedad']])>60,NA,num(raw[['2.Antiguedad']])),female=ifelse(raw[['5. Género']]=='Mujer',1,ifelse(raw[['5. Género']]=='Hombre',0,NA)),
 edad=ifelse(num(raw[['6. Edad']])>80|num(raw[['6. Edad']])<20,NA,num(raw[['6. Edad']])),titularidad=raw[['9.Titularidad']],
 public=ifelse(is.na(raw[['9.Titularidad']]),NA,as.integer(raw[['9.Titularidad']]=='Pública')),
 public_permanent=as.integer(raw[['10.Situación Lab.Pub']]%in%c('Contratada/o laboral indefinido','Funcionaria/o con destino definitivo')),
 grupos_docencia=ifelse(num(raw[['13. Grupos Clase']])>20,NA,num(raw[['13. Grupos Clase']])),
 impact=coalesce(num(raw[['19.impacto_centro_1']]),num(raw[['19.impacto_region_1']])),
 empathy=coalesce(num(raw[['21.Empatía_Control_1']]),ceiling(num(raw[['21.Empatía_T1_1']])/20),ceiling(num(raw[['21.Empatía_T2_1']])/20),ceiling(num(raw[['21.Empatía_T3_1']])/20)),
 meritocracia=num(raw[['22.Meritocracia']]),resources_excess=coalesce(num(raw[['19.impacto_centro_4']]),num(raw[['19.impacto_region_4']])),
 arm=factor(ifelse(num(raw$Treatment)==1,'Control',ifelse(num(raw$Treatment)%in%2:4,'Policy',ifelse(num(raw$Treatment)%in%5:7,'Revelation',ifelse(num(raw$Treatment)%in%8:10,'Awareness',NA)))),levels=c('Control','Policy','Revelation','Awareness')),
 cp=as.character(raw[['8 C.Postal']]),start=as.character(raw$StartDate))
 cov$empathy[cov$empathy==0&!is.na(cov$empathy)]<-1
 cm<-match(cov$id,clean$id);cov$cp[!is.na(cm)]<-as.character(clean$cp[cm[!is.na(cm)]])
 old<-readRDS('3. Output/paper_A/reports/analysis_objects.rds')
 errors<-bind_rows(lapply(x3,function(v){a<-cov[[v]][match(old$t$id,cov$id)];b<-old$t[[v]];data.frame(variable=v,mismatches=sum((is.na(a)!=is.na(b))|(!is.na(a)&!is.na(b)&as.character(a)!=as.character(b))))}))
 csv(errors,'covariate_replication');stopifnot(all(errors$mismatches==0))
 al<-all%>%filter(!is.na(response),id%in%clean$id,!card%in%c('alumno_1_1','alumno_2_1'))
 bl<-vv%>%filter(screen_retained,!is.na(response));cl<-bl%>%filter(!collision);dl<-bl%>%filter(id%in%td$id[td$collisions==0])
 ll<-list(A=al,B=bl,C=cl,D=dl,B_full8=bl%>%filter(id%in%td$id[td$stored==8]),C_full8=cl%>%filter(id%in%td$id[td$stored==8]),C_inclusive=vv%>%filter(!is.na(response),!collision))
 for(s in names(ll)){
  o<-rebuild(ll[[s]],cov);o$scenario<-s;saveRDS(o,object_path(s));message(s,': ',nrow(o$t),' records, ',nrow(o$long),' decisions, ',nrow(o$common),' complete covariates')
 }
 a<-geto('A');stopifnot(nrow(a$t)==2705,nrow(a$long)==22124,nrow(a$common)==2157,setequal(a$t$id,old$t$id),max(abs(a$t$hb-old$t$hb[match(a$t$id,old$t$id)]))<1e-12)
 metrics<-data.frame(metric=c('raw_records','unique_ResponseId','valid_sequences','no_collisions_raw','one_collision_raw','two_collisions_raw','eight_stored_no_collisions_raw','A_with_valid_order','A_without_valid_order','A_adverse_examples_outside_order','raw_shared_fields','raw_shared_records','current_screen_exclusions'),value=c(nrow(raw),n_distinct(raw$ResponseId),nrow(td),sum(td$collisions==0),sum(td$collisions==1),sum(td$collisions==2),sum(td$stored==8&td$collisions==0),sum(a$t$id%in%valid_ids),sum(!a$t$id%in%valid_ids),sum(extra$id%in%a$t$id&extra$card%in%c('alumno_2_8','alumno_3_8')),sum(vv$collision),sum(td$collisions>0),sum(ex)))
 csv(metrics,'audit_counts')
 write_parquet(vv,file.path(local,'teacher_position_provenance.parquet'));saveRDS(list(raw_id=raw$ResponseId,screen=ex,valid_order=od,ordered=vv,teacher_design=td,cov=cov,any_card=raw$ResponseId%in%all$id[!is.na(all$response)],group_end=rowSums(!is.na(raw[,paste0('Grupo',1:8)]))>0,finished=raw$Finished=='True',final_module=!is.na(raw[['time_iniciobloque3_Page Submit']])),file.path(local,'provenance.rds'))
 js(list(raw_md5=unname(tools::md5sum('1. Data/raw data/df_named.parquet')),clean_md5=unname(tools::md5sum('1. Data/processed data/cleandata.parquet')),scenario_md5=as.list(setNames(unname(tools::md5sum(vapply(scenarios,object_path,character(1)))),scenarios)),R=R.version.string,seed_policy='Point estimates deterministic; all stochastic seeds specified in code'), 'inputs')
 capture.output(sessionInfo(),file=file.path(out,'diagnostics','R_session.txt'))
 quit(save='no')
}

core<-function(s){
 o<-geto(s);t<-o$t;l<-o$long;cc<-o$common
 dir.create(file.path(out,s),recursive=TRUE,showWarnings=FALSE)
 # Match observation order and fixest t references used by current source scripts.
 m<-feols(reformulate(paste0(paste(attrs,collapse='+'),'|id'),'y'),l,cluster=~id,notes=FALSE)
 a<-fe_tidy(m,'All');csv(a,'attributes',s)
 z<-filter(l,arm=='Control');mc<-feols(reformulate(paste0(paste(attrs,collapse='+'),'|id'),'y'),z,cluster=~id,notes=FALSE);csv(fe_tidy(mc,'Control'),'attributes_control',s)
 im<-feols(y~male+background+failed+competence+absent+disruptive+failed:competence+failed:absent+failed:disruptive|id,l,cluster=~id,notes=FALSE)
 it<-fe_tidy(im,'Academic interaction model');it$p_holm<-NA_real_;ix<-grepl(':',it$term);it$p_holm[ix]<-p.adjust(it$p[ix],'holm');csv(it,'academic_interactions',s)
 l$academic<-factor(paste0(l$failed,l$competence),levels=c('00','01','10','11'),labels=c('Few failures / adequate competence','Few failures / low competence','Many failures / adequate competence','Many failures / low competence'))
 am<-feols(y~0+academic,l,cluster=~id,notes=FALSE);V<-vcov(am);b<-coef(am);se<-sqrt(diag(V));q<-qt(.975,fixest::degrees_freedom(am,type='t'))
 card<-l%>%group_by(card)%>%summarise(n=n(),repeats=sum(y),rate=mean(y),disagreement=2*sum(y)*(n()-sum(y))/(n()*(n()-1)),academic=first(academic),.groups='drop')
 ag<-card%>%group_by(academic)%>%summarise(decisions=sum(n),identifiers=n(),disagreement=weighted.mean(disagreement,n),.groups='drop')
 ag$estimate<-as.numeric(b);ag$se<-as.numeric(se);ag$lo<-ag$estimate-q*ag$se;ag$hi<-ag$estimate+q*ag$se
 csv(card,'card_disagreement',s);csv(ag,'academic_categories',s)
 csv(t%>%group_by(tercile)%>%summarise(n=n(),mean_hb=mean(hb),mean_ha=mean(ha),age=mean(edad,na.rm=TRUE),experience=mean(experiencia,na.rm=TRUE),secondary=mean(level=='Secondary',na.rm=TRUE),female=mean(female,na.rm=TRUE),effort=mean(meritocracia,na.rm=TRUE),resources_excess=mean(resources_excess,na.rm=TRUE),.groups='drop'),'tercile_descriptives',s)
 byterc<-bind_rows(lapply(levels(t$tercile),function(g)fe_tidy(feols(reformulate(paste0(paste(attrs,collapse='+'),'|id'),'y'),filter(l,tercile==g),cluster=~id,notes=FALSE),g)));csv(byterc,'attributes_by_tercile',s)
 fits<-list(X1_common=lm(reformulate(x1,'hb'),cc),X2_common=lm(reformulate(x2,'hb'),cc),X3_common=lm(reformulate(x3,'hb'),cc),X1_available=lm(reformulate(x1,'hb'),t))
 csv(bind_rows(lapply(names(fits),function(n)lm_tidy(fits[[n]],n))),'characterization',s)
 alt<-cc[complete.cases(cc[,c('ha','hb','hc','hd')]),];csv(bind_rows(lapply(c('ha','hb','hc','hd'),function(y)lm_tidy(lm(reformulate(x3,y),alt),y))),'four_outcomes',s)
 csv(data.frame(variable=x3,n_observed=sapply(t[,x3],function(z)sum(!is.na(z))),n_total=nrow(t)),'covariate_availability',s)
 csv(t%>%count(n_cards),'response_counts',s)
 support<-bind_rows(lapply(attrs,function(v){within<-l%>%group_by(id)%>%summarise(n_values=n_distinct(.data[[v]]),.groups='drop');data.frame(attribute=v,zero=sum(l[[v]]==0),one=sum(l[[v]]==1),teachers_both=sum(within$n_values==2),teachers_total=nrow(t))}));csv(support,'attribute_support',s)
 csv(l%>%group_by(across(all_of(attrs)))%>%summarise(decisions=n(),teachers=n_distinct(id),.groups='drop'),'profile_support',s)
 csv(bind_rows(lapply(c('level','titularidad','arm'),function(v)t%>%group_by(category=.data[[v]])%>%summarise(n=n(),.groups='drop')%>%mutate(variable=v))),'composition_categories',s)
 csv(bind_rows(lapply(c('edad','experiencia','female','public','hb'),function(v){b<-complete.cases(t[,setdiff(x3,x1)]);a<-t[[v]][b];d<-t[[v]][!b];data.frame(variable=v,n_with=sum(!is.na(a)),n_without=sum(!is.na(d)),with=mean(a,na.rm=TRUE),without=mean(d,na.rm=TRUE),smd=(mean(a,na.rm=TRUE)-mean(d,na.rm=TRUE))/sqrt((var(a,na.rm=TRUE)+var(d,na.rm=TRUE))/2))})),'belief_selection',s)
 summary<-data.frame(scenario=s,teachers=nrow(t),decisions=nrow(l),fe_teachers=a$teachers[1],fe_decisions=nobs(m),common_teachers=nrow(cc),common_decisions=sum(l$id%in%cc$id),identifiers=n_distinct(l$card),coded_profiles=nrow(unique(l[,attrs])),mean_repeat=mean(l$y),mean_teacher_repeat=mean(t$ha),mean_hb=mean(t$hb),sd_hb=sd(t$hb),q10=quantile(t$hb,.1),q90=quantile(t$hb,.9),tercile_low=o$cuts[1],tercile_high=o$cuts[2],disagreement=weighted.mean(card$disagreement,card$n),r2_full=summary(fits$X3_common)$r.squared,belief_complete=sum(complete.cases(t[,setdiff(x3,x1)])))
 csv(summary,'summary',s)
 X<-model.matrix(reformulate(attrs),l);XM<-model.matrix(reformulate(x3),cc)
 js(list(attribute_rank=qr(X)$rank,attribute_columns=ncol(X),characterization_rank=qr(XM)$rank,characterization_columns=ncol(XM),fe_removed_singletons=nrow(l)-nobs(m),fe_collinear=m$collin.var,interaction_collinear=im$collin.var,alternative_logit=o$glm_diagnostics,warnings='GLM warnings retained in execution log; estimability documented here.'),'core_diagnostics',s)
 message('Core saved: ',s)
}

if(mode=='core'){
 ss<-if(length(args)>=2)strsplit(args[2],',',fixed=TRUE)[[1]]else scenarios
 for(s in ss)core(s)
 # Comparisons of unchanged teachers; rebuild all scores inside each bridge sample.
 aa<-geto('A');bb<-geto('B');ids<-intersect(aa$t$id,bb$t$id)
 for(s in c('A','B')){o<-geto(s);ob<-rebuild(o$long%>%filter(id%in%ids)%>%select(-any_of(c('level','titularidad','ownership','arm','tercile','hc_dev','hd_dev'))),o$t%>%select(-any_of(c('hb','ha','n_cards','tercile','hc','hd'))));m<-feols(reformulate(paste0(paste(attrs,collapse='+'),'|id'),'y'),ob$long,cluster=~id,notes=FALSE);csv(fe_tidy(m,paste0(s,'_same_teachers_AB')),paste0('bridge_',s,'_attributes'));csv(data.frame(scenario=s,teachers=nrow(ob$t),decisions=nrow(ob$long),mean_hb=mean(ob$t$hb),sd_hb=sd(ob$t$hb)),'bridge_summary',paste0('bridge_',s))}
 quit(save='no')
}

blocks<-list(Student=c(attrs,apply(combn(attrs,2),2,paste,collapse=':')),Professional=x1,Beliefs=setdiff(x3,x1),Harshness='hb')
form<-as.formula(paste('y~',paste(unlist(blocks),collapse='+'),'+(0+',paste(paste0(attrs,'_c'),collapse='+'),'|id)'))
prepare_model<-function(l,t){z<-left_join(l[,c('id','card','y',attrs)],t[,c('id',x3,'hb')],by='id');z<-z[complete.cases(z),];z$id<-factor(z$id);for(v in attrs)z[[paste0(v,'_c')]]<-z[[v]]-mean(z[[v]]);z}
allocate<-function(m,z){
 eta<-as.numeric(predict(m,re.form=NA));ff<-nobars(formula(m));X<-model.matrix(ff,z);labs<-attr(terms(ff),'term.labels');groups<-lapply(blocks,function(a)which(attr(X,'assign')%in%match(a,labs)))
 K<-length(blocks);val<-numeric(2^K);shap<-numeric(K)
 for(mask in 0:(2^K-1)){sel<-which(as.logical(intToBits(mask)[seq_len(K)]));cols<-unique(c(1,unlist(groups[sel])));val[mask+1]<-var(lm.fit(X[,cols,drop=FALSE],eta)$fitted.values)}
 for(j in seq_len(K))for(mask in 0:(2^K-1)){sel<-which(as.logical(intToBits(mask)[seq_len(K)]));if(j%in%sel)next;s<-length(sel);shap[j]<-shap[j]+factorial(s)*factorial(K-s-1)/factorial(K)*(val[bitwOr(mask,bitwShiftL(1,j-1))+1]-val[mask+1])}
 names(shap)<-names(blocks);S<-as.matrix(VarCorr(m)$id);Z<-as.matrix(z[,colnames(S)]);v<-c(shap,Sensitivities=mean(rowSums((Z%*%S)*Z)),Residual=sigma(m)^2)
 stopifnot(abs(sum(shap)-var(eta))<1e-8,all(v>=-1e-12));v/sum(v)
}
diagnose<-function(m){g<-m@optinfo$derivs$gradient;H<-m@optinfo$derivs$Hessian;sc<-tryCatch(solve(chol(H),g),error=function(e)rep(Inf,length(g)));c(singular=as.numeric(isSingular(m)),gradient=max(pmin(abs(sc),abs(g))),min_hessian=min(eigen(H,symmetric=TRUE)$values),optimizer=as.numeric(m@optinfo$conv$opt),min_covariance=min(eigen(as.matrix(VarCorr(m)$id),symmetric=TRUE)$values))}
fit_point<-function(s){
 o<-geto(s);z<-prepare_model(o$long,o$t);path<-file.path(local,paste0('point_',s,'.rds'));fp<-unname(tools::md5sum(object_path(s)))
 if(file.exists(path)){old<-readRDS(path);if(identical(old$fingerprint,fp)){message('Point cache verified: ',s);return(invisible(old))}}
 X<-model.matrix(nobars(form),z);rank<-qr(X)$rank
 warnings<-character();fitonce<-function(opt){withCallingHandlers(lmer(form,z,REML=TRUE,control=lmerControl(optimizer=opt,calc.derivs=TRUE,optCtrl=if(opt=='nloptwrap')list(maxeval=30000)else list(maxfun=100000))),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})}
 started<-Sys.time();m<-fitonce('nloptwrap');di<-diagnose(m);optimizer_used<-'nloptwrap'
 if(di['optimizer']!=0||di['gradient']>=.002||di['min_hessian']<=0){m<-fitonce('bobyqa');di<-diagnose(m);optimizer_used<-'bobyqa'}
 p<-allocate(m,z);status<-if(di['optimizer']==0&&di['gradient']<.002&&di['min_hessian']>0&&rank==ncol(X))if(di['singular']==0)'accepted'else'accepted_boundary'else'diagnostic_failure'
 saveRDS(list(model=m,fingerprint=fp,status=status,diagnostics=di),path)
 csv(data.frame(component=names(p),share=as.numeric(p)),'decomposition_point',s)
 csv(data.frame(term=names(fixef(m)),estimate=as.numeric(fixef(m))),'decomposition_fixed_effects',s)
 csv(data.frame(as.matrix(VarCorr(m)$id),check.names=FALSE),'decomposition_covariance',s)
 js(list(scenario=s,status=status,n=nrow(z),teachers=nlevels(z$id),fixed_rank=rank,fixed_columns=ncol(X),covariance_parameters=length(getME(m,'theta')),diagnostics=as.list(di),optimizer_used=optimizer_used,harshness_coefficient=unname(fixef(m)['hb']),outside_01=mean(predict(m)<0|predict(m)>1),elapsed_seconds=as.numeric(difftime(Sys.time(),started,units='secs')),warnings=unique(warnings),object_md5=fp),'decomposition_diagnostics',s)
 message('Point ',s,': ',status,'; ',paste(round(p,5),collapse=', '));invisible(list(model=m,status=status))
}
if(mode=='points'){ss<-if(length(args)>=2)strsplit(args[2],',',fixed=TRUE)[[1]]else scenarios;for(s in ss)tryCatch(fit_point(s),error=function(e){js(list(scenario=s,status='error',message=conditionMessage(e)),'decomposition_diagnostics',s);message('POINT FAILURE ',s,': ',conditionMessage(e))});quit(save='no')}

if(mode=='bootstrap'){
 s<-args[2];B<-if(length(args)>=3)as.integer(args[3])else 199L;workers<-if(length(args)>=4)as.integer(args[4])else 4L
 # Every main scenario must have a saved point diagnosis before uncertainty starts.
 stopifnot(all(file.exists(file.path(out,c('A','B','C','D'),'decomposition_diagnostics.json'))))
 o<-geto(s);pt<-readRDS(file.path(local,paste0('point_',s,'.rds')));stopifnot(pt$fingerprint==unname(tools::md5sum(object_path(s))))
 if(!pt$status%in%c('accepted','accepted_boundary'))stop('Point stationarity/rank diagnostics unresolved; no bootstrap reported.')
 t<-o$t;l<-o$long[,c('id','card','y',attrs)];idx<-match(l$id,t$id);cidx<-as.integer(factor(l$card));by_teacher<-split(seq_len(nrow(l)),idx);theta_start<-getME(pt$model,'theta')
 # Own-response exclusion is per sampled clone, not per original ID.
 resample_data<-function(draw){w<-tabulate(draw,nbins=nrow(t));lw<-w[idx];sm<-rowsum(l$y*lw,cidx);ct<-rowsum(lw,cidx);sm<-setNames(as.numeric(sm),rownames(sm));ct<-setNames(as.numeric(ct),rownames(ct));stopifnot(all(ct[as.character(cidx[lw>0])]>1));ref<-(sm[as.character(cidx)]-l$y)/(ct[as.character(cidx)]-1);h<-tapply(l$y-ref,idx,mean);tb<-t[draw,];tb$id<-as.character(seq_along(draw));tb$hb<-as.numeric(h[as.character(draw)]);rows<-unlist(by_teacher[as.character(draw)],use.names=FALSE);lb<-l[rows,];lb$id<-rep(tb$id,lengths(by_teacher[as.character(draw)]));stopifnot(nrow(lb)==sum(w*t$n_cards),all(is.finite(tb$hb)),max(abs(tb$hb))<=1+1e-10,abs(sum(tb$hb*tb$n_cards))<1e-8,!anyDuplicated(tb$id));list(t=tb,long=lb)}
 run_one<-function(b){started<-Sys.time();set.seed(1004000+b);draw<-sample.int(nrow(t),replace=TRUE)
  tryCatch({d<-resample_data(draw);z<-prepare_model(d$long,d$t);warn<-character();m<-withCallingHandlers(lmer(form,z,REML=TRUE,start=theta_start,control=lmerControl(optimizer='nloptwrap',calc.derivs=FALSE,optCtrl=list(maxeval=30000,xtol_abs=1e-8,ftol_abs=1e-9))),warning=function(w){warn<<-c(warn,conditionMessage(w));invokeRestart('muffleWarning')});p<-allocate(m,z);op<-as.numeric(m@optinfo$conv$opt);data.frame(draw=b,as.list(p),optimizer=op,singular=as.numeric(isSingular(m)),min_covariance=min(eigen(as.matrix(VarCorr(m)$id),symmetric=TRUE)$values),accepted=op==0&&all(is.finite(p)),seconds=as.numeric(difftime(Sys.time(),started,units='secs')),error='',warnings=paste(unique(warn),collapse=' | '))},error=function(e)data.frame(draw=b,accepted=FALSE,error=conditionMessage(e)))
 }
 path<-file.path(out,s,'decomposition_bootstrap_draws.csv')
 method_fingerprint<-list(scenario_md5=pt$fingerprint,bootstrap_method='clone-LOO-six-correlated-slopes-v1',formula=paste(deparse(form),collapse=''),seed='1004000 + draw',R=R.version.string,lme4=as.character(packageVersion('lme4')),point_theta=as.numeric(theta_start))
 method_path<-file.path(out,s,'bootstrap_fingerprint.json')
 if(file.exists(path)){stopifnot(file.exists(method_path));prior_method<-read_json(method_path,simplifyVector=TRUE);stopifnot(isTRUE(all.equal(prior_method,method_fingerprint,check.attributes=FALSE,tolerance=1e-12)))}else write_json(method_fingerprint,method_path,pretty=TRUE,auto_unbox=TRUE,digits=NA)
 prior<-if(file.exists(path))read.csv(path)else data.frame(draw=integer());stopifnot(!anyDuplicated(prior$draw),all(prior$draw>=1))
 todo<-setdiff(seq_len(B),prior$draw);cl<-parallel::makeCluster(workers,rscript_args='--vanilla')
 parallel::clusterCall(cl,function(paths).libPaths(paths),.libPaths())
 worker_env<-parallel::clusterEvalQ(cl,{suppressPackageStartupMessages({library(dplyr);library(lme4)});list(R=R.version.string,lme4=as.character(packageVersion('lme4')),dplyr=as.character(packageVersion('dplyr')))})
 stopifnot(all(vapply(worker_env,function(e)e$lme4==as.character(packageVersion('lme4')),logical(1))))
 js(worker_env,'worker_environment',s)
 parallel::clusterExport(cl,c('t','l','idx','cidx','by_teacher','theta_start','attrs','x1','x3','blocks','form','prepare_model','allocate','resample_data','run_one'),envir=environment())
 batch<-2L*workers
 for(first in seq.int(1,max(1,length(todo)),by=batch)){if(!length(todo))break;ids<-todo[first:min(first+batch-1,length(todo))];ans<-parallel::parLapply(cl,ids,run_one);prior<-bind_rows(prior,bind_rows(ans));write.csv(prior,path,row.names=FALSE,na='');message('Bootstrap ',s,': ',nrow(prior),'/',B)}
 parallel::stopCluster(cl)
 prior<-prior[prior$draw<=B,];good<-prior$accepted%in%TRUE
 pp<-read.csv(file.path(out,s,'decomposition_point.csv'));namesp<-pp$component
 if(sum(good)>=ceiling(.95*B)&&B>=50){pp$se<-sapply(prior[good,namesp],sd);pp$lo_percentile<-sapply(prior[good,namesp],quantile,.025);pp$hi_percentile<-sapply(prior[good,namesp],quantile,.975);pp$successful<-sum(good);pp$requested<-B;csv(pp,'decomposition',s)}
 js(list(scenario=s,seed='1004000 + draw',requested=B,attempted=nrow(prior),accepted=sum(good),failed=sum(!good),boundary_accepted=sum(prior$singular[good]==1,na.rm=TRUE),acceptance='optimizer code 0, finite nonnegative allocations; boundary fits retained; no per-draw Hessian, as current script 49',interpretation='Descriptive bootstrap spread conditional on the estimator; percentile ranges do not establish calibrated coverage at a boundary',point_status=pt$status,workers=workers),'bootstrap_status',s)
 quit(save='no')
}
if(!mode%in%c('prepare','core','points','bootstrap','library'))stop('Unknown mode')
