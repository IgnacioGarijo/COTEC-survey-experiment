# Exploratory adverse-example analysis; run from repository root.
options(stringsAsFactors=FALSE, width=140)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(arrow);library(dplyr);library(fixest);library(sandwich);library(jsonlite)})
setFixest_nthreads(1)
O<-'3. Output/adverse_example_analysis';dir.create(O,recursive=TRUE,showWarnings=FALSE)
write<-function(x,n)write.csv(x,file.path(O,paste0(n,'.csv')),row.names=FALSE,na='')
r<-as.data.frame(read_parquet('1. Data/raw data/df_named.parquet'))
p<-readRDS('3. Output/paper_A_final_revision/local/provenance.rds')
b<-readRDS('3. Output/paper_A_final_revision/local/scenario_B.rds')
o<-readRDS('3. Output/paper_A_final_revision/local/scenario_B_full8.rds')
stopifnot(!anyDuplicated(r$ResponseId),!anyDuplicated(o$t$id),identical(r$ResponseId,p$raw_id),nrow(o$t)==2520,all(o$t$n_cards==8))
# Own administered example: codes 1/2 male adverse example, codes 3/4 female.
ex<-as.character(r$Ejemplo);known<-ex%in%as.character(1:4)
afield<-ifelse(ex%in%c('1','2'),'Grupo 3.8_1',ifelse(ex%in%c('3','4'),'Grupo 2.8_1',NA))
acard<-ifelse(ex%in%c('1','2'),'alumno_3_8',ifelse(ex%in%c('3','4'),'alumno_2_8',NA))
favorable<-ifelse(ex%in%c('1','2'),as.character(r[['Grupo 1.1_1']]),ifelse(ex%in%c('3','4'),as.character(r[['Grupo 2.1_1']]),NA))
adverse<-ifelse(ex%in%c('1','2'),as.character(r[['Grupo 3.8_1']]),ifelse(ex%in%c('3','4'),as.character(r[['Grupo 2.8_1']]),NA))
stopifnot(all(is.na(adverse)|adverse%in%c('pasa','repite')),all(p$screen==(!is.na(favorable)&favorable=='repite')))
z<-data.frame(id=r$ResponseId,example=ex,adverse,favorable,adverse_card=acard,adverse_field=afield,promote=ifelse(is.na(adverse),NA,as.integer(adverse=='pasa')),screen=p$screen)
z$shared_adverse<-paste(z$id,z$adverse_card)%in%paste(p$ordered$id,p$ordered$card_id)
z$adverse_gender<-factor(ifelse(ex%in%c('1','2'),'Boy',ifelse(ex%in%c('3','4'),'Girl',NA)))
t<-left_join(o$t,z,by='id');t$secondary<-as.integer(t$level=='Secondary')
stopifnot(!anyNA(t$promote),sum(t$promote)==348,nrow(t)==2520)
map<-read.csv('3. Output/paper_A_reconstruction/tables/card_mapping.csv')
check<-map[map$card%in%c('alumno_2_8','alumno_3_8'),];stopifnot(nrow(check)==2,all(as.matrix(check[,c('background','failed','competence','absent','disruptive')])==1))
write(check,'adverse_mapping')
cohorts<-list('Respuesta extrema disponible (sin exigir decisiones completas)'=z[!is.na(z$promote),], 'Muestra B tras cribado, con respuesta extrema'=z[z$id%in%b$t$id&!is.na(z$promote),], 'Principal: ocho tarjetas'=t, 'Principal: campo extremo no compartido'=t[!t$shared_adverse,], 'Principal: covariables completas'=t[complete.cases(t[,o$x3]),])
write(bind_rows(lapply(names(cohorts),function(n){d<-cohorts[[n]];data.frame(cohort=n,n=nrow(d),promote=sum(d$promote==1),n_repeat=sum(d$promote==0),pct=100*mean(d$promote))})),'sample_flow')
# Other-decision outcomes remove ALL shared example fields AND both adverse profiles.
l<-left_join(o$long,t%>%select(id,promote,shared_adverse,adverse_gender,secondary),by='id')
l$adverse_profile<-with(l,background==1&failed==1&competence==1&absent==1&disruptive==1)
k<-filter(l,!collision,!adverse_profile)
stopifnot(!any(k$collision),!any(k$adverse_profile),n_distinct(k$id)==2520,!anyDuplicated(k[,c('id','card')]))
k<-k%>%group_by(profile)%>%mutate(clean_reference_n=n(),clean_reference=(sum(y)-y)/(n()-1),clean_deviation=y-clean_reference)%>%ungroup()
stopifnot(all(k$clean_reference_n>1),abs(sum(k$clean_deviation))<1e-8)
h<-k%>%group_by(id)%>%summarise(clean_repeat=100*mean(y),clean_hb=100*mean(clean_deviation),clean_n=n(),.groups='drop')
t<-left_join(t,h,by='id');stopifnot(!anyNA(t$clean_hb))
labels<-c(secondary='Secundaria (%)',experiencia='Experiencia (años)',female='Mujer (%)',edad='Edad (años)',public='Centro público (%)',public_permanent='Nombramiento público permanente (%)',grupos_docencia='Grupos docentes',impact='Impacto percibido (0–10)',empathy='Empatía (1–5)',meritocracia='Atribución al esfuerzo (1–10)',resources_excess='Recursos excesivos para repetidores (0–10)',clean_repeat='Repetición en otras tarjetas (%)',clean_hb='Harshness en otras tarjetas (pp)')
binary<-c('secondary','female','public','public_permanent');prof<-c('secondary','experiencia','female','edad','public','public_permanent','grupos_docencia');belief<-c('impact','empathy','meritocracia','resources_excess')
compare<-function(d,scenario){ans<-bind_rows(lapply(names(labels),function(v){a<-d[[v]][d$promote==1];b<-d[[v]][d$promote==0];a<-a[!is.na(a)];b<-b[!is.na(b)];va<-var(a);vb<-var(b);sdpool<-sqrt(((length(a)-1)*va+(length(b)-1)*vb)/(length(a)+length(b)-2));isbin<-v%in%binary
 if(isbin){pv<-fisher.test(table(factor(d$promote,levels=0:1),factor(d[[v]],levels=0:1)))$p.value}else{pv<-t.test(a,b)$p.value}
 data.frame(scenario,variable=v,label=unname(labels[v]),family=if(v%in%prof)'Professional'else if(v%in%belief)'Beliefs'else'Other decisions',n_promote=length(a),n_repeat=length(b),mean_promote=mean(a)*if(isbin)100 else 1,mean_repeat=mean(b)*if(isbin)100 else 1,difference=(mean(a)-mean(b))*if(isbin)100 else 1,smd=(mean(a)-mean(b))/sdpool,p=pv)
 }));ans$p[ans$family=='Other decisions']<-NA_real_;ans%>%group_by(family)%>%mutate(p_bh=p.adjust(p,'BH'))%>%ungroup()}
write(bind_rows(compare(t,'Main'),compare(filter(t,!shared_adverse),'Unshared')),'group_comparisons')
write(t%>%group_by(promote)%>%summarise(teachers=n(),complete_covariates=sum(complete.cases(pick(all_of(o$x3)))),shared_adverse=sum(shared_adverse),min_other_cards=min(clean_n),max_other_cards=max(clean_n),median_other_cards=median(clean_n),zero_other_repeat=sum(clean_repeat==0),any_other_repeat=sum(clean_repeat>0),.groups='drop'),'group_counts')
write(t%>%group_by(promote)%>%summarise(q10=quantile(clean_hb,.1),q25=quantile(clean_hb,.25),median=median(clean_hb),q75=quantile(clean_hb,.75),q90=quantile(clean_hb,.9),.groups='drop'),'harshness_quantiles')
# Three partitions are descriptive diagnostics, not independent treatment effects.
context<-bind_rows(lapply(c('example','adverse_gender','arm'),function(v)t%>%group_by(category=as.character(.data[[v]]))%>%summarise(n=n(),promote=sum(promote),.groups='drop')%>%mutate(pct=100*promote/n,variable=v)))
stopifnot(all(context$pct>=0&context$pct<=100));write(context,'context_rates')
# LPM: same complete-case sample for professional / expanded covariates, HC3.
# Continuous inputs scaled by ONE SD, binaries unchanged; Y in percentage points.
vars<-c(prof,belief);continuous<-setdiff(vars,binary);cc<-t[complete.cases(t[,c(vars,'arm','adverse_gender')]),]
scales<-data.frame(variable=continuous,sd=sapply(cc[,continuous],sd));write(scales,'scales')
scales_named<-setNames(scales$sd,scales$variable)
models<-list();diag<-list()
fit_models<-function(d,scenario){for(v in continuous)d[[v]]<-(d[[v]]-mean(cc[[v]]))/scales_named[v]
 for(spec in c('Professional','Full')){f<-reformulate(c(if(spec=='Professional')prof else vars,'arm','adverse_gender'),'promote');m<-lm(f,d);V<-vcovHC(m,type='HC3');se<-sqrt(diag(V));bb<-coef(m);stopifnot(!anyNA(bb),qr(model.matrix(m))$rank==ncol(model.matrix(m)),all(is.finite(se)));df<-df.residual(m)
 res<-data.frame(scenario,model=spec,term=names(bb),estimate=100*bb,se=100*se,p=2*pt(-abs(bb/se),df),lo=100*(bb-qt(.975,df)*se),hi=100*(bb+qt(.975,df)*se),n=nobs(m),r2=summary(m)$r.squared)
 use<-res$term%in%vars;res$p_bh<-NA_real_;res$p_bh[use]<-p.adjust(res$p[use],'BH');models[[paste(scenario,spec)]]<<-res
 diag[[paste(scenario,spec)]]<<-data.frame(scenario,model=spec,n=nobs(m),events=sum(d$promote),rank=m$rank,columns=ncol(model.matrix(m)),r2=summary(m)$r.squared,pred_below0=sum(fitted(m)<0),pred_above1=sum(fitted(m)>1),condition=kappa(model.matrix(m)))
 }}
fit_models(cc,'Main');fit_models(cc[!cc$shared_adverse,],'Unshared');write(bind_rows(models),'adjusted_models');write(bind_rows(diag),'model_diagnostics')
# Profile-adjusted difference in nonshared/non-adverse experimental responses.
# Repeat after dropping participants whose adverse field is shared.
adj<-bind_rows(lapply(c('Main','Unshared'),function(sc){d<-if(sc=='Main')k else filter(k,!shared_adverse);m<-feols(y~promote+arm+adverse_gender|profile+set,d,cluster=~id,notes=FALSE);v<-coeftable(m)['promote',];data.frame(scenario=sc,estimate=100*v[1],se=100*v[2],p=v[4],teachers=n_distinct(d$id),decisions=nrow(d))}));write(adj,'other_decisions_adjusted')
# Aggregate histogram; no respondent-level output is written.
breaks<-seq(-100,100,by=10);hh<-bind_rows(lapply(0:1,function(g){v<-t$clean_hb[t$promote==g];q<-hist(v,breaks=breaks,plot=FALSE);data.frame(promote=g,mid=q$mids,n=q$counts,percent=100*q$counts/length(v))}));write(hh,'harshness_histogram')
# Audit includes input hashes, operational scope and no invented chronology.
status<-list(raw_n=nrow(r),known_example=sum(known),adverse_available=sum(!is.na(adverse)),full8=nrow(t),main_promote=sum(t$promote),main_repeat=sum(!t$promote),shared_adverse=sum(t$shared_adverse),shared_promote=sum(t$shared_adverse&t$promote==1),clean_other_decisions=nrow(k),clean_other_teachers=n_distinct(k$id),common=nrow(cc),input_md5=as.list(tools::md5sum(c('1. Data/raw data/df_named.parquet','3. Output/paper_A_final_revision/local/scenario_B_full8.rds','3. Output/paper_A_final_revision/local/scenario_B.rds','3. Output/paper_A_final_revision/local/provenance.rds','3. Output/paper_A_reconstruction/tables/card_mapping.csv'))),script_md5=unname(tools::md5sum('2. Code/adverse_example_analysis/analyze.R')),group_definition='Own adverse example response: pasa vs repite; shared fields ambiguous example/task, retained under reconstruction assumption; unshared sensitivity.',independence='Other-decision outcomes exclude all shared example fields and all adverse profiles; descriptive within-survey association, not external prediction.')
write_json(status,file.path(O,'audit.json'),pretty=TRUE,auto_unbox=TRUE)
capture.output(sessionInfo(),file=file.path(O,'R_session.txt'))
print(status[1:12]);print(filter(bind_rows(models),model=='Full',scenario=='Main',term%in%vars));print(read.csv(file.path(O,'group_comparisons.csv'))%>%filter(scenario=='Main'))

