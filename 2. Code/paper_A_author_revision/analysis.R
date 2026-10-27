# Author-approved Paper A revision: isolated outputs, unchanged source inputs.
options(stringsAsFactors=FALSE,width=140)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
requested_phase<-if(length(commandArgs(TRUE)))commandArgs(TRUE)[1]else'prepare'
reconstruction_library_mode<-TRUE
original_commandArgs<-commandArgs
commandArgs<-function(trailingOnly=FALSE)if(trailingOnly)'library'else original_commandArgs(FALSE)
source('2. Code/paper_A_final_revision/models.R')
rm(commandArgs)
out<-'3. Output/paper_A_author_revision';local<-file.path(out,'local');original<-'3. Output/paper_A_final_revision/local'
for(p in c(local,file.path(out,'tables'),file.path(out,'diagnostics')))dir.create(p,recursive=TRUE,showWarnings=FALSE)
rawinputs<-c(file.path(original,c('scenario_B.rds','scenario_B_full8.rds','scenario_NoScreen.rds','provenance.rds')),'1. Data/raw data/df_named.parquet')
if(requested_phase=='prepare'){
 base<-readRDS(file.path(original,'scenario_B.rds'));main<-readRDS(file.path(original,'scenario_B_full8.rds'));pr<-readRDS(file.path(original,'provenance.rds'));cov<-pr$cov
 stopifnot(nrow(main$t)==2520,nrow(main$long)==20160,nrow(main$common)==2164,nrow(main$common)*8==17312,all(main$t$n_cards==8),abs(mean(main$t$hb))<1e-12)
 strip<-function(l)l%>%select(-any_of(c('reference_n','reference_repeats','loo','deviation','level','titularidad','arm','tercile','ownership','hc_dev','hd_dev')))
 no<-readRDS(file.path(original,'scenario_NoScreen.rds'));noids<-no$t$id[no$t$n_cards==8]
 cases<-main;cases$t<-main$common;cases$long<-filter(main$long,id%in%cases$t$id);cases$common<-cases$t
 objects<-list(Main=main,Common=cases,Partial=base,NoShared=rebuild(filter(strip(main$long),!collision),cov),NoScreen=rebuild(filter(strip(no$long),id%in%noids),cov),ByID=rebuild(mutate(strip(main$long),card=card_id),cov))
 for(s in names(objects)){o<-objects[[s]];o$scenario<-s;saveRDS(o,object_path(s));core(s)}
 h<-hist(100*main$t$hb,breaks=seq(-106.25,106.25,12.5),plot=FALSE);csv(data.frame(left=head(h$breaks,-1),right=tail(h$breaks,-1),count=h$counts,share=100*h$counts/sum(h$counts)),'harshness_histogram','Main')
 # Attribute comparisons use the same Main harshness/cuts for Common teachers.
 cmp<-bind_rows(lapply(c('Main','Common'),function(s){d<-read.csv(file.path(out,s,'attributes.csv'));d$sample<-s;d}));csv(cmp,'common_sample_attributes')
 # Pooled reference checks and empirical sample assertions.
 stopifnot(all(table(main$long$id)==8),!anyDuplicated(main$long[,c('id','profile')]),max(abs(main$t$hb-main$common$hb[match(main$t$id,main$common$id)]),na.rm=TRUE)<1e-12)
 cc<-main$common;vars<-c('edad','experiencia','grupos_docencia','impact','empathy','meritocracia','resources_excess');csv(data.frame(variable=vars,sd=sapply(cc[,vars],sd)),'covariate_scales')
 ch<-read.csv(file.path(out,'Main/characterization.csv'));ch<-filter(ch,model=='X3_common',term%in%vars);ch$sd<-sapply(ch$term,function(v)sd(cc[[v]]));ch$contrast_pp<-100*ch$estimate*ch$sd;ch$contrast_se_pp<-100*ch$se*ch$sd;csv(ch,'one_sd_contrasts')
 # Composition starts from original classified population and selects new samples.
 po<-read.csv('3. Output/paper_A_final_revision/B/population_comparison.csv');rawcov<-main$t;rawcov$sex<-ifelse(is.na(rawcov$female),NA,ifelse(rawcov$female==1,'Women','Men'));rawcov$ownership<-ifelse(is.na(rawcov$public),NA,ifelse(rawcov$public==1,'Public','Private'));rawcov$age_band<-cut(rawcov$edad,c(20,25,30,35,40,45,50,55,60,65,Inf),right=FALSE,labels=c('20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65+'))
 codes<-c('02'='Albacete','13'='Ciudad Real','16'='Cuenca','19'='Guadalajara','45'='Toledo');cp<-trimws(as.character(rawcov$cp));rawcov$province<-unname(codes[ifelse(!is.na(cp)&grepl('^[0-9]{5}$',cp),substr(cp,1,2),NA)])
 pop<-po%>%select(variable,category,reference_count,reference_n,reference_pct)%>%distinct()
 margins<-bind_rows(lapply(c('main','complete'),function(co){d<-if(co=='main')rawcov else rawcov[rawcov$id%in%cc$id,];bind_rows(lapply(c('sex','ownership','age_band','province','level'),function(v){a<-as.data.frame(table(d[[v]],useNA='no'));names(a)<-c('category','sample_count');a$variable<-v;a$sample_n<-sum(!is.na(d[[v]]));a$cohort<-co;a}))}))
 margins<-left_join(margins,pop,by=c('variable','category'));margins$sample_pct<-100*margins$sample_count/margins$sample_n;margins$difference_pp<-margins$sample_pct-margins$reference_pct;csv(margins,'population_comparison','Main')
 flow<-read_json('3. Output/paper_A_final_revision/diagnostics/flow.json',simplifyVector=TRUE);flow$recoverable<-flow$analysis;flow$analysis<-2520;flow$decisions<-20160;flow$complete_covariates<-2164;flow$shared_analyzed<-sum(main$long$collision);flow$partial_excluded<-84;flow$shared_main<-sum(main$long$collision);js(flow,'flow')
 # Progression retains the eligible original cohort, independent of eight-card restriction.
 for(n in c('task_progression','timing_slope','progression_population'))file.copy(file.path('3. Output/paper_A_final_revision/B',paste0(n,'.csv')),file.path(out,'Main',paste0(n,'.csv')),overwrite=TRUE)
 js(list(input_md5=as.list(tools::md5sum(rawinputs)),main=2520,main_decisions=20160,complete=2164,complete_decisions=17312,reference='2520 full-eight teachers; Common retains Main reference and tercile cuts',excluded_partial=84,shared_main=sum(main$long$collision),baseline='8306e22'),'provenance')
 capture.output(sessionInfo(),file=file.path(out,'R_session.txt'));quit(save='no')
}
if(requested_phase=='models'){
 ans<-list()
 for(kind in names(forms))for(opt in c('nloptwrap','bobyqa')){c<-fitone('Main',kind,opt);m<-c$model;ans[[length(ans)+1]]<-data.frame(scenario='Main',model=kind,optimizer=opt,AIC=AIC(m),n=nobs(m),theta=length(getME(m,'theta')),as.list(c$diagnostics),as.list(c$shares));csv(bind_rows(ans),'covariance_candidates')}
 for(s in c('Main','Partial','NoShared','NoScreen','ByID')){c<-fitone(s,'diagonal');if(c$diagnostics['gradient']>.002||c$diagnostics['min_hessian']<=0)c<-fitone(s,'diagonal','bobyqa');stopifnot(c$diagnostics['optimizer']==0,c$diagnostics['gradient']<.002,c$diagnostics['min_hessian']>0);saveRDS(c,file.path(local,paste0('selected_',s,'.rds')));csv(data.frame(component=names(c$shares),share=as.numeric(c$shares)),'decomposition_point',s);js(list(n=nobs(c$model),teachers=nrow(geto(s)$common),kind='diagonal',diagnostics=as.list(c$diagnostics),outside01=mean(predict(c$model)<0|predict(c$model)>1),hb=unname(fixef(c$model)['hb'])),'decomposition_diagnostics',s)}
 quit(save='no')
}
if(requested_phase=='bootstrap'){
 # Reuse audited cloning/LOO/centering estimator, writing only owned outputs.
 code<-readLines('2. Code/paper_A_final_revision/models.R',encoding='UTF-8');start<-grep("^if\\(phase=='bootstrap'",code);block<-paste(code[start:length(code)],collapse='\n')
 args<-c('bootstrap','Main','199','4');phase<-'bootstrap';eval(parse(text=block));quit(save='no')
}
