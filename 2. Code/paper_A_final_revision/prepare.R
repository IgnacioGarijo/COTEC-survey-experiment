# Rebuild approved B with example-specific screen and pooled six-attribute profiles.
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
reconstruction_library_mode<-TRUE
source('2. Code/paper_A_reconstruction/reconstruct.R')
out<-'3. Output/paper_A_final_revision';local<-file.path(out,'local')
prov<-readRDS('3. Output/paper_A_reconstruction/local/provenance.rds')
r<-as.data.frame(read_parquet('1. Data/raw data/df_named.parquet'))
stopifnot(identical(r$ResponseId,prov$raw_id))
ex<-as.character(r$Ejemplo)
favorable<-ifelse(ex%in%c('1','2'),as.character(r[['Grupo 1.1_1']]),ifelse(ex%in%c('3','4'),as.character(r[['Grupo 2.1_1']]),NA))
screen<-!is.na(favorable)&favorable=='repite'
stopifnot(sum(screen)==379)
ord<-prov$ordered
ord$card_id<-ord$card
ord$profile<-apply(ord[,attrs],1,paste0,collapse='')
ord$set<-as.integer(sub('alumno_([1-8])_.*','\\1',ord$card_id))
stopifnot(all(ord$valid),all(table(ord$id)==8),all(sapply(split(ord$position,ord$id),function(x)setequal(x,1:8))))
stopifnot(!anyDuplicated(ord[,c('id','profile')]))
ord$card<-ord$profile
bl<-ord%>%filter(!id%in%r$ResponseId[screen],!is.na(response))
ll<-list(B=bl,C=filter(bl,!collision),NoScreen=filter(ord,!is.na(response)),B_full8=filter(bl,id%in%names(which(table(bl$id)==8))),ByID=mutate(bl,card=card_id))
for(s in names(ll)){
 o<-rebuild(ll[[s]],prov$cov);o$scenario<-s;saveRDS(o,object_path(s));core(s)
 csv(o$long%>%count(card_id,card,profile),'profile_identifier_mapping',s)
 message(s,': ',nrow(o$t),' teachers ',nrow(o$long),' decisions; ',nrow(o$common),' complete')
}
# Changes from archived current results are audited without changing those outputs.
a<-readRDS('3. Output/paper_A_reconstruction/local/scenario_A.rds');b<-geto('B')
cc<-inner_join(a$t%>%select(id,hb_old=hb,tercile_old=tercile),b$t%>%select(id,hb_new=hb,tercile_new=tercile),by='id')
csv(data.frame(shared=nrow(cc),correlation=cor(cc$hb_old,cc$hb_new),mean_absolute_change=mean(abs(cc$hb_old-cc$hb_new)),tercile_changes=sum(cc$tercile_old!=cc$tercile_new)),'change_audit')
# Assignment audit: raw recorded assignments, prior to outcome screen.
csv(ord%>%distinct(id,set)%>%count(set),'assignment_sets')
csv(ord%>%left_join(prov$cov%>%select(id,arm),by='id')%>%distinct(id,set,arm)%>%count(set,arm),'assignment_sets_by_arm')
csv(ord%>%count(set,position,card_id),'assignment_order')
csv(data.frame(legacy=prov$screen,example_specific=screen)%>%count(legacy,example_specific),'screen_comparison')
saveRDS(list(ordered=ord,screen=screen,raw_id=r$ResponseId,cov=prov$cov),file.path(local,'provenance.rds'))
# Progression: non-shared experimental responses only. Keep assigned-position denominator.
pop<-read.csv('3. Output/population/comparison.csv')%>%select(variable,category,reference_count,reference_n,reference_pct)%>%distinct()
for(s in c('B','C','NoScreen','B_full8')){
 o<-geto(s);t<-o$t
 eligible<-if(s=='NoScreen')unique(ord$id)else setdiff(unique(ord$id),r$ResponseId[screen])
 if(s=='B_full8')eligible<-t$id
 po<-filter(ord,id%in%eligible,!collision)
 pr<-po%>%group_by(position)%>%summarise(eligible=n(),observed=sum(!is.na(response)),response_rate=mean(!is.na(response)),n_timed=sum(is.finite(seconds)&seconds>0),median_seconds=median(seconds[is.finite(seconds)&seconds>0],na.rm=TRUE),p25=quantile(seconds[is.finite(seconds)&seconds>0],.25,na.rm=TRUE),p75=quantile(seconds[is.finite(seconds)&seconds>0],.75,na.rm=TRUE),.groups='drop');csv(pr,'task_progression',s)
 tim<-filter(po,is.finite(seconds),seconds>0,!is.na(response));tim$logseconds<-log(tim$seconds)
 tm<-feols(logseconds~position|id+card,tim,cluster=~id,notes=FALSE);csv(fe_tidy(tm,'Eligible nonshared ordered cards'),'timing_slope',s)
 csv(data.frame(eligible_records=length(eligible),nonshared_slots=nrow(po),shared_slots_excluded=sum(ord$id%in%eligible & ord$collision)),'progression_population',s)
 t$sex<-ifelse(is.na(t$female),NA,ifelse(t$female==1,'Women','Men'));t$ownership<-ifelse(is.na(t$public),NA,ifelse(t$public==1,'Public','Private'))
 t$age_band<-cut(t$edad,c(20,25,30,35,40,45,50,55,60,65,Inf),right=FALSE,labels=c('20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65+'))
 cp<-trimws(as.character(t$cp));codes<-c('02'='Albacete','13'='Ciudad Real','16'='Cuenca','19'='Guadalajara','45'='Toledo');t$province<-unname(codes[ifelse(!is.na(cp)&grepl('^[0-9]{5}$',cp),substr(cp,1,2),NA)])
 margins<-bind_rows(lapply(c('main','complete'),function(co){d<-if(co=='main')t else t[t$id%in%o$common$id,];bind_rows(lapply(c('sex','ownership','age_band','province','level'),function(v){tab<-as.data.frame(table(d[[v]],useNA='no'));names(tab)<-c('category','sample_count');tab$variable<-v;tab$sample_n<-sum(!is.na(d[[v]]));tab$cohort<-co;tab}))}))
 margins<-left_join(margins,pop,by=c('variable','category'));margins$sample_pct<-100*margins$sample_count/margins$sample_n;margins$difference_pp<-margins$sample_pct-margins$reference_pct;csv(margins,'population_comparison',s)
}
# Mutually exclusive flow, with final analytical-sample reconciliation.
fmap<-read.csv('3. Output/paper_A/reports/survey_exit_field_map.csv')
seen<-sapply(fmap$fields,function(f){cols<-strsplit(f,' | ',fixed=TRUE)[[1]];rowSums(!is.na(r[,cols,drop=FALSE]))>0})
last<-apply(seen*rep(seq_len(ncol(seen)),each=nrow(seen)),1,max);engaged<-last>=4
any<-prov$any_card;end<-prov$group_end;finish<-prov$finished;finish[is.na(finish)]<-FALSE
flow<-list(raw=nrow(r),minimal=sum(!engaged),engaged=sum(engaged),before_cards=sum(engaged&!any),any_card=sum(engaged&any),during=sum(engaged&any&!end),reached_end=sum(engaged&any&end),screen_all=sum(screen),screen_during=sum(engaged&any&!end&screen),screen_after_end=sum(engaged&any&end&screen),end_retained=sum(engaged&any&end&!screen),after_cards=sum(engaged&any&end&!screen&!finish),finished=sum(engaged&any&end&!screen&finish),analysis=nrow(b$t),decisions=nrow(b$long),partial_in_analysis=sum(!b$t$id%in%r$ResponseId[end]),complete_covariates=nrow(b$common),all8=sum(b$t$n_cards==8),partial=sum(b$t$n_cards<8),valid_sequences=n_distinct(ord$id),eligible_sequences=length(setdiff(unique(ord$id),r$ResponseId[screen])),no_analyzable=sum(!screen & any & !r$ResponseId%in%b$t$id),shared_analyzed=sum(b$long$collision))
stopifnot(flow$engaged==flow$before_cards+flow$any_card,flow$any_card==flow$during+flow$reached_end,flow$reached_end==flow$screen_after_end+flow$end_retained,flow$end_retained==flow$after_cards+flow$finished)
js(flow,'flow')
js(list(baseline='09f9cd4',scenario='B',screen='favorable example for own Ejemplo variant',reference='six binary attribute profile',confirmed_wording='complex social background',raw_md5=unname(tools::md5sum('1. Data/raw data/df_named.parquet')),R=R.version.string,lme4=as.character(packageVersion('lme4'))),'provenance')
capture.output(sessionInfo(),file=file.path(out,'R_session.txt'))
