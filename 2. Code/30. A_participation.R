# Aggregate flow, module availability, recorded task progression; no new exclusions.
options(stringsAsFactors=FALSE);invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(arrow);library(dplyr);library(tidyr);library(fixest);library(jsonlite)})
out<-'3. Output/paper_A';o<-readRDS(file.path(out,'reports/analysis_objects.rds'));t<-o$t
r<-as.data.frame(read_parquet('1. Data/raw data/df_named.parquet'));clean<-as.data.frame(read_parquet('1. Data/processed data/cleandata.parquet'))
export<-function(x,n)write.csv(x,file.path(out,'tables',paste0(n,'.csv')),row.names=FALSE,na='')
filt<-c('Grupo 1.1_1','Grupo 2.1_1','Grupo 5.1_1','Grupo 8.1_1')
excluded<-rowSums(r[,filt]=='repite',na.rm=TRUE)>0
stopifnot(setequal(r$ResponseId[!excluded],clean$id))
in_sample<-r$ResponseId%in%t$id;complete<-r$ResponseId%in%o$common$id
final_start<-!is.na(r[['time_iniciobloque3_Page Submit']]);finished<-r$Finished=='True';finished[is.na(finished)]<-FALSE
belief_complete<-complete.cases(t[,setdiff(o$x3,o$x1)])
flow<-data.frame(stage=c('Raw survey records','Screening exclusions','Retained records','Retained without decisions','Teachers with decisions','With all selected belief items','Complete analysis covariates'),n=c(nrow(r),sum(excluded),nrow(clean),sum(!excluded&!in_sample),sum(in_sample),sum(belief_complete),sum(complete)))
export(flow,'sample_flow')
export(data.frame(group=c('All records','Teachers with decisions','Complete covariates'),n=c(nrow(r),sum(in_sample),sum(complete)),final_module_flag=c(sum(final_start),sum(final_start&in_sample),sum(final_start&complete)),finished=c(sum(finished),sum(finished&in_sample),sum(finished&complete))),'module_availability')
export(as.data.frame(table(t$n_cards))%>%setNames(c('responses','teachers')),'response_counts')
# Compare observables of respondents with versus without selected final belief items.
sel<-bind_rows(lapply(c('edad','experiencia','female','public','hb'),function(v){a<-t[[v]][belief_complete];b<-t[[v]][!belief_complete];data.frame(variable=v,n_with=sum(!is.na(a)),n_without=sum(!is.na(b)),with=mean(a,na.rm=TRUE),without=mean(b,na.rm=TRUE),smd=(mean(a,na.rm=TRUE)-mean(b,na.rm=TRUE))/sqrt((var(a,na.rm=TRUE)+var(b,na.rm=TRUE))/2))}))
export(sel,'belief_selection')
cols<-grep('^FL_[0-9]+_DO_Grupo[1-8]\\.[1-8]$',names(r),value=TRUE)
ord<-r%>%select(id=ResponseId,all_of(cols))%>%pivot_longer(-id,names_to='field',values_to='position')%>%filter(!is.na(position))%>%mutate(position=as.numeric(position),card=sub('^.*_DO_Grupo([1-8])\\.([1-8])$','alumno_\\1_\\2',field))
valid<-ord%>%group_by(id)%>%summarise(valid=n()==8&n_distinct(card)==8&setequal(position,1:8),.groups='drop')
ord<-filter(ord,id%in%valid$id[valid$valid]);ord$rawrow<-match(ord$id,r$ResponseId)
response_field <- function(c) {
 if(c=='alumno_3_5')return('Grupo 5_1')
 if(c=='alumno_6_1')return('Grupo 6_1')
 sub('alumno_([1-8])_([1-8])','Grupo \\1.\\2_1',c)
}
stopifnot(all(vapply(unique(ord$card),response_field,character(1)) %in% names(r)))
ord$response<-mapply(function(a,c)r[[response_field(c)]][a],ord$rawrow,ord$card)
ord$seconds<-mapply(function(a,c)as.numeric(r[[sub('alumno_([1-8])_([1-8])','time_grupo\\1.\\2_Page Submit',c)]][a]),ord$rawrow,ord$card)
ord$retained<-ord$id%in%clean$id
ord$ambiguous_example<-ord$card%in%c('alumno_1_1','alumno_2_1','alumno_2_8','alumno_3_8','alumno_5_1','alumno_8_1')
export(ord%>%filter(!ambiguous_example)%>%group_by(position)%>%summarise(eligible=n(),observed=sum(!is.na(response)),response_rate=mean(!is.na(response)),n_timed=sum(is.finite(seconds)&seconds>0),median_seconds=median(seconds[is.finite(seconds)&seconds>0],na.rm=TRUE),p25=quantile(seconds[is.finite(seconds)&seconds>0],.25,na.rm=TRUE),p75=quantile(seconds[is.finite(seconds)&seconds>0],.75,na.rm=TRUE),.groups='drop'),'task_progression')
tim<-ord%>%filter(!ambiguous_example,is.finite(seconds),seconds>0,!is.na(response));tim$logseconds<-log(tim$seconds)
tm<-feols(logseconds~position|id+card,tim,cluster=~id,notes=FALSE)
export(data.frame(term='position',estimate=coef(tm),lo=confint(tm)[,1],hi=confint(tm)[,2],n=nobs(tm),teachers=n_distinct(tim$id)),'timing_slope')
export(data.frame(valid_raw=n_distinct(ord$id),valid_retained=n_distinct(ord$id[ord$retained]),timed_responses=nrow(tim),ambiguous_excluded=sum(ord$ambiguous_example)),'progression_support')
write_json(list(raw=nrow(r),excluded=sum(excluded),retained=nrow(clean),sample=sum(in_sample),common=sum(complete),belief_complete=sum(belief_complete)),file.path(out,'reports/flow_summary.json'),pretty=TRUE,auto_unbox=TRUE)
message('Participation analysis completed')
