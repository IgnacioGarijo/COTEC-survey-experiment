# Aggregate-only inputs for the explicitly hypothetical population comparison.
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(jsonlite)})
out<-'3. Output/paper_A/reports'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
o<-readRDS('3. Output/paper_A/reports/analysis_objects.rds')
d<-o$t
stopifnot(nrow(d)==2705)
age_labels<-c('20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65+')
d$age_band<-cut(d$edad,breaks=c(20,25,30,35,40,45,50,55,60,65,Inf),right=FALSE,labels=age_labels)
d$sex<-ifelse(is.na(d$female),NA,ifelse(d$female==1,'Women','Men'))
d$ownership<-ifelse(is.na(d$public),NA,ifelse(d$public==1,'Public','Private'))
codes<-c('02'='Albacete','13'='Ciudad Real','16'='Cuenca','19'='Guadalajara','45'='Toledo')
cp<-trimws(as.character(d$cp))
valid<-!is.na(cp)&grepl('^[0-9]{5}$',cp)
d$province<-unname(codes[ifelse(valid,substr(cp,1,2),NA)])
d$cp_status<-ifelse(is.na(cp)|cp=='','Missing',ifelse(!valid,'Invalid format',ifelse(is.na(d$province),'Outside region','Mapped')))
# Do not use IP geolocation or cp_auto as a school-location proxy.
summ<-function(z){
 counts<-function(x){tb<-table(x,useNA='no');as.list(setNames(as.integer(tb),names(tb)))}
 vars<-c('sex','ownership','age_band','province','level')
 marg<-setNames(lapply(vars,function(v)list(counts=counts(z[[v]]),observed=sum(!is.na(z[[v]])),missing=sum(is.na(z[[v]])))),vars)
 strata<-bind_rows(lapply(c('Public','Private','Primary','Secondary'),function(s){a<-if(s%in%c('Public','Private'))z[z$ownership==s&!is.na(z$ownership),] else z[z$level==s&!is.na(z$level),];data.frame(stratum=s,n=nrow(a),women=sum(a$sex=='Women',na.rm=TRUE),sex_n=sum(!is.na(a$sex)),public=sum(a$ownership=='Public',na.rm=TRUE),ownership_n=sum(!is.na(a$ownership)))}))
 list(n=nrow(z),marginals=marg,strata=strata,cp_status=counts(z$cp_status),age=list(mean=mean(z$edad,na.rm=TRUE),median=median(z$edad,na.rm=TRUE),min=min(z$edad,na.rm=TRUE),max=max(z$edad,na.rm=TRUE)),dates=range(as.character(z$start),na.rm=TRUE),infantil_selected=sum(!is.na(z$infantil)))
}
ans<-list(main=summ(d),complete=summ(d[complete.cases(d[,o$x3]),]),notes='Revised manuscript cohorts, 27 September 2026. Postal code is cleaned reported school code, not cp_imputado or geolocation.',input_md5=unname(tools::md5sum('3. Output/paper_A/reports/analysis_objects.rds')))
stopifnot(ans$complete$n==nrow(o$common))
write_json(ans,file.path(out,'sample_aggregates.json'),pretty=TRUE,auto_unbox=TRUE,na='null')
cat(toJSON(list(main_n=ans$main$n,complete_n=ans$complete$n,main=ans$main$marginals,cp=ans$main$cp_status,dates=ans$main$dates),pretty=TRUE,auto_unbox=TRUE))
