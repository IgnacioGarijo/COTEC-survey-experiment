# Read-only audit of experimental order versus shared example fields.
# Run from repository root. Does not change analysis data or manuscripts.
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
sink('3. Output/paper_A/reports/experimental_order_audit_2026_10_19.txt')
cat('AUDIT 19 October 2026. Counts are records, not deduplicated people.\n')
suppressPackageStartupMessages({library(arrow);library(dplyr);library(tidyr)})
r<-as.data.frame(read_parquet('1. Data/raw data/df_named.parquet'))
for(e in parse('2. Code/0. main.R',encoding='UTF-8'))if(is.call(e)&&identical(e[[1]],as.name('<-'))&&grepl('^vector_',as.character(e[[2]])))eval(e)
cards<-as.vector(outer(1:8,1:8,function(x,y)paste0('alumno_',x,'_',y)))
a<-data.frame(card=cards,male=as.integer(cards%in%get(ls(pattern='^vector_ni')[1])),background=as.integer(cards%in%vector_extranjero),failed=as.integer(cards%in%vector_suspensos),competence=as.integer(cards%in%vector_carencias),absent=as.integer(cards%in%vector_absentista),disruptive=as.integer(cards%in%vector_expulsion))
cols<-grep('^FL_[0-9]+_DO_Grupo[1-8]\\.[1-8]$',names(r),value=TRUE)
o<-r%>%mutate(row=row_number())%>%select(row,all_of(cols))%>%pivot_longer(-row,names_to='field',values_to='position')%>%filter(!is.na(position))%>%mutate(position=as.numeric(position),card=sub('^.*_DO_Grupo([1-8])\\.([1-8])$','alumno_\\1_\\2',field))
v<-o%>%group_by(row)%>%summarise(n=n(),valid=n()==8&&n_distinct(card)==8&&setequal(position,1:8),.groups='drop');cat('ORDER\n');print(table(v$n,v$valid))
rf<-function(c){if(c=='alumno_3_5')return('Grupo 5_1');if(c=='alumno_6_1')return('Grupo 6_1');sub('alumno_([1-8])_([1-8])','Grupo \\1.\\2_1',c)}
l<-bind_rows(lapply(cards,function(c)data.frame(row=seq_len(nrow(r)),card=c,response=r[[rf(c)]])))%>%filter(!is.na(response))
vo<-o%>%filter(row%in%v$row[v$valid]);extra<-anti_join(l%>%filter(row%in%v$row[v$valid]),vo,by=c('row','card'))
cat('OUTSIDE ORDER\n');print(extra%>%count(card)%>%left_join(a,by='card'))
cat('BY EXAMPLE\n');print(extra%>%mutate(example=r$Ejemplo[row])%>%count(example,card))
joined<-left_join(vo,l,by=c('row','card'));s<-joined%>%group_by(row)%>%summarise(answered=sum(!is.na(response)),.groups='drop');cat('ANSWERED ORDERED\n');print(table(s$answered));cat('completion marker\n');print(table(s$answered,r$B2_complet[s$row],useNA='ifany'))
cat('EXTREME ATTRIBUTES\n');print(a[rowSums(a[,4:7])%in%c(0,4),])
clean<-read_parquet('1. Data/processed data/cleandata.parquet');cat('Retained valid:',sum(r$ResponseId[s$row]%in%clean$id),' all8:',sum(s$answered==8 & r$ResponseId[s$row]%in%clean$id),'\n')
cat('No order with response:',length(setdiff(unique(l$row),v$row[v$valid])),'\n')

o<-o%>%mutate(example=r$Ejemplo[row],shared=ifelse(example%in%c('1','2'),card%in%c('alumno_1_1','alumno_3_8'),ifelse(example%in%c('3','4'),card%in%c('alumno_2_1','alumno_2_8'),NA)))
cat('Shared rows / teachers:',sum(o$shared,na.rm=TRUE),n_distinct(o$row[o$shared%in%TRUE]),'\n')
print(o%>%group_by(row)%>%summarise(nshared=sum(shared),.groups='drop')%>%count(nshared))
cat('all8 no collision:',sum(s$answered==8 & !s$row%in%o$row[o$shared%in%TRUE]),'\n')
cat('EXTRA original exclusions retained in analysis\n');print(extra%>%filter(!card%in%c('alumno_1_1','alumno_2_1'))%>%count(card))
cat('No-order patterns\n');print(l%>%filter(!row%in%o$row)%>%count(card))
cat('Repeated text field candidates\n');print(names(r)[grepl('Grupo [123]\\.[18]|Ejemplo|[Cc]alibr',names(r))])
cat('Missing order responses despite shared\n');print(left_join(o,l,by=c('row','card'))%>%group_by(shared)%>%summarise(n=n(),missing=sum(is.na(response))))

m<-readRDS('3. Output/paper_A/reports/analysis_objects.rds'); ii<-which(r$ResponseId%in%m$t$id);cat('current',nrow(m$t),'validorder',sum(ii%in%s$row),'noorder',sum(!ii%in%s$row),'\n');cat('current old used example-only observations:',sum(extra$row%in%ii & extra$card%in%c('alumno_2_8','alumno_3_8')),'\n');cat('current all8 in order:',sum(s$row%in%ii & s$answered==8),'\n')

cat('
INTERPRETATION
Order identifies the eight allocated cards, not proof of eight completed answers.
The four outside-order profiles and their association with Ejemplo support the example mapping.
Shared fields do not reveal whether the saved value came from the example or the later task.
All 706 shared fields are nonmissing, including partial records: nonmissing is not proof of a later answer.
The analysis currently retains high-risk example-only responses. Restricting to ordered cards requires reanalysis, not just a count correction.
No manuscript or canonical data were changed by this audit.
')
sink()
