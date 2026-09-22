options(stringsAsFactors=FALSE);Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8')
suppressPackageStartupMessages(library(dplyr))
src<-'3. Output/two_papers_2026-09-21';out<-'3. Output/two_papers_2026-09-22';o<-readRDS(file.path(src,'reports/analysis_objects.rds'));z<-readRDS(file.path(out,'reports/deepened_objects.rds'));l<-z$long;t<-z$t
rd<-function(n)read.csv(file.path(out,'tables',paste0(n,'.csv')))
stopifnot(nrow(t)==2705,nrow(l)==22124,!anyDuplicated(l[,c('id','card')]))
h<-l%>%group_by(card)%>%mutate(rebuilt=y-(sum(y)-y)/(n()-1))%>%group_by(id)%>%summarise(h=mean(rebuilt),.groups='drop')
stopifnot(max(abs(h$h-t$hb[match(h$id,t$id)]))<1e-12,abs(sum(t$hb*t$n_cards))<1e-10)
c<-rd('coherence_permutation');stopifnot(abs(c$observed_sd[1]-sd(t$hb))<1e-12)
a<-rd('academic_categories');stopifnot(sum(a$decisions)==nrow(l),abs(weighted.mean(a$estimate,a$decisions)-mean(l$y))<1e-10)
s<-rd('ranking_complete_shares');stopifnot(all(abs((s%>%group_by(side,arm)%>%summarise(s=sum(share)))$s-1)<1e-12),sum(unique(s[,c('arm','n')])$n)==2546)
stopifnot(all(is.finite(z$bootstrap)),nrow(z$bootstrap)==1999)
# The response-weighted profile model must assign total weight one to every teacher.
stopifnot(all(abs((l%>%group_by(id)%>%summarise(w=sum(teacher_weight)))$w-1)<1e-12))
if(file.exists(file.path(out,'reports/nested_prediction_objects.rds'))){p<-readRDS(file.path(out,'reports/nested_prediction_objects.rds'))$predictions
stopifnot(!anyNA(p$prediction),!anyDuplicated(p[,c('id','population','repeat_id','model')]),all((p%>%group_by(population,repeat_id,id)%>%summarise(n=n_distinct(target),.groups='drop'))$n==1))
stopifnot(all((p%>%filter(population=='Common')%>%count(repeat_id,model))$n==2156),all((p%>%filter(population=='X1 available')%>%count(repeat_id,model))$n==2641))
}
writeLines(c('PASS: unchanged retained sample and unique teacher-card rows.','PASS: score reconstructed independently and exact response-weighted centering.','PASS: permutation observed SD equals original SD.','PASS: academic-category counts and means reconcile.','PASS: favorite and least distributions each sum to one; N=2546.','PASS: 1999 finite bootstrap draws.','PASS: teacher-level total weights equal one.','Prediction validation executed when prediction file present.'),file.path(out,'reports/validation_deepening.txt'))
message('Deepening validation passed')
