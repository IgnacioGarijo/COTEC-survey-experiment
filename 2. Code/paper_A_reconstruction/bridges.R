# Separate changes in the source-teacher composition from changes in responses.
# Each side rebuilds its own references and scores in the shared teacher pool.
reconstruction_library_mode<-TRUE
source('2. Code/paper_A_reconstruction/reconstruct.R',local=TRUE)
results<-list();summaries<-list()
for(pair in list(c('A','B'),c('A','C'),c('B','C'),c('C','D'))){
 contrast<-paste(pair,collapse=' versus ');a<-geto(pair[1]);b<-geto(pair[2]);ids<-intersect(a$t$id,b$t$id)
 for(s in pair){
  o<-geto(s)
  l<-o$long%>%filter(id%in%ids)%>%select(-any_of(c('level','titularidad','ownership','arm','tercile','hc_dev','hd_dev')))
  cov<-o$t%>%select(-any_of(c('hb','ha','n_cards','tercile','hc','hd')))
  ob<-rebuild(l,cov)
  m<-feols(reformulate(paste0(paste(attrs,collapse='+'),'|id'),'y'),ob$long,cluster=~id,notes=FALSE)
  results[[length(results)+1]]<-fe_tidy(m,s)%>%mutate(contrast=contrast,source=s)
  summaries[[length(summaries)+1]]<-data.frame(contrast=contrast,source=s,teachers=nrow(ob$t),decisions=nrow(ob$long),mean_repeat=mean(ob$long$y),mean_hb=mean(ob$t$hb),sd_hb=sd(ob$t$hb),common_teachers=nrow(ob$common))
 }
}
csv(bind_rows(results),'shared_teacher_attributes')
csv(bind_rows(summaries),'shared_teacher_summaries')
message('Shared teacher bridges saved; references rebuilt separately on each side.')
