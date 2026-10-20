# Optional aggregate export for figures.R. Reads isolated local scores only.
# Existing CSVs are validated and never overwritten; no individual row is saved.
# Rscript --vanilla "2. Code/paper_A_reconstruction/export_harshness.R"
options(stringsAsFactors=FALSE)
script_arg<-grep('^--file=',commandArgs(FALSE),value=TRUE)
stopifnot(length(script_arg)==1)
script_path<-normalizePath(sub('^--file=','',script_arg),winslash='/',mustWork=TRUE)
root<-normalizePath(file.path(dirname(script_path),'..','..'),winslash='/',mustWork=TRUE)
out<-file.path(root,'3. Output/paper_A_reconstruction')
main<-c('A','B','C','D')
breaks<-seq(-1.0625,1.0625,by=.125)
midpoints<-seq(-1,1,by=.125)
stopifnot(file.exists(file.path(out,'.gitignore')),
 grepl('local/',paste(readLines(file.path(out,'.gitignore'),warn=FALSE),collapse='')))
for(s in main){
 path<-file.path(out,s,'harshness_distribution.csv')
 if(file.exists(path)){
  z<-read.csv(path)
  stopifnot(all(c('midpoint','share')%in%names(z)),
   isTRUE(all.equal(z$midpoint,midpoints,tolerance=1e-12)),abs(sum(z$share)-1)<1e-8)
  message(s,': existing aggregate retained')
  next
 }
 object<-readRDS(file.path(out,'local',paste0('scenario_',s,'.rds')))
 hb<-object$t$hb
 stopifnot(length(hb)>0,all(is.finite(hb)),all(hb>=-1&hb<=1))
 bins<-hist(hb,breaks=breaks,right=FALSE,include.lowest=TRUE,plot=FALSE)
 stopifnot(sum(bins$counts)==length(hb),isTRUE(all.equal(bins$mids,midpoints,tolerance=1e-12)))
 z<-data.frame(left=head(breaks,-1),right=tail(breaks,-1),midpoint=midpoints,
  count=bins$counts,share=bins$counts/length(hb),teachers=length(hb))
 write.csv(z,path,row.names=FALSE)
 message(s,': exported ',nrow(z),' aggregate bins; no respondent rows')
 rm(object,hb)
}
