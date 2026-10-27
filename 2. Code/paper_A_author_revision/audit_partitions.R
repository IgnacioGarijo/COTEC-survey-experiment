# Recheck all stored partitions, exporting aggregate diagnostics only.
p<-readLines('2. Code/paper_A_author_revision/prediction.R',encoding='UTF-8');eval(parse(text=p[1:(grep('^phase<-',p)-1)]))
a<-list()
for(label in c('B_random','B_ordered','C_random','NoScreen_random','Matched_random')){
 cache<-readRDS(file.path(local,paste0('prediction_',label,'.rds')));old<-read.csv(file.path(out,'tables',paste0('partition_sensitivity_',label,'.csv')))
 for(k in if(grepl('ordered',label))1 else 1:20){x<-pipeline(cache$o,split_history(cache$o,261025+k,grepl('ordered',label)));expected<-as.numeric(old[old$partition==k,names(x$stats)]);stopifnot(max(abs(x$stats-expected))<1e-11,x$rank_drop==0,x$other_warnings==0);a[[length(a)+1]]<-data.frame(scenario=label,partition=k,rank_drop=x$rank_drop,weight_warnings=x$weight_warnings,other_warnings=x$other_warnings,max_stat_error=max(abs(x$stats-expected)))}
 message('Partition audit: ',label)
}
write.csv(do.call(rbind,a),file.path(out,'tables/partition_audit.csv'),row.names=FALSE)
