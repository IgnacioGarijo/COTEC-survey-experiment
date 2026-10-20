# Numerical fallback starting from the candidate rather than redoing its path.
# Uses the same predeclared cases; never changes model, objective or sample.
reconstruction_library_mode<-TRUE;source('2. Code/paper_A_reconstruction/reconstruct.R',local=TRUE)
ans<-list()
for(s in c('B','C','D'))for(b in c(1L,50L,199L)){
 path<-file.path(local,paste0('optimizer_audit_',s,'_',b,'.rds'));if(!file.exists(path))next
 old<-readRDS(path)$model;z<-model.frame(old);started<-Sys.time()
 m<-suppressWarnings(update(old,start=getME(old,'theta'),control=lmerControl(optimizer='bobyqa',calc.derivs=TRUE,optCtrl=list(maxfun=100000))))
 p<-allocate(m,z);oldp<-allocate(old,z)
 ans[[length(ans)+1]]<-data.frame(scenario=s,draw=b,logLik=as.numeric(logLik(m)),gain=as.numeric(logLik(m)-logLik(old)),max_share_delta=max(abs(p-oldp)),as.list(diagnose(m)),seconds=as.numeric(difftime(Sys.time(),started,units='secs')))
 csv(bind_rows(ans),'fallback_speed_audit');message('Warm candidate fallback ',s,' ',b)
}
