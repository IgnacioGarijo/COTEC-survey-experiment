# Predeclared post-fit derivative flag check, using identical fitted-model frames.
reconstruction_library_mode<-TRUE;source('2. Code/paper_A_reconstruction/reconstruct.R',local=TRUE)
source_lines<-deparse(lme4:::optwrap)
capture.output(cat(paste(source_lines,collapse='\n')),file=file.path(out,'diagnostics','installed_lme4_optwrap.txt'))
ans<-list()
for(s in c('B','C','D'))for(b in c(1L,50L,199L)){
 old<-readRDS(file.path(local,paste0('optimizer_audit_',s,'_',b,'.rds')))$model
 z<-model.frame(old);started<-Sys.time()
 m<-suppressWarnings(lmer(form,z,REML=TRUE,control=lmerControl(optimizer='nloptwrap',calc.derivs=FALSE,optCtrl=list(maxeval=30000))))
 p<-allocate(m,z);pold<-allocate(old,z)
 ans[[length(ans)+1]]<-data.frame(scenario=s,draw=b,logLik_full=as.numeric(logLik(old)),logLik_no_derivatives=as.numeric(logLik(m)),delta_logLik=as.numeric(logLik(m)-logLik(old)),max_share_delta=max(abs(p-pold)),optimizer=as.numeric(m@optinfo$conv$opt),singular=isSingular(m),seconds=as.numeric(difftime(Sys.time(),started,units='secs')))
 csv(bind_rows(ans),'derivative_flag_audit');message('Derivative flag audit ',s,' ',b)
}
