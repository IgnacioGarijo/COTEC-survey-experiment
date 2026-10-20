# Independent numerical objective check for the B point fit (bobyqa fallback).
reconstruction_library_mode<-TRUE;source('2. Code/paper_A_reconstruction/reconstruct.R',local=TRUE)
o<-geto('B');z<-prepare_model(o$long,o$t);old<-readRDS(file.path(local,'point_B.rds'))$model
candidate<-suppressWarnings(lmer(form,z,REML=TRUE,control=lmerControl(optimizer='nloptwrap',calc.derivs=TRUE,optCtrl=list(maxeval=30000,xtol_abs=1e-8,ftol_abs=1e-9))))
p<-allocate(candidate,z);pold<-allocate(old,z)
csv(data.frame(saved_logLik=as.numeric(logLik(old)),automatic_logLik=as.numeric(logLik(candidate)),automatic_minus_saved_logLik=as.numeric(logLik(candidate)-logLik(old)),max_share_delta=max(abs(p-pold)),as.list(diagnose(candidate))),'B_point_start_audit')
saveRDS(candidate,file.path(local,'B_point_alternative_start.rds'))
