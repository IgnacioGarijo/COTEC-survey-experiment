# Numerical and provenance invariants. No canonical output is written.
reconstruction_library_mode<-TRUE;source('2. Code/paper_A_reconstruction/reconstruct.R',local=TRUE)
checks<-list();add<-function(name,ok,value=NA_real_,tolerance=NA_real_){checks[[length(checks)+1]]<<-data.frame(check=name,passed=isTRUE(ok),value=value,tolerance=tolerance)}
oldroot<-'3. Output/paper_A'
for(n in c('attributes','academic_interactions','characterization','four_outcomes','attributes_by_tercile')){
 old<-read.csv(file.path(oldroot,'tables',paste0(n,'.csv')));new<-read.csv(file.path(out,'A',paste0(n,'.csv')))
 if(n=='attributes')old<-filter(old,model=='All')
 keys<-paste(old$model,old$term);ix<-match(keys,paste(new$model,new$term));add(paste0(n,' keys'),!anyNA(ix))
 for(v in intersect(c('estimate','se','lo','hi','r2','n'),intersect(names(old),names(new)))){delta<-max(abs(old[[v]]-new[[v]][ix]),na.rm=TRUE);add(paste(n,v),delta<1e-8,delta,1e-8)}
}
old<-read.csv(file.path(oldroot,'tables','academic_categories.csv'));new<-read.csv(file.path(out,'A','academic_categories.csv'));ix<-match(old$academic,new$academic)
for(v in c('decisions','identifiers','estimate','se','lo','hi','disagreement')){delta<-max(abs(old[[v]]-new[[v]][ix]));add(paste('academic categories',v),delta<1e-10,delta,1e-10)}
old<-read.csv(file.path(oldroot,'tables','final_decomposition.csv'));new<-read.csv(file.path(out,'A','decomposition_point.csv'));delta<-max(abs(old$share-new$share[match(old$component,new$component)]));add('A mixed shares',delta<1e-4,delta,1e-4)
ss<-c(scenarios,'C_example_screen');prov<-readRDS(file.path(local,'provenance.rds'))
for(s in ss){
 o<-geto(s);l<-o$long;t<-o$t
 add(paste(s,'unique teacher/card'),!anyDuplicated(l[,c('id','card')]))
 add(paste(s,'all finite scores'),all(is.finite(t$hb))&&all(abs(t$hb)<=1+1e-12))
 bins<-hist(t$hb,breaks=seq(-1.0625,1.0625,by=.125),plot=FALSE);csv(data.frame(midpoint=bins$mids,count=bins$counts,share=bins$counts/nrow(t)),'harshness_distribution',s)
 add(paste(s,'weighted score zero'),abs(sum(t$n_cards*t$hb))<1e-8,abs(sum(t$n_cards*t$hb)),1e-8)
 if(s!='A')add(paste(s,'at most eight answers'),max(t$n_cards)<=8,max(t$n_cards),8)
 if(s%in%c('C','C_full8','C_inclusive','C_example_screen'))add(paste(s,'no collisions'),!any(l$collision))
 if(s=='D')add('D no collision teachers',all(t$id%in%prov$teacher_design$id[prov$teacher_design$collisions==0]))
 # Actual alternative-logit numerical diagnostics, not inherited claims.
 g<-glm(reformulate(attrs,'y'),l,family=binomial());gd<-glm(reformulate(c(attrs,'level','ownership'),'y'),l,family=binomial(),na.action=na.exclude)
 js(list(hc_converged=g$converged,hd_converged=gd$converged,hc_rank=g$rank,hd_rank=gd$rank,hc_finite=all(is.finite(coef(g))),hd_finite=all(is.finite(coef(gd)))),'alternative_logit_diagnostics',s)
 add(paste(s,'alternative logit convergence'),g$converged&&gd$converged)
 pt<-readRDS(file.path(local,paste0('point_',s,'.rds')));m<-pt$model;z<-prepare_model(l,t);S<-as.matrix(VarCorr(m)$id);Z<-as.matrix(z[,colnames(S)]);eta<-predict(m,re.form=NA);sens<-mean(rowSums((Z%*%S)*Z));T<-var(eta)+sens+sigma(m)^2;p<-allocate(m,z)
 csv(data.frame(component=names(p),variance=as.numeric(p*T),share=as.numeric(p),denominator=T),'decomposition_unnormalized',s)
 d<-diagnose(m);graw<-m@optinfo$derivs$gradient;H<-m@optinfo$derivs$Hessian;scaled<-tryCatch(solve(chol(H),graw),error=function(e)rep(Inf,length(graw)))
 js(list(status=pt$status,diagnostics=as.list(d),max_raw_gradient=max(abs(graw)),max_scaled_gradient=max(abs(scaled)),combined_gradient_definition='max(pmin(abs(scaled),abs(raw)))',final_lme4_messages=m@optinfo$conv$lme4$messages,logLik=as.numeric(logLik(m)),covariance_eigenvalues=eigen(S,symmetric=TRUE)$values,model_formula=paste(deparse(formula(m)),collapse=''),object_md5=pt$fingerprint),'point_audit',s)
 add(paste(s,'fixed rank'),qr(model.matrix(nobars(form),z))$rank==34)
 add(paste(s,'allocation sum'),abs(sum(p)-1)<1e-10,abs(sum(p)-1),1e-10)
}
# Explicit clone expansion cross-check of the weighted LOO bootstrap formula.
o<-geto('C');t<-o$t;l<-o$long;idx<-match(l$id,t$id);ci<-as.integer(factor(l$card));set.seed(20261021);draw<-sample.int(nrow(t),replace=TRUE);w<-tabulate(draw,nbins=nrow(t));lw<-w[idx];sm<-rowsum(l$y*lw,ci);ct<-rowsum(lw,ci);sm<-setNames(as.numeric(sm),rownames(sm));ct<-setNames(as.numeric(ct),rownames(ct));h<-tapply(l$y-(sm[as.character(ci)]-l$y)/(ct[as.character(ci)]-1),idx,mean)
by_teacher<-split(seq_len(nrow(l)),idx);rows<-unlist(by_teacher[as.character(draw)],use.names=FALSE);expanded<-l[rows,c('card','y')];expanded$clone<-rep(seq_along(draw),lengths(by_teacher[as.character(draw)]));expanded<-expanded%>%group_by(card)%>%mutate(dev=y-(sum(y)-y)/(n()-1))%>%ungroup();hexp<-expanded%>%group_by(clone)%>%summarise(h=mean(dev),.groups='drop');delta<-max(abs(hexp$h-as.numeric(h[as.character(draw)])));add('bootstrap weighted vs expanded clone LOO',delta<1e-12,delta,1e-12)
vv<-prov$ordered;add('provenance has 8 rows per valid record',all(table(vv$id)==8));add('provenance valid positions',all(vapply(split(vv$position,vv$id),function(x)setequal(x,1:8),logical(1))))
csv(bind_rows(checks),'verification_checks');stopifnot(all(vapply(checks,function(c)c$passed,logical(1))));message(length(checks),' checks passed')
