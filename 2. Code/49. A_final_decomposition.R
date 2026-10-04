# Approved model and teacher bootstrap, rebuilding harshness references each draw.
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(lme4);library(jsonlite)})
out <- '3. Output/paper_A'
o <- readRDS(file.path(out,'reports/analysis_objects.rds'))
attrs <- c('male','background','failed','competence','absent','disruptive')
blocks <- list(Student=c(attrs,apply(combn(attrs,2),2,paste,collapse=':')),
 Professional=o$x1,Beliefs=setdiff(o$x3,o$x1),Harshness='hb')
f <- as.formula(paste('y~',paste(unlist(blocks),collapse='+'),'+(0+',paste(paste0(attrs,'_c'),collapse='+'),'|id)'))
base_long <- o$long[,c('id','card','y',attrs)]
prepare <- function(l,t) {
 z<-left_join(l,t[,c('id',o$x3,'hb')],by='id');z<-z[complete.cases(z),];z$id<-factor(z$id)
 for(v in attrs)z[[paste0(v,'_c')]]<-z[[v]]-mean(z[[v]])
 z
}
fit <- function(z) suppressWarnings(lmer(f,z,REML=TRUE,control=lmerControl(optimizer='nloptwrap',calc.derivs=TRUE,optCtrl=list(maxeval=30000))))
allocate <- function(m,z) {
 eta<-as.numeric(predict(m,re.form=NA));ff<-nobars(formula(m));X<-model.matrix(ff,z)
 labs<-attr(terms(ff),'term.labels');groups<-lapply(blocks,function(a)which(attr(X,'assign')%in%match(a,labs)))
 K<-length(blocks);val<-numeric(2^K);shap<-numeric(K)
 for(mask in 0:(2^K-1)) {
  sel<-which(as.logical(intToBits(mask)[seq_len(K)]));cols<-unique(c(1,unlist(groups[sel])))
  val[mask+1]<-var(lm.fit(X[,cols,drop=FALSE],eta)$fitted.values)
 }
 for(j in seq_len(K))for(mask in 0:(2^K-1)) {
  sel<-which(as.logical(intToBits(mask)[seq_len(K)]));if(j%in%sel)next;s<-length(sel)
  shap[j]<-shap[j]+factorial(s)*factorial(K-s-1)/factorial(K)*(val[bitwOr(mask,bitwShiftL(1,j-1))+1]-val[mask+1])
 }
 names(shap)<-names(blocks);S<-as.matrix(VarCorr(m)$id);Z<-as.matrix(z[,colnames(S)])
 v<-c(shap,Sensitivities=mean(rowSums((Z%*%S)*Z)),Residual=sigma(m)^2)
 stopifnot(abs(sum(shap)-var(eta))<1e-8,all(v>=0))
 v/sum(v)
}
diagnose <- function(m) {
 g<-m@optinfo$derivs$gradient;H<-m@optinfo$derivs$Hessian
 sc<-tryCatch(solve(chol(H),g),error=function(e)rep(Inf,length(g)))
 c(singular=as.numeric(isSingular(m)),gradient=max(pmin(abs(sc),abs(g))),
   min_hessian=min(eigen(H,symmetric=TRUE)$values),optimizer=as.numeric(m@optinfo$conv$opt),
   min_covariance=min(eigen(as.matrix(VarCorr(m)$id),symmetric=TRUE)$values))
}
z<-prepare(base_long,o$t);stopifnot(nrow(z)==18608,nlevels(z$id)==2157)
cache<-file.path(out,'reports/final_decomposition_model.rds')
if(file.exists(cache)) m<-readRDS(cache) else {m<-fit(z);saveRDS(m,cache)}
p<-allocate(m,z);di<-diagnose(m)
if(di['gradient']>=.002 || di['optimizer']!=0 || di['min_hessian']<=0) {
 m<-suppressWarnings(update(m,control=lmerControl(optimizer='bobyqa',calc.derivs=TRUE,optCtrl=list(maxfun=100000))))
 saveRDS(m,cache);p<-allocate(m,z);di<-diagnose(m)
}
stopifnot(di['singular']==0,di['optimizer']==0,di['gradient']<.002,di['min_hessian']>0)
old<-read.csv(file.path(out,'reports/all_attributes_harshness_partition.csv'))
stopifnot(max(abs(p-unlist(old$share[old$model=='Harshness_six' & old$component!='General'])))<1e-4)
write.csv(data.frame(component=names(p),share=as.numeric(p)),file.path(out,'tables/final_decomposition.csv'),row.names=FALSE)
write_json(list(n=nrow(z),teachers=nlevels(z$id),fixed_rank=qr(getME(m,'X'))$rank,
 covariance_parameters=length(getME(m,'theta')),diagnostics=as.list(di),
 harshness_coefficient=unname(fixef(m)['hb']),outside_01=mean(predict(m)<0|predict(m)>1),
 denominator=var(predict(m,re.form=NA))+mean(rowSums((as.matrix(z[,paste0(attrs,'_c')])%*%as.matrix(VarCorr(m)$id))*as.matrix(z[,paste0(attrs,'_c')])))+sigma(m)^2),
 file.path(out,'reports/final_decomposition_diagnostics.json'),pretty=TRUE,auto_unbox=TRUE)
print(p);print(di)
B<-as.integer(Sys.getenv('A_BOOTSTRAP_DRAWS','199'));workers<-as.integer(Sys.getenv('A_BOOTSTRAP_WORKERS','2'))
if(B>0) {
 t<-o$t;l<-base_long;idx<-match(l$id,t$id);cidx<-as.integer(factor(l$card));by_teacher<-split(seq_len(nrow(l)),idx)
 run_one<-function(b) {
  set.seed(1004000+b);draw<-sample.int(nrow(t),replace=TRUE);w<-tabulate(draw,nbins=nrow(t));lw<-w[idx]
  ct<-as.numeric(rowsum(lw,cidx));sm<-as.numeric(rowsum(l$y*lw,cidx))
  ref<-(sm[cidx]-l$y)/(ct[cidx]-1);h<-tapply(l$y-ref,idx,mean)
  tb<-t[draw,];tb$id<-as.character(seq_along(draw));tb$hb<-as.numeric(h[as.character(draw)])
  rows<-unlist(by_teacher[as.character(draw)],use.names=FALSE);lb<-l[rows,]
  lb$id<-rep(tb$id,lengths(by_teacher[as.character(draw)]))
  zz<-prepare(lb,tb)
  tryCatch({
   # Bootstrap requires refitting, not a costly numerical Hessian for every draw.
   # The point fit has full derivative diagnostics. Use its covariance as a warm start,
   # tight objective tolerances, and record optimizer status and boundary fits here.
   mm<-suppressWarnings(lmer(f,zz,REML=TRUE,start=theta_start,
     control=lmerControl(optimizer='nloptwrap',calc.derivs=FALSE,
       optCtrl=list(maxeval=30000,xtol_abs=1e-8,ftol_abs=1e-9))))
   dg<-c(singular=as.numeric(isSingular(mm)),gradient=NA_real_,min_hessian=NA_real_,
         optimizer=as.numeric(mm@optinfo$conv$opt),
         min_covariance=min(eigen(as.matrix(VarCorr(mm)$id),symmetric=TRUE)$values))
   data.frame(draw=b,as.list(allocate(mm,zz)),as.list(dg),error='')
  },error=function(e)data.frame(draw=b,error=conditionMessage(e)))
 }
 path<-file.path(out,'tables/final_decomposition_bootstrap.csv')
 prior<-if(file.exists(path))read.csv(path)else data.frame(draw=integer())
 if('error'%in%names(prior))prior<-prior[is.finite(prior$Student) & (is.na(prior$error)|prior$error==''),]
 todo<-setdiff(seq_len(B),prior$draw)
 cl<-parallel::makeCluster(workers)
 parallel::clusterEvalQ(cl,{suppressPackageStartupMessages({library(dplyr);library(lme4)});NULL})
 theta_start<-getME(m,'theta')
 parallel::clusterExport(cl,c('t','l','idx','cidx','by_teacher','o','attrs','blocks','f','prepare','fit','allocate','diagnose','run_one','theta_start'),envir=environment())
 for(start in seq.int(1,max(1,length(todo)),by=10)) {
  if(!length(todo))break
  ids<-todo[start:min(start+9,length(todo))]
  ans<-parallel::parLapply(cl,ids,run_one);prior<-bind_rows(prior,bind_rows(ans));write.csv(prior,path,row.names=FALSE)
  message('Bootstrap saved: ',nrow(prior),' / ',B)
 }
 parallel::stopCluster(cl)
 keep<-(is.na(prior$error)|prior$error=='') & prior$optimizer==0 & is.finite(prior$Student)
 keep[is.na(keep)]<-FALSE
 if(sum(keep)>=.95*B) {
  se<-sapply(prior[keep,names(p)],sd)
  write.csv(data.frame(component=names(p),share=as.numeric(p),se=se,successful=sum(keep),requested=B),file.path(out,'tables/final_decomposition.csv'),row.names=FALSE)
 } else warning('Insufficient numerical bootstrap success; no SE published')
}
