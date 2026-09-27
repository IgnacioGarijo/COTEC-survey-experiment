# Logistic sensitivity on the probability scale: law of total variance under the model.
# Integrate over the fitted random-intercept distribution independently of observed profiles.
options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library(glmmTMB))
out<-'3. Output/paper_A_2026-09-27';m<-readRDS(file.path(out,'reports/variance_models.rds'))$logit
eta<-as.numeric(predict(m,newdata=m$frame,re.form=NA));tau<-sqrt(as.numeric(VarCorr(m)$cond$id[1]))
results<-lapply(c(30,60),function(k){
 # Golub-Welsch rule for standard normal: off-diagonal sqrt(1),...,sqrt(k-1).
 J<-matrix(0,k,k);J[cbind(1:(k-1),2:k)]<-sqrt(1:(k-1));J<-J+t(J)
 e<-eigen(J,symmetric=TRUE);q<-list(nodes=e$values,weights=e$vectors[1,]^2)
 stopifnot(abs(sum(q$weights*q$nodes^2)-1)<1e-10)
 p<-plogis(outer(eta,tau*q$nodes,'+'))
 mu<-as.vector(p%*%q$weights);ep2<-as.vector((p^2)%*%q$weights)
 vf<-mean((mu-mean(mu))^2);vu<-mean(ep2-mu^2);ve<-mean(mu-ep2);total<-vf+vu+ve
 stopifnot(abs(total-mean(mu)*(1-mean(mu)))<1e-10)
 data.frame(nodes=k,mean_probability=mean(mu),fixed_share=vf/total,teacher_share=vu/total,residual_share=ve/total)
})
ans<-do.call(rbind,results);stopifnot(max(abs(as.numeric(ans[1,-1])-as.numeric(ans[2,-1])))<1e-5)
write.csv(ans,file.path(out,'tables/logit_observed_variance.csv'),row.names=FALSE)
print(ans)
