# Teacher-level external-fold validation. Run after script 9.
options(stringsAsFactors=FALSE);Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8')
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(ranger);library(fastshap);library(ggplot2)})
out<-'3. Output/two_papers_2026-09-21';o<-readRDS(file.path(out,'reports/analysis_objects.rds'))
d<-o$t[complete.cases(o$t[,o$x3]),];l<-filter(o$long,id%in%d$id)
export<-function(x,n)write.csv(x,file.path(out,'tables',paste0(n,'.csv')),row.names=FALSE)
predwrap<-function(object,newdata)predict(object,data=newdata)$predictions
allpred<-list();shaps<-list();importance<-list();jj<-1
for(r in 1:3){set.seed(9210+r);fold<-sample(rep(1:5,length.out=nrow(d)))
 for(k in 1:5){message('Validation repeat ',r,', fold ',k)
  tr<-d[fold!=k,];te<-d[fold==k,];lr<-filter(l,id%in%tr$id);le<-filter(l,id%in%te$id)
  ref<-lr%>%group_by(card)%>%summarise(s=sum(y),n=n(),.groups='drop')
  stopifnot(all(le$card%in%ref$card),min(ref$n)>1)
  trainh<-left_join(lr,ref,by='card')%>%group_by(id)%>%summarise(target=mean(y-(s-y)/(n-1)),.groups='drop')
  testh<-left_join(le,ref,by='card')%>%group_by(id)%>%summarise(target=mean(y-s/n),.groups='drop')
  tr<-left_join(tr,trainh,by='id');te<-left_join(te,testh,by='id')
  # Composite standardization is fitted on the training fold only.
  v<-c('resources_excess','resources_ineffective');mu<-sapply(tr[,v],mean);sig<-sapply(tr[,v],sd)
  ar<-rowMeans(sweep(sweep(as.matrix(tr[,v]),2,mu,'-'),2,sig,'/'));ae<-rowMeans(sweep(sweep(as.matrix(te[,v]),2,mu,'-'),2,sig,'/'))
  te$resources<-(ae-mean(ar))/sd(ar);tr$resources<-(ar-mean(ar))/sd(ar)
  base<-mean(tr$target)
  rows<-data.frame(id=te$id,repeat_id=r,fold=k,target=te$target,baseline=base,model='Baseline',prediction=base)
  for(s in 1:3){x<-o[[paste0('x',s)]]
   ol<-lm(reformulate(x,'target'),tr);rf<-ranger(x=tr[,x],y=tr$target,num.trees=500,mtry=max(1,floor(sqrt(length(x)))),min.node.size=20,seed=10000+r*100+k*10+s,num.threads=2)
   for(m in c('OLS','RF')){z<-rows[rows$model=='Baseline',];z$model<-paste0(m,'_X',s);z$prediction<-if(m=='OLS')predict(ol,te)else predwrap(rf,te[,x]);rows<-bind_rows(rows,z)}
   # Use unique baseline rows to avoid duplication introduced by model appending.
   rows<-distinct(rows,id,model,.keep_all=TRUE)
   if(r==1&&s==3){set.seed(300+k);bg<-tr[sample(nrow(tr),min(128,nrow(tr))),x,drop=FALSE]
    sh<-fastshap::explain(rf,X=as.data.frame(bg),newdata=as.data.frame(te[,x,drop=FALSE]),pred_wrapper=predwrap,nsim=32,adjust=TRUE)
    shaps[[k]]<-data.frame(id=te$id,as.data.frame(sh),check.names=FALSE)
    base_mse<-mean((te$target-predwrap(rf,te[,x]))^2)
    importance[[k]]<-bind_rows(lapply(x,function(v){inc<-replicate(20,{xp<-te[,x];xp[[v]]<-sample(xp[[v]]);mean((te$target-predwrap(rf,xp))^2)-base_mse});data.frame(fold=k,variable=v,mse_increase=mean(inc))}))
   }
  }
  allpred[[jj]]<-rows;jj<-jj+1
 }
}
p<-bind_rows(allpred);stopifnot(nrow(p)==nrow(d)*3*7)
met<-p%>%group_by(repeat_id,model)%>%summarise(n=n(),rmse=sqrt(mean((target-prediction)^2)),mae=mean(abs(target-prediction)),r2=1-sum((target-prediction)^2)/sum((target-baseline)^2),.groups='drop');export(met,'predictive_metrics_by_repeat')
summary<-met%>%group_by(model)%>%summarise(n=first(n),rmse_min=min(rmse),rmse_max=max(rmse),rmse=mean(rmse),mae=mean(mae),r2_min=min(r2),r2_max=max(r2),r2=mean(r2),.groups='drop');export(summary,'predictive_metrics')
sh<-bind_rows(shaps)%>%pivot_longer(-id,names_to='variable',values_to='shap')
ss<-sh%>%group_by(variable)%>%summarise(mean_abs_shap=mean(abs(shap)),mean_shap=mean(shap),.groups='drop')%>%arrange(desc(mean_abs_shap));export(ss,'shap_oof')
export(bind_rows(importance)%>%group_by(variable)%>%summarise(mse_increase=mean(mse_increase),.groups='drop'),'permutation_importance_oof')
ggsave(file.path(out,'figures/prediction.png'),ggplot(summary,aes(x=reorder(model,r2),y=r2))+geom_hline(yintercept=0,color='grey60')+geom_point(size=3,color='#215e70')+geom_errorbar(aes(ymin=r2_min,ymax=r2_max),width=.15)+coord_flip()+labs(x=NULL,y='Out-of-fold R-squared (range across three partitions)')+theme_minimal(base_size=12),width=8,height=4.5,dpi=220,bg='white')
ggsave(file.path(out,'figures/shap.png'),ggplot(ss,aes(x=reorder(variable,mean_abs_shap),y=100*mean_abs_shap))+geom_col(fill='#215e70')+coord_flip()+labs(x=NULL,y='Mean absolute out-of-fold SHAP value (percentage points)')+theme_minimal(base_size=12),width=8,height=5,dpi=220,bg='white')
saveRDS(list(predictions=p,shap=sh,metrics=summary),file.path(out,'reports/prediction_objects.rds'))
message('Predictive validation completed.')


