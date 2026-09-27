# A-only updated nested validation. Excess-resource item; no school tenure.
options(stringsAsFactors=FALSE);Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8')
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(ranger);library(fastshap);library(ggplot2)})
o<-readRDS('3. Output/paper_A_2026-09-27/reports/analysis_objects.rds');out<-'3. Output/paper_A_2026-09-27'
export<-function(x,n)write.csv(x,file.path(out,'tables',paste0(n,'.csv')),row.names=FALSE)
prepare<-function(tr,te){
 lr<-o$long[o$long$id%in%tr$id,];le<-o$long[o$long$id%in%te$id,];ref<-lr%>%group_by(card)%>%summarise(s=sum(y),n=n(),.groups='drop');stopifnot(all(le$card%in%ref$card),min(ref$n)>1)
 ht<-left_join(lr,ref,by='card')%>%group_by(id)%>%summarise(target=mean(y-(s-y)/(n-1)),.groups='drop');he<-left_join(le,ref,by='card')%>%group_by(id)%>%summarise(target=mean(y-s/n),.groups='drop')
 tr<-left_join(tr,ht,by='id');te<-left_join(te,he,by='id')
 list(tr=tr,te=te)
}
ridgepred<-function(tr,te,x,lambda){a<-model.matrix(reformulate(x),tr)[,-1,drop=FALSE];b<-model.matrix(reformulate(x),te)[,-1,drop=FALSE];mu<-colMeans(a);sig<-apply(a,2,sd);sig[sig==0]<-1;a<-scale(a,mu,sig);b<-scale(b,mu,sig);ym<-mean(tr$target);drop(b%*%solve(crossprod(a)/nrow(a)+diag(lambda,ncol(a)),crossprod(a,tr$target-ym)/nrow(a))+ym)}
forest<-function(tr,x,mtry,node,seed,trees=500)ranger(x=tr[,x,drop=FALSE],y=tr$target,num.trees=trees,mtry=mtry,min.node.size=node,seed=seed,num.threads=2)
predwrap<-function(object,newdata)predict(object,data=newdata)$predictions
preds<-list();tuning<-list();shaps<-list();perms<-list();counter<-1
for(pop in c('Common','X1 available')){
 x<-if(pop=='Common')o$x3 else o$x1;d<-o$t[complete.cases(o$t[,x]),];grid<-expand.grid(mtry=unique(c(floor(sqrt(length(x))),length(x))),node=c(5,20,50));lambdas<-10^seq(-4,2,length.out=10)
 for(re in 1:5){set.seed(92230+re);fold<-sample(rep(1:5,length.out=nrow(d)))
 for(k in 1:5){message(pop,': repeat ',re,', outer fold ',k);train<-d[fold!=k,];test<-d[fold==k,];set.seed(5000+re*10+k);inner<-sample(rep(1:3,length.out=nrow(train)));rferr<-matrix(0,3,nrow(grid));rierr<-matrix(0,3,length(lambdas))
 for(j in 1:3){p<-prepare(train[inner!=j,],train[inner==j,]);for(g in 1:nrow(grid)){m<-forest(p$tr,x,grid$mtry[g],grid$node[g],6000+k*100+j*10+g,250);rferr[j,g]<-mean((p$te$target-predwrap(m,p$te[,x,drop=FALSE]))^2)};for(g in seq_along(lambdas))rierr[j,g]<-mean((p$te$target-ridgepred(p$tr,p$te,x,lambdas[g]))^2)}
 g<-which.min(colMeans(rferr));lambda<-lambdas[which.min(colMeans(rierr))];p<-prepare(train,test);tr<-p$tr;te<-p$te;m<-forest(tr,x,grid$mtry[g],grid$node[g],7000+re*10+k)
 tuning[[counter]]<-data.frame(population=pop,repeat_id=re,fold=k,mtry=grid$mtry[g],node=grid$node[g],lambda=lambda)
 base<-data.frame(id=te$id,population=pop,repeat_id=re,fold=k,target=te$target,baseline=mean(tr$target),model='Baseline',prediction=mean(tr$target))
 rows<-list(base);mods<-list(RF_tuned=predwrap(m,te[,x,drop=FALSE]),Ridge=ridgepred(tr,te,x,lambda))
 for(s in if(pop=='Common')1:3 else 1)mods[[paste0('OLS_X',s)]]<-predict(lm(reformulate(o[[paste0('x',s)]],'target'),tr),te)
 for(n in names(mods)){z<-base;z$model<-n;z$prediction<-mods[[n]];rows[[n]]<-z};preds[[counter]]<-bind_rows(rows);counter<-counter+1
 if(pop=='Common'&&re==1){set.seed(922+k);bg<-tr[sample(nrow(tr),128),x,drop=FALSE];sh<-fastshap::explain(m,X=as.data.frame(bg),newdata=as.data.frame(te[,x,drop=FALSE]),pred_wrapper=predwrap,nsim=32,adjust=TRUE);shaps[[k]]<-data.frame(id=te$id,as.data.frame(sh),check.names=FALSE)
 mse<-mean((te$target-predwrap(m,te[,x]))^2);perms[[k]]<-bind_rows(lapply(x,function(v){inc<-replicate(20,{xp<-te[,x];xp[[v]]<-xp[[v]][sample.int(nrow(xp))];mean((te$target-predwrap(m,xp))^2)-mse});data.frame(variable=v,fold=k,mse_increase=mean(inc))}))}
 }
 }
}
p<-bind_rows(preds);met<-p%>%group_by(population,repeat_id,model)%>%summarise(n=n(),rmse=sqrt(mean((target-prediction)^2)),mae=mean(abs(target-prediction)),r2=1-sum((target-prediction)^2)/sum((target-baseline)^2),.groups='drop')
export(met,'nested_prediction_repeats');export(met%>%group_by(population,model)%>%summarise(n=first(n),rmse=mean(rmse),mae=mean(mae),r2_min=min(r2),r2_max=max(r2),r2=mean(r2),.groups='drop'),'nested_prediction_summary');export(bind_rows(tuning),'nested_tuning')
# Corrected repeated-CV intervals: approximate, accounts for overlap, not an exact CI.
fm<-p%>%group_by(population,repeat_id,fold,model)%>%summarise(mse=mean((target-prediction)^2),.groups='drop')%>%pivot_wider(names_from=model,values_from=mse)
cmp<-list();for(pop in unique(fm$population)){z<-filter(fm,population==pop);for(pair in list(c('RF_tuned',if(pop=='Common')'OLS_X3'else'OLS_X1'),c('Ridge',if(pop=='Common')'OLS_X3'else'OLS_X1'),c(if(pop=='Common')'OLS_X3'else'OLS_X1','Baseline'))){dif<-z[[pair[1]]]-z[[pair[2]]];se<-sqrt((1/length(dif)+1/4)*var(dif));e<-mean(dif);cmp[[paste(pop,paste(pair,collapse='-'))]]<-data.frame(population=pop,contrast=paste(pair,collapse=' - '),mse_difference=e,se=se,lo=e-qt(.975,length(dif)-1)*se,hi=e+qt(.975,length(dif)-1)*se,p=2*pt(-abs(e/se),length(dif)-1),folds=length(dif))}}
export(bind_rows(cmp),'prediction_paired_corrected');sh<-bind_rows(shaps)%>%pivot_longer(-id,names_to='variable',values_to='shap');export(sh%>%group_by(variable)%>%summarise(mean_abs_shap=mean(abs(shap)),.groups='drop'),'nested_shap');export(bind_rows(perms)%>%group_by(variable)%>%summarise(mse_increase=mean(mse_increase),.groups='drop'),'nested_permutation_importance')
# Directional display: quartile means of SHAP for numeric covariates, level means for factors.
dirs<-list();for(v in o$x3){z<-filter(sh,variable==v);val<-o$t[[v]][match(z$id,o$t$id)];z$group<-if(is.numeric(val)&&length(unique(val))>5)paste0('Q',pmin(4,ceiling(rank(val,ties.method='average')/length(val)*4)))else as.character(val);dirs[[v]]<-z%>%group_by(variable,group)%>%summarise(n=n(),mean_shap=mean(shap),.groups='drop')};export(bind_rows(dirs),'nested_shap_direction')
ggsave(file.path(out,'figures/nested_shap_direction.png'),ggplot(filter(bind_rows(dirs),variable%in%c('edad','experiencia','resources_excess','meritocracia')),aes(group,100*mean_shap))+geom_col(fill='#215e70')+facet_wrap(~variable)+labs(x='Within-variable quartile (low to high)',y='Mean held-out SHAP contribution (pp)')+theme_minimal(),width=8,height=5,dpi=220,bg='white')
saveRDS(list(predictions=p,shap=sh),file.path(out,'reports/nested_prediction_objects.rds'));message('Nested prediction complete')

