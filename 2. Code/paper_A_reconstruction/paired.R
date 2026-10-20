# Paired teacher bootstrap for changes A/B/C/D. Same source-teacher draw for all scenarios.
reconstruction_library_mode<-TRUE;source('2. Code/paper_A_reconstruction/reconstruct.R',local=TRUE)
ss<-c('A','B','C','D');oo<-setNames(lapply(ss,geto),ss);universe<-sort(unique(unlist(lapply(oo,function(o)o$t$id))))
B<-999L
prepare_pair<-function(o){
 l<-o$long;t<-o$t;idx<-match(l$id,t$id);nc<-tabulate(idx,nbins=nrow(t));X<-model.matrix(reformulate(c(attrs,'failed:competence','failed:absent','failed:disruptive')),l)[,-1,drop=FALSE]
 mx<-rowsum(X,idx)/nc;my<-as.numeric(rowsum(l$y,idx))/nc
 X<-X-mx[idx,,drop=FALSE];y<-l$y-my[idx]
 cc<-complete.cases(t[,x3]);list(l=l,t=t,idx=idx,cardidx=as.integer(factor(l$card)),source=match(t$id,universe),X=X,y=y,cc=cc,charX=model.matrix(reformulate(x3),t[cc,]))
}
dd<-lapply(oo,prepare_pair)
calc<-function(d,source_weights){
 w<-source_weights[d$source];lw<-w[d$idx];l<-d$l
 fit<-function(X){V<-crossprod(X,X*lw);stopifnot(qr(V)$rank==ncol(X));setNames(as.numeric(solve(V,crossprod(X,d$y*lw))),colnames(X))}
 additive<-fit(d$X[,seq_along(attrs),drop=FALSE]);interactions<-fit(d$X)
 sm<-rowsum(l$y*lw,d$cardidx);ct<-rowsum(lw,d$cardidx);sm<-setNames(as.numeric(sm),rownames(sm));ct<-setNames(as.numeric(ct),rownames(ct))
 stopifnot(all(ct[as.character(d$cardidx[lw>0])]>1))
 ref<-(sm[as.character(d$cardidx)]-l$y)/(ct[as.character(d$cardidx)]-1);h<-as.numeric(tapply(l$y-ref,d$idx,mean));stopifnot(all(is.finite(h[w>0])))
 h[!is.finite(h)&w==0]<-0
 fitc<-lm.wfit(d$charX,h[d$cc],w[d$cc]);stopifnot(fitc$rank==ncol(d$charX));wc<-w[d$cc];hc<-h[d$cc];r2<-1-sum(wc*fitc$residuals^2)/sum(wc*(hc-weighted.mean(hc,wc))^2)
 card_dis<-2*sm*(ct-sm)/(ct*(ct-1));dis<-sum(card_dis*ct)/sum(ct)
 n<-sum(w);hsd<-sqrt((sum(w*h*h)-sum(w*h)^2/n)/(n-1))
 acad<-paste0(l$failed,l$competence);rates<-sapply(c('00','01','10','11'),function(a)sum(lw[acad==a]*l$y[acad==a])/sum(lw[acad==a]))
 c(setNames(additive,paste0('attribute:',names(additive))),setNames(interactions[grepl(':',names(interactions))],paste0('interaction:',names(interactions)[grepl(':',names(interactions))])),setNames(fitc$coefficients,paste0('characterization:',colnames(d$charX))),setNames(rates,paste0('academic_rate:',names(rates))),disagreement=dis,harshness_sd=hsd,r2_full=r2,repeat_rate=sum(lw*l$y)/sum(lw))
}
point<-bind_rows(lapply(ss,function(s){v<-calc(dd[[s]],rep(1,length(universe)));data.frame(scenario=s,metric=names(v),estimate=as.numeric(v))}));csv(point,'paired_point_metrics')
# Point validation against independently fitted fixest, lm and direct rates.
for(s in ss){p<-filter(point,scenario==s);a<-read.csv(file.path(out,s,'attributes.csv'));stopifnot(max(abs(p$estimate[match(paste0('attribute:',a$term),p$metric)]-a$estimate))<1e-9)}
rows<-list();status<-list();set.seed(20261020)
for(b in 1:B){
 w<-tabulate(sample.int(length(universe),replace=TRUE),nbins=length(universe))
 for(s in ss){res<-tryCatch(calc(dd[[s]],w),error=function(e)e);ok<-!inherits(res,'error');status[[length(status)+1]]<-data.frame(draw=b,scenario=s,accepted=ok,error=if(ok)''else conditionMessage(res));if(ok)rows[[length(rows)+1]]<-data.frame(draw=b,scenario=s,metric=names(res),estimate=as.numeric(res))}
 if(b%%100==0)message('Paired bootstrap ',b,'/',B)
}
draws<-bind_rows(rows);csv(draws,'paired_bootstrap_draws');csv(bind_rows(status),'paired_bootstrap_status')
wide<-draws%>%pivot_wider(names_from=scenario,values_from=estimate);diffs<-bind_rows(lapply(list(c('B','A'),c('C','A'),c('D','A'),c('C','B'),c('D','C')),function(pair){v<-wide[[pair[1]]]-wide[[pair[2]]];data.frame(draw=wide$draw,metric=wide$metric,contrast=paste(pair,collapse=' - '),delta=v)}))
summ<-diffs%>%group_by(metric,contrast)%>%summarise(successful=sum(is.finite(delta)),se=sd(delta,na.rm=TRUE),lo=quantile(delta,.025,na.rm=TRUE),hi=quantile(delta,.975,na.rm=TRUE),.groups='drop');summ$delta<-NA_real_
for(i in seq_len(nrow(summ))){pair<-strsplit(summ$contrast[i],' - ',fixed=TRUE)[[1]];p<-point[point$metric==summ$metric[i],];summ$delta[i]<-p$estimate[match(pair[1],p$scenario)]-p$estimate[match(pair[2],p$scenario)]}
summ$requested<-B;summ$seed<-20261020;csv(summ,'paired_differences')
js(list(draws=B,seed=20261020,source_population=length(universe),description='Union of analytical source records A/B/C/D; same source-teacher multinomial weights in each draw. References rebuilt within each scenario. Scenario sample counts may vary. Weighted within-teacher normal equations reproduce cloned FE point estimates. Paired intervals are sampling summaries, not causal effects of cleaning.'),'paired_method')
