# Additional analyses agreed in the editorial comparison. Existing samples/coding retained.
options(stringsAsFactors=FALSE);Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8')
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(fixest);library(sandwich);library(ggplot2)})
setFixest_nthreads(2)
src<-'3. Output/shared';out<-'3. Output/paper_B'
for(s in c('tables','figures','reports'))dir.create(file.path(out,s),recursive=TRUE,showWarnings=FALSE)
for(n in c('policy_broad','policy_cells','availability_by_arm')) stopifnot(file.copy(file.path(src,'figures',paste0(n,'.png')),file.path(out,'figures'),overwrite=TRUE))
o<-readRDS(file.path(src,'reports/analysis_objects.rds'));t<-o$t;l<-o$long;r<-o$rawt
attrs<-c('male','background','failed','competence','absent','disruptive');broad<-list(c('Policy','Control'),c('Revelation','Policy'),c('Awareness','Revelation'))
export<-function(x,n)if (n %in% jsonlite::read_json('2. Code/artifact_manifest.json')$paper_B$tables) write.csv(x,file.path(out,'tables',paste0(n,'.csv')),row.names=FALSE)
tidy<-function(m,label,fe=FALSE){V<-if(fe)vcov(m)else vcovHC(m,type='HC3');b<-coef(m)[rownames(V)];s<-sqrt(diag(V));df<-if(fe)fixest::degrees_freedom(m,type='t') else df.residual(m);data.frame(model=label,term=names(b),estimate=b,se=s,lo=b-qt(.975,df)*s,hi=b+qt(.975,df)*s,p=2*pt(-abs(b/s),df),n=nobs(m),row.names=NULL)}
saveplot<-function(p,n,w=8,h=5)if (n %in% jsonlite::read_json('2. Code/artifact_manifest.json')$paper_B$figures) ggsave(file.path(out,'figures',paste0(n,'.png')),p,width=w,height=h,dpi=220,bg='white')

# Four observed academic categories; no extrapolated complete factorial profiles.
l$academic<-factor(paste0(l$failed,l$competence),levels=c('00','01','10','11'),labels=c('Few failures / adequate competence','Few failures / low competence','Many failures / adequate competence','Many failures / low competence'))
am<-feols(y~0+academic,l,cluster=~id)
ag<-tidy(am,'Observed academic categories',TRUE)
dd<-l%>%group_by(academic,card)%>%summarise(n=n(),s=sum(y),.groups='drop')%>%mutate(dis=2*s*(n-s)/(n*(n-1)))%>%group_by(academic)%>%summarise(decisions=sum(n),identifiers=n(),disagreement=weighted.mean(dis,n),.groups='drop')
ag<-bind_cols(dd,ag%>%select(estimate,se,lo,hi));export(ag,'academic_categories')
im<-feols(y~male+background+failed+competence+absent+disruptive+failed:competence+failed:absent+failed:disruptive|id,l,cluster=~id)
it<-tidy(im,'Academic interaction model',TRUE);it$p_holm<-NA;ix<-grepl(':',it$term);it$p_holm[ix]<-p.adjust(it$p[ix],'holm');export(it,'academic_interactions')
saveplot(ggplot(ag,aes(100*estimate,reorder(academic,estimate)))+geom_errorbar(aes(xmin=100*lo,xmax=100*hi),orientation='y',width=.1)+geom_point(size=3,color='#215e70')+labs(x='Observed repetition recommendations (%) and 95% intervals',y=NULL)+theme_minimal(base_size=11),'academic_categories',9,4)

# Restore Hd, preserving the three school-ownership categories rather than binary public.
stopifnot('titularidad'%in%names(t));l$ownership<-t$titularidad[match(l$id,t$id)]
gd<-glm(reformulate(c(attrs,'level','ownership'),'y'),data=l,family=binomial(),na.action=na.exclude)
l$hd_dev<-l$y-predict(gd,newdata=l,type='response');hd<-l%>%group_by(id)%>%summarise(hd=if(all(is.na(hd_dev)))NA_real_ else mean(hd_dev,na.rm=TRUE),.groups='drop');t<-left_join(t,hd,by='id')
# Treatment changes in academic slopes, teacher FE absorb arm main effects.
slopes<-list();joints<-list()
for(pair in broad){z<-filter(l,arm%in%pair);z$treated<-as.integer(z$arm==pair[1]);m<-feols(y~male+background+failed+competence+absent+disruptive+treated:failed+treated:competence|id,z,cluster=~id)
 a<-tidy(m,paste(pair,collapse=' - '),TRUE)%>%filter(grepl('treated',term));slopes[[pair[1]]]<-a;nn<-a$term;b<-coef(m)[nn];V<-vcov(m)[nn,nn];W<-drop(t(b)%*%solve(V,b));joints[[pair[1]]]<-data.frame(contrast=paste(pair,collapse=' - '),chi2=W,df=2,p=pchisq(W,2,lower.tail=FALSE))}
export(bind_rows(slopes)%>%mutate(p_holm=p.adjust(p,'holm')),'policy_academic_slopes');export(bind_rows(joints)%>%mutate(p_holm=p.adjust(p,'holm')),'policy_academic_joint')
rob<-list();for(pair in broad){z<-filter(l,arm%in%pair);z$treated<-as.integer(z$arm==pair[1]);z$w<-1/t$n_cards[match(z$id,t$id)];for(spec in c('Profile FE','Profile FE teacher weights')){m<-feols(y~treated:failed+treated:competence|id+card,z,weights=if(spec=='Profile FE')NULL else ~w,cluster=~id);rob[[paste(pair[1],spec)]]<-tidy(m,paste(paste(pair,collapse=' - '),spec),TRUE)}};export(bind_rows(rob),'policy_academic_robustness')

# Favorite AND least-favorite distributions, all retained ranking respondents.
pref<-filter(t,!is.na(favorite),!is.na(least));shares<-list();cons<-list()
for(side in c('favorite','least'))for(a in levels(t$arm))for(k in 1:3){z<-filter(pref,arm==a);n<-nrow(z);s<-sum(z[[side]]==k);p<-s/n;den<-1+1.96^2/n;mid<-(p+1.96^2/(2*n))/den;half<-1.96*sqrt(p*(1-p)/n+1.96^2/(4*n^2))/den;shares[[paste(side,a,k)]]<-data.frame(side=side,arm=a,policy=k,n=n,count=s,share=p,lo=mid-half,hi=mid+half)}
for(side in c('favorite','least'))for(pair in broad)for(k in 1:3){z<-filter(pref,arm%in%pair);z$selected<-as.integer(z[[side]]==k);z$treated<-as.integer(z$arm==pair[1]);cons[[paste(side,pair[1],k)]]<-tidy(lm(selected~treated,z),paste(side,k,paste(pair,collapse=' - ')))%>%filter(term=='treated')}
export(bind_rows(shares),'ranking_complete_shares');export(bind_rows(cons)%>%mutate(p_holm=p.adjust(p,'holm')),'ranking_complete_contrasts')

# Match teacher weights while adding identifier FE.
l$teacher_weight<-1/t$n_cards[match(l$id,t$id)]
wr<-list();for(pair in broad){z<-filter(l,arm%in%pair);z$treated<-as.integer(z$arm==pair[1]);m<-feols(y~treated|card,z,weights=~teacher_weight,cluster=~id);wr[[pair[1]]]<-tidy(m,paste(pair,collapse=' - '),TRUE)};export(bind_rows(wr),'policy_teacherweighted_profiles')

# Missing-outcome sensitivity on Ha: two independent deviations from each arm's observed mean.
rr<-read.csv(file.path(src,'tables/selection_outcome_bounds.csv'));sens<-list()
for(pair in broad){a<-rr[rr$arm==pair[1],];b<-rr[rr$arm==pair[2],];g<-expand.grid(delta_treated=seq(-.3,.3,.025),delta_reference=seq(-.3,.3,.025));g$contrast<-paste(pair,collapse=' - ');g$effect<-a$r*a$mean_ha+(1-a$r)*pmax(0,pmin(1,a$mean_ha+g$delta_treated))-b$r*b$mean_ha-(1-b$r)*pmax(0,pmin(1,b$mean_ha+g$delta_reference));sens[[pair[1]]]<-g}
sens<-bind_rows(sens);export(sens,'missing_outcome_grid');saveplot(ggplot(sens,aes(100*delta_reference,100*delta_treated,fill=100*effect))+geom_tile()+geom_contour(aes(z=effect),breaks=0,color='black')+facet_wrap(~contrast)+scale_fill_gradient2(low='#2878a5',mid='white',high='#b65332')+labs(x='Missing minus observed mean in reference arm (pp)',y='Missing minus observed mean in treated arm (pp)',fill='Effect (pp)')+theme_minimal(base_size=10),'missing_outcome_grid',10,4.8)

# Restore precision for every alignment contrast, without changing its causal status.
aa<-read.csv(file.path(src,'tables/alignment.csv'));ap<-read.csv(file.path(src,'tables/alignment_preassignment.csv'));ap<-filter(ap,grepl('alignment',term),!grepl(':',term));ap$contrast<-paste(ap$model,ap$term);ap$family<-'Pre-assignment ranking';ap$lo90<-ap$estimate-qt(.95,ap$n-4)*ap$se;ap$hi90<-ap$estimate+qt(.95,ap$n-4)*ap$se
if(!'lo90'%in%names(aa)){aa$lo90<-aa$estimate-1.645*aa$se;aa$hi90<-aa$estimate+1.645*aa$se}
aa<-bind_rows(aa,ap);aa$mde80<-(qnorm(.975)+qnorm(.8))*aa$se;for(v in c(2,3,5))aa[[paste0('equivalent_',v)]]<-aa$lo90> -v/100 & aa$hi90<v/100;export(aa,'alignment_precision_complete')

# Assignment-population teacher bootstrap: refit selection model AND pooled profile references.
ip<-r;for(v in c('age','female','experience','secondary','public')){ip[[paste0(v,'_missing')]]<-as.integer(is.na(ip[[v]]));ip[[v]][is.na(ip[[v]])]<-median(ip[[v]],na.rm=TRUE)}
sf<-score_observed~arm+age+female+experience+secondary+public+age_missing+female_missing+experience_missing+secondary_missing+public_missing
lid<-match(l$id,t$id);lc<-match(l$card,unique(l$card));ncards<-max(lc);t_raw<-match(t$id,ip$id);counts<-tabulate(lid,nbins=nrow(t))
set.seed(92220);B<-1999;boot<-matrix(NA_real_,B,6)
for(b in seq_len(B)){w<-tabulate(sample.int(nrow(ip),nrow(ip),replace=TRUE),nbins=nrow(ip));tw<-w[t_raw];lw<-tw[lid];ns<-as.numeric(rowsum(lw,lc,reorder=TRUE));ss<-as.numeric(rowsum(lw*l$y,lc,reorder=TRUE));dev<-l$y-(ss[lc]-l$y)/(ns[lc]-1);h<-as.numeric(rowsum(dev,lid,reorder=TRUE))/counts
 sm<-suppressWarnings(glm(sf,ip,family=binomial(),weights=w));pr<-pmax(suppressWarnings(predict(sm,newdata=ip,type='response'))[t_raw],.05);stopifnot(sm$converged,all(is.finite(pr)))
 means<-sapply(levels(t$arm),function(a)weighted.mean(h[t$arm==a],tw[t$arm==a]));im<-sapply(levels(t$arm),function(a)weighted.mean(h[t$arm==a],(tw/pr)[t$arm==a]));boot[b,]<-c(sapply(broad,function(p)means[p[1]]-means[p[2]]),sapply(broad,function(p)im[p[1]]-im[p[2]]));if(b%%250==0)message('Bootstrap ',b,'/',B)
}
br<-bind_rows(lapply(1:6,function(j)data.frame(method=if(j<=3)'Rebuilt reference'else'Rebuilt reference + reestimated IPW',contrast=paste(broad[[(j-1)%%3+1]],collapse=' - '),lo=quantile(boot[,j],.025),hi=quantile(boot[,j],.975),lo90=quantile(boot[,j],.05),hi90=quantile(boot[,j],.95),replicates=B)));br$equivalent_3<-br$lo90>-.03 & br$hi90<.03;export(br,'policy_bootstrap_refitted')
saveRDS(list(t=t,long=l,bootstrap=boot),file.path(out,'reports/deepened_objects.rds'))
capture.output(sessionInfo(),file=file.path(out,'reports/R_session_deepening.txt'))
message('All deepening analyses complete')
