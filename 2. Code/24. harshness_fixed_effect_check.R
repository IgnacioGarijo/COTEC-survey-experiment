# Internal diagnostic requested 2026-09-26; no manuscript changes.
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(fixest);library(ggplot2)})
setFixest_nthreads(2)
out <- '3. Output/harshness_fixed_effect_check_2026-09-26'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
o <- readRDS('3. Output/two_papers_2026-09-21/reports/analysis_objects.rds')
attrs <- c('male','background','failed','competence','absent','disruptive')
eligible <- o$t$id[o$t$n_cards>=2]
l <- o$long[o$long$id%in%eligible,]
stopifnot(nrow(l)==22006,length(unique(l$id))==2587)
m <- feols(y~male+background+failed+competence+absent+disruptive|id,l,cluster=~id)
old <- read.csv('3. Output/two_papers_2026-09-22/tables/attributes.csv')
old <- old[old$model=='All',]
stopifnot(max(abs(coef(m)[old$term]-old$estimate))<1e-10)
fe <- fixef(m)$id
l$net <- l$y-drop(as.matrix(l[,attrs])%*%coef(m)[attrs])
d <- l%>%group_by(id)%>%summarise(alpha=mean(net),n_cards=n(),.groups='drop')%>%left_join(o$t%>%select(id,hb),by='id')
stopifnot(!anyNA(d),max(abs(d$alpha-unname(fe[d$id])))<1e-10)
# Center both across the same teachers; do not change the manuscript's H^b.
d$alpha_centered <- d$alpha-mean(d$alpha)
d$hb_centered <- d$hb-mean(d$hb)
stats <- bind_rows(lapply(c('All FE teachers','At least eight responses'),function(s){z<-if(s=='All FE teachers')d else filter(d,n_cards>=8);data.frame(sample=s,n=nrow(z),pearson=cor(z$hb,z$alpha),spearman=cor(z$hb,z$alpha,method='spearman'),hb_mean=mean(z$hb),alpha_mean=mean(z$alpha),hb_sd=sd(z$hb),alpha_sd=sd(z$alpha),centered_difference_rmse=sqrt(mean(((z$hb-mean(z$hb))-(z$alpha-mean(z$alpha)))^2)))}))
write.csv(stats,file.path(out,'comparison_summary.csv'),row.names=FALSE)
write.csv(data.frame(term=names(coef(m)),estimate=unname(coef(m))),file.path(out,'attribute_coefficients.csv'),row.names=FALSE)
r<-stats$pearson[1]
p<-ggplot(d,aes(100*hb_centered,100*alpha_centered))+geom_abline(slope=1,intercept=0,linetype='dashed',color='grey45')+geom_point(alpha=.22,size=1.2,color='#215e70')+coord_equal()+theme_minimal(base_size=12)+labs(title='Harshness and estimated teacher fixed effects',subtitle=sprintf('Internal diagnostic | N = %s | Pearson r = %.3f',format(nrow(d),big.mark=','),r),x='Harshness H^b, centered (percentage points)',y='Teacher fixed effect, centered (percentage points)',caption='Both measures use the same recorded decisions. Dashed line: equality.\nCentering uses the same 2,587 teachers; H^b retains the original full-sample card references.')
ggsave(file.path(out,'harshness_vs_fixed_effect.png'),p,width=8,height=6,dpi=180,bg='white')
z<-d%>%select(hb_centered,alpha_centered)%>%pivot_longer(everything(),names_to='measure',values_to='value')
z$measure<-factor(z$measure,levels=c('hb_centered','alpha_centered'),labels=c('Harshness H^b','Teacher fixed effect'))
p2<-ggplot(z,aes(100*value))+geom_histogram(binwidth=5,boundary=0,fill='#215e70',color='white')+facet_wrap(~measure,ncol=1)+theme_minimal(base_size=12)+labs(title='Distributions on the same teacher sample',subtitle='Internal diagnostic | 2,587 teachers with at least two responses',x='Centered estimate (percentage points)',y='Teachers',caption='Equal teacher weights. These are distributions of estimated scores, including estimation noise.')
ggsave(file.path(out,'distributions.png'),p2,width=8,height=6,dpi=180,bg='white')
writeLines(sub('[[:space:]]+$','',capture.output(sessionInfo())),file.path(out,'R_session.txt'))
write.csv(data.frame(input=c('3. Output/two_papers_2026-09-21/reports/analysis_objects.rds','2. Code/24. harshness_fixed_effect_check.R'),md5=unname(tools::md5sum(c('3. Output/two_papers_2026-09-21/reports/analysis_objects.rds','2. Code/24. harshness_fixed_effect_check.R')))),file.path(out,'input_manifest.csv'),row.names=FALSE)
print(stats)
