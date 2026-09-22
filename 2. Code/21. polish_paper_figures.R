# Presentation-only rendering from frozen aggregate tables; no model reestimation.
options(stringsAsFactors=FALSE)
suppressPackageStartupMessages(library(ggplot2))
out<-'3. Output/two_papers_2026-09-22'
labs<-c(edad='Age (years)',experiencia='Teaching experience (years)',meritocracia='Effort attribution',resources='Resource skepticism')
d<-read.csv(file.path(out,'tables/characterization_curves.csv'));d$variable<-factor(d$variable,levels=names(labs),labels=unname(labs))
ggsave(file.path(out,'figures/characterization_curves.png'),ggplot(d,aes(x,100*mean))+geom_errorbar(aes(ymin=100*(mean-1.96*se),ymax=100*(mean+1.96*se)),width=0)+geom_point(color='#215e70')+facet_wrap(~variable,scales='free_x')+labs(x=NULL,y='Mean relative propensity (percentage points)')+theme_minimal(base_size=13),width=9,height=6,dpi=220,bg='white')
d<-read.csv(file.path(out,'tables/nested_shap_direction.csv'));d<-d[d$variable%in%names(labs),];d$variable<-factor(d$variable,levels=names(labs),labels=unname(labs))
ggsave(file.path(out,'figures/nested_shap_direction.png'),ggplot(d,aes(group,100*mean_shap))+geom_col(fill='#215e70')+facet_wrap(~variable)+labs(x='Within-variable quartile (low to high)',y='Mean held-out SHAP contribution (pp)')+theme_minimal(base_size=13),width=8,height=5,dpi=220,bg='white')
