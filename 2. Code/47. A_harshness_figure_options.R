# Internal Figure 2 alternatives; never overwrites the manuscript figure.
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
suppressPackageStartupMessages({library(ggplot2);library(dplyr)})
out <- '3. Output/paper_A/reports'
o <- readRDS(file.path(out,'analysis_objects.rds'));t <- o$t
x <- 100*t$hb
stopifnot(length(x)==2705,all(is.finite(x)),!anyDuplicated(t$id))
summary <- data.frame(n=length(x),mean=mean(x),sd=sd(x),min=min(x),max=max(x),
 q10=unname(quantile(x,.1)),q25=unname(quantile(x,.25)),median=median(x),q75=unname(quantile(x,.75)),q90=unname(quantile(x,.9)),
 skewness=mean((x-mean(x))^3)/mean((x-mean(x))^2)^1.5,
 fd_width=2*IQR(x)/length(x)^(1/3),nrd0_bw=bw.nrd0(x),SJ_bw=bw.SJ(x),
 median_one_answer_step=median(100/t$n_cards))
write.csv(summary,file.path(out,'harshness_figure_summary.csv'),row.names=FALSE)
counts <- t%>%count(n_cards,name='teachers')
write.csv(counts,file.path(out,'harshness_figure_response_counts.csv'),row.names=FALSE)
print(summary);print(counts)

theme_set(theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),
 panel.grid.major.x=element_blank(),plot.title=element_text(face='bold'),
 plot.subtitle=element_text(color='#555555'),strip.text=element_text(face='bold'),
 plot.caption=element_text(hjust=0,color='#555555'),plot.margin=margin(12,18,12,12)))
col <- '#215e70'; accent <- '#c16b29'
lims <- c(-100,75)
specs <- data.frame(width=c(5,10,12.5,15),label=c('A. Barras de 5 puntos (actual)','B. Barras de 10 puntos','C. Barras de 12,5 puntos','D. Barras de 15 puntos'))
histdata <- bind_rows(lapply(seq_len(nrow(specs)),function(i){
 w<-specs$width[i];br<-seq(-ceiling(100/w)*w-w/2,ceiling(100/w)*w+w/2,by=w)
 h<-hist(x,breaks=br,plot=FALSE)
 stopifnot(sum(h$counts)==length(x))
 data.frame(left=head(h$breaks,-1),right=tail(h$breaks,-1),density=h$density,label=specs$label[i])
}))
p <- ggplot(histdata,aes(xmin=left,xmax=right,ymin=0,ymax=density))+
 geom_rect(fill=col,color='white',linewidth=.3)+geom_vline(xintercept=0,linetype=2,color='#777777',linewidth=.4)+
 facet_wrap(~label,ncol=2)+coord_cartesian(xlim=lims)+
 scale_x_continuous(breaks=c(-75,-50,-25,0,25,50,75))+
 labs(title='La forma depende del ancho de las barras',subtitle='Mismos 2.705 profesores; barras centradas en cero y escala vertical común',
 x='Harshness (puntos porcentuales)',y='Densidad',caption='Las áreas, no las alturas, permiten comparar intervalos de distinto ancho. No se añaden observaciones.')
ggsave(file.path(out,'harshness_histogram_options.png'),p,width=11,height=7,dpi=180,bg='white')

# Bandwidth sensitivity includes automatic selectors and a transparent coarser scale.
bws <- c('Sheather-Jones'=bw.SJ(x),'Regla automática'=bw.nrd0(x),'5 puntos'=5,'7,5 puntos'=7.5)
dens <- bind_rows(lapply(names(bws),function(n){
 d<-density(x,bw=bws[[n]],from=-100,to=100,n=2048)
 data.frame(x=d$x,density=d$y,label=paste0(n,' (h = ',format(round(bws[[n]],2),nsmall=2),')'))
}))
p <- ggplot(dens,aes(x,density,color=label))+geom_line(linewidth=.8)+
 coord_cartesian(xlim=lims)+scale_color_manual(values=c('#c16b29','#215e70','#7e5794','#737373'))+
 labs(title='Suavizar no demuestra que la distribución sea normal',subtitle='Sensibilidad de la curva al ancho de banda; núcleo gaussiano, sin imponer una distribución normal',
 x='Harshness (puntos porcentuales)',y='Densidad',color=NULL)+theme(legend.position='bottom')
ggsave(file.path(out,'harshness_density_options.png'),p,width=10,height=5.3,dpi=180,bg='white')

# A transparent descriptive candidate: wide bins, no fitted normal curve.
w <- 12.5;br <- seq(-106.25,106.25,by=w);h <- hist(x,breaks=br,plot=FALSE)
hh <- data.frame(left=head(br,-1),right=tail(br,-1),share=100*h$counts/length(x))
stopifnot(sum(h$counts)==length(x),abs(sum(hh$share)-100)<1e-8)
p <- ggplot(hh,aes(xmin=left,xmax=right,ymin=0,ymax=share))+
 geom_rect(fill=col,color='white',linewidth=.5)+geom_vline(xintercept=0,linetype=2,color='#555555',linewidth=.55)+
 coord_cartesian(xlim=lims)+scale_x_continuous(breaks=c(-75,-50,-25,0,25,50,75))+
 labs(title='Distribution of teacher harshness',subtitle='2,705 teachers · Each teacher receives equal weight',
 x='Harshness (percentage points)',y='Teachers (%)',
 caption='Bins: 12.5 percentage points, centred on zero. Dashed line: same-card reference.\nNegative values: fewer retention recommendations than peers; positive values: more.')
ggsave(file.path(out,'harshness_histogram_candidate.png'),p,width=8.3,height=4.7,dpi=220,bg='white')

# Optional overlay, on a density scale so areas are comparable.
dd <- density(x,bw=5,from=-100,to=100,n=2048)
p <- ggplot(hh,aes(xmin=left,xmax=right,ymin=0,ymax=share/(100*w)))+
 geom_rect(fill=col,alpha=.35,color='white',linewidth=.5)+
 geom_line(data=data.frame(x=dd$x,y=dd$y),aes(x=x,y=y),inherit.aes=FALSE,color=col,linewidth=1)+
 geom_vline(xintercept=0,linetype=2,color='#777777',linewidth=.4)+
 coord_cartesian(xlim=lims)+scale_x_continuous(breaks=c(-75,-50,-25,0,25,50,75))+
 labs(title='Distribution of teacher harshness',subtitle='2,705 teachers · Histogram and smoothed density',
 x='Harshness (percentage points)',y='Density',
 caption='Bins: 12.5 percentage points, centred on zero. Gaussian kernel bandwidth: 5 points.\nThe curve describes smoothed observed scores; it does not impose normality.')
ggsave(file.path(out,'harshness_density_overlay_candidate.png'),p,width=8.3,height=4.7,dpi=220,bg='white')

# ECDF uses every observed value and needs neither bins nor bandwidth.
q <- quantile(x,c(.1,.25,.5,.75,.9));ec<-ecdf(x)
p <- ggplot(data.frame(x=x),aes(x))+stat_ecdf(geom='step',linewidth=.85,color=col)+
 geom_vline(xintercept=0,linetype=2,color='#777777',linewidth=.4)+
 geom_hline(yintercept=c(.1,.5,.9),linetype=3,color='#aaaaaa',linewidth=.4)+
 annotate('point',x=as.numeric(q[c(1,3,5)]),y=c(.1,.5,.9),size=2.3,color=accent)+
 scale_y_continuous(labels=function(y)paste0(round(100*y),'%'),breaks=seq(0,1,.25),limits=c(0,1))+
 scale_x_continuous(breaks=c(-75,-50,-25,0,25,50,75))+coord_cartesian(xlim=lims)+
 labs(title='Distribución acumulada: sin agrupación ni suavizado',
 subtitle='Altura de la curva: porcentaje de profesores con harshness igual o inferior al valor del eje horizontal',
 x='Harshness (puntos porcentuales)',y='Profesores acumulados',
 caption=sprintf('Percentiles 10 / 50 / 90: %.1f / %.1f / %.1f puntos. Todos los profesores, sin datos simulados.',q[1],q[3],q[5]))
ggsave(file.path(out,'harshness_ecdf_candidate.png'),p,width=10,height=5.3,dpi=180,bg='white')

# Bin-origin sensitivity: same width, shifted by half a bin.
origin <- bind_rows(lapply(c(0,w/2),function(shift){
 br<-seq(-125,125,by=w)+shift;h<-hist(x,breaks=br,plot=FALSE)
 data.frame(left=head(br,-1),right=tail(br,-1),share=100*h$counts/length(x),
 label=if(shift==0)'Borde en cero' else 'Centro en cero')
}))
p <- ggplot(origin,aes(xmin=left,xmax=right,ymin=0,ymax=share))+
 geom_rect(fill=col,color='white',linewidth=.4)+facet_wrap(~label,ncol=2)+coord_cartesian(xlim=lims)+
 labs(title='Comprobación del origen de las barras',subtitle='Mismo ancho de 12,5 puntos; desplazamiento de medio intervalo',
 x='Harshness (puntos porcentuales)',y='Profesores (%)')
ggsave(file.path(out,'harshness_bin_origin_check.png'),p,width=11,height=4.5,dpi=180,bg='white')

write.csv(histdata,file.path(out,'harshness_histogram_options.csv'),row.names=FALSE)
write.csv(dens,file.path(out,'harshness_density_options.csv'),row.names=FALSE)
write.csv(hh,file.path(out,'harshness_histogram_candidate.csv'),row.names=FALSE)
# Check whether broad asymmetry survives restricting the number of responses.
by_count <- bind_rows(lapply(list(All=seq_len(nrow(t)),At_least_eight=which(t$n_cards>=8),Nine=which(t$n_cards==9)),function(ix){
 xx<-x[ix];data.frame(n=length(xx),mean=mean(xx),median=median(xx),sd=sd(xx),
 skewness=mean((xx-mean(xx))^3)/mean((xx-mean(xx))^2)^1.5,
 q10=unname(quantile(xx,.1)),q90=unname(quantile(xx,.9)))
}),.id='sample')
write.csv(by_count,file.path(out,'harshness_figure_sample_check.csv'),row.names=FALSE)
print(by_count)
message('Figure alternatives saved; manuscript figure untouched.')
