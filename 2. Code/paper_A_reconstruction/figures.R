# Generate the visual report's PNGs from aggregate CSV files only.
# Run after appendix outputs and histogram aggregates exist:
# Rscript --vanilla "2. Code/paper_A_reconstruction/figures.R"
options(stringsAsFactors=FALSE)
invisible(Sys.setlocale('LC_CTYPE','Spanish_Spain.utf8'))
script_arg<-grep('^--file=',commandArgs(FALSE),value=TRUE)
stopifnot(length(script_arg)==1)
script_path<-normalizePath(sub('^--file=','',script_arg),winslash='/',mustWork=TRUE)
root<-normalizePath(file.path(dirname(script_path),'..','..'),winslash='/',mustWork=TRUE)
project_library<-file.path(root,'renv/library/windows/R-4.4/x86_64-w64-mingw32')
if(dir.exists(project_library)).libPaths(c(project_library,.libPaths()))
suppressPackageStartupMessages(library(ggplot2))
out<-file.path(root,'3. Output/paper_A_reconstruction')
figdir<-file.path(out,'figures')
main<-c('A','B','C','D')
scenario_colors<-c(A='#687781',B='#218c8d',C='#b25a22',D='#7356a5')
attr_labels<-c(failed='Tres o más suspensos',competence='Competencias bajas',
 absent='Absentismo',disruptive='Disciplina',background='Contexto social complejo',male='Alumno varón')
comp_labels<-c(Student='Atributos e interacciones',Professional='Perfil docente',Beliefs='Creencias',
 Harshness='Harshness observado',Sensitivities='Sensibilidades docentes',Residual='Residuo')
comp_colors<-setNames(c('#215e70','#b1c9ca','#e5c58c','#c27540','#7d67a3','#d9dde0'),names(comp_labels))
csv_path<-function(name,s=NULL)file.path(out,if(is.null(s))'tables'else s,paste0(name,'.csv'))
required<-c(unlist(lapply(main,function(s)vapply(c('attributes','decomposition_point','summary','harshness_distribution'),csv_path,character(1),s=s))),
 vapply(c('B','C','D'),function(s)csv_path('task_progression',s),character(1)),csv_path('legacy_raw_progression_reproduced'))
missing<-required[!file.exists(required)]
if(length(missing))stop('Missing aggregate inputs; complete appendices/export_harshness.R first: ',paste(basename(missing),collapse=', '))
read_aggregate<-function(name,s=NULL)read.csv(csv_path(name,s),fileEncoding='UTF-8',check.names=FALSE)
combine<-function(name)do.call(rbind,lapply(main,function(s){z<-read_aggregate(name,s);z$scenario<-s;z}))
attr_data<-combine('attributes')
parts<-combine('decomposition_point')
samples<-combine('summary')
histograms<-combine('harshness_distribution')
legacy<-read_aggregate('legacy_raw_progression_reproduced');legacy$scenario<-'A'
progress<-rbind(legacy[,c('scenario','position','response_rate','median_seconds')],
 do.call(rbind,lapply(c('B','C','D'),function(s){z<-read_aggregate('task_progression',s);z$scenario<-s;z[,c('scenario','position','response_rate','median_seconds')]})))
stopifnot(all(c('term','estimate','lo','hi')%in%names(attr_data)),
 all(c('component','share')%in%names(parts)),all(c('teachers','decisions')%in%names(samples)),
 all(c('midpoint','share')%in%names(histograms)))
attr_data<-attr_data[attr_data$term%in%names(attr_labels),]
stopifnot(nrow(attr_data)==24,all(is.finite(attr_data$estimate)),all(attr_data$lo<=attr_data$estimate),all(attr_data$estimate<=attr_data$hi))
for(s in main){
 stopifnot(setequal(parts$component[parts$scenario==s],names(comp_labels)),
  abs(sum(parts$share[parts$scenario==s])-1)<1e-8,
  all(parts$share[parts$scenario==s]>=0),
  abs(sum(histograms$share[histograms$scenario==s])-1)<1e-8,
  isTRUE(all.equal(histograms$midpoint[histograms$scenario==s],seq(-1,1,by=.125),tolerance=1e-12)))
}
dir.create(figdir,recursive=TRUE,showWarnings=FALSE)
theme_set(theme_minimal(base_size=10)+theme(panel.grid.minor=element_blank(),
 plot.background=element_rect(fill='white',colour=NA),legend.position='bottom',
 strip.text=element_text(face='bold',hjust=0),plot.margin=margin(12,14,12,12)))
save_png<-function(p,name,width,height)ggsave(file.path(figdir,paste0(name,'.png')),p,
 width=width,height=height,units='in',dpi=175,bg='white')
format_count<-function(x)format(round(x),big.mark='.',decimal.mark=',',scientific=FALSE,trim=TRUE)

# Attribute effects and teacher-clustered 95% intervals, in percentage points.
attr_data$attribute<-factor(unname(attr_labels[attr_data$term]),levels=unname(attr_labels))
attr_data$scenario_pos<-match(attr_data$scenario,main)
p<-ggplot(attr_data,aes(colour=scenario))+
 geom_vline(xintercept=0,colour='#adb5bd',linewidth=.4)+
 geom_segment(aes(x=100*lo,xend=100*hi,y=scenario_pos,yend=scenario_pos),linewidth=.65)+
 geom_point(aes(x=100*estimate,y=scenario_pos),size=2.2)+
 facet_wrap(~attribute,ncol=2,scales='free_x')+
 scale_y_reverse(breaks=seq_along(main),labels=main,limits=c(4.5,.5))+
 scale_colour_manual(values=scenario_colors,guide='none')+
 labs(x='Coeficiente e intervalo del 95 % (pp)',y=NULL)
save_png(p,'attribute_comparison',11,8.4)

# Exact point shares from the existing decomposition CSVs; no new estimation.
rectangles<-do.call(rbind,lapply(main,function(s){
 z<-parts[parts$scenario==s,];z<-z[match(names(comp_labels),z$component),]
 z$right<-cumsum(100*z$share);z$left<-c(0,head(z$right,-1));z$scenario_pos<-match(s,main);z
}))
rectangles$component<-factor(rectangles$component,levels=names(comp_labels))
labels<-rectangles[100*rectangles$share>5,]
labels$label<-formatC(100*labels$share,format='f',digits=1,decimal.mark=',')
labels$text_colour<-ifelse(labels$component=='Residual','#333333','white')
p<-ggplot(rectangles)+
 geom_rect(aes(xmin=left,xmax=right,ymin=scenario_pos-.32,ymax=scenario_pos+.32,fill=component))+
 geom_text(data=labels,aes(x=(left+right)/2,y=scenario_pos,label=label,colour=text_colour),size=3.5)+
 scale_colour_identity()+scale_fill_manual(values=comp_colors,breaks=names(comp_labels),labels=unname(comp_labels),drop=FALSE)+
 scale_y_reverse(breaks=seq_along(main),labels=main,limits=c(4.6,.4))+
 scale_x_continuous(expand=expansion(mult=0))+coord_cartesian(xlim=c(0,100))+
 guides(fill=guide_legend(nrow=2,byrow=TRUE))+
 labs(x='Cuota de la varianza implícita en el modelo (%)',y=NULL,fill=NULL)
save_png(p,'decomposition_comparison',10.5,4.6)

# Native sample sizes; free count axes for teachers and responses.
sample_data<-rbind(data.frame(scenario=samples$scenario,metric='Registros docentes con respuestas',value=samples$teachers),
 data.frame(scenario=samples$scenario,metric='Decisiones analizadas',value=samples$decisions))
sample_data$scenario<-factor(sample_data$scenario,levels=main)
sample_data$metric<-factor(sample_data$metric,levels=c('Registros docentes con respuestas','Decisiones analizadas'))
p<-ggplot(sample_data,aes(x=scenario,y=value,fill=scenario))+
 geom_col(width=.7)+geom_text(aes(label=format_count(value)),vjust=-.5,size=3.5)+
 facet_wrap(~metric,nrow=1,scales='free_y')+scale_fill_manual(values=scenario_colors,guide='none')+
 scale_y_continuous(expand=expansion(mult=c(0,.16)))+labs(x=NULL,y='Recuento')
save_png(p,'sample_comparison',11,4.2)

# Equal-weight teacher distributions; common 12.5-pp bins centered on zero.
histograms$scenario<-factor(histograms$scenario,levels=main)
p<-ggplot(histograms,aes(x=100*midpoint,y=100*share,colour=scenario))+
 geom_step(direction='mid',linewidth=.8)+scale_colour_manual(values=scenario_colors)+
 scale_x_continuous(breaks=seq(-100,100,25))+
 guides(colour=guide_legend(nrow=1))+
 labs(x='Harshness respecto a pares de la misma tarjeta (pp)',y='Porcentaje de docentes',colour=NULL)
save_png(p,'harshness_comparison',10.5,4.5)

# Keep raw legacy and native scenario denominators separate, as in the report.
progress_data<-rbind(data.frame(scenario=progress$scenario,position=progress$position,
 metric='Valores guardados / posiciones elegibles (%)',value=100*progress$response_rate),
 data.frame(scenario=progress$scenario,position=progress$position,
 metric='Mediana del tiempo guardado (segundos)',value=progress$median_seconds))
progress_data$scenario<-factor(progress_data$scenario,levels=main)
progress_data$metric<-factor(progress_data$metric,levels=c('Valores guardados / posiciones elegibles (%)','Mediana del tiempo guardado (segundos)'))
scenario_labels<-c(A='A · Bruto; omite seis IDs',B='B',C='C',D='D')
p<-ggplot(progress_data,aes(x=position,y=value,colour=scenario,linetype=scenario))+
 geom_line(linewidth=.65)+geom_point(data=progress_data[progress_data$scenario!='A',],size=1.8)+
 facet_wrap(~metric,nrow=1,scales='free_y')+
 scale_colour_manual(values=scenario_colors,breaks=main,labels=scenario_labels)+
 scale_linetype_manual(values=c(A='dashed',B='solid',C='solid',D='solid'),breaks=main,labels=scenario_labels)+
 scale_x_continuous(breaks=1:8)+guides(colour=guide_legend(nrow=1),linetype=guide_legend(nrow=1))+
 labs(x='Posición asignada',y=NULL,colour=NULL,linetype=NULL)
save_png(p,'progression_comparison',11,4.3)
source(file.path(root,'2. Code/paper_A_reconstruction/visual_comparisons.R'))
message('Visual comparison figures saved under ',figdir)
