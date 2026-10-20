# Presentation only. Reads aggregate results; never refits or changes estimates.
stopifnot(exists('read_aggregate'),exists('save_png'))
all_s<-c(main,'B_full8','C_full8','C_inclusive','C_example_screen')
gather_s<-function(name,ss=main){
 rows<-lapply(ss,function(s){z<-read_aggregate(name,s);z$scenario<-s;z})
 columns<-unique(unlist(lapply(rows,names)))
 do.call(rbind,lapply(rows,function(z){for(nm in setdiff(columns,names(z)))z[[nm]]<-NA;z[,columns,drop=FALSE]}))
}
long_metrics<-function(d,labels,scale=1){do.call(rbind,lapply(names(labels),function(v){z<-d;z$metric<-unname(labels[v]);z$value<-scale*d[[v]];z}))}
numlab<-function(x,digits=2)formatC(x,format='f',digits=digits,decimal.mark=',',big.mark='.')
dot_metrics<-function(d,name,height=5.4,ncol=2,labels=TRUE){
 d$scenario<-factor(d$scenario,levels=rev(unique(as.character(d$scenario))))
 p<-ggplot(d,aes(x=value,y=scenario,colour=scenario))+geom_point(size=2.5)+
  facet_wrap(~metric,scales='free_x',ncol=ncol)+
  scale_colour_manual(values=c(scenario_colors,B_full8='#418eaa',C_full8='#be8545',C_inclusive='#aa4670',C_example_screen='#526e47'),guide='none')+
  scale_x_continuous(expand=expansion(mult=c(.15,.25)))+labs(x=NULL,y=NULL)
 if(labels)p<-p+geom_text(aes(label=ifelse(grepl('Correlación',metric),numlab(value,3),ifelse(grepl('Docentes|Registros|Decisiones',metric)&!grepl('%',metric,fixed=TRUE),format_count(value),numlab(value)))),vjust=-.8,size=3)
 save_png(p,name,11,height)
}
forest<-function(d,name,labels,height=5.5,ncol=2,axis_label='Estimación e intervalo del 95 % (pp)'){
 d<-d[d$term%in%names(labels),];d$attribute<-factor(unname(labels[d$term]),levels=unname(labels))
 d$scenario_pos<-match(d$scenario,main)
 p<-ggplot(d,aes(colour=scenario))+geom_vline(xintercept=0,colour='#aab4bb',linewidth=.4)+
  geom_segment(aes(x=100*lo,xend=100*hi,y=scenario_pos,yend=scenario_pos),linewidth=.65)+
  geom_point(aes(x=100*estimate,y=scenario_pos),size=2.3)+
  facet_wrap(~attribute,ncol=ncol,scales='free_x')+
  scale_y_reverse(breaks=1:4,labels=main,limits=c(4.6,.35))+
  scale_colour_manual(values=scenario_colors,guide='none')+labs(x=axis_label,y=NULL)
 save_png(p,name,11,height)
}

# Native sample and support: different units receive separate panels.
labs<-c(teachers='Registros docentes',decisions='Decisiones',fe_teachers='Docentes en efectos fijos',
 fe_decisions='Decisiones en efectos fijos',common_teachers='Docentes con X3 completo',common_decisions='Decisiones con X3 completo',
 identifiers='Identificadores de tarjeta',coded_profiles='Perfiles codificados')
d<-long_metrics(samples,labs)
d$scenario<-factor(d$scenario,levels=main)
p<-ggplot(d,aes(x=scenario,y=value,fill=scenario))+geom_col(width=.65)+
 geom_text(aes(label=format_count(value)),vjust=-.4,size=3)+facet_wrap(~metric,scales='free_y',ncol=2)+
 scale_fill_manual(values=scenario_colors,guide='none')+scale_y_continuous(expand=expansion(mult=c(0,.2)))+labs(x=NULL,y='Recuento')
save_png(p,'sample_support',11,10)

# Audit values are not additive: separate denominators, explicit labels.
a<-read_aggregate('audit_counts')
labels<-c(raw_records='Registros brutos',unique_ResponseId='IDs de registro únicos',valid_sequences='Órdenes válidos',
 no_collisions_raw='Sin coincidencias',one_collision_raw='Una coincidencia',two_collisions_raw='Dos coincidencias',
 eight_stored_no_collisions_raw='Ocho guardados, sin coincidencias',A_with_valid_order='A con orden',A_without_valid_order='A sin orden',
 A_adverse_examples_outside_order='Ejemplos adversos fuera del orden (respuestas)',raw_shared_fields='Campos compartidos',raw_shared_records='Registros con campos compartidos',current_screen_exclusions='Excluidos por el cribado')
a$label<-unname(labels[a$metric]);a$population<-ifelse(a$metric%in%c('raw_records','unique_ResponseId','valid_sequences','current_screen_exclusions'),'Conjunto bruto · registros',ifelse(startsWith(a$metric,'A_'),'Muestra A · unidades indicadas','Bruto con orden · unidades indicadas'))
p<-ggplot(a,aes(x=value,y=reorder(label,value)))+geom_segment(aes(x=0,xend=value,yend=reorder(label,value)),colour='#bfd0d6')+
 geom_point(size=2.5,colour='#215e70')+geom_text(aes(label=format_count(value)),hjust=-.2,size=3)+
 facet_wrap(~population,scales='free',ncol=1)+scale_x_continuous(expand=expansion(mult=c(.02,.15)))+labs(x='Recuento · las categorías pueden solaparse',y=NULL)
save_png(p,'audit_counts_visual',11,9)

d<-do.call(rbind,lapply(c('B','C','D'),function(s){z<-read_aggregate('observation_changes_vs_A',s);aggregate(n~change,z,sum)|>transform(scenario=s)}))
d$value<-ifelse(d$change=='removed',-d$n,d$n);d$change<-factor(d$change,levels=c('removed','added'),labels=c('Retiradas','Incorporadas'))
p<-ggplot(d,aes(x=scenario,y=value,fill=change))+geom_hline(yintercept=0,colour='#adb5bd')+geom_col(width=.65)+
 geom_text(aes(label=format_count(abs(value))),vjust=ifelse(d$value<0,1.4,-.5),size=3.5)+
 scale_fill_manual(values=c('Retiradas'='#b25a22','Incorporadas'='#218c8d'))+scale_y_continuous(expand=expansion(mult=c(.1,.15)))+
 labs(x=NULL,y='Cambio de decisiones frente a A',fill=NULL)
save_png(p,'observation_changes',10.5,4.5)
forest(gather_s('academic_interactions'),'interaction_comparison',c('failed:competence'='Suspensos × competencias','failed:absent'='Suspensos × absentismo','failed:disruptive'='Suspensos × disciplina'),4.3,3)

academic_labels<-c('Few failures / adequate competence'='Pocos suspensos\nCompetencias adecuadas','Few failures / low competence'='Pocos suspensos\nCompetencias bajas','Many failures / adequate competence'='Muchos suspensos\nCompetencias adecuadas','Many failures / low competence'='Muchos suspensos\nCompetencias bajas')
a<-gather_s('academic_categories');a$academic<-factor(unname(academic_labels[a$academic]),levels=unname(academic_labels));a$scenario_pos<-match(a$scenario,main)
for(kind in c('rate','disagreement','count')){
 a$value<-if(kind=='rate')100*a$estimate else if(kind=='disagreement')100*a$disagreement else a$decisions
 p<-ggplot(a,aes(x=value,y=scenario_pos,colour=scenario))
 if(kind=='rate')p<-p+geom_segment(aes(x=100*lo,xend=100*hi,yend=scenario_pos),linewidth=.6)
 p<-p+geom_point(size=2.5)+geom_text(aes(label=if(kind=='count')format_count(value)else numlab(value)),vjust=-.8,size=3)+
  facet_wrap(~academic,ncol=2,scales=if(kind=='count')'free_x'else'fixed')+
  scale_y_reverse(breaks=1:4,labels=main,limits=c(4.5,.4))+scale_colour_manual(values=scenario_colors,guide='none')+
  labs(y=NULL,x=switch(kind,rate='Recomendaciones de repetición (%) · IC 95 %',disagreement='Desacuerdo entre dos docentes ante la misma tarjeta (%)',count='Decisiones por categoría académica'))
 save_png(p,paste0('academic_',kind),11,6)
}

sh<-read_aggregate('shared_teacher_summaries');sh$scenario<-paste(gsub(' versus ',' / ',sh$contrast),sh$source,sep=' · ')
d<-long_metrics(sh,c(teachers='Docentes comunes',decisions='Decisiones utilizadas',mean_repeat='Repetición (%)',mean_hb='Media de harshness (pp)',sd_hb='SD de harshness (pp)'))
d$value[d$metric%in%c('Repetición (%)','Media de harshness (pp)','SD de harshness (pp)')]<-100*d$value[d$metric%in%c('Repetición (%)','Media de harshness (pp)','SD de harshness (pp)')]
d$scenario<-factor(d$scenario,levels=rev(unique(d$scenario)))
p<-ggplot(d,aes(x=value,y=scenario))+geom_point(colour='#215e70',size=2.3)+geom_text(aes(label=ifelse(metric%in%c('Docentes comunes','Decisiones utilizadas'),format_count(value),numlab(value))),hjust=-.15,size=2.8)+
 facet_wrap(~metric,scales='free_x',ncol=2)+scale_x_continuous(expand=expansion(mult=c(.08,.28)))+labs(x=NULL,y='Par comparado · fuente de respuestas')
save_png(p,'shared_teacher_comparison',11,10)
ac<-sh[sh$contrast=='A versus C',]
d<-rbind(data.frame(population='Muestras nativas',scenario=c('A','C'),value=100*samples$sd_hb[match(c('A','C'),samples$scenario)]),data.frame(population='Mismos docentes',scenario=ac$source,value=100*ac$sd_hb))
p<-ggplot(d,aes(x=scenario,y=value,fill=scenario))+geom_col(width=.6)+geom_text(aes(label=numlab(value)),vjust=-.5)+facet_wrap(~population)+scale_fill_manual(values=scenario_colors,guide='none')+scale_y_continuous(expand=expansion(mult=c(0,.15)))+labs(x=NULL,y='SD de harshness (pp)')
save_png(p,'dispersion_bridge',10.5,4)
dot_metrics(long_metrics(samples,c(mean_repeat='Repetición por respuesta (%)',mean_teacher_repeat='Media de tasas docentes (%)',mean_hb='Media de Hb (pp)',sd_hb='SD de Hb (pp)'),100),'harshness_metrics')
d<-rbind(data.frame(scenario=samples$scenario,metric='P10 a P90 de Hb',lo=100*samples$q10,hi=100*samples$q90),data.frame(scenario=samples$scenario,metric='Cortes de los terciles',lo=100*samples$tercile_low,hi=100*samples$tercile_high))
d$pos<-match(d$scenario,main)
p<-ggplot(d,aes(colour=scenario))+geom_vline(xintercept=0,colour='#adb5bd')+geom_segment(aes(x=lo,xend=hi,y=pos,yend=pos),linewidth=1)+
 geom_point(aes(x=lo,y=pos),size=2.5)+geom_point(aes(x=hi,y=pos),size=2.5)+facet_wrap(~metric,scales='free_x')+
 scale_y_reverse(breaks=1:4,labels=main)+scale_colour_manual(values=scenario_colors,guide='none')+labs(x='Harshness (pp) · extremos son cuantiles, no IC',y=NULL)
save_png(p,'harshness_quantiles',11,4.3)
d<-read_aggregate('score_changes_vs_A');d<-d[d$scenario%in%c('B','C','D'),]
d$mean_absolute_delta<-100*d$mean_absolute_delta;d$changed_tercile<-100*d$changed_tercile
dot_metrics(long_metrics(d,c(shared_teachers='Docentes comunes con A',lost_teachers='Docentes perdidos frente a A',gained_teachers='Docentes recuperados frente a A',correlation='Correlación Hb con A',mean_absolute_delta='Cambio absoluto medio de Hb (pp)',changed_tercile='Docentes que cambian de tercil (%)')),'harshness_changes',7)
t<-gather_s('tercile_descriptives');sl<-gather_s('attributes_by_tercile');sl<-sl[sl$term=='failed',]
t$failed<-sl$estimate[match(paste(t$scenario,t$tercile),paste(sl$scenario,sl$model))]
t$tercile<-factor(t$tercile,levels=c('Lower','Middle','Upper'),labels=c('Bajo','Medio','Alto'))
d<-long_metrics(t,c(n='Docentes',mean_hb='Media de harshness (pp)',failed='Coeficiente de suspensos (pp)'));d$value[d$metric!='Docentes']<-100*d$value[d$metric!='Docentes']
p<-ggplot(d,aes(x=tercile,y=value,colour=scenario,group=scenario))+geom_line()+geom_point(size=2)+facet_wrap(~metric,scales='free_y',ncol=3)+scale_colour_manual(values=scenario_colors)+labs(x='Tercil reconstruido en cada escenario',y=NULL,colour=NULL)
save_png(p,'tercile_comparison',11,4.5)

cdata<-gather_s('characterization');forest(cdata[cdata$model=='X3_common',],'characterization_comparison',c(levelSecondary='Secundaria frente a primaria',edad='Edad · por año',experiencia='Experiencia · por año',resources_excess='Recursos excesivos · por punto'),5.8)
r<-unique(cdata[cdata$model%in%c('X1_common','X2_common','X3_common'),c('scenario','model','r2')]);r$model<-factor(r$model,levels=c('X1_common','X2_common','X3_common'),labels=c('X1','X2','X3'))
p<-ggplot(r,aes(x=model,y=100*r2,colour=scenario,group=scenario))+geom_line()+geom_point(size=2.5)+scale_colour_manual(values=scenario_colors)+scale_y_continuous(limits=c(0,NA))+labs(x='Modelo · misma muestra completa dentro de escenario',y='R² de harshness (%)',colour=NULL)
save_png(p,'characterization_r2',10.5,4.3)
dec<-gather_s('decomposition');dec$term<-dec$component;dec$estimate<-dec$share;dec$lo<-dec$lo_percentile;dec$hi<-dec$hi_percentile
forest(dec,'decomposition_ranges',comp_labels,8,axis_label='Cuota (%) · rango percentil bootstrap del 95 %')
st<-do.call(rbind,lapply(main,function(s){z<-jsonlite::read_json(file.path(out,s,'bootstrap_status.json'));data.frame(scenario=s,n=z$boundary_accepted)}))
p<-ggplot(st,aes(x=scenario,y=n,fill=scenario))+geom_col(width=.6)+geom_text(aes(label=paste0(n,' / 199')),vjust=-.5)+scale_fill_manual(values=scenario_colors,guide='none')+scale_y_continuous(limits=c(0,220),breaks=c(0,50,100,150,199))+labs(x=NULL,y='Réplicas con covarianza singular')
save_png(p,'bootstrap_singularity',10.5,4.1)
pair<-read_aggregate('paired_differences');pair<-pair[pair$contrast=='C - A'&grepl('^(attribute|interaction):',pair$metric),]
lookup<-c(attr_labels,'failed:competence'='Suspensos × competencias','failed:absent'='Suspensos × absentismo','failed:disruptive'='Suspensos × disciplina')
pair$term<-sub('^(attribute|interaction):','',pair$metric);pair$label<-factor(unname(lookup[pair$term]),levels=rev(unname(lookup)))
p<-ggplot(pair,aes(y=label))+geom_vline(xintercept=0,colour='#adb5bd')+geom_segment(aes(x=100*lo,xend=100*hi,yend=label),colour='#b25a22',linewidth=.8)+geom_point(aes(x=100*delta),colour='#b25a22',size=2.5)+labs(x='Cambio C − A (pp) · rango bootstrap del 95 %',y=NULL)
save_png(p,'paired_changes',11,6)

sens<-gather_s('summary',all_s);sens$mean_repeat<-100*sens$mean_repeat;sens$sd_hb<-100*sens$sd_hb;sens$r2_full<-100*sens$r2_full
dot_metrics(long_metrics(sens,c(teachers='Registros docentes',decisions='Decisiones',common_teachers='Docentes con X3 completo',mean_repeat='Repetición (%)',sd_hb='SD de Hb (pp)',r2_full='R² de X3 (%)')),'sensitivity_comparison',11)
screen<-read_aggregate('screening_rule_comparison');grid<-expand.grid(legacy=c(FALSE,TRUE),example_only=c(FALSE,TRUE));grid<-merge(grid,screen,all.x=TRUE);grid$n[is.na(grid$n)]<-0
grid$legacy<-factor(grid$legacy,levels=c(FALSE,TRUE),labels=c('Conserva','Excluye'));grid$example_only<-factor(grid$example_only,levels=c(FALSE,TRUE),labels=c('Conserva','Excluye'))
p<-ggplot(grid,aes(x=example_only,y=legacy,fill=n))+geom_tile(colour='white',linewidth=2)+geom_text(aes(label=format_count(n)),size=5)+scale_fill_gradient(low='#f2f6f7',high='#87b4bc',guide='none')+labs(x='Filtro del ejemplo favorable específico',y='Filtro vigente')
save_png(p,'screening_comparison',8,4.5)
ss<-c('C','B_full8','C_full8','C_inclusive','C_example_screen');z<-gather_s('decomposition_point',ss);z$component<-factor(z$component,levels=rev(names(comp_labels)));z$scenario<-factor(z$scenario,levels=rev(ss))
p<-ggplot(z,aes(x=scenario,y=100*share,fill=component))+geom_col(width=.65)+coord_flip()+scale_fill_manual(values=comp_colors,breaks=names(comp_labels),labels=comp_labels)+scale_y_continuous(expand=expansion(mult=0))+labs(x=NULL,y='Cuota puntual de varianza (%)',fill=NULL)+guides(fill=guide_legend(nrow=2,byrow=TRUE))
save_png(p,'sensitivity_decomposition',11,5)
split<-gather_s('split_half_summary');split$pos<-match(split$scenario,main)
p<-ggplot(split,aes(colour=scenario))+geom_segment(aes(x=p025,xend=p975,y=pos,yend=pos),linewidth=.9)+geom_point(aes(x=mean,y=pos),size=2.8)+scale_y_reverse(breaks=1:4,labels=main)+scale_colour_manual(values=scenario_colors,guide='none')+labs(x='Correlación media y rango central de las 1.000 particiones · no es IC',y=NULL)
save_png(p,'split_half_comparison',11,4.1)
seqs<-gather_s('sequence_summary');seqs$pos<-match(seqs$scenario,main)
p<-ggplot(seqs)+geom_segment(aes(x=100*permutation_lo,xend=100*permutation_hi,y=pos,yend=pos),linewidth=3,colour='#d4e0e5')+
 geom_point(aes(x=100*permutation_mean,y=pos,shape='Media permutada'),size=2.8,colour='#516f7c')+
 geom_point(aes(x=100*observed_switch,y=pos,shape='Observado'),size=3,colour='#b25a22')+
 geom_text(aes(x=100*permutation_hi,y=pos,label=paste0('  p=',numlab(p,3),' · n=',format_count(complete_eight))),hjust=0,size=3)+
 scale_y_reverse(breaks=1:4,labels=main)+scale_x_continuous(expand=expansion(mult=c(.05,.55)))+scale_shape_manual(values=c('Media permutada'=18,'Observado'=16))+labs(x='Transiciones que cambian la decisión (%)',y=NULL,shape=NULL)
save_png(p,'sequence_comparison',11,4.5)

