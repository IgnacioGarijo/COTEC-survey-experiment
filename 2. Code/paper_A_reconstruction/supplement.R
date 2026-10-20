# Appendix, provenance/screening sensitivities, and reconstructed coefficient uncertainty.
# Uses only isolated objects and writes only the reconstruction tree.
args0<-commandArgs(TRUE);reconstruction_library_mode<-TRUE;suppressWarnings(source('2. Code/paper_A_reconstruction/reconstruct.R',local=TRUE))
phase<-if(length(args0))args0[1]else'appendix'
prov<-readRDS(file.path(local,'provenance.rds'))

if(phase=='screen'){
 r<-as.data.frame(read_parquet('1. Data/raw data/df_named.parquet'));example<-as.character(r$Ejemplo)
 favorable<-ifelse(example%in%c('1','2'),as.character(r[['Grupo 1.1_1']]),ifelse(example%in%c('3','4'),as.character(r[['Grupo 2.1_1']]),NA))
 example_excluded<-!is.na(favorable)&favorable=='repite'
 csv(data.frame(legacy=prov$screen,example_only=example_excluded)%>%count(legacy,example_only),'screening_rule_comparison')
 z<-prov$ordered%>%filter(!is.na(response),!collision,!id%in%r$ResponseId[example_excluded])
 o<-rebuild(z,prov$cov);o$scenario<-'C_example_screen';saveRDS(o,object_path('C_example_screen'))
 message('C_example_screen: ',nrow(o$t),' records; ',nrow(o$long),' decisions; ',nrow(o$common),' complete')
 quit(save='no')
}

if(phase=='appendix'){
 appendix_algorithm<-'exact-v1' # Preserve the existing sample/rbinom call order and RNG stream.
 ss<-c(scenarios,'C_example_screen');ss<-ss[file.exists(vapply(ss,object_path,character(1)))]
 ord<-prov$ordered;td<-prov$teacher_design
 rawids<-prov$raw_id
 # Reconstruct engaged denominator from questionnaire stages, reusing only the published field map.
 r<-as.data.frame(read_parquet('1. Data/raw data/df_named.parquet'))
 fieldmap<-read.csv('3. Output/paper_A/reports/survey_exit_field_map.csv')
 seen<-sapply(fieldmap$fields,function(f){cols<-strsplit(f,' | ',fixed=TRUE)[[1]];rowSums(!is.na(r[,cols,drop=FALSE]))>0})
 last<-apply(seen*rep(seq_len(ncol(seen)),each=nrow(seen)),1,max);engaged<-last>=4
 stopifnot(sum(engaged)==4037,sum(!engaged)==905)
 flow<-list();scorechanges<-list();transitions<-list();refpop<-read.csv('3. Output/population/comparison.csv')%>%select(variable,category,reference_count,reference_n,reference_pct)%>%distinct()
 for(s in ss){
  o<-geto(s);t<-o$t;l<-o$long
  base_eligible<-if(s=='C_inclusive')rep(TRUE,length(rawids))else if(s=='C_example_screen'){ex<-as.character(r$Ejemplo);f<-ifelse(ex%in%c('1','2'),as.character(r[['Grupo 1.1_1']]),ifelse(ex%in%c('3','4'),as.character(r[['Grupo 2.1_1']]),NA));is.na(f)|f!='repite'}else!prov$screen
  if(s=='A') eligible<-rawids[base_eligible] else {ids<-td$id;if(s=='D')ids<-td$id[td$collisions==0];if(s%in%c('B_full8','C_full8'))ids<-td$id[td$stored==8];eligible<-intersect(rawids[base_eligible],ids)}
  sample<-rawids%in%t$id;common<-rawids%in%o$common$id;finished<-prov$finished;finished[is.na(finished)]<-FALSE
  flow[[s]]<-data.frame(scenario=s,stage=c('Registros brutos','Participación desde experiencia','Excluidos por filtro','Elegibles por orden/filtro/escenario','Con respuesta analizable','Con todas las creencias','Con covariables completas','Con respuesta y marca de cierre','Con respuesta y B2_complet','Con respuesta y encuesta terminada','Con respuesta parcial sin marca de cierre'),n=c(length(rawids),sum(engaged),sum(!base_eligible),length(eligible),nrow(t),sum(complete.cases(t[,setdiff(x3,x1)])),nrow(o$common),sum(sample&prov$group_end),sum(sample&!is.na(r$B2_complet)),sum(sample&finished),sum(sample&!prov$group_end)))
  csv(as.data.frame(table(stored=td$stored,analyzed=td$id%in%t$id)),'assigned_stored_inclusion',s)
  # Population margins: retain the canonical reported-school code cleaning for screened respondents.
  pop<-t;pop$sex<-ifelse(is.na(pop$female),NA,ifelse(pop$female==1,'Women','Men'));pop$ownership<-ifelse(is.na(pop$public),NA,ifelse(pop$public==1,'Public','Private'))
  pop$age_band<-cut(pop$edad,breaks=c(20,25,30,35,40,45,50,55,60,65,Inf),right=FALSE,labels=c('20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65+'))
  cp<-trimws(as.character(pop$cp));codes<-c('02'='Albacete','13'='Ciudad Real','16'='Cuenca','19'='Guadalajara','45'='Toledo');pop$province<-unname(codes[ifelse(!is.na(cp)&grepl('^[0-9]{5}$',cp),substr(cp,1,2),NA)])
  margins<-bind_rows(lapply(c('main','complete'),function(co){d<-if(co=='main')pop else pop[pop$id%in%o$common$id,];bind_rows(lapply(c('sex','ownership','age_band','province','level'),function(v){tab<-as.data.frame(table(d[[v]],useNA='no'));names(tab)<-c('category','sample_count');tab$variable<-v;tab$sample_n<-sum(!is.na(d[[v]]));tab$cohort<-co;tab}))}))
  margins<-left_join(margins,refpop,by=c('variable','category'));margins$sample_pct<-100*margins$sample_count/margins$sample_n;margins$difference_pp<-margins$sample_pct-margins$reference_pct;csv(margins,'population_comparison',s)
  # Eligible assigned positions remain the denominator even when no response was saved.
  po<-ord%>%filter(id%in%eligible)
  if(s%in%c('C','C_full8','C_inclusive','C_example_screen'))po<-filter(po,!collision)
  if(s=='A')po<-filter(po,!card%in%c('alumno_1_1','alumno_2_1','alumno_2_8','alumno_3_8','alumno_5_1','alumno_8_1'))
  pr<-po%>%group_by(position)%>%summarise(eligible=n(),observed=sum(!is.na(response)),response_rate=mean(!is.na(response)),n_timed=sum(is.finite(seconds)&seconds>0),median_seconds=median(seconds[is.finite(seconds)&seconds>0],na.rm=TRUE),p25=quantile(seconds[is.finite(seconds)&seconds>0],.25,na.rm=TRUE),p75=quantile(seconds[is.finite(seconds)&seconds>0],.75,na.rm=TRUE),.groups='drop');csv(pr,'task_progression',s)
  tim<-po%>%filter(is.finite(seconds),seconds>0,!is.na(response));tim$logseconds<-log(tim$seconds);tm<-feols(logseconds~position|id+card,tim,cluster=~id,notes=FALSE);csv(fe_tidy(tm,'Scenario eligible ordered positions'),'timing_slope',s)
  if(!file.exists(file.path(out,s,'sequence_summary.csv'))){
  # Actual gaps in position are respected for adjacent transitions.
  sq<-inner_join(l%>%select(id,card,y),ord%>%select(id,card,position),by=c('id','card'))%>%arrange(id,position)
  adjacent<-sq%>%group_by(id)%>%mutate(previous=lag(y),prevpos=lag(position))%>%ungroup()%>%filter(position-prevpos==1)
  full<-sq%>%group_by(id)%>%filter(n()==8,n_distinct(position)==8)%>%ungroup()%>%select(id,position,y)%>%pivot_wider(names_from=position,values_from=y)
  M<-as.matrix(full[,as.character(1:8)]);obs<-mean(M[,-1]!=M[,-8]);set.seed(921);perm<-replicate(999,{P<-t(apply(M,1,sample));mean(P[,-1]!=P[,-8])})
  csv(data.frame(complete_eight=nrow(M),adjacent_pairs=nrow(adjacent),teachers_with_pairs=n_distinct(adjacent$id),observed_switch=obs,permutation_mean=mean(perm),permutation_lo=quantile(perm,.025),permutation_hi=quantile(perm,.975),p=(1+sum(abs(perm-mean(perm))>=abs(obs-mean(perm))))/1000,replicates=999,seed=921,algorithm=appendix_algorithm),'sequence_summary',s)
  }else message('Sequence summary already exists; skipped: ',s)
  if(!file.exists(file.path(out,s,'split_half_summary.csv'))){
  # Independent-reference response halves, exact existing algorithm/seeds.
  idx<-match(l$id,t$id);set.seed(92702);fold<-sample(rep(1:5,length.out=nrow(t)));pool<-sample(rep(1:2,length.out=nrow(t)));ra<-rb<-numeric(nrow(l));support<-list()
  for(k in 1:5)for(g in 1:2){ev<-which(fold[idx]==k);z<-l[fold[idx]!=k&pool[idx]==g,];rr<-tapply(z$y,z$card,mean);nn<-table(z$card);stopifnot(all(l$card[ev]%in%names(rr)),min(nn)>1);if(g==1)ra[ev]<-rr[l$card[ev]]else rb[ev]<-rr[l$card[ev]];support[[length(support)+1]]<-data.frame(fold=k,pool=g,min_per_card=min(nn),teachers=n_distinct(z$id))}
  csv(bind_rows(support),'split_reference_support',s);ii<-split(seq_len(nrow(l)),idx);el<-which(t$n_cards>=8)
  # Cache numeric deviations and eligible row lists; no RNG calls or reduction changes.
  half_a_deviation<-as.numeric(l$y)-ra;half_b_deviation<-as.numeric(l$y)-rb
  eligible_rows<-unname(ii[as.character(el)]);eligible_positions<-seq_along(el);eligible_n<-length(el)
  cors<-numeric(1000);set.seed(92703)
  for(b in 1:1000){h1<-h2<-numeric(eligible_n);for(a in eligible_positions){rows<-sample(eligible_rows[[a]]);n1<-floor(length(rows)/2);if(length(rows)%%2==1)n1<-n1+rbinom(1,1,.5);ia<-rows[seq_len(n1)];ib<-rows[-seq_len(n1)];h1[a]<-mean(half_a_deviation[ia]);h2[a]<-mean(half_b_deviation[ib])};cors[b]<-cor(h1,h2)}
  csv(data.frame(partition=1:1000,correlation=cors),'split_half_partitions',s);csv(data.frame(teachers=length(el),min_responses=8,partitions=1000,mean=mean(cors),p025=quantile(cors,.025),p975=quantile(cors,.975),reference_seed=92702,partition_seed=92703,algorithm=appendix_algorithm),'split_half_summary',s)
  }else message('Split-half summary already exists; skipped: ',s)
  message('Appendix saved: ',s)
 }
 csv(bind_rows(flow),'participation_comparison')
 # Reproduce the manuscript's progression population independently: raw valid sequences, six global identifiers omitted.
 oldpo<-ord%>%filter(!card%in%c('alumno_1_1','alumno_2_1','alumno_2_8','alumno_3_8','alumno_5_1','alumno_8_1'))
 csv(oldpo%>%group_by(position)%>%summarise(eligible=n(),observed=sum(!is.na(response)),response_rate=mean(!is.na(response)),median_seconds=median(seconds[is.finite(seconds)&seconds>0],na.rm=TRUE),p25=quantile(seconds[is.finite(seconds)&seconds>0],.25,na.rm=TRUE),p75=quantile(seconds[is.finite(seconds)&seconds>0],.75,na.rm=TRUE),.groups='drop'),'legacy_raw_progression_reproduced')
 a<-geto('A');for(s in setdiff(ss,'A')){b<-geto(s);ab<-inner_join(a$t%>%select(id,hb_A=hb,tercile_A=tercile),b$t%>%select(id,hb_B=hb,tercile_B=tercile),by='id');scorechanges[[s]]<-data.frame(scenario=s,shared_teachers=nrow(ab),lost_teachers=sum(!a$t$id%in%b$t$id),gained_teachers=sum(!b$t$id%in%a$t$id),correlation=cor(ab$hb_A,ab$hb_B),mean_delta=mean(ab$hb_B-ab$hb_A),mean_absolute_delta=mean(abs(ab$hb_B-ab$hb_A)),rmse=sqrt(mean((ab$hb_B-ab$hb_A)^2)),changed_tercile=mean(ab$tercile_A!=ab$tercile_B));transitions[[s]]<-ab%>%count(tercile_A,tercile_B)%>%mutate(scenario=s)
  ao<-a$long%>%select(id,card);bo<-b$long%>%select(id,card);lost<-anti_join(ao,bo,by=c('id','card'));gained<-anti_join(bo,ao,by=c('id','card'));csv(bind_rows(lost%>%count(card)%>%mutate(change='removed'),gained%>%count(card)%>%mutate(change='added')),'observation_changes_vs_A',s)
 }
 csv(bind_rows(scorechanges),'score_changes_vs_A');csv(bind_rows(transitions),'tercile_transitions')
 quit(save='no')
}

if(phase=='coefficient_bootstrap'){
 ss<-if(length(args0)>1)strsplit(args0[2],',',fixed=TRUE)[[1]]else c(scenarios,'C_example_screen');B<-999L
 for(s in ss){o<-geto(s);t<-o$t;l<-o$long;idx<-match(l$id,t$id);cidx<-as.integer(factor(l$card));common<-complete.cases(t[,x3]);X<-model.matrix(reformulate(x3),t[common,]);bt<-matrix(NA_real_,B,ncol(X));colnames(bt)<-colnames(X);failure<-character(B)
  set.seed(92701)
  for(b in seq_len(B)){draw<-sample.int(nrow(t),replace=TRUE);w<-tabulate(draw,nbins=nrow(t));lw<-w[idx];sm<-rowsum(l$y*lw,cidx);ct<-rowsum(lw,cidx);sm<-setNames(as.numeric(sm),rownames(sm));ct<-setNames(as.numeric(ct),rownames(ct));if(any(ct[as.character(cidx[lw>0])]<=1)){failure[b]<-'insufficient_card_reference';next};ref<-(sm[as.character(cidx)]-l$y)/(ct[as.character(cidx)]-1);h<-as.numeric(tapply(l$y-ref,idx,mean));fit<-lm.wfit(X,h[common],w[common]);if(fit$rank!=ncol(X)||any(!is.finite(fit$coefficients))){failure[b]<-'rank_or_finite';next};bt[b,]<-fit$coefficients}
  ok<-complete.cases(bt);point<-read.csv(file.path(out,s,'characterization.csv'));point<-point[point$model=='X3_common',];point<-point[match(colnames(bt),point$term),];csv(data.frame(term=colnames(bt),estimate=point$estimate,se=apply(bt[ok,,drop=FALSE],2,sd),lo=apply(bt[ok,,drop=FALSE],2,quantile,.025),hi=apply(bt[ok,,drop=FALSE],2,quantile,.975),successful=sum(ok),requested=B,seed=92701),'characterization_bootstrap',s);csv(data.frame(draw=1:B,accepted=ok,error=failure),'coefficient_bootstrap_status',s);message('Coefficient bootstrap saved: ',s)
 }
 quit(save='no')
}
stop('Unknown supplement phase')
