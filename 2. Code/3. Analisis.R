#=========================#
#### 0. LOAD LIBRARIES ####
#=========================#

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("0. main.R")

df<-read_parquet("cleandata.parquet")

#=============================#
#### 1. HARSHNESS MEASURES ####
#=============================#

#----------------------------------#
##### A. NUMBER OF REPETITIONS #####
#----------------------------------#

namelist<- setdiff(namelist, c("alumno_1_1", "alumno_2_1")) # Eliminar los alumnos de los ejemplos

df1<-df[c("id",namelist)]

dfnrep<-df1 %>% 
  pivot_longer(all_of(namelist)) %>%
  drop_na(value) %>% 
  group_by(id) %>%
  mutate(value=ifelse(value=="repite", 1, 0)) %>% 
  summarise(ha=mean(value, na.rm=T)) %>% 
  ungroup()

#------------------------------#
##### B. AVERAGE DEVIATION #####
#------------------------------#

df2<-df1 %>% 
  pivot_longer(all_of(namelist)) %>% 
  mutate(value=ifelse(value=="repite", 1, 0)) %>% 
  group_by(name) %>% 
  mutate(tasa_rep=(sum(value, na.rm=T)-value)/(n()-1)  
            ) %>% 
  ungroup()

dfad <-df2 %>% 
  drop_na(value) %>% 
  mutate(diferencia= value-tasa_rep) %>% 
  group_by(id) %>% 
  summarise(hb=mean(diferencia, na.rm=T))

#------------------------------------------------------------#
##### C. DEVIATION WITH RESPECT TO PREDICTED PROBABILITY #####
#------------------------------------------------------------#

df2<-df1 %>% 
  pivot_longer(all_of(namelist), names_to="alumno", values_to="repite") %>% 
  mutate(repite=ifelse(repite=="repite", 1, 0)) %>% 
  drop_na(repite)

df2<-df2 %>% 
  mutate(niño=ifelse(alumno %in% vector_niño,1,0), 
         extranjero=ifelse(alumno %in% vector_extranjero, 1, 0), 
         suspensos=ifelse(alumno%in% vector_suspensos, 1, 0), 
         competencias=ifelse(alumno %in% vector_carencias, 1, 0), 
         absentista=ifelse(alumno %in% vector_absentista, 1, 0), 
         disruptivo=ifelse(alumno %in% vector_expulsion, 1, 0), 
         suma=niño+extranjero+suspensos+competencias+absentista+disruptivo)


modelo<-glm(data=df2, formula= repite ~ niño+ extranjero+ suspensos+ competencias+ absentista+ disruptivo, family=binomial)

df2$preds<-predict(modelo, type="response")

plot.roc(roc(response=df2$repite, df2$preds), print.auc = T)

dfdm<-df2 %>% 
  mutate(diferencia=repite-preds) %>% 
  group_by(id) %>% 
  summarise(hc=mean(diferencia, na.rm=T))

#------------------------------------------------------------------------------#
##### D. DEVIATION WITH RESPECT TO PREDICTED PROBABILITY IN COMPLETE MODEL #####
#------------------------------------------------------------------------------#

df2<-df1 %>% 
  pivot_longer(all_of(namelist), names_to="alumno", values_to="repite") %>% 
  mutate(repite=ifelse(repite=="repite", 1, 0)) %>% 
  drop_na(repite)

df2<-df2 %>% 
  mutate(niño=ifelse(alumno %in% vector_niño,1,0), 
         extranjero=ifelse(alumno %in% vector_extranjero, 1, 0), 
         suspensos=ifelse(alumno%in% vector_suspensos, 1, 0), 
         competencias=ifelse(alumno %in% vector_carencias, 1, 0), 
         absentista=ifelse(alumno %in% vector_absentista, 1, 0), 
         disruptivo=ifelse(alumno %in% vector_expulsion, 1, 0), 
         suma=niño+extranjero+suspensos+competencias+absentista+disruptivo)

df4<- df %>% select(id, primaria, titularidad)

df2<-left_join(df2, df4, by="id")

modelo<-glm(data=df2, formula= repite ~ niño+ extranjero+ suspensos+ competencias+ absentista+ disruptivo + primaria + titularidad, family=binomial)

df2$preds_compl<-predict(modelo, type="response")

plot.roc(roc(response=df2$repite, df2$preds_compl), print.auc = T)

dfdm_comp<-df2 %>% 
  mutate(diferencia=repite-preds_compl) %>% 
  group_by(id) %>% 
  summarise(hd=mean(diferencia, na.rm=T))

#===========================#
#### 2. MERGING MEASURES #### 
#===========================#

df3<-inner_join(dfad, dfdm)
df3<-inner_join(df3, dfnrep)
df3<-inner_join(df3, dfdm_comp)

GGally::ggpairs(df3[, c("ha","hb","hc", "hd")])

dfanalisis<- left_join(df3, df)

dfanalisis %>% 
  pivot_longer(cols=c("ha", "hb", "hc", "hd")) %>% 
  group_by(edad, name) %>% 
  summarise(value=mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(aes(edad, value, color=name))+
  geom_point()+
  geom_line()

#========================================#
#### 3. CHARACTERIZATION THE TEACHERS ####
#========================================#

# nivel, experiencia, antiguedad3, female, edad, titularidad, sitlabpub, grupos_docencia, impacto_centro_estudiantes/impacto_region_estudiantes, empatia, meritocracia

dfanalisis <-dfanalisis %>% 
  mutate(indefinido= ifelse(sitlabpub %in% c("Contratada/o laboral indefinido", "Funcionaria/o con destino definitivo"), "indefinido", "temporal"), 
         impacto_estudiantes= ifelse(is.na(impacto_centro_estudiantes), impacto_region_estudiantes, impacto_centro_estudiantes), 
         empatia= case_when(!is.na(empatia_escala_1a5)~ empatia_escala_1a5, 
                            !is.na(empatia_escala_0a100_t1) ~ ceiling(empatia_escala_0a100_t1/20), 
                            !is.na(empatia_escala_0a100_t2) ~ ceiling(empatia_escala_0a100_t2/20), 
                            !is.na(empatia_escala_0a100_t3) ~ ceiling(empatia_escala_0a100_t3/20), 
                            TRUE ~ NA
                            ), 
         empatia= ifelse(empatia==0, 1, empatia)
         )


dfrf<- dfanalisis %>% 
  transmute(primary= ifelse(nivel=="E. Primaria", 1, 0), 
         experience= ntile3_label(experiencia),
         tenure=ntile3_label(antiguedad3), 
         age= ntile3_label(edad),
         groups= ntile3_label(grupos_docencia), 
         student_impact= ntile3_label(impacto_estudiantes),
         meritocracy= ntile3_label(meritocracia), 
         high_empathy=factor(ifelse(empatia>=5, 1, 0)),
         public= factor(ifelse(titularidad== "Pública", 1,0)),
         permanent= factor(ifelse(indefinido=="indefinido", 1, 0)),
         female,
         hb
         )

#--------------------------#
##### A. RANDOM FOREST #####
#--------------------------#

vars<- setdiff(colnames(dfrf), "hb")
fmla<- paste("hb ~", paste(vars, collapse = "+"))
  
set.seed(1)

modelo_rf <- ranger(
  formula = fmla,
  data = dfrf,
  importance = "permutation",
  num.trees = 1000,
  mtry = 3,
  min.node.size = 5, 
  respect.unordered.factors = TRUE
)

###### Variable importance ######

dfimportance<- data.frame(vars=names(modelo_rf$variable.importance),
                          importance=modelo_rf$variable.importance)

dfimportance %>% 
  ggplot(aes(fct_reorder(vars, importance), importance))+
  geom_col()

dfrf$pred_rf <- predict(modelo_rf, data = dfrf)$predictions

dfrf %>% 
  ggplot(aes(x = hb, y = pred_rf)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se=F, method="lm")

###### Shap values ######

variables_rf<-names(modelo_rf$variable.importance)

dfshap<- dfrf[c(variables_rf)]

# shap_values<-Predictor$new(modelo_rf, data=dfshap) # No necesario por ahora, del paquete iml
# shapley<-Shapley$new(shap_values, x.interest = dfshap[1,])

set.seed(1)

shap_values<-fastshap::explain(
  object= modelo_rf, 
  X= dfshap, 
  pred_wrapper=f, 
  nsim=30, 
  adjust=T
)

sv<-shapviz(shap_values, X=dfshap)

sdf <- as.data.frame(sv$S) %>% 
  mutate(row = row_number()) %>% 
  pivot_longer(
    cols = -row,
    names_to = "variable",       # 👈 mismo nombre que en el segundo pivot
    values_to = "shap_value"
  ) %>% 
  left_join(
    as.data.frame(sv$X) %>% 
      mutate(row = row_number()) %>% 
      pivot_longer(
        cols = -row,
        names_to = "variable",
        values_to = "feature_value",
        values_transform = list(feature_value = as.character)  # 👈 evita conflicto de tipos
      ),
    by = c("row", "variable")
  )

sdf<-sdf %>% 
  drop_na(feature_value) %>% 
  mutate(feature_value= factor(ifelse(feature_value %in% c("high", "1"), "High/Yes", 
                               ifelse(feature_value %in% c("low", "0"), "Low/No", 
                                      "Medium")), levels= c("Low/No", "Medium", "High/Yes"))) %>% 
  group_by(feature_value, variable) %>% 
  mutate(mean_feature=mean(abs(shap_value), na.rm=T))

sdf<-sdf %>% 
  left_join(dfimportance, by=c("variable"="vars"))
  
sdf %>% 
  ggplot(aes(x = shap_value, y = fct_reorder(variable, -importance), color=feature_value)) +
  geom_quasirandom(alpha = 0.1, width=.3)+
  geom_point(aes(x=mean_feature, fill=feature_value),shape=23, stroke=1, color="black", size=3)+
  scale_color_manual(values = paleta3)+
  scale_fill_manual(values=paleta3)

###### Distribution of continuous variables ######

variablescontinuas<- c("experiencia", "grupos_docencia", "meritocracia", "impacto_estudiantes", "antiguedad3", "edad")
plots<-make_quartile_hists(dfanalisis, variablescontinuas)

#------------------#
##### B. LOGIT #####
#------------------#

dfanalisis$edad_quad<- dfanalisis$edad^2

modeloa<-lm(data=dfanalisis, formula = ha ~ nivel + experiencia + antiguedad3 + female + edad +
          titularidad + indefinido + grupos_docencia + impacto_estudiantes +
          empatia )
modelob<-lm(data=dfanalisis, formula = hb ~ nivel + experiencia + antiguedad3 + female + edad +
              titularidad + indefinido + grupos_docencia + impacto_estudiantes +
              empatia )
modeloc<-lm(data=dfanalisis, formula = hc ~ nivel + experiencia + antiguedad3 + female + edad +
              titularidad + indefinido + grupos_docencia + impacto_estudiantes +
              empatia )
modelod<-lm(data=dfanalisis, formula = hd ~ nivel + experiencia + antiguedad3 + female + edad +
              titularidad + indefinido + grupos_docencia + impacto_estudiantes +
              empatia )


modelsummary::modelsummary(models = list(modeloa, modelob, modeloc, modelod),
                           stars=c("*"=.1, "**"=.05, "***"=.01),
                           include.rsquared = FALSE,
                           include.adjrs = FALSE,
                           include.nobs = FALSE,
                           include.rmse = FALSE)



modeloa2<-lm(data=dfanalisis, formula = ha ~ nivel + experiencia + antiguedad3 + female + edad + 
              titularidad + indefinido + grupos_docencia + impacto_estudiantes +
              empatia + meritocracia)
modelob2<-lm(data=dfanalisis, formula = hb ~ nivel + experiencia + antiguedad3 + female + edad + 
              titularidad + indefinido + grupos_docencia + impacto_estudiantes +
              empatia + meritocracia)
modeloc2<-lm(data=dfanalisis, formula = hc ~ nivel + experiencia + antiguedad3 + female + edad +
              titularidad + indefinido + grupos_docencia + impacto_estudiantes +
              empatia + meritocracia)
modelod2<-lm(data=dfanalisis, formula = hd ~ nivel + experiencia + antiguedad3 + female + edad + 
              titularidad + indefinido + grupos_docencia + impacto_estudiantes +
              empatia + meritocracia)


modelsummary(models = list(modeloa2, modelob2, modeloc2, modelod2),
                           stars=c("*"=.1, "**"=.05, "***"=.01),
                           include.rsquared = FALSE,
                           include.adjrs = FALSE,
                           include.nobs = FALSE,
                           include.rmse = FALSE) # ESTE IMPORTANTE PRESENTAR Y EDAD CUADRÁTICA
dfanalisis %>% 
  group_by(edad) %>% 
  summarise(value=mean(hb, na.rm=T)) %>% 
  ggplot(aes(edad, value))+
  geom_point()+
  geom_line()

gg<-modelob2 %>%
  tidy(conf.int = TRUE) %>%       # estimates + IC 95%
  filter(term != "(Intercept)") %>%  
  mutate(sign=ifelse(conf.low<=0 & conf.high>=0, "No", "Si"), 
         term= case_when(term == "titularidadPrivada" ~ "Privada", 
                         term == "nivelE. Secundaria" ~ "Secundaria", 
                         term == "indefinidotemporal" ~ "Temporal", 
                         term== "titularidadPública" ~ "Pública",
                         term== "female1" ~ "Mujer", 
                         TRUE ~ term), 
         discr= ifelse(term %in% c("meritocracia", "grupos_docencia", "experiencia", "antiguedad3", "impacto_estudiantes", "edad", "empatia"), "cont", "discr")) %>% 
  ggplot(aes(x = reorder(term, estimate),
             y = estimate,
             ymin = conf.low,
             ymax = conf.high, 
             color=sign)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, color="grey40", linetype="longdash")+
  coord_flip()+
  guides(color="none")+
  scale_color_manual(values= c("#537d90", "#00b89f"))+
  facet_wrap(~discr, scales="free", labeller = labeller(.default = ~""))


ggsave(gg, file=paste0(salidas, "coefs.jpeg"), width=7, height=5)

modelob2<-lm(data=dfrf, formula=fmla)

gg<-modelob2 %>%
  tidy(conf.int = TRUE) %>%       
  filter(term != "(Intercept)") %>%  
  mutate(sign=ifelse(conf.low<=0 & conf.high>=0, "No", "Si")) %>% 
  ggplot(aes(x = reorder(term, estimate),
             y = estimate,
             ymin = conf.low,
             ymax = conf.high, 
             color=sign)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, color="grey40", linetype="longdash")+
  coord_flip()+
  guides(color="none")+
  scale_color_manual(values= c("#537d90", "#00b89f"))

gg

#=====================#
#### 3. HYPOTHESES #### 
#=====================#

#--------------------#
##### A. STUDY 1 #####
#--------------------#

###### H11 ######

dfanalisis<-dfanalisis %>% 
  mutate(control=ifelse(treatment==1, 1, 0),
         D= case_when(treatment==1~ "Control", 
                      treatment %in% c(2:4) ~ "Exogeneous", 
                      treatment %in% c(5:7) ~ "Endogenous", 
                      treatment %in% c(8:10) ~ "Awareness"))

modeloh11<- lm(data=dfanalisis, formula= hb ~ control)

modelsummary(models = modeloh11,
             stars=c("*"=.1, "**"=.05, "***"=.01),
             include.rsquared = FALSE,
             include.adjrs = FALSE,
             include.nobs = FALSE,
             include.rmse = FALSE)


###### H12 ######

dfanalisis<-dfanalisis %>% 
  mutate(lambda= factor(ifelse(control==1, "Control", paste0("Policy",politica))),
         lambda= relevel(lambda, ref="Control"),
         favorite = case_when(orden_pref_refuerzo == 1 ~ "reinforcement",
                              orden_pref_criterios_promo == 1 ~ "promotion_criteria",
                              orden_pref_formacion_prof == 1 ~ "training"),
         least_favorite = case_when(orden_pref_refuerzo == 3 ~ "reinforcement",
                                    orden_pref_criterios_promo == 3 ~ "promotion_criteria",
                                    orden_pref_formacion_prof == 3 ~ "training"
  )
)

modeloh12<- lm(data=dfanalisis, formula= hb ~ lambda+favorite)

modelsummary(models = modeloh12,
             stars=c("*"=.1, "**"=.05, "***"=.01),
             include.rsquared = FALSE,
             include.adjrs = FALSE,
             include.nobs = FALSE,
             include.rmse = FALSE)

###### H13 ######

dfanalisis_h13 <- dfanalisis %>%
  mutate(favorite_num= case_when(orden_pref_refuerzo==1~ 1, 
                                 orden_pref_criterios_promo==1 ~2, 
                                 orden_pref_formacion_prof==1 ~3), 
         least_favorite_num = case_when(orden_pref_refuerzo==3~ 1, 
                                        orden_pref_criterios_promo==3 ~2, 
                                        orden_pref_formacion_prof==3 ~3)) %>% 
  filter(favorite_num== politica | least_favorite_num==politica | control==1) %>% 
  mutate(assignation= case_when(favorite_num==politica ~ "favorite", 
                                least_favorite_num==politica ~ "least-favorite", 
                                control==1 ~ "Control"),
         assignation= relevel(factor(assignation), ref="Control"), 
         politica= factor(paste0("Policy", politica)))
  
modelo_h13<- lm(data=dfanalisis_h13, formula= hb ~ assignation+politica)

modelsummary(models = modelo_h13,
             stars=c("*"=.1, "**"=.05, "***"=.01),
             include.rsquared = FALSE,
             include.adjrs = FALSE,
             include.nobs = FALSE,
             include.rmse = FALSE)

#--------------------#
##### B. STUDY 2 #####
#--------------------#

