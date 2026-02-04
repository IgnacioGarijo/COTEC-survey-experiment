## REPETIR GRÁFICO COTEC MONOTONIA SIN LAS CARACTERÍSTICAS QUE NO GUSTAN NADA, PARA QUE LA MONOTONÍA NO SEA SIMPLEMENTE UN REFLEJO DE QUE HAY MÁS PROBABILIDADES DE QUE SALGA ALGO MALO

#==========================================#
#### 0. LOAD LIBRARIES, THEMES AND DATA ####
#==========================================#

source("2. Code/0. main.R")

df<-read_parquet(cleandata)

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
  filter(!is.na(value)) %>% 
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

ggpairs(df3[, c("ha","hb","hc", "hd")],
                diag = list(
                  continuous = wrap("barDiag", bins = 20)
                ))


dfanalisis<- left_join(df3, df)



gg<-dfanalisis %>% 
  select(ha, hb, hc,hd) %>% 
  pivot_longer(cols = c("ha", "hb", "hc", "hd")) %>% 
  ggplot(aes(value))+
  geom_histogram(bins=20, alpha=.8, fill= paleta_alt[[1]])+
  geom_vline(xintercept = 0, linetype="longdash", color=paleta[[4]], linewidth=.6)+
  facet_wrap(~name)+ # Sacar
  scale_y_continuous(expand = c(0,0))+
  theme(
    strip.text = element_text(
      size = 16,       # tamaño
      face = "bold",   # negrita
    )
  )

ggsave(gg, filename=file.path(graficos, "histograms.jpeg"), width = 12, height = 8)


dfanalisis %>% 
  pivot_longer(cols=c("ha", "hb", "hc", "hd")) %>% 
  group_by(edad, name) %>% 
  summarise(value=mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(aes(edad, value, color=name))+
  geom_point()+
  geom_line()

#======================================#
#### 3. CHARACTERIZING THE TEACHERS ####
#======================================#

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

###### Error ######

sd_hb <- sd(dfrf$hb)
rmse_oob <- sqrt(modelo_rf$prediction.error)

rmse_oob / sd_hb

rmse_mean <- sqrt(mean((dfrf$hb - mean(dfrf$hb))^2))
rmse_oob / rmse_mean

rmse_oob / mean(dfrf$hb)

yhat <- modelo_rf$predictions

resid <- dfrf$hb - yhat
summary(abs(resid))
quantile(abs(resid), c(.5, .75, .9))

###### Variable importance ######

dfimportance<- data.frame(vars=names(modelo_rf$variable.importance),
                          importance=modelo_rf$variable.importance)

gg<-dfimportance %>% 
  ggplot(aes(importance,fct_reorder(vars, importance)))+
  geom_col(fill=paleta_alt[[1]], alpha=.85)+ # Sacar
  scale_x_continuous(expand=c(0,0))+
  theme(text= element_text(size=18))

ggsave(gg, filename=file.path(graficos, "variable_importance.jpeg"), width = 12, height = 8)

dfrf$pred_rf <- predict(modelo_rf, data = dfrf)$predictions

gg<-dfrf %>% 
  ggplot(aes(x = hb, y = pred_rf)) +
  geom_point(alpha = 0.6, color=paleta_alt[[1]]) +
  geom_smooth(se=F, method="lm", color= "grey50") + # Sacar
  theme(axis.title = element_text())+
  ylab("Predicciones")+
  xlab("Hb")

ggsave(gg, filename=file.path(graficos, "correlation.jpeg"), width = 12, height = 8)


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
  mutate(mean_feature=mean(shap_value, na.rm=T))

sdf<-sdf %>% 
  left_join(dfimportance, by=c("variable"="vars"))
  
gg<-sdf %>% 
  ggplot(aes(x = shap_value, y = fct_reorder(variable, importance), color=feature_value)) +
  geom_vline(xintercept = 0, linetype="longdash")+
  geom_quasirandom(alpha = 0.3, width=.3)+
  geom_point(aes(x=mean_feature, fill=feature_value),shape=23, stroke=1, color="black", size=3)+
  scale_color_manual(values = paleta3)+
  scale_fill_manual(values=paleta3) # Sacar

ggsave(gg, filename=file.path(graficos, "SHAP.jpeg"), width = 12, height = 8)

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
  facet_wrap(~discr, scales="free", labeller = labeller(.default = ~"")) +# Sacar
  theme(text = element_text(size=15))

ggsave(gg, file=file.path(graficos, "coefs.jpeg"), width=7, height=5)

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

#-----------------------------#
##### A. PREVISUALIZATION #####
#-----------------------------#

dfanalisis<-dfanalisis %>% 
  mutate(control=ifelse(treatment==1, 1, 0),
         D= case_when(treatment==1~ "Control", 
                      treatment %in% c(2:4) ~ "Policy", 
                      treatment %in% c(5:7) ~ "Revelation", 
                      treatment %in% c(8:10) ~ "Awareness"),
         D= factor(D, levels=c("Control", "Policy", "Revelation", "Awareness")),
         lambda= factor(ifelse(control==1, "Control", paste0("Policy ",politica))),
         lambda= relevel(lambda, ref="Control"),
         favorite = case_when(orden_pref_refuerzo == 1 ~ "reinforcement",
                              orden_pref_criterios_promo == 1 ~ "promotion_criteria",
                              orden_pref_formacion_prof == 1 ~ "training"),
         least_favorite = case_when(orden_pref_refuerzo == 3 ~ "reinforcement",
                                    orden_pref_criterios_promo == 3 ~ "promotion_criteria",
                                    orden_pref_formacion_prof == 3 ~ "training"),
         favorite_num= case_when(orden_pref_refuerzo==1~ 1, 
                                 orden_pref_criterios_promo==1 ~2, 
                                 orden_pref_formacion_prof==1 ~3), 
         least_favorite_num = case_when(orden_pref_refuerzo==3~ 1, 
                                        orden_pref_criterios_promo==3 ~2, 
                                        orden_pref_formacion_prof==3 ~3),
         politica=ifelse(D!="Control", politica, NA)) 

gg<-dfanalisis %>% 
  drop_na(favorite, lambda) %>% 
  group_by(favorite, lambda= case_when(lambda=="Policy 1" ~ "reinforcement", 
                                       lambda=="Policy 2" ~ "promotion_criteria", 
                                       lambda=="Policy 3" ~ "Training", 
                                       lambda=="Control" ~ "control")) %>% 
  summarise(value=mean(hb, na.rm=T)) %>% 
  mutate(color_aes=factor(ifelse(lambda=="control", 1, 0))) %>% 
  ggplot(aes(favorite, lambda, fill=value))+
  geom_tile()+
  geom_label(aes(label= round(value, 3), color=color_aes))+
  scale_fill_gradient(low=paleta_alt[2], high=paleta_alt[[3]])+
  scale_color_manual(values = c("0"="grey80", "1"="#cea183"))+
  guides(fill="none", color="none")+
  xlab("favorite policy")+
  ylab("assigned policy")+
  theme(axis.title = element_text(), 
        text=element_text(size=12))+ # Sacar
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))

ggsave(gg, file=file.path(graficos, "heatmatp_fav.jpeg"), width=7, height=5)


gg<-dfanalisis %>% 
  drop_na(least_favorite, lambda) %>% 
  group_by(least_favorite, lambda= case_when(lambda=="Policy 1" ~ "reinforcement", 
                                       lambda=="Policy 2" ~ "promotion_criteria", 
                                       lambda=="Policy 3" ~ "Training", 
                                       lambda=="Control" ~ "control")) %>% 
  summarise(value=mean(hb, na.rm=T)) %>% 
  mutate(color_aes=factor(ifelse(lambda=="control", 1, 0))) %>% 
  ggplot(aes(least_favorite, lambda, fill=value))+
  geom_tile()+
  geom_label(aes(label= round(value, 3), color=color_aes))+
  scale_fill_gradient(low=paleta_alt[2], high=paleta_alt[[3]])+
  scale_color_manual(values = c("0"="grey80", "1"="#cea183"))+
  guides(fill="none", color="none")+
  xlab("least favorite policy")+
  ylab("assigned policy")+
  theme(axis.title = element_text(),
        text=element_text(size=12))+ # Sacar
  scale_y_discrete(expand=c(0,0))+
  scale_x_discrete(expand=c(0,0))

ggsave(gg, file=file.path(graficos, "heatmatp_leastfav.jpeg"), width=7, height=5)


#--------------------#
##### A. STUDY 1 #####
#--------------------#

###### H11 ######


modeloh11<- lm(data=dfanalisis, formula= hb ~ control)

modelsummary(models = modeloh11,
             stars=c("*"=.1, "**"=.05, "***"=.01),
             coef_map = c(control= "<b>Control</b>"),
             gof_omit = "BIC|AIC|R2 Within| R2 Within Adj.|Log.Lik.|R2 Adj.|RMSE",
             format="html",
             escape=F,
             output = file.path(tables, "h11.html"))

###### H12 ######


modeloh12a<- lm(data=dfanalisis, formula= hb ~ lambda)
modeloh12<- lm(data=dfanalisis, formula= hb ~ lambda+favorite)

modelsummary(models = list(modeloh12a, modeloh12),
             stars=c("*"=.1, "**"=.05, "***"=.01),
             gof_omit = "BIC|AIC|R2 Within| R2 Within Adj.|Log.Lik.|R2 Adj.|RMSE",
             format="html",
             output = file.path(tables, "h12.html"))

###### H13 ######

dfanalisis_h13 <- dfanalisis %>% 
  filter(favorite_num== politica | least_favorite_num==politica | control==1) %>%
  mutate(assignation= case_when(favorite_num==politica & control==0 ~ "favorite", 
                                least_favorite_num==politica & control==0 ~ "least-favorite", 
                                control==1 ~ "Control"),
         assignation= relevel(factor(assignation), ref="Control"), 
         politica= factor(paste0("Policy", politica)))


modelo_h13a<- lm(data=dfanalisis_h13, formula= hb ~ assignation)
modelo_h13<- lm(data=dfanalisis_h13, formula= hb ~ assignation+lambda+ favorite)

#CHECK: esto hay que pensarlo bien porque comparar con el grupo de control no queda muy claro, y si se debe controlar por la política preferida o por la que te ha tocado tampoco

modelsummary(models = list(modelo_h13a, modelo_h13),
             stars=c("*"=.1, "**"=.05, "***"=.01),
             gof_omit = "BIC|AIC|R2 Within| R2 Within Adj.|Log.Lik.|R2 Adj.|RMSE",
             format="html",
             output = file.path(tables, "h13.html"))

#--------------------#
##### B. STUDY 2 #####
#--------------------#

###### H21 ######

dfanalisish2<-dfanalisis %>% 
  filter(D %in% c("Policy", "Revelation")) %>% 
  mutate(D= relevel(factor(D), ref="Policy"))

modelo_h21<- lm(data=dfanalisish2, formula= hb ~ D)

modelsummary(models = modelo_h21,
             stars=c("*"=.1, "**"=.05, "***"=.01),
             gof_omit = "BIC|AIC|R2 Within| R2 Within Adj.|Log.Lik.|R2 Adj.|RMSE",
             format="html",
             output = file.path(tables, "h21.html"))

###### H22 ######

modelo_h22<- lm(data=dfanalisish2, formula= hb ~ D+lambda+ lambda:D+favorite)

modelsummary(models = modelo_h22,
             stars=c("*"=.1, "**"=.05, "***"=.01),
             gof_omit = "BIC|AIC|R2 Within| R2 Within Adj.|Log.Lik.|R2 Adj.|RMSE",
             format="html",
             output = file.path(tables, "h22.html"))

###### H23 ######

dfanalisish23f<-dfanalisish2 %>% 
  filter(favorite_num==politica) 

modelo_h23f<- lm(data=dfanalisish23f, formula= hb ~ D+favorite)


dfanalisish23lf<-dfanalisish2 %>% 
  filter(least_favorite_num==politica) 

modelo_h23lf<- lm(data=dfanalisish23lf, formula= hb ~ D+favorite)

modelsummary(models = list("Favorite"=modelo_h23f, "Least\n favorite"=modelo_h23lf),
             stars=c("*"=.1, "**"=.05, "***"=.01),
             gof_omit = "BIC|AIC|R2 Within| R2 Within Adj.|Log.Lik.|R2 Adj.|RMSE",
             format="html",
             output = file.path(tables, "h23.html"))

#--------------------#
##### C. STUDY 3 #####
#--------------------#

###### H31 ######

dfanalisish3<-dfanalisis %>% 
  filter(D %in% c("Revelation", "Awareness")) %>% 
  mutate(D= relevel(factor(D), ref="Revelation"))

modelo_h31<- lm(data=dfanalisish3, formula= hb ~ D)

modelsummary(models = modelo_h31,
             stars=c("*"=.1, "**"=.05, "***"=.01),
             gof_omit = "BIC|AIC|R2 Within| R2 Within Adj.|Log.Lik.|R2 Adj.|RMSE",
             format="html",
             output = file.path(tables, "h31.html"))

###### H32 ######

modelo_h32<- lm(data=dfanalisish3, formula= hb ~ D+lambda+ lambda:D+favorite)

modelsummary(models = modelo_h32,
             stars=c("*"=.1, "**"=.05, "***"=.01),
             gof_omit = "BIC|AIC|R2 Within| R2 Within Adj.|Log.Lik.|R2 Adj.|RMSE",
             format="html",
             output = file.path(tables, "h32.html"))


###### H33 ######

dfanalisish33f<-dfanalisish3 %>% 
  filter(favorite_num==politica) 

modelo_h33f<- lm(data=dfanalisish33f, formula= hb ~ D+favorite)


dfanalisish33lf<-dfanalisish3 %>% 
  filter(least_favorite_num==politica) 

modelo_h33lf<- lm(data=dfanalisish33lf, formula= hb ~ D+favorite)

modelsummary(models = list("Favorite"=modelo_h33f, "Least\n favorite"=modelo_h33lf),
             stars=c("*"=.1, "**"=.05, "***"=.01),
             gof_omit = "BIC|AIC|R2 Within| R2 Within Adj.|Log.Lik.|R2 Adj.|RMSE",
             format="html",
             output = file.path(tables, "h33.html"))

#--------------------------------#
##### D. STUDY 3 ALTERNATIVE #####
#--------------------------------#

###### H3B1 ######

dfanalisish3b<-dfanalisis %>% 
  filter(D %in% c("Control", "Awareness")) %>% 
  mutate(D= relevel(factor(D), ref="Control"))

modelo_h3b1<- lm(data=dfanalisish3b, formula= hb ~ D)

modelsummary(models = modelo_h3b1,
             stars=c("*"=.1, "**"=.05, "***"=.01),
             gof_omit = "BIC|AIC|R2 Within| R2 Within Adj.|Log.Lik.|R2 Adj.|RMSE",
             format="html",
             output = file.path(tables, "h31_alt.html"))


###### H3b3 ######

dfanalisish3b3f<-dfanalisish3b %>% 
  filter(favorite_num==politica | D=="Control") 

modelo_h3b3f<- lm(data=dfanalisish3b3f, formula= hb ~ D+favorite)

modelsummary(models = modelo_h3b3f,
             stars=c("*"=.1, "**"=.05, "***"=.01),
             include.rsquared = FALSE,
             include.adjrs = FALSE,
             include.nobs = FALSE,
             include.rmse = FALSE)

dfanalisish3b3lf<-dfanalisish3b %>% 
  filter(least_favorite_num==politica | D=="Control") 

modelo_h3b3lf<- lm(data=dfanalisish3b3lf, formula= hb ~ D+favorite)

modelsummary(models = list("Favorite"=modelo_h3b3f, "Least\n favorite"=modelo_h3b3lf),
             stars=c("*"=.1, "**"=.05, "***"=.01),
             gof_omit = "BIC|AIC|R2 Within| R2 Within Adj.|Log.Lik.|R2 Adj.|RMSE",
             format="html",
             output = file.path(tables, "h33_alt.html"))

#--------------------#
##### E. STUDY 4 #####
#--------------------#

###### H41 ######

# NO SE PUEDE EN PRINCIPIO

###### H42 ######

dfanalisis42<-dfanalisis %>% 
  filter(D == "Policy")

dfanalisis42_list<- list()
models_f<-list()
models_lf<-list()

for (x in 1:3){
  if (x==1){name= "Reinforcement"}else if (x==2){name= "Promotion criteria"}else{name="Training"}
  dfanalisis42_list[[paste0("policy_",x)]]<-dfanalisis42 %>%
    mutate(fav=as.numeric(favorite_num==x), 
           least_fav= as.numeric((least_favorite_num==x)), 
           assigned= as.numeric(politica==x))

models_f[[paste0(name)]] <- glm(data=dfanalisis42_list[[paste0("policy_",x)]], formula = fav ~ assigned, family = "binomial") 
models_lf[[paste0(name)]]<-  glm(data=dfanalisis42_list[[paste0("policy_",x)]], formula = least_fav ~ assigned, family = "binomial") 

}


models_all <- c(models_lf, models_f)

gt_tbl <- modelsummary(
  models = models_all,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_omit = "BIC|AIC|R2 Within| R2 Within Adj.|Log.Lik.|R2 Adj.|RMSE",
  output = "gt"
)

gt_tbl <- gt_tbl %>%
  gt::tab_spanner(
    label = "Least favorite",
    columns = 2:(1 + length(models_lf))
  ) %>%
  gt::tab_spanner(
    label = "Favorite",
    columns = (2 + length(models_lf)):(1 + length(models_all))
  )

gt::gtsave(
  gt_tbl,
  file = file.path(tables, "h42.html")
)
###### H43 ######

gg<-dfanalisis %>% 
  drop_na(favorite) %>% 
  group_by(D, favorite) %>% 
  summarise(valor = n(), .groups = "drop_last") %>% 
  mutate(
    n = sum(valor),
    p = valor / n,
    se = sqrt(p * (1 - p) / n),
    z = qnorm(0.875),   # 75% IC
    lo = p - z * se,
    hi = p + z * se
  ) %>% 
  ungroup() %>%  
  ggplot(aes(D, p, fill = favorite)) +
  geom_col(alpha=.85) +
  geom_errorbar(
    aes(ymin = lo, ymax = hi),
    width = 0.3, 
    color="grey40",
    linewidth=1
  ) +
  geom_text(
    aes(y = p / 2, label = percent(p, 1)),
    alpha=.85, 
    color="#faf3e3",
    size=7
  ) +
  scale_y_continuous(labels = label_percent(), expand=c(0,0)) +
  facet_wrap(~favorite, ncol = 3)+
  scale_fill_manual(values=c("#153a33", "#7498ae", "#6c0e33"))+
  guides(fill="none")+
  theme(axis.text = element_text(size=18))


ggsave(gg, filename=file.path(graficos, "h43_fav.jpeg"), width = 12, height = 8)

gg<-dfanalisis %>% 
  drop_na(least_favorite) %>% 
  group_by(D, least_favorite) %>% 
  summarise(valor = n(), .groups = "drop_last") %>% 
  mutate(
    n = sum(valor),
    p = valor / n,
    se = sqrt(p * (1 - p) / n),
    z = qnorm(0.875),   # 75% IC
    lo = p - z * se,
    hi = p + z * se
  ) %>% 
  ungroup() %>%  
  ggplot(aes(D, p, fill = least_favorite)) +
  geom_col(alpha=.85) +
  geom_errorbar(
    aes(ymin = lo, ymax = hi),
    width = 0.3, 
    color="grey40",
    linewidth=1
  ) +
  geom_text(
    aes(y = p / 2, label = percent(p, 1)),
    alpha=.85, 
    color="#faf3e3",
    size=7
  ) +
  scale_y_continuous(labels = label_percent(), expand=c(0,0)) +
  facet_wrap(~least_favorite, ncol = 3)+
  scale_fill_manual(values=c("#153a33", "#7498ae", "#6c0e33"))+
  guides(fill="none")+
  theme(axis.text = element_text(size=18))

ggsave(gg, filename=file.path(graficos, "h43_lfav.jpeg"), width = 12, height = 8)

dfanalisis %>% 
  filter(D!="Control") %>% 
  group_by(D, politica) %>% 
  summarise(valor=n()) %>% 
  group_by(D) %>% 
  mutate(ratio=valor/sum(valor))


# Posibles adiciones ¿Podemos explicar cuál es la política favorita de cada profesor?
