source("C:/Users/ignac/OneDrive/Documentos/GitHub/COTEC-survey-experiment/0. main.R")

df<-read_parquet("cleandata.parquet")


#### HARSHNESS MEASURES ####
############################

## NUMBER OF REPETITIONS

namelist<- setdiff(namelist, c("alumno_1_1", "alumno_2_1")) # Eliminar los alumnos de los ejemplos

df1<-df[c("id",namelist)]

dfnrep<-df1 %>% 
  pivot_longer(all_of(namelist)) %>%
  drop_na(value) %>% 
  group_by(id) %>%
  mutate(value=ifelse(value=="repite", 1, 0)) %>% 
  summarise(ha=mean(value, na.rm=T)) %>% 
  ungroup()

## AVERAGE DEVIATION

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

## DEVIATION WITH RESPECT TO PREDICTED PROBABILITY


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


## DEVIATION WITH RESPECT TO PREDICTED PROBABILITY IN COMPLETE MODEL


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

## Unión 

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


#### CHARACTERIZING THE TEACHERS ####
#####################################

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



set.seed(123)

modelo_rf <- ranger(
  formula = hb ~ nivel + experiencia + antiguedad3 + female + edad +
    titularidad + indefinido + grupos_docencia + impacto_estudiantes +
    empatia + meritocracia,
  data = dfanalisis,
  importance = "permutation",
  num.trees = 1000,
  mtry = 3,
  min.node.size = 5, 
  respect.unordered.factors = TRUE
)

modelo_rf$variable.importance

dfanalisis$pred_rf <- predict(modelo_rf, data = dfanalisis)$predictions

ggplot(dfanalisis, aes(x = hb, y = pred_rf)) +
  geom_point(alpha = 0.6) +
  geom_abline(color = "red") +
  labs(x = "Observado (ha)", y = "Predicho (Random Forest)") 

### Shap values ###

library(iml)
library(fastshap)
library(shapviz)
library(ggbeeswarm)

variables_rf<-names(modelo_rf$variable.importance)

dfshap<- dfanalisis[c(variables_rf)]

shap_values<-Predictor$new(modelo_rf, data=dfshap)
shapley<-Shapley$new(shap_values, x.interest = dfshap[1,])

f<- function(object, newdata) {
  predict(modelo_rf, data=newdata)$predictions
}

set.seed(123)

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

sdf %>% 
  ggplot(aes(x = shap_value, y = variable)) +
  geom_quasirandom(alpha = 0.5, width=.3)+ 
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

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

#### HYPOTHESES ####
####################

#### STUDY I

# H11

dfanalisis<-dfanalisis %>% 
  mutate(control=ifelse(treatment==1, 1, 0))

modeloh11a<- lm(data=dfanalisis, formula= ha ~ control)

modelsummary(models = modeloh11a,
             stars=c("*"=.1, "**"=.05, "***"=.01),
             include.rsquared = FALSE,
             include.adjrs = FALSE,
             include.nobs = FALSE,
             include.rmse = FALSE)




modeloh11a<- lm(data=dfanalisis, formula= ha ~ politica)

modelsummary(models = modeloh11a,
             stars=c("*"=.1, "**"=.05, "***"=.01),
             include.rsquared = FALSE,
             include.adjrs = FALSE,
             include.nobs = FALSE,
             include.rmse = FALSE)

###################################

dfanalisis %>% 
  mutate(
    # Crear cortes
    x_cut = cut(
      hb, 
      breaks = seq(-1, 1, by = 0.05),
      include.lowest = TRUE,
      right = FALSE
    ),
    # Convertir cada corte en el punto medio del intervalo
    x = as.numeric(sub("\\((.+),.*", "\\1", sub("\\[|\\)", "", x_cut))) + 0.025
  ) %>% 
  group_by(x) %>% 
  summarise(alumnos= mean(pct_culpa_alumnos, na.rm=T),
            familias= mean(pct_culpa_familias, na.rm=T), 
            profesorado= mean(pct_culpa_profesorado, na.rm=T), 
            sistema= mean(pct_culpa_sistema_educativo, na.rm=T)) %>% 
  pivot_longer(cols = c("alumnos", "familias", "profesorado", "sistema")) %>% 
  ggplot(aes(x, value ,color=name))+
  geom_point()+
  geom_hline(yintercept = 25)

g<-dfanalisis %>% 
  mutate(
    x = floor(hb / 0.2) * 0.2   # redondea hacia arriba al múltiplo más cercano de 0.05
  ) %>% 
  group_by(x) %>% 
  summarise(
    alumnos = mean(pct_culpa_alumnos, na.rm = TRUE),
    familias = mean(pct_culpa_familias, na.rm = TRUE), 
    profesorado = mean(pct_culpa_profesorado, na.rm = TRUE), 
    sistema = mean(pct_culpa_sistema_educativo, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  pivot_longer(cols = c("alumnos", "familias", "profesorado", "sistema")) %>% 
  drop_na(value) %>% 
  ggplot(aes(x, value, color = name)) +
  geom_line(size=1) +
  geom_point()+
  geom_hline(yintercept = 25, linetype = "dashed")+
  geom_vline(xintercept = mean(dfanalisis$hb, na.rm=T), linetype="dashed") +
  scale_x_continuous(breaks = seq(-0.2, 0.8, by=.2))


dfanalisis<-dfanalisis %>% 
  mutate(alumno2= pct_culpa_alumnos^2, 
         familias2= pct_culpa_familias^2, 
         profesorado2= pct_culpa_profesorado^2, 
         sistema2= pct_culpa_sistema_educativo^2
  )

#summary(lm(data=dfanalisis, formula= hb ~ pct_culpa_profesorado )) # si le restas 25 es diferencia con respecto a 1/4

modalu<-lm(data=dfanalisis, formula =  hb ~ pct_culpa_alumnos)
modfam<-lm(data=dfanalisis, formula = hb ~ pct_culpa_familias)
modprof<-lm(data=dfanalisis, formula =  hb ~ pct_culpa_profesorado)
modsist<-lm(data=dfanalisis, formula = hb ~ pct_culpa_sistema_educativo)

dfanalisisrec<-dfanalisis %>% drop_na(hb, pct_culpa_alumnos)

dfanalisisrec$pred_alumno<-predict(modalu, newdata = dfanalisisrec)
dfanalisisrec$pred_fam<-predict(modfam, newdata = dfanalisisrec)
dfanalisisrec$pred_prof<-predict(modprof, newdata = dfanalisisrec)
dfanalisisrec$pred_sist<-predict(modsist, newdata = dfanalisisrec)

dfanalisisrec %>% 
  pivot_longer(cols = c("pred_alumno", "pred_fam", "pred_prof", "pred_sist")) %>%
  arrange(name, hb) %>% 
  ggplot(aes(hb, value, color=name))+
  geom_point()

## de quién es la culpa por harshness

dfanalisis %>% 
  select(hb, pct_culpa_alumnos, pct_culpa_familias, pct_culpa_profesorado, pct_culpa_sistema_educativo) %>% 
  pivot_longer(cols = c("pct_culpa_alumnos", "pct_culpa_familias", "pct_culpa_profesorado", "pct_culpa_sistema_educativo")) %>% 
  mutate(name=substr(name, 11, 200)) %>% 
  ggplot(aes(hb, value, color=name))+
  geom_smooth(se=F, size=2,method="loess", linetype="dotted")

gg<- dfanalisis %>% 
  select(hb, pct_culpa_alumnos, pct_culpa_familias, pct_culpa_profesorado, pct_culpa_sistema_educativo) %>% 
  pivot_longer(cols = c("pct_culpa_alumnos", "pct_culpa_familias", "pct_culpa_profesorado", "pct_culpa_sistema_educativo")) %>% 
  mutate(name=substr(name, 11, 200)) %>% 
  ggplot(aes(hb, value, color=name))+
  geom_smooth(se=F, method="lm", size=2,linetype="dashed") + #ESTE incidencia en separación a lo largo de x y 
  scale_color_manual(values=c("#00b89f", "#002059", "#a29cb8","#69d3e3"))

ggsave(gg, file=paste0(salidas, "culpa.jpeg"), width=7, height=5)


## Distribución de harshness
gg<-dfanalisis %>% 
  ggplot(aes(x= hb))+
  geom_density(size=1, fill= "#537d90", alpha=.2,color="#537d90")+
  #geom_vline(xintercept = mean(dfanalisis$hb, na.rm=T), size=1, color="#00b89f")+
  geom_vline(xintercept = median(dfanalisis$hb, na.rm=T), size=1, color="#a29cb8") #ESTE

ggsave(gg, file=paste0(salidas, "densidad.jpeg"), width=7, height=5)


gg<- 
  dfanalisis %>% 
  mutate(politica_preferida= case_when(orden_pref_criterios_promo==1 ~ "Promoción", 
                                       orden_pref_formacion_prof==1 ~ "Formación", 
                                       orden_pref_refuerzo==1 ~ "Refuerzo")) %>% 
  filter(!is.na(politica_preferida)) %>% 
  ggplot(aes(x=hb, color= politica_preferida, fill=politica_preferida))+
  geom_density(size=1, alpha=.07)+
  geom_vline(aes(xintercept=0.6), linetype="longdash", color="#537d90")+
  geom_vline(aes(xintercept=0.4),  linetype="longdash", color="#537d90" )+
  scale_x_continuous(breaks = seq(-0.2 ,1,by=.1)) + # Forzar a que fuera bimodal, formacio promoción y refuerzo como nombres, hablar de que son grupos con distinto tamñao # ESTE
  scale_color_manual(values = c("#002059", "#00b89f", "#a29cb8"))+
  theme(legend.position = c(.15,.8), 
        legend.text  = element_text(size = 12))

ggsave(gg, file=paste0(salidas, "densidad_politicas.jpeg"), width=7, height=5)



# harshness por meritocracia
gg<-
  dfanalisis %>% 
  filter(!is.na(meritocracia), meritocracia!=1) %>%
  group_by(meritocracia=ifelse(meritocracia %in% c(2:4), "2-4", 
                               ifelse(meritocracia %in% c(5:7), "3-5", 
                                      ifelse(meritocracia %in% c(8:10), "8-10", NA)))) %>% 
  summarise(value=mean(hb, na.rm=T), 
            n=n(), 
            se= 1.96*sd(hb, na.rm=T)/sqrt(n)) %>%
    mutate(low= value-se, 
           high=value+se) %>% 
  ggplot(aes(meritocracia, value))+
  geom_col(fill="#537d90")+
    geom_errorbar(aes(ymin=low, ymax=high), size=1, width=.2, color="#a29cb8")+
  geom_text(color="white", aes(y=value/2,label= round(value*100, 1)))
 
ggsave(gg, file=paste0(salidas, "meritocracia.jpeg"), width=7, height=5)


## Harshness por política preferida

mu_total <- mean(dfanalisis$hb, na.rm = TRUE)

res <- dfanalisis %>%
  mutate(
    politica_preferida = case_when(
      orden_pref_criterios_promo == 1 ~ "Promoción",
      orden_pref_formacion_prof == 1 ~ "Formación",
      orden_pref_refuerzo == 1 ~ "Refuerzo",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(politica_preferida), !is.na(hb)) %>%
  group_by(politica_preferida) %>%
  do(tidy(t.test(.$hb, mu = mu_total))) %>%
  ungroup()

plotdata <- res %>%
  mutate(
    diff = estimate - mu_total,
    lower = conf.low - mu_total,
    upper = conf.high - mu_total
  )

gg<-plotdata %>% 
ggplot(aes(x = politica_preferida, y = diff)) +
  geom_col(fill="#537d90") +
  geom_text(color="white", aes(y=diff/2,label= round(diff*100, 1)))+
  #geom_errorbar(aes(ymin = lower, ymax = upper), color="#a29cb8", width = 0.2, size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") 

ggsave(gg, file=paste0(salidas, "politicas_pref_harshness.jpeg"), width=7, height=5)

## Harshness por nivel de empatía

dfanalisis %>% 
  group_by(empatia) %>% 
  summarise(value=mean(hb), 
            se= 1.96*sd(hb, na.rm=T)/sqrt(n()), 
            n()) %>%
  mutate(lower= value-se, 
         higher=value+se) %>% 
  ggplot(aes(empatia, value))+
  geom_point()+
  geom_line(size=2)+
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.3, size=2) 


## multipregunta 21

# ggs<-list()
# 
# for (i in c("estudiantes", "pasar_sin_competencias", "preparados_nivel_sig", "demasiados_recursos_repetidores", "recursos_repetidores_ineficaces")){
# 
#   variable1<- paste0("impacto_centro_", i)
#   variable2<-paste0("impacto_region_", i)
#   
#   ggs[[i]]<-dfanalisis %>%
#   group_by(x=ifelse(!is.na(get(variable1)), get(variable1), get(variable2)), 
#            cat= ifelse(!is.na(get(variable1)), "centro", "region")) %>% 
#   summarise(value=mean(hb, na.rm=T)) %>% 
#   ggplot(aes(x, value, color=cat))+
#   geom_point()+
#   geom_line()+
#   ggtitle(i)+ 
#   theme(legend.position = "none")
# }
# 
# legend_plot <- get_legend(ggs[[1]] + theme(legend.position = "right"))
# 
# plot_grid(
#   plotlist = c(ggs, list(legend_plot)),
#   ncol = 2
# )
# 
# # Segunda opción (todo unido)
# 
# ggs2<-list()
# 
# for (i in c("estudiantes", "pasar_sin_competencias", "preparados_nivel_sig", "demasiados_recursos_repetidores", "recursos_repetidores_ineficaces")){
#   
#   variable1<- paste0("impacto_centro_", i)
#   variable2<-paste0("impacto_region_", i)
#   
#   i<-ifelse(i=="demasiados_recursos_repetidores", "demasiados_recursos", i)
#   
#   ggs2[[i]]<-dfanalisis %>%
#     group_by(x=ifelse(!is.na(get(variable1)), get(variable1), get(variable2))) %>% 
#     summarise(value=mean(hb, na.rm=T),
#               se196=1.96*sd(hb, na.rm=T)/sqrt(n())) %>%
#     mutate(lower= value-se196, 
#            higher= value+se196) %>% 
#     ggplot(aes(x, value))+
#     geom_point()+
#     geom_line()+
#     geom_errorbar(aes(ymin=lower, ymax=higher))+
#     ggtitle(i)+ 
#     theme(legend.position = "none")
# }
# 
# plot_grid(
#   plotlist = ggs2)
# 
# 
# ggs2<-list()
# 
# for (i in c("estudiantes", "pasar_sin_competencias", "preparados_nivel_sig", "demasiados_recursos_repetidores", "recursos_repetidores_ineficaces")){
#   
#   variable1<- paste0("impacto_centro_", i)
#   variable2<-paste0("impacto_region_", i)
#   
#   i<-ifelse(i=="demasiados_recursos_repetidores", "demasiados_recursos", i)
#   
#   ggs2[[i]]<-dfanalisis %>%
#     mutate(
#       x1= ifelse(!is.na(get(variable1)), get(variable1), get(variable2)),
#       x = cut(
#         x1,
#         breaks = c(-Inf, 3, 6, 10),   # cortes
#         labels = c("0-3", "4-6", "7-10"), 
#         right = TRUE
#       )
#     ) %>%
#     drop_na(x) %>% 
#     group_by(x) %>% 
#     summarise(
#       value = mean(hb, na.rm = TRUE),
#       se196 = 1.96 * sd(hb, na.rm = TRUE) / sqrt(n())
#     ) %>%
#     mutate(lower = value - se196,
#            higher = value + se196) %>%
#     ggplot(aes(x, value, group = 1)) +   # group=1 para que conecte puntos
#     geom_point() +
#     geom_col() +
#     geom_errorbar(aes(ymin = lower, ymax = higher)) +
#     ggtitle(i) +
#     theme(legend.position = "none")
#   
# } # restar media 
# 
# plot_grid(
#   plotlist = ggs2) # ver este con grupos
# 
# 
# 
# ggs2 <- list()
# 
# for (i in c("estudiantes", "pasar_sin_competencias", "preparados_nivel_sig",
#             "demasiados_recursos_repetidores", "recursos_repetidores_ineficaces")) {
#   
#   variable1 <- paste0("impacto_centro_", i)
#   variable2 <- paste0("impacto_region_", i)
#   
#   i <- ifelse(i == "demasiados_recursos_repetidores", "demasiados_recursos", i)
#   
#   # Construyo dataset con cortes
#   dat <- dfanalisis %>%
#     mutate(
#       x1 = ifelse(!is.na(get(variable1)), get(variable1), get(variable2)),
#       x = cut(
#         x1,
#         breaks = c(-Inf, 3, 6, 10),
#         labels = c("0-3", "4-6", "7-10"),
#         right = TRUE
#       )
#     ) %>%
#     drop_na(x)
#   
#   # media total (baseline)
#   total_vals <- dat$hb
#   
#   # test de medias por grupo vs. total
#   stats <- dat %>%
#     group_by(x) %>%
#     summarise(
#       broom::tidy(t.test(hb, total_vals)) %>%
#         mutate(coef = estimate1 - estimate2), # diferencia de medias
#       .groups = "drop"
#     )
#   
#   ggs2[[i]] <- stats %>%
#     mutate(
#       lower = conf.low,
#       higher = conf.high
#     ) %>%
#     ggplot(aes(x, coef, group = 1)) +
#     geom_col(fill="#a29cb8") +
#     #geom_point() +
#     #geom_errorbar(aes(ymin = lower, ymax = higher)) +
#     geom_hline(yintercept = 0, linetype = "dashed") +
#     ggtitle(i) +
#     theme(legend.position = "none")
# }
# 
# plot_grid(plotlist = ggs2)

data<-dfanalisis

variables_centro_region<-c("estudiantes", "pasar_sin_competencias", "preparados_nivel_sig",
                           "demasiados_recursos_repetidores", "recursos_repetidores_ineficaces")
variables_cr_corr<- c(variables_centro_region[c(1:3, 5)], "demasiados_recursos")

for (i in variables_centro_region) {
  
  variable1 <- paste0("impacto_centro_", i)
  variable2 <- paste0("impacto_region_", i)
  
  i <- ifelse(i == "demasiados_recursos_repetidores", "demasiados_recursos", i)
  
  # Construyo dataset con cortes
  data <- data %>%
    mutate(!!i := ifelse(!is.na(get(variable1)), get(variable1), get(variable2)))
}

gg<-data %>% 
  select(hb, variables_cr_corr) %>% 
  pivot_longer(cols = variables_cr_corr) %>% 
  ggplot(aes(x=value, hb, color=name))+
  geom_smooth(se=F, method="lm")+
  scale_color_manual(values=c("#00b89f","#537d90", "#002059", "#a47dab","#69d3e3"))

ggsave(gg, file=paste0(salidas, "impacto.jpeg"), width=11, height=5)



GGally::ggpairs(data[, c(variables_cr_corr)])

combs <- combn(variables_cr_corr, 2, simplify = FALSE)

# Creamos un gráfico para cada combinación
plots <- lapply(combs, function(vars) {
  data %>%
    ggplot(aes_string(x = vars[1], y = vars[2])) +
    #geom_point(alpha = 0.5) +
    geom_smooth(method = "lm") +
    ggtitle(paste(vars[1], "vs", vars[2]))
})

# Mostrar todos los gráficos (en RStudio Viewer se verá uno por uno)
plots[[1]]  # primer gráfico
plots[[2]]  # segundo gráfico, etc.

plot_grid(plotlist = plots)
