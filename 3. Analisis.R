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
  importance = "permutation",  # mejor que "impurity" en regresión
  num.trees = 1000,
  mtry = 3,        # número de variables candidatas en cada split
  min.node.size = 5, # tamaño mínimo de nodo terminal
  respect.unordered.factors = TRUE
)

modelo_rf$variable.importance

dfanalisis$pred_rf <- predict(modelo_rf, data = dfanalisis)$predictions

ggplot(dfanalisis, aes(x = hb, y = pred_rf)) +
  geom_point(alpha = 0.6) +
  geom_abline(color = "red") +
  labs(x = "Observado (ha)", y = "Predicho (Random Forest)") +
  theme_minimal()

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

###################################3333

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
  mutate(name=substr(name, 11, 200))+
  ggplot(aes(hb, value, color=name))+
  geom_smooth(se=F, size=2,method="loess", linetype="dotted")

dfanalisis %>% 
  select(hb, pct_culpa_alumnos, pct_culpa_familias, pct_culpa_profesorado, pct_culpa_sistema_educativo) %>% 
  pivot_longer(cols = c("pct_culpa_alumnos", "pct_culpa_familias", "pct_culpa_profesorado", "pct_culpa_sistema_educativo")) %>% 
  mutate(name=substr(name, 11, 200)) %>% 
  ggplot(aes(hb, value, color=name))+
  geom_smooth(se=F, method="lm", size=2,linetype="dashed") #ESTE incidencia en separación a lo largo de x y 

g

## Distribución de harshness
dfanalisis %>% 
  ggplot(aes(x= hb))+
  geom_density(smooth=1)+
  geom_vline(xintercept = mean(dfanalisis$hb, na.rm=T), color="red")+
  geom_vline(xintercept = median(dfanalisis$hb, na.rm=T), color="blue") #ESTE

dfanalisis %>% 
  mutate(politica_preferida= case_when(orden_pref_criterios_promo==1 ~ "Promoción", 
                                       orden_pref_formacion_prof==1 ~ "Formación", 
                                       orden_pref_refuerzo==1 ~ "Refuerzo")) %>% 
  filter(!is.na(politica_preferida)) %>% 
  ggplot(aes(x=hb, color= politica_preferida))+
  geom_density(size=2)+
  geom_vline(aes(xintercept=0.6))+
  geom_vline(aes(xintercept=0.4))+
  scale_x_continuous(breaks = seq(-0.2 ,1,by=.1)) # Forzar a que fuera bimodal, formacio promoción y refuerzo como nombres, hablar de que son grupos con distinto tamñao # ESTE




# harshness por meritocracia
dfanalisis %>% 
  filter(!is.na(meritocracia), meritocracia!=1) %>%
  group_by(meritocracia) %>% 
  summarise(value=mean(hb, na.rm=T)) %>%
  ggplot(aes(meritocracia, value))+
  geom_point()+
  geom_smooth(se=F)+
  scale_x_continuous(breaks = seq(0,10,by=1)) # Probar con 3 barras de 234, 567, 8910

dfanalisis %>% 
  filter(!is.na(meritocracia), meritocracia!=1) %>%
  group_by(meritocracia=ifelse(meritocracia %in% c(2:4), "2-4", 
                               ifelse(meritocracia %in% c(5:7), "3-5", 
                                      ifelse(meritocracia %in% c(8:10), "8-10")))) %>% 
  summarise(value=mean(hb, na.rm=T)) %>%
  ggplot(aes(meritocracia, value))+
  geom_col()
  scale_x_continuous(breaks = seq(0,10,by=1))


## Harshness por política preferida

dfanalisis %>% 
  mutate(politica_preferida= case_when(orden_pref_criterios_promo==1 ~ "Promoción", 
                                     orden_pref_formacion_prof==1 ~ "Formación", 
                                     orden_pref_refuerzo==1 ~ "Refuerzo")) %>% 
  filter(!is.na(politica_preferida)) %>% 
  group_by(politica_preferida) %>% 
  summarise(value=mean(hb, na.rm=T)- mean(dfanalisis$hb, na.rm=T), 
            se=1.96*sd(hb, na.rm=T)/sqrt(n())) %>%
  mutate(lower= value-se, 
         higher=value+se) %>% 
  ggplot(aes(politica_preferida, value))+
  geom_col()+ # Restar media
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.3, size=2) # ESTE


## Harshness por nivel de empatía

dfanalisis %>% 
  group_by(empatia) %>% 
  summarise(value=mean(hb), 
            se= 1.96*sd(hb, na.rm=T)/sqrt(n())) %>%
  mutate(lower= value-se, 
         higher=value+se) %>% 
  ggplot(aes(empatia, value))+
  geom_point()+
  geom_line(size=2)+
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.3, size=2) # ESTE


summary(lm)

## multipregunta 21

ggs<-list()

for (i in c("estudiantes", "pasar_sin_competencias", "preparados_nivel_sig", "demasiados_recursos_repetidores", "recursos_repetidores_ineficaces")){

  variable1<- paste0("impacto_centro_", i)
  variable2<-paste0("impacto_region_", i)
  
  ggs[[i]]<-dfanalisis %>%
  group_by(x=ifelse(!is.na(get(variable1)), get(variable1), get(variable2)), 
           cat= ifelse(!is.na(get(variable1)), "centro", "region")) %>% 
  summarise(value=mean(hb, na.rm=T)) %>% 
  ggplot(aes(x, value, color=cat))+
  geom_point()+
  geom_line()+
  ggtitle(i)+ 
  theme(legend.position = "none")
}

legend_plot <- get_legend(ggs[[1]] + theme(legend.position = "right"))

plot_grid(
  plotlist = c(ggs, list(legend_plot)),
  ncol = 2
)

# Segunda opción (todo unido)

ggs2<-list()

for (i in c("estudiantes", "pasar_sin_competencias", "preparados_nivel_sig", "demasiados_recursos_repetidores", "recursos_repetidores_ineficaces")){
  
  variable1<- paste0("impacto_centro_", i)
  variable2<-paste0("impacto_region_", i)
  
  i<-ifelse(i=="demasiados_recursos_repetidores", "demasiados_recursos", i)
  
  ggs2[[i]]<-dfanalisis %>%
    group_by(x=ifelse(!is.na(get(variable1)), get(variable1), get(variable2))) %>% 
    summarise(value=mean(hb, na.rm=T),
              se196=1.96*sd(hb, na.rm=T)/sqrt(n())) %>%
    mutate(lower= value-se196, 
           higher= value+se196) %>% 
    ggplot(aes(x, value))+
    geom_point()+
    geom_line()+
    geom_errorbar(aes(ymin=lower, ymax=higher))+
    ggtitle(i)+ 
    theme(legend.position = "none")
}

plot_grid(
  plotlist = ggs2)


ggs2<-list()

for (i in c("estudiantes", "pasar_sin_competencias", "preparados_nivel_sig", "demasiados_recursos_repetidores", "recursos_repetidores_ineficaces")){
  
  variable1<- paste0("impacto_centro_", i)
  variable2<-paste0("impacto_region_", i)
  
  i<-ifelse(i=="demasiados_recursos_repetidores", "demasiados_recursos", i)
  
  ggs2[[i]]<-dfanalisis %>%
    group_by(
      x = cut(
        ifelse(!is.na(get(variable1)), get(variable1), get(variable2)),
        breaks = c(-Inf, 3, 6, 10),   # cortes
        labels = c("0-3", "4-6", "7-10"), 
        right = TRUE
      )
    ) %>%
    summarise(
      value = mean(hb, na.rm = TRUE),
      se196 = 1.96 * sd(hb, na.rm = TRUE) / sqrt(n())
    ) %>%
    mutate(lower = value - se196,
           higher = value + se196) %>%
    ggplot(aes(x, value, group = 1)) +   # group=1 para que conecte puntos
    geom_point() +
    geom_col() +
    geom_errorbar(aes(ymin = lower, ymax = higher)) +
    ggtitle(i) +
    theme(legend.position = "none")
  
} # restar media 

plot_grid(
  plotlist = ggs2) # ver este con grupos


