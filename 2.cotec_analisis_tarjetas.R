library(mapSpain)
library(sf)
library(tidyverse)
library(readxl)
library(scales)
library(randomForest)
library(arrow)
library(treemapify)

wd<- "C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Phd/RCT/Datos y codigo/"
setwd(wd)

salidas<-paste0(wd, "Presentacion_cotec/graficos/")

df<-read_parquet("cleandata.parquet")

theme_set(theme_minimal()+
            theme(axis.text = element_text(face="bold", color="#404040"),
                  axis.title=element_blank(),
                  legend.title=element_blank(), 
                  panel.grid.major.y = element_line(color="grey80"),
                  panel.grid.major.x = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  axis.line = element_line(color="grey50")))

paleta<- c("#002059","#011552","#537d90","#a29cb8", "#69d3e3", "#a47dab", "#00b89f")


namelist<-c()
namelist_ps<-c()
for (x in 1:8){
  for (y in 1:8) {
    name<-paste("alumno", x,y, sep = "_")
    name_ps<-paste0(name, "_ps")
    namelist<-c(namelist,name)
    namelist_ps<-c(namelist_ps, name, name_ps)
  }
}

vector_niño<- c("alumno_1_3", "alumno_1_4", "alumno_1_7", "alumno_1_8",
               "alumno_2_1", "alumno_2_2", "alumno_2_5", "alumno_2_6",
               "alumno_3_3", "alumno_3_4", "alumno_3_7", "alumno_3_8",
               "alumno_4_1", "alumno_4_2", "alumno_4_5", "alumno_4_6",
               "alumno_5_1", "alumno_5_4", "alumno_5_5", "alumno_5_8",
               "alumno_6_2", "alumno_6_3", "alumno_6_6", "alumno_6_7",
               "alumno_7_1", "alumno_7_4", "alumno_7_5", "alumno_7_8",
               "alumno_8_2", "alumno_8_3", "alumno_8_6", "alumno_8_7")
vector_extranjero<- c("alumno_1_3", "alumno_1_4", "alumno_1_5", "alumno_1_6",
                     "alumno_2_5", "alumno_2_6", "alumno_2_7", "alumno_2_8",
                     "alumno_3_1", "alumno_3_2", "alumno_3_7", "alumno_3_8",
                     "alumno_4_1", "alumno_4_2", "alumno_4_3", "alumno_4_4",
                     "alumno_5_4", "alumno_5_5", "alumno_5_6", "alumno_5_7",
                     "alumno_6_1", "alumno_6_6", "alumno_6_7", "alumno_6_8",
                     "alumno_7_1", "alumno_7_2", "alumno_7_3", "alumno_7_8",
                     "alumno_8_2", "alumno_8_3", "alumno_8_4", "alumno_8_5")

vector_suspensos<-c("alumno_1_4", "alumno_1_7", "alumno_1_8",
                    "alumno_2_4", "alumno_2_7", "alumno_2_8",
                    "alumno_3_4", "alumno_3_7", "alumno_3_8",
                    "alumno_4_4", "alumno_4_7", "alumno_4_8",
                    "alumno_5_2", "alumno_5_4", "alumno_5_5", "alumno_5_7", "alumno_5_8",
  "alumno_6_2", "alumno_6_4", "alumno_6_5", "alumno_6_7", "alumno_6_8",
  "alumno_7_2", "alumno_7_4", "alumno_7_5", "alumno_7_7", "alumno_7_8",
  "alumno_8_2", "alumno_8_4", "alumno_8_5", "alumno_8_7", "alumno_8_8")

vector_carencias <- c("alumno_1_2", "alumno_1_5", "alumno_1_7", "alumno_1_8",
                     "alumno_2_2", "alumno_2_5", "alumno_2_7", "alumno_2_8",
                     "alumno_3_2", "alumno_3_5", "alumno_3_7", "alumno_3_8",
                     "alumno_4_2", "alumno_4_5", "alumno_4_7", "alumno_4_8",
                     "alumno_5_4", "alumno_5_6", "alumno_5_7", "alumno_5_8",
                     "alumno_6_4", "alumno_6_6", "alumno_6_7", "alumno_6_8",
                     "alumno_7_4", "alumno_7_6", "alumno_7_7", "alumno_7_8",
                     "alumno_8_4", "alumno_8_6", "alumno_8_7", "alumno_8_8")

vector_absentista<-  c("alumno_1_4", "alumno_1_5", "alumno_1_6", "alumno_1_8",
                                            "alumno_2_4", "alumno_2_5", "alumno_2_6", "alumno_2_8",
                                            "alumno_3_4", "alumno_3_5", "alumno_3_6", "alumno_3_8",
                                            "alumno_4_4", "alumno_4_5", "alumno_4_6", "alumno_4_8",
                                            "alumno_5_3", "alumno_5_7",
                                            "alumno_6_3", "alumno_6_7",
                                            "alumno_7_3", "alumno_7_7",
                                            "alumno_8_3", "alumno_8_7")

vector_expulsion<- c("alumno_1_3", "alumno_1_6", "alumno_1_7", "alumno_1_8",
                     "alumno_2_3", "alumno_2_6", "alumno_2_7", "alumno_2_8",
                     "alumno_3_3", "alumno_3_6", "alumno_3_7", "alumno_3_8",
                     "alumno_4_3", "alumno_4_6", "alumno_4_7", "alumno_4_8",
                     "alumno_5_5", "alumno_5_6", "alumno_5_8",
                     "alumno_6_5", "alumno_6_6", "alumno_6_8",
                     "alumno_7_5", "alumno_7_6", "alumno_7_8",
                     "alumno_8_5", "alumno_8_6", "alumno_8_8")


df1<-df[,c("id", "orden_pref_criterios_promo", "orden_pref_formacion_prof", "orden_pref_refuerzo", namelist_ps)]

df2<-df1 %>% 
  select(-ends_with("_ps")) %>% 
  pivot_longer(cols = all_of(namelist),
               names_to = "alumno",
               values_to = "repite"
               ) %>% 
  filter(!is.na(repite)) %>% 
  mutate(repite=ifelse(repite=="pasa", 0,1))

df3<-df1 %>% 
  select(id, ends_with("_ps")) %>% 
  pivot_longer(cols = ends_with("_ps"), 
               names_to="alumno", 
               values_to = "tiempo") %>% 
  filter(!is.na(tiempo)) %>% 
  mutate(alumno=substr(alumno, 1, 10))

df2<-inner_join(df2, df3)


df2<-df2 %>% 
  mutate(niño=ifelse(alumno %in% vector_niño,1,0), 
         extranjero=ifelse(alumno %in% vector_extranjero, 1, 0), 
         suspensos=ifelse(alumno%in% vector_suspensos, 1, 0), 
         carencias=ifelse(alumno %in% vector_carencias, 1, 0), 
         absentista=ifelse(alumno %in% vector_absentista, 1, 0), 
         expulsion=ifelse(alumno %in% vector_expulsion, 1, 0), 
         suma=niño+extranjero+suspensos+carencias+absentista+expulsion)

df2<-df2 %>% filter(!alumno %in% c("alumno_1_1", "alumno_2_1"))



# Alguno vs 0

gg<-df2 %>% 
  mutate(suma %in% c(0,6)) %>% 
  group_by(alumno=ifelse(suma==0, "Sin\ncondicionantes", "Con\ncondicionantes")) %>%
  summarise(ratio=sum(repite)/n()) %>% 
  ggplot(aes(alumno, ratio))+
  geom_col(fill=paleta[3])+
  geom_text(aes(y=ratio/2,label=percent(ratio, .1)),color="grey90", fontface="bold")+
  scale_y_continuous(labels = scales::label_percent())

ggsave(gg, filename=paste0(salidas, "algunovsninguno.jpeg"), width=5)


# MONOTONÍA

gg<-df2 %>% 
  group_by(suma) %>% 
  summarise(p=mean(repite), 
            nn=n()) %>% 
  mutate(se=sqrt(p*(1-p)/nn), 
         conf_low=p-1.96*se, 
         conf_high=p+1.96*se) %>% 
  ggplot(aes(suma, p))+
  geom_point(shape=18, color=paleta[3]) +
  geom_line( color=paleta[3])+
  geom_errorbar(aes(ymax=conf_high, ymin=conf_low), width=.2, color=paleta[3])+
  scale_x_continuous(breaks = seq(0,8,1))+
  scale_y_continuous(labels = label_percent())

ggsave(gg, filename=paste0(salidas, "monotonia",".jpeg"), width=5)


# 1 FALLO 

gg<-df2 %>% 
  filter(suma <=1) %>%
  mutate(alumno= case_when(alumno=="alumno_1_2" ~ "carencias",
                           alumno=="alumno_2_3"~ "expulsion",
                           alumno=="alumno_3_1" ~"extranjero",
                           alumno=="alumno_5_1" ~"niño",
                           alumno=="alumno_5_2" ~ "suspensos",
                           alumno=="alumno_5_3" ~ "absentista",
                           alumno=="alumno_6_1" ~"extranjero",
                           alumno=="alumno_8_1" ~ "control")) %>%
  group_by(alumno) %>%
  summarise(p=mean(repite),
            nn=n()
            ) %>% 
  mutate(se=sqrt(p*(1-p)/nn), 
         conf_low=p-1.96*se, 
         conf_high=p+1.96*se,
         control=factor(ifelse(alumno=="control",1,0)),
         alumno=factor(alumno, levels=c("control", "niño", "extranjero", "absentista", "expulsion", "carencias", "suspensos"))) %>% 
  ggplot(aes(alumno, p, fill=control))+
  geom_col(alpha=.85)+
  geom_errorbar(aes(ymin=conf_low, ymax=conf_high), width=.2, color=paleta[2])+
  geom_text(aes(label=percent(p, .1)), position = position_stack(vjust=.5), color="grey85",fontface="bold")+
  scale_y_continuous(labels = label_percent())+
  scale_fill_manual(values=c(paleta[3], paleta[4]))+
  guides(fill="none")

ggsave(gg, filename=paste0(salidas, "unfallo",".jpeg"), width=5)

# GEOM_TILE INTERACCIÓN DOS FALLOS

df3<-df2 %>%   
  filter(suma %in% c(1,2)) %>% 
  mutate(
  carac_1 = case_when(
    niño == 1 ~ "niño",
    extranjero == 1 ~ "extranjero",
    suspensos == 1 ~ "suspensos",
    carencias == 1 ~ "carencias",
    absentista == 1 ~ "absentista",
    expulsion == 1 ~ "expulsion"
  ),
  carac_2 = case_when(
    niño == 1 & carac_1 != "niño" ~ "niño",
    extranjero == 1 & carac_1 != "extranjero" ~ "extranjero",
    suspensos == 1 & carac_1 != "suspensos" ~ "suspensos",
    carencias == 1 & carac_1 != "carencias" ~ "carencias",
    absentista == 1 & carac_1 != "absentista" ~ "absentista",
    expulsion == 1 & carac_1 != "expulsion" ~ "expulsion"
  ),
  carac_2=ifelse(is.na(carac_2), carac_1, carac_2),
  carac_1= factor(carac_1, levels=c("niño", "extranjero", "absentista", "expulsion", "carencias", "suspensos")),
  carac_2= factor(carac_2, levels=c("niño", "extranjero", "absentista", "expulsion", "carencias", "suspensos"))

) %>% 
  group_by(carac_1, carac_2) %>% 
  summarise(p=mean(repite)) 

gg<-df3%>%
  bind_rows(df3 %>% rename(carac_1 = carac_2, carac_2 = carac_1)) %>%
  distinct(carac_1, carac_2, .keep_all = TRUE) %>% 
  mutate(border=factor(ifelse(carac_1==carac_2, 1, 0))) %>% 
  ggplot(aes(x=carac_1, y=carac_2))+
  geom_tile(aes(fill=p), color="#8eaad1", alpha=.9)+
  geom_tile(data = ~ filter(., border == 1), aes(fill = p), color = "#8eaad1", size = 1.5) +
  geom_text(aes(label=percent(p, .1)), color= "white", fontface="bold")+
  scale_fill_gradient(low=paleta[5], high=paleta[1])+
  guides(fill="none")
  
ggsave(gg, filename=paste0(salidas, "interacciondosfallos",".jpeg"), width=5)


# GUSTOS EN POLÍTICA

gg<-df2 %>% 
  mutate(politica_preferida= case_when(orden_pref_criterios_promo==1 ~ "Criterios promoción", 
                                       orden_pref_formacion_prof==1 ~ "Formación profesores", 
                                       orden_pref_refuerzo==1 ~ "Refuerzo")) %>% 
  filter(!is.na(politica_preferida)) %>% 
  group_by(politica_preferida) %>% 
  summarise(p=mean(repite), 
            nn=n()) %>% 
  mutate(se= sqrt(p*(1-p)/nn), 
         conf_low=p-1.96*se, 
         conf_high=p+1.96*se) %>% 
  ggplot(aes(politica_preferida, p))+
  geom_col(fill= paleta[3],alpha=.9)+
  geom_errorbar(aes(ymin=conf_low, ymax=conf_high), width=.2, color=paleta[1])+
  geom_text(aes(y=p/2,label=percent(p, .1)), color="grey90", fontface="bold")+
  scale_y_continuous(labels=label_percent())

ggsave(gg, filename=paste0(salidas, "politica_preferida",".jpeg"), width=5)

## GUSTOS EN POLÍTICA MÁS UN FALLO

gg<-df2 %>% 
  filter(suma <=1) %>%
  mutate(alumno= case_when(alumno=="alumno_1_2" ~ "carencias",
                           alumno=="alumno_2_3"~ "expulsion",
                           alumno=="alumno_3_1" ~"extranjero",
                           alumno=="alumno_5_1" ~"niño",
                           alumno=="alumno_5_2" ~ "suspensos",
                           alumno=="alumno_5_3" ~ "absentista",
                           alumno=="alumno_6_1" ~"extranjero",
                           alumno=="alumno_8_1" ~ "control")) %>% 
  mutate(politica_preferida= case_when(orden_pref_criterios_promo==1 ~ "Criterios promoción", 
                                       orden_pref_formacion_prof==1 ~ "Formación profesores", 
                                       orden_pref_refuerzo==1 ~ "Refuerzo")) %>% 
  filter(!is.na(politica_preferida)) %>% 
  group_by(politica_preferida, alumno ) %>% 
  summarise(p=mean(repite),
            nn=n()
  ) %>% 
  mutate(control=factor(ifelse(alumno=="control",1,0)),
         alumno=factor(alumno, levels=c("control", "niño", "extranjero", "absentista", 
                                        "expulsion", "carencias", "suspensos"))) %>% 
  ggplot(aes(alumno, p, fill=politica_preferida))+
  geom_col(alpha=.85, position="dodge", aes(color=control), size=.6)+
  geom_text(aes(y=p/2,label=percent(p, .1)), 
            position = position_dodge(width=.9), size=2.1,color="grey90", fontface="bold")+
  scale_y_continuous(labels = label_percent())+
  scale_fill_manual(values=c(paleta[4], paleta[3], paleta[2]), name="Política preferida")+
  scale_color_manual(values = c("grey", paleta[1]))+
  guides(color="none")+
  theme(legend.position = c(.15,.8), 
        legend.title = element_text() )



ggsave(gg, filename=paste0(salidas, "penalizacion1fallo_gustospolitica",".jpeg"), width=7)




## RANDOM FOREST

df2$repite <- factor(df2$repite)

# Seleccionamos solo las 6 características binarias y la variable repite
# No incluimos 'id' porque queremos que el modelo solo use las características
# Ajustamos un modelo randomForest
set.seed(123)
modelo_rf <- randomForest(repite ~ niño + extranjero + carencias + suspensos + absentista + expulsion,
                          data = df2,
                          importance = TRUE,   # para ver importancia de variables
                          ntree = 500)         # número de árboles


# Convertirlo en un data frame ordenado
df_importancia <- as.data.frame(importance(modelo_rf, type=1)) %>%
  tibble::rownames_to_column("variable") %>%
  arrange(desc(MeanDecreaseAccuracy)) %>% 
  transmute(variable=factor(variable, levels=c("niño", "extranjero", "absentista", 
                                               "expulsion", "carencias", "suspensos")),
            coeficiente=MeanDecreaseAccuracy)

df_importancia %>% 
  ggplot(aes(coeficiente, variable))+
  geom_col(alpha=.85, fill=paleta[3])

gg<-df_importancia %>% 
ggplot(aes(area = coeficiente, fill = variable, label = variable)) +
  geom_treemap() +
  geom_treemap_text(color = "white", place = "centre") +
  scale_fill_manual(values = rev(c("#537d90", "#002059",   "lightblue","#69d3e3","#00b89f",   "#a29cb8")))+
  guides(fill="none")

ggsave(gg, filename=paste0(salidas, "random_forest",".jpeg"), width=5)

# ## RANDOM FOREST DE TIEMPO
# 
# df_repite1 <- df2[df2$repite == 1, ]
# 
# # Creamos el random forest
# modelo_tiempo <- randomForest(
#   tiempo ~ niño + extranjero + carencias + suspensos + absentista + expulsion,
#   data = df_repite1,
#   importance = TRUE,
#   ntree = 500
# )
# 
# # Importancia de las variables
# importancia <- importance(modelo_tiempo, type = 1) # %IncMSE = MeanDecreaseAccuracy
# df_importancia1 <- data.frame(
#   variable = rownames(importancia),
#   coeficiente = importancia[,1]
# )
# 
# 
# df_importancia1 %>% 
#   ggplot(aes(area = coeficiente, fill = variable, label = variable)) + 
#   geom_treemap() +
#   geom_treemap_text(color = "white", place = "centre") +
#   scale_fill_manual(values = rev(c("#002059", "#537d90", "#69d3e3", "lightblue","#00b89f",   "#a29cb8")))+
#   guides(fill="none")

df5<-df2 %>% 
  filter(repite==1) %>% 
  mutate(
    niño=factor(niño), 
    extranjero=factor(extranjero), 
    expulsion=factor(expulsion), 
    suspensos=factor(suspensos), 
    carencias=factor(carencias), 
    absentista=factor(absentista)
  )

summary(lm(data=df5, formula=tiempo ~ niño + extranjero+ expulsion+ suspensos+ carencias+ absentista))


###########################################################################################
# Comprobación alumnos que faltan: 

df2 %>% 
  mutate(alumno2=paste0(niño, extranjero, suspensos, carencias, absentista, expulsion)) %>% 
  group_by(alumno2) %>% 
  mutate(ndist=n_distinct(alumno)) %>% 
  filter(ndist>1) %>% 
  group_by(alumno, alumno2) %>% 
  summarise(ratio=sum(repite)/n()) %>% 
  ggplot(aes(alumno2, ratio, fill=alumno2, group=alumno)) +
  geom_col(position="dodge", color="black")+
  geom_text(aes(y=ratio/2,label=paste(alumno, "\n",scales::percent(ratio, .1))), 
            position = position_dodge(width = .9))+
  scale_y_continuous(labels=scales::label_percent())

vars_binarias <- c("niño", "extranjero", "suspensos", "carencias", "absentista", "expulsion")
dfalumno<-df2 %>% group_by(alumno) %>% slice(1) %>% select(alumno, all_of(vars_binarias))

# Crear todas las combinaciones posibles de 0 y 1 para las 6 variables
combinaciones_posibles <- expand.grid(rep(list(0:1), length(vars_binarias)))
colnames(combinaciones_posibles) <- vars_binarias

# Obtener las combinaciones únicas en tu dataframe
combinaciones_presentes <- unique(dfalumno[vars_binarias])

# Verificar si están todas las combinaciones
total_posibles <- nrow(combinaciones_posibles)
total_presentes <- nrow(combinaciones_presentes)

faltan <- anti_join(combinaciones_posibles, combinaciones_presentes, by = vars_binarias)
