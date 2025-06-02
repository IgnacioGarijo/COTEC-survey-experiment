library(mapSpain)
library(sf)
library(tidyverse)
library(readxl)
library(scales)
library(randomForest)
library(arrow)
library(treemapify)
library(knitr)
library(kableExtra)

wd<- "C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Phd/RCT/Datos y codigo/"
github<- "C:/Users/ignac/OneDrive/Documentos/GitHub/COTEC-survey-experiment/"
salidas<- paste0(github, "graficos/")
setwd(wd)

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


df1<-df[,c("id", "orden_pref_criterios_promo", "orden_pref_formacion_prof", "orden_pref_refuerzo", "politica", "treatment", "primaria","female", "cp", "edad", "titularidad", namelist_ps)]

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
         `sit. compleja`=ifelse(alumno %in% vector_extranjero, 1, 0), 
         suspensos=ifelse(alumno%in% vector_suspensos, 1, 0), 
         `competencias(-)`=ifelse(alumno %in% vector_carencias, 1, 0), 
         absentista=ifelse(alumno %in% vector_absentista, 1, 0), 
         disruptivo=ifelse(alumno %in% vector_expulsion, 1, 0), 
         suma=niño+`sit. compleja`+suspensos+`competencias(-)`+absentista+disruptivo)

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
  geom_errorbar(aes(ymax=conf_high, ymin=conf_low), width=.2, color="grey30")+
  scale_x_continuous(breaks = seq(0,8,1))+
  scale_y_continuous(labels = label_percent())

ggsave(gg, filename=paste0(salidas, "monotonia",".jpeg"), width=5)


# 1 FALLO 


alumnos_rojos <- c("Control", "Niño", "Sit. compleja")

gg <- df2 %>%
  filter(suma <= 1) %>%
  mutate(
    alumno = case_when(
      alumno == "alumno_1_2" ~ "Competencias (-)",
      alumno == "alumno_2_3" ~ "Disruptivo",
      alumno == "alumno_3_1" ~ "Sit. compleja",
      alumno == "alumno_5_1" ~ "Niño",
      alumno == "alumno_5_2" ~ "Suspensos",
      alumno == "alumno_5_3" ~ "Absentista",
      alumno == "alumno_6_1" ~ "Sit. compleja",
      alumno == "alumno_8_1" ~ "Control"
    )
  ) %>%
  group_by(alumno) %>%
  summarise(
    p = mean(repite),
    nn = n()
  ) %>%
  mutate(
    se = sqrt(p * (1 - p) / nn),
    conf_low = p - 1.96 * se,
    conf_high = p + 1.96 * se,
    control = factor(ifelse(alumno == "Control", 1, 0)),
    alumno = factor(alumno, levels = c("Control", "Niño", "Sit. compleja", "Absentista", "Disruptivo", "Competencias (-)", "Suspensos")),
    errorbar_color = ifelse(alumno %in% alumnos_rojos, "#bb002f", "grey30")
  ) %>%
  ggplot(aes(alumno, p, fill = control)) +
  geom_col(alpha = .85) +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high, color = errorbar_color),
    width = .2
  ) +
  geom_text(
    aes(label = percent(p, .1)),
    position = position_stack(vjust = .5),
    color = "grey85",
    fontface = "bold"
  ) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c(paleta[3], paleta[4])) +
  scale_color_identity() +  # Esto usa los colores tal cual están en la variable
  guides(fill = "none")


ggsave(gg, filename=paste0(salidas, "unfallo",".jpeg"), width=7)

# GEOM_TILE INTERACCIÓN DOS FALLOS

df3<-df2 %>%   
  filter(suma %in% c(1,2)) %>% 
  mutate(
  carac_1 = case_when(
    niño == 1 ~ "Niño",
    `sit. compleja` == 1 ~ "Sit. Compleja",
    suspensos == 1 ~ "Suspensos",
    `competencias(-)` == 1 ~ "Competencias (-)",
    absentista == 1 ~ "Absentista",
    disruptivo == 1 ~ "Disruptivo"
  ),
  carac_2 = case_when(
    niño == 1 & carac_1 != "Niño" ~ "Niño",
    `sit. compleja` == 1 & carac_1 != "Sit. Compleja" ~ "Sit. Compleja",
    suspensos == 1 & carac_1 != "Suspensos" ~ "Suspensos",
    `competencias(-)` == 1 & carac_1 != "Competencias (-)" ~ "Competencias (-)",
    absentista == 1 & carac_1 != "Absentista" ~ "Absentista",
    disruptivo == 1 & carac_1 != "Disruptivo" ~ "Disruptivo"
  ),
  carac_2=ifelse(is.na(carac_2), carac_1, carac_2),
  carac_1= factor(carac_1, levels=c("Niño", "Sit. Compleja", "Absentista", "Disruptivo", "Competencias (-)", "Suspensos")),
  carac_2= factor(carac_2, levels=c("Niño", "Sit. Compleja", "Absentista", "Disruptivo", "Competencias (-)", "Suspensos"))

) %>% 
  group_by(carac_1, carac_2) %>% 
  summarise(p=mean(repite)) 

gg<-df3%>%
  bind_rows(df3 %>% rename(carac_1 = carac_2, carac_2 = carac_1)) %>%
  distinct(carac_1, carac_2, .keep_all = TRUE) %>%
  filter((carac_1=="Niño" & carac_2=="Niño") | 
           (carac_1=="Sit. Compleja" & carac_2 %in% c("Niño", "Sit. Compleja")) |
           (carac_1== "Absentista" & carac_2 %in% c("Niño", "Sit. Compleja", "Absentista")) |
           (carac_1=="Disruptivo" & carac_2 %in% c("Niño", "Sit. Compleja", "Absentista", "Disruptivo"))|
           (carac_1=="Competencias (-)" & carac_2 %in% c("Niño", "Sit. Compleja", "Absentista", "Disruptivo", "Competencias (-)"))|
           carac_1== "Suspensos" & carac_2 %in% c("Niño", "Sit. Compleja", "Absentista", "Disruptivo", "Competencias (-)", "Suspensos")) %>% 
  mutate(border=factor(ifelse(carac_1==carac_2, 1, 0))) %>% 
  ggplot(aes(x=carac_1, y=carac_2))+
  geom_tile(aes(fill=p), color="#8eaad1", alpha=.9)+
  geom_tile(data = ~ filter(., border == 1), aes(fill = p), color = "#8eaad1", size = 1.5) +
  geom_text(aes(label=percent(p, .1)), color= "white", fontface="bold")+
  scale_fill_gradient(low=paleta[5], high=paleta[1])+
  guides(fill="none")
  
ggsave(gg, filename=paste0(salidas, "interacciondosfallos",".jpeg"), width=7)


#### GUSTOS EN POLÍTICA

gg<-df2 %>% 
  mutate(politica_preferida= case_when(orden_pref_criterios_promo==1 ~ "Criterios\nprom.", 
                                       orden_pref_formacion_prof==1 ~ "Formación\nprofesores", 
                                       orden_pref_refuerzo==1 ~ "Refuerzo")) %>% 
  drop_na(politica_preferida) %>% 
  group_by(politica_preferida) %>% 
  summarise(value=n()) %>% 
  ungroup() %>% 
  mutate(ratio=value/sum(value)) %>% 
  mutate(label = paste0(politica_preferida, "\n", percent(ratio, accuracy = 1))) %>% 
  ggplot(aes(x = "", y = ratio, fill = politica_preferida)) +
  geom_col(width = 1, color = "white") +  
  coord_polar(theta = "y") +               
  theme_void() +                           
  labs(fill = "Política preferida") +      
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 3) +  # Ajusta el tamaño según lo necesites
  scale_fill_manual(values = c(paleta[1], paleta[3], paleta[4]))+
  guides(fill="none")

ggsave(gg, filename=paste0(salidas, "piechart.jpeg"), width=6)


# GUSTOS EN POLÍTICA DECISIONES

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
  ggplot(aes(fct_reorder(politica_preferida, p), p))+
  geom_col(fill= paleta[3],alpha=.9)+
  geom_errorbar(aes(ymin=conf_low, ymax=conf_high), width=.2, color="grey30")+
  geom_text(aes(y=p/2,label=percent(p, .1)), color="grey90", fontface="bold")+
  scale_y_continuous(labels=label_percent())+
  ylab("Tasa de repetición")+
  theme(axis.title.y = element_text())

ggsave(gg, filename=paste0(salidas, "politica_preferida",".jpeg"), width=5)

## GUSTOS EN POLÍTICA MÁS UN FALLO

gg<-df2 %>% 
  filter(suma <=1) %>%
  mutate(alumno= case_when(alumno=="alumno_1_2" ~ "Competencias (-)",
                           alumno=="alumno_2_3"~ "Disruptivo",
                           alumno=="alumno_3_1" ~"Sit. compleja",
                           alumno=="alumno_5_1" ~"Niño",
                           alumno=="alumno_5_2" ~ "Suspensos",
                           alumno=="alumno_5_3" ~ "Absentista",
                           alumno=="alumno_6_1" ~"Sit. compleja",
                           alumno=="alumno_8_1" ~ "Control")) %>% 
  mutate(politica_preferida= case_when(orden_pref_criterios_promo==1 ~ "Criterios promoción", 
                                       orden_pref_formacion_prof==1 ~ "Formación profesores", 
                                       orden_pref_refuerzo==1 ~ "Refuerzo")) %>% 
  filter(!is.na(politica_preferida)) %>% 
  group_by(politica_preferida, alumno ) %>% 
  summarise(p=mean(repite),
            nn=n()
  ) %>% 
  mutate(control=factor(ifelse(alumno=="Control",1,0)),
         alumno=factor(alumno, levels=c("Control", "Niño", "Sit. compleja", "Absentista", 
                                        "Disruptivo", "Competencias (-)", "Suspensos"))) %>% 
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



# Seleccionamos solo las 6 características binarias y la variable repite
# No incluimos 'id' porque queremos que el modelo solo use las características
# Ajustamos un modelo randomForest
dfrf<- df2 %>% 
  mutate(sitcompleja= `sit. compleja`, 
         competencias= `competencias(-)`, 
         repite=factor(repite))

set.seed(123)
modelo_rf <- randomForest(repite ~ niño +  sitcompleja + competencias + suspensos + absentista + disruptivo,
                          data = dfrf,
                          importance = TRUE,   # para ver importancia de variables
                          ntree = 500)         # número de árboles


# Convertirlo en un data frame ordenado
df_importancia <- as.data.frame(importance(modelo_rf, type=1)) %>%
  tibble::rownames_to_column("variable") %>%
  arrange(desc(MeanDecreaseAccuracy)) %>% 
  transmute(variable= ifelse(variable=="sitcompleja", "Sit. Compleja",
                             ifelse(variable== "competencias", "Competencias (-)", variable )), 
            variable=factor(variable, levels=c("niño", "Sit. Compleja", "absentista", 
                                               "disruptivo", "Competencias (-)", "suspensos")),
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
    `sit. compleja`=factor(`sit. compleja`), 
    disruptivo=factor(disruptivo), 
    suspensos=factor(suspensos), 
    `competencias(-)`=factor(`competencias(-)`), 
    absentista=factor(absentista)
  )

modelolm<-lm(data=df5, formula=tiempo ~ niño + `sit. compleja`+ disruptivo+ suspensos+ `competencias(-)`+ absentista)

confint_modelo<- confint(modelolm)

dfmodelo<-data.frame(variable= names(modelolm$coefficients), 
           coeficiente= as.numeric(modelolm$coefficients), 
           low_ci= confint_modelo[,1], 
           high_ci= confint_modelo[,2]) %>% 
  mutate(variable= ifelse(variable %in% c("suspensos1", "niño1", "disruptivo1", "absentista1"),
                          substr(variable, 1, nchar(variable)-1), 
                          ifelse(variable %in% c("`sit. compleja`1", "`competencias(-)`1"), substr(variable,2, nchar(variable)-2), variable)))

gg<-dfmodelo %>% 
  mutate(colorin=ifelse(low_ci<0 & high_ci>0, "no", "si")) %>% 
  ggplot(aes(coeficiente,fct_reorder(variable, -coeficiente), color=colorin))+
  geom_point()+
  geom_errorbar(aes(xmin=low_ci, xmax=high_ci), width=.2)+
  geom_vline(xintercept=0)+
  scale_color_manual(values = c("no" ="#bb002f", "si"=paleta[3]))+
  guides(color="none")


ggsave(gg, filename=paste0(salidas, "coefplot.jpeg"), width=5)
## Política asignada 

df2 %>% 
  mutate(repite=repite) %>% 
  group_by(politica=case_when(politica==2 ~ "Criterios promoción", 
                              politica==3 ~ "Formación profesores", 
                              politica==1 ~ "Refuerzo"),
           politica_preferida= case_when(orden_pref_criterios_promo==1 ~ "Criterios promoción", 
                                                   orden_pref_formacion_prof==1 ~ "Formación profesores", 
                                                   orden_pref_refuerzo==1 ~ "Refuerzo")) %>% 
  filter(!is.na(politica), !is.na(politica_preferida)) %>% 
  summarise(value=mean(repite)) %>% 
  ggplot(aes(politica, politica_preferida, fill=value, label=percent(value, .1)))+
  geom_tile()+
  geom_text(color="white")+
  scale_fill_gradient(low=paleta[5], high=paleta[1])+
  theme(axis.title = element_text())+
  xlab("Política asignada")+
  ylab("Política pref")+
  guides(fill="none")

# 1refuerzo
   # 2 criterios
  
  df2 %>% 
    mutate(treatment=as.numeric(treatment), 
           treatment=case_when(treatment <2 ~"control", 
                               treatment %in% c(2:4) ~"exogeno", 
                               treatment %in% c(5:7) ~ "endogeno", 
                               treatment %in% c(8:10) ~ "awareness", 
                               TRUE ~ NA_character_)) %>%
    filter(!is.na(treatment)) %>% 
    group_by(treatment) %>% 
    summarise(mean(repite))
  
  
  # GEOM_TILE INTERACCIÓN DOS FALLOS PRIMARIA Y SECUNDARIA
  
  for (x in c(1,0)){
    if(x==1){titulo="primaria"} else {titulo="secundaria"}
    
  df3<-df2 %>%   
    filter(suma %in% c(1,2), primaria==x) %>% 
    mutate(
      carac_1 = case_when(
        niño == 1 ~ "Niño",
        `sit. compleja` == 1 ~ "Sit. Compleja",
        suspensos == 1 ~ "Suspensos",
        `competencias(-)` == 1 ~ "Competencias (-)",
        absentista == 1 ~ "Absentista",
        disruptivo == 1 ~ "Disruptivo"
      ),
      carac_2 = case_when(
        niño == 1 & carac_1 != "Niño" ~ "Niño",
        `sit. compleja` == 1 & carac_1 != "Sit. Compleja" ~ "Sit. Compleja",
        suspensos == 1 & carac_1 != "Suspensos" ~ "Suspensos",
        `competencias(-)` == 1 & carac_1 != "Competencias (-)" ~ "Competencias (-)",
        absentista == 1 & carac_1 != "Absentista" ~ "Absentista",
        disruptivo == 1 & carac_1 != "Disruptivo" ~ "Disruptivo"
      ),
      carac_2=ifelse(is.na(carac_2), carac_1, carac_2),
      carac_1= factor(carac_1, levels=c("Niño", "Sit. Compleja", "Absentista", "Disruptivo", "Competencias (-)", "Suspensos")),
      carac_2= factor(carac_2, levels=c("Niño", "Sit. Compleja", "Absentista", "Disruptivo", "Competencias (-)", "Suspensos"))
      
    ) %>% 
    group_by(carac_1, carac_2) %>% 
    summarise(p=mean(repite)) 
  
  gg<-df3%>%
    bind_rows(df3 %>% rename(carac_1 = carac_2, carac_2 = carac_1)) %>%
    distinct(carac_1, carac_2, .keep_all = TRUE) %>%
    filter((carac_1=="Niño" & carac_2=="Niño") | 
             (carac_1=="Sit. Compleja" & carac_2 %in% c("Niño", "Sit. Compleja")) |
             (carac_1== "Absentista" & carac_2 %in% c("Niño", "Sit. Compleja", "Absentista")) |
             (carac_1=="Disruptivo" & carac_2 %in% c("Niño", "Sit. Compleja", "Absentista", "Disruptivo"))|
             (carac_1=="Competencias (-)" & carac_2 %in% c("Niño", "Sit. Compleja", "Absentista", "Disruptivo", "Competencias (-)"))|
             carac_1== "Suspensos" & carac_2 %in% c("Niño", "Sit. Compleja", "Absentista", "Disruptivo", "Competencias (-)", "Suspensos")) %>% 
    mutate(border=factor(ifelse(carac_1==carac_2, 1, 0))) %>% 
    ggplot(aes(x=carac_1, y=carac_2))+
    geom_tile(aes(fill=p), color="#8eaad1", alpha=.9)+
    geom_tile(data = ~ filter(., border == 1), aes(fill = p), color = "#8eaad1", size = 1.5) +
    geom_text(aes(label=percent(p, .1)), color= "white", fontface="bold")+
    scale_fill_gradient(low=paleta[5], high=paleta[1])+
    guides(fill="none")
  
  ggsave(gg, filename=paste0(salidas, "interacciondosfallos",titulo,".jpeg"), width=7)
  }
  
  ## 1 FALLO PRIMARIA/SECUNDARIA
  
  
  for (x in c(0,1)) {
    alumnos_rojos <- c("Control", "Niño", "Sit. compleja")
    
    if(x==1){titulo="primaria"
    alumnos_rojos <- c(alumnos_rojos, "Absentista" )
    } else {titulo="secundaria"
    alumnos_rojos <- c(alumnos_rojos, "Disruptivo" )}
    
  gg <- df2 %>%
    filter(suma <= 1, primaria==x) %>%
    mutate(
      alumno = case_when(
        alumno == "alumno_1_2" ~ "Competencias (-)",
        alumno == "alumno_2_3" ~ "Disruptivo",
        alumno == "alumno_3_1" ~ "Sit. compleja",
        alumno == "alumno_5_1" ~ "Niño",
        alumno == "alumno_5_2" ~ "Suspensos",
        alumno == "alumno_5_3" ~ "Absentista",
        alumno == "alumno_6_1" ~ "Sit. compleja",
        alumno == "alumno_8_1" ~ "Control"
      )
    ) %>%
    group_by(alumno) %>%
    summarise(
      p = mean(repite),
      nn = n()
    ) %>%
    mutate(
      se = sqrt(p * (1 - p) / nn),
      conf_low = p - 1.96 * se,
      conf_high = p + 1.96 * se,
      control = factor(ifelse(alumno == "Control", 1, 0)),
      alumno = factor(alumno, levels = c("Control", "Niño", "Sit. compleja", "Absentista", "Disruptivo", "Competencias (-)", "Suspensos")),
      errorbar_color = ifelse(alumno %in% alumnos_rojos, "#bb002f", "grey30")
    ) %>%
    ggplot(aes(alumno, p, fill = control)) +
    geom_col(alpha = .85) +
    geom_errorbar(
      aes(ymin = conf_low, ymax = conf_high, color = errorbar_color),
      width = .2
    ) +
    geom_text(
      aes(label = percent(p, .1)),
      position = position_stack(vjust = .5),
      color = "grey85",
      fontface = "bold"
    ) +
    scale_y_continuous(labels = label_percent()) +
    scale_fill_manual(values = c(paleta[3], paleta[4])) +
    scale_color_identity() +  # Esto usa los colores tal cual están en la variable
    guides(fill = "none")
  
  
  ggsave(gg, filename=paste0(salidas, "unfallo", titulo,".jpeg"), width=7)
  
  }
  
  ### PROFESORES
  
 mujeres<- sum(df$female==1, na.rm=T)/sum(!is.na(df$female))
 hombres<- sum(df$female==0, na.rm=T)/sum(!is.na(df$female))
 
 municipios<-n_distinct(df$cp) # total 919
 
 prof_mun<- df %>% group_by(cp) %>% summarise(valor=n()) %>% ungroup() %>% summarise(mean(valor))
 prof_mun<- prof_mun$`mean(valor)`
 
 edad<-mean(df$edad, na.rm=T)
 
 primaria<- sum(df$primaria==1, na.rm=T)/sum(!is.na(df$primaria))
 secundaria<- sum(df$primaria==0, na.rm=T)/sum(!is.na(df$primaria))
 
 publico<- sum(df$titularidad=="Pública" , na.rm=T)/sum(!is.na(df$titularidad))
 privada<- sum(df$titularidad=="Privada", na.rm=T)/sum(!is.na(df$titularidad))
 concertada<- sum(df$titularidad=="Concertada", na.rm=T)/sum(!is.na(df$titularidad))
 
 tabla_resumen <- tibble(
   Métrica = c(
     "Porcentaje mujeres", 
     "Porcentaje hombres", 
     "Total municipios", 
     "Profesores por municipio (media)", 
     "Edad media", 
     "Porcentaje primaria", 
     "Porcentaje secundaria", 
     "Porcentaje titularidad pública", 
     "Porcentaje titularidad privada", 
     "Porcentaje titularidad concertada"
   ),
   Valor = c(
     mujeres*100, 
     hombres*100, 
     municipios, 
     prof_mun, 
     edad, 
     primaria*100, 
     secundaria*100, 
     publico*100, 
     privada*100, 
     concertada*100
   )
 )
 

 # Guardarla como archivo HTML
 saveRDS(tabla_resumen, paste0(salidas,"tabla_resumen.rds"))
 
###########################################################################################
# Comprobación alumnos que faltan: 

df2 %>% 
  mutate(alumno2=paste0(niño, `sit. compleja`, suspensos, `competencias(-)`, absentista, disruptivo)) %>% 
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

vars_binarias <- c("Niño", "Sit. Compleja", "Suspensos", "Competencias (-)", "Absentista", "Disruptivo")
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
