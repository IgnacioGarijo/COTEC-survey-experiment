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

df3<-inner_join(dfad, dfdm)
df3<-inner_join(df3, dfnrep)

GGally::ggpairs(df3[, c("ha","hb","hc")])

dfanalisis<- left_join(df3, df)

dfanalisis %>% 
  pivot_longer(cols=c("ha", "hb", "hc")) %>% 
  group_by(edad, name) %>% 
  summarise(value=mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  ggplot(aes(edad, value, color=name))+
  geom_point()


#### CHARACTERIZING THE TEACHERS ####
#####################################

# nivel, experiencia, antiguedad3, female, edad, titularidad, sitlabpub, grupos_docencia