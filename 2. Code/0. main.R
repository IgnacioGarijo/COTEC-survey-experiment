#====================#
#### 1. LIBRARIES #### 
#====================#

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
library(pROC)
library(ranger)
library(modelsummary)
library(cowplot)
library(broom)
library(shapviz)
library(ggbeeswarm)
library(iml)
library(fastshap)
library(GGally)
library(patchwork)

#====================#
#### 2. PATHS #### 
#====================#

wd<-here::here()
setwd(wd)

output<- file.path(wd, "3. Output/")
graficos<- file.path(output, "figures/")
tables<- file.path(output, "tables/")
code<- file.path(wd, "2. Code")
data<- file.path(wd, "1. Data")
rawdata<- file.path(data, "raw data")
processed_data<-file.path(data, "processed data")

#------------------#
##### 2.1 DATA #####
#------------------#

dataname<-file.path(rawdata,"Cotec_+SaveTheChildren_29+de+abril+de+2025_13.51.xlsx")
dataparquet<-file.path(rawdata, "df_named.parquet")
dfmapa<-esp_get_ccaa(moveCAN = T)
dfcentrocp<- read_excel(file.path(rawdata,"listado_centros.xls"))
cleandata<- file.path(processed_data, "cleandata.parquet")


#============================#
#### 3. THEME AND PALETTE #### 
#============================#

theme_set(theme_minimal()+
            theme(axis.text = element_text(face="bold", color="#404040"),
                  axis.title=element_blank(),
                  legend.title=element_blank(), 
                  panel.grid.major.y = element_line(color="grey80"),
                  panel.grid.major.x = element_blank(), 
                  panel.grid.minor = element_blank(), 
                  axis.line = element_line(color="grey50")))

paleta<- c("#002059","#011552","#537d90","#a29cb8", "#69d3e3", "#a47dab", "#00b89f")
paleta3<- c("#94e1b4", "#25998c", "#033854")

#============================#
#### 4. VECTORS FOR CARDS #### 
#============================#

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



#====================#
#### 5. FUNCTIONS #### 
#====================#


make_quartile_hists <- function(data, vars) {
  
  out <- lapply(vars, function(v) {
    
    qs <- quantile(data[[v]], probs = c(0.333, .666), na.rm = TRUE)
    xmax <- max(data[[v]], na.rm = TRUE)
    
    # crear variable categórica según cuantil
    df_tmp <- data %>%
      mutate(
        quart_cat = cut(
          .data[[v]],
          breaks = c(-Inf, qs[1], qs[2], Inf),
          labels = c("low", "mid", "high"),
          include.lowest = TRUE
        )
      )
    
    df_tmp %>%
      ggplot(aes(x = .data[[v]], fill = quart_cat)) +
      geom_histogram() +
      geom_vline(xintercept = qs, linetype = "dashed") +
      ggtitle(paste0(v)) +
      scale_x_continuous(
        breaks = if (xmax <= 10) c(0:10) else seq(0, 100, by = 10)
      ) +
      scale_fill_manual(values = paleta3)+
      guides(fill="none")
  })
  
  names(out) <- vars
  out
}



ntile3_label <- function(x) {
  dplyr::case_match(
    dplyr::ntile(x, 3),
    1 ~ "low",
    2 ~ "medium",
    3 ~ "high"
  ) |> factor(levels = c("low", "medium", "high"))
}

f<- function(object, newdata) {
  predict(modelo_rf, data=newdata)$predictions
}
