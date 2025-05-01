library(mapSpain)
library(sf)
library(tidyverse)
library(readxl)
library(arrow)

wd<- "C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Phd/RCT/Datos y codigo/"
setwd(wd)
dataname<-paste0(wd,"Cotec_+SaveTheChildren_29+de+abril+de+2025_13.51.xlsx")
dataparquet<-paste0(wd, "df_named.parquet")
dfmapa<-esp_get_ccaa(moveCAN = T)
dfcentrocp<- read_excel("listado_centros.xls")


if (!file.exists(dataparquet)){
# Leer las primeras dos filas del archivo (si solo interesa obtener la cabecera y quizás otra fila)
full_data <- read_excel(dataname, n_max = 1)
# Leer el resto de los datos a partir de la segunda fila (omitimos la cabecera)
data <- read_excel(dataname, skip = 2, col_names = F)
# Asignar los nombres de columnas utilizando la cabecera obtenida
colnames(data) <- colnames(full_data)

write_parquet(data, sink = paste0(wd, "df_named.parquet"))
}

df<-read_parquet(paste0(wd, "df_named.parquet"))


df1<-df %>% 
  transmute(
    start=StartDate,
    end=EndDate,
    #BORRO STATUS
    ip=IPAddress, 
    progress=Progress,
    duration=`Duration (in seconds)`, 
    finished=Finished,
    registry_date=RecordedDate, 
    id=ResponseId, 
    # BORRO APELLIDOS, NOMBRES, EMAIL Y REFERENCE
    latitude=LocationLatitude, 
    longitude=LocationLongitude,
    #BORRO DISTRIBUTION_CHANNEL, 
    #BORRO USERLANGUAGE,
    captcha=Q_RecaptchaScore, 
    id_duplicate= Q_RelevantIDDuplicate, 
    id_duplicate_score= Q_RelevantIDDuplicateScore,
    id_fraud_score= Q_RelevantIDFraudScore,
    last_start_date= Q_RelevantIDLastStartDate,
    browser=info_Browser, 
    version=info_Version, 
    os=`info_Operating System`,
    resolution= info_Resolution, 
    p0_fc=`time_T0.1_First Click`, 
    p0_lc=`time_T0.1_Last Click`, 
    p0_s=`time_T0.1_Page Submit`,
    p0_nc= `time_T0.1_Click Count`,
    intro= Intro,
    browser_182= Q182_Browser, 
    version_182=Q182_Version, 
    os_182=`Q182_Operating System`,
    resolution_182=Q182_Resolution,
    primaria_fc=`time_1aPrimaria_First Click`,
    primaria_lc=`time_1aPrimaria_Last Click`,
    primaria_s= `time_1aPrimaria_Page Submit`, 
    primaria_nc= `time_1aPrimaria_Click Count`,
    primaria= ifelse(`1Primaria/Secundaria`=="E. Primaria", 1, 0),
    infantil= `1a Primaria_1`,
    primaria_1 = `1a Primaria_2`,
    primaria_2 = `1a Primaria_3`,
    primaria_3 = `1a Primaria_4`,
    primaria_4 = `1a Primaria_5`,
    primaria_5 = `1a Primaria_6`,
    primaria_6 = `1a Primaria_7`,
    primaria_otro = `1a Primaria_8`,
    primaria_txt = `1a Primaria_8_TEXT`,
    
    secundaria_fc = `time_1aSecundaria_First Click`,
    secundaria_lc = `time_1aSecundaria_Last Click`,
    secundaria_ps = `time_1aSecundaria_Page Submit`,
    secundaria_cc = `time_1aSecundaria_Click Count`,
    secundaria_1 = `1aSecundaria_1`,
    secundaria_2 = `1aSecundaria_2`,
    secundaria_3 = `1aSecundaria_4`,
    secundaria_4 = `1aSecundaria_5`,
    secundaria_bach = `1aSecundaria_6`,
    secundaria_ciclo = `1aSecundaria_7`,
    secundaria_otro = `1aSecundaria_8`,
    secundaria_txt = `1aSecundaria_8_TEXT`,
    
    secundaria_dept_fc = `time_1bSecundaria_First Click`,
    secundaria_dept_lc = `time_1bSecundaria_Last Click`,
    secundaria_dept_ps = `time_1bSecundaria_Page Submit`,
    secundaria2_dept_cc = `time_1bSecundaria_Click Count`,
    secundaria2_dept = `1bSecundaria`,
    secundaria2_dept_txt = `1bSecundaria_18_TEXT`,
    
    experiencia_fc = `time_2_First Click`,
    experiencia_lc = `time_2_Last Click`,
    experiencia_ps = `time_2_Page Submit`,
    experiencia_cc = `time_2_Click Count`,
    experiencia = ifelse(`2.Antiguedad`>60, NA, `2.Antiguedad`),
    
    antiguedad3_fc = `time_3_First Click`,
    antiguedad3_lc = `time_3_Last Click`,
    antiguedad3_ps = `time_3_Page Submit`,
    antiguedad3_cc = `time_3_Click Count`,
    antiguedad3 = ifelse(`3.Aniguedad en Centr`>=60, NA,`3.Aniguedad en Centr`),
    
    centrosprevios_fc = `time_4_First Click`,
    centrosprevios_lc = `time_4_Last Click`,
    centrosprevios_ps = `time_4_Page Submit`,
    centrosprevios_cc = `time_4_Click Count`,
    centrosprevios = ifelse(`4.CentrosPrevios`>30, NA,`4.CentrosPrevios`) , # Check 30 son demasiados no?
    
    genero_fc = `time_5_First Click`,
    genero_lc = `time_5_Last Click`,
    genero_ps = `time_5_Page Submit`,
    genero_cc = `time_5_Click Count`,
    genero = `5. Género`, # check, habría que dar un valor a cada respuesta pero no sé qué significa cada número
    
    edad_fc = `time_6_First Click`,
    edad_lc = `time_6_Last Click`,
    edad_ps = `time_6_Page Submit`,
    edad_cc = `time_6_Click Count`,
    edad = ifelse(`6. Edad`>80 |`6. Edad`<20, NA, `6. Edad`) , # check Eliminar outliers que no tienen sentido
    
    tutor_fc = `time_7_First Click`,
    tutor_lc = `time_7_Last Click`,
    tutor_ps = `time_7_Page Submit`,
    tutor_cc = `time_7_Click Count`,
    tutor = `7. Tutor`,
    
    tutor_curso_fc = `time_8a1_First Click`,
    tutor_curso_lc = `time_8a1_Last Click`,
    tutor_curso_ps = `time_8a1_Page Submit`,
    tutor_curso_cc = `time_8a1_Click Count`,
    tutor_curso = `8a1Tutores`, # Check convertir a variable tratable
    
    tutor_estudiantes_fc = `time_8a2_First Click`,
    tutor_estudiantes_lc = `time_8a2_Last Click`,
    tutor_estudiantes_ps = `time_8a2_Page Submit`,
    tutor_estudiantes_cc = `time_8a2_Click Count`,
    tutor_estudiantes = ifelse(`8a2Tutores`>50, NA,`8a2Tutores`) ,
    
    tutores_pos_rep_fc = `time_8a3_First Click`,
    tutores_pos_rep_lc = `time_8a3_Last Click`,
    tutores_pos_rep_ps = `time_8a3_Page Submit`,
    tutores_pos_rep_cc = `time_8a3_Click Count`,
    tutores_pos_rep = ifelse(`8a3Tutores`>`8a2Tutores`, NA, `8a3Tutores`),
    
    tutores_rep_fc = `time_8a4_First Click`,
    tutores_rep_lc = `time_8a4_Last Click`,
    tutores_rep_ps = `time_8a4_Page Submit`,
    tutores_rep_cc = `time_8a4_Click Count`,
    tutores_rep = ifelse(`8a4Tutores`>`8a2Tutores`, NA, `8a4Tutores`),
    
    cp_fc = `time_8C.Posta_First Click`,
    cp_lc = `time_8C.Posta_Last Click`,
    cp_ps = `time_8C.Posta_Page Submit`,
    cp_cc = `time_8C.Posta_Click Count`,
    cp = `8 C.Postal`, # check convertir registros de colegios a cp
    
    titularidad_fc = `time_9_First Click`,
    titularidad_lc = `time_9_Last Click`,
    titularidad_ps = `time_9_Page Submit`,
    titularidad_cc = `time_9_Click Count`,
    titularidad = `9.Titularidad`,
    
    sitlabpub_fc = `time_10Publico_First Click`,
    sitlabpub_lc = `time_10Publico_Last Click`,
    sitlabpub_ps = `time_10Publico_Page Submit`,
    sitlabpub_cc = `time_10Publico_Click Count`,
    sitlabpub = `10.Situación Lab.Pub`,
    sitlabpub_txt = `10.Situación Lab.Pub_13_TEXT`,
    
    enseñanzas_centro_fc = `time_11_First Click`,
    enseñanzas_centro_lc = `time_11_Last Click`,
    enseñanzas_centro_ps = `time_11_Page Submit`,
    enseñanzas_centro_cc = `time_11_Click Count`,
    enseñanzas_centro_infantil= `11.EnseñanzasCentro_1`,
    enseñanzas_centro_primaria= `11.EnseñanzasCentro_2`,
    enseñanzas_centro_eso= `11.EnseñanzasCentro_3`,
    enseñanzas_centro_bach= `11.EnseñanzasCentro_4`,
    enseñanzas_centro_fpbasico= `11.EnseñanzasCentro_5`,
    enseñanzas_centro_fpmedio= `11.EnseñanzasCentro_6`,
    enseñanzas_centro_fpsuperior= `11.EnseñanzasCentro_7`,
    enseñanzas_centro_otro= `11.EnseñanzasCentro_8`,
    enseñanzas_centro_txt= `11.EnseñanzasCentro_8_TEXT`,
   
    
    lineas_eso_fc = `time_12A_First Click`,
    lineas_eso_lc = `time_12A_Last Click`,
    lineas_eso_ps = `time_12A_Page Submit`,
    lineas_eso_cc = `time_12A_Click Count`,
    lineas_eso = ifelse(`12.ALíneas ESO`>10, NA,`12.ALíneas ESO`) ,
    
    lineas_primaria_fc = `time_12B_First Click`,
    lineas_primaria_lc = `time_12B_Last Click`,
    lineas_primaria_ps = `time_12B_Page Submit`,
    lineas_primaria_cc = `time_12B_Click Count`,
    lineas_primaria =  ifelse( `12.BLíneas Prim.` >10, NA,`12.BLíneas Prim.`),
    
    grupos_docencia_fc = `time_13_First Click`,
    grupos_docencia_lc = `time_13_Last Click`,
    grupos_docencia_ps = `time_13_Page Submit`,
    grupos_docencia_cc = `time_13_Click Count`,
    grupos_docencia = ifelse(`13. Grupos Clase`>20,NA, `13. Grupos Clase`),
    
    alumnos_grupo_grande_fc = `time_14_First Click`,
    alumnos_grupo_grande_lc = `time_14_Last Click`,
    alumnos_grupo_grande_ps = `time_14_Page Submit`,
    alumnos_grupo_grande_cc = `time_14_Click Count`,
    alumnos_grupo_grande = ifelse(`14.AlumnosClase`>40, NA, `14.AlumnosClase`),
    
    horas_sustitucion_mes_fc = `time_15_First Click`,
    horas_sustitucion_mes_lc = `time_15_Last Click`,
    horas_sustitucion_mes_ps = `time_15_Page Submit`,
    horas_sustitucion_mes_cc = `time_15_Click Count`,
    horas_sustitucion_mes = `15. Horas Guardia`,
    
    pct_absentismo_injustificado_fc = `time_16_First Click`,
    pct_absentismo_injustificado_lc = `time_16_Last Click`,
    pct_absentismo_injustificado_ps = `time_16_Page Submit`,
    pct_absentismo_injustificado_cc = `time_16_Click Count`,
    pct_absentismo_injustificado = `16.Absentistas`,
    
    alumnos_conflictivos_fc = `time_17_First Click`,
    alumnos_conflictivos_lc = `time_17_Last Click`,
    alumnos_conflictivos_ps = `time_17_Page Submit`,
    alumnos_conflictivos_cc = `time_17_Click Count`,
    alumnos_conflictivos = `17. Conflictivos`, #Check capamos aquí? Hay 8 que dicen 100...
    
    programas_vulnerabilidad_fc = `time_18_First Click`,
    programas_vulnerabilidad_lc = `time_18_Last Click`,
    programas_vulnerabilidad_ps = `time_18_Page Submit`,
    programas_vulnerabilidad_cc = `time_18_Click Count`,
    programas_vulnerabilidad = ifelse(`18. Programas`=="Sí", 1, 0), # check ifelse 
    
    directora_fc = `time_19_First Click`,
    directora_lc = `time_19_Last Click`,
    directora_ps = `time_19_Page Submit`,
    directora_cc = `time_19_Click Count`,
    directora = ifelse(`19. Género Director`=="Mujer", 1, 0),
    
     exp_director_fc = `time_20_First Click...196`, #CHECK importante que tanto para la 20 como para la 21 se han generado dos variables de tiempo para cada cosa y no coinciden, por ejemplo: `time_21_First Click...201`, `time_21_First Click...664`. Entiendo que las primeras son para la experiencia director y las segundas para porcentaje de culpa de profesores etc.(también pregunta 20)
     exp_director_lc = `time_20_Last Click...197`,
     exp_director_ps = `time_20_Page Submit...198`,
     exp_director_cc = `time_20_Click Count...199`,
     exp_director = `20. Exp. Director`,

    pct_alumnos_desfavorecidos_1_fc = `time_21_First Click...201`,
    pct_alumnos_desfavorecidos_1_lc = `time_21_Last Click...202`,
    pct_alumnos_desfavorecidos_1_ps = `time_21_Page Submit...203`,
    pct_alumnos_desfavorecidos_1_cc = `time_21_Click Count...204`,
    pct_alumnos_desfavorecidos_1 = `21.%desfavorecidos_1`,
    
    rankingfin_fc = `time_inst_rankingfin_First Click`, # Check Esto no sé lo que es, entiendo que el ranking de políticas, pero por si acaso lo dejo así
    rankingfin_lc = `time_inst_rankingfin_Last Click`,
    rankingfin_ps = `time_inst_rankingfin_Page Submit`,
    rankingfin_cc = `time_inst_rankingfin_Click Count`,
    
    tiempo_presentacion_p_fc = `time_presentacion_p_First Click`,
    tiempo_presentacion_p_lc = `time_presentacion_p_Last Click`,
    tiempo_presentacion_p_ps = `time_presentacion_p_Page Submit`,
    tiempo_presentacion_p_cc = `time_presentacion_p_Click Count`,
    
    alumno_1_1_fc = `time_grupo1.1_First Click`,
    alumno_1_1_lc = `time_grupo1.1_Last Click`,
    alumno_1_1_ps = `time_grupo1.1_Page Submit`,
    alumno_1_1_cc = `time_grupo1.1_Click Count`,
    alumno_1_1 = `Grupo 1.1_1`,
    
    alumno_1_2_fc = `time_grupo1.2_First Click`,
    alumno_1_2_lc = `time_grupo1.2_Last Click`,
    alumno_1_2_ps = `time_grupo1.2_Page Submit`,
    alumno_1_2_cc = `time_grupo1.2_Click Count`,
    alumno_1_2 = `Grupo 1.2_1`,
    
    alumno_1_3_fc = `time_grupo1.3_First Click`,
    alumno_1_3_lc = `time_grupo1.3_Last Click`,
    alumno_1_3_ps = `time_grupo1.3_Page Submit`,
    alumno_1_3_cc = `time_grupo1.3_Click Count`,
    alumno_1_3 = `Grupo 1.3_1`,
    
    alumno_1_4_fc = `time_grupo1.4_First Click`,
    alumno_1_4_lc = `time_grupo1.4_Last Click`,
    alumno_1_4_ps = `time_grupo1.4_Page Submit`,
    alumno_1_4_cc = `time_grupo1.4_Click Count`,
    alumno_1_4 = `Grupo 1.4_1`,
    
    alumno_1_5_fc = `time_grupo1.5_First Click`,
    alumno_1_5_lc = `time_grupo1.5_Last Click`,
    alumno_1_5_ps = `time_grupo1.5_Page Submit`,
    alumno_1_5_cc = `time_grupo1.5_Click Count`,
    alumno_1_5 = `Grupo 1.5_1`,
    
    alumno_1_6_fc = `time_grupo1.6_First Click`,
    alumno_1_6_lc = `time_grupo1.6_Last Click`,
    alumno_1_6_ps = `time_grupo1.6_Page Submit`,
    alumno_1_6_cc = `time_grupo1.6_Click Count`,
    alumno_1_6 = `Grupo 1.6_1`,
    
    alumno_1_7_fc = `time_grupo1.7_First Click`,
    alumno_1_7_lc = `time_grupo1.7_Last Click`,
    alumno_1_7_ps = `time_grupo1.7_Page Submit`,
    alumno_1_7_cc = `time_grupo1.7_Click Count`,
    alumno_1_7 = `Grupo 1.7_1`,
    
    alumno_1_8_fc = `time_grupo1.8_First Click`,
    alumno_1_8_lc = `time_grupo1.8_Last Click`,
    alumno_1_8_ps = `time_grupo1.8_Page Submit`,
    alumno_1_8_cc = `time_grupo1.8_Click Count`,
    alumno_1_8 = `Grupo 1.8_1`,
    
    alumno_2_1_fc = `time_grupo2.1_First Click`,
    alumno_2_1_lc = `time_grupo2.1_Last Click`,
    alumno_2_1_ps = `time_grupo2.1_Page Submit`,
    alumno_2_1_cc = `time_grupo2.1_Click Count`,
    alumno_2_1 = `Grupo 2.1_1`,
    
    alumno_2_2_fc = `time_grupo2.2_First Click`,
    alumno_2_2_lc = `time_grupo2.2_Last Click`,
    alumno_2_2_ps = `time_grupo2.2_Page Submit`,
    alumno_2_2_cc = `time_grupo2.2_Click Count`,
    alumno_2_2 = `Grupo 2.2_1`,
    
    alumno_2_3_fc = `time_grupo2.3_First Click`,
    alumno_2_3_lc = `time_grupo2.3_Last Click`,
    alumno_2_3_ps = `time_grupo2.3_Page Submit`,
    alumno_2_3_cc = `time_grupo2.3_Click Count`,
    alumno_2_3 = `Grupo 2.3_1`,
    
    alumno_2_4_fc = `time_grupo2.4_First Click`,
    alumno_2_4_lc = `time_grupo2.4_Last Click`,
    alumno_2_4_ps = `time_grupo2.4_Page Submit`,
    alumno_2_4_cc = `time_grupo2.4_Click Count`,
    alumno_2_4 = `Grupo 2.4_1`,
    
    alumno_2_5_fc = `time_grupo2.5_First Click`,
    alumno_2_5_lc = `time_grupo2.5_Last Click`,
    alumno_2_5_ps = `time_grupo2.5_Page Submit`,
    alumno_2_5_cc = `time_grupo2.5_Click Count`,
    alumno_2_5 = `Grupo 2.5_1`,
    
    alumno_2_6_fc = `time_grupo2.6_First Click`,
    alumno_2_6_lc = `time_grupo2.6_Last Click`,
    alumno_2_6_ps = `time_grupo2.6_Page Submit`,
    alumno_2_6_cc = `time_grupo2.6_Click Count`,
    alumno_2_6 = `Grupo 2.6_1`,
    
    alumno_2_7_fc = `time_grupo2.7_First Click`,
    alumno_2_7_lc = `time_grupo2.7_Last Click`,
    alumno_2_7_ps = `time_grupo2.7_Page Submit`,
    alumno_2_7_cc = `time_grupo2.7_Click Count`,
    alumno_2_7 = `Grupo 2.7_1`,
    
    alumno_2_8_fc = `time_grupo2.8_First Click`,
    alumno_2_8_lc = `time_grupo2.8_Last Click`,
    alumno_2_8_ps = `time_grupo2.8_Page Submit`,
    alumno_2_8_cc = `time_grupo2.8_Click Count`,
    alumno_2_8 = `Grupo 2.8_1`,
    
    alumno_3_1_fc = `time_grupo3.1_First Click`,
    alumno_3_1_lc = `time_grupo3.1_Last Click`,
    alumno_3_1_ps = `time_grupo3.1_Page Submit`,
    alumno_3_1_cc = `time_grupo3.1_Click Count`,
    alumno_3_1 = `Grupo 3.1_1`,
    
    alumno_3_2_fc = `time_grupo3.2_First Click`,
    alumno_3_2_lc = `time_grupo3.2_Last Click`,
    alumno_3_2_ps = `time_grupo3.2_Page Submit`,
    alumno_3_2_cc = `time_grupo3.2_Click Count`,
    alumno_3_2 = `Grupo 3.2_1`,
    
    alumno_3_3_fc = `time_grupo3.3_First Click`,
    alumno_3_3_lc = `time_grupo3.3_Last Click`,
    alumno_3_3_ps = `time_grupo3.3_Page Submit`,
    alumno_3_3_cc = `time_grupo3.3_Click Count`,
    alumno_3_3 = `Grupo 3.3_1`,
    
    alumno_3_4_fc = `time_grupo3.4_First Click`,
    alumno_3_4_lc = `time_grupo3.4_Last Click`,
    alumno_3_4_ps = `time_grupo3.4_Page Submit`,
    alumno_3_4_cc = `time_grupo3.4_Click Count`,
    alumno_3_4 = `Grupo 3.4_1`,
    
    alumno_3_5_fc = `time_grupo3.5_First Click`,
    alumno_3_5_lc = `time_grupo3.5_Last Click`,  
    alumno_3_5_ps = `time_grupo3.5_Page Submit`,  
    alumno_3_5_cc = `time_grupo3.5_Click Count`,  
    alumno_3_5 = `Grupo 5_1`,  # IMPORTANTE QUE LA 3_5 ESTÁ GUARDADA COMO 5
    
    alumno_3_6_fc = `time_grupo3.6_First Click`,
    alumno_3_6_lc = `time_grupo3.6_Last Click`,
    alumno_3_6_ps = `time_grupo3.6_Page Submit`,
    alumno_3_6_cc = `time_grupo3.6_Click Count`,
    alumno_3_6 = `Grupo 3.6_1`,
    
    alumno_3_7_fc = `time_grupo3.7_First Click`,
    alumno_3_7_lc = `time_grupo3.7_Last Click`,
    alumno_3_7_ps = `time_grupo3.7_Page Submit`,
    alumno_3_7_cc = `time_grupo3.7_Click Count`,
    alumno_3_7 = `Grupo 3.7_1`,
    
    alumno_3_8_fc = `time_grupo3.8_First Click`,
    alumno_3_8_lc = `time_grupo3.8_Last Click`,
    alumno_3_8_ps = `time_grupo3.8_Page Submit`,
    alumno_3_8_cc = `time_grupo3.8_Click Count`,
    alumno_3_8 = `Grupo 3.8_1`,
    
    alumno_4_1_fc = `time_grupo4.1_First Click`,
    alumno_4_1_lc = `time_grupo4.1_Last Click`,
    alumno_4_1_ps = `time_grupo4.1_Page Submit`,
    alumno_4_1_cc = `time_grupo4.1_Click Count`,
    alumno_4_1 = `Grupo 4.1_1`,
    
    alumno_4_2_fc = `time_grupo4.2_First Click`,
    alumno_4_2_lc = `time_grupo4.2_Last Click`,
    alumno_4_2_ps = `time_grupo4.2_Page Submit`,
    alumno_4_2_cc = `time_grupo4.2_Click Count`,
    alumno_4_2 = `Grupo 4.2_1`,
    
    alumno_4_3_fc = `time_grupo4.3_First Click`,
    alumno_4_3_lc = `time_grupo4.3_Last Click`,
    alumno_4_3_ps = `time_grupo4.3_Page Submit`,
    alumno_4_3_cc = `time_grupo4.3_Click Count`,
    alumno_4_3 = `Grupo 4.3_1`,
    
    alumno_4_4_fc = `time_grupo4.4_First Click`,
    alumno_4_4_lc = `time_grupo4.4_Last Click`,
    alumno_4_4_ps = `time_grupo4.4_Page Submit`,
    alumno_4_4_cc = `time_grupo4.4_Click Count`,
    alumno_4_4 = `Grupo 4.4_1`,
    
    alumno_4_5_fc = `time_grupo4.5_First Click`,
    alumno_4_5_lc = `time_grupo4.5_Last Click`,
    alumno_4_5_ps = `time_grupo4.5_Page Submit`,
    alumno_4_5_cc = `time_grupo4.5_Click Count`,
    alumno_4_5 = `Grupo 4.5_1`,
    
    alumno_4_6_fc = `time_grupo4.6_First Click`,
    alumno_4_6_lc = `time_grupo4.6_Last Click`,
    alumno_4_6_ps = `time_grupo4.6_Page Submit`,
    alumno_4_6_cc = `time_grupo4.6_Click Count`,
    alumno_4_6 = `Grupo 4.6_1`,
    
    alumno_4_7_fc = `time_grupo4.7_First Click`,
    alumno_4_7_lc = `time_grupo4.7_Last Click`,
    alumno_4_7_ps = `time_grupo4.7_Page Submit`,
    alumno_4_7_cc = `time_grupo4.7_Click Count`,
    alumno_4_7 = `Grupo 4.7_1`,
    
    alumno_4_8_fc = `time_grupo4.8_First Click`,
    alumno_4_8_lc = `time_grupo4.8_Last Click`,
    alumno_4_8_ps = `time_grupo4.8_Page Submit`,
    alumno_4_8_cc = `time_grupo4.8_Click Count`,
    alumno_4_8 = `Grupo 4.8_1`,
    
    alumno_5_1_fc = `time_grupo5.1_First Click`,
    alumno_5_1_lc = `time_grupo5.1_Last Click`,
    alumno_5_1_ps = `time_grupo5.1_Page Submit`,
    alumno_5_1_cc = `time_grupo5.1_Click Count`,
    alumno_5_1 = `Grupo 5.1_1`,
    
    alumno_5_2_fc = `time_grupo5.2_First Click`,
    alumno_5_2_lc = `time_grupo5.2_Last Click`,
    alumno_5_2_ps = `time_grupo5.2_Page Submit`,
    alumno_5_2_cc = `time_grupo5.2_Click Count`,
    alumno_5_2 = `Grupo 5.2_1`,
    
    alumno_5_3_fc = `time_grupo5.3_First Click`,
    alumno_5_3_lc = `time_grupo5.3_Last Click`,
    alumno_5_3_ps = `time_grupo5.3_Page Submit`,
    alumno_5_3_cc = `time_grupo5.3_Click Count`,
    alumno_5_3 = `Grupo 5.3_1`,
    
    alumno_5_4_fc = `time_grupo5.4_First Click`,
    alumno_5_4_lc = `time_grupo5.4_Last Click`,
    alumno_5_4_ps = `time_grupo5.4_Page Submit`,
    alumno_5_4_cc = `time_grupo5.4_Click Count`,
    alumno_5_4 = `Grupo 5.4_1`,
    
    alumno_5_5_fc = `time_grupo5.5_First Click`,
    alumno_5_5_lc = `time_grupo5.5_Last Click`,
    alumno_5_5_ps = `time_grupo5.5_Page Submit`,
    alumno_5_5_cc = `time_grupo5.5_Click Count`,
    alumno_5_5 = `Grupo 5.5_1`,
    
    alumno_5_6_fc = `time_grupo5.6_First Click`,
    alumno_5_6_lc = `time_grupo5.6_Last Click`,
    alumno_5_6_ps = `time_grupo5.6_Page Submit`,
    alumno_5_6_cc = `time_grupo5.6_Click Count`,
    alumno_5_6 = `Grupo 5.6_1`,
    
    alumno_5_7_fc = `time_grupo5.7_First Click`,
    alumno_5_7_lc = `time_grupo5.7_Last Click`,
    alumno_5_7_ps = `time_grupo5.7_Page Submit`,
    alumno_5_7_cc = `time_grupo5.7_Click Count`,
    alumno_5_7 = `Grupo 5.7_1`,
    
    alumno_5_8_fc = `time_grupo5.8_First Click`,
    alumno_5_8_lc = `time_grupo5.8_Last Click`,
    alumno_5_8_ps = `time_grupo5.8_Page Submit`,
    alumno_5_8_cc = `time_grupo5.8_Click Count`,
    alumno_5_8 = `Grupo 5.8_1`,
    
    alumno_6_1_fc = `time_grupo6.1_First Click`,
    alumno_6_1_lc = `time_grupo6.1_Last Click`,
    alumno_6_1_ps = `time_grupo6.1_Page Submit`,
    alumno_6_1_cc = `time_grupo6.1_Click Count`,
    alumno_6_1 = `Grupo 6_1`, # Check ojo que 6.1_1 está como 6_1
    
    alumno_6_2_fc = `time_grupo6.2_First Click`,
    alumno_6_2_lc = `time_grupo6.2_Last Click`,
    alumno_6_2_ps = `time_grupo6.2_Page Submit`,
    alumno_6_2_cc = `time_grupo6.2_Click Count`,
    alumno_6_2 = `Grupo 6.2_1`,
    
    alumno_6_3_fc = `time_grupo6.3_First Click`,
    alumno_6_3_lc = `time_grupo6.3_Last Click`,
    alumno_6_3_ps = `time_grupo6.3_Page Submit`,
    alumno_6_3_cc = `time_grupo6.3_Click Count`,
    alumno_6_3 = `Grupo 6.3_1`,
    
    alumno_6_4_fc = `time_grupo6.4_First Click`,
    alumno_6_4_lc = `time_grupo6.4_Last Click`,
    alumno_6_4_ps = `time_grupo6.4_Page Submit`,
    alumno_6_4_cc = `time_grupo6.4_Click Count`,
    alumno_6_4 = `Grupo 6.4_1`,
    
    alumno_6_5_fc = `time_grupo6.5_First Click`,
    alumno_6_5_lc = `time_grupo6.5_Last Click`,
    alumno_6_5_ps = `time_grupo6.5_Page Submit`,
    alumno_6_5_cc = `time_grupo6.5_Click Count`,
    alumno_6_5 = `Grupo 6.5_1`,
    
    alumno_6_6_fc = `time_grupo6.6_First Click`,
    alumno_6_6_lc = `time_grupo6.6_Last Click`,
    alumno_6_6_ps = `time_grupo6.6_Page Submit`,
    alumno_6_6_cc = `time_grupo6.6_Click Count`,
    alumno_6_6 = `Grupo 6.6_1`,
    
    alumno_6_7_fc = `time_grupo6.7_First Click`,
    alumno_6_7_lc = `time_grupo6.7_Last Click`,
    alumno_6_7_ps = `time_grupo6.7_Page Submit`,
    alumno_6_7_cc = `time_grupo6.7_Click Count`,
    alumno_6_7 = `Grupo 6.7_1`,
    
    alumno_6_8_fc = `time_grupo6.8_First Click`,
    alumno_6_8_lc = `time_grupo6.8_Last Click`,
    alumno_6_8_ps = `time_grupo6.8_Page Submit`,
    alumno_6_8_cc = `time_grupo6.8_Click Count`,
    alumno_6_8 = `Grupo 6.8_1`,
    
    alumno_7_1_fc = `time_grupo7.1_First Click`,
    alumno_7_1_lc = `time_grupo7.1_Last Click`,
    alumno_7_1_ps = `time_grupo7.1_Page Submit`,
    alumno_7_1_cc = `time_grupo7.1_Click Count`,
    alumno_7_1 = `Grupo 7.1_1`,
    
    alumno_7_2_fc = `time_grupo7.2_First Click`,
    alumno_7_2_lc = `time_grupo7.2_Last Click`,
    alumno_7_2_ps = `time_grupo7.2_Page Submit`,
    alumno_7_2_cc = `time_grupo7.2_Click Count`,
    alumno_7_2 = `Grupo 7.2_1`,
    
    alumno_7_3_fc = `time_grupo7.3_First Click`,
    alumno_7_3_lc = `time_grupo7.3_Last Click`,
    alumno_7_3_ps = `time_grupo7.3_Page Submit`,
    alumno_7_3_cc = `time_grupo7.3_Click Count`,
    alumno_7_3 = `Grupo 7.3_1`,
    
    alumno_7_4_fc = `time_grupo7.4_First Click`,
    alumno_7_4_lc = `time_grupo7.4_Last Click`,
    alumno_7_4_ps = `time_grupo7.4_Page Submit`,
    alumno_7_4_cc = `time_grupo7.4_Click Count`,
    alumno_7_4 = `Grupo 7.4_1`,
    
    alumno_7_5_fc = `time_grupo7.5_First Click`,
    alumno_7_5_lc = `time_grupo7.5_Last Click`,
    alumno_7_5_ps = `time_grupo7.5_Page Submit`,
    alumno_7_5_cc = `time_grupo7.5_Click Count`,
    alumno_7_5 = `Grupo 7.5_1`,
    
    alumno_7_6_fc = `time_grupo7.6_First Click`,
    alumno_7_6_lc = `time_grupo7.6_Last Click`,
    alumno_7_6_ps = `time_grupo7.6_Page Submit`,
    alumno_7_6_cc = `time_grupo7.6_Click Count`,
    alumno_7_6 = `Grupo 7.6_1`,
    
    alumno_7_7_fc = `time_grupo7.7_First Click`,
    alumno_7_7_lc = `time_grupo7.7_Last Click`,
    alumno_7_7_ps = `time_grupo7.7_Page Submit`,
    alumno_7_7_cc = `time_grupo7.7_Click Count`,
    alumno_7_7 = `Grupo 7.7_1`,
    
    alumno_7_8_fc = `time_grupo7.8_First Click`,
    alumno_7_8_lc = `time_grupo7.8_Last Click`,
    alumno_7_8_ps = `time_grupo7.8_Page Submit`,
    alumno_7_8_cc = `time_grupo7.8_Click Count`,
    alumno_7_8 = `Grupo 7.8_1`,
    
    alumno_8_1_fc = `time_grupo8.1_First Click`,
    alumno_8_1_lc = `time_grupo8.1_Last Click`,
    alumno_8_1_ps = `time_grupo8.1_Page Submit`,
    alumno_8_1_cc = `time_grupo8.1_Click Count`,
    alumno_8_1 = `Grupo 8.1_1`,
    
    alumno_8_2_fc = `time_grupo8.2_First Click`,
    alumno_8_2_lc = `time_grupo8.2_Last Click`,
    alumno_8_2_ps = `time_grupo8.2_Page Submit`,
    alumno_8_2_cc = `time_grupo8.2_Click Count`,
    alumno_8_2 = `Grupo 8.2_1`,
    
    alumno_8_3_fc = `time_grupo8.3_First Click`,
    alumno_8_3_lc = `time_grupo8.3_Last Click`,
    alumno_8_3_ps = `time_grupo8.3_Page Submit`,
    alumno_8_3_cc = `time_grupo8.3_Click Count`,
    alumno_8_3 = `Grupo 8.3_1`,
    
    alumno_8_4_fc = `time_grupo8.4_First Click`,
    alumno_8_4_lc = `time_grupo8.4_Last Click`,
    alumno_8_4_ps = `time_grupo8.4_Page Submit`,
    alumno_8_4_cc = `time_grupo8.4_Click Count`,
    alumno_8_4 = `Grupo 8.4_1`,
    
    alumno_8_5_fc = `time_grupo8.5_First Click`,
    alumno_8_5_lc = `time_grupo8.5_Last Click`,
    alumno_8_5_ps = `time_grupo8.5_Page Submit`,
    alumno_8_5_cc = `time_grupo8.5_Click Count`,
    alumno_8_5 = `Grupo 8.5_1`,
    
    alumno_8_6_fc = `time_grupo8.6_First Click`,
    alumno_8_6_lc = `time_grupo8.6_Last Click`,
    alumno_8_6_ps = `time_grupo8.6_Page Submit`,
    alumno_8_6_cc = `time_grupo8.6_Click Count`,
    alumno_8_6 = `Grupo 8.6_1`,
    
    alumno_8_7_fc = `time_grupo8.7_First Click`,
    alumno_8_7_lc = `time_grupo8.7_Last Click`,
    alumno_8_7_ps = `time_grupo8.7_Page Submit`,
    alumno_8_7_cc = `time_grupo8.7_Click Count`,
    alumno_8_7 = `Grupo 8.7_1`,
    
    alumno_8_8_fc = `time_grupo8.8_First Click`,
    alumno_8_8_lc = `time_grupo8.8_Last Click`,
    alumno_8_8_ps = `time_grupo8.8_Page Submit`,
    alumno_8_8_cc = `time_grupo8.8_Click Count`,
    alumno_8_8 = `Grupo 8.8_1`, 
    
    browser_212 = Q212_Browser,
    version_212 = Q212_Version,
    os_212 = `Q212_Operating System`,
    resolution_212 = Q212_Resolution,
    
    orden_pref_refuerzo_fc = `time_R1_First Click`,
    orden_pref_refuerzo_lc = `time_R1_Last Click`,
    orden_pref_refuerzo_sp = `time_R1_Page Submit`,
    orden_pref_refuerzo_cc = `time_R1_Click Count`,
    orden_pref_refuerzo = R1_1,
    orden_pref_criterios_promo = R1_2,
    orden_pref_formacion_prof = R1_3,

    browser_382 = Q382_Browser,
    version_382 = Q382_Version,
    os_382 = `Q382_Operating System`,
    resolution_382 = Q382_Resolution,
    
    tiempo_q383_fc = `Q383_First Click`, # CHECK A QUÉ SE REFIEREN ESTAS?
    tiempo_q383_lc = `Q383_Last Click`,
    tiempo_q383_sp = `Q383_Page Submit`,
    tiempo_q383_cc = `Q383_Click Count`,
    
    browser_q219 = Q219_Browser,
    version_q219 = Q219_Version,
    ossistema_operativo_q219 = `Q219_Operating System`,
    resolution_q219 = Q219_Resolution,
    
    prohibir_repeticion_ley_fc = `time_slider1repley_First Click`,
    prohibir_repeticion_ley_lc = `time_slider1repley_Last Click`,
    prohibir_repeticion_ley_sp = `time_slider1repley_Page Submit`,
    prohibir_repeticion_ley_cc = `time_slider1repley_Click Count`,
    prohibir_repeticion_ley = slider_1,
    
    prohibir_repeticion_txt_fc = `time_text1repley_First Click`,
    prohibir_repeticion_txt_lc = `time_text1repley_Last Click`,
    prohibir_repeticion_txt_sp = `time_text1repley_Page Submit`,
    prohibir_repeticion_txt_cc = `time_text1repley_Click Count`,
    prohibir_repeticion_txt = TEX_repetición_ley,
    
    informe_competencias_fc = `time_slider2elimeso_First Click`,
    informe_competencias_lc = `time_slider2elimeso_Last Click`,
    informe_competencias_sp = `time_slider2elimeso_Page Submit`,
    informe_competencias_cc = `time_slider2elimeso_Click Count`,
    informe_competencias = `slider 2_1`,
    
    informe_competencias_txt_fc = `time_text2elimeso_First Click`,
    informe_competencias_txt_lc = `time_text2elimeso_Last Click`,
    informe_competencias_txt_sp = `time_text2elimeso_Page Submit`,
    informe_competencias_txt_cc = `time_text2elimeso_Click Count`,
    informe_competencias_txt = `TEX_informe Solo eso`,
    
    browser_q226 = Q226_Browser,
    version_q226 = Q226_Version,
    os_q226 = `Q226_Operating System`,
    resolution_q226 = Q226_Resolution,
    
    inicio_bloque3_fc = `time_iniciobloque3_First Click`, # CHECK de nuevo estos clicks no sé si son muy importantes
    inicio_bloque3_lc = `time_iniciobloque3_Last Click`,
    inicio_bloque3_sp = `time_iniciobloque3_Page Submit`,
    inicio_bloque3_cc = `time_iniciobloque3_Click Count`,
    
    impacto_centro_fc = `time_19_0a10_First Click`,
    impacto_centro_lc = `time_19_0a10_Last Click`,
    impacto_centro_sp = `time_19_0a10_Page Submit`,
    impacto_centro_cc = `time_19_0a10_Click Count`,
    impacto_centro_estudiantes = `19.impacto_centro_1`,
    impacto_centro_pasar_sin_competencias = `19.impacto_centro_2`,
    impacto_centro_preparados_nivel_sig = `19.impacto_centro_3`,
    impacto_centro_demasiados_recursos_repetidores = `19.impacto_centro_4`,
    impacto_centro_recursos_repetidores_ineficaces = `19.impacto_centro_5`,
    # Variables con _DO_ excluidas
    
    impacto_region_fc = `time_19_0a10_First Click`,
    impacto_region_lc = `time_19_0a10_Last Click`,
    impacto_region_sp = `time_19_0a10_Page Submit`,
    impacto_region_cc = `time_19_0a10_Click Count`,
    impacto_region_estudiantes = `19.impacto_region_1`,
    impacto_region_pasar_sin_competencias = `19.impacto_region_2`,
    impacto_region_preparados_nivel_sig = `19.impacto_region_3`,
    impacto_region_demasiados_recursos_repetidores = `19.impacto_region_4`,
    impacto_region_recursos_repetidores_ineficaces = `19.impacto_region_5`,
    # Variables con _DO_ excluidas
    
    porcentaje_culpa_fc = `time_20_First Click...652`, #Check importante el 652, 653, etc. para estas y las de empatía
    porcentaje_culpa_lc = `time_20_Last Click...653`,
    porcentaje_culpa_sp = `time_20_Page Submit...654`,
    porcentaje_culpa_cc = `time_20_Click Count...655`,
    pct_culpa_alumnos = `20.PorcentajesCulpa_1_1`,
    pct_culpa_familias = `20.PorcentajesCulpa_2_1`,
    pct_culpa_profesorado = `20.PorcentajesCulpa_3_1`,
    pct_culpa_sistema_educativo = `20.PorcentajesCulpa_4_1`,
    # Variables con _DO_ excluidas
    
    empatia_fc = `time_21_First Click...664`,
    empatia_lc = `time_21_Last Click...665`,
    empatia_sp = `time_21_Page Submit...666`,
    empatia_cc = `time_21_Click Count...667`,
    empatia_escala_1a5 = `21.Empatía_Control_1`,
    empatia_escala_0a100_t1 = `21.Empatía_T1_1`, # check estas a lo mejor queréis cambiar vosotros los nombres según si empiezan en principio, medio, final
    empatia_escala_0a100_t2 = `21.Empatía_T2_1`,
    empatia_escala_0a100_t3 = `21.Empatía_T3_1`,
    
    meritocracia_fc = `time_22_First Click`,
    meritocracia_lc = `time_22_Last Click`,
    meritocracia_sp = `time_22_Page Submit`,
    meritocracia_cc = `time_22_Click Count`,
    meritocracia = `22.Meritocracia`,
    
    tl_centro_fc = `time_23_First Click`,
    tl_centro_lc = `time_23_Last Click`,
    tl_centro_sp = `time_23_Page Submit`,
    tl_centro_cc = `time_23_Click Count`,
    tl_centro_normas_sociales = `23.TL_centro_1`,
    tl_centro_expectativas_claras = `23.TL_centro_2`,
    tl_centro_acuerdo_comportamientos = `23.TL_centro_3`,
    tl_centro_libertad_comportamiento = `23.TL_centro_4`,
    tl_centro_desaprobacion_inapropiado = `23.TL_centro_5`,
    tl_centro_cumplimiento_normas = `23.TL_centro_6`,
    # Variables con _DO_ excluidas
    
    tl_sociedad_fc = `time_23_First Click`,
    tl_sociedad_lc = `time_23_Last Click`,
    tl_sociedad_sp = `time_23_Page Submit`,
    tl_sociedad_cc = `time_23_Click Count`,
    tl_sociedad_normas_sociales = `23.TL_sociedad_1`,
    tl_sociedad_expectativas_claras = `23.TL_sociedad_2`,
    tl_sociedad_acuerdo_comportamientos = `23.TL_sociedad_3`,
    tl_sociedad_libertad_comportamiento = `23.TL_sociedad_4`,
    tl_sociedad_desaprobacion_inapropiado = `23.TL_sociedad_5`,
    tl_sociedad_cumplimiento_normas = `23.TL_sociedad_6`,
    # Variables con _DO_ excluidas
    
    prohibir_sustituir_fc = `time_24_First Click`,
    prohibir_sustituir_lc = `time_24_Last Click`,
    prohibir_sustituir_sp = `time_24_Page Submit`,
    prohibir_sustituir_cc = `time_24_Click Count`,
    prohibir_repeticion = `24aProhibirRepetició`,
    sustituir_eso = `24b.Sustituir ESO`,
    
    tasa_repeticion_fc = `time_25_First Click`,
    tasa_repeticion_lc = `time_25_Last Click`,
    tasa_repeticion_sp = `time_25_Page Submit`,
    tasa_repeticion_cc = `time_25_Click Count`,
    tasa_repeticion_ano_6 = `25.TasaRep2_año_6`, #Check, aquí no conozco las diferentes modalidades para poder cambiar los nombres de variables
    tasa_repeticion_ano_1_1 = `25.TasaRep3_año_1`,
    tasa_repeticion_ano_1_2 = `25.TasaRep4_año_1`,
    tasa_repeticion_ano_1_3 = `25.TasaRep5_año_1`,
    tasa_repeticion_15ano_1 = `25.TasaRep5_15_1`,
    tasa_repeticion_15ano_2 = `25.TasaRep4_15_1`,
    tasa_repeticion_15ano_3 = `25.TasaRep3_15_1`,
    tasa_repeticion_15ano_4 = `25.TasaRep2_15año_1`,
    
    oneclick = oneclick, #Check no sé lo que es pero solo hay 1
    url = url,
    survey_version_id = SurveyVersionID,
    cp_auto = CP_auto,
    city_auto = City_auto,
    consentimiento_pass = consentimiento_pass,
    started = started,
    original_response_id = OriginalResponseID,
    rid = RID,
    session_id = SessionID,
    treatment = Treatment,
    politica = Politica,
    tucentro1 = tucentro1,
    opciones_19 = `19_opciones`,
    ano_19 = `19_año1`,
    ejemplo = Ejemplo,
    empatia = empatía,
    b1_end = B1_end,
    nivel = nivel,
    calibracion = Calibracion,
    grupo1 = Grupo1,
    grupo2 = Grupo2,
    grupo3 = Grupo3,
    grupo4 = Grupo4,
    grupo5 = Grupo5,
    grupo6 = Grupo6,
    grupo7 = Grupo7,
    grupo8 = Grupo8,
    exogeno = Exogeno,
    endogeno = Endógeno,
    awerness = Awerness,
    b2_out_text = B2_outText,
    b2_complet = B2_complet,
    end = end,
    device_identifier = DeviceIdentifier,
    hook_params = hookParams
    #Todas las FL las elimino porque no estoy seguro de que sean necesarias
    )


dfcentrocp1<-dfcentrocp %>% transmute(cp_correcto=`CÓD POSTAL`, 
                         centro= `CÓDIGO`)

df<-left_join(df, dfcentrocp1, by=c("cp"="centro")) %>% 
  mutate(cp=ifelse(!is.na(cp_correcto), cp_correcto, cp)) %>% 
  select(-cp_correcto)

df<-df %>% 
  mutate(cp=ifelse(cp=="Quintanar del rey", "16220", 
                   ifelse(cp=="Tomelloso", "13700", 
                          ifelse(str_detect(cp, "13580"), "13850", 
                                 ifelse(cp=="45003358C", "45694",
                                        ifelse(str_detect(cp,"19210"), "19210", 
                                               ifelse(str_detect(cp,"45000448"), "45180", 
                                                      ifelse(cp=="19004 Y 19208", "19004",
                                                             ifelse(cp=="13.500", "13500", 
                                                                    ifelse(str_detect(cp, "02694"), "02694", 
                                                                           ifelse(cp=="020001160", "02640", 
                                                                                  ifelse(cp=="02.006", "02006", cp))))))))))))

table(nchar(df$cp)!=5) # Sólo 84 de los 3683 han respondido cps que no se corresponden con un cp de verdad

df<-df %>% 
  mutate(cp_imputado=ifelse(
    (nchar(cp)!=5 | grepl("[A-Za-z]",cp) ) & substr(cp_auto,1,2) %in% c("02", "13", "16", "19", "45"),
    cp_auto, 
    ifelse((nchar(cp)!=5 | grepl("[A-Za-z]",cp) ) & !(substr(cp_auto,1,2) %in% c("02", "13", "16", "19", "45")),
           NA, cp))) # Creo la variable cp_imputado aunque sea para un grupo minoritario



# Exploraciones tontas


df1<-df %>% 
  mutate(os_group = case_when(
             str_detect(os, "Android") ~ "Android",
             str_detect(os, "iPhone|iPad|Macintosh") ~ "Apple",
             str_detect(os, "Windows") ~ "Windows",
             str_detect(os, "Linux|Ubuntu|X11") ~ "Linux",
             str_detect(os, "CrOS") ~ "ChromeOS",
             TRUE ~ "Otro"
           )) %>% 
  filter(!is.na(meritocracia), !is.na(os_group)) %>% 
  group_by(os_group, meritocracia) %>% 
  summarise(value=n()) %>% 
  group_by(os_group) %>% 
  mutate(ratio= value/sum(value, na.rm=T))



df1 %>% 
  filter(os_group %in% c("Android", "Apple", "Windows")) %>% 
  ggplot(aes(as.factor(as.numeric(meritocracia)),ratio, fill=os_group, group=os_group)) + 
  geom_col(position="dodge")+
  scale_y_continuous(labels=scales::label_percent())+
  theme_light()+
  theme(axis.title = element_blank(), 
        legend.title=element_blank())+
  ggtitle("¿Cuánto cree usted que influye el esfuerzo en la posición económica que alcanza una persona en España?", 
          "1 es suerte (entorno familiar y contactos) y 10 es esfuerzo (educación y valía)")



puntos <- st_as_sf(df %>% filter(!is.na(latitude)), coords = c("longitude", "latitude"), crs = 4326)


dfmapa %>% 
ggplot() +
  geom_sf(fill = "white", color = "gray70", size = 0.1) +
  geom_sf(data = puntos, color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Puntos geográficos sobre el mapa de España")+
  coord_sf(xlim = st_bbox(dfmapa)[c("xmin", "xmax")],
           ylim = st_bbox(dfmapa)[c("ymin", "ymax")],
           expand = FALSE)


