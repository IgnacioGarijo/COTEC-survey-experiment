# Informe extra para Ignacio

## 1. Coherencia entre MDE del paper y MDE del prerregistro

El prerregistro reportaba MDEs en desviaciones estandar: 2500 profesores = 0.24 SD; 3500 profesores = 0.2 SD; 4500 profesores = 0.18 SD; 5500 profesores = 0.16 SD; 6500 profesores = 0.15 SD; 7500 profesores = 0.14 SD.

El paper actual calcula MDEs post-estimacion para cada contraste preregistrado usando el error estandar observado. Es conceptualmente coherente con el prerregistro porque ambos calculos preguntan que tamano de efecto se podia detectar con 80% de potencia, pero no son numericamente identicos porque el prerregistro era ex ante, en SD, con R2 = 0.1, SD = 0.10 y 10 brazos; el paper usa SEs observados, muestras por contraste y unidades de harshness.

La desviacion estandar observada de hb es 0.192. Los MDEs estandarizados actuales para los contrastes principales son: H1|1 = 0.20 SD; H2|1 = 0.14 SD; H3|1 = 0.15 SD.

Mi lectura: los MDEs actuales estan en el mismo orden de magnitud que los del prerregistro para comparaciones amplias, aunque algo mas grandes para algunos contrastes especificos de policy/alignment. Tiene sentido presentarlos como sensibilidad, no como reemplazo del power analysis ex ante.

## 2. Study 1 ampliado: Control vs todos los tratamientos

Cuando se agrupan Policy treatment, Revelation treatment y Awareness treatment como un unico grupo tratado, el efecto de Any treatment vs Control es -0.016 (p = 0.208).

H1|2 ampliada recalcula policy asignada controlando por favorite policy. Los coeficientes principales de assigned policy son: assignedPolicy 1 = -0.025 (p = 0.070); assignedPolicy 2 = -0.003 (p = 0.824); assignedPolicy 3 = -0.009 (p = 0.515).

H1|3 ampliada recalcula alignment. Los coeficientes de alignment son: assignationfavorite = -0.011 (p = 0.437); assignationleast-favorite = -0.010 (p = 0.486).

Esto no cambia la historia: con mas power al juntar los tres brazos, sigue sin aparecer un efecto medio de estar en cualquier tratamiento frente a Control ni un efecto claro de alignment.

La tabla completa esta en `3. Output/tables/paper_rewrite_study1_all_treatments.tex` y el CSV en `3. Output/tables/paper_rewrite_study1_all_treatments.csv`.

## 3. Tiempos de encuesta

Duracion total mediana: 12.8 minutos. Percentil 1.75: 0.1 minutos. Percentil 98.25: 1448.3 minutos. Maximo: 14881.5 minutos.

La regla preregistrada de quitar 1.75% mas rapidos y 1.75% mas lentos marcaria 160 observaciones.

| Grupo | N | Media total (min) | Mediana total (min) | Media tarjetas (min) | Mediana tarjetas (min) | Harshness media |
|---|---:|---:|---:|---:|---:|---:|
| Fastest 1.75% |    80 | 0.1 | 0.1 | 0.0 | 0.0 | NA |
| Middle 96.5% | 4,393 | 34.3 | 12.8 | 2.1 | 1.7 | 0.000 |
| Slowest 1.75% |    80 | 4918.0 | 3120.0 | 2.1 | 1.7 | -0.027 |

La cola rapida no parece formar parte de la muestra analitica de tarjetas: su tiempo medio y mediano en tarjetas es cero, y por eso no aparece harshness media. La cola lenta, en cambio, si parece haber llegado al task, pero la duracion total es desproporcionada y probablemente refleja encuestas dejadas abiertas.

Mi lectura: la cola lenta es claramente compatible con gente que dejo la encuesta abierta y volvio despues. La cola rapida debe excluirse si no completa el task. La regla preregistrada de trim simetrico es defendible como limpieza mecanica, pero conviene reportar robustez con y sin trim si los resultados principales cambian.
