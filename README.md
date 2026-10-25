# COTEC survey experiment

Repositorio de trabajo con los manuscritos y las presentaciones vigentes. Las versiones anteriores están en el historial de Git, no en carpetas duplicadas.

## Documentos actuales

- `4. Latex/paper_A/paper_A_teacher_decisions.pdf`: decisiones docentes; fuente `.tex` en la misma carpeta.
- `4. Latex/paper_A/supporting_analyses.pdf`: análisis conservados fuera del manuscrito (mitades, bootstrap de coeficientes y predicción); fuente `.tex` en la misma carpeta.
- `4. Latex/paper_B/paper_B_policy_scenarios.pdf`: políticas; fuente `.tex` en la misma carpeta.
- `5. Presentation/COTEC/index.html`: presentación COTEC; fuente `ppt_total.qmd`.
- `5. Presentation/Prelim results/index.html`: resultados preliminares; fuente `ppt.qmd`.
- `5. Presentation/SEET2026/presentacion_seet.pdf`: presentación SEET; fuente `.tex`.
- `4. Latex/Pre_registration.pdf`: documento de diseño y cuestionario, conservado como fuente.

Los colores del paper A comparan el texto con la versión editada por los autores guardada en `09f9cd4` (24 de octubre de 2026, antes de aplicar las revisiones de coautores y reconstrucción): azul para contenido conservado, naranja para modificaciones y rojo para adiciones. El paper B y las presentaciones mantienen sus especificaciones propias.

El análisis actual de A está en `2. Code/paper_A_final_revision/` y sus resultados en `3. Output/paper_A_final_revision/`. Utiliza reconstrucción B, cribado por el ejemplo favorable del propio registro y referencias agrupadas por los seis atributos. Son 2.604 docentes, 20.467 decisiones y 2.164 casos completos. `3. Output/paper_A/figures/` conserva las rutas canónicas de sus figuras. Las tablas/objetos anteriores de `paper_A` son dependencias históricas del documento de análisis conservados y de la reconstrucción, no los resultados actuales del manuscrito.

## Estructura

- `1. Data/`: fuentes originales y base limpia, locales y excluidas de Git. Incluye `teacher_population.xlsx` y `cotec_presentation_clean.parquet`; ya no dependen de Descargas ni de otra carpeta de trabajo.
- `2. Code/`: preparación de datos, análisis compartidos, análisis actuales de A y B, construcción de tablas/figuras y comprobaciones.
- `3. Output/shared/`: cálculos compartidos necesarios para los dos papers.
- `3. Output/paper_A/` y `paper_B/`: resultados agregados vigentes y objetos locales necesarios para recalcularlos.
- `3. Output/population/`: extracción de la referencia poblacional y auditoría de celdas.
- `3. Output/figures/` y `tables/`: recursos utilizados por las presentaciones históricas.
- `4. Latex/paper_A/` y `paper_B/`: una fuente canónica y un PDF vigente por paper.
- `5. Presentation/`: fuentes, estilos, recursos y salida vigente de cada presentación.

`2. Code/artifact_manifest.json` enumera los resultados que deben conservarse. Los scripts compartidos limitan sus exportaciones a ese conjunto para no recrear salidas descartadas.

## Reproducir los documentos desde los resultados guardados

Desde la raíz, con Python, R y LaTeX disponibles:

```powershell
python -m pip install -r "2. Code/requirements.txt"
python "2. Code/run_pipeline.py"
```

Esto construye las figuras y tablas actuales a partir de los agregados guardados, compila los dos papers dos veces y comprueba dependencias, muestras y referencias. El documento de análisis conservados permanece como instantánea histórica y no se recompila con la muestra nueva. No reescribe la argumentación del manuscrito. Para regenerar solo tablas y figuras, añadir `--no-pdf`.

Para reconstruir también las tres presentaciones desde sus fuentes y recursos, añadir `--presentations`. Requiere Quarto y los paquetes R utilizados por las diapositivas. El programa admite `RSCRIPT_PATH`, `PDFLATEX_PATH` y `QUARTO_PATH` si no están en PATH. Los archivos HTML con tablas incrustadas mantienen referencias relativas a `3. Output/tables/`; no se debe distribuir el HTML de resultados preliminares aislado de esos recursos.

## Reestimar los análisis de los papers

```powershell
python "2. Code/run_pipeline.py" --analysis
```

Esta opción reestima A mediante `paper_A_final_revision/prepare.R`, los tres candidatos de `models.R`, los puntos de la especificación diagonal en cinco muestras, `audit_models.R` y 199 remuestreos docentes. Después construye tablas, figuras y PDF. B se recompila con sus resultados guardados; no se reestima en esta revisión. Se conservan las dependencias locales `paper_A/reports/analysis_objects.rds` y `paper_A_reconstruction/local/provenance.rds`; si falta esta última, la pipeline ejecuta la preparación de la reconstrucción. Los datos individuales y los objetos RDS permanecen fuera de Git. Los scripts 9/11/15 y los análisis anteriores se conservan como dependencias de B, las presentaciones y los análisis archivados.

Si es necesario reconstruir los parquet desde el Excel de encuesta, ejecutar antes `Rscript "2. Code/1.limpieza_datos.R"` desde la raíz. `0. main.R` define las rutas, funciones y vectores de atributos. Los datos originales no se eliminan ni se sobreescriben en la limpieza del repositorio.

El proyecto conserva `renv.lock`; las versiones utilizadas en los análisis recientes están en los archivos `R_session*.txt` de los resultados. La pipeline actual requiere, entre otros, arrow, dplyr, tidyr, fixest, sandwich, ggplot2, jsonlite, ranger, glmnet, fastshap, lme4 y glmmTMB. No se instalan ni actualizan automáticamente paquetes durante la ejecución.

Los scripts 2 y 3 y `presentation_supplement.R` se conservan porque son fuentes de las figuras y tablas de las presentaciones. El suplemento utiliza objetos de una sesión de análisis y no es una pipeline autónoma. Las presentaciones se recompilan con sus recursos guardados; no se sustituyen sus cifras por las del paper A. El script 2 utiliza la instantánea histórica local específica de COTEC.

## Comprobaciones y recuperación

```powershell
Rscript --vanilla "2. Code/check_analysis.R"
python "2. Code/check_repository.py"
```

La descomposición vigente se reproduce con `paper_A_final_revision/models.R`: seis atributos, 15 interacciones, harshness observado y seis pendientes aleatorias con covarianza diagonal, sin intercepto aleatorio ni ramas experimentales. Las covarianzas por pares y sin restricciones se conservan como sensibilidades en frontera. El bootstrap reconstruye referencias, harshness y centrado en cada remuestra; todos sus ajustes comprueban gradiente y Hessiano. `paper_A_final_revision/audit.py` verifica el manuscrito actual; `35. audit_A_revision.py` y `49. A_final_decomposition.R` corresponden a la especificación histórica. `revise_text.py` documenta una edición puntual desde el commit de referencia y NO se ejecuta en la pipeline, para no sobrescribir futuras ediciones del autor.

Los PDF vigentes se incluyen en Git. Para recuperaciones anteriores a la limpieza del 29/09/2026, usar la etiqueta local `pre-cleanup-2026-09-29`; conserva también las notas y revisiones que faltaban por versionar. Borrar archivos de la versión de trabajo no elimina las versiones anteriores de los commits. La limpieza no reescribe el historial ni realiza un push.

Los archivos auxiliares de LaTeX, caches, resultados con datos individuales y `tmp/` quedan ignorados. No crear nuevas carpetas fechadas para cada revisión: actualizar los archivos canónicos y hacer commits.
