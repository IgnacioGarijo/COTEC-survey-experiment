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

El PDF principal de A compara con la versión aceptada inmediatamente anterior a la revisión del 27/10/2026, congelada en `8306e22`: texto conservado negro, adiciones azules y eliminaciones rojas tachadas. Se ha reiniciado la comparación; las eliminaciones históricas anteriores ya no se arrastran. Las tablas y figuras sustituidas se conservan en rojo con una etiqueta explícita. La fuente editable del artículo aceptado es `4. Latex/paper_A/paper_A_content.tex`; no editar directamente el archivo generado `paper_A_teacher_decisions.tex`. `comparison_baseline/` conserva las imágenes necesarias para recuperar los gráficos de origen. El paper B y las presentaciones mantienen sus especificaciones propias.

El análisis actual de A está en `2. Code/paper_A_author_revision/` y sus resultados en `3. Output/paper_A_author_revision/`. Utiliza reconstrucción B, cribado por el ejemplo favorable del propio registro y referencias agrupadas por los seis atributos. La muestra principal contiene 2.520 docentes con ocho respuestas (20.160 decisiones); la muestra común con covariables completas, 2.164 docentes (17.312 decisiones), conserva las referencias y cortes de terciles principales. Los registros parciales quedan como sensibilidad. `3. Output/paper_A/figures/` conserva las rutas canónicas. Las bibliotecas y objetos de `paper_A_final_revision/` siguen siendo dependencias de preparación y reconstrucción; sus cifras anteriores no son la muestra principal vigente.

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

Esta opción ejecuta `paper_A_author_revision/analysis.R` (preparación, candidatos de covarianza, sensibilidades y 199 remuestreos docentes), `standardize.R` (1.999 remuestras sobre soporte común), `prediction.R` (399 reestimaciones por comparación predictiva, incluida la ampliación con covariables completas) y `audit_partitions.R`. Después construye tablas, figuras y PDF. B se recompila con sus resultados guardados; no se reestima en esta revisión. La reestimación predictiva completa puede ser prolongada. Se conservan como entradas locales los objetos de reconstrucción de `paper_A_final_revision/local/`, además de los datos originales y las dependencias anteriores documentadas en `3. Output/paper_A_author_revision/README.md`. Los datos individuales y objetos RDS permanecen fuera de Git. Los scripts 9/11/15 y los análisis anteriores se conservan como dependencias de B, las presentaciones y los análisis archivados.

Si es necesario reconstruir los parquet desde el Excel de encuesta, ejecutar antes `Rscript "2. Code/1.limpieza_datos.R"` desde la raíz. `0. main.R` define las rutas, funciones y vectores de atributos. Los datos originales no se eliminan ni se sobreescriben en la limpieza del repositorio.

El proyecto conserva `renv.lock`; las versiones utilizadas en los análisis recientes están en los archivos `R_session*.txt` de los resultados. La pipeline actual requiere, entre otros, arrow, dplyr, tidyr, fixest, sandwich, ggplot2, jsonlite, ranger, glmnet, fastshap, lme4 y glmmTMB. No se instalan ni actualizan automáticamente paquetes durante la ejecución.

Los scripts 2 y 3 y `presentation_supplement.R` se conservan porque son fuentes de las figuras y tablas de las presentaciones. El suplemento utiliza objetos de una sesión de análisis y no es una pipeline autónoma. Las presentaciones se recompilan con sus recursos guardados; no se sustituyen sus cifras por las del paper A. El script 2 utiliza la instantánea histórica local específica de COTEC.

## Comprobaciones y recuperación

```powershell
Rscript --vanilla "2. Code/check_analysis.R"
python "2. Code/check_repository.py"
```

La descomposición vigente se reproduce con `paper_A_author_revision/analysis.R`, que reutiliza la biblioteca de `paper_A_final_revision/models.R`: seis atributos, 15 interacciones, harshness observado y seis pendientes aleatorias con covarianza diagonal, sin intercepto aleatorio ni ramas experimentales. Las covarianzas por pares y sin restricciones se conservan como sensibilidades en frontera. El bootstrap reconstruye referencias, harshness y centrado en cada remuestra; todos sus ajustes comprueban gradiente y Hessiano. `paper_A_author_revision/audit.py` verifica el manuscrito actual; los auditores anteriores corresponden a otras especificaciones. `revise_text.py` documenta una edición puntual desde el commit de referencia y NO se ejecuta en la pipeline, para no sobrescribir futuras ediciones del autor.

Los PDF vigentes se incluyen en Git. Para recuperaciones anteriores a la limpieza del 29/09/2026, usar la etiqueta local `pre-cleanup-2026-09-29`; conserva también las notas y revisiones que faltaban por versionar. Borrar archivos de la versión de trabajo no elimina las versiones anteriores de los commits. La limpieza no reescribe el historial ni realiza un push.

Los archivos auxiliares de LaTeX, caches, resultados con datos individuales y `tmp/` quedan ignorados. No crear nuevas carpetas fechadas para cada revisión: actualizar los archivos canónicos y hacer commits.

La comparación se regenera con `2. Code/paper_A_author_revision/redline.py` (requiere Perl y latexdiff-so; admite PERL_PATH y LATEXDIFF_SCRIPT). `compile.py` compila la versión aceptada y la comparación en `tmp/`, y publica únicamente el PDF principal. `audit.py` comprueba la recuperación de ambos textos y las exhibiciones desde la comparación. Los scripts históricos de edición y el actual `revise_text.py` documentan revisiones puntuales y NO se ejecutan en la pipeline: podrían sobrescribir cambios posteriores del autor.
