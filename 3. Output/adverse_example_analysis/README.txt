ANÁLISIS COMPLEMENTARIO: EJEMPLO DESFAVORABLE

Estado: exploratorio, separado del manuscrito; incorporación no autorizada todavía.
Fuente de estimación: 2. Code/adverse_example_analysis/analyze.R.
Informe: informe.qmd; distribución autónoma: informe.html.

Reproducción desde la raíz:
Rscript --vanilla "2. Code/adverse_example_analysis/analyze.R"
quarto render "3. Output/adverse_example_analysis/informe.qmd"

En el entorno actual Rscript es R 4.4.0 y Quarto está en C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe. QUARTO_R puede fijarse a C:/Program Files/R/R-4.4.0/bin. No se ha restaurado renv ni instalado paquetes para este análisis; R_session.txt registra versiones. Quarto informa que el proyecto renv no está sincronizado; el render se ha completado utilizando el entorno disponible.

La estimación requiere el parquet privado y los RDS locales ya existentes; el QMD solo lee agregados CSV. No se escriben registros individuales ni identificadores. Los agregados, código, QMD y HTML se conservan juntos con auditoría y revisión independiente. No forma parte de la pipeline del manuscrito, para evitar que una compilación normal incorpore un análisis aún no seleccionado.

Control: 2.520 profesores, 348 progresa y 2.172 repite; 2.164 casos completos para modelos conjuntos; 19.217 otras decisiones tras retirar campos compartidos y ambos perfiles desfavorables. Modelos de caracterización con HC3 y BH; otras decisiones con perfil/conjunto/contexto/género y SE agrupados por profesor. Ningún contraste de medias del harshness se usa para inferencia, pues las referencias están estimadas conjuntamente. Sensibilidad sin campo extremo compartido: 2.220 profesores.

Verificación independiente: independent_review.txt. Revisión visual del HTML a 1440 y 850 píxeles, dos figuras legibles, tablas técnicas desplegables, sin imágenes ausentes ni desbordamiento horizontal de la página. Los auxiliares de QA permanecen en tmp/, no versionados.
