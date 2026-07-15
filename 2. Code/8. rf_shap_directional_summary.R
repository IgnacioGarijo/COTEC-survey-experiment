#=======================================================#
#### Directional SHAP summaries for teacher RF       ####
#=======================================================#

source("2. Code/0. main.R")

df <- read_parquet(cleandata)

if (!dir.exists(tables)) dir.create(tables, recursive = TRUE)
if (!dir.exists(graficos)) dir.create(graficos, recursive = TRUE)

namelist_cards <- setdiff(namelist, c("alumno_1_1", "alumno_2_1"))

df_cards <- df[, c("id", namelist_cards)]

df_average_deviation <- df_cards %>%
  pivot_longer(all_of(namelist_cards), names_to = "alumno", values_to = "decision") %>%
  drop_na(decision) %>%
  mutate(repite = ifelse(decision == "repite", 1, 0)) %>%
  group_by(alumno) %>%
  mutate(tasa_rep = (sum(repite, na.rm = TRUE) - repite) / (n() - 1)) %>%
  ungroup() %>%
  mutate(diferencia = repite - tasa_rep) %>%
  group_by(id) %>%
  summarise(hb = mean(diferencia, na.rm = TRUE), .groups = "drop")

teacher_data <- df_average_deviation %>%
  left_join(df, by = "id") %>%
  mutate(
    indefinido = ifelse(
      sitlabpub %in% c("Contratada/o laboral indefinido", "Funcionaria/o con destino definitivo"),
      "Permanent", "Temporary"
    ),
    impacto_estudiantes = ifelse(
      is.na(impacto_centro_estudiantes),
      impacto_region_estudiantes,
      impacto_centro_estudiantes
    ),
    empatia = case_when(
      !is.na(empatia_escala_1a5) ~ empatia_escala_1a5,
      !is.na(empatia_escala_0a100_t1) ~ ceiling(empatia_escala_0a100_t1 / 20),
      !is.na(empatia_escala_0a100_t2) ~ ceiling(empatia_escala_0a100_t2 / 20),
      !is.na(empatia_escala_0a100_t3) ~ ceiling(empatia_escala_0a100_t3 / 20),
      TRUE ~ NA_real_
    ),
    empatia = ifelse(empatia == 0, 1, empatia),
    titularidad = ifelse(titularidad %in% c("Pública", "PÃºblica"), "Public", "Private/charter"),
    level = ifelse(nivel == "E. Primaria", "Primary", "Secondary"),
    female = factor(female),
    too_many_resources = coalesce(
      impacto_centro_demasiados_recursos_repetidores,
      impacto_region_demasiados_recursos_repetidores
    ),
    resources_ineffective = coalesce(
      impacto_centro_recursos_repetidores_ineficaces,
      impacto_region_recursos_repetidores_ineficaces
    ),
    z_too_many_resources = as.numeric(scale(too_many_resources)),
    z_resources_ineffective = as.numeric(scale(resources_ineffective)),
    resource_skepticism_index = ifelse(
      is.na(z_too_many_resources) | is.na(z_resources_ineffective),
      NA_real_,
      (z_too_many_resources + z_resources_ineffective) / 2
    ),
    resource_skepticism_index = as.numeric(scale(resource_skepticism_index))
  )

dfrf <- teacher_data %>%
  transmute(
    level = factor(level, levels = c("Primary", "Secondary")),
    experiencia,
    antiguedad3,
    female,
    edad,
    titularidad = factor(titularidad, levels = c("Private/charter", "Public")),
    indefinido = factor(indefinido, levels = c("Permanent", "Temporary")),
    grupos_docencia,
    impacto_estudiantes,
    empatia,
    meritocracia,
    resource_skepticism_index,
    hb
  ) %>%
  drop_na()

rf_formula <- hb ~ level + experiencia + antiguedad3 + female + edad +
  titularidad + indefinido + grupos_docencia + impacto_estudiantes +
  empatia + meritocracia + resource_skepticism_index

set.seed(123)
rf_model <- ranger(
  formula = rf_formula,
  data = dfrf,
  importance = "permutation",
  num.trees = 500,
  mtry = 3,
  min.node.size = 5,
  respect.unordered.factors = TRUE
)

predict_rf <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

dfshap <- dfrf %>% select(-hb)

set.seed(123)
shap_values <- fastshap::explain(
  object = rf_model,
  X = dfshap,
  pred_wrapper = predict_rf,
  nsim = 30,
  adjust = TRUE
)

variable_labels <- tibble(
  variable = c(
    "level", "experiencia", "antiguedad3", "female", "edad", "titularidad",
    "indefinido", "grupos_docencia", "impacto_estudiantes", "empatia",
    "meritocracia", "resource_skepticism_index"
  ),
  variable_label = c(
    "Secondary education", "Years teaching", "Years in current school",
    "Female", "Age", "Public school", "Temporary contract",
    "Number of teaching groups", "Perceived impact on students",
    "Self-reported empathy", "Belief in effort", "Resource skepticism index"
  )
)

factor_reference_map <- tibble(
  variable = c("level", "female", "titularidad", "indefinido"),
  reference_value = c("Primary", "0", "Private/charter", "Permanent"),
  reported_value = c("Secondary", "1", "Public", "Temporary"),
  reference_label = c("Primary education", "Male", "Private/charter school", "Permanent contract"),
  value_label_for_table = c("Secondary education", "Female", "Public school", "Temporary contract")
)

continuous_variables <- tibble(
  variable = c(
    "experiencia", "antiguedad3", "edad", "grupos_docencia",
    "impacto_estudiantes", "empatia", "meritocracia",
    "resource_skepticism_index"
  )
)

dfshap_values <- dfshap %>%
  mutate(row = row_number())

dfshap_values_char <- dfshap_values %>%
  mutate(across(-row, as.character)) %>%
  pivot_longer(-row, names_to = "variable", values_to = "value")

dfshap_values_numeric <- dfshap_values %>%
  select(row, all_of(continuous_variables$variable)) %>%
  pivot_longer(-row, names_to = "variable", values_to = "raw_value_numeric")

shap_long <- as.data.frame(shap_values) %>%
  mutate(row = row_number()) %>%
  pivot_longer(-row, names_to = "variable", values_to = "shap_value") %>%
  left_join(dfshap_values_char, by = c("row", "variable")) %>%
  left_join(dfshap_values_numeric, by = c("row", "variable")) %>%
  left_join(variable_labels, by = "variable") %>%
  mutate(raw_value_numeric = ifelse(is.na(raw_value_numeric), suppressWarnings(as.numeric(value)), raw_value_numeric))

set.seed(123)
boot_means <- map_dfr(1:500, function(b) {
  sampled_rows <- sample(unique(shap_long$row), replace = TRUE)
  tibble(row = sampled_rows) %>%
    count(row, name = "boot_weight") %>%
    left_join(shap_long, by = "row") %>%
    group_by(variable, variable_label, value) %>%
    summarise(mean_shap = weighted.mean(shap_value, boot_weight), .groups = "drop") %>%
    mutate(boot = b)
})

shap_summary <- shap_long %>%
  group_by(variable, variable_label, value) %>%
  summarise(
    n = n(),
    mean_shap = mean(shap_value, na.rm = TRUE),
    sd_shap = sd(shap_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    boot_means %>%
      group_by(variable, variable_label, value) %>%
      summarise(
        ci_low = quantile(mean_shap, 0.025, na.rm = TRUE),
        ci_high = quantile(mean_shap, 0.975, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("variable", "variable_label", "value")
  ) %>%
  mutate(
    mean_shap_pp = 100 * mean_shap,
    ci_low_pp = 100 * ci_low,
    ci_high_pp = 100 * ci_high
  ) %>%
  arrange(variable_label, value)

write.csv(
  shap_summary,
  file.path(tables, "paper_rewrite_rf_directional_shap.csv"),
  row.names = FALSE
)

factor_reference_point <- shap_summary %>%
  left_join(factor_reference_map, by = "variable") %>%
  filter(value == reference_value) %>%
  select(variable, reference_mean_shap = mean_shap)

factor_reference_contrasts <- shap_summary %>%
  left_join(factor_reference_map, by = "variable") %>%
  left_join(factor_reference_point, by = "variable") %>%
  filter(value == reported_value) %>%
  select(-ci_low, -ci_high) %>%
  mutate(
    estimate = mean_shap - reference_mean_shap,
    estimate_pp = 100 * estimate
  )

factor_reference_boot <- boot_means %>%
  left_join(factor_reference_map, by = "variable") %>%
  filter(value == reference_value) %>%
  select(variable, boot, reference_boot_mean = mean_shap)

factor_contrast_boot <- boot_means %>%
  left_join(factor_reference_map, by = "variable") %>%
  left_join(factor_reference_boot, by = c("variable", "boot")) %>%
  filter(value == reported_value) %>%
  mutate(estimate = mean_shap - reference_boot_mean) %>%
  group_by(variable) %>%
  summarise(
    ci_low = quantile(estimate, 0.025, na.rm = TRUE),
    ci_high = quantile(estimate, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

continuous_slopes <- shap_long %>%
  inner_join(continuous_variables, by = "variable") %>%
  group_by(variable, variable_label) %>%
  summarise(
    n = n(),
    estimate = coef(lm(shap_value ~ raw_value_numeric))[["raw_value_numeric"]],
    .groups = "drop"
  )

continuous_slope_boot <- map_dfr(1:500, function(b) {
  sampled_rows <- sample(unique(shap_long$row), replace = TRUE)
  tibble(row = sampled_rows) %>%
    left_join(shap_long, by = "row") %>%
    inner_join(continuous_variables, by = "variable") %>%
    group_by(variable, variable_label) %>%
    summarise(
      estimate = coef(lm(shap_value ~ raw_value_numeric))[["raw_value_numeric"]],
      .groups = "drop"
    ) %>%
    mutate(boot = b)
})

continuous_slopes <- continuous_slopes %>%
  left_join(
    continuous_slope_boot %>%
      group_by(variable) %>%
      summarise(
        ci_low = quantile(estimate, 0.025, na.rm = TRUE),
        ci_high = quantile(estimate, 0.975, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "variable"
  )

shap_reference_contrasts <- bind_rows(
  factor_reference_contrasts %>%
    left_join(factor_contrast_boot, by = "variable") %>%
    transmute(
      variable,
      Variable = value_label_for_table,
      Type = "Categorical contrast",
      Reference = reference_label,
      n,
      estimate_pp,
      ci_low_pp = 100 * ci_low,
      ci_high_pp = 100 * ci_high
    ),
  continuous_slopes %>%
    transmute(
      variable,
      Variable = variable_label,
      Type = "Slope per one-unit increase",
      Reference = "Same scale as OLS",
      n,
      estimate_pp = 100 * estimate,
      ci_low_pp = 100 * ci_low,
      ci_high_pp = 100 * ci_high
    )
  ) %>%
  mutate(
    order = match(
      variable,
      c("level", "experiencia", "antiguedad3", "female", "edad", "titularidad",
        "indefinido", "grupos_docencia", "impacto_estudiantes", "empatia",
        "meritocracia", "resource_skepticism_index")
    )
  ) %>%
  select(
    variable,
    Variable,
    Type,
    Reference,
    n,
    estimate_pp,
    ci_low_pp,
    ci_high_pp,
    order
  ) %>%
  arrange(order) %>%
  select(-order)

write.csv(
  shap_reference_contrasts,
  file.path(tables, "paper_rewrite_rf_reference_shap.csv"),
  row.names = FALSE
)

shap_reference_contrasts %>%
  mutate(
    `SHAP estimate` = sprintf("%.3f", estimate_pp),
    `95\\% interval` = paste0("[", sprintf("%.3f", ci_low_pp), ", ", sprintf("%.3f", ci_high_pp), "]")
  ) %>%
  select(
    Variable,
    Type,
    Reference,
    N = n,
    `SHAP estimate`,
    `95\\% interval`
  ) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  kableExtra::save_kable(file.path(tables, "paper_rewrite_rf_reference_shap.tex"))

lines <- readLines(file.path(tables, "paper_rewrite_rf_reference_shap.tex"))
lines <- lines[!grepl("^\\\\begin\\{table\\}|^\\\\end\\{table\\}|^\\\\caption\\{|^\\\\label\\{", lines)]
lines <- lines[!grepl("^\\\\centering$", lines)]
lines <- gsub("\\\\begin\\{tabular\\}\\[t\\]", "\\\\begin{tabular}", lines)
writeLines(lines, file.path(tables, "paper_rewrite_rf_reference_shap.tex"))

shap_summary %>%
  mutate(
    `Mean SHAP` = sprintf("%.2f", mean_shap_pp),
    `95\\% interval` = paste0("[", sprintf("%.2f", ci_low_pp), ", ", sprintf("%.2f", ci_high_pp), "]")
  ) %>%
  select(
    Variable = variable_label,
    Value = value,
    N = n,
    `Mean SHAP`,
    `95\\% interval`
  ) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  kableExtra::save_kable(file.path(tables, "paper_rewrite_rf_directional_shap.tex"))

lines <- readLines(file.path(tables, "paper_rewrite_rf_directional_shap.tex"))
lines <- lines[!grepl("^\\\\begin\\{table\\}|^\\\\end\\{table\\}|^\\\\caption\\{|^\\\\label\\{", lines)]
lines <- lines[!grepl("^\\\\centering$", lines)]
lines <- gsub("\\\\begin\\{tabular\\}\\[t\\]", "\\\\begin{tabular}", lines)
writeLines(lines, file.path(tables, "paper_rewrite_rf_directional_shap.tex"))

plot_data <- shap_summary %>%
  mutate(
    variable_label = fct_reorder(variable_label, mean_shap_pp, .fun = max),
    value = factor(value)
  )

gg_shap <- ggplot(plot_data, aes(x = mean_shap_pp, y = variable_label, color = value)) +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey45") +
  geom_errorbar(aes(xmin = ci_low_pp, xmax = ci_high_pp), orientation = "y", width = 0.18, linewidth = 0.6) +
  geom_point(size = 2.7) +
  labs(
    title = "Directional SHAP values from the teacher-characterization random forest",
    subtitle = "Mean contribution to predicted teacher harshness by value of each predictor",
    x = "Mean SHAP value, percentage points of teacher harshness",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(
  file.path(graficos, "paper_rewrite_rf_directional_shap.jpeg"),
  gg_shap,
  width = 10.5,
  height = 7,
  dpi = 300
)

message("Finished directional SHAP table and plot.")
