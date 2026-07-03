#====================================================#
#### Paper rewrite: preregistered and descriptive ####
#====================================================#

source("2. Code/0. main.R")

df <- read_parquet(cleandata)

if (!dir.exists(tables)) dir.create(tables, recursive = TRUE)
if (!dir.exists(graficos)) dir.create(graficos, recursive = TRUE)
if (!dir.exists(file.path(output, "reports"))) dir.create(file.path(output, "reports"), recursive = TRUE)

#==============================#
#### 1. Harshness measures ####
#==============================#

namelist_cards <- setdiff(namelist, c("alumno_1_1", "alumno_2_1"))

df_cards <- df[, c("id", namelist_cards)]

df_nrep <- df_cards %>%
  pivot_longer(all_of(namelist_cards), names_to = "alumno", values_to = "decision") %>%
  drop_na(decision) %>%
  mutate(repite = ifelse(decision == "repite", 1, 0)) %>%
  group_by(id) %>%
  summarise(ha = mean(repite, na.rm = TRUE), .groups = "drop")

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

df_card_long <- df_cards %>%
  pivot_longer(all_of(namelist_cards), names_to = "alumno", values_to = "decision") %>%
  drop_na(decision) %>%
  mutate(
    repite = ifelse(decision == "repite", 1, 0),
    male = ifelse(alumno %in% vector_niño, 1, 0),
    complex_background = ifelse(alumno %in% vector_extranjero, 1, 0),
    failed_subjects = ifelse(alumno %in% vector_suspensos, 1, 0),
    low_competence = ifelse(alumno %in% vector_carencias, 1, 0),
    absent = ifelse(alumno %in% vector_absentista, 1, 0),
    disruptive = ifelse(alumno %in% vector_expulsion, 1, 0)
  )

model_card_simple <- glm(
  repite ~ male + complex_background + failed_subjects + low_competence + absent + disruptive,
  family = binomial,
  data = df_card_long
)

df_predicted_simple <- df_card_long %>%
  mutate(preds = predict(model_card_simple, type = "response"),
         diferencia = repite - preds) %>%
  group_by(id) %>%
  summarise(hc = mean(diferencia, na.rm = TRUE), .groups = "drop")

df_school_info <- df %>%
  select(id, primaria, titularidad)

df_card_complete <- df_card_long %>%
  left_join(df_school_info, by = "id")

model_card_complete <- glm(
  repite ~ male + complex_background + failed_subjects + low_competence +
    absent + disruptive + primaria + titularidad,
  family = binomial,
  data = df_card_complete
)

df_predicted_complete <- df_card_complete %>%
  mutate(preds_compl = predict(model_card_complete, type = "response"),
         diferencia = repite - preds_compl) %>%
  group_by(id) %>%
  summarise(hd = mean(diferencia, na.rm = TRUE), .groups = "drop")

df_harshness <- df_average_deviation %>%
  inner_join(df_predicted_simple, by = "id") %>%
  inner_join(df_nrep, by = "id") %>%
  inner_join(df_predicted_complete, by = "id")

#=============================#
#### 2. Teacher-level data ####
#=============================#

teacher_data <- df_harshness %>%
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
    titularidad = ifelse(titularidad == "Pública", "Public", "Private/charter"),
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
    resource_skepticism_index = as.numeric(scale(resource_skepticism_index)),
    D = case_when(
      treatment == 1 ~ "Control",
      treatment %in% c(2:4) ~ "Policy treatment",
      treatment %in% c(5:7) ~ "Revelation treatment",
      treatment %in% c(8:10) ~ "Awareness treatment"
    ),
    D = factor(D, levels = c("Control", "Policy treatment", "Revelation treatment", "Awareness treatment")),
    control = factor(ifelse(treatment == 1, "Control", "Non-control"),
                     levels = c("Control", "Non-control")),
    assigned = factor(ifelse(control == "Control", "Control", paste0("Policy ", politica))),
    assigned = relevel(assigned, ref = "Control"),
    favorite = factor(case_when(
      orden_pref_refuerzo == 1 ~ "Policy 1",
      orden_pref_criterios_promo == 1 ~ "Policy 2",
      orden_pref_formacion_prof == 1 ~ "Policy 3"
    )),
    favorite = relevel(favorite, ref = "Policy 1"),
    least_favorite = factor(case_when(
      orden_pref_refuerzo == 3 ~ "Policy 1",
      orden_pref_criterios_promo == 3 ~ "Policy 2",
      orden_pref_formacion_prof == 3 ~ "Policy 3"
    )),
    least_favorite = relevel(least_favorite, ref = "Policy 1"),
    favorite_num = factor(case_when(
      orden_pref_refuerzo == 1 ~ 1,
      orden_pref_criterios_promo == 1 ~ 2,
      orden_pref_formacion_prof == 1 ~ 3
    )),
    favorite_num = relevel(favorite_num, ref = "1"),
    least_favorite_num = factor(case_when(
      orden_pref_refuerzo == 3 ~ 1,
      orden_pref_criterios_promo == 3 ~ 2,
      orden_pref_formacion_prof == 3 ~ 3
    )),
    least_favorite_num = relevel(least_favorite_num, ref = "1"),
    politica = factor(ifelse(D != "Control", paste0("Policy ", politica), NA)),
    politica = relevel(politica, ref = "Policy 1")
  ) %>%
  drop_na(favorite)

#=========================================#
#### 3. Utilities for exported tables ####
#=========================================#

stars <- function(p) {
  ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))
}

fmt_coef <- function(x, p) {
  paste0(sprintf("%.3f", x), stars(p))
}

fmt_se <- function(x) {
  paste0("(", sprintf("%.3f", x), ")")
}

clean_tex_table <- function(file) {
  lines <- readLines(file)
  lines <- lines[!grepl("^\\\\begin\\{table\\}|^\\\\end\\{table\\}|^\\\\caption\\{|^\\\\label\\{", lines)]
  lines <- lines[!grepl("^\\\\centering$", lines)]
  lines <- gsub("\\\\begin\\{tabular\\}\\[t\\]", "\\\\begin{tabular}", lines)
  writeLines(lines, file)
}

cluster_vcov <- function(model, cluster) {
  keep <- seq_along(cluster)
  if (!is.null(model$na.action)) keep <- keep[-as.integer(model$na.action)]
  cluster <- cluster[keep]
  X <- model.matrix(model)
  u <- residuals(model)
  bread <- solve(crossprod(X))
  meat <- matrix(0, ncol(X), ncol(X))
  for (cl in unique(cluster)) {
    idx <- which(cluster == cl)
    xu <- crossprod(X[idx, , drop = FALSE], u[idx])
    meat <- meat + xu %*% t(xu)
  }
  G <- length(unique(cluster))
  N <- nrow(X)
  K <- ncol(X)
  correction <- (G / (G - 1)) * ((N - 1) / (N - K))
  correction * bread %*% meat %*% bread
}

tidy_cluster <- function(model, cluster) {
  vc <- cluster_vcov(model, cluster)
  se <- sqrt(diag(vc))
  est <- coef(model)
  dfree <- length(unique(cluster)) - 1
  tibble(
    term = names(est),
    estimate = as.numeric(est),
    std.error = as.numeric(se),
    statistic = estimate / std.error,
    p.value = 2 * pt(abs(statistic), df = dfree, lower.tail = FALSE)
  )
}

#================================#
#### 4. Teacher characterization ####
#================================#

char_model_x1 <- lm(
  hb ~ level + experiencia + antiguedad3 + female + edad +
    titularidad + indefinido + grupos_docencia,
  data = teacher_data
)

char_model_x2 <- lm(
  hb ~ level + experiencia + antiguedad3 + female + edad +
    titularidad + indefinido + grupos_docencia + impacto_estudiantes +
    empatia + meritocracia,
  data = teacher_data
)

char_model_narrow <- lm(
  hb ~ level + experiencia + antiguedad3 + female + edad +
    titularidad + indefinido + grupos_docencia + impacto_estudiantes +
    empatia + meritocracia + resource_skepticism_index,
  data = teacher_data
)

modelsummary::modelsummary(
  list("X1 covariates" = char_model_x1,
       "X2 covariates" = char_model_x2,
       "X2 + resource skepticism" = char_model_narrow),
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_map = c(
    "levelSecondary" = "Secondary education",
    "experiencia" = "Years teaching",
    "antiguedad3" = "Years in current school",
    "female1" = "Female",
    "edad" = "Age",
    "titularidadPublic" = "Public school",
    "indefinidoTemporary" = "Temporary contract",
    "grupos_docencia" = "Number of teaching groups",
    "impacto_estudiantes" = "Perceived impact on students",
    "empatia" = "Self-reported empathy",
    "meritocracia" = "Belief in effort",
    "resource_skepticism_index" = "Resource skepticism index"
  ),
  gof_omit = "AIC|BIC|Log.Lik.|RMSE",
  output = file.path(tables, "paper_rewrite_characterization_regression.tex")
)
clean_tex_table(file.path(tables, "paper_rewrite_characterization_regression.tex"))

harshness_plot_data <- teacher_data %>%
  transmute(
    hb,
    `Secondary education (=1)` = ifelse(level == "Secondary", 1, 0),
    Age = edad,
    `Belief in effort` = meritocracia,
    `Resource skepticism` = resource_skepticism_index
  ) %>%
  pivot_longer(-hb, names_to = "variable", values_to = "x_value") %>%
  filter(!is.na(hb), !is.na(x_value))

harshness_plot_labels <- harshness_plot_data %>%
  group_by(variable) %>%
  summarise(
    correlation = cor(x_value, hb),
    .groups = "drop"
  ) %>%
  mutate(label = paste0("r = ", sprintf("%.2f", correlation)))

harshness_significant_plot <- ggplot(harshness_plot_data, aes(x = x_value, y = hb)) +
  geom_point(alpha = 0.18, size = 0.7, position = position_jitter(width = 0.03, height = 0)) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.7, color = "#1f5a85", fill = "#9ecae1") +
  geom_text(
    data = harshness_plot_labels,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.35,
    size = 3.2
  ) +
  facet_wrap(~ variable, scales = "free_x", ncol = 2) +
  labs(x = NULL, y = "Teacher harshness") +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.margin = ggplot2::margin(8, 8, 8, 8)
  )
ggsave(
  filename = file.path(graficos, "paper_rewrite_harshness_significant_predictors.jpeg"),
  plot = harshness_significant_plot,
  width = 8.5,
  height = 6.2,
  dpi = 300
)

appendix_models <- list(
  "Number repetitions" = lm(ha ~ level + experiencia + antiguedad3 + female + edad +
                              titularidad + indefinido + grupos_docencia + impacto_estudiantes +
                              empatia + meritocracia + resource_skepticism_index, data = teacher_data),
  "Average deviation" = char_model_narrow,
  "Predicted deviation" = lm(hc ~ level + experiencia + antiguedad3 + female + edad +
                               titularidad + indefinido + grupos_docencia + impacto_estudiantes +
                               empatia + meritocracia + resource_skepticism_index, data = teacher_data),
  "Complete predicted deviation" = lm(hd ~ level + experiencia + antiguedad3 + female + edad +
                                        titularidad + indefinido + grupos_docencia + impacto_estudiantes +
                                        empatia + meritocracia + resource_skepticism_index, data = teacher_data)
)

modelsummary::modelsummary(
  appendix_models,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_map = c(
    "levelSecondary" = "Secondary education",
    "experiencia" = "Years teaching",
    "antiguedad3" = "Years in current school",
    "female1" = "Female",
    "edad" = "Age",
    "titularidadPublic" = "Public school",
    "indefinidoTemporary" = "Temporary contract",
    "grupos_docencia" = "Number of teaching groups",
    "impacto_estudiantes" = "Perceived impact on students",
    "empatia" = "Self-reported empathy",
    "meritocracia" = "Belief in effort",
    "resource_skepticism_index" = "Resource skepticism index"
  ),
  gof_omit = "AIC|BIC|Log.Lik.|RMSE",
  output = file.path(tables, "paper_rewrite_harshness_measures_appendix.tex")
)
clean_tex_table(file.path(tables, "paper_rewrite_harshness_measures_appendix.tex"))

# Random forest characterization with resource skepticism
dfrf <- teacher_data %>%
  transmute(
    primary = factor(ifelse(level == "Primary", 1, 0)),
    experience = ntile3_label(experiencia),
    tenure = ntile3_label(antiguedad3),
    age = ntile3_label(edad),
    groups = ntile3_label(grupos_docencia),
    student_impact = ntile3_label(impacto_estudiantes),
    meritocracy = ntile3_label(meritocracia),
    resource_skepticism = ntile3_label(resource_skepticism_index),
    high_empathy = factor(ifelse(empatia >= 5, 1, 0)),
    public = factor(ifelse(titularidad == "Public", 1, 0)),
    permanent = factor(ifelse(indefinido == "Permanent", 1, 0)),
    female,
    hb
  ) %>%
  drop_na()

rf_formula <- hb ~ primary + experience + tenure + age + groups + student_impact +
  meritocracy + resource_skepticism + high_empathy + public + permanent + female

rf_importance_list <- list()
rf_rmse <- c()

for (s in 1:50) {
  set.seed(s)
  rf_model <- ranger(
    formula = rf_formula,
    data = dfrf,
    importance = "permutation",
    num.trees = 1000,
    mtry = 3,
    min.node.size = 5,
    respect.unordered.factors = TRUE
  )
  rf_importance_list[[s]] <- data.frame(
    variable = names(rf_model$variable.importance),
    importance = as.numeric(rf_model$variable.importance)
  )
  rf_rmse[s] <- sqrt(rf_model$prediction.error)
}

rf_importance <- bind_rows(rf_importance_list) %>%
  group_by(variable) %>%
  summarise(
    mean_importance = mean(importance, na.rm = TRUE),
    se_importance = sd(importance, na.rm = TRUE),
    ci_low = mean_importance - 1.96 * se_importance,
    ci_high = mean_importance + 1.96 * se_importance,
    .groups = "drop"
  ) %>%
  mutate(
    variable = case_when(
      variable == "resource_skepticism" ~ "Resource skepticism",
      variable == "student_impact" ~ "Perceived impact on students",
      variable == "meritocracy" ~ "Belief in effort",
      variable == "experience" ~ "Years teaching",
      variable == "tenure" ~ "Years in current school",
      variable == "groups" ~ "Number of teaching groups",
      variable == "high_empathy" ~ "High empathy",
      variable == "primary" ~ "Primary education",
      variable == "public" ~ "Public school",
      variable == "permanent" ~ "Permanent contract",
      variable == "female" ~ "Female",
      variable == "age" ~ "Age",
      TRUE ~ variable
    )
  ) %>%
  arrange(desc(mean_importance))

write.csv(rf_importance, file.path(tables, "paper_rewrite_rf_importance.csv"), row.names = FALSE)

rf_importance %>%
  mutate(
    `Mean importance` = sprintf("%.4f", mean_importance),
    `Lower interval` = sprintf("%.4f", ci_low),
    `Upper interval` = sprintf("%.4f", ci_high)
  ) %>%
  select(Variable = variable, `Mean importance`, `Lower interval`, `Upper interval`) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  kableExtra::save_kable(file.path(tables, "paper_rewrite_rf_importance.tex"))
clean_tex_table(file.path(tables, "paper_rewrite_rf_importance.tex"))

data.frame(
  n = nrow(dfrf),
  mean_rmse = mean(rf_rmse),
  sd_hb = sd(dfrf$hb),
  rmse_over_sd = mean(rf_rmse) / sd(dfrf$hb)
) %>%
  write.csv(file.path(tables, "paper_rewrite_rf_summary.csv"), row.names = FALSE)

#===========================================#
#### 5. Descriptive card-level attributes ####
#===========================================#

df_within <- df_card_long %>%
  group_by(id) %>%
  mutate(
    y_within = repite - mean(repite, na.rm = TRUE),
    male_within = male - mean(male, na.rm = TRUE),
    complex_background_within = complex_background - mean(complex_background, na.rm = TRUE),
    failed_subjects_within = failed_subjects - mean(failed_subjects, na.rm = TRUE),
    low_competence_within = low_competence - mean(low_competence, na.rm = TRUE),
    absent_within = absent - mean(absent, na.rm = TRUE),
    disruptive_within = disruptive - mean(disruptive, na.rm = TRUE)
  ) %>%
  ungroup()

attr_model_demeaned_check <- lm(
  y_within ~ 0 + male_within + complex_background_within + failed_subjects_within +
    low_competence_within + absent_within + disruptive_within,
  data = df_within
)

attr_model <- fixest::feols(
  repite ~ male + complex_background + failed_subjects + low_competence + absent + disruptive | id,
  data = df_card_long,
  cluster = ~id
)

attr_terms <- as.data.frame(fixest::coeftable(attr_model)) %>%
  rownames_to_column("term") %>%
  as_tibble() %>%
  transmute(
    term,
    estimate = Estimate,
    std.error = `Std. Error`,
    statistic = `t value`,
    p.value = `Pr(>|t|)`
  ) %>%
  mutate(
    conf.low = estimate - qt(.975, df = n_distinct(df_card_long$id) - 1) * std.error,
    conf.high = estimate + qt(.975, df = n_distinct(df_card_long$id) - 1) * std.error,
    attribute = case_when(
      term == "male" ~ "Male student",
      term == "complex_background" ~ "Complex/migrant background",
      term == "failed_subjects" ~ "Three or more failed subjects",
      term == "low_competence" ~ "Low math/linguistic competence",
      term == "absent" ~ "Absenteeism",
      term == "disruptive" ~ "Disruptive behavior",
      TRUE ~ term
    )
  )

attr_equivalence_check <- tibble(
  attribute = c("male", "complex_background", "failed_subjects", "low_competence", "absent", "disruptive"),
  fixed_effects = coef(attr_model)[attribute],
  demeaned = coef(attr_model_demeaned_check)[paste0(attribute, "_within")]
) %>%
  mutate(abs_difference = abs(fixed_effects - demeaned))
write.csv(attr_equivalence_check, file.path(tables, "paper_rewrite_attribute_fe_equivalence_check.csv"), row.names = FALSE)

attr_terms %>%
  transmute(
    Attribute = attribute,
    Estimate = fmt_coef(estimate, p.value),
    `Std. Error` = fmt_se(std.error),
    `95\\% CI` = paste0("[", sprintf("%.3f", conf.low), ", ", sprintf("%.3f", conf.high), "]")
  ) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  kableExtra::save_kable(file.path(tables, "paper_rewrite_attribute_weights.tex"))
clean_tex_table(file.path(tables, "paper_rewrite_attribute_weights.tex"))

teacher_terciles <- df_harshness %>%
  mutate(
    harshness_group = case_when(
      hb <= quantile(hb, 1 / 3, na.rm = TRUE) ~ "Lenient tercile",
      hb >= quantile(hb, 2 / 3, na.rm = TRUE) ~ "Harsh tercile",
      TRUE ~ "Middle tercile"
    ),
    harshness_group = factor(harshness_group, levels = c("Lenient tercile", "Middle tercile", "Harsh tercile"))
  ) %>%
  select(id, harshness_group)

df_card_terciles <- df_card_long %>%
  left_join(teacher_terciles, by = "id")

group_terms <- list()
for (g in levels(df_card_terciles$harshness_group)) {
  group_data <- df_card_terciles %>% filter(harshness_group == g)
  group_model <- fixest::feols(
    repite ~ male + complex_background + failed_subjects + low_competence + absent + disruptive | id,
    data = group_data,
    cluster = ~id
  )
  group_terms[[g]] <- as.data.frame(fixest::coeftable(group_model)) %>%
    rownames_to_column("term") %>%
    as_tibble() %>%
    transmute(
      term,
      estimate = Estimate,
      std.error = `Std. Error`,
      statistic = `t value`,
      p.value = `Pr(>|t|)`,
      harshness_group = g
    )
}

group_terms <- bind_rows(group_terms) %>%
  mutate(
    attribute = case_when(
      term == "male" ~ "Male student",
      term == "complex_background" ~ "Complex/migrant background",
      term == "failed_subjects" ~ "Three or more failed subjects",
      term == "low_competence" ~ "Low math/linguistic competence",
      term == "absent" ~ "Absenteeism",
      term == "disruptive" ~ "Disruptive behavior",
      TRUE ~ term
    ),
    cell = fmt_coef(estimate, p.value)
  ) %>%
  select(attribute, harshness_group, cell) %>%
  pivot_wider(names_from = harshness_group, values_from = cell)

group_terms %>%
  rename(Attribute = attribute) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  kableExtra::save_kable(file.path(tables, "paper_rewrite_attribute_weights_by_harshness.tex"))
clean_tex_table(file.path(tables, "paper_rewrite_attribute_weights_by_harshness.tex"))

#======================================#
#### 6. Preregistered hypothesis models ####
#======================================#

teacher_data_1 <- teacher_data %>%
  filter(D %in% c("Control", "Policy treatment"))

model_h11 <- lm(hb ~ D, data = teacher_data_1)
model_h12a <- lm(hb ~ assigned, data = teacher_data_1)
model_h12 <- lm(hb ~ assigned + favorite, data = teacher_data_1)

teacher_data_h13 <- teacher_data_1 %>%
  filter(favorite == politica | least_favorite == politica | control == "Control") %>%
  mutate(
    assignation = case_when(
      favorite == politica & control == "Non-control" ~ "favorite",
      least_favorite == politica & control == "Non-control" ~ "least-favorite",
      control == "Control" ~ "Control"
    ),
    assignation = relevel(factor(assignation), ref = "Control")
  )

model_h13a <- lm(hb ~ assignation, data = teacher_data_h13)
model_h13 <- lm(hb ~ assignation + favorite, data = teacher_data_h13)

teacher_data_2 <- teacher_data %>%
  filter(D %in% c("Policy treatment", "Revelation treatment")) %>%
  mutate(D = relevel(factor(D), ref = "Policy treatment"))

model_h21 <- lm(hb ~ D, data = teacher_data_2)
model_h22a <- lm(hb ~ D + assigned, data = teacher_data_2)
model_h22b <- lm(hb ~ D + assigned + favorite, data = teacher_data_2)
model_h22 <- lm(hb ~ D + assigned + D:assigned + favorite, data = teacher_data_2)

teacher_data_h23_f <- teacher_data_2 %>% filter(favorite == politica)
teacher_data_h23_lf <- teacher_data_2 %>% filter(least_favorite == politica)
model_h23_f <- lm(hb ~ D + favorite, data = teacher_data_h23_f)
model_h23_lf <- lm(hb ~ D + favorite, data = teacher_data_h23_lf)

teacher_data_3 <- teacher_data %>%
  filter(D %in% c("Revelation treatment", "Awareness treatment")) %>%
  mutate(D = relevel(factor(D), ref = "Revelation treatment"))

model_h31 <- lm(hb ~ D, data = teacher_data_3)
model_h32a <- lm(hb ~ D + assigned, data = teacher_data_3)
model_h32b <- lm(hb ~ D + assigned + favorite, data = teacher_data_3)
model_h32 <- lm(hb ~ D + assigned + D:assigned + favorite, data = teacher_data_3)

teacher_data_h33_f <- teacher_data_3 %>% filter(favorite == politica)
teacher_data_h33_lf <- teacher_data_3 %>% filter(least_favorite == politica)
model_h33_f <- lm(hb ~ D + favorite, data = teacher_data_h33_f)
model_h33_lf <- lm(hb ~ D + favorite, data = teacher_data_h33_lf)

modelsummary::modelsummary(
  list("H1|1" = model_h11, "(2)" = model_h12a, "H1|2" = model_h12),
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_omit = "AIC|BIC|Log.Lik.|RMSE",
  output = file.path(tables, "paper_rewrite_h1_agg.tex")
)
clean_tex_table(file.path(tables, "paper_rewrite_h1_agg.tex"))

teacher_data_1_all <- teacher_data %>%
  mutate(
    any_treatment = factor(ifelse(D == "Control", "Control", "Any treatment"),
                           levels = c("Control", "Any treatment"))
  )

model_h11_all <- lm(hb ~ any_treatment, data = teacher_data_1_all)
model_h12_all <- lm(hb ~ assigned + favorite, data = teacher_data_1_all)

teacher_data_h13_all <- teacher_data_1_all %>%
  filter(favorite == politica | least_favorite == politica | control == "Control") %>%
  mutate(
    assignation = case_when(
      favorite == politica & control == "Non-control" ~ "favorite",
      least_favorite == politica & control == "Non-control" ~ "least-favorite",
      control == "Control" ~ "Control"
    ),
    assignation = relevel(factor(assignation), ref = "Control")
  )

model_h13_all <- lm(hb ~ assignation + favorite, data = teacher_data_h13_all)

modelsummary::modelsummary(
  list("Any treatment" = model_h11_all,
       "Assigned policy" = model_h12_all,
       "Alignment" = model_h13_all),
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_omit = "AIC|BIC|Log.Lik.|RMSE",
  output = file.path(tables, "paper_rewrite_study1_all_treatments.tex")
)
clean_tex_table(file.path(tables, "paper_rewrite_study1_all_treatments.tex"))

modelsummary::modelsummary(
  list("(1)" = model_h13a, "H1|3" = model_h13),
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_omit = "AIC|BIC|Log.Lik.|RMSE",
  output = file.path(tables, "paper_rewrite_h13.tex")
)
clean_tex_table(file.path(tables, "paper_rewrite_h13.tex"))

modelsummary::modelsummary(
  list("H2|1" = model_h21, "(2)" = model_h22a, "(3)" = model_h22b, "H2|2" = model_h22),
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_omit = "AIC|BIC|Log.Lik.|RMSE",
  output = file.path(tables, "paper_rewrite_h2_agg.tex")
)
clean_tex_table(file.path(tables, "paper_rewrite_h2_agg.tex"))

modelsummary::modelsummary(
  list("Favorite policy" = model_h23_f, "Least favorite policy" = model_h23_lf),
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_omit = "AIC|BIC|Log.Lik.|RMSE",
  output = file.path(tables, "paper_rewrite_h23.tex")
)
clean_tex_table(file.path(tables, "paper_rewrite_h23.tex"))

modelsummary::modelsummary(
  list("H3|1" = model_h31, "(2)" = model_h32a, "(3)" = model_h32b, "H3|2" = model_h32),
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_omit = "AIC|BIC|Log.Lik.|RMSE",
  output = file.path(tables, "paper_rewrite_h3_agg.tex")
)
clean_tex_table(file.path(tables, "paper_rewrite_h3_agg.tex"))

modelsummary::modelsummary(
  list("Favorite policy" = model_h33_f, "Least favorite policy" = model_h33_lf),
  stars = c("*" = .1, "**" = .05, "***" = .01),
  gof_omit = "AIC|BIC|Log.Lik.|RMSE",
  output = file.path(tables, "paper_rewrite_h33.tex")
)
clean_tex_table(file.path(tables, "paper_rewrite_h33.tex"))

h22_contrasts <- as.data.frame(emmeans(model_h22, pairwise ~ D | assigned)$contrasts)
h32_contrasts <- as.data.frame(emmeans(model_h32, pairwise ~ D | assigned)$contrasts)

h22_contrasts %>%
  mutate(estimate = fmt_coef(estimate, p.value),
         SE = sprintf("%.3f", SE),
         p.value = sprintf("%.3f", p.value)) %>%
  select(assigned, contrast, estimate, SE, p.value) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, escape = FALSE,
                  col.names = c("Assigned policy", "Contrast", "Estimate", "SE", "p-value")) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  kableExtra::save_kable(file.path(tables, "paper_rewrite_h22_contrasts.tex"))
clean_tex_table(file.path(tables, "paper_rewrite_h22_contrasts.tex"))

h32_contrasts %>%
  mutate(estimate = fmt_coef(estimate, p.value),
         SE = sprintf("%.3f", SE),
         p.value = sprintf("%.3f", p.value)) %>%
  select(assigned, contrast, estimate, SE, p.value) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, escape = FALSE,
                  col.names = c("Assigned policy", "Contrast", "Estimate", "SE", "p-value")) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  kableExtra::save_kable(file.path(tables, "paper_rewrite_h32_contrasts.tex"))
clean_tex_table(file.path(tables, "paper_rewrite_h32_contrasts.tex"))

#===============================================#
#### 7. MDE and equivalence for teacher models ####
#===============================================#

coef_sensitivity <- function(model, term, label) {
  td <- broom::tidy(model)
  row <- td %>% filter(term == !!term)
  dfree <- df.residual(model)
  est <- row$estimate
  se <- row$std.error
  data.frame(
    hypothesis = label,
    estimate = est,
    std_error = se,
    p_value = row$p.value,
    ci_low = est - qt(.975, dfree) * se,
    ci_high = est + qt(.975, dfree) * se,
    mde_80 = (qt(.975, dfree) + qt(.8, dfree)) * se
  )
}

contrast_sensitivity <- function(contrast_df, label_prefix) {
  contrast_df %>%
    transmute(
      hypothesis = paste0(label_prefix, ": ", assigned),
      estimate = estimate,
      std_error = SE,
      p_value = p.value,
      ci_low = estimate - qt(.975, df) * SE,
      ci_high = estimate + qt(.975, df) * SE,
      mde_80 = (qt(.975, df) + qt(.8, df)) * SE
    )
}

sensitivity <- bind_rows(
  coef_sensitivity(model_h11, "DPolicy treatment", "H1|1: Policy treatment vs Control"),
  coef_sensitivity(model_h12, "assignedPolicy 1", "H1|2: assigned Policy 1 vs Control"),
  coef_sensitivity(model_h12, "assignedPolicy 2", "H1|2: assigned Policy 2 vs Control"),
  coef_sensitivity(model_h12, "assignedPolicy 3", "H1|2: assigned Policy 3 vs Control"),
  coef_sensitivity(model_h13, "assignationfavorite", "H1|3: assigned favorite vs Control"),
  coef_sensitivity(model_h13, "assignationleast-favorite", "H1|3: assigned least favorite vs Control"),
  coef_sensitivity(model_h21, "DRevelation treatment", "H2|1: Revelation vs Policy"),
  contrast_sensitivity(h22_contrasts, "H2|2: Revelation vs Policy"),
  coef_sensitivity(model_h23_f, "DRevelation treatment", "H2|3: favorite assigned, Revelation vs Policy"),
  coef_sensitivity(model_h23_lf, "DRevelation treatment", "H2|3: least favorite assigned, Revelation vs Policy"),
  coef_sensitivity(model_h31, "DAwareness treatment", "H3|1: Awareness vs Revelation"),
  contrast_sensitivity(h32_contrasts, "H3|2: Awareness vs Revelation"),
  coef_sensitivity(model_h33_f, "DAwareness treatment", "H3|3: favorite assigned, Awareness vs Revelation"),
  coef_sensitivity(model_h33_lf, "DAwareness treatment", "H3|3: least favorite assigned, Awareness vs Revelation")
)

sensitivity_equiv <- sensitivity %>%
  crossing(equiv_margin = c(.02, .03, .05)) %>%
  mutate(
    p_lower = pt((estimate + equiv_margin) / std_error, df = 100000, lower.tail = FALSE),
    p_upper = pt((estimate - equiv_margin) / std_error, df = 100000, lower.tail = TRUE),
    tost_p = pmax(p_lower, p_upper),
    equivalent = tost_p < .05
  )

write.csv(sensitivity, file.path(tables, "paper_rewrite_teacher_mde.csv"), row.names = FALSE)
write.csv(sensitivity_equiv, file.path(tables, "paper_rewrite_teacher_equivalence.csv"), row.names = FALSE)

sensitivity %>%
  mutate(
    Estimate = fmt_coef(estimate, p_value),
    `Std. Error` = fmt_se(std_error),
    `95\\% CI` = paste0("[", sprintf("%.3f", ci_low), ", ", sprintf("%.3f", ci_high), "]"),
    `MDE 80\\%` = sprintf("%.3f", mde_80)
  ) %>%
  select(Hypothesis = hypothesis, Estimate, `Std. Error`, `95\\% CI`, `MDE 80\\%`) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::save_kable(file.path(tables, "paper_rewrite_teacher_mde.tex"))
clean_tex_table(file.path(tables, "paper_rewrite_teacher_mde.tex"))

sensitivity_equiv %>%
  filter(equiv_margin == .03) %>%
  mutate(
    Estimate = sprintf("%.3f", estimate),
    `TOST p` = sprintf("%.3f", tost_p),
    Equivalent = ifelse(equivalent, "Yes", "No"),
    Margin = paste0("+/-", sprintf("%.2f", equiv_margin))
  ) %>%
  select(Hypothesis = hypothesis, Margin, Estimate, `TOST p`, Equivalent) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::save_kable(file.path(tables, "paper_rewrite_teacher_equivalence.tex"))
clean_tex_table(file.path(tables, "paper_rewrite_teacher_equivalence.tex"))

#======================================#
#### 8. Additional preregistered H4 ####
#======================================#

teacher_data_42 <- teacher_data %>% filter(D == "Policy treatment")

h42_models <- list()
for (x in 1:3) {
  policy_name <- paste0("Policy ", x)
  tmp <- teacher_data_42 %>%
    mutate(
      fav = as.numeric(favorite_num == x),
      least_fav = as.numeric(least_favorite_num == x),
      assigned_x = as.numeric(politica == policy_name)
    )
  h42_models[[paste0(policy_name, " favorite")]] <- glm(fav ~ assigned_x, data = tmp, family = binomial)
  h42_models[[paste0(policy_name, " least favorite")]] <- glm(least_fav ~ assigned_x, data = tmp, family = binomial)
}

modelsummary::modelsummary(
  h42_models,
  stars = c("*" = .1, "**" = .05, "***" = .01),
  coef_map = c("assigned_x" = "Assigned to policy"),
  gof_omit = "AIC|BIC|Log.Lik.|RMSE",
  output = file.path(tables, "paper_rewrite_h42.tex")
)
clean_tex_table(file.path(tables, "paper_rewrite_h42.tex"))

h43_favorite <- teacher_data %>%
  drop_na(favorite) %>%
  group_by(D, favorite) %>%
  summarise(n_policy = n(), .groups = "drop_last") %>%
  mutate(
    n_arm = sum(n_policy),
    share = n_policy / n_arm,
    se = sqrt(share * (1 - share) / n_arm),
    ci_low = share - 1.96 * se,
    ci_high = share + 1.96 * se,
    preference = "Favorite"
  ) %>%
  ungroup() %>%
  rename(policy = favorite)

h43_least <- teacher_data %>%
  drop_na(least_favorite) %>%
  group_by(D, least_favorite) %>%
  summarise(n_policy = n(), .groups = "drop_last") %>%
  mutate(
    n_arm = sum(n_policy),
    share = n_policy / n_arm,
    se = sqrt(share * (1 - share) / n_arm),
    ci_low = share - 1.96 * se,
    ci_high = share + 1.96 * se,
    preference = "Least favorite"
  ) %>%
  ungroup() %>%
  rename(policy = least_favorite)

bind_rows(h43_favorite, h43_least) %>%
  mutate(
    Share = paste0(sprintf("%.1f", 100 * share), "\\%"),
    `95\\% CI` = paste0("[", sprintf("%.1f", 100 * ci_low), ", ", sprintf("%.1f", 100 * ci_high), "]")
  ) %>%
  select(Preference = preference, Treatment = D, Policy = policy, Share, `95\\% CI`) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  kableExtra::save_kable(file.path(tables, "paper_rewrite_h43_preference_shares.tex"))
clean_tex_table(file.path(tables, "paper_rewrite_h43_preference_shares.tex"))

#=========================================#
#### 9. Extra diagnostic report data ####
#=========================================#

duration_vars <- names(df)[grepl("(_ps$|_sp$)", names(df))]
card_time_vars <- names(df)[grepl("^alumno_.*_ps$", names(df))]

timing_data <- df %>%
  mutate(
    total_duration_seconds = duration,
    total_page_seconds = rowSums(across(all_of(duration_vars)), na.rm = TRUE),
    card_page_seconds = rowSums(across(all_of(card_time_vars)), na.rm = TRUE),
    n_page_times = rowSums(!is.na(across(all_of(duration_vars)))),
    flagged_prereg_time_trim = percent_rank(total_duration_seconds) <= .0175 |
      percent_rank(total_duration_seconds) >= .9825
  )

timing_summary <- timing_data %>%
  summarise(
    n = sum(!is.na(total_duration_seconds)),
    mean_minutes = mean(total_duration_seconds, na.rm = TRUE) / 60,
    sd_minutes = sd(total_duration_seconds, na.rm = TRUE) / 60,
    p1_minutes = quantile(total_duration_seconds, .01, na.rm = TRUE) / 60,
    p1_75_minutes = quantile(total_duration_seconds, .0175, na.rm = TRUE) / 60,
    p5_minutes = quantile(total_duration_seconds, .05, na.rm = TRUE) / 60,
    p25_minutes = quantile(total_duration_seconds, .25, na.rm = TRUE) / 60,
    median_minutes = quantile(total_duration_seconds, .50, na.rm = TRUE) / 60,
    p75_minutes = quantile(total_duration_seconds, .75, na.rm = TRUE) / 60,
    p95_minutes = quantile(total_duration_seconds, .95, na.rm = TRUE) / 60,
    p98_25_minutes = quantile(total_duration_seconds, .9825, na.rm = TRUE) / 60,
    p99_minutes = quantile(total_duration_seconds, .99, na.rm = TRUE) / 60,
    max_minutes = max(total_duration_seconds, na.rm = TRUE) / 60,
    n_prereg_trim = sum(flagged_prereg_time_trim, na.rm = TRUE)
  )

timing_by_trim <- timing_data %>%
  mutate(time_group = case_when(
    percent_rank(total_duration_seconds) <= .0175 ~ "Fastest 1.75%",
    percent_rank(total_duration_seconds) >= .9825 ~ "Slowest 1.75%",
    TRUE ~ "Middle 96.5%"
  )) %>%
  group_by(time_group) %>%
  summarise(
    n = n(),
    mean_total_minutes = mean(total_duration_seconds, na.rm = TRUE) / 60,
    median_total_minutes = median(total_duration_seconds, na.rm = TRUE) / 60,
    mean_card_minutes = mean(card_page_seconds, na.rm = TRUE) / 60,
    median_card_minutes = median(card_page_seconds, na.rm = TRUE) / 60,
    mean_hb = mean(teacher_data$hb[match(id, teacher_data$id)], na.rm = TRUE),
    .groups = "drop"
  )

write.csv(timing_summary, file.path(tables, "paper_rewrite_timing_summary.csv"), row.names = FALSE)
write.csv(timing_by_trim, file.path(tables, "paper_rewrite_timing_by_trim.csv"), row.names = FALSE)

timing_by_trim %>%
  mutate(
    `Mean total minutes` = sprintf("%.1f", mean_total_minutes),
    `Median total minutes` = sprintf("%.1f", median_total_minutes),
    `Mean card minutes` = sprintf("%.1f", mean_card_minutes),
    `Mean harshness` = sprintf("%.3f", mean_hb)
  ) %>%
  select(Group = time_group, N = n, `Mean total minutes`, `Median total minutes`,
         `Mean card minutes`, `Mean harshness`) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kableExtra::kable_styling(latex_options = c("hold_position")) %>%
  kableExtra::save_kable(file.path(tables, "paper_rewrite_timing_by_trim.tex"))
clean_tex_table(file.path(tables, "paper_rewrite_timing_by_trim.tex"))

prereg_mde <- data.frame(
  n = c(2500, 3500, 4500, 5500, 6500, 7500),
  prereg_mde_sd = c(.24, .20, .18, .16, .15, .14)
)

actual_mde_standardized <- sensitivity %>%
  mutate(
    actual_mde_sd = mde_80 / sd(teacher_data$hb, na.rm = TRUE),
    n_actual = case_when(
      grepl("^H1", hypothesis) ~ nrow(teacher_data_1),
      grepl("^H2", hypothesis) ~ nrow(teacher_data_2),
      grepl("^H3", hypothesis) ~ nrow(teacher_data_3),
      TRUE ~ nrow(teacher_data)
    )
  )

write.csv(actual_mde_standardized, file.path(tables, "paper_rewrite_actual_mde_standardized.csv"), row.names = FALSE)

study1_all_terms <- broom::tidy(model_h11_all) %>%
  bind_rows(broom::tidy(model_h12_all) %>% mutate(model = "assigned_policy")) %>%
  bind_rows(broom::tidy(model_h13_all) %>% mutate(model = "alignment"))
write.csv(study1_all_terms, file.path(tables, "paper_rewrite_study1_all_treatments.csv"), row.names = FALSE)

timing_by_trim_md <- timing_by_trim %>%
  mutate(
    n = format(n, big.mark = ","),
    mean_total_minutes = sprintf("%.1f", mean_total_minutes),
    median_total_minutes = sprintf("%.1f", median_total_minutes),
    mean_card_minutes = sprintf("%.1f", mean_card_minutes),
    median_card_minutes = sprintf("%.1f", median_card_minutes),
    mean_hb = ifelse(is.na(mean_hb), "NA", sprintf("%.3f", mean_hb))
  )

timing_by_trim_lines <- c(
  "| Grupo | N | Media total (min) | Mediana total (min) | Media tarjetas (min) | Mediana tarjetas (min) | Harshness media |",
  "|---|---:|---:|---:|---:|---:|---:|",
  paste0(
    "| ", timing_by_trim_md$time_group,
    " | ", timing_by_trim_md$n,
    " | ", timing_by_trim_md$mean_total_minutes,
    " | ", timing_by_trim_md$median_total_minutes,
    " | ", timing_by_trim_md$mean_card_minutes,
    " | ", timing_by_trim_md$median_card_minutes,
    " | ", timing_by_trim_md$mean_hb,
    " |"
  )
)

study1_h12_all_terms <- broom::tidy(model_h12_all) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    estimate_txt = sprintf("%.3f", estimate),
    p_txt = sprintf("%.3f", p.value)
  )

study1_h13_all_terms <- broom::tidy(model_h13_all) %>%
  filter(term %in% c("assignationfavorite", "assignationleast-favorite")) %>%
  mutate(
    estimate_txt = sprintf("%.3f", estimate),
    p_txt = sprintf("%.3f", p.value)
  )

report_lines <- c(
  "# Informe extra para Ignacio",
  "",
  "## 1. Coherencia entre MDE del paper y MDE del prerregistro",
  "",
  paste0("El prerregistro reportaba MDEs en desviaciones estandar: ",
         paste0(prereg_mde$n, " profesores = ", prereg_mde$prereg_mde_sd, " SD", collapse = "; "), "."),
  "",
  paste0("El paper actual calcula MDEs post-estimacion para cada contraste preregistrado usando el error estandar observado. ",
         "Es conceptualmente coherente con el prerregistro porque ambos calculos preguntan que tamano de efecto se podia detectar con 80% de potencia, ",
         "pero no son numericamente identicos porque el prerregistro era ex ante, en SD, con R2 = 0.1, SD = 0.10 y 10 brazos; el paper usa SEs observados, muestras por contraste y unidades de harshness."),
  "",
  paste0("La desviacion estandar observada de hb es ", sprintf("%.3f", sd(teacher_data$hb, na.rm = TRUE)), ". ",
         "Los MDEs estandarizados actuales para los contrastes principales son: H1|1 = ",
         sprintf("%.2f", actual_mde_standardized$actual_mde_sd[actual_mde_standardized$hypothesis == "H1|1: Policy treatment vs Control"]),
         " SD; H2|1 = ",
         sprintf("%.2f", actual_mde_standardized$actual_mde_sd[actual_mde_standardized$hypothesis == "H2|1: Revelation vs Policy"]),
         " SD; H3|1 = ",
         sprintf("%.2f", actual_mde_standardized$actual_mde_sd[actual_mde_standardized$hypothesis == "H3|1: Awareness vs Revelation"]),
         " SD."),
  "",
  "Mi lectura: los MDEs actuales estan en el mismo orden de magnitud que los del prerregistro para comparaciones amplias, aunque algo mas grandes para algunos contrastes especificos de policy/alignment. Tiene sentido presentarlos como sensibilidad, no como reemplazo del power analysis ex ante.",
  "",
  "## 2. Study 1 ampliado: Control vs todos los tratamientos",
  "",
  paste0("Cuando se agrupan Policy treatment, Revelation treatment y Awareness treatment como un unico grupo tratado, el efecto de Any treatment vs Control es ",
         sprintf("%.3f", coef(model_h11_all)[["any_treatmentAny treatment"]]),
         " (p = ", sprintf("%.3f", broom::tidy(model_h11_all)$p.value[broom::tidy(model_h11_all)$term == "any_treatmentAny treatment"]), ")."),
  "",
  paste0("H1|2 ampliada recalcula policy asignada controlando por favorite policy. Los coeficientes principales de assigned policy son: ",
         paste0(study1_h12_all_terms$term[grepl("^assigned", study1_h12_all_terms$term)],
                " = ", study1_h12_all_terms$estimate_txt[grepl("^assigned", study1_h12_all_terms$term)],
                " (p = ", study1_h12_all_terms$p_txt[grepl("^assigned", study1_h12_all_terms$term)], ")",
                collapse = "; "),
         "."),
  "",
  paste0("H1|3 ampliada recalcula alignment. Los coeficientes de alignment son: ",
         paste0(study1_h13_all_terms$term,
                " = ", study1_h13_all_terms$estimate_txt,
                " (p = ", study1_h13_all_terms$p_txt, ")",
                collapse = "; "),
         "."),
  "",
  "Esto no cambia la historia: con mas power al juntar los tres brazos, sigue sin aparecer un efecto medio de estar en cualquier tratamiento frente a Control ni un efecto claro de alignment.",
  "",
  "La tabla completa esta en `3. Output/tables/paper_rewrite_study1_all_treatments.tex` y el CSV en `3. Output/tables/paper_rewrite_study1_all_treatments.csv`.",
  "",
  "## 3. Tiempos de encuesta",
  "",
  paste0("Duracion total mediana: ", sprintf("%.1f", timing_summary$median_minutes), " minutos. ",
         "Percentil 1.75: ", sprintf("%.1f", timing_summary$p1_75_minutes), " minutos. ",
         "Percentil 98.25: ", sprintf("%.1f", timing_summary$p98_25_minutes), " minutos. ",
         "Maximo: ", sprintf("%.1f", timing_summary$max_minutes), " minutos."),
  "",
  paste0("La regla preregistrada de quitar 1.75% mas rapidos y 1.75% mas lentos marcaria ",
         timing_summary$n_prereg_trim, " observaciones."),
  "",
  timing_by_trim_lines,
  "",
  "La cola rapida no parece formar parte de la muestra analitica de tarjetas: su tiempo medio y mediano en tarjetas es cero, y por eso no aparece harshness media. La cola lenta, en cambio, si parece haber llegado al task, pero la duracion total es desproporcionada y probablemente refleja encuestas dejadas abiertas.",
  "",
  "Mi lectura: la cola lenta es claramente compatible con gente que dejo la encuesta abierta y volvio despues. La cola rapida debe excluirse si no completa el task. La regla preregistrada de trim simetrico es defendible como limpieza mecanica, pero conviene reportar robustez con y sin trim si los resultados principales cambian."
)

writeLines(report_lines, file.path("3. Output/reports", "paper_rewrite_extra_diagnostics.md"))

message("Finished paper rewrite analysis.")
