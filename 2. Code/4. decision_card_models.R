#============================================================#
#### Card-level models, equivalence tests, and belief index ####
#============================================================#

# This script is exploratory and complements the preregistered teacher-level
# analyses. It keeps the current clean sample and moves the outcome to the
# teacher-card decision level.

suppressPackageStartupMessages({
  source("2. Code/0. main.R")
})

dir.create(file.path(output, "reports"), recursive = TRUE, showWarnings = FALSE)

card_out_prefix <- "card_level"

#-----------------------------#
#### 1. Helper functions ####
#-----------------------------#

std_z <- function(x) {
  x <- as.numeric(x)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

row_mean_min <- function(data, vars, min_nonmissing = 1) {
  mat <- as.matrix(data[, vars, drop = FALSE])
  n_ok <- rowSums(!is.na(mat))
  out <- rowMeans(mat, na.rm = TRUE)
  out[n_ok < min_nonmissing] <- NA_real_
  out
}

cronbach_alpha <- function(data, vars) {
  mat <- as.matrix(data[, vars, drop = FALSE])
  mat <- mat[stats::complete.cases(mat), , drop = FALSE]
  k <- ncol(mat)
  if (nrow(mat) < 3 || k < 2) return(NA_real_)
  total_var <- stats::var(rowSums(mat))
  if (is.na(total_var) || total_var == 0) return(NA_real_)
  k / (k - 1) * (1 - sum(apply(mat, 2, stats::var)) / total_var)
}

coef_no_na <- function(model) {
  beta <- stats::coef(model)
  beta[!is.na(beta)]
}

vcov_cluster_lm <- function(model, data, cluster_var) {
  beta <- coef_no_na(model)
  keep <- names(beta)
  X <- stats::model.matrix(model)[, keep, drop = FALSE]
  u <- stats::residuals(model)
  cluster <- data[[cluster_var]]
  if (!is.null(model$na.action)) {
    cluster <- cluster[-as.integer(model$na.action)]
  }
  cluster <- as.factor(cluster)

  n <- nrow(X)
  k <- ncol(X)
  g <- nlevels(cluster)

  bread <- qr.solve(crossprod(X))
  meat <- matrix(0, nrow = k, ncol = k)

  split_idx <- split(seq_len(n), cluster)
  for (idx in split_idx) {
    Xg <- X[idx, , drop = FALSE]
    ug <- u[idx]
    score_g <- crossprod(Xg, ug)
    meat <- meat + score_g %*% t(score_g)
  }

  adj <- (g / (g - 1)) * ((n - 1) / (n - k))
  out <- adj * bread %*% meat %*% bread
  dimnames(out) <- list(keep, keep)
  out
}

tidy_cluster_lm <- function(model, data, cluster_var, conf_level = 0.95) {
  beta <- coef_no_na(model)
  vc <- vcov_cluster_lm(model, data, cluster_var)
  se <- sqrt(diag(vc))
  cluster <- data[[cluster_var]]
  if (!is.null(model$na.action)) {
    cluster <- cluster[-as.integer(model$na.action)]
  }
  df <- length(unique(cluster)) - 1
  crit <- stats::qt(1 - (1 - conf_level) / 2, df = df)
  tibble::tibble(
    term = names(beta),
    estimate = as.numeric(beta),
    std.error = as.numeric(se),
    statistic = estimate / std.error,
    p.value = 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE),
    conf.low = estimate - crit * std.error,
    conf.high = estimate + crit * std.error,
    df = df,
    n_obs = stats::nobs(model)
  )
}

linear_combo_lm <- function(model, data, cluster_var, weights, label) {
  beta <- coef_no_na(model)
  vc <- vcov_cluster_lm(model, data, cluster_var)

  missing_terms <- setdiff(names(weights), names(beta))
  if (length(missing_terms) > 0) {
    stop("Missing terms in model: ", paste(missing_terms, collapse = ", "))
  }

  L <- rep(0, length(beta))
  names(L) <- names(beta)
  L[names(weights)] <- weights

  est <- sum(L * beta)
  se <- sqrt(drop(t(L) %*% vc %*% L))
  cluster <- data[[cluster_var]]
  if (!is.null(model$na.action)) {
    cluster <- cluster[-as.integer(model$na.action)]
  }
  df <- length(unique(cluster)) - 1
  crit <- stats::qt(0.975, df = df)

  tibble::tibble(
    contrast = label,
    estimate = est,
    std.error = se,
    statistic = est / se,
    p.value = 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE),
    conf.low = est - crit * se,
    conf.high = est + crit * se,
    df = df
  )
}

tidy_lm_default <- function(model, conf_level = 0.95) {
  broom::tidy(model, conf.int = TRUE, conf.level = conf_level) %>%
    dplyr::mutate(df = stats::df.residual(model), n_obs = stats::nobs(model))
}

linear_combo_default <- function(model, weights, label) {
  beta <- coef_no_na(model)
  vc <- stats::vcov(model)[names(beta), names(beta), drop = FALSE]

  missing_terms <- setdiff(names(weights), names(beta))
  if (length(missing_terms) > 0) {
    stop("Missing terms in model: ", paste(missing_terms, collapse = ", "))
  }

  L <- rep(0, length(beta))
  names(L) <- names(beta)
  L[names(weights)] <- weights

  est <- sum(L * beta)
  se <- sqrt(drop(t(L) %*% vc %*% L))
  df <- stats::df.residual(model)
  crit <- stats::qt(0.975, df = df)

  tibble::tibble(
    contrast = label,
    estimate = est,
    std.error = se,
    statistic = est / se,
    p.value = 2 * stats::pt(abs(statistic), df = df, lower.tail = FALSE),
    conf.low = est - crit * se,
    conf.high = est + crit * se,
    df = df
  )
}

add_mde <- function(tbl, power = 0.80, alpha = 0.05) {
  tbl %>%
    dplyr::mutate(
      mde_80 = (stats::qt(1 - alpha / 2, df = df) +
                  stats::qt(power, df = df)) * std.error
    )
}

equivalence_grid <- function(tbl, margins, alpha = 0.05) {
  tidyr::crossing(tbl, tibble::tibble(equiv_margin = margins)) %>%
    dplyr::mutate(
      p_lower = 1 - stats::pt((estimate + equiv_margin) / std.error, df = df),
      p_upper = stats::pt((estimate - equiv_margin) / std.error, df = df),
      tost_p = pmax(p_lower, p_upper),
      ci90_low = estimate - stats::qt(1 - alpha, df = df) * std.error,
      ci90_high = estimate + stats::qt(1 - alpha, df = df) * std.error,
      equivalent = ci90_low > -equiv_margin & ci90_high < equiv_margin
    )
}

nice_write_csv <- function(x, file) {
  readr::write_csv(x, file.path(tables, file))
}

#---------------------------------------#
#### 2. Build teacher and card data ####
#---------------------------------------#

df <- arrow::read_parquet(cleandata)

decision_cards <- setdiff(namelist, c("alumno_1_1", "alumno_2_1"))

teacher_data <- df %>%
  dplyr::mutate(
    treatment_arm = dplyr::case_when(
      treatment == 1 ~ "Control",
      treatment %in% 2:4 ~ "Policy treatment",
      treatment %in% 5:7 ~ "Revelation treatment",
      treatment %in% 8:10 ~ "Awareness treatment",
      TRUE ~ NA_character_
    ),
    treatment_arm = factor(
      treatment_arm,
      levels = c("Control", "Policy treatment", "Revelation treatment", "Awareness treatment")
    ),
    assigned_policy = dplyr::case_when(
      is.na(treatment_arm) ~ NA_character_,
      treatment_arm == "Control" ~ "Control",
      politica == 1 ~ "Reinforcement",
      politica == 2 ~ "Promotion criteria",
      politica == 3 ~ "Training",
      TRUE ~ NA_character_
    ),
    assigned_policy = factor(
      assigned_policy,
      levels = c("Control", "Reinforcement", "Promotion criteria", "Training")
    ),
    favorite_policy = dplyr::case_when(
      orden_pref_refuerzo == 1 ~ "Reinforcement",
      orden_pref_criterios_promo == 1 ~ "Promotion criteria",
      orden_pref_formacion_prof == 1 ~ "Training",
      TRUE ~ NA_character_
    ),
    favorite_policy = factor(
      favorite_policy,
      levels = c("Reinforcement", "Promotion criteria", "Training")
    ),
    least_favorite_policy = dplyr::case_when(
      orden_pref_refuerzo == 3 ~ "Reinforcement",
      orden_pref_criterios_promo == 3 ~ "Promotion criteria",
      orden_pref_formacion_prof == 3 ~ "Training",
      TRUE ~ NA_character_
    ),
    least_favorite_policy = factor(
      least_favorite_policy,
      levels = c("Reinforcement", "Promotion criteria", "Training")
    ),
    policy_alignment = dplyr::case_when(
      treatment_arm == "Control" ~ "Control",
      as.character(assigned_policy) == as.character(favorite_policy) ~ "Favorite assigned",
      as.character(assigned_policy) == as.character(least_favorite_policy) ~ "Least favorite assigned",
      !is.na(assigned_policy) ~ "Middle assigned",
      TRUE ~ NA_character_
    ),
    policy_alignment = factor(
      policy_alignment,
      levels = c("Control", "Favorite assigned", "Middle assigned", "Least favorite assigned")
    ),
    permanent = factor(
      dplyr::if_else(
        sitlabpub %in% c("Contratada/o laboral indefinido", "Funcionaria/o con destino definitivo"),
        "Permanent", "Temporary",
        missing = NA_character_
      )
    ),
    school_type = dplyr::case_when(
      titularidad == "Publica" ~ "Public",
      titularidad == "Pública" ~ "Public",
      titularidad %in% c("Privada", "Concertada") ~ "Private/charter",
      TRUE ~ as.character(titularidad)
    ),
    school_type = factor(school_type),
    grade_level = factor(nivel)
  ) %>%
  dplyr::mutate(
    impact_students = dplyr::coalesce(impacto_centro_estudiantes, impacto_region_estudiantes),
    pass_without_competence = dplyr::coalesce(
      impacto_centro_pasar_sin_competencias,
      impacto_region_pasar_sin_competencias
    ),
    prepared_next_level = dplyr::coalesce(
      impacto_centro_preparados_nivel_sig,
      impacto_region_preparados_nivel_sig
    ),
    too_many_repeater_resources = dplyr::coalesce(
      impacto_centro_demasiados_recursos_repetidores,
      impacto_region_demasiados_recursos_repetidores
    ),
    repeater_resources_ineffective = dplyr::coalesce(
      impacto_centro_recursos_repetidores_ineficaces,
      impacto_region_recursos_repetidores_ineficaces
    )
  ) %>%
  dplyr::mutate(
    z_meritocracy = std_z(meritocracia),
    z_blame_students = std_z(pct_culpa_alumnos),
    z_blame_system_rev = -std_z(pct_culpa_sistema_educativo),
    z_pass_without_competence = std_z(pass_without_competence),
    z_prepared_next_level_rev = -std_z(prepared_next_level),
    z_too_many_resources = std_z(too_many_repeater_resources),
    z_resources_ineffective = std_z(repeater_resources_ineffective),
    resource_skepticism_index = row_mean_min(
      dplyr::pick(z_too_many_resources, z_resources_ineffective),
      c("z_too_many_resources", "z_resources_ineffective"),
      min_nonmissing = 2
    ),
    academic_standards_index = row_mean_min(
      dplyr::pick(z_pass_without_competence, z_prepared_next_level_rev),
      c("z_pass_without_competence", "z_prepared_next_level_rev"),
      min_nonmissing = 2
    ),
    student_attribution_index = row_mean_min(
      dplyr::pick(z_blame_students, z_blame_system_rev),
      c("z_blame_students", "z_blame_system_rev"),
      min_nonmissing = 2
    ),
    responsibility_index = row_mean_min(
      dplyr::pick(z_meritocracy, z_blame_students, z_blame_system_rev),
      c("z_meritocracy", "z_blame_students", "z_blame_system_rev"),
      min_nonmissing = 2
    ),
    remediation_skepticism_index = row_mean_min(
      dplyr::pick(z_pass_without_competence, z_too_many_resources, z_resources_ineffective),
      c("z_pass_without_competence", "z_too_many_resources", "z_resources_ineffective"),
      min_nonmissing = 2
    ),
    strictness_beliefs_index = row_mean_min(
      dplyr::pick(
        z_meritocracy, z_blame_students, z_blame_system_rev,
        z_pass_without_competence, z_too_many_resources, z_resources_ineffective
      ),
      c(
        "z_meritocracy", "z_blame_students", "z_blame_system_rev",
        "z_pass_without_competence", "z_too_many_resources", "z_resources_ineffective"
      ),
      min_nonmissing = 4
    ),
    strictness_beliefs_index = std_z(strictness_beliefs_index),
    resource_skepticism_index = std_z(resource_skepticism_index),
    academic_standards_index = std_z(academic_standards_index),
    student_attribution_index = std_z(student_attribution_index),
    responsibility_index = std_z(responsibility_index),
    remediation_skepticism_index = std_z(remediation_skepticism_index)
  )

cards_long <- df %>%
  dplyr::select(id, dplyr::all_of(decision_cards)) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(decision_cards),
    names_to = "card",
    values_to = "decision"
  ) %>%
  dplyr::filter(!is.na(decision)) %>%
  dplyr::mutate(
    repeat_decision = dplyr::case_when(
      decision == "repite" ~ 1,
      decision == "pasa" ~ 0,
      TRUE ~ NA_real_
    ),
    card_group = as.integer(sub("alumno_([0-9]+)_.*", "\\1", card)),
    card_number = as.integer(sub("alumno_[0-9]+_([0-9]+)", "\\1", card)),
    boy = as.integer(card %in% vector_niño),
    complex_background = as.integer(card %in% vector_extranjero),
    failed_subjects = as.integer(card %in% vector_suspensos),
    low_competence = as.integer(card %in% vector_carencias),
    absent = as.integer(card %in% vector_absentista),
    disruptive = as.integer(card %in% vector_expulsion),
    negative_attributes = boy + complex_background + failed_subjects +
      low_competence + absent + disruptive,
    card = factor(card)
  ) %>%
  dplyr::filter(!is.na(repeat_decision)) %>%
  dplyr::left_join(
    teacher_data %>%
      dplyr::select(
        id, treatment_arm, assigned_policy, favorite_policy, least_favorite_policy,
        policy_alignment, strictness_beliefs_index, responsibility_index,
        remediation_skepticism_index, resource_skepticism_index,
        academic_standards_index, student_attribution_index, grade_level,
        edad, experiencia, school_type, permanent, female
      ),
    by = "id"
  ) %>%
  dplyr::filter(!is.na(treatment_arm))

teacher_harshness <- cards_long %>%
  dplyr::group_by(card) %>%
  dplyr::mutate(card_repeat_peer = (sum(repeat_decision) - repeat_decision) / (dplyr::n() - 1)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(hb_card = repeat_decision - card_repeat_peer) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    hb = mean(hb_card, na.rm = TRUE),
    ha = mean(repeat_decision, na.rm = TRUE),
    n_decisions = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    teacher_data %>%
      dplyr::select(
        id, treatment_arm, assigned_policy, favorite_policy, least_favorite_policy,
        policy_alignment, strictness_beliefs_index, responsibility_index,
        remediation_skepticism_index, resource_skepticism_index,
        academic_standards_index, student_attribution_index, grade_level,
        edad, experiencia, school_type, permanent, female
      ),
    by = "id"
  )

belief_item_vars <- c(
  "z_meritocracy", "z_blame_students", "z_blame_system_rev",
  "z_pass_without_competence", "z_too_many_resources", "z_resources_ineffective"
)

homogeneous_index_specs <- list(
  resource_skepticism_index = c("z_too_many_resources", "z_resources_ineffective"),
  academic_standards_index = c("z_pass_without_competence", "z_prepared_next_level_rev"),
  student_attribution_index = c("z_blame_students", "z_blame_system_rev"),
  merit_student_responsibility_index = c("z_meritocracy", "z_blame_students"),
  remediation_skepticism_index = c(
    "z_pass_without_competence",
    "z_too_many_resources",
    "z_resources_ineffective"
  ),
  strictness_beliefs_index = belief_item_vars
)

make_index_screening <- function(data, specs, outcome) {
  dplyr::bind_rows(lapply(names(specs), function(index_name) {
    vars <- specs[[index_name]]
    idx <- std_z(row_mean_min(data, vars, min_nonmissing = length(vars)))
    screening_data <- data
    screening_data$.idx <- idx
    model <- stats::lm(stats::as.formula(paste0(outcome, " ~ .idx")), data = screening_data)
    coefs <- summary(model)$coefficients
    corr_mat <- stats::cor(data[, vars, drop = FALSE], use = "complete.obs")
    tibble::tibble(
      index = index_name,
      k_items = length(vars),
      items = paste(vars, collapse = " + "),
      n_complete_items = sum(stats::complete.cases(data[, vars, drop = FALSE])),
      n_index = sum(!is.na(idx)),
      alpha = cronbach_alpha(data, vars),
      mean_interitem_correlation = if (length(vars) > 1) {
        mean(corr_mat[upper.tri(corr_mat)])
      } else {
        NA_real_
      },
      bivariate_estimate = coefs[".idx", "Estimate"],
      bivariate_p_value = coefs[".idx", "Pr(>|t|)"]
    )
  }))
}

belief_summary <- teacher_data %>%
  dplyr::summarise(
    n_teachers = dplyr::n(),
    n_index_nonmissing = sum(!is.na(strictness_beliefs_index)),
    n_resource_skepticism_nonmissing = sum(!is.na(resource_skepticism_index)),
    index_mean = mean(strictness_beliefs_index, na.rm = TRUE),
    index_sd = stats::sd(strictness_beliefs_index, na.rm = TRUE),
    resource_skepticism_alpha = cronbach_alpha(
      teacher_data,
      c("z_too_many_resources", "z_resources_ineffective")
    ),
    academic_standards_alpha = cronbach_alpha(
      teacher_data,
      c("z_pass_without_competence", "z_prepared_next_level_rev")
    ),
    student_attribution_alpha = cronbach_alpha(
      teacher_data,
      c("z_blame_students", "z_blame_system_rev")
    ),
    responsibility_alpha = cronbach_alpha(
      teacher_data,
      c("z_meritocracy", "z_blame_students", "z_blame_system_rev")
    ),
    remediation_alpha = cronbach_alpha(
      teacher_data,
      c("z_pass_without_competence", "z_too_many_resources", "z_resources_ineffective")
    ),
    strictness_alpha = cronbach_alpha(teacher_data, belief_item_vars)
  )

sample_summary <- tibble::tibble(
  metric = c(
    "teachers_with_decisions",
    "card_decisions",
    "mean_decisions_per_teacher",
    "sd_teacher_hb",
    "mean_repeat_rate"
  ),
  value = c(
    dplyr::n_distinct(cards_long$id),
    nrow(cards_long),
    mean(teacher_harshness$n_decisions),
    stats::sd(teacher_harshness$hb, na.rm = TRUE),
    mean(cards_long$repeat_decision, na.rm = TRUE)
  )
)

nice_write_csv(sample_summary, paste0(card_out_prefix, "_sample_summary.csv"))
nice_write_csv(belief_summary, paste0(card_out_prefix, "_belief_index_summary.csv"))

index_screening <- make_index_screening(
  teacher_harshness %>%
    dplyr::left_join(
      teacher_data %>%
        dplyr::select(
          id, z_meritocracy, z_blame_students, z_blame_system_rev,
          z_pass_without_competence, z_prepared_next_level_rev,
          z_too_many_resources, z_resources_ineffective
        ),
      by = "id"
    ) %>%
    dplyr::select(
      hb, z_meritocracy, z_blame_students, z_blame_system_rev,
      z_pass_without_competence, z_prepared_next_level_rev,
      z_too_many_resources, z_resources_ineffective
    ),
  homogeneous_index_specs,
  "hb"
)

nice_write_csv(index_screening, paste0(card_out_prefix, "_homogeneous_index_screening.csv"))

#----------------------------#
#### 3. Card-level models ####
#----------------------------#

student_attrs <- c(
  "boy", "complex_background", "failed_subjects",
  "low_competence", "absent", "disruptive"
)

demean_by_id <- function(x, id) {
  x - ave(x, id, FUN = function(z) mean(z, na.rm = TRUE))
}

# H4|1 in the preregistration asks about sequential dependence across decisions.
# We do not estimate it here because the analysis data do not contain a validated
# card-level response order. The numeric card identifiers are design labels, not
# an observed sequence, so using them as lags would be misleading.

within_data <- cards_long
within_data$y_within <- demean_by_id(within_data$repeat_decision, within_data$id)

for (attr in student_attrs) {
  within_data[[paste0(attr, "_within")]] <- demean_by_id(within_data[[attr]], within_data$id)

  strictness_term <- paste0("strictness_x_", attr)
  within_data[[strictness_term]] <- within_data$strictness_beliefs_index * within_data[[attr]]
  within_data[[paste0(strictness_term, "_within")]] <- demean_by_id(
    within_data[[strictness_term]],
    within_data$id
  )

  resource_term <- paste0("resource_skepticism_x_", attr)
  within_data[[resource_term]] <- within_data$resource_skepticism_index * within_data[[attr]]
  within_data[[paste0(resource_term, "_within")]] <- demean_by_id(
    within_data[[resource_term]],
    within_data$id
  )

  for (arm in c("Policy treatment", "Revelation treatment", "Awareness treatment")) {
    arm_slug <- gsub("[^A-Za-z0-9]+", "_", tolower(arm))
    treat_term <- paste0(arm_slug, "_x_", attr)
    within_data[[treat_term]] <- as.integer(within_data$treatment_arm == arm) * within_data[[attr]]
    within_data[[paste0(treat_term, "_within")]] <- demean_by_id(
      within_data[[treat_term]],
      within_data$id
    )
  }
}

student_attrs_within <- paste0(student_attrs, "_within")
strictness_attrs_within <- paste0("strictness_x_", student_attrs, "_within")
resource_skepticism_attrs_within <- paste0("resource_skepticism_x_", student_attrs, "_within")
treatment_attrs_within <- as.vector(outer(
  gsub("[^A-Za-z0-9]+", "_", tolower(c(
    "Policy treatment", "Revelation treatment", "Awareness treatment"
  ))),
  student_attrs,
  paste,
  sep = "_x_"
))
treatment_attrs_within <- paste0(treatment_attrs_within, "_within")

alignment_card_data <- cards_long %>%
  dplyr::filter(treatment_arm != "Control", !is.na(policy_alignment)) %>%
  dplyr::mutate(
    policy_alignment_treated = factor(
      policy_alignment,
      levels = c("Middle assigned", "Favorite assigned", "Least favorite assigned")
    )
  )

alignment_teacher_data <- teacher_harshness %>%
  dplyr::filter(treatment_arm != "Control", !is.na(policy_alignment)) %>%
  dplyr::mutate(
    policy_alignment_treated = factor(
      policy_alignment,
      levels = c("Middle assigned", "Favorite assigned", "Least favorite assigned")
    )
  )

within_alignment_data <- within_data %>%
  dplyr::filter(treatment_arm != "Control", !is.na(policy_alignment)) %>%
  dplyr::mutate(
    policy_alignment_treated = factor(
      policy_alignment,
      levels = c("Middle assigned", "Favorite assigned", "Least favorite assigned")
    )
  )

for (attr in student_attrs) {
  for (align_level in c("Favorite assigned", "Least favorite assigned")) {
    align_slug <- gsub("[^A-Za-z0-9]+", "_", tolower(align_level))
    align_term <- paste0(align_slug, "_x_", attr)
    within_alignment_data[[align_term]] <-
      as.integer(within_alignment_data$policy_alignment == align_level) *
      within_alignment_data[[attr]]
    within_alignment_data[[paste0(align_term, "_within")]] <- demean_by_id(
      within_alignment_data[[align_term]],
      within_alignment_data$id
    )
  }
}

alignment_attrs_within <- as.vector(outer(
  gsub("[^A-Za-z0-9]+", "_", tolower(c("Favorite assigned", "Least favorite assigned"))),
  student_attrs,
  paste,
  sep = "_x_"
))
alignment_attrs_within <- paste0(alignment_attrs_within, "_within")

m_card_treat <- stats::lm(
  repeat_decision ~ treatment_arm + card,
  data = cards_long
)

m_card_rules <- stats::lm(
  stats::as.formula(
    paste0("repeat_decision ~ treatment_arm * (", paste(student_attrs, collapse = " + "), ")")
  ),
  data = cards_long
)

m_card_belief <- stats::lm(
  repeat_decision ~ strictness_beliefs_index + treatment_arm + favorite_policy +
    grade_level + edad + experiencia + school_type + permanent + card,
  data = cards_long
)

m_card_belief_components <- stats::lm(
  repeat_decision ~ responsibility_index + remediation_skepticism_index +
    treatment_arm + favorite_policy + grade_level + edad + experiencia +
    school_type + permanent + card,
  data = cards_long
)

m_card_resource_skepticism <- stats::lm(
  repeat_decision ~ resource_skepticism_index + treatment_arm + favorite_policy +
    grade_level + edad + experiencia + school_type + permanent + card,
  data = cards_long
)

m_card_belief_rules <- stats::lm(
  stats::as.formula(
    paste0(
      "repeat_decision ~ strictness_beliefs_index + treatment_arm + favorite_policy + ",
      "grade_level + edad + experiencia + school_type + permanent + card + ",
      paste0("strictness_beliefs_index:", student_attrs, collapse = " + ")
    )
  ),
  data = cards_long
)

m_within_attrs <- stats::lm(
  stats::as.formula(paste0("y_within ~ 0 + ", paste(student_attrs_within, collapse = " + "))),
  data = within_data
)

m_within_belief_rules <- stats::lm(
  stats::as.formula(paste0(
    "y_within ~ 0 + ",
    paste(c(student_attrs_within, strictness_attrs_within), collapse = " + ")
  )),
  data = within_data
)

m_within_resource_skepticism_rules <- stats::lm(
  stats::as.formula(paste0(
    "y_within ~ 0 + ",
    paste(c(student_attrs_within, resource_skepticism_attrs_within), collapse = " + ")
  )),
  data = within_data
)

within_harshness_data <- within_data %>%
  dplyr::left_join(
    teacher_harshness %>% dplyr::select(id, hb),
    by = "id"
  ) %>%
  dplyr::mutate(
    hb_z = std_z(hb)
  )

harshness_cutoffs <- stats::quantile(
  teacher_harshness$hb,
  probs = c(1 / 3, 2 / 3),
  na.rm = TRUE
)

within_harshness_data <- within_harshness_data %>%
  dplyr::mutate(
    harshness_group = dplyr::case_when(
      hb <= harshness_cutoffs[[1]] ~ "Lenient tercile",
      hb >= harshness_cutoffs[[2]] ~ "Harsh tercile",
      TRUE ~ "Middle tercile"
    ),
    harshness_group = factor(
      harshness_group,
      levels = c("Lenient tercile", "Middle tercile", "Harsh tercile")
    )
  )

harshness_attrs_within <- paste0("hb_z_x_", student_attrs, "_within")
for (attr in student_attrs) {
  within_harshness_data[[paste0("hb_z_x_", attr, "_within")]] <-
    within_harshness_data$hb_z * within_harshness_data[[paste0(attr, "_within")]]
}

m_within_harshness_rules <- stats::lm(
  stats::as.formula(paste0(
    "y_within ~ 0 + ",
    paste(c(student_attrs_within, harshness_attrs_within), collapse = " + ")
  )),
  data = within_harshness_data
)

m_within_treat_rules <- stats::lm(
  stats::as.formula(paste0(
    "y_within ~ 0 + ",
    paste(c(student_attrs_within, treatment_attrs_within), collapse = " + ")
  )),
  data = within_data
)

m_card_alignment <- stats::lm(
  repeat_decision ~ policy_alignment_treated + treatment_arm + favorite_policy +
    grade_level + edad + experiencia + school_type + permanent + card,
  data = alignment_card_data
)

m_teacher_alignment <- stats::lm(
  hb ~ policy_alignment_treated + treatment_arm + favorite_policy +
    grade_level + edad + experiencia + school_type + permanent,
  data = alignment_teacher_data
)

m_within_alignment_rules <- stats::lm(
  stats::as.formula(paste0(
    "y_within ~ 0 + ",
    paste(c(student_attrs_within, alignment_attrs_within), collapse = " + ")
  )),
  data = within_alignment_data
)

m_teacher_treat <- stats::lm(
  hb ~ treatment_arm,
  data = teacher_harshness
)

m_teacher_belief_index <- stats::lm(
  hb ~ strictness_beliefs_index + treatment_arm + favorite_policy +
    grade_level + edad + experiencia + school_type + permanent,
  data = teacher_harshness
)

m_teacher_belief_components <- stats::lm(
  hb ~ responsibility_index + remediation_skepticism_index +
    treatment_arm + favorite_policy + grade_level + edad + experiencia +
    school_type + permanent,
  data = teacher_harshness
)

m_teacher_resource_skepticism <- stats::lm(
  hb ~ resource_skepticism_index + treatment_arm + favorite_policy +
    grade_level + edad + experiencia + school_type + permanent,
  data = teacher_harshness
)

card_treat_terms <- tidy_cluster_lm(m_card_treat, cards_long, "id") %>%
  dplyr::filter(grepl("^treatment_arm", term))

card_rules_terms <- tidy_cluster_lm(m_card_rules, cards_long, "id") %>%
  dplyr::filter(grepl(":", term))

card_belief_terms <- tidy_cluster_lm(m_card_belief, cards_long, "id") %>%
  dplyr::filter(
    term %in% c("strictness_beliefs_index") |
      grepl("^favorite_policy", term) |
      grepl("^treatment_arm", term) |
      grepl("^grade_level", term)
  )

card_belief_component_terms <- tidy_cluster_lm(m_card_belief_components, cards_long, "id") %>%
  dplyr::filter(
    term %in% c("responsibility_index", "remediation_skepticism_index") |
      grepl("^favorite_policy", term) |
      grepl("^treatment_arm", term) |
      grepl("^grade_level", term)
  )

card_resource_skepticism_terms <- tidy_cluster_lm(m_card_resource_skepticism, cards_long, "id") %>%
  dplyr::filter(
    term %in% c("resource_skepticism_index") |
      grepl("^favorite_policy", term) |
      grepl("^treatment_arm", term) |
      grepl("^grade_level", term)
  )

card_belief_rules_terms <- tidy_cluster_lm(m_card_belief_rules, cards_long, "id") %>%
  dplyr::filter(grepl("^strictness_beliefs_index:", term))

within_attr_terms <- tidy_cluster_lm(m_within_attrs, within_data, "id")

within_belief_rules_terms <- tidy_cluster_lm(m_within_belief_rules, within_data, "id") %>%
  dplyr::filter(grepl("^strictness_x_", term))

within_resource_skepticism_rules_terms <- tidy_cluster_lm(
  m_within_resource_skepticism_rules,
  within_data,
  "id"
) %>%
  dplyr::filter(grepl("^resource_skepticism_x_", term))

within_resource_skepticism_full_terms <- tidy_cluster_lm(
  m_within_resource_skepticism_rules,
  within_data,
  "id"
)

attribute_labels <- c(
  boy = "Male student",
  complex_background = "Complex/migrant background",
  failed_subjects = "Failed subjects",
  low_competence = "Low competence",
  absent = "Absenteeism",
  disruptive = "Disruptive behavior"
)

resource_skepticism_slope_comparison <- dplyr::bind_rows(lapply(student_attrs, function(attr) {
  main_term <- paste0(attr, "_within")
  interaction_term <- paste0("resource_skepticism_x_", attr, "_within")
  label <- unname(attribute_labels[attr])

  base_row <- within_attr_terms %>%
    dplyr::filter(term == main_term) %>%
    dplyr::mutate(
      attribute = label,
      model = "Base within-teacher",
      contrast = term
    )

  adjusted_row <- within_resource_skepticism_full_terms %>%
    dplyr::filter(term == main_term) %>%
    dplyr::mutate(
      attribute = label,
      model = "With resource skepticism: average beliefs",
      contrast = term
    )

  low_row <- linear_combo_lm(
    m_within_resource_skepticism_rules,
    within_data,
    "id",
    stats::setNames(c(1, -1), c(main_term, interaction_term)),
    paste0(main_term, " at -1 SD resource skepticism")
  ) %>%
    dplyr::mutate(
      term = main_term,
      attribute = label,
      model = "Low resource skepticism (-1 SD)"
    )

  high_row <- linear_combo_lm(
    m_within_resource_skepticism_rules,
    within_data,
    "id",
    stats::setNames(c(1, 1), c(main_term, interaction_term)),
    paste0(main_term, " at +1 SD resource skepticism")
  ) %>%
    dplyr::mutate(
      term = main_term,
      attribute = label,
      model = "High resource skepticism (+1 SD)"
    )

  difference_row <- linear_combo_lm(
    m_within_resource_skepticism_rules,
    within_data,
    "id",
    stats::setNames(2, interaction_term),
    paste0(main_term, " high-minus-low resource skepticism")
  ) %>%
    dplyr::mutate(
      term = interaction_term,
      attribute = label,
      model = "High minus low resource skepticism"
    )

  dplyr::bind_rows(base_row, adjusted_row, low_row, high_row, difference_row)
})) %>%
  dplyr::select(
    attribute, model, term, contrast, estimate, std.error, statistic, p.value,
    conf.low, conf.high, df, dplyr::any_of("n_obs")
  )

within_harshness_rules_terms <- tidy_cluster_lm(
  m_within_harshness_rules,
  within_harshness_data,
  "id"
) %>%
  dplyr::filter(grepl("^hb_z_x_", term))

within_attr_by_harshness_group_terms <- dplyr::bind_rows(lapply(
  levels(within_harshness_data$harshness_group),
  function(group_name) {
    group_data <- within_harshness_data %>%
      dplyr::filter(harshness_group == group_name)

    group_model <- stats::lm(
      stats::as.formula(paste0(
        "y_within ~ 0 + ",
        paste(student_attrs_within, collapse = " + ")
      )),
      data = group_data
    )

    tidy_cluster_lm(group_model, group_data, "id") %>%
      dplyr::mutate(
        harshness_group = group_name,
        n_teachers = dplyr::n_distinct(group_data$id)
      )
  }
))

within_treat_rules_terms <- tidy_cluster_lm(m_within_treat_rules, within_data, "id") %>%
  dplyr::filter(grepl("_treatment_x_", term))

card_alignment_terms <- tidy_cluster_lm(m_card_alignment, alignment_card_data, "id") %>%
  dplyr::filter(
    grepl("^policy_alignment_treated", term) |
      grepl("^favorite_policy", term) |
      grepl("^treatment_arm", term)
  )

teacher_alignment_terms <- tidy_lm_default(m_teacher_alignment) %>%
  dplyr::filter(
    grepl("^policy_alignment_treated", term) |
      grepl("^favorite_policy", term) |
      grepl("^treatment_arm", term)
  )

within_alignment_rules_terms <- tidy_cluster_lm(
  m_within_alignment_rules,
  within_alignment_data,
  "id"
) %>%
  dplyr::filter(grepl("_assigned_x_", term))

teacher_belief_terms <- tidy_lm_default(m_teacher_belief_index) %>%
  dplyr::filter(
    term %in% c("strictness_beliefs_index") |
      grepl("^favorite_policy", term) |
      grepl("^treatment_arm", term) |
      grepl("^grade_level", term)
  )

teacher_belief_component_terms <- tidy_lm_default(m_teacher_belief_components) %>%
  dplyr::filter(
    term %in% c("responsibility_index", "remediation_skepticism_index") |
      grepl("^favorite_policy", term) |
      grepl("^treatment_arm", term) |
      grepl("^grade_level", term)
  )

teacher_resource_skepticism_terms <- tidy_lm_default(m_teacher_resource_skepticism) %>%
  dplyr::filter(
    term %in% c("resource_skepticism_index") |
      grepl("^favorite_policy", term) |
      grepl("^treatment_arm", term) |
      grepl("^grade_level", term)
  )

nice_write_csv(card_treat_terms, paste0(card_out_prefix, "_treatment_terms.csv"))
nice_write_csv(card_rules_terms, paste0(card_out_prefix, "_treatment_x_attributes.csv"))
nice_write_csv(card_belief_terms, paste0(card_out_prefix, "_belief_terms.csv"))
nice_write_csv(card_belief_component_terms, paste0(card_out_prefix, "_belief_component_terms.csv"))
nice_write_csv(card_resource_skepticism_terms, paste0(card_out_prefix, "_resource_skepticism_terms.csv"))
nice_write_csv(card_belief_rules_terms, paste0(card_out_prefix, "_belief_x_attributes.csv"))
nice_write_csv(within_attr_terms, paste0(card_out_prefix, "_within_teacher_attribute_weights.csv"))
nice_write_csv(within_belief_rules_terms, paste0(card_out_prefix, "_within_teacher_belief_x_attributes.csv"))
nice_write_csv(
  within_resource_skepticism_rules_terms,
  paste0(card_out_prefix, "_within_teacher_resource_skepticism_x_attributes.csv")
)
nice_write_csv(
  within_resource_skepticism_full_terms,
  paste0(card_out_prefix, "_within_teacher_attribute_weights_with_resource_skepticism_full.csv")
)
nice_write_csv(
  resource_skepticism_slope_comparison,
  paste0(card_out_prefix, "_within_teacher_resource_skepticism_slope_comparison.csv")
)
nice_write_csv(
  within_harshness_rules_terms,
  paste0(card_out_prefix, "_within_teacher_harshness_x_attributes.csv")
)
nice_write_csv(
  within_attr_by_harshness_group_terms,
  paste0(card_out_prefix, "_within_teacher_attribute_weights_by_harshness_group.csv")
)
nice_write_csv(within_treat_rules_terms, paste0(card_out_prefix, "_within_teacher_treatment_x_attributes.csv"))
nice_write_csv(card_alignment_terms, paste0(card_out_prefix, "_alignment_card_terms.csv"))
nice_write_csv(teacher_alignment_terms, paste0(card_out_prefix, "_alignment_teacher_terms.csv"))
nice_write_csv(within_alignment_rules_terms, paste0(card_out_prefix, "_alignment_within_attribute_terms.csv"))
nice_write_csv(teacher_belief_terms, paste0(card_out_prefix, "_teacher_belief_terms.csv"))
nice_write_csv(teacher_belief_component_terms, paste0(card_out_prefix, "_teacher_belief_component_terms.csv"))
nice_write_csv(
  teacher_resource_skepticism_terms,
  paste0(card_out_prefix, "_teacher_resource_skepticism_terms.csv")
)

#--------------------------------#
#### 4. Contrasts, MDE, TOST ####
#--------------------------------#

card_contrasts <- dplyr::bind_rows(
  linear_combo_lm(
    m_card_treat, cards_long, "id",
    c("treatment_armPolicy treatment" = 1),
    "Policy treatment - Control"
  ),
  linear_combo_lm(
    m_card_treat, cards_long, "id",
    c("treatment_armRevelation treatment" = 1, "treatment_armPolicy treatment" = -1),
    "Revelation treatment - Policy treatment"
  ),
  linear_combo_lm(
    m_card_treat, cards_long, "id",
    c("treatment_armAwareness treatment" = 1, "treatment_armRevelation treatment" = -1),
    "Awareness treatment - Revelation treatment"
  )
) %>%
  add_mde()

teacher_contrasts <- dplyr::bind_rows(
  linear_combo_default(
    m_teacher_treat,
    c("treatment_armPolicy treatment" = 1),
    "Policy treatment - Control"
  ),
  linear_combo_default(
    m_teacher_treat,
    c("treatment_armRevelation treatment" = 1, "treatment_armPolicy treatment" = -1),
    "Revelation treatment - Policy treatment"
  ),
  linear_combo_default(
    m_teacher_treat,
    c("treatment_armAwareness treatment" = 1, "treatment_armRevelation treatment" = -1),
    "Awareness treatment - Revelation treatment"
  )
) %>%
  add_mde()

card_equivalence <- equivalence_grid(card_contrasts, margins = c(0.02, 0.03, 0.05))
teacher_equivalence <- equivalence_grid(teacher_contrasts, margins = c(0.02, 0.03, 0.05))

nice_write_csv(card_contrasts, paste0(card_out_prefix, "_card_treatment_contrasts_mde.csv"))
nice_write_csv(teacher_contrasts, paste0(card_out_prefix, "_teacher_hb_contrasts_mde.csv"))
nice_write_csv(card_equivalence, paste0(card_out_prefix, "_card_equivalence_tost.csv"))
nice_write_csv(teacher_equivalence, paste0(card_out_prefix, "_teacher_equivalence_tost.csv"))

#----------------------#
#### 5. Figures ####
#----------------------#

plot_card_contrasts <- card_contrasts %>%
  dplyr::mutate(
    contrast = factor(contrast, levels = rev(unique(contrast))),
    ci90_low = estimate - stats::qt(0.95, df = df) * std.error,
    ci90_high = estimate + stats::qt(0.95, df = df) * std.error
  )

p_treat <- ggplot2::ggplot(plot_card_contrasts, ggplot2::aes(x = estimate, y = contrast)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  ggplot2::geom_vline(xintercept = c(-0.03, 0.03), linetype = "dotted", color = "grey55") +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = conf.low, xmax = conf.high),
    width = 0.2,
    color = paleta_alt[[1]]
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = ci90_low, xmax = ci90_high),
    width = 0.35,
    linewidth = 1.2,
    color = paleta_alt[[3]]
  ) +
  ggplot2::geom_point(size = 2.5, color = paleta_alt[[1]]) +
  ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  ggplot2::labs(
    x = "Treatment effect on probability of repetition",
    y = NULL,
    title = "Card-level treatment contrasts",
    subtitle = "Thin intervals: 95% CI; thick intervals: 90% CI; dotted lines: +/-3 pp"
  ) +
  ggplot2::theme_minimal(base_size = 13)

ggplot2::ggsave(
  file.path(graficos, paste0(card_out_prefix, "_treatment_contrasts.jpeg")),
  p_treat,
  width = 10,
  height = 5
)

plot_belief_rules <- card_belief_rules_terms %>%
  dplyr::mutate(
    attribute = dplyr::case_when(
      grepl("boy", term) ~ "Boy",
      grepl("complex_background", term) ~ "Complex/migrant background",
      grepl("failed_subjects", term) ~ "Failed subjects",
      grepl("low_competence", term) ~ "Low competence",
      grepl("absent", term) ~ "Absenteeism",
      grepl("disruptive", term) ~ "Disruptive behavior",
      TRUE ~ term
    ),
    attribute = factor(attribute, levels = rev(c(
      "Boy", "Complex/migrant background", "Failed subjects",
      "Low competence", "Absenteeism", "Disruptive behavior"
    )))
  )

p_belief_rules <- ggplot2::ggplot(
  plot_belief_rules,
  ggplot2::aes(x = estimate, y = attribute)
) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey45") +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = conf.low, xmax = conf.high),
    width = 0.2,
    color = paleta_alt[[1]]
  ) +
  ggplot2::geom_point(size = 2.5, color = paleta_alt[[3]]) +
  ggplot2::scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  ggplot2::labs(
    x = "Interaction with strictness beliefs index",
    y = NULL,
    title = "Do beliefs change the weight of student attributes?"
  ) +
  ggplot2::theme_minimal(base_size = 13)

ggplot2::ggsave(
  file.path(graficos, paste0(card_out_prefix, "_belief_x_attributes.jpeg")),
  p_belief_rules,
  width = 10,
  height = 5
)

#-----------------------------#
#### 6. Automated report ####
#-----------------------------#

fmt_num <- function(x, digits = 3) format(round(x, digits), nsmall = digits)
fmt_pp <- function(x, digits = 1) paste0(format(round(100 * x, digits), nsmall = digits), " pp")

main_belief <- card_belief_terms %>%
  dplyr::filter(term == "strictness_beliefs_index") %>%
  dplyr::slice(1)

component_beliefs <- card_belief_component_terms %>%
  dplyr::filter(term %in% c("responsibility_index", "remediation_skepticism_index"))

favorite_terms <- card_belief_terms %>%
  dplyr::filter(grepl("^favorite_policy", term))

equiv_3pp <- card_equivalence %>%
  dplyr::filter(equiv_margin == 0.03)

report_lines <- c(
  "# Card-level exploratory analyses",
  "",
  "This report is generated by `2. Code/4. decision_card_models.R`.",
  "",
  "## Sample",
  "",
  paste0("- Teachers with valid card decisions: ", dplyr::n_distinct(cards_long$id), "."),
  paste0("- Card-level decisions: ", nrow(cards_long), "."),
  paste0("- Mean decisions per teacher: ", fmt_num(mean(teacher_harshness$n_decisions), 2), "."),
  paste0("- Mean repeat decision rate: ", fmt_pp(mean(cards_long$repeat_decision), 1), "."),
  "",
  "## Belief index",
  "",
  "The script builds an exploratory `strictness_beliefs_index` from standardized items whose higher values imply more individual-responsibility/strictness-oriented beliefs: meritocracy, blame attributed to students, reverse blame attributed to the education system, belief that students pass without competencies, belief that too many resources go to repeating students, and belief that additional resources for repeating students are ineffective.",
  "",
  paste0("- Non-missing index observations: ", belief_summary$n_index_nonmissing, "."),
  paste0("- Cronbach alpha for all six items: ", fmt_num(belief_summary$strictness_alpha, 3), "."),
  paste0("- Cronbach alpha for responsibility subindex: ", fmt_num(belief_summary$responsibility_alpha, 3), "."),
  paste0("- Cronbach alpha for remediation skepticism subindex: ", fmt_num(belief_summary$remediation_alpha, 3), "."),
  "",
  "## Treatment effects at card level",
  "",
  paste0(
    "- Policy treatment vs Control: ",
    fmt_pp(card_contrasts$estimate[card_contrasts$contrast == "Policy treatment - Control"], 2),
    " (MDE80: ",
    fmt_pp(card_contrasts$mde_80[card_contrasts$contrast == "Policy treatment - Control"], 2),
    ")."
  ),
  paste0(
    "- Revelation vs Policy: ",
    fmt_pp(card_contrasts$estimate[card_contrasts$contrast == "Revelation treatment - Policy treatment"], 2),
    " (MDE80: ",
    fmt_pp(card_contrasts$mde_80[card_contrasts$contrast == "Revelation treatment - Policy treatment"], 2),
    ")."
  ),
  paste0(
    "- Awareness vs Revelation: ",
    fmt_pp(card_contrasts$estimate[card_contrasts$contrast == "Awareness treatment - Revelation treatment"], 2),
    " (MDE80: ",
    fmt_pp(card_contrasts$mde_80[card_contrasts$contrast == "Awareness treatment - Revelation treatment"], 2),
    ")."
  ),
  "",
  "Equivalence test with +/-3 percentage point bounds:",
  paste0(
    "- ",
    equiv_3pp$contrast,
    ": equivalent = ",
    equiv_3pp$equivalent,
    " (TOST p = ",
    fmt_num(equiv_3pp$tost_p, 3),
    ")."
  ),
  "",
  "## Beliefs and preferences",
  "",
  paste0(
    "- One SD increase in strictness beliefs predicts ",
    fmt_pp(main_belief$estimate, 2),
    " higher repetition probability, controlling for treatment, preferred policy, demographics, and card fixed effects."
  ),
  paste0(
    "- Component estimates: ",
    paste(
      paste0(component_beliefs$term, " = ", fmt_pp(component_beliefs$estimate, 2)),
      collapse = "; "
    ),
    "."
  ),
  paste0(
    "- Favorite-policy coefficients relative to Reinforcement: ",
    paste(
      paste0(favorite_terms$term, " = ", fmt_pp(favorite_terms$estimate, 2)),
      collapse = "; "
    ),
    "."
  ),
  "",
  "## Files written",
  "",
  paste0("- Tables: `", normalizePath(tables, winslash = "/"), "/*", card_out_prefix, "*.csv`."),
  paste0("- Figures: `", normalizePath(graficos, winslash = "/"), "/", card_out_prefix, "_*.jpeg`.")
)

writeLines(
  report_lines,
  file.path(output, "reports", paste0(card_out_prefix, "_report.md")),
  useBytes = TRUE
)

message("Finished card-level exploratory analysis.")
message("Report: ", file.path(output, "reports", paste0(card_out_prefix, "_report.md")))
