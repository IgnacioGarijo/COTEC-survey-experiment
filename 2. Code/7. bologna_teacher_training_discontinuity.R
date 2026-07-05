#===========================================================#
#### Bologna teacher-training cohorts and repetition     ####
#===========================================================#

source("2. Code/0. main.R")

df <- read_parquet(cleandata)

if (!dir.exists(graficos)) dir.create(graficos, recursive = TRUE)
if (!dir.exists(tables)) dir.create(tables, recursive = TRUE)

# The survey file is dated April 2025. Bologna degrees and the new secondary
# teacher-training master's started around academic year 2009-10.
survey_year <- 2025
bologna_start_year <- 2009
primary_start_age <- 18
secondary_master_age <- 23

primary_cutoff_age <- survey_year - (bologna_start_year - primary_start_age)
secondary_cutoff_age <- survey_year - (bologna_start_year - secondary_master_age)

namelist_cards <- setdiff(namelist, c("alumno_1_1", "alumno_2_1"))

df_cards <- df[, c("id", namelist_cards)]

df_repetition <- df_cards %>%
  pivot_longer(all_of(namelist_cards), names_to = "alumno", values_to = "decision") %>%
  drop_na(decision) %>%
  mutate(repite = ifelse(decision == "repite", 1, 0)) %>%
  group_by(id) %>%
  summarise(raw_repetition_rate = mean(repite, na.rm = TRUE), .groups = "drop")

df_harshness <- df_cards %>%
  pivot_longer(all_of(namelist_cards), names_to = "alumno", values_to = "decision") %>%
  drop_na(decision) %>%
  mutate(repite = ifelse(decision == "repite", 1, 0)) %>%
  group_by(alumno) %>%
  mutate(card_mean_other_teachers = (sum(repite, na.rm = TRUE) - repite) / (n() - 1)) %>%
  ungroup() %>%
  mutate(relative_harshness = repite - card_mean_other_teachers) %>%
  group_by(id) %>%
  summarise(relative_harshness = mean(relative_harshness, na.rm = TRUE), .groups = "drop")

teacher_cohorts <- df %>%
  select(id, edad, nivel, experiencia, female, treatment, politica) %>%
  left_join(df_repetition, by = "id") %>%
  left_join(df_harshness, by = "id") %>%
  mutate(
    stage = case_when(
      nivel == "E. Primaria" ~ "Primary teachers",
      nivel == "E. Secundaria" ~ "Secondary teachers",
      TRUE ~ NA_character_
    ),
    cutoff_age = ifelse(stage == "Primary teachers", primary_cutoff_age, secondary_cutoff_age),
    years_from_cutoff = edad - cutoff_age,
    bologna_cohort = ifelse(edad <= cutoff_age, "Bologna-era training", "Pre-Bologna training"),
    cutoff_side = ifelse(edad <= cutoff_age, "Younger side", "Older side")
  ) %>%
  filter(
    !is.na(edad),
    !is.na(stage),
    !is.na(raw_repetition_rate),
    !is.na(relative_harshness)
  )

age_means <- teacher_cohorts %>%
  group_by(stage, edad, cutoff_age) %>%
  summarise(
    n_teachers = n(),
    se_raw = sd(raw_repetition_rate, na.rm = TRUE) / sqrt(n_teachers),
    se_harshness = sd(relative_harshness, na.rm = TRUE) / sqrt(n_teachers),
    raw_repetition_rate = mean(raw_repetition_rate, na.rm = TRUE),
    relative_harshness = mean(relative_harshness, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_teachers >= 5) %>%
  mutate(
    raw_repetition_rate_pp = 100 * raw_repetition_rate,
    relative_harshness_pp = 100 * relative_harshness,
    raw_low = 100 * (raw_repetition_rate - 1.96 * se_raw),
    raw_high = 100 * (raw_repetition_rate + 1.96 * se_raw),
    harshness_low = 100 * (relative_harshness - 1.96 * se_harshness),
    harshness_high = 100 * (relative_harshness + 1.96 * se_harshness)
  )

cutoff_labels <- teacher_cohorts %>%
  distinct(stage, cutoff_age) %>%
  mutate(
    x = cutoff_age,
    label = ifelse(stage == "Primary teachers",
                   "Approx. Bologna cutoff: age 34",
                   "Approx. Bologna cutoff: age 39")
  )

write.csv(
  teacher_cohorts,
  file.path(tables, "bologna_teacher_training_teacher_level.csv"),
  row.names = FALSE
)

write.csv(
  age_means,
  file.path(tables, "bologna_teacher_training_age_means.csv"),
  row.names = FALSE
)

teacher_cohorts <- teacher_cohorts %>%
  mutate(
    bologna_side = ifelse(years_from_cutoff <= 0, 1, 0),
    raw_repetition_rate_pp = 100 * raw_repetition_rate,
    relative_harshness_pp = 100 * relative_harshness
  )

rd_estimates <- tibble()

for (bw in c(5, 7, 10)) {
  for (s in c("Primary teachers", "Secondary teachers")) {
    tmp <- teacher_cohorts %>%
      filter(stage == s, abs(years_from_cutoff) <= bw)

    model_raw <- lm(raw_repetition_rate_pp ~ bologna_side * years_from_cutoff, data = tmp)
    model_harsh <- lm(relative_harshness_pp ~ bologna_side * years_from_cutoff, data = tmp)

    raw_coef <- broom::tidy(model_raw) %>% filter(term == "bologna_side")
    harsh_coef <- broom::tidy(model_harsh) %>% filter(term == "bologna_side")

    rd_estimates <- bind_rows(
      rd_estimates,
      tibble(
        stage = s,
        outcome = "Raw repetition rate",
        bandwidth_years = bw,
        n_teachers = nrow(tmp),
        jump_bologna_minus_pre_pp = raw_coef$estimate,
        se = raw_coef$std.error,
        p_value = raw_coef$p.value
      ),
      tibble(
        stage = s,
        outcome = "Relative harshness",
        bandwidth_years = bw,
        n_teachers = nrow(tmp),
        jump_bologna_minus_pre_pp = harsh_coef$estimate,
        se = harsh_coef$std.error,
        p_value = harsh_coef$p.value
      )
    )
  }
}

write.csv(
  rd_estimates,
  file.path(tables, "bologna_teacher_training_discontinuity_estimates.csv"),
  row.names = FALSE
)

plot_raw <- ggplot(age_means, aes(x = edad, y = raw_repetition_rate_pp)) +
  geom_vline(
    data = cutoff_labels,
    aes(xintercept = cutoff_age),
    linetype = "dashed",
    color = "grey45",
    linewidth = 0.6
  ) +
  geom_line(color = paleta[[3]], linewidth = 0.9) +
  geom_point(aes(size = n_teachers), color = paleta[[3]], alpha = 0.85) +
  geom_smooth(
    data = teacher_cohorts,
    aes(x = edad, y = 100 * raw_repetition_rate, group = cutoff_side),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = paleta[[7]],
    fill = paleta[[7]],
    alpha = 0.16,
    linewidth = 0.9
  ) +
  geom_text(
    data = cutoff_labels,
    aes(x = x, y = Inf, label = label),
    inherit.aes = FALSE,
    angle = 90,
    hjust = 1.05,
    vjust = -0.35,
    color = "grey35",
    size = 3.2
  ) +
  facet_wrap(~ stage, scales = "free_x") +
  scale_y_continuous(labels = label_number(accuracy = 1, suffix = " pp")) +
  scale_size_continuous(range = c(1.7, 4.4)) +
  labs(
    title = "Teacher-training cohorts and repetition decisions",
    subtitle = "Average teacher-level repetition rate by age; dashed lines mark approximate Bologna exposure cutoffs",
    x = "Teacher age",
    y = "Average repetition rate",
    size = "Teachers"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

plot_harshness <- ggplot(age_means, aes(x = edad, y = relative_harshness_pp)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.45) +
  geom_vline(
    data = cutoff_labels,
    aes(xintercept = cutoff_age),
    linetype = "dashed",
    color = "grey45",
    linewidth = 0.6
  ) +
  geom_line(color = paleta[[3]], linewidth = 0.9) +
  geom_point(aes(size = n_teachers), color = paleta[[3]], alpha = 0.85) +
  geom_smooth(
    data = teacher_cohorts,
    aes(x = edad, y = 100 * relative_harshness, group = cutoff_side),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = paleta[[7]],
    fill = paleta[[7]],
    alpha = 0.16,
    linewidth = 0.9
  ) +
  geom_text(
    data = cutoff_labels,
    aes(x = x, y = Inf, label = label),
    inherit.aes = FALSE,
    angle = 90,
    hjust = 1.05,
    vjust = -0.35,
    color = "grey35",
    size = 3.2
  ) +
  facet_wrap(~ stage, scales = "free_x") +
  scale_y_continuous(labels = label_number(accuracy = 1, suffix = " pp")) +
  scale_size_continuous(range = c(1.7, 4.4)) +
  labs(
    title = "Teacher-training cohorts and relative harshness",
    subtitle = "Mean deviation from other teachers facing the same student cards, by age",
    x = "Teacher age",
    y = "Relative harshness",
    size = "Teachers"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(
  file.path(graficos, "bologna_teacher_training_repetition_rate.jpeg"),
  plot_raw,
  width = 10.5,
  height = 6.2,
  dpi = 300
)

ggsave(
  file.path(graficos, "bologna_teacher_training_relative_harshness.jpeg"),
  plot_harshness,
  width = 10.5,
  height = 6.2,
  dpi = 300
)

message("Finished Bologna cohort graphs.")
