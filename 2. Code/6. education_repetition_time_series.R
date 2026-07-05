#===========================================================#
#### Education repetition time series from Educabase PX  ####
#===========================================================#

suppressMessages({
  library(tidyverse)
  library(scales)
})

output <- file.path("3. Output")
graficos <- file.path(output, "figures")
tables <- file.path(output, "tables")
raw_px_dir <- file.path("1. Data", "raw data", "educabase_repetition_px")

if (!dir.exists(graficos)) dir.create(graficos, recursive = TRUE)
if (!dir.exists(tables)) dir.create(tables, recursive = TRUE)
if (!dir.exists(raw_px_dir)) dir.create(raw_px_dir, recursive = TRUE)

# Palette used elsewhere in the project (see 2. Code/0. main.R).
paleta <- c("#002059", "#011552", "#537d90", "#a29cb8", "#69d3e3", "#a47dab", "#00b89f")

base_url <- "https://estadisticas.educacion.gob.es/EducaJaxiPx/files/_px/es/px/no-universitaria/alumnado/resultados/series-hasta-2023-2024-rd/porcentaje/l0"

primary_files <- tibble(
  stage = "Primary education",
  grade = paste("Primary", 1:6),
  file = sprintf("series_01_%02d.px", seq(1, 11, by = 2))
)

secondary_files <- tibble(
  stage = "Lower secondary education",
  grade = paste("ESO", 1:4),
  file = sprintf("series_02_%02d.px", seq(1, 7, by = 2))
)

download_px <- function(file) {
  dest <- file.path(raw_px_dir, file)
  if (!file.exists(dest)) {
    download.file(file.path(base_url, file), destfile = dest, mode = "wb", quiet = TRUE)
  }
  dest
}

collapse_px_statements <- function(lines) {
  statements <- c()
  current <- ""
  for (line in lines) {
    line <- trimws(line)
    if (line == "") next
    current <- paste(current, line)
    if (grepl(";$", line)) {
      statements <- c(statements, trimws(current))
      current <- ""
    }
  }
  statements
}

quoted_values <- function(x) {
  rhs <- stringr::str_replace(x, '^VALUES\\("[^"]+"\\)=', "")
  stringr::str_match_all(rhs, '"([^"]*)"')[[1]][, 2]
}

read_px <- function(path) {
  lines <- readLines(path, encoding = "latin1", warn = FALSE)
  statements <- collapse_px_statements(lines)

  value_statements <- statements[grepl("^VALUES\\(", statements)]
  dim_names <- stringr::str_match(value_statements, '^VALUES\\("([^"]+)"\\)')[, 2]
  values <- lapply(value_statements, quoted_values)
  names(values) <- dim_names

  data_statement <- statements[grepl("^DATA=", statements)]
  data_raw <- stringr::str_match_all(data_statement, '"\\.\\."|-?\\d+(?:\\.\\d+)?')[[1]][, 1]
  data_values <- rep(NA_real_, length(data_raw))
  available_values <- data_raw != '".."'
  data_values[available_values] <- as.numeric(data_raw[available_values])

  grid <- do.call(tidyr::expand_grid, values)
  if (nrow(grid) != length(data_values)) {
    stop("PX dimensions do not match data length in: ", basename(path))
  }

  grid %>% mutate(value = data_values)
}

parse_year_start <- function(period) {
  as.integer(stringr::str_sub(period, 1, 4))
}

extract_repetition <- function(meta_row) {
  px <- read_px(download_px(meta_row$file))

  if (meta_row$stage == "Primary education") {
    out <- px %>%
      filter(Sexo == "AMBOS SEXOS", `Comunidad autónoma` == "TOTAL") %>%
      transmute(
        stage = meta_row$stage,
        grade = meta_row$grade,
        period = periodo,
        year_start = parse_year_start(period),
        promotion_rate = value,
        repetition_rate = 100 - promotion_rate
      )
  } else {
    out <- px %>%
      filter(
        `Tipo de promoción` == "Total promociona",
        Sexo == "AMBOS SEXOS",
        `Comunidad autónoma` == "TOTAL"
      ) %>%
      transmute(
        stage = meta_row$stage,
        grade = meta_row$grade,
        period = periodo,
        year_start = parse_year_start(period),
        promotion_rate = value,
        repetition_rate = 100 - promotion_rate
      )
  }

  out
}

all_grade_rates <- bind_rows(primary_files, secondary_files) %>%
  split(seq_len(nrow(.))) %>%
  map_dfr(extract_repetition) %>%
  arrange(stage, grade, year_start)

repetition_series <- all_grade_rates %>%
  group_by(stage, year_start) %>%
  summarise(
    n_grades = sum(!is.na(repetition_rate)),
    repetition_rate = mean(repetition_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(repetition_rate)) %>%
  mutate(
    period = paste0(year_start, "-", stringr::str_sub(as.character(year_start + 1), 3, 4)),
    label = sprintf("%.1f", repetition_rate),
    stage = factor(stage, levels = c("Primary education", "Lower secondary education"))
  )

education_reforms <- tibble(
  year_start = c(2014, 2021),
  label = c("LOMCE", "LOMLOE")
)

write.csv(all_grade_rates, file.path(tables, "education_repetition_rates_by_grade.csv"), row.names = FALSE)
write.csv(repetition_series, file.path(tables, "education_repetition_rates_stage_average.csv"), row.names = FALSE)

base_plot <- ggplot(repetition_series, aes(x = year_start, y = repetition_rate, color = stage, group = stage)) +
  geom_vline(
    data = education_reforms,
    aes(xintercept = year_start),
    linetype = "dashed",
    color = "grey45",
    linewidth = 0.55
  ) +
  geom_text(
    data = education_reforms,
    aes(x = year_start, y = Inf, label = label),
    inherit.aes = FALSE,
    angle = 90,
    vjust = -0.35,
    hjust = 1.05,
    size = 3.3,
    color = "grey35"
  ) +
  scale_color_manual(values = c("Primary education" = paleta[[3]], "Lower secondary education" = paleta[[7]])) +
  scale_x_continuous(
    breaks = sort(unique(repetition_series$year_start)),
    labels = sort(unique(repetition_series$period))
  ) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, suffix = "%")) +
  labs(
    x = "Academic year",
    y = "Repetition rate",
    color = NULL,
    title = "Evolution of school repetition rates in Spain",
    subtitle = "Average across grades within primary and lower secondary education"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.margin = margin(10, 18, 10, 10)
  )

plot_points <- base_plot +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4)

plot_labels <- base_plot +
  geom_line(linewidth = 0.9) +
  geom_label(
    aes(label = label, fill = stage),
    color = "white",
    linewidth = 0,
    size = 2.7,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("Primary education" = paleta[[3]], "Lower secondary education" = paleta[[7]]))

ggsave(
  file.path(graficos, "education_repetition_rates_lines_points.jpeg"),
  plot_points,
  width = 10.5,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(graficos, "education_repetition_rates_lines_labels.jpeg"),
  plot_labels,
  width = 10.5,
  height = 6,
  dpi = 300
)

message("Finished education repetition time-series scraping and plots.")
