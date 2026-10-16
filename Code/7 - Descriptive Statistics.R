library(tidyverse)
library(readxl)
library(writexl)
library(janitor)

# Import data ---------------------------------------------------------------

# Read the document
paper_sample <- read_xlsx("Data/3 - Validity analysis/Input/Validity Assessed.xlsx") %>%
  clean_names() %>%
  select(-starts_with("x")) # remove columns not initialized

# Select output directory
output_dir <- "Data/3 - Validity analysis/Output/descriptive statistics/"

# Orders and classifications ------------------------------------------------

# Required order of the text-modelling approaches.
approach_levels <- c(
  "Word count",
  "Dictionary-based approach",
  "Representation-based machine learning",
  "Large Language Model",
  "Topic modelling",
  "Text similarity",
  "Text clustering"
)

# Logical order of the research objectives.
objective_levels <- c("Operationalise Innovation constructs",
                      "Explore textual content",
                      "Explain innovation-related phenomena",
                      "Predict innovation-related outcomes")

# ABS/AJG ranking used to order journals: highest rating first, then
# alphabetical order within each rating. Update this lookup if you use a
# different edition of the ABS/AJG guide.
source_abs <- tribble(
  ~source_title,                                      ~abs_star, ~abs_order,
  "Academy of Management Journal",                    "4*",      1,
  "Management Science",                              "4*",      1,
  "Organization Science",                            "4*",      1,
  "Research Policy",                                 "4*",      1,
  "Strategic Management Journal",                    "4*",      1,
  "Journal of Product Innovation Management",        "4",       2,
  "Industry and Innovation",                         "3",       3,
  "Journal of Technology Transfer",                  "3",       3,
  "R and D Management",                              "3",       3,
  "Technological Forecasting and Social Change",     "3",       3,
  "Technovation",                                    "3",       3
)

source_levels <- source_abs %>%
  arrange(abs_order, source_title) %>%
  pull(source_title)

# Data cleaning and standardisation -----------------------------------------

paper_sample <- paper_sample %>%
  mutate(
    eid = as.character(eid),
    year = as.integer(year),
    source_title = str_squish(source_title),
    objectives = str_squish(objectives),
    text_modelling_approach = str_squish(text_modelling_approach),
    pre_defined_modelling_approach = str_squish(pre_defined_modelling_approach),
    
    # Harmonise labels that differ only because of extra spaces or spelling.
    text_modelling_approach = case_when(
      text_modelling_approach == "Dictionary-based approach" ~
        "Dictionary-based approach",
      text_modelling_approach == "Text clustering" ~
        "Text clustering",
      TRUE ~ text_modelling_approach
    ),
    
    pre_defined_modelling_approach = case_when(
      pre_defined_modelling_approach == "Pre-defined" ~ "Pre-defined",
      pre_defined_modelling_approach == "Customized" ~ "Customized",
      TRUE ~ pre_defined_modelling_approach
    )
  ) %>%
  filter(!is.na(eid), eid != "")


# Trend Analysis ----------------------------------------------------------

# Trend in general
paper_year <- paper_sample |> 
  select(eid, year) |> 
  unique() |> 
  count(year) |> 
  mutate(perc = n/sum(n)*100) |> 
  filter(year < 2026)

# Year range
year_levels <- seq(
  min(paper_sample$year, na.rm = TRUE),
  max(paper_sample$year, na.rm = TRUE)
)

# Scaling factor for the secondary axis
total_articles <- paper_sample |> 
  select(eid, year) |> 
  unique() |> 
  nrow()

# ggplot
paper_year |> 
  ggplot(aes(x = year)) +
  geom_line(aes(y = n, linetype = "Number"), linewidth = 0.8, show.legend = F) +
  geom_point(aes(y = n, shape = "Number"), size = 2, show.legend = F) +
  scale_x_continuous(breaks = year_levels) +
  scale_y_continuous(name = "Number of articles", n.breaks = 10, 
                     sec.axis = dup_axis(name = "Percentage of articles",
                       labels = function(x) {scales::percent(x / total_articles, accuracy = 1)})) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.right = element_text(face = "bold"),
    axis.title.y.left = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  xlab(NULL)

ggsave(filename = "Data/3 - Validity analysis/Output/descriptive statistics/01_article_trend_by_year.png", width = 7)

# Paper per source
paper_year_source <- paper_sample |> 
  distinct(eid, year, source_title) |> 
  count(year, source_title, name = "n") |> 
  complete(year = year_levels, source_title = source_levels, fill = list(n = 0)) |> 
  mutate(perc = n / total_articles * 100,
         n_perc = str_c(n, "\n(", round(perc, 2), "%)"),
         text_col = if_else(n > max(n) / 2, "white", "black"),
         source_title = factor(source_title, levels = rev(source_levels)))

# ggplot
ggplot(paper_year_source, aes(x = factor(year, levels = year_levels), y = source_title, fill = n)) +
  geom_tile(color = "black", linewidth = 0.5) +
  geom_text(aes(label = n_perc, color = text_col), size = 3, lineheight = 0.9) +
  scale_fill_gradient(low = "white", high = "navy", name = "Number of articles\n") +
  scale_color_identity() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave(filename = "Data/3 - Validity analysis/Output/descriptive statistics/02_article_trend_by_year_source.png", width = 12.8, height = 4.5)

# Trend per approach
paper_year_approach <- paper_sample |> 
  distinct(eid, year, text_modelling_approach) |> 
  count(year, text_modelling_approach, name = "n") |> 
  complete(year = year_levels, text_modelling_approach = approach_levels, fill = list(n = 0)) |> 
  mutate(perc = n / total_articles * 100,
         n_perc = str_c(n, "\n(", round(perc, 2), "%)"),
         text_col = if_else(n > max(n, na.rm = TRUE) / 2, "white", "black"),
         text_modelling_approach = factor(text_modelling_approach, levels = rev(approach_levels)))

# ggplot
ggplot(paper_year_approach, aes(x = factor(year, levels = year_levels),
                                y = text_modelling_approach, fill = n)) +
  geom_tile(color = "grey50", linewidth = 0.4) +
  geom_text(aes(label = n_perc, color = text_col), size = 3, lineheight = 0.9) +
  scale_fill_gradient(
    low = "white",
    high = "navy",
    name = "Number of articles\n",
    breaks = seq(
      0,
      ceiling(max(paper_year_approach$n, na.rm = TRUE) / 5) * 5,
      by = 5
    )
  ) +
  scale_color_identity() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "top")


ggsave(filename = "Data/3 - Validity analysis/Output/descriptive statistics/03_article_trend_by_year_NLP.png", width = 12.8, height = 4.5)


# Research objecrives
paper_year_objective <- paper_sample |> 
  mutate(objectives = recode(
    objectives,
    "Using text to operationalise Innovation constructs" = "Operationalise Innovation constructs",
    "Exploratory content analysis" = "Explore textual content",
    "Explanatory modelling" = "Explain innovation-related phenomena",
    "Predicting innovation-related outcomes" = "Predict innovation-related outcomes"
  )) |>
  distinct(eid, year, objectives) |> 
  count(year, objectives, name = "n") |> 
  complete(year = year_levels, objectives = objective_levels, fill = list(n = 0)) |> 
  mutate(perc = n / total_articles * 100,
         n_perc = str_c(n, "\n(", round(perc, 2), "%)"),
         text_col = if_else(n > max(n, na.rm = TRUE) / 2, "white", "black"),
         objectives = factor(objectives, levels = rev(objective_levels)))

ggplot(paper_year_objective, aes(x = factor(year, levels = year_levels),
                                 y = objectives, fill = n)) +
  geom_tile(color = "grey50", linewidth = 0.4) +
  geom_text(aes(label = n_perc, color = text_col), size = 3, lineheight = 0.9) +
  scale_fill_gradient(low = "white", high = "navy",
                      name = "Number of articles",
                      breaks = seq(0, ceiling(max(paper_year_objective$n, na.rm = TRUE) / 5) * 5, by = 5)) +
  scale_color_identity() +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave(filename = "Data/3 - Validity analysis/Output/descriptive statistics/04_article_trend_by_year_objective.png", width = 12, height = 3)



# Journal -----------------------------------------------------------------

# Source x Obecctive
paper_source_objective <- paper_sample |> 
  rename(research_objective = objectives) |> 
  mutate(research_objective = recode(research_objective,
                                     "Using text to operationalise Innovation constructs" = "Operationalise Innovation constructs",
                                     "Exploratory content analysis" = "Explore textual content",
                                     "Explanatory modelling" = "Explain innovation-related phenomena",
                                     "Predicting innovation-related outcomes" = "Predict innovation-related outcomes",
                                     .default = research_objective)) |> 
  distinct(eid, source_title, research_objective) |> 
  count(source_title, research_objective, name = "n") |> 
  complete(source_title = source_levels, research_objective = objective_levels, fill = list(n = 0)) |> 
  mutate(perc = n / total_articles * 100,
         n_perc = str_c(n, "\n(", round(perc, 2), "%)"),
         text_col = if_else(n > max(n, na.rm = TRUE) / 2, "white", "black"),
         source_title = factor(source_title, levels = rev(source_levels)),
         research_objective = factor(research_objective, levels = objective_levels))

ggplot(paper_source_objective,
       aes(x = research_objective, y = source_title, fill = n)) +
  geom_tile(color = "grey50", linewidth = 0.4) +
  geom_text(aes(label = n_perc, color = text_col), size = 3, lineheight = 0.9) +
  scale_fill_gradient(low = "white", high = "navy", name = "Number of articles",
                      breaks = seq(0, ceiling(max(paper_source_objective$n) / 5) * 5, by = 5)) +
  scale_color_identity() +
  scale_x_discrete(labels = \(x) str_wrap(x, width = 22)) +
  labs(title = "Scientific articles by source and research objective",
       x = "Research objective", y = "Source") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0, vjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "top")

paper_sample_excel <- paper_sample |> 
  rename(research_objective = objectives) |> 
  mutate(research_objective = recode(
    research_objective,
    "Using text to operationalise Innovation constructs" = "Operationalise Innovation constructs",
    "Exploratory content analysis" = "Explore textual content",
    "Explanatory modelling" = "Explain innovation-related phenomena",
    "Predicting innovation-related outcomes" = "Predict innovation-related outcomes",
    .default = research_objective
  ))

make_excel_table <- function(data, row, column, row_levels, column_levels) {
  
  cells <- data |> 
    filter(!is.na(.data[[row]]), !is.na(.data[[column]])) |> 
    distinct(eid, across(all_of(c(row, column)))) |> 
    count(across(all_of(c(row, column))), name = "n")
  
  cells <- expand_grid(
    !!!setNames(list(row_levels, column_levels), c(row, column))
  ) |> 
    left_join(cells, by = c(row, column)) |> 
    mutate(n = replace_na(n, 0L),
           value = sprintf("%d (%.2f%%)", n, n / total_articles * 100)) |> 
    select(all_of(c(row, column)), value) |> 
    pivot_wider(names_from = all_of(column), values_from = value)
  
  row_totals <- data |> 
    filter(!is.na(.data[[row]])) |> 
    distinct(eid, across(all_of(row))) |> 
    count(across(all_of(row)), name = "n") |> 
    mutate(Total = sprintf("%d (%.2f%%)", n, n / total_articles * 100)) |> 
    select(all_of(row), Total)
  
  column_totals <- data |> 
    filter(!is.na(.data[[column]])) |> 
    distinct(eid, across(all_of(column))) |> 
    count(across(all_of(column)), name = "n")
  
  total_row <- tibble(!!column := column_levels) |> 
    left_join(column_totals, by = column) |> 
    mutate(n = replace_na(n, 0L),
           value = sprintf("%d (%.2f%%)", n, n / total_articles * 100)) |> 
    select(all_of(column), value) |> 
    pivot_wider(names_from = all_of(column), values_from = value) |> 
    mutate(!!row := "Total", .before = 1,
           Total = sprintf("%d (100.00%%)", total_articles))
  
  cells |> 
    left_join(row_totals, by = row) |> 
    bind_rows(total_row)
}



source_objective <- make_excel_table(
  paper_sample_excel, "source_title", "research_objective",
  source_levels, objective_levels
)

source_approach <- make_excel_table(
  paper_sample_excel, "source_title", "text_modelling_approach",
  source_levels, approach_levels
)

write_xlsx(source_objective, "Data/3 - Validity analysis/Output/descriptive statistics/1 - Objectives and Journal.xlsx")

write_xlsx(source_approach, "Data/3 - Validity analysis/Output/descriptive statistics/2 - Approach and Journal.xlsx")


# Pre-defined -------------------------------------------------------------

supervised_approaches <- c(
  "Dictionary-based approach",
  "Representation-based machine learning",
  "Large Language Model"
)

approach_type_table <- paper_sample |>
  mutate(
    text_modelling_approach = str_squish(text_modelling_approach),
    modelling_type = case_when(
      str_to_lower(pre_defined_modelling_approach) == "pre-defined" ~ "Pre-defined",
      str_to_lower(pre_defined_modelling_approach) %in%
        c("customized", "customised") ~ "Customised",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    text_modelling_approach %in% supervised_approaches,
    !is.na(modelling_type)
  ) |>
  distinct(eid, text_modelling_approach, modelling_type) |>
  count(text_modelling_approach, modelling_type, name = "n") |>
  complete(
    text_modelling_approach = supervised_approaches,
    modelling_type = c("Pre-defined", "Customised"),
    fill = list(n = 0)
  ) |>
  group_by(text_modelling_approach) |>
  mutate(
    total = sum(n),
    percentage = if_else(total > 0, n / total * 100, 0),
    value = sprintf("%d (%.2f%%)", n, percentage)
  ) |>
  ungroup() |>
  select(text_modelling_approach, modelling_type, value, total) |>
  pivot_wider(names_from = modelling_type, values_from = value) |>
  mutate(
    Total = sprintf("%d\n(100.00%%)", total),
    text_modelling_approach = factor(
      text_modelling_approach,
      levels = supervised_approaches
    )
  ) |>
  arrange(text_modelling_approach) |>
  select(
    `Text modelling approach` = text_modelling_approach,
    `Pre-defined`,
    Customised,
    Total
  )

write_xlsx(approach_type_table, "Data/3 - Validity analysis/Output/descriptive statistics/3 - Approach and predefined.xlsx")













# Check whether repeated EIDs have inconsistent article-level metadata.
metadata_check <- paper_sample %>%
  group_by(eid) %>%
  summarise(
    n_years = n_distinct(year, na.rm = TRUE),
    n_sources = n_distinct(source_title, na.rm = TRUE),
    n_objectives = n_distinct(objectives, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_years > 1 | n_sources > 1 | n_objectives > 1)

if (nrow(metadata_check) > 0) {
  warning(
    nrow(metadata_check),
    " repeated EID(s) have inconsistent year, source, or objective values."
  )
}

# Article-level dataset: one row per scientific article.
articles <- paper_sample %>%
  arrange(eid) %>%
  distinct(eid, .keep_all = TRUE) %>%
  mutate(
    source_title = factor(source_title, levels = source_levels),
    objectives = factor(objectives, levels = objective_levels)
  )

# Article-approach dataset: one row per article and modelling approach.
# This preserves multiple approaches for the same article but removes exact
# duplicate article-approach observations.
article_approaches <- paper_sample %>%
  filter(!is.na(text_modelling_approach), text_modelling_approach != "") %>%
  distinct(
    eid,
    year,
    source_title,
    objectives,
    text_modelling_approach,
    pre_defined_modelling_approach
  ) %>%
  mutate(
    source_title = factor(source_title, levels = source_levels),
    objectives = factor(objectives, levels = objective_levels),
    text_modelling_approach = factor(
      text_modelling_approach,
      levels = approach_levels
    )
  )

unknown_approaches <- article_approaches %>%
  filter(is.na(text_modelling_approach))

if (nrow(unknown_approaches) > 0) {
  stop(
    "At least one text-modelling approach is not included in `approach_levels`."
  )
}



total_articles <- n_distinct(articles$eid)

# 1. Article trend by year ---------------------------------------------------
# Percentage = share of all unique articles in the dataset.

article_year <- articles %>%
  count(year, name = "n_articles") %>%
  complete(year = year_levels, fill = list(n_articles = 0L)) %>%
  mutate(
    percentage = round(100 * n_articles / total_articles, 2)
  ) %>%
  arrange(year)

article_year_plot_data <- bind_rows(
  article_year %>%
    transmute(
      year,
      measure = "Number of articles",
      value = as.numeric(n_articles),
      label = as.character(n_articles)
    ),
  article_year %>%
    transmute(
      year,
      measure = "Percentage of all articles (%)",
      value = percentage,
      label = paste0(format(percentage, nsmall = 1), "%")
    )
) %>%
  mutate(
    measure = factor(
      measure,
      levels = c(
        "Number of articles",
        "Percentage of all articles (%)"
      )
    )
  )

article_year_figure <- ggplot(
  article_year_plot_data,
  aes(x = year, y = value, group = 1)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_text(
    aes(label = label),
    vjust = -0.7,
    size = 3,
    check_overlap = TRUE
  ) +
  facet_wrap(~measure, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = year_levels) +
  labs(
    title = "Trend of scientific articles by publication year",
    x = "Publication year",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave(
  filename = file.path(output_dir, "01_article_trend_by_year.png"),
  plot = article_year_figure,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)

# Common annual denominators ------------------------------------------------

year_totals <- articles %>%
  count(year, name = "total_articles_in_year")

# 2. Article trend by year and source ---------------------------------------
# Percentage = percentage of unique articles in that year published by the
# source. Percentages sum to 100% within each year.

source_year_counts <- articles %>%
  count(year, source_title, name = "n_articles")

source_year <- crossing(
  year = year_levels,
  source_title = source_levels
) %>%
  mutate(source_title = factor(source_title, levels = source_levels)) %>%
  left_join(source_abs, by = "source_title") %>%
  left_join(source_year_counts, by = c("year", "source_title")) %>%
  left_join(year_totals, by = "year") %>%
  mutate(
    n_articles = replace_na(n_articles, 0L),
    total_articles_in_year = replace_na(total_articles_in_year, 0L),
    percentage_within_year = if_else(
      total_articles_in_year > 0,
      round(100 * n_articles / total_articles_in_year, 2),
      0
    )
  ) %>%
  arrange(abs_order, source_title, year) %>%
  select(
    abs_star,
    source_title,
    year,
    n_articles,
    percentage_within_year
  )

# 3. Article trend by year and text-modelling approach ----------------------
# Percentage = percentage of unique articles in that year using the approach.
# Because one article can use multiple approaches, percentages can sum to more
# than 100% within a year.

approach_year_counts <- article_approaches %>%
  distinct(eid, year, text_modelling_approach) %>%
  count(year, text_modelling_approach, name = "n_articles")

approach_year <- crossing(
  year = year_levels,
  text_modelling_approach = approach_levels
) %>%
  mutate(
    text_modelling_approach = factor(
      text_modelling_approach,
      levels = approach_levels
    )
  ) %>%
  left_join(
    approach_year_counts,
    by = c("year", "text_modelling_approach")
  ) %>%
  left_join(year_totals, by = "year") %>%
  mutate(
    n_articles = replace_na(n_articles, 0L),
    total_articles_in_year = replace_na(total_articles_in_year, 0L),
    percentage_within_year = if_else(
      total_articles_in_year > 0,
      round(100 * n_articles / total_articles_in_year, 2),
      0
    )
  ) %>%
  arrange(text_modelling_approach, year) %>%
  select(
    text_modelling_approach,
    year,
    n_articles,
    percentage_within_year
  )

# 4. Source and objective ---------------------------------------------------
# Percentage = percentage of the source's unique articles assigned to the
# objective. Percentages sum to 100% within each source.

source_totals <- articles %>%
  count(source_title, name = "total_articles_in_source")

source_objective_counts <- articles %>%
  distinct(eid, source_title, objectives) %>%
  count(source_title, objectives, name = "n_articles")

source_objective <- crossing(
  source_title = source_levels,
  objectives = objective_levels
) %>%
  mutate(
    source_title = factor(source_title, levels = source_levels),
    objectives = factor(objectives, levels = objective_levels)
  ) %>%
  left_join(source_abs, by = "source_title") %>%
  left_join(
    source_objective_counts,
    by = c("source_title", "objectives")
  ) %>%
  left_join(source_totals, by = "source_title") %>%
  mutate(
    n_articles = replace_na(n_articles, 0L),
    percentage_within_source = round(
      100 * n_articles / total_articles_in_source,
      2
    )
  ) %>%
  arrange(abs_order, source_title, objectives) %>%
  select(
    abs_star,
    source_title,
    objectives,
    n_articles,
    percentage_within_source
  )

# 5. Source and text-modelling approach -------------------------------------
# Percentage = percentage of the source's unique articles using the approach.
# Because one article can use multiple approaches, percentages can sum to more
# than 100% within a source.

source_approach_counts <- article_approaches %>%
  distinct(eid, source_title, text_modelling_approach) %>%
  count(source_title, text_modelling_approach, name = "n_articles")

source_approach <- crossing(
  source_title = source_levels,
  text_modelling_approach = approach_levels
) %>%
  mutate(
    source_title = factor(source_title, levels = source_levels),
    text_modelling_approach = factor(
      text_modelling_approach,
      levels = approach_levels
    )
  ) %>%
  left_join(source_abs, by = "source_title") %>%
  left_join(
    source_approach_counts,
    by = c("source_title", "text_modelling_approach")
  ) %>%
  left_join(source_totals, by = "source_title") %>%
  mutate(
    n_articles = replace_na(n_articles, 0L),
    percentage_within_source = round(
      100 * n_articles / total_articles_in_source,
      2
    )
  ) %>%
  arrange(abs_order, source_title, text_modelling_approach) %>%
  select(
    abs_star,
    source_title,
    text_modelling_approach,
    n_articles,
    percentage_within_source
  )

# 6. Approach and pre-defined/customized classification ---------------------
# Only Dictionary, machine learning, and LLM are included, as requested.
# Percentage = percentage within each text-modelling approach.

definition_approaches <- c(
  "Dictionary-based approach",
  "Representation-based machine learning",
  "Large Language Model"
)

definition_levels <- c("Pre-defined", "Customized")

approach_definition_counts <- article_approaches %>%
  filter(
    text_modelling_approach %in% definition_approaches,
    pre_defined_modelling_approach %in% definition_levels
  ) %>%
  distinct(
    eid,
    text_modelling_approach,
    pre_defined_modelling_approach
  ) %>%
  count(
    text_modelling_approach,
    pre_defined_modelling_approach,
    name = "n_articles"
  )

approach_definition_totals <- article_approaches %>%
  filter(
    text_modelling_approach %in% definition_approaches,
    pre_defined_modelling_approach %in% definition_levels
  ) %>%
  distinct(
    eid,
    text_modelling_approach,
    pre_defined_modelling_approach
  ) %>%
  count(
    text_modelling_approach,
    name = "total_classified_articles"
  )

approach_definition <- crossing(
  text_modelling_approach = definition_approaches,
  approach_definition = definition_levels
) %>%
  mutate(
    text_modelling_approach = factor(
      text_modelling_approach,
      levels = approach_levels
    )
  ) %>%
  left_join(
    approach_definition_counts,
    by = c(
      "text_modelling_approach",
      "approach_definition" = "pre_defined_modelling_approach"
    )
  ) %>%
  left_join(
    approach_definition_totals,
    by = "text_modelling_approach"
  ) %>%
  mutate(
    n_articles = replace_na(n_articles, 0L),
    total_classified_articles = replace_na(total_classified_articles, 0L),
    percentage_within_approach = if_else(
      total_classified_articles > 0,
      round(100 * n_articles / total_classified_articles, 2),
      0
    )
  ) %>%
  arrange(
    text_modelling_approach,
    factor(approach_definition, levels = definition_levels)
  ) %>%
  select(
    text_modelling_approach,
    approach_definition,
    n_articles,
    percentage_within_approach
  )

# 7. Combinations of text-modelling approaches used together ----------------
# Only articles using at least two distinct approaches are shown.

article_combinations <- article_approaches %>%
  distinct(eid, text_modelling_approach) %>%
  arrange(eid, text_modelling_approach) %>%
  group_by(eid) %>%
  summarise(
    n_approaches = n_distinct(text_modelling_approach),
    approach_combination = str_c(
      as.character(text_modelling_approach),
      collapse = " + "
    ),
    .groups = "drop"
  ) %>%
  filter(n_approaches >= 2)

n_multi_approach_articles <- n_distinct(article_combinations$eid)

approach_combinations <- article_combinations %>%
  count(
    n_approaches,
    approach_combination,
    name = "n_articles"
  ) %>%
  mutate(
    percentage_of_all_articles = round(
      100 * n_articles / total_articles,
      2
    ),
    percentage_of_multi_approach_articles =
      if (n_multi_approach_articles > 0) {
        round(
          100 * n_articles / n_multi_approach_articles,
          2
        )
      } else {
        rep(0, n())
      }
  ) %>%
  arrange(
    desc(n_articles),
    n_approaches,
    approach_combination
  )
# Notes explaining denominators and duplicate handling ----------------------

notes <- tribble(
  ~item, ~definition,
  "Unit of analysis", paste0(
    "The input contains ", nrow(paper_sample),
    " rows and ", total_articles,
    " unique articles. Repeated EIDs are retained only when an approach-level analysis is required."
  ),
  "Year percentage", "Percentage of all unique articles in the complete dataset.",
  "Year x source percentage", "Percentage of unique articles in the publication year.",
  "Year x approach percentage", "Percentage of unique articles in the publication year using the approach; totals may exceed 100% because articles can use multiple approaches.",
  "Source x objective percentage", "Percentage of unique articles within the source.",
  "Source x approach percentage", "Percentage of unique articles within the source using the approach; totals may exceed 100% because articles can use multiple approaches.",
  "Approach x definition percentage", "Percentage within the selected modelling approach among articles classified as Pre-defined or Customized.",
  "Approach-combination percentage", "The table contains only articles using two or more distinct approaches and reports percentages of all articles and of multi-approach articles.",
  "Source order", "Sources are ordered by ABS/AJG rating (4*, 4, 3) and alphabetically within each rating."
)

# Export all tables to one Excel workbook -----------------------------------

output_tables <- list(
  "Notes" = notes,
  "Year trend" = article_year,
  "Year x source" = source_year,
  "Year x approach" = approach_year,
  "Source x objective" = source_objective,
  "Source x approach" = source_approach,
  "Approach x definition" = approach_definition,
  "Approach combinations" = approach_combinations
)

write_xlsx(
  output_tables,
  path = file.path(output_dir, "descriptive_statistics_tables.xlsx")
)

# Optional console confirmation --------------------------------------------

message("Saved tables to: ", file.path(
  output_dir,
  "descriptive_statistics_tables.xlsx"
))
message("Saved figure to: ", file.path(
  output_dir,
  "01_article_trend_by_year.png"
))

