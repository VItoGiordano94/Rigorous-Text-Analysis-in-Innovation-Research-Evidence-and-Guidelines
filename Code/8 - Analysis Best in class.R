library(tidyverse)
library(readxl)
library(writexl)
library(janitor)

# Import data ---------------------------------------------------------------

# Read the document
paper_sample <- read_xlsx("Data/3 - Validity analysis/Input/Validity Assessed.xlsx") %>%
  clean_names() %>%
  select(-starts_with("x")) # remove columns not initialized

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

# Logical order of the validity.
validity_levels <- c(
  "Construct validity",
  "Statistical conclusion validity",
  "Internal validity",
  "External validity",
  "Stability",
  "Reproducibility"
)

# Best in class -----------------------------------------------------------

# Clean dataset
paper_sample_clean <- paper_sample |> 
  select(doi, source_title, objectives, text_modelling_approach, pre_defined_modelling_approach, ends_with("second_report")) |>
  mutate(across(
    ends_with("_second_report"),
    ~ case_when(str_to_lower(str_trim(as.character(.x))) %in% c("no", "0") ~ 0L,
      str_to_lower(str_trim(as.character(.x))) %in% c("yes", "1") ~ 1L,
      TRUE ~ NA_integer_))) |> 
  rename_with(~ str_remove(.x, "_second_report$")) |> 
  mutate(objectives = recode(
    objectives,
    "Using text to operationalise Innovation constructs" = "Operationalise Innovation constructs",
    "Exploratory content analysis" = "Explore textual content",
    "Explanatory modelling" = "Explain innovation-related phenomena",
    "Predicting innovation-related outcomes" = "Predict innovation-related outcomes"
  ))

# Type of validity per article
paper_sample_dimensions <- paper_sample_clean |> 
  pivot_longer(cols = c(statistical_conclusion_validity, internal_validity, external_validity, construct_validity, stability, reproducibility)) |> 
  filter(value == 1) |> 
  group_by(doi, source_title, objectives, text_modelling_approach, pre_defined_modelling_approach) |> 
  mutate(name = str_replace_all(name, "_", " ")) |> 
  mutate(name = str_to_sentence(name)) |> 
  mutate(name = factor(name, levels = validity_levels)) |> 
  arrange(name) |> 
  mutate(group = if_else(name %in% c("Reproducibility", "Stability"), "Reliability", "Validity")) |>
  group_by(doi, source_title, objectives, text_modelling_approach, pre_defined_modelling_approach, group) |>
  summarise(rigor = str_c(name, collapse = "; ")) |> 
  ungroup() |> 
  pivot_wider(names_from = group, values_from = rigor) |> 
  mutate(across(everything(), ~ replace_na(as.character(.x), ""))) |> 
  mutate(Rigor = str_c(Validity, "; ", Reliability)) |> 
  mutate(Rigor = str_remove(Rigor, ";$")) |> 
  mutate(Rigor = str_remove(Rigor, "; ")) 
  
  
# Number of validity per article
paper_sample_final <- paper_sample_clean |> 
  mutate(n_assessed = statistical_conclusion_validity + internal_validity + external_validity + construct_validity + reproducibility + stability) |> 
  mutate(n_assessed_validity = statistical_conclusion_validity + internal_validity + external_validity + construct_validity) |> 
  mutate(n_assessed_reliability = reproducibility + stability) |> 
  select(-statistical_conclusion_validity, -internal_validity, -external_validity,
         - construct_validity, -reproducibility, -stability) |> 
  mutate(across(everything(), ~ replace_na(as.character(.x), ""))) |> 
  left_join(paper_sample_dimensions) |> 
  mutate(across(everything(), ~ replace_na(as.character(.x), ""))) |> 
  arrange(desc(n_assessed)) |> 
  select(doi, "Journal" = source_title, 
         "Research objective" = objectives,
         "Text Modelling approach" = text_modelling_approach,
         "Predefined vs Customised approach" = pre_defined_modelling_approach,
         "Number of Rigor dimension" = n_assessed,
         "Number of Validity dimension" = n_assessed_validity,
         "Number of Reliability Dimension" = n_assessed_reliability,
         "Rigor dimension" = Rigor, 
         "Validity dimension" = Validity,
         "Reliability dimension" = Reliability)
  
write_xlsx(paper_sample_final, "Data/3 - Validity analysis/Output//descriptive statistics/Best in class.xlsx")
