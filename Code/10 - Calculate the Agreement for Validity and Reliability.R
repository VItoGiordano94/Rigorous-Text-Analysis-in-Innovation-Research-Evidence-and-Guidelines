library(tidyverse)
library(readxl)
library(writexl)

# Set the source workbook for Researcher 1.
researcher_1_file <- "Data/4 - Inter-coder realiability/Input/Articles & Rigor reviewer 1.xlsx"

# Set the source workbook for Researcher 2.
researcher_2_file <- "Data/4 - Inter-coder realiability/Input/Articles & Rigor reviewer 2.xlsx"

# Choose the name of the output workbook.
output_file <- "Data/4 - Inter-coder realiability/Output/Cohen_Kappa_Validity.xlsx"

# Select the corresponding Excel columns from each source workbook.
field_names <- c(
  "eid", "title", "objectives", "approach", "predefined", "validity",
  "reliability", "primary_construct", "primary_statistical",
  "primary_internal", "primary_external", "primary_stability",
  "primary_reproducibility", "secondary_construct",
  "secondary_statistical", "secondary_internal", "secondary_external",
  "secondary_stability", "secondary_reproducibility"
)

# Define a reusable reader for one researcher’s workbook.
read_codings <- function(file, columns) {
  read_excel(file, sheet = "Sheet1", skip = 2, col_names = FALSE,
             col_types = "text") %>%
    select(all_of(columns)) %>%
    set_names(field_names) %>%
    mutate(
      across(everything(), str_squish),
      excel_row = row_number() + 2L
    ) %>%
    group_by(eid) %>%
    mutate(technique_number = row_number()) %>%
    ungroup()
}

# Read and standardize Researcher 1’s ratings.
researcher_1 <- read_codings(
  researcher_1_file,
  # Map the validity columns in Researcher 1’s workbook.
  c(1, 2, 9, 10, 11, 16, 17, 27, 24, 25, 26, 28, 29, 33, 30, 31, 32, 34, 35)
)

# Read and standardize Researcher 2’s ratings.
researcher_2 <- read_codings(
  researcher_2_file,
  # Map the same validity columns in Researcher 2’s workbook.
  c(2, 4, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
)

# Verify the one-to-one pairing before comparing any ratings.
pairing_errors <- full_join(
  researcher_1 %>% select(eid, technique_number, title),
  researcher_2 %>% select(eid, technique_number, title),
  by = c("eid", "technique_number"),
  suffix = c("_researcher_1", "_researcher_2"),
  relationship = "one-to-one"
) %>%
  filter(
    coalesce(title_researcher_1, "") != coalesce(title_researcher_2, "") |
      coalesce(title_researcher_1, "") == ""
  ) %>%
  summarise(n = n()) %>%
  pull(n)

# Stop when any article identifier, title, or technique position fails to match.
if (pairing_errors > 0L) {
  rlang::abort("Article IDs, technique positions, or titles do not match")
}

# Harmonize labels that mean the same thing in the two workbooks.
paired <- inner_join(
  researcher_1, researcher_2,
  by = c("eid", "technique_number"),
  suffix = c("_researcher_1", "_researcher_2"),
  relationship = "one-to-one"
) %>%
  mutate(
    objectives_researcher_1 = case_match(
      objectives_researcher_1,
      "Exploratory content analysis" ~ "Explore textual content",
      "Explanatory modelling" ~ "Explain innovation-related phenomena",
      "Predicting innovation-related outcomes" ~
        "Predict innovation-related outcomes",
      "Using text to operationalise Innovation constructs" ~
        "Operationalize Innovation constructs",
      .default = NA_character_
    ),
    approach_researcher_1 = case_match(
      approach_researcher_1,
      "Word count" ~ "Word Count",
      "Large Language Model" ~ "Large Language Models (LLMs)",
      .default = approach_researcher_1
    ),
    predefined_researcher_1 = replace_na(predefined_researcher_1, "Not applicable"),
    predefined_researcher_2 = replace_na(predefined_researcher_2, "Not applicable")
  )

# Define the 17 requested dimensions and their report sections.
dimensions <- tribble(
  ~Section, ~Dimension, ~field,
  "Classification", "Objectives", "objectives",
  "Classification", "Text modelling approach", "approach",
  "Classification", "Pre-defined modelling approach", "predefined",
  "Primary report", "Validity", "validity",
  "Primary report", "Reliability", "reliability",
  "Primary report", "Construct Validity", "primary_construct",
  "Primary report", "Statistical Conclusion Validity", "primary_statistical",
  "Primary report", "Internal Validity", "primary_internal",
  "Primary report", "External Validity", "primary_external",
  "Primary report", "Stability", "primary_stability",
  "Primary report", "Reproducibility", "primary_reproducibility",
  "Secondary report", "Construct Validity", "secondary_construct",
  "Secondary report", "Statistical Conclusion Validity", "secondary_statistical",
  "Secondary report", "Internal Validity", "secondary_internal",
  "Secondary report", "External Validity", "secondary_external",
  "Secondary report", "Stability", "secondary_stability",
  "Secondary report", "Reproducibility", "secondary_reproducibility"
)

# Reshape the paired columns into one row per technique and dimension.
fields <- dimensions %>% pull(field)

# Reshape the paired data into one row per technique and dimension.
ratings <- paired %>%
  select(
    eid, technique_number,
    all_of(str_c(fields, "_researcher_1")),
    all_of(str_c(fields, "_researcher_2"))
  ) %>%
  pivot_longer(
    cols = -c(eid, technique_number),
    names_to = c("field", ".value"),
    names_pattern = "^(.*)_(researcher_1|researcher_2)$"
  ) %>%
  drop_na(researcher_1, researcher_2)

# Count the paired observations and exact agreements.
pairs_per_dimension <- ratings %>% count(field, name = "N")

# Count exact matches between the two researchers.
agreements <- ratings %>%
  filter(researcher_1 == researcher_2) %>%
  count(field, name = "Agreements")

# Calculate chance agreement from each researcher's category frequencies.
researcher_1_marginals <- ratings %>%
  count(field, category = researcher_1, name = "n_researcher_1")

# Count each category assigned by Researcher 2.
researcher_2_marginals <- ratings %>%
  count(field, category = researcher_2, name = "n_researcher_2")

# Combine the two sets of category marginals to estimate chance agreement.
expected_agreement_counts <- full_join(
  researcher_1_marginals, researcher_2_marginals,
  by = c("field", "category")
) %>%
  mutate(
    n_researcher_1 = replace_na(n_researcher_1, 0L),
    n_researcher_2 = replace_na(n_researcher_2, 0L)
  ) %>%
  group_by(field) %>%
  summarise(marginal_products = sum(n_researcher_1 * n_researcher_2),
            .groups = "drop")

# Apply Cohen's unweighted kappa formula to each dimension.
results <- dimensions %>%
  left_join(pairs_per_dimension, by = "field") %>%
  left_join(agreements, by = "field") %>%
  left_join(expected_agreement_counts, by = "field") %>%
  mutate(
    Agreements = replace_na(Agreements, 0L),
    Observed_agreement = Agreements / N,
    Expected_agreement = marginal_products / N^2,
    Cohen_kappa = if_else(
      Expected_agreement == 1,
      NA_real_,
      (Observed_agreement - Expected_agreement) /
        (1 - Expected_agreement)
    ),
    Note = if_else(
      Expected_agreement == 1,
      "Undefined: both researchers use only one category",
      ""
    )
  ) %>%
  select(Section, Dimension, N, Agreements, Observed_agreement,
         Expected_agreement, Cohen_kappa, Note)

# Export the resulting table to a new Excel workbook.
write_xlsx(list(Kappa = results), path = output_file)
