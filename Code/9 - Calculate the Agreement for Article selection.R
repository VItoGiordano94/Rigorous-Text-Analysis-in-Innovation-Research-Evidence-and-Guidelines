library(tidyverse)
library(readxl)
library(writexl)

# Set the source workbook for Researcher 1.
researcher_1_file <- "Data/4 - Inter-coder realiability/Input/Articles Selection reviewer 1.xlsx"

# Set the source workbook for Researcher 2.
researcher_2_file <- "Data/4 - Inter-coder realiability/Input/Articles Selection reviewer 2.xlsx"

# Choose the name of the output workbook.
output_file <- "Data/4 - Inter-coder realiability/Output/Cohen_Kappa_Article_Selection.xlsx"

# Read the same three screening criteria from both workbooks.
criteria <- c(
  "Relevance to NLP",
  "Focus on Innovation Management Research",
  "Empirical Articles"
)

# Define a reusable reader for one researcher’s workbook.
read_codings <- function(file) {
  read_excel(file, sheet = "Sheet1", col_types = "text") %>%
    select(EID, Title, all_of(criteria)) %>%
    mutate(
      excel_row = row_number() + 1L,
      across(all_of(criteria), ~ str_to_lower(str_squish(.x)))
    )
}

# Read and standardize Researcher 1’s ratings.
researcher_1 <- read_codings(researcher_1_file)

# Read and standardize Researcher 2’s ratings.
researcher_2 <- read_codings(researcher_2_file)

# Verify that every article is paired one-to-one across workbooks.
pairing_errors <- full_join(
  researcher_1 %>% select(EID, Title),
  researcher_2 %>% select(EID, Title),
  by = "EID",
  suffix = c("_researcher_1", "_researcher_2"),
  relationship = "one-to-one"
) %>%
  filter(
    coalesce(Title_researcher_1, "") != coalesce(Title_researcher_2, "") |
      coalesce(Title_researcher_1, "") == ""
  ) %>%
  summarise(n = n()) %>%
  pull(n)

# Stop when any article identifier or title fails to match.
if (pairing_errors > 0L) {
  rlang::abort("Article EIDs or titles do not match across workbooks")
}

# Join the two researchers’ matched rows into one data set.
paired <- inner_join(
  researcher_1, researcher_2,
  by = "EID",
  suffix = c("_researcher_1", "_researcher_2"),
  relationship = "one-to-one"
)

# Check that every source rating is include or not include.
invalid_ratings <- paired %>%
  select(
    all_of(str_c(criteria, "_researcher_1")),
    all_of(str_c(criteria, "_researcher_2"))
  ) %>%
  pivot_longer(everything(), values_to = "rating") %>%
  mutate(rating = replace_na(rating, "missing")) %>%
  filter(!rating %in% c("include", "not include")) %>%
  summarise(n = n()) %>%
  pull(n)

# Stop when any screening label is missing or unexpected.
if (invalid_ratings > 0L) {
  rlang::abort("Missing or unexpected ratings: expected include/not include")
}

# Derive each researcher's final decision separately.
paired <- paired %>%
  mutate(
    final_researcher_1 = if_else(
      if_all(all_of(str_c(criteria, "_researcher_1")), ~ .x == "include"),
      "include", "not include"
    ),
    final_researcher_2 = if_else(
      if_all(all_of(str_c(criteria, "_researcher_2")), ~ .x == "include"),
      "include", "not include"
    )
  )

# List the final decision and the three individual criteria.
dimensions <- tribble(
  ~field, ~Dimension,
  "final", "Final inclusion decision (all three include)",
  "Relevance to NLP", "Relevance to NLP",
  "Focus on Innovation Management Research",
    "Focus on Innovation Management Research",
  "Empirical Articles", "Empirical Articles"
)

# Extract the source field names in report order.
fields <- dimensions %>% pull(field)

# Reshape the paired data into one row per article and measure.
ratings <- paired %>%
  select(
    EID,
    all_of(str_c(fields, "_researcher_1")),
    all_of(str_c(fields, "_researcher_2"))
  ) %>%
  pivot_longer(
    cols = -EID,
    names_to = c("field", ".value"),
    names_pattern = "^(.*)_(researcher_1|researcher_2)$"
  )

# Count the paired articles and exact agreements.
pairs_per_dimension <- ratings %>% count(field, name = "N")

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

# Apply Cohen's unweighted kappa formula to each measure.
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
    )
  ) %>%
  select(Dimension, N, Agreements, Observed_agreement,
         Expected_agreement, Cohen_kappa)

# Build a contingency table for inspecting each pair of ratings.
contingencies <- ratings %>%
  left_join(dimensions, by = "field") %>%
  count(Dimension, researcher_1, researcher_2, name = "N") %>%
  complete(
    Dimension,
    researcher_1 = c("include", "not include"),
    researcher_2 = c("include", "not include")
  ) %>%
  mutate(N = replace_na(N, 0L))

# Export kappa results, contingency counts, and paired source rows.
write_xlsx(
  list(
    Kappa = results,
    Contingencies = contingencies,
    Paired_articles = paired
  ),
  path = output_file
)
