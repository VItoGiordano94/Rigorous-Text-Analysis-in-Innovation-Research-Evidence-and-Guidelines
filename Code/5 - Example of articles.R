library(tidyverse)
library(readxl)
library(writexl)
library(janitor)

# Import data -------------------------------------------------------------

# Import included papers dataset
paper_sample <- read_xlsx("Data/3 - Validity analysis/Input/Validity Assessed - Version 1.xlsx") %>% 
  clean_names() %>% 
  select(-starts_with("x")) # remove columns not initialized

# Import citations per article
paper_dataset_citation <- read_csv("Data/2 - Article Selection/Input/All articles.csv") |> 
  clean_names()

# Data preprocessing ------------------------------------------------------

# Select useful column
paper_sample_clean <- paper_sample |> 
  select(doi, innovation_area, objectives, text_modelling_approach, theoretical_construct)


# Select useful column
paper_dataset_citation_clean <- paper_dataset_citation |> 
  select(doi, cited_by)


# Merge all
paper_dataset <- paper_sample_clean |> 
  left_join(paper_dataset_citation_clean) |> 
  arrange(desc(cited_by)) |> 
  head(21) |> 
  mutate("Main Purpose" = "") |> 
  select(Authors = doi, "Main Purpose", text_modelling_approach, objectives, theoretical_construct)

paper_dataset |> write_xlsx("Data/3 - Validity analysis/Wip/Illustrative examples.xlsx")
