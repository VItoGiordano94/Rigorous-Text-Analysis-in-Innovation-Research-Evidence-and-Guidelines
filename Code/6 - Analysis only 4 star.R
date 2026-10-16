library(tidyverse)
library(readxl)
library(writexl)
library(janitor)

# Import data -------------------------------------------------------------

# Import Inclusion and exclusion criteria dataset
paper_query <- read_xlsx("Data/2 - Article Selection/Ouput/Reviewed Articles.xlsx") %>% 
  clean_names() |> 
  filter(source_title %in% c("Research Policy", "Academy of Management Journal",
                             "Journal of Product Innovation Management",
                             "Management Science", "Research Policy", "Organization Science",
                             "Strategic Management Journal"))

# Import included papers dataset
paper_sample <- read_xlsx("Data/3 - Validity analysis/Input/Validity Assessed.xlsx") %>% 
  clean_names() %>% 
  select(-starts_with("x")) |>  # remove columns not initialized
  filter(source_title %in% c("Research Policy", "Academy of Management Journal",
                             "Journal of Product Innovation Management",
                             "Management Science", "Research Policy", "Organization Science",
                             "Strategic Management Journal"))

# Inclusion and Exclusion criteria analysis -------------------------------

# Calculate number of papers download with the query
n_of_starting_paper <- paper_query %>% 
  select(eid) %>% 
  unique() %>% 
  nrow()


# Descriptive statistics --------------------------------------------------

# Calculate number of papers in the sample
n_of_paper <- paper_sample %>% 
  select(eid) %>% 
  unique() %>% 
  nrow()

# Number of papers per objectives
count_paper_objectives <- paper_sample %>% 
  select(eid, objectives) %>%
  unique() %>% 
  count(objectives) %>% 
  mutate(perc = round(n/n_of_paper*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>%  
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(text_modelling_approach = "Total") %>% 
  pivot_wider(names_from = objectives, values_from = "n_perc") 
  
# Number of papers per text modelling approach
count_paper_text_modelling <- paper_sample %>% 
  select(eid, text_modelling_approach) %>%
  unique() %>% 
  count(text_modelling_approach) %>%
  mutate(perc = round(n/n_of_paper*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>%  
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  rename(Total = n_perc)

# Calculate number of papers per text modelling approach and objectives
obejective_text_analysis_count <- paper_sample %>% 
  count(objectives, text_modelling_approach) %>% 
  complete(objectives, text_modelling_approach, fill = list(n = 0)) %>% 
  mutate(perc = round(n/n_of_paper*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>%  
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  pivot_wider(names_from = objectives, values_from = "n_perc") %>% 
  bind_rows(count_paper_objectives) %>% 
  left_join(count_paper_text_modelling) %>% 
  rename("Text Modelling Approach" = text_modelling_approach)
  
write_xlsx(obejective_text_analysis_count, "Data/3 - Validity analysis/Output/only top/Descriptive Statistics Objectives and Text Modelling approach.xlsx")

# Validity analysis (No synonym) -------------------------------------------------------

# Calculate number of papers in the sample
n_of_paper <- paper_sample %>% 
  select(eid) %>% 
  unique() %>% 
  nrow()

# Calculate number of papers that assess validity/reliability
n_of_paper_validity <- paper_sample %>%
  filter(validity_reliability_evaluated == "Yes") %>% 
  select(eid, title) %>% 
  unique() %>% 
  nrow()

# Calculate percentage of papers that assess validity/reliability
perc_of_paper_validity <- paper_sample %>%
  filter(validity_reliability_evaluated == "Yes") %>% 
  select(eid, title) %>% 
  unique() %>% 
  nrow()/n_of_paper*100

# Number of papers per journal
count_paper_journal <- paper_sample %>% 
  select(eid, source_title) %>%
  unique() %>% 
  count(source_title) %>% 
  rename(n_paper_journal = n)

# In general validity and reliability assessment
validity_in_general_count <- paper_sample %>% 
  select(eid, validity_explicitly:reproducibility_explicitly) %>%
  unique() %>% 
  pivot_longer(cols = validity_explicitly:reproducibility_explicitly, 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  count(validity_reliability, assessed) %>% 
  complete(validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  mutate(perc = round(n/n_of_paper*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  mutate(validity_reliability = str_replace(validity_reliability, "Explicitly", "(explicitly)")) %>% 
  mutate(Journal = "Total", "Total No. of Papers" = n_of_paper) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc")

# Journal and Validity
validity_count <- paper_sample %>% 
  select(eid, source_title, validity_explicitly:reproducibility_explicitly) %>%
  unique() %>% 
  pivot_longer(cols = validity_explicitly:reproducibility_explicitly, 
              names_to = "validity_reliability", values_to = "assessed") %>% 
  count(source_title, validity_reliability, assessed) %>% 
  complete(source_title, validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  left_join(count_paper_journal) %>% 
  mutate(perc = round(n/n_paper_journal*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  mutate(validity_reliability = str_replace(validity_reliability, "Explicitly", "(explicitly)")) %>% 
  rename("Journal" = source_title, "Total No. of Papers" = n_paper_journal) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc") %>% 
  bind_rows(validity_in_general_count)

# Save
write_xlsx(validity_count, "Data/3 - Validity analysis/Output/only top/Validity & Reliability Count - Primary Report NO synonym.xlsx")

# Validity analysis (With synonym) -------------------------------------------------------

# In general validity and reliability assessment
validity_in_general_count_synonym <- paper_sample %>% 
  select(eid, validity_explicitly:reliability_explicitly, statistical_conclusion_validity_explicitly_synonym:reproducibility_explicitly_synonym) %>%
  unique() %>% 
  pivot_longer(cols = c(validity_explicitly:reliability_explicitly, statistical_conclusion_validity_explicitly_synonym:reproducibility_explicitly_synonym), 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  count(validity_reliability, assessed) %>% 
  complete(validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  mutate(perc = round(n/n_of_paper*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  mutate(validity_reliability = str_replace(validity_reliability, "Explicitly", "(explicitly)")) %>% 
  mutate(Journal = "Total", "Total No. of Papers" = n_of_paper) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc")

# Journal and Validity
validity_synonym_count <- paper_sample %>% 
  select(eid, source_title, validity_explicitly:reliability_explicitly, statistical_conclusion_validity_explicitly_synonym:reproducibility_explicitly_synonym) %>%
  unique() %>% 
  pivot_longer(cols = c(validity_explicitly:reliability_explicitly, statistical_conclusion_validity_explicitly_synonym:reproducibility_explicitly_synonym), 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  count(source_title, validity_reliability, assessed) %>% 
  complete(source_title, validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  left_join(count_paper_journal) %>% 
  mutate(perc = round(n/n_paper_journal*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  mutate(validity_reliability = str_replace(validity_reliability, "Explicitly", "(explicitly)")) %>% 
  rename("Journal" = source_title, "Total No. of Papers" = n_paper_journal) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc") %>% 
  bind_rows(validity_in_general_count_synonym)

# Save
write_xlsx(validity_synonym_count, "Data/3 - Validity analysis/Output/only top/Validity & Reliability Count - Primary Report WITH synonym.xlsx")

# Validity analysis (Secondary report) -------------------------------------------------------

# Calculate number of papers that assess validity/reliability
n_of_paper_temp <- paper_sample %>%
  filter(!is.na(construct_validity_second_report)) %>% 
  select(eid, title) %>% 
  unique() %>% 
  nrow()

# In general validity and reliability assessment
validity_in_general_count_secondary_report <- paper_sample %>% 
  filter(!is.na(construct_validity_second_report)) %>% 
  select(eid, statistical_conclusion_validity_second_report:reproducibility_second_report) %>%
  unique() %>% 
  pivot_longer(cols = statistical_conclusion_validity_second_report:reproducibility_second_report, 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  unique() %>% 
  count(validity_reliability, assessed) %>% 
  complete(validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  mutate(perc = round(n/n_of_paper_temp*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  mutate(validity_reliability = str_replace(validity_reliability, "Explicitly", "(explicitly)")) %>% 
  mutate(Journal = "Total", "Total No. of Papers" = n_of_paper_temp) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc")

# Validity and reliability in general count
validity_jorunal_in_general_count_secondary_report <- paper_sample %>% 
  filter(!is.na(construct_validity_second_report)) %>% 
  select(eid, statistical_conclusion_validity_second_report:reproducibility_second_report) %>% 
  mutate(validity_second_report = if_else(statistical_conclusion_validity_second_report == "Yes"|
                                            internal_validity_second_report == "Yes"|
                                            external_validity_second_report == "Yes"|
                                            construct_validity_second_report == "Yes", "Yes", "No")) %>% 
  mutate(reliability_second_report = if_else(stability_second_report == "Yes"|
                                               reproducibility_second_report == "Yes", "Yes", "No")) %>% 
  select(eid, validity_second_report, reliability_second_report) %>% 
  unique() %>% 
  filter(validity_second_report == "Yes"|reliability_second_report == "Yes") %>% 
  pivot_longer(cols = validity_second_report:reliability_second_report, 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  count(validity_reliability, assessed) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  mutate(perc = round(n/n_of_paper_temp*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  mutate("Journal" = "Total", "Total No. of Papers" = n_of_paper_temp) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc") 

# Validity and reliability in general count
validity_journal_count_secondary_report <- paper_sample %>% 
  filter(!is.na(construct_validity_second_report)) %>% 
  select(eid, source_title, statistical_conclusion_validity_second_report:reproducibility_second_report) %>% 
  mutate(validity_second_report = if_else(statistical_conclusion_validity_second_report == "Yes"|
                                            internal_validity_second_report == "Yes"|
                                            external_validity_second_report == "Yes"|
                                            construct_validity_second_report == "Yes", "Yes", "No")) %>% 
  mutate(reliability_second_report = if_else(stability_second_report == "Yes"|
                                               reproducibility_second_report == "Yes", "Yes", "No")) %>% 
  select(eid, source_title, validity_second_report, reliability_second_report) %>% 
  unique() %>% 
  filter(validity_second_report == "Yes"|reliability_second_report == "Yes") %>% 
  pivot_longer(cols = validity_second_report:reliability_second_report, 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  count(source_title, validity_reliability, assessed) %>% 
  complete(source_title, validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  left_join(count_paper_journal) %>% 
  mutate(perc = round(n/n_paper_journal*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  rename("Journal" = source_title, "Total No. of Papers" = n_paper_journal) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc") %>% 
  bind_rows(validity_jorunal_in_general_count_secondary_report)

# Journal and Validity
validity_secondary_report_count <- paper_sample %>% 
  select(eid, source_title, statistical_conclusion_validity_second_report:reproducibility_second_report) %>%
  unique() %>% 
  pivot_longer(cols = statistical_conclusion_validity_second_report:reproducibility_second_report, 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  unique() %>% 
  count(source_title, validity_reliability, assessed) %>% 
  complete(source_title, validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  left_join(count_paper_journal) %>% 
  mutate(perc = round(n/n_paper_journal*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  mutate(validity_reliability = str_replace(validity_reliability, "Explicitly", "(explicitly)")) %>% 
  rename("Journal" = source_title, "Total No. of Papers" = n_paper_journal) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc") %>% 
  bind_rows(validity_in_general_count_secondary_report) %>% 
  left_join(validity_journal_count_secondary_report) %>% 
  mutate(`Validity Second Report` = if_else(is.na(`Validity Second Report`), "0\n(0.00%)", `Validity Second Report`)) %>% 
  mutate(`Reliability Second Report` = if_else(is.na(`Reliability Second Report`), "0\n(0.00%)", `Reliability Second Report`))


# Save
write_xlsx(validity_secondary_report_count, "Data/3 - Validity analysis/Output/only top/Validity & Reliability Count - Secondary Report.xlsx")

# Validity analysis (Objectives) -------------------------------------------------------

# Number of papers per journal
count_paper_objectives <- paper_sample %>% 
  filter(!is.na(construct_validity_second_report)) %>% 
  select(eid, objectives) %>%
  unique() %>% 
  count(objectives) %>% 
  rename(n_paper_objectives = n)

# Validity and reliability in general count
validity_objectives_count_in_general <- paper_sample %>% 
  filter(!is.na(construct_validity_second_report)) %>% 
  select(eid, objectives, statistical_conclusion_validity_second_report:reproducibility_second_report) %>% 
  mutate(validity_second_report = if_else(statistical_conclusion_validity_second_report == "Yes"|
                                            internal_validity_second_report == "Yes"|
                                            external_validity_second_report == "Yes"|
                                            construct_validity_second_report == "Yes", "Yes", "No")) %>% 
  mutate(reliability_second_report = if_else(stability_second_report == "Yes"|
                                               reproducibility_second_report == "Yes", "Yes", "No")) %>% 
  select(eid, objectives, validity_second_report, reliability_second_report) %>% 
  unique() %>% 
  filter(validity_second_report == "Yes"|reliability_second_report == "Yes") %>% 
  pivot_longer(cols = validity_second_report:reliability_second_report, 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  count(objectives, validity_reliability, assessed) %>% 
  complete(objectives, validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  left_join(count_paper_objectives) %>% 
  mutate(perc = round(n/n_paper_objectives*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  rename("Objectives" = objectives, "Total No. of Papers" = n_paper_objectives) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc") 

# Journal and Validity
validity_objectives_count <- paper_sample %>% 
  filter(!is.na(construct_validity_second_report)) %>% 
  select(eid, objectives, statistical_conclusion_validity_second_report:reproducibility_second_report) %>%
  unique() %>% 
  pivot_longer(cols = statistical_conclusion_validity_second_report:reproducibility_second_report, 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  unique() %>% 
  count(objectives, validity_reliability, assessed) %>% 
  complete(objectives, validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  left_join(count_paper_objectives) %>% 
  mutate(perc = round(n/n_paper_objectives*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  mutate(validity_reliability = str_replace(validity_reliability, "Explicitly", "(explicitly)")) %>% 
  rename("Objectives" = objectives, "Total No. of Papers" = n_paper_objectives) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc") %>% 
  left_join(validity_objectives_count_in_general) %>% 
  mutate(`Validity Second Report` = if_else(is.na(`Validity Second Report`), "0\n(0.00%)", `Validity Second Report`)) %>% 
  mutate(`Reliability Second Report` = if_else(is.na(`Reliability Second Report`), "0\n(0.00%)", `Reliability Second Report`))


# Save
write_xlsx(validity_objectives_count, "Data/3 - Validity analysis/Output/only top/Validity & Reliability Count - Objectives.xlsx")

# Validity analysis (NLP Techniques) -------------------------------------------------------

# Number of papers per journal
count_paper_nlp <- paper_sample %>% 
  filter(!is.na(construct_validity_second_report)) %>% 
  select(eid, text_modelling_approach) %>%
  unique() %>% 
  count(text_modelling_approach) %>% 
  rename(n_paper_techniques = n)

# Validity and reliability in general count
validity_nlp_count_in_general <- paper_sample %>% 
  filter(!is.na(construct_validity_second_report)) %>% 
  select(eid, text_modelling_approach, statistical_conclusion_validity_second_report:reproducibility_second_report) %>% 
  mutate(validity_second_report = if_else(statistical_conclusion_validity_second_report == "Yes"|
                                            internal_validity_second_report == "Yes"|
                                            external_validity_second_report == "Yes"|
                                            construct_validity_second_report == "Yes", "Yes", "No")) %>% 
  mutate(reliability_second_report = if_else(stability_second_report == "Yes"|
                                               reproducibility_second_report == "Yes", "Yes", "No")) %>% 
  select(eid, text_modelling_approach, validity_second_report, reliability_second_report) %>% 
  unique() %>% 
  filter(validity_second_report == "Yes"|reliability_second_report == "Yes") %>% 
  pivot_longer(cols = validity_second_report:reliability_second_report, 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  count(text_modelling_approach, validity_reliability, assessed) %>% 
  complete(text_modelling_approach, validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  left_join(count_paper_nlp) %>% 
  mutate(perc = round(n/n_paper_techniques*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  rename("Text Modelling Approach" = text_modelling_approach, "Total No. of Papers" = n_paper_techniques) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc") 

# Journal and Validity
validity_nlp_count <- paper_sample %>% 
  filter(!is.na(construct_validity_second_report)) %>% 
  select(eid, text_modelling_approach, statistical_conclusion_validity_second_report:reproducibility_second_report) %>%
  unique() %>% 
  pivot_longer(cols = statistical_conclusion_validity_second_report:reproducibility_second_report, 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  unique() %>% 
  count(text_modelling_approach, validity_reliability, assessed) %>% 
  complete(text_modelling_approach, validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  left_join(count_paper_nlp) %>% 
  mutate(perc = round(n/n_paper_techniques*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  mutate(validity_reliability = str_replace(validity_reliability, "Explicitly", "(explicitly)")) %>% 
  rename("Text Modelling Approach" = text_modelling_approach, "Total No. of Papers" = n_paper_techniques) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc") %>% 
  left_join(validity_nlp_count_in_general) %>% 
  mutate(`Validity Second Report` = if_else(is.na(`Validity Second Report`), "0\n(0.00%)", `Validity Second Report`)) %>% 
  mutate(`Reliability Second Report` = if_else(is.na(`Reliability Second Report`), "0\n(0.00%)", `Reliability Second Report`))


# Save
write_xlsx(validity_nlp_count, "Data/3 - Validity analysis/Output/only top/Validity & Reliability Count - Text Modelling Approach.xlsx")


# Validity analysis (Predefined/Customized) -------------------------------------------------------

# Number of papers per journal
count_paper_predefined <- paper_sample %>% 
  filter(!is.na(construct_validity_second_report)) %>%
  filter(!is.na(pre_defined_modelling_approach)) %>% 
  select(eid, pre_defined_modelling_approach) %>%
  unique() %>% 
  count(pre_defined_modelling_approach) %>% 
  rename(n_paper_techniques = n) 

# Validity and reliability in general count
validity_predefined_count_in_general <- paper_sample %>% 
  filter(!is.na(construct_validity_second_report)) %>%
  filter(!is.na(pre_defined_modelling_approach)) %>% 
  select(eid, pre_defined_modelling_approach, statistical_conclusion_validity_second_report:reproducibility_second_report) %>% 
  mutate(validity_second_report = if_else(statistical_conclusion_validity_second_report == "Yes"|
                                            internal_validity_second_report == "Yes"|
                                            external_validity_second_report == "Yes"|
                                            construct_validity_second_report == "Yes", "Yes", "No")) %>% 
  mutate(reliability_second_report = if_else(stability_second_report == "Yes"|
                                               reproducibility_second_report == "Yes", "Yes", "No")) %>% 
  select(eid, pre_defined_modelling_approach, validity_second_report, reliability_second_report) %>% 
  unique() %>% 
  filter(validity_second_report == "Yes"|reliability_second_report == "Yes") %>% 
  pivot_longer(cols = validity_second_report:reliability_second_report, 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  count(pre_defined_modelling_approach, validity_reliability, assessed) %>% 
  complete(pre_defined_modelling_approach, validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  left_join(count_paper_predefined) %>% 
  mutate(perc = round(n/n_paper_techniques*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  rename("Predefined/Customized" = pre_defined_modelling_approach, "Total No. of Papers" = n_paper_techniques) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc") 

# Journal and Validity
validity_predefined_count <- paper_sample %>% 
  filter(!is.na(construct_validity_second_report)) %>%
  filter(!is.na(pre_defined_modelling_approach)) %>% 
  select(eid, pre_defined_modelling_approach, statistical_conclusion_validity_second_report:reproducibility_second_report) %>%
  unique() %>% 
  pivot_longer(cols = statistical_conclusion_validity_second_report:reproducibility_second_report, 
               names_to = "validity_reliability", values_to = "assessed") %>% 
  unique() %>% 
  count(pre_defined_modelling_approach, validity_reliability, assessed) %>% 
  complete(pre_defined_modelling_approach, validity_reliability, assessed, fill = list(n = 0)) %>% 
  filter(assessed == "Yes") %>% 
  select(-assessed) %>%
  left_join(count_paper_predefined) %>% 
  mutate(perc = round(n/n_paper_techniques*100, 2)) %>% 
  mutate(perc = format(perc, scientific = FALSE, big.mark = ",")) %>% 
  mutate(n_perc = str_c(n, "\n", "(", perc, "%)")) %>% 
  mutate(n_perc = str_replace(n_perc, "\\(\\s*", "(")) %>% 
  select(-n, -perc) %>% 
  mutate(validity_reliability = str_replace_all(validity_reliability, "_", " ")) %>% 
  mutate(validity_reliability = str_to_title(validity_reliability)) %>% 
  mutate(validity_reliability = str_replace(validity_reliability, "Explicitly", "(explicitly)")) %>% 
  rename("Predefined/Customized" = pre_defined_modelling_approach, "Total No. of Papers" = n_paper_techniques) %>% 
  pivot_wider(names_from = validity_reliability, values_from = "n_perc") %>% 
  left_join(validity_predefined_count_in_general) %>% 
  mutate(`Validity Second Report` = if_else(is.na(`Validity Second Report`), "0\n(0.00%)", `Validity Second Report`)) %>% 
  mutate(`Reliability Second Report` = if_else(is.na(`Reliability Second Report`), "0\n(0.00%)", `Reliability Second Report`))


# Save
write_xlsx(validity_predefined_count, "Data/3 - Validity analysis/Output/only top/Validity & Reliability Count - Predefined vs Customized.xlsx")

