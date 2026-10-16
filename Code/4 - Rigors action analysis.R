library(tidyverse)
library(readxl)
library(writexl)


# Import data -------------------------------------------------------------

# Import rigoros action dataset
paper_data <- read_xlsx("Data/3 - Validity analysis/Input/Rigor action.xlsx") 
  

# Import citation
in_text_cit_data <- read_xlsx("Data/3 - Validity analysis/Input/doi_in_text_citation_dataset.xlsx")


# Analyse rigoros action --------------------------------------------------

# Prepare data for the analysis
paper_data_clean <- paper_data %>%
  pivot_longer(cols = `Increase Statistical Power - Statistical Conclusion Validity` :`Ensure code availability - Reproducibility`, 
               names_to = "threats", values_to = "strategies") %>%
  filter(!is.na(strategies)) %>% 
  unique() %>% 
  separate(col = threats, into = c("threats", "validity_type"), sep = " - ") %>% 
  separate_rows(strategies, sep = "; ") %>% 
  separate(col = strategies, into = c("strategies", "strategies_type"), sep = " \\(") %>% 
  mutate(strategies_type = str_remove(strategies_type, "\\)")) %>% 
  left_join(in_text_cit_data %>% select(DOI, in_text_cit))

# Count number of article for each threat
threat_count <- paper_data_clean %>% 
  select(DOI, threats, validity_type) %>% 
  unique() %>% 
  count(validity_type, threats)

# Count number of article for each action
action_count <- paper_data_clean %>% 
  select(DOI, threats, validity_type, strategies) %>% 
  unique() %>% 
  count(validity_type, threats, strategies) %>%
  arrange(desc(n)) %>% 
  mutate(strategies_n = str_c(strategies, " (", n, ")")) %>% 
  group_by(validity_type, threats) %>% 
  summarise(strategies = str_c(strategies_n, collapse = "; "))

# Merge togheter trhat count and rigor action
threat_rigor_action <- threat_count %>% 
  left_join(action_count)

write_xlsx(threat_rigor_action, "Data/3 - Validity analysis/Output/Threats and Rigor Strategies.xlsx")
