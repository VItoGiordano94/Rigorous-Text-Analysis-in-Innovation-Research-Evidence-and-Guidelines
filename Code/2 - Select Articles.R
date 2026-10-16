library(tidyverse)
library(readxl)
library(writexl)


# import data -------------------------------------------------------------

# import dataset of areviewd articles
reviewed_papers <- read_xlsx("Data/2 - Article Selection/Ouput/Reviewed Articles.xlsx") 

# Analysis ----------------------------------------------------------------

# Aanlysis of exlucded papers for each criteria
excluded_paper_count <- reviewed_papers %>% 
  pivot_longer(cols = `Relevance to NLP`: `Empirical Articles`, names_to = "criteria", values_to = "value") %>% 
  filter(value == "not include") %>% 
  count(criteria)

# Save results
write_xlsx(excluded_paper_count, "Data/2 - Article Selection/Ouput/Count for Excluded Articles.xlsx")


# Number of included papers
n_of_included_papers <- reviewed_papers %>% 
  filter(`Relevance to NLP` == "include",
         `Focus on Innovation Management Research`== "include",
         `Empirical Articles` == "include") %>% 
  nrow()

# Included papers
included_papers <- reviewed_papers %>% 
  filter(`Relevance to NLP` == "include",
         `Focus on Innovation Management Research`== "include",
         `Empirical Articles` == "include") %>% 
  select(-`Relevance to NLP`, -`Focus on Innovation Management Research`, 
         -`Empirical Articles`) %>% 
  arrange(`Source title`, desc(Year)) 

# Save results
write_xlsx(included_papers, "Data/2 - Article Selection/Ouput/Selected Articles.xlsx")

# Analysis of included papers ---------------------------------------------

# Number of papers per journal
journal_count <- included_papers %>% 
  count(`Source title`, sort = T)

# Save results
write_xlsx(journal_count, "Data/2 - Article Selection/Ouput/Journal Count.xlsx")


# Number of papers per year
included_papers %>% 
  count(Year) %>% 
  filter(Year < 2026) %>% 
  ggplot(aes(x = Year, y = n)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(y = "Number of Papers")

# Save results
ggsave(filename = "Data/2 - Article Selection/Ouput/Articles per year.png", scale = 2)
