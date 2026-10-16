library(tidyverse)
library(writexl)


# Import data -------------------------------------------------------------

article <- read_csv("Data/2 - Article Selection/Input/Scopus Article 21_10_2025.csv")


# Prepare dataset for the classification ----------------------------------

article_clean <- article %>% 
  select(EID, DOI, Title, Year, `Source title`, Abstract, `Index Keywords`, `Author Keywords`) %>% 
  mutate("Relevance to NLP" = "") %>% 
  mutate("Focus on Innovation Management Research" = "") %>% 
  mutate("Exclusion of Review Articles" = "") %>% 
  mutate("NLP for Methodological Scope" = "") %>% 
  mutate("Evaluating Validity" = "")

# Save file
write_xlsx(article_clean,
           "Data/2 - Article Selection/Ouput/Articles to Review.xlsx")