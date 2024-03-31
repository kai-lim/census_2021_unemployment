library(tidyverse)
library(janitor)

data <- read.csv("opendata/census_unemployment_sex_age.csv") %>% 
  clean_names() %>% 
  select(lower_tier_local_authorities_code,
         lower_tier_local_authorities, 
         economic_activity_status_7_categories_code,
         economic_activity_status_7_categories, 
         age_6_categories,
         age_6_categories_code,
         sex_2_categories, 
         observation)


data_filtered <- data %>% 
  filter(sex_2_categories == "Female") %>% 
  filter(economic_activity_status_7_categories_code == 1 | economic_activity_status_7_categories_code == 2) %>% 
  filter(age_6_categories_code!=1) %>% 
  mutate(econ_active = recode(economic_activity_status_7_categories_code, 
                              `1` = "employed", 
                              `2` = "unemployed"),
         age = recode(age_6_categories_code, 
                      `2` = "16_24", 
                      `3` = "25_34",
                      `4` = "35_49", 
                      `5` = "50_64",
                      `6` = "65_over")) %>% 
  select(lower_tier_local_authorities_code,
         lower_tier_local_authorities, 
         econ_active, 
         age,
         observation)



unemployment_rates <- data_filtered %>%
  # Convert values to numeric if they're not already
  mutate(value = as.numeric(observation)) %>%
  # Group by the necessary identifiers and age groups
  group_by(lower_tier_local_authorities_code, lower_tier_local_authorities, age) %>%
  # Summarize to calculate unemployment rate
  summarise(
    total_employed = sum(value[econ_active == "employed"]),
    total_unemployed = sum(value[econ_active == "unemployed"]),
    unemployment_rate = total_unemployed / (total_employed + total_unemployed),
    .groups = 'drop' # Drop the grouping structure after summarisation
  )


