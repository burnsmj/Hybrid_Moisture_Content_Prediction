### Hybrid Validation: Multi Environment Trial Inventory
### Michael Burns
### 7/9/21

library(tidyverse)
library(readxl)

scan_id <- read_xlsx("Hybrid_Validation/Inventory/Multi_Environment_Hybrids_Inventory.xlsx") %>%
  select(-Notes)
sample_codex <- read_xlsx("Hybrid_Validation/Inventory/Multi_Environment_Hybrids_Inventory.xlsx", sheet = 2) %>%
  select(-Notes)
env_codex <- read_xlsx("Hybrid_Validation/Inventory/Multi_Environment_Hybrids_Inventory.xlsx", sheet = 3)

Cleaned_Multi_Env_Hyb <- scan_id %>%
  filter(str_detect(string = Scan_ID, pattern = "_[0-9]_")) %>%
  separate(col = Scan_ID, into = c('Letter', 'Number', 'Year'), sep = "_") %>%
  mutate(Number = as.numeric(Number)) %>%
  full_join(sample_codex) %>%
  full_join(env_codex) %>%
  mutate(Scan_ID = paste(SampleID, Environment, Year, sep = "_"))
  
Cleaned_Multi_Env_Hyb %>%
  group_by(Letter, Number) %>%
  summarise(n = n()) %>%
  filter(n > 1)
# No duplicates

Cleaned_Multi_Env_Hyb %>%
  group_by(Letter) %>%
  summarise(n = n()) %>%
  filter(n < 7)
# C, I, Q have less than all 7 environments

Cleaned_Multi_Env_Hyb %>%
  group_by(Number) %>%
  summarise(n = n()) %>%
  filter(n > 20)
# Five environments have more than 20 hybrids

Cleaned_Multi_Env_Hyb %>%
  group_by(Number) %>%
  summarise(n = n()) %>%
  filter(n < 20)
# Zero environments have less than 20 hybrids

Letter_w_enough_seed <- Cleaned_Multi_Env_Hyb %>%
  mutate(Enough_Seed = case_when(Mass >= 420 ~ "YES",
                                 Mass < 420 ~ "NO")) %>%
  group_by(Letter, Enough_Seed) %>%
  summarise(n = n()) %>%
  filter(Enough_Seed == "YES" & n == 7)

Cleaned_Multi_Env_Hyb %>%
  filter(Letter %in% Letter_w_enough_seed$Letter) %>%
  select(Scan_ID) %>%
  write_csv("Hybrid_Validation/Inventory/Multi_Env_Inventory.csv")
