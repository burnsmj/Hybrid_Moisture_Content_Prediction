### NIR Approximates Cleaning
### Michael Burns
### 6/27/23

# Libraries
library(tidyverse)
library(readxl)

# Read in genotype cross ref
genos_xref = read_csv('/Users/michael/Desktop/Grad_School/Research/Projects/Machine_Learning/Hybrid_Validation/Hybrid_Genotypes_XRef.csv')

# Read in old approximates
round_1_comps = readxl::read_xlsx('~/Desktop/Grad_School/Research/Projects/Machine_Learning/Hybrid_Validation/Scan_Data/Hybrids_Rescans_Composition_Approximates.xlsx') %>%
  rename(Sample_ID = Id) %>% 
  select('Sample_ID', 'Moisture', 'Protein As is', 'Starch As is', 'Fat As is', 'Fiber As is', 'Ash As is') %>%
  mutate(Sample_ID = toupper(Sample_ID)) %>%
  filter(!str_detect(Sample_ID, 'TEST'),
         !str_detect(Sample_ID, 'PERICARP'),
         !str_detect(Sample_ID, 'BLANK'),
         !str_detect(Sample_ID, 'CH22')) %>%
  mutate(Sample_ID = str_replace_all(Sample_ID, '\\)', '0'),
         Sample_ID = str_replace_all(Sample_ID, '\\!', '1'),
         Sample_ID = str_replace_all(Sample_ID, '\\@', '2'),
         Sample_ID = str_replace_all(Sample_ID, '\\#', '3'),
         Sample_ID = str_replace_all(Sample_ID, '\\$', '4'),
         Sample_ID = str_replace_all(Sample_ID, '\\%', '5'),
         Sample_ID = str_replace_all(Sample_ID, '\\^', '6'),
         Sample_ID = str_replace_all(Sample_ID, '\\&', '7'),
         Sample_ID = str_replace_all(Sample_ID, '\\*', '8'),
         Sample_ID = str_replace_all(Sample_ID, '\\(', '9')) %>%
  mutate(row = row_number()) %>%
  arrange(desc(row)) %>%
  mutate(Sample_ID = str_remove_all(Sample_ID, '_RESCAN'),
         Sample_ID = str_remove_all(Sample_ID, ' x2')) %>%
  distinct(Sample_ID, .keep_all = T) %>%
  arrange(Sample_ID) %>%
  select(-row) %>%
  group_by(Sample_ID) %>%
  mutate(Sample_ID = str_remove(Sample_ID, '_[1-2]$'),
         Sample_ID = str_remove(Sample_ID, '-[1-2]$')) %>%
  summarise(Moisture = mean(Moisture, na.rm = T),
            Protein = mean(`Protein As is`, na.rm = T),
            Starch = mean(`Starch As is`, na.rm = T),
            Fat = mean(`Fat As is`, na.rm = T),
            Fiber = mean(`Fiber As is`, na.rm = T),
            Ash = mean(`Ash As is`, na.rm = T))

# Read in new approximates
read_xlsx('/Users/michael/Downloads/Product Report - Re-Analyzed 071023.xlsx', skip = 1) %>%
  select(4, 6:11) %>%
  rename(Moisture = Moisture...6,
         Protein = `Protein As is...7`,
         Starch = `Starch As is...11`,
         Fiber = `Fiber As is`,
         Fat = `Fat As is`,
         Ash = `Ash As is...10`) %>%
  mutate(Sample_ID = toupper(Sample_ID)) %>%
  filter(!str_detect(Sample_ID, 'TEST'),
         !str_detect(Sample_ID, 'PERICARP'),
         !str_detect(Sample_ID, 'BLANK')) %>%
  mutate(Sample_ID = str_replace_all(Sample_ID, '\\)', '0'),
         Sample_ID = str_replace_all(Sample_ID, '\\!', '1'),
         Sample_ID = str_replace_all(Sample_ID, '\\@', '2'),
         Sample_ID = str_replace_all(Sample_ID, '\\#', '3'),
         Sample_ID = str_replace_all(Sample_ID, '\\$', '4'),
         Sample_ID = str_replace_all(Sample_ID, '\\%', '5'),
         Sample_ID = str_replace_all(Sample_ID, '\\^', '6'),
         Sample_ID = str_replace_all(Sample_ID, '\\&', '7'),
         Sample_ID = str_replace_all(Sample_ID, '\\*', '8'),
         Sample_ID = str_replace_all(Sample_ID, '\\(', '9')) %>%
  mutate(row = row_number()) %>%
  arrange(desc(row)) %>%
  mutate(Sample_ID = str_remove_all(Sample_ID, '_RESCAN'),
         Sample_ID = str_remove_all(Sample_ID, ' x2')) %>%
  distinct(Sample_ID, .keep_all = T) %>%
  arrange(Sample_ID) %>%
  select(-row) %>%
  bind_rows(round_1_comps) %>%
  left_join(genos_xref) %>%
  select(Sample_ID, Genotype, Moisture, Protein, Starch, Fiber, Fat, Ash) %>%
  write_csv('/Users/michael/Desktop/Grad_School/Research/Projects/Machine_Learning/Hybrid_Validation/Hybrid_Maize_Composition_Predictions.csv')
