# Determine which egg parents have been cooked already
cooked_egg_parents = hybrid_data %>%
  select(Genotype) %>%
  separate(Genotype, c('Egg', 'Pollen'), ' X ')

val_egg_parents = hybrid_val_data %>%
  select(Genotype) %>%
  separate(Genotype, c('Egg', 'Pollen'), ' X ')

# Separate out the YCH samples
YCH22_samples = hybrid_scans_uncooked %>%
  filter(str_detect(Sample_ID, '^YCH22')) 
YCH23_samples = hybrid_scans_uncooked %>%
  filter(str_detect(Sample_ID, '^YCH23'))

# Determine what evenly spaced samples will be
even_values_22 = seq(min(YCH22_samples$Moisture_Content_Pred), max(YCH22_samples$Moisture_Content_Pred), length.out = 15)
even_values_23 = seq(min(YCH23_samples$Moisture_Content_Pred), max(YCH23_samples$Moisture_Content_Pred), length.out = 15)

# Step 1 (for YCH22): remove any even values that can be associated with an existing sample
# even_genos_val = tibble()
# for(value in even_values){
#     #print(value) # Debugging
#     even_genos_val = even_genos_val %>%
#     bind_rows(ych23_samples[abs(ych23_samples$Moisture_Content_Pred-value) == min(abs(ych23_samples$Moisture_Content_Pred-value)),] %>%
#                 mutate(closest_value = value,
#                        distance = Moisture_Content_Pred - value))
# 
#     ych23_samples = ych23_samples %>% filter(!Sample_ID %in% even_genos_val$Sample_ID)
# }
sample_values = val_data_snv %>%
  select(Sample_ID, Moisture_Content_Pred) %>%
  filter(str_detect(Sample_ID, '^YCH22')) %>%
  select(Moisture_Content_Pred) %>%
  pull()

for(value in sample_values){
  print(value)
  even_values_22 = even_values_22[!abs(even_values_22 - value) == min(abs(even_values_22 - value))]
}

# Step 2 (for YCH22): find samples for the remaining even values
YCH22_samples = YCH22_samples %>%
  left_join(hybrid_genos_xref) %>%
  filter(Genotype != 'FILL',
         !str_detect(Genotype, 'RIB$'),
         !Sample_ID %in% c('YCH22:2387', 'YCH22:2473')) %>%
  separate(Genotype, c('Egg', 'Pollen'), ' X ') %>%
  filter(Pollen == 'B73' | Pollen == 'Mo17') %>%
  filter(!Egg %in% cooked_egg_parents$Egg,
         !Egg %in% val_egg_parents$Egg) %>%
  select(Egg, Sample_ID, Moisture_Content_Pred)

even_genos_22 = tibble()
for(value in even_values_22){
  even_genos_22 = even_genos_22 %>%
    bind_rows(YCH22_samples[abs(YCH22_samples$Moisture_Content_Pred - value) == min(abs(YCH22_samples$Moisture_Content_Pred - value)),] %>%
    mutate(closest_value = value,
           distance = Moisture_Content_Pred - value))
  
  YCH22_samples = YCH22_samples %>% filter(!Egg %in% even_genos_22$Egg)
}

# Step 1 (for YCH23): remove any even values that can be associated with an existing sample
YCH23_samples = YCH23_samples %>%
  left_join(hybrid_genos_xref) %>%
  filter(Genotype != 'FILL',
         !str_detect(Genotype, 'RIB$')) %>%
  separate(Genotype, c('Egg', 'Pollen'), ' X ') %>%
  filter(Pollen == 'B73' | Pollen == 'Mo17') %>%
  filter(!Egg %in% cooked_egg_parents$Egg,
         !Egg %in% val_egg_parents$Egg,
         !Egg %in% even_genos_22$Egg) %>%
  select(Egg, Sample_ID, Moisture_Content_Pred)

# Step 2 (for YCH23): find samples for the remaining even values
even_genos_23 = tibble()
for(value in even_values_23){
  even_genos_23 = even_genos_23 %>%
    bind_rows(YCH23_samples[abs(YCH23_samples$Moisture_Content_Pred - value) == min(abs(YCH23_samples$Moisture_Content_Pred - value)),] %>%
                mutate(closest_value = value,
                       distance = Moisture_Content_Pred - value))
  
  YCH23_samples = YCH23_samples %>% filter(!Egg %in% even_genos_23$Egg)
}

cooked_egg_parents %>%
  bind_rows(val_egg_parents) %>%
  filter(toupper(Egg) %in% toupper(even_genos_22$Egg),
         toupper(Egg) %in% toupper(even_genos_23$Egg))

even_genos_22 %>%
  filter(toupper(Egg) %in% cooked_egg_parents$Egg,
         Egg %in% val_egg_parents$Egg,
         Egg %in% even_genos_23$Egg)

even_genos_23 %>%
  filter(Egg %in% cooked_egg_parents$Egg,
         Egg %in% val_egg_parents$Egg,
         Egg %in% even_genos_23$Egg)

even_genos_22 %>%
  bind_rows(even_genos_23) %>%
  select(Sample_ID) %>%
  write_csv('~/Desktop/Grad_School/Research/Hybrid_NMC/Data/Second_Attempt_Validation_Samples_Final_To_Find_2022_2023.csv')

#Make labels for cook test packaging
set.seed(123)
even_genos_val_labels = even_genos_22 %>%
  bind_rows(even_genos_23) %>%
  select(Sample_ID) %>%
   mutate(A = NA,
          B = NA) %>%
   pivot_longer(cols = c(A, B),
                names_to = 'Hotplate_ID') %>%
   select(-value) %>%
   sample_frac(1) %>%
   group_by(Hotplate_ID) %>%
   mutate(Hotplate_Pos = rep_len(c(1:4), n())) %>%
   group_by(Hotplate_ID, Hotplate_Pos) %>%
   mutate(Cook_Day = row_number()) %>%
   arrange(Cook_Day, Hotplate_ID, Hotplate_Pos)

even_genos_val_labels %>%
    write_csv('/Users/michael/Desktop/Grad_School/Research/Hybrid_NMC/Data/Labels/Validation_2023_Samples.csv')

even_genos_val_labels %>%
   mutate(Moisture_Y = NA,
          Moisture_Z = NA,
          DML_1 = NA,
          DML_2 = NA) %>%
   pivot_longer(cols = c(Moisture_Y, Moisture_Z, DML_1, DML_2),
                names_to = 'Subsample') %>%
   select(-value) %>%
   write_csv('/Users/michael/Desktop/Grad_School/Research/Hybrid_NMC/Data/Labels/Validation_2023_Subsamples.csv')
