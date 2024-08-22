# spectral data
# inbreds
inbreds_spec = read_csv('~/Desktop/Grad_School/Research/Hybrid_NMC/Data/Scans/WiDiv_Inbreds_NIR_Spectra.csv')
# hybrids
hybrids_spec = read_csv('~/Desktop/Grad_School/Research/Hybrid_NMC/Data/Scans/Hybrid_Maize_NIR_Spectra.csv')
# compositional data
comp_data = read_csv('~/Desktop/Grad_School/Research/Hybrid_NMC/Data/Scans/Hybrid_Maize_Composition_Predictions.csv')

# Combine datasets
comb_data = hybrids_spec %>%
  bind_rows(inbreds_spec) %>%
  mutate(Sample_ID = toupper(Sample_ID)) %>%
  left_join(comp_data, by = c('Sample_ID' = 'Sample_ID')) %>%
  mutate(Dataset = case_when(Sample_ID %in% hybrid_data$Sample_ID ~ 'Training',
                             Sample_ID %in% val_data_snv$Sample_ID ~ 'Validation',
                             TRUE ~ 'Remaining'))
comb_data %>%
  mutate(PLS_Predicted_Moisture_Content = predict(hybrid_model, comb_data %>%
                                                    select(-as.character(seq(950,1650,5))) %>%
                                                    bind_cols(as_tibble(detrend(comb_data %>%
                                                                                  select(as.character(seq(950,1650,5))),
                                                                                wav = as.numeric(seq(950,1650,5)),
                                                                                p = 2))))) %>%
  select(Sample_ID, Location, Year,
         Source, Experiment, Dataset,
         Moisture, Protein, Starch,
         Fiber, Fat, Ash,
         PLS_Predicted_Moisture_Content,
         as.character(seq(950,1650,5))) %>%
  left_join(hybrid_genos_xref, by = c('Sample_ID' = 'Sample_ID')) %>%
  write_csv('~/Desktop/Genotype_Information.csv')
