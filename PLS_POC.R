### Proof of Concept: Modelling Moisture Content with PLS
### Michael Burns
### 6/27/22

# Libraries
library(tidyverse)
library(caret)
library(readxl)

# Loading data
train_data = read_csv('~/Desktop/Grad_School/Research/Projects/Machine_Learning/Data/Master_Training_Spectra_No_Outliers.csv')
test_spectra = read_csv('~/Desktop/Grad_School/Research/Projects/Machine_Learning/Hybrid_Validation/Scan_Data/Cleaned_Hybrid_NIR_Scans.csv')
test_cook = read_csv("Downloads/ML_Hybrid_Validation - Sheet1.csv")
lsvm_preds = read_csv("~/Desktop/Grad_School/Research/Projects/Machine_Learning/Hybrid_Validation/Predictions/Cleaned_Hybrid_NIR_Scans_Predictions.csv")

# Check data
head(train_data)
head(test_spectra)
head(test_cook)
head(lsvm_preds)

# Average cooking and prediction replicates
test_cook_sum = test_cook %>%
  group_by(Sample_ID) %>%
  summarise(Moisture_Content = mean(Moisture.Avg)) %>%
  ungroup()

lsvm_preds_sum = lsvm_preds %>%
  mutate(Sample_ID = str_remove(Sample_ID, "_[1-2]$")) %>%
  group_by(Sample_ID) %>%
  select(-Prediction_Quality) %>%
  summarise(Mean_Pred_MC = mean(Predicted_Moisture_Content)) %>%
  ungroup()

# Training multiple varitations
pls_model_results = tibble()
for(seed in c(1:10)){
  print(seed)
  set.seed(seed)
  pls_model = train(Moisture_Uptake ~ ., 
                    data = train_data %>%
                      select_if(is.numeric), 
                    method = "pls", 
                    metric = "Rsquared", 
                    tuneGrid = expand.grid(ncomp = c(1:30)),
                    trControl = trainControl(method = "cv",
                                             index = groupKFold(train_data$Genotype, k = 10), 
                                             savePredictions = T,
                                             allowParallel = T
                    )
  )
  
  ncomps = c(1:30)
  Rsq = pls_model$results$Rsquared
  RMSE = pls_model$results$RMSE
  
  pls_model_results = pls_model_results %>%
    bind_rows(tibble(seed = seed,
                     ncomps = ncomps,
                     Rsq = Rsq,
                     RMSE = RMSE))
}

pls_model_results %>%
  group_by(ncomps) %>%
  summarise(mean_rsq = mean(Rsq),
            mean_rmse = mean(RMSE)) %>% 
  pivot_longer(cols = -ncomps, names_to = 'metric', values_to = 'performance')%>%
  ggplot(aes(x = ncomps, y = performance, color = metric))+
  geom_smooth()+
  theme_classic()

pls_model_results %>%
  group_by(ncomps) %>%
  summarise(mean_rsq = mean(Rsq),
            mean_rmse = mean(RMSE)) %>%
  arrange(desc(mean_rsq))

# Training Model
pls_model = train(Moisture_Uptake ~ ., 
                  data = train_data %>%
                    select_if(is.numeric), 
                  method = "pls", 
                  metric = "Rsquared", 
                  tuneGrid = expand.grid(ncomp = 16),
                  trControl = trainControl(method = "cv",
                                           index = groupKFold(train_data$Genotype, k = 10), 
                                           savePredictions = T,
                                           allowParallel = T
                  )
)

# Testing
test_spectra$Moisture_Content_Prediction = predict(pls_model, test_spectra)

# Make a table of all like samples for true values, LSVM values, and PLS values
moisture_content_data = test_spectra %>%
  mutate(Sample_ID = str_remove(Sample_ID, "_[1-2]$")) %>%
  group_by(Sample_ID) %>%
  summarise(Mean_PLS_MC_Prediction = mean(Moisture_Content_Prediction)) %>%
  inner_join(lsvm_preds_sum) %>%
  rename(Mean_LSVM_MC_Prediction = Mean_Pred_MC) %>%
  inner_join(test_cook_sum)

# Assessing
cor(moisture_content_data$Moisture_Content, moisture_content_data$Mean_PLS_MC_Prediction)
cor(moisture_content_data$Moisture_Content, moisture_content_data$Mean_LSVM_MC_Prediction)

moisture_content_data %>%
  ggplot(aes(x = Moisture_Content, y = Mean_PLS_MC_Prediction))+
  geom_smooth(method = 'lm', se = F)+
  geom_point()+
  ylim(0.40,0.52)+
  xlim(0.40,0.52)+
  geom_abline(slope = 1, intercept = 0)+
  annotate(geom = 'text', x = 0.475, y = 0.425, label = paste0('R: ', round(cor(moisture_content_data$Moisture_Content, moisture_content_data$Mean_PLS_MC_Prediction), 4)))+
  annotate(geom = 'text', x = 0.475, y = 0.42, label = paste0('RMSE: ', round(RMSE(moisture_content_data$Mean_PLS_MC_Prediction, moisture_content_data$Moisture_Content), 4)))+
  theme_classic()

moisture_content_data %>%
  ggplot(aes(x = Moisture_Content, y = Mean_LSVM_MC_Prediction))+
  geom_smooth(method = 'lm', se = F)+
  geom_point()+
  ylim(0.40,0.52)+
  xlim(0.40,0.52)+
  geom_abline(slope = 1, intercept = 0)+
  annotate(geom = 'text', x = 0.475, y = 0.425, label = paste0('R: ', round(cor(moisture_content_data$Moisture_Content, moisture_content_data$Mean_LSVM_MC_Prediction), 4)))+
  annotate(geom = 'text', x = 0.475, y = 0.42, label = paste0('RMSE: ', round(RMSE(moisture_content_data$Mean_LSVM_MC_Prediction, moisture_content_data$Moisture_Content), 4)))+
  theme_classic()

# The predictions are not very good. Could this be due to a difference in machine? Or a difference in hybrids from inbreds?
# Looking at the data, most of the new hybrids were scanned on a different setting of the same machine (not sure if this would
# affect spectra or just the macromolecular predictions), and some were scanned on a different machine, but with the same setting.
# Lets look at the distribution of macromolecular predictions for the training inbreds, the hybrids on the same machine but
# different setting, and the hybrids on a different machine but similar setting.
# Load data
inbreds = read_csv('~/Desktop/Grad_School/Research/Projects/Machine_Learning/Data/N5000_Macro_Cleaned_Master.csv') %>%
  mutate(Product = 'Ground Corn (Small cup)',
         Instrument = '1611154') %>%
  rename(Sample_ID = SampleID,
         Protein = Protein_As_is,
         Starch = Starch_As_is,
         Fiber = Fiber_As_is,
         Fat = Fat_As_is) %>%
  select(Instrument, Product, Sample_ID, Protein, Starch, Fiber, Fat) %>%
  mutate(Type = 'Inbred')

hybrids = read_xlsx('~/Desktop/Grad_School/Research/Projects/Pericarp/Maize_Composition_Predictions_Raw.xlsx') %>%
  rename(Sample_ID = Id,
         Instrument = InstrumentSerialNumber,
         Protein = `Protein As is`,
         Starch = `Starch As is`,
         Fiber = `Fiber As is`) %>%
  mutate(Fat = case_when(!is.na(`Oil As is`) ~ `Oil As is`,
                         !is.na(`Fat As is`) ~ `Fat As is`)) %>%
  select(Instrument, Product, Sample_ID, Protein, Starch, Fiber, Fat) %>%
  mutate(Type = 'Hybrid',
         Instrument = as.character(Instrument))

inbreds %>%
  bind_rows(hybrids) %>%
  mutate(Group = paste0(Instrument, Product, Type)) %>%
  filter(Starch > 60,
         Fiber < 10) %>%
  pivot_longer(cols = c(Protein, Starch, Fiber, Fat), names_to = 'Molecule', values_to = 'Composition') %>%
  filter(Composition > 0) %>%
  ggplot(aes(x = Composition, fill = Group))+
  geom_density(alpha = 0.5)+
  facet_wrap(~Molecule, scales = 'free')+
  theme_classic()

# The data looks fairly distinct for inbreds and hybrids, which could be why they are performing differently.
# Lets check to see if the hybrids are different from the inbreds in terms of spectra
# Read in the hybrid scan data
hybrid_scans = read_csv('~/Desktop/Grad_School/Research/Projects/Machine_Learning/Hybrid_Validation/Scan_Data/Cleaned_Hybrid_NIR_Scans.csv')
# Read in all of the inbred scan data
inbred_scans = read_csv('~/Desktop/Grad_School/Research/Projects/Machine_Learning/Data/N5000_Spectra_Cleaned_Master.csv')
# Read in all of the inbred validation data
inbred_val_scans = read_csv('~/Desktop/Grad_School/Research/Projects/Machine_Learning/Data/ML_Master_Validation_Dataset.csv') %>%
  filter(Genotype != 'MS72') # Remove an outlier

# Combine the spectral data
hybrid_inbred_data = hybrid_scans %>%
  mutate(Sample_ID = str_remove(Sample_ID, '_[1-2]$')) %>% # Remove the trailing rep number
  mutate(Genetics = 'Hybrid_All') %>% # Assign a label
  filter(!Sample_ID %in% test_cook$Sample_ID) %>% # Remove samples that will be added for another group
  bind_rows(train_data %>% # combine with the inbred training data
              mutate(Genetics = 'Inbred_Train')) %>% # assign a label
  bind_rows(inbred_scans %>% # combine with the full inbred scan set
              filter(!SampleID %in% train_data$Sample_ID, # remove duplicates from the training set
                     !SampleID %in% inbred_val_scans$SampleID) %>% # remove duplicates from the validation set
              mutate(Genetics = 'Inbred_All')) %>% # assign a label
  bind_rows(inbred_val_scans %>% # combine with the inbred validation set
             mutate(Genetics = 'Inbred_Val')) %>% # assign a group label
  bind_rows(test_spectra %>% # combine with the hybrids that have already been cooked
              mutate(Sample_ID = str_remove(Sample_ID, '_[1-2]$')) %>% # remove trailing rep number
              filter(Sample_ID %in% test_cook$Sample_ID) %>% # filter for only the hybrids scans associated with a cook
              mutate(Genetics = 'Hybrid_Val')) # assign a group label

pca = prcomp(hybrid_inbred_data[,6:146], center = T, scale. = T) # perform a PCA on the data

summary(pca) # check the PVE of the PCA

# Plot out the PCA of the data
hybrid_inbred_data %>%
  group_by(Genetics) %>%
  mutate(count = n()) %>% # this is to sort the data for appearance (smallest groups on top in the plot)
  arrange(desc(count)) %>%
  select(c(1,149)) %>%
  bind_cols(as_tibble(pca$x)) %>%
  ggplot(aes(x = PC1, y = PC2, color = Genetics))+
  geom_point()+
  theme_classic()

# It looks like each of these groups have different levels of extremes.  Lets use the dataset above to plot out the spectra (maybe 
# just a geom_smooth?) to see which groups are the most extreme.
hybrid_inbred_data %>%
  filter(`950` < 0.2) %>%
  mutate(Sample_ID = case_when(!is.na(Sample_ID) ~ Sample_ID,
                               is.na(Sample_ID) ~ SampleID)) %>%
  select(Sample_ID, Genetics, 6:146) %>%
  pivot_longer(cols = -c(Sample_ID, Genetics), names_to = 'Waveband', values_to = 'Absorbance') %>%
  mutate(Waveband = as.numeric(Waveband)) %>%
  group_by(Sample_ID, Waveband, Genetics) %>%
  summarise(Absorbance = mean(Absorbance)) %>%
  group_by(Genetics, Waveband) %>%
  mutate(Min = min(Absorbance),
         Max = max(Absorbance)) %>%
  ggplot(aes(x = Waveband, y = Absorbance, color = Genetics))+
  geom_ribbon(aes(ymin = Min, ymax = Max, color = Genetics), alpha = 0.2)+
  geom_smooth()+
  theme_classic()

# The spectra is more extreme for the inbreds (which isn't that surprising), but then why is it predicting so poorly, and why does
# the PCA show the hybrids as being more extreme?  Is it due to interaction terms?

# What if I average the wavebands before making predictions?













# Have any of the hybrids that overlap with the inbreds been cooked?  If so, what is their correlation?
hybrid_inbred_data %>%
  select(c(1,149)) %>%
  bind_cols(as_tibble(pca$x)) %>%
  ggplot(aes(x = PC1, y = PC2, color = Genetics))+
  geom_abline(intercept = 2.5, slope = -0.25)+
  geom_abline(intercept = -5, slope = -0.25)+
  geom_point()+
  theme_classic()

hybrids_within_inbreds = hybrid_inbred_data %>%
  select(c(1,149)) %>%
  bind_cols(as_tibble(pca$x)[1:2]) %>%
  filter(PC2 < (2.5 + (-0.25*PC1)) & PC2 > (-5 + (-0.25*PC1))) %>%
  mutate(Sample_ID = str_remove(Sample_ID, '_[1-2]$')) %>%
  group_by(Sample_ID) %>%
  summarise(count = n()) %>%
  filter(count == 2)

# view(hybrids_within_inbreds) # Most of the hybrids that overlap are ones from pioneer that haven't been cooked yet.
library(magrittr)

cooked_hybrids_within_inbreds = moisture_content_data %>%
  filter(Sample_ID %in% hybrids_within_inbreds$Sample_ID)

cooked_hybrids_within_inbreds %$%
  cor(Mean_LSVM_MC_Prediction, Moisture_Content)

cooked_hybrids_within_inbreds %>%
  ggplot(aes(x = Moisture_Content, y = Mean_LSVM_MC_Prediction))+
  geom_point()

cooked_hybrids_within_inbreds %$%
  cor(Mean_PLS_MC_Prediction, Moisture_Content)

hybrid_inbred_data %>%
  select(c(1,149)) %>%
  bind_cols(as_tibble(pca$x)) %>%
  mutate(Sample_ID = str_remove(Sample_ID, '_[1-2]$')) %>%
  mutate(HOI = case_when(Sample_ID %in% cooked_hybrids_within_inbreds$Sample_ID ~ 'Y',
                         !Sample_ID %in% cooked_hybrids_within_inbreds$Sample_ID ~ 'N')) %>%
  ggplot(aes(x = PC1, y = PC2, color = HOI))+
  geom_abline(intercept = 2.5, slope = -0.25)+
  geom_abline(intercept = -5, slope = -0.25)+
  geom_point(alpha = 0.5)+
  theme_classic()

# Even the samples that overlapped with the inbred training set didn't predict well (granted the sample size is small), indicating
# that this problem is more than just having more extreme spectra.  We mostly need to make sure it is a biological issue rather
# than a technical one so the method isn't flawed from the start.

# Lets make a PCA plot to see if there is any grouping of the hybrid NIR scans
hybrid_scans[,6]
hybrid_scans[,146]
pca = prcomp(hybrid_scans[6:146], center = T, scale. = T)

summary(pca)

hybrid_pca = hybrid_scans %>%
  select(c(1:5, 147)) %>%
  bind_cols(as_tibble(pca$x)) %>%
  mutate(`INSTRUMENT S/N` = as.character(`INSTRUMENT S/N`))

pca$sdev %>%
  as_tibble() %>%
  mutate(PC = row_number(),
         Variance = value^2,
         tot_var = sum(Variance),
         PVE = Variance / tot_var,
         Cumulative_PVE = cumsum(PVE)) %>%
  pivot_longer(cols = c(PVE, Cumulative_PVE), names_to = 'PVE_Type', values_to = 'PVE') %>%
  ggplot(aes(x = PC, y = PVE, color = PVE_Type))+
  geom_line()+
  geom_point()+
  theme_classic()

hybrid_pca %>%
  ggplot(aes(x = PC1, y = PC2))+
  geom_point()+
  theme_classic()

hybrid_pca %>%
  ggplot(aes(x = PC1, y = PC2, color = Machine))+
  geom_point()+
  theme_classic()

hybrid_pca %>%
  ggplot(aes(x = PC1, y = PC2, color = `Tray Type`))+
  geom_point(alpha = 0.5)+
  theme_classic()

hybrid_pca %>%
  rowwise() %>%
  mutate(Scan_Date = unlist(str_split(`Long Date`, ' '))[1]) %>%
  mutate(weeks = case_when(Scan_Date == '7/14/21' | Scan_Date == '7/15/21' | Scan_Date == '7/16/21' ~ 'Week_1',
                           Scan_Date == '8/3/21' | Scan_Date == '8/5/21' | Scan_Date == '8/6/21' ~ 'Week_2',
                           Scan_Date == '8/9/21' | Scan_Date == '8/10/21' | Scan_Date == '8/12/21' ~ 'Week_3',
                           Scan_Date == '8/17/21' | Scan_Date == '8/18/21' | Scan_Date == '8/20/21' ~ 'Week_4')) %>%
  ggplot(aes(x = PC1, y = PC2, color = weeks))+
  geom_point(alpha = 0.5)+
  theme_classic()

hybrid_pca %>%
  rowwise() %>%
  mutate(Tray = unlist(str_split(`Tray Type`, ','))[1]) %>%
  ggplot(aes(x = PC1, y = PC2, color = Tray))+
  geom_point(alpha = 0.5)+
  theme_classic()

hybrid_pca %>%
  ggplot(aes(x = PC1, y = PC2, color = Source))+
  geom_point(alpha = 0.5)+
  theme_classic()

# I can't seem to find a factor that separates this into two or three logical groups.

