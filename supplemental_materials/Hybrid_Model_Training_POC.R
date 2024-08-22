### Hybrid Model Training POC
### Michael Burns
### 9/2/22

# Purpose: To see if training a model on hybrids will in fact create a more robust prediction model than is currently achieveable with inbreds

# Libraries
library(tidyverse)
library(caret)

# Load data
hybrid_scans = read_csv('~/Desktop/Grad_School/Research/Projects/Machine_Learning/Hybrid_Validation/Hybrid_Rescans_Clean_Averaged.csv')

hybrid_data = read_csv('~/Desktop/Grad_School/Research/Projects/Machine_Learning/Hybrid_Validation/ML_Hybrid_Validation.csv') %>%
  select(Sample_ID, Moisture.Avg) %>%
  group_by(Sample_ID) %>%
  mutate(count = n()) %>%
  filter(count < 3) %>%
  select(-count) %>%
  summarise(Moisture.Avg = mean(Moisture.Avg)) %>%
  left_join(hybrid_scans) %>%
  filter(!is.na(`950`)) %>%
  ungroup()

inbred_data = read_csv('~/Desktop/Grad_School/Research/Projects/Machine_Learning/Moisture_Content_Predictor/Moisture_Uptake_Master_Dataset_Cook_Macro_Spectra.csv') %>%
  select(1,2,26:167)


# Learning Curve Function
learning_curve = function(outcome_col, data, test_partition, reps, iterations, model, metric, tuneparams){
  # Separate Training and Testing Set
  test_index = createDataPartition(data[[outcome_col]], p = test_partition, list = F)
  training_full = data[-test_index,]
  
  testing = data[test_index,]
  
  # Check dimensions of datasets
  #dim(training_full) 
  #dim(testing)

  # Create a tibble to store data in
  performance = tibble(rep = NULL, p = NULL, test = NULL, train = NULL, train_size = NULL)
  for(rep in 1:reps){
    print(rep) # Print out what rep (larger iteration) we are on as a loading bar proxy
    for(p in iterations){
      set.seed(rep) # Set the seed to be the same within a rep, but different across reps
      # Reduce size of training set
      training_index = createDataPartition(training_full[[outcome_col]], p = p/max(iterations), list = F)
      training = training_full[training_index,]
      training = rename(training, outcome = outcome_col)

      # Train model
      pls_model = train(outcome ~ ., 
                        data = training, 
                        method = model, 
                        metric = metric, 
                        tuneGrid = tuneparams,
                        trControl = trainControl(method = "none",
                                                 savePredictions = T,
                                                 allowParallel = T
                        )
      )
      
      # Predict on test and train sets
      test_preds = predict(pls_model, testing)
      train_preds = predict(pls_model, training)
      
      # Store the data in the tibble created above
      performance = bind_rows(performance, tibble(rep = rep,
                                                  p = p,
                                                  test_rmse = RMSE(test_preds, testing[[outcome_col]]),
                                                  train_rmse = RMSE(train_preds, training[[outcome_col]]),
                                                  test_spearmanr = cor(testing[[outcome_col]], test_preds, method = 'spearman'),
                                                  train_spearmanr = cor(training[[outcome_col]], train_preds, method = 'spearman'),
                                                  train_size = nrow(training)))
      }
  }
  # Return the results
  return(performance)
}

# Create a learning curve for the hybrid data
hybrid_lc = learning_curve(outcome_col = 1,
                           data = hybrid_data[2:143],
                           test_partition = 0.2,
                           reps = 10,
                           iterations = 4:30,
                           model = "pls",
                           metric = "Rsquared",
                           tuneparams = expand.grid(ncomp = 15))

#print(hybrid_lc) # Look at the data

# Plot out the curves
hybrid_lc %>%
  pivot_longer(cols = c(test_rmse, train_rmse, test_spearmanr, train_spearmanr), names_to = 'Group_Metric', values_to = 'Performance') %>%
  separate(col = Group_Metric, into = c('Group', 'Metric'), sep = "_") %>%
  mutate(Metric = case_when(Metric == 'rmse' ~ 'RMSE',
                            Metric == 'spearmanr' ~ 'Spearman R')) %>%
  filter(Metric == 'RMSE') %>%
  ggplot(aes(x = train_size, y = Performance, color = Group))+
  stat_summary()+
  geom_smooth(se = F)+
  facet_wrap(~Metric, scales = 'free')+
  xlim(0,420)+ # This is to keep it consistent with the inbreds
  ylim(0,0.07)+
  scale_color_manual(values = c('black', 'gray'), breaks = c('train', 'test'))+
  labs(title = 'Hybrid on Hybrid Learning Curve',
       x = 'Training Size')+
  theme_classic()+
  theme(text = element_text(size = 15))

hybrid_lc %>%
  pivot_longer(cols = c(test_rmse, train_rmse, test_spearmanr, train_spearmanr), names_to = 'Group_Metric', values_to = 'Performance') %>%
  separate(col = Group_Metric, into = c('Group', 'Metric'), sep = "_") %>%
  mutate(Metric = case_when(Metric == 'rmse' ~ 'RMSE',
                            Metric == 'spearmanr' ~ 'Spearman R')) %>%
  filter(Metric == 'Spearman R') %>%
  ggplot(aes(x = train_size, y = Performance, color = Group))+
  stat_summary()+
  geom_smooth(se = F)+
  facet_wrap(~Metric, scales = 'free')+
  xlim(0,420)+ # This is to keep it consistent with the inbreds
  ylim(0,1)+
  scale_color_manual(values = c('black', 'gray'), breaks = c('train', 'test'))+
  labs(title = 'Hybrid on Hybrid Learning Curve',
       x = 'Training Size')+
  theme_classic()+
  theme(text = element_text(size = 15))
# Looks like we need to go to at least 150 samples with how things are looking.

# Create learning curve for inbred data to compare
inbred_lc = learning_curve(outcome_col = 1,
                           data = inbred_data[3:144],
                           test_partition = 0.08, # Matching the number of samples from hybrids
                           reps = 10,
                           iterations = 3:60, # Trying to get close to the spacing of data in the hybrids
                           model = "pls",
                           metric = "Rsquared",
                           tuneparams = expand.grid(ncomp = 15))

#print(inbred_lc) # Look at the data

# Plot out the curves
inbred_lc %>%
  pivot_longer(cols = c(test_rmse, train_rmse, test_spearmanr, train_spearmanr), names_to = 'Group_Metric', values_to = 'Performance') %>%
  separate(col = Group_Metric, into = c('Group', 'Metric'), sep = "_") %>%
  mutate(Metric = case_when(Metric == 'rmse' ~ 'RMSE',
                            Metric == 'spearmanr' ~ 'Spearman R')) %>%
  filter(Metric == 'RMSE') %>%
  ggplot(aes(x = train_size, y = Performance, color = Group))+
  stat_summary()+
  geom_smooth(se = F)+
  facet_wrap(~Metric, scales = 'free')+
  xlim(0,420)+
  ylim(0,0.07)+
  scale_color_manual(values = c('black', 'gray'), breaks = c('train', 'test'))+
  labs(title = 'Inbred on Inbred Learning Curve',
       x = 'Training Size')+
  theme_classic()+
  theme(text = element_text(size = 15))

inbred_lc %>%
  pivot_longer(cols = c(test_rmse, train_rmse, test_spearmanr, train_spearmanr), names_to = 'Group_Metric', values_to = 'Performance') %>%
  separate(col = Group_Metric, into = c('Group', 'Metric'), sep = "_") %>%
  mutate(Metric = case_when(Metric == 'rmse' ~ 'RMSE',
                            Metric == 'spearmanr' ~ 'Spearman R')) %>%
  filter(Metric == 'Spearman R') %>%
  ggplot(aes(x = train_size, y = Performance, color = Group))+
  stat_summary()+
  geom_smooth(se = F)+
  facet_wrap(~Metric, scales = 'free')+
  xlim(0,420)+
  ylim(0,1)+
  scale_color_manual(values = c('black', 'gray'), breaks = c('train', 'test'))+
  labs(title = 'Inbred on Inbred Learning Curve',
       x = 'Training Size')+
  theme_classic()+
  theme(text = element_text(size = 15))

# Attempt transfer learning with inbred and hybrid data
# Combine the data into one dataset
combined_data = hybrid_data %>%
  rename(Moisture_Uptake = Moisture.Avg) %>%
  bind_rows(inbred_data %>%
              select(-Genotype))

# Create the learning curve for the combined data
combined_lc = learning_curve(outcome_col = 1,
                           data = combined_data[2:143],
                           test_partition = 0.08, # Matching the number of samples from hybrids
                           reps = 10,
                           iterations = 3:60, # Trying to get close to the spacing of data in the hybrids
                           model = "pls",
                           metric = "Rsquared",
                           tuneparams = expand.grid(ncomp = 15))

#print(combined_lc) # Show the data

# Plot the curves
combined_lc %>%
  pivot_longer(cols = c(test_rmse, train_rmse, test_spearmanr, train_spearmanr), names_to = 'Group_Metric', values_to = 'Performance') %>%
  separate(col = Group_Metric, into = c('Group', 'Metric'), sep = "_") %>%
  mutate(Metric = case_when(Metric == 'rmse' ~ 'RMSE',
                            Metric == 'spearmanr' ~ 'Spearman R')) %>%
  filter(Metric == 'RMSE') %>%
  ggplot(aes(x = train_size, y = Performance, color = Group))+
  stat_summary()+
  geom_smooth(se = F)+
  facet_wrap(~Metric, scales = 'free')+
  xlim(0,420)+
  ylim(0,0.07)+
  scale_color_manual(values = c('black', 'gray'), breaks = c('train', 'test'))+
  labs(title = 'Inbred + Hybrid on Hybrid Learning Curve',
       x = 'Training Size')+
  theme_classic()+
  theme(text = element_text(size = 15))

combined_lc %>%
  pivot_longer(cols = c(test_rmse, train_rmse, test_spearmanr, train_spearmanr), names_to = 'Group_Metric', values_to = 'Performance') %>%
  separate(col = Group_Metric, into = c('Group', 'Metric'), sep = "_") %>%
  mutate(Metric = case_when(Metric == 'rmse' ~ 'RMSE',
                            Metric == 'spearmanr' ~ 'Spearman R')) %>%
  filter(Metric == 'Spearman R') %>%
  ggplot(aes(x = train_size, y = Performance, color = Group))+
  stat_summary()+
  geom_smooth(se = F)+
  facet_wrap(~Metric, scales = 'free')+
  xlim(0,420)+
  ylim(0,1)+
  scale_color_manual(values = c('black', 'gray'), breaks = c('train', 'test'))+
  labs(title = 'Inbred + Hybrid on Hybrid Learning Curve',
       x = 'Training Size')+
  theme_classic()+
  theme(text = element_text(size = 15))

# According to this data, the combined data should be pretty good at predicting!  But I am not so sure that is true.  A lot of 
# this success is likely coming from the inbreds. I think that we either need to perform this function but guarantee the test
# group is only new hybrids (perhaps everything that isn't PA?), and see the performance.  It might also be worth while to create
# a sort of transfer learning system where the SVML predictions are a feature and a new model basically predicts how wrong the
# SVML model is based on NIR spectra. 

# Try creating learning curves for the data where the testing set is a random group of hybrids (about the same number we have been working with) and the training set is the original inbreds
hybrid_test_index = createDataPartition(hybrid_data$Moisture.Avg, p = 0.2, list = F)

hybrid_train = hybrid_data[-hybrid_test_index,]

training_full = inbred_data %>%
                  select(-Genotype)
  
testing = hybrid_data[hybrid_test_index,] %>%
  rename(Moisture_Uptake = Moisture.Avg)

dim(training_full) 
dim(testing)

# Create storage variable
performance = tibble(rep = NULL, p = NULL, test = NULL, train = NULL, train_size = NULL)
for(rep in 1:10){
  print(rep)
  for(p in 3:60){
    set.seed(rep)
    training_index = createDataPartition(training_full$Moisture_Uptake, p = p/60, list = F)
    training = training_full[training_index,]
    
    # Train model
    pls_model = train(Moisture_Uptake ~ ., 
                      data = training %>%
                        select_if(is.numeric), 
                      method = 'pls', 
                      metric = 'Rsquared', 
                      tuneGrid = expand_grid(ncomp = 15),
                      trControl = trainControl(method = "none",
                                               savePredictions = T,
                                               allowParallel = T
                      )
    )
    
    # Predict test and train samples
    test_preds = predict(pls_model, testing)
    train_preds = predict(pls_model, training)
    
    # Add to storage variable
    performance = bind_rows(performance, tibble(rep = rep,
                                                p = p,
                                                test_rmse = RMSE(test_preds, testing$Moisture_Uptake),
                                                train_rmse = RMSE(train_preds, training$Moisture_Uptake),
                                                test_spearmanr = cor(testing$Moisture_Uptake, test_preds, method = 'spearman'),
                                                train_spearmanr = cor(training$Moisture_Uptake, train_preds, method = 'spearman'),
                                                train_size = nrow(training)))
  }
}

#print(performance) # Show the data

# Plot the curves
performance %>%
  pivot_longer(cols = c(test_rmse, train_rmse, test_spearmanr, train_spearmanr), names_to = 'Group_Metric', values_to = 'Performance') %>%
  separate(col = Group_Metric, into = c('Group', 'Metric'), sep = "_") %>%
  mutate(Metric = case_when(Metric == 'rmse' ~ 'RMSE',
                            Metric == 'spearmanr' ~ 'Spearman R')) %>%
  filter(Metric == 'RMSE') %>%
  ggplot(aes(x = train_size, y = Performance, color = Group))+
  stat_summary()+
  geom_smooth(se = F)+
  facet_wrap(~Metric, scales = 'free')+
  xlim(0,420)+
  ylim(0,0.07)+
  scale_color_manual(values = c('black', 'gray'), breaks = c('train', 'test'))+
  labs(title = 'Inbred on Hybrid Learning Curve',
       x = 'Training Size')+
  theme_classic()+
  theme(text = element_text(size = 15))

performance %>%
  pivot_longer(cols = c(test_rmse, train_rmse, test_spearmanr, train_spearmanr), names_to = 'Group_Metric', values_to = 'Performance') %>%
  separate(col = Group_Metric, into = c('Group', 'Metric'), sep = "_") %>%
  mutate(Metric = case_when(Metric == 'rmse' ~ 'RMSE',
                            Metric == 'spearmanr' ~ 'Spearman R')) %>%
  filter(Metric == 'Spearman R') %>%
  ggplot(aes(x = train_size, y = Performance, color = Group))+
  stat_summary()+
  geom_smooth(se = F)+
  facet_wrap(~Metric, scales = 'free')+
  xlim(0,420)+
  ylim(0,1)+
  scale_color_manual(values = c('black', 'gray'), breaks = c('train', 'test'))+
  labs(title = 'Inbred on Hybrid Learning Curve',
       x = 'Training Size')+
  theme_classic()+
  theme(text = element_text(size = 15))

# Learning Curves for Transformed Spectra
# Descatter Data
snv_hybrid_data = hybrid_data %>%
  select(1:2) %>%
  bind_cols(as_tibble(detrend(hybrid_data[,3:143], wav = as.numeric(colnames(hybrid_data[,3:143])), p = 2))) 

hybrid_data %>%
  pivot_longer(cols = -c(Sample_ID, Moisture.Avg), names_to = 'Waveband', values_to = 'Absorbance') %>%
  mutate(Waveband = as.numeric(Waveband)) %>%
  ggplot(aes(x = Waveband, y = Absorbance, group = Sample_ID))+
  geom_line()+
  theme_classic()+
  theme(text = element_text(size = 15))

snv_hybrid_data %>%
  pivot_longer(cols = -c(Sample_ID, Moisture.Avg), names_to = 'Waveband', values_to = 'Absorbance') %>%
  mutate(Waveband = as.numeric(Waveband)) %>%
  ggplot(aes(x = Waveband, y = Absorbance, group = Sample_ID))+
  geom_line()+
  theme_classic()+
  theme(text = element_text(size = 15))

# Create a learning curve for the hybrid data
snv_hybrid_lc = learning_curve(outcome_col = 1,
                           data = snv_hybrid_data[2:143],
                           test_partition = 0.2,
                           reps = 10,
                           iterations = 4:30,
                           model = "pls",
                           metric = "Rsquared",
                           tuneparams = expand.grid(ncomp = 15))

#print(hybrid_lc) # Look at the data

# Plot out the curves
snv_hybrid_lc %>%
  pivot_longer(cols = c(test_rmse, train_rmse, test_spearmanr, train_spearmanr), names_to = 'Group_Metric', values_to = 'Performance') %>%
  separate(col = Group_Metric, into = c('Group', 'Metric'), sep = "_") %>%
  mutate(Metric = case_when(Metric == 'rmse' ~ 'RMSE',
                            Metric == 'spearmanr' ~ 'Spearman R')) %>%
  filter(Metric == 'RMSE') %>%
  ggplot(aes(x = train_size, y = Performance, color = Group))+
  stat_summary()+
  geom_smooth(se = F)+
  facet_wrap(~Metric, scales = 'free')+
  xlim(0,420)+ # This is to keep it consistent with the inbreds
  ylim(0,0.07)+
  scale_color_manual(values = c('black', 'gray'), breaks = c('train', 'test'))+
  labs(title = 'Hybrid on Hybrid Learning Curve',
       x = 'Training Size')+
  theme_classic()+
  theme(text = element_text(size = 15))

snv_hybrid_lc %>%
  pivot_longer(cols = c(test_rmse, train_rmse, test_spearmanr, train_spearmanr), names_to = 'Group_Metric', values_to = 'Performance') %>%
  separate(col = Group_Metric, into = c('Group', 'Metric'), sep = "_") %>%
  mutate(Metric = case_when(Metric == 'rmse' ~ 'RMSE',
                            Metric == 'spearmanr' ~ 'Spearman R')) %>%
  filter(Metric == 'Spearman R') %>%
  ggplot(aes(x = train_size, y = Performance, color = Group))+
  stat_summary()+
  geom_smooth(se = F)+
  facet_wrap(~Metric, scales = 'free')+
  xlim(0,420)+ # This is to keep it consistent with the inbreds
  ylim(0,1)+
  scale_color_manual(values = c('black', 'gray'), breaks = c('train', 'test'))+
  labs(title = 'Hybrid on Hybrid Learning Curve',
       x = 'Training Size')+
  theme_classic()+
  theme(text = element_text(size = 15))

# Remove the Baseline
bsln_hybrid_data = snv_hybrid_data %>%
  select(1:2) %>%
  bind_cols(as_tibble(baseline(X = snv_hybrid_data[,3:143], wav = as.numeric(colnames(snv_hybrid_data[,3:143])))))

bsln_hybrid_data %>%
  pivot_longer(cols = -c(Sample_ID, Moisture.Avg), names_to = 'Waveband', values_to = 'Absorbance') %>%
  mutate(Waveband = as.numeric(Waveband)) %>%
  ggplot(aes(x = Waveband, y = Absorbance, group = Sample_ID))+
  geom_line()+
  theme_classic()+
  theme(text = element_text(size = 15))

# Create a learning curve for the hybrid data
bsln_hybrid_lc = learning_curve(outcome_col = 1,
                               data = bsln_hybrid_data[2:143],
                               test_partition = 0.2,
                               reps = 10,
                               iterations = 4:30,
                               model = "pls",
                               metric = "Rsquared",
                               tuneparams = expand.grid(ncomp = 15))

#print(hybrid_lc) # Look at the data

# Plot out the curves
bsln_hybrid_lc %>%
  pivot_longer(cols = c(test_rmse, train_rmse, test_spearmanr, train_spearmanr), names_to = 'Group_Metric', values_to = 'Performance') %>%
  separate(col = Group_Metric, into = c('Group', 'Metric'), sep = "_") %>%
  mutate(Metric = case_when(Metric == 'rmse' ~ 'RMSE',
                            Metric == 'spearmanr' ~ 'Spearman R')) %>%
  filter(Metric == 'RMSE') %>%
  ggplot(aes(x = train_size, y = Performance, color = Group))+
  stat_summary()+
  geom_smooth(se = F)+
  facet_wrap(~Metric, scales = 'free')+
  xlim(0,420)+ # This is to keep it consistent with the inbreds
  ylim(0,0.07)+
  scale_color_manual(values = c('black', 'gray'), breaks = c('train', 'test'))+
  labs(title = 'Hybrid on Hybrid Learning Curve',
       x = 'Training Size')+
  theme_classic()+
  theme(text = element_text(size = 15))

bsln_hybrid_lc %>%
  pivot_longer(cols = c(test_rmse, train_rmse, test_spearmanr, train_spearmanr), names_to = 'Group_Metric', values_to = 'Performance') %>%
  separate(col = Group_Metric, into = c('Group', 'Metric'), sep = "_") %>%
  mutate(Metric = case_when(Metric == 'rmse' ~ 'RMSE',
                            Metric == 'spearmanr' ~ 'Spearman R')) %>%
  filter(Metric == 'Spearman R') %>%
  ggplot(aes(x = train_size, y = Performance, color = Group))+
  stat_summary()+
  geom_smooth(se = F)+
  facet_wrap(~Metric, scales = 'free')+
  xlim(0,420)+ # This is to keep it consistent with the inbreds
  ylim(0,1)+
  scale_color_manual(values = c('black', 'gray'), breaks = c('train', 'test'))+
  labs(title = 'Hybrid on Hybrid Learning Curve',
       x = 'Training Size')+
  theme_classic()+
  theme(text = element_text(size = 15))
