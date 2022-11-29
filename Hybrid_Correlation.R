library(tidyverse)
library(readxl)
library(magrittr)

pepsico_data = read_xlsx('Desktop/Grad_School/Research/Projects/Machine_Learning/Hybrid_Validation/Hybrid_2015_PepsiCo_Moisture.xlsx', sheet = 1) %>%
  filter(!is.na(Moisture_Content)) %>%
  select(SampleID, Moisture_Content) %>%
  rename(Sample_ID = SampleID)
dim(pepsico_data)

benchtop_data = read_xlsx('Desktop/Grad_School/Research/Projects/Machine_Learning/Hybrid_Validation/Hybrid_2015_PepsiCo_Moisture.xlsx', sheet = 2) %>%
  group_by(Sample_ID) %>%
  summarise(Moisture_Content_Bench = mean(Moisture.Avg))

benchtop_data %>%
  filter(Sample_ID %in% pepsico_data$Sample_ID) %>%
  left_join(pepsico_data) %$%
  cor(Moisture_Content, Moisture_Content_Bench)

benchtop_data %>%
  filter(Sample_ID %in% pepsico_data$Sample_ID) %>%
  left_join(pepsico_data) %>%
  ggplot(aes(x = Moisture_Content, y = Moisture_Content_Bench))+
  geom_smooth(method = 'lm', se = F)+
  geom_abline()+
  geom_point()+
  theme_classic()
