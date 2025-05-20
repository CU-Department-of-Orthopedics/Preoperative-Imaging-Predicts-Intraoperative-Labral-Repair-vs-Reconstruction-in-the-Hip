## Data Read/Clean 

rm(list = ls())

library(readxl)
library(tidyverse)

dat <- ...
dat_unique <- dat %>% 
  distinct_at(vars(patient_id), .keep_all = T)

dat_unique %>% group_by(recon) %>% summarize(n = n())

# table(dat$location)
# str(dat)

## Clean 

dat <- dat %>% 
  mutate(recon = ifelse(recon == "Y", 1, 0)) %>% 
  mutate_if(is.character, as.factor)

# dat %>% group_by(recon) %>% summarize(n = n())

dat_acet <- dat %>% filter(location == "T2-Acetabulum")
dat_fem <- dat %>% filter(location == "T2-Femur")
dat_labr <- dat %>% filter(location == "T2-Labrum")

dat_dem <- ...
