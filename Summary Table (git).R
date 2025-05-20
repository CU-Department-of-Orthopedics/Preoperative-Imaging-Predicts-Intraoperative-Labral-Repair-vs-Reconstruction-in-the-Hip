## Summary Statistics 

source("Data Clean.R")

library(tidyverse)

{library(gtsummary)
  
  reset_gtsummary_theme() 
  
  summary_render <- list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  )
  
  
  anova_stat_render <- list(
    all_continuous() ~ "aov", 
    all_categorical() ~ "chisq.test")
  
  
  ttest_stat_render <- list(
    all_continuous() ~ "t.test", 
    all_categorical() ~ "chisq.test")
  
}


## Demographics 
dat_dem$recon <- factor(dat_dem$recon, labels = c("Repair", "Reconstruction"))

dat_dem_tab <- dat_dem %>% 
  select(recon, age_yrs, sex, bmi) %>% 
  tbl_summary(
    by = recon,
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c(
        "{mean} ({sd})", 
        "{median} ({p25}, {p75})",
        "{min}, {max}"),      
      all_categorical() ~ "{n} ({p}%)"
    ), 
    digits = list(
      all_continuous() ~ 2
    ),
    missing_text = "(Missing)",
    label = list(
      age_yrs ~ "Age (yrs)",
      sex ~ "Sex",
      bmi ~ "BMI"
    )
  ) %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_p(    
    test = ttest_stat_render
  )

dat_dem_tab



## Acetabulum 

dat_acet_sum <- dat_acet %>% 
  group_by(
    patient_id, recon
  ) %>% 
  summarize(mean_mapping_value = mean(mapping_value)) %>% 
  ungroup()

dat_acet_sum$recon <- factor(dat_acet_sum$recon, labels = c("Repair", "Reconstruction"))

dat_acet_sum_tab <- dat_acet_sum %>% 
  select(recon, mean_mapping_value) %>% 
  tbl_summary(
    by = recon,
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c(
        "{mean} ({sd})", 
        "{median} ({p25}, {p75})",
        "{min}, {max}"),      
      all_categorical() ~ "{n} ({p}%)"
    ), 
    digits = list(
      all_continuous() ~ 2
    ),
    missing_text = "(Missing)",
    label = list(
      mean_mapping_value ~ "Acetabular Mean Mapping Value"
    )
  ) %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_p(    
    test = ttest_stat_render
  ) %>%
  as_gt(include = -cols_align) %>%
  gt::tab_source_note(gt::md("*Note: Patients are averaged over rater, & pass, measurement, and zone numbers*"))

dat_acet_sum_tab


## Labrum 
dat_labr_sum <- dat_labr %>% 
  group_by(
    patient_id, recon
  ) %>% 
  summarize(mean_mapping_value = mean(mapping_value)) %>% 
  ungroup()

dat_labr_sum$recon <- factor(dat_labr_sum$recon, labels = c("Repair", "Reconstruction"))

dat_labr_sum_tab <- dat_labr_sum %>% 
  dplyr::select(recon, mean_mapping_value) %>% 
  tbl_summary(
    by = recon,
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c(
        "{mean} ({sd})", 
        "{median} ({p25}, {p75})",
        "{min}, {max}"),      
      all_categorical() ~ "{n} ({p}%)"
    ), 
    digits = list(
      all_continuous() ~ 2
    ),
    missing_text = "(Missing)",
    label = list(
      mean_mapping_value ~ "Labral Mean Mapping Value"
    )
  ) %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_p(    
    test = ttest_stat_render
  ) %>%
  as_gt(include = -cols_align) %>%
  gt::tab_source_note(gt::md("*Note: Patients are averaged over rater, & pass, measurement, and zone numbers*"))

dat_labr_sum_tab


# Femur 
dat_fem_sum <- dat_fem %>% 
  group_by(
    patient_id, recon
  ) %>% 
  summarize(mean_mapping_value = mean(mapping_value)) %>% 
  ungroup()

dat_fem_sum$recon <- factor(dat_fem_sum$recon, labels = c("Repair", "Reconstruction"))

dat_fem_sum_tab <- dat_fem_sum %>% 
  dplyr::select(recon, mean_mapping_value) %>% 
  tbl_summary(
    by = recon,
    type = all_continuous() ~ "continuous2",
    statistic = list(
      all_continuous() ~ c(
        "{mean} ({sd})", 
        "{median} ({p25}, {p75})",
        "{min}, {max}"),      
      all_categorical() ~ "{n} ({p}%)"
    ), 
    digits = list(
      all_continuous() ~ 2
    ),
    missing_text = "(Missing)",
    label = list(
      mean_mapping_value ~ "Femur Mean Mapping Value"
    )
  ) %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  add_p(    
    test = ttest_stat_render
  ) %>%
  as_gt(include = -cols_align) %>%
  gt::tab_source_note(gt::md("*Note: Patients are averaged over rater, & pass, measurement, and zone numbers*"))

dat_fem_sum_tab















