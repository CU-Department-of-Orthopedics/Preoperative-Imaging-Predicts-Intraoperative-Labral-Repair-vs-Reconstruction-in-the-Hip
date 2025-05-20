### Analysis 

source("Data Clean.R")

library(kableExtra)

# Models (try)

# (1|patient_id:rater:pass_num:measurement_num:zone)

## Bayesian Mixed Models 
library(brms)

priors <- c(
  set_prior("normal(0, 5)", class = "Intercept"),
  set_prior("student_t(10, 0, 1)", class = "b"),
  set_prior("gamma(1, 2)", class = "sd")
)

# mod_labr2 <- brm(recon ~ mapping_value + age_yrs + sex +
#                   (1|patient_id:rater:pass_num:measurement_num:zone),
#                 data = dat_labr,
#                 family = bernoulli(link = "logit"),
#                 prior = priors,
#                 chains = 4,
#                 warmup = 1000,
#                 iter = 6000,
#                 control = list(adapt_delta = 0.9)
#                 )
# 
# saveRDS(mod_labr2, "labr_mod2.RDS")

mod_labr2 <- readRDS("labr_mod2.RDS")

labr_posterior_samples <- posterior_samples(mod_labr2)[, 1:4]

# Exponentiate coefficients
labr_exp_samples <- exp(labr_posterior_samples)

# CIs for Odds 
quantile(1-labr_exp_samples$b_mapping_value, c(0.025, 0.975))*100
quantile(labr_exp_samples$b_age_yrs, c(0.025, 0.975))
quantile(labr_exp_samples$b_sexM, c(0.025, 0.975))*100


mod_labr2_sum <- as.data.frame(fixef(mod_labr2)) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  mutate(
    `95% Cred. Int` = paste0("[", Q2.5, ", ", Q97.5, "]"),
    Signficance = ifelse(
      test = sign(Q2.5) + sign(Q97.5) != 0, 
      yes = "S",
      no = "NS")
  ) %>% 
  select(1, 2, 5, 6) %>% 
  `names<-`(c("Coefficient", "Est. Error", "95% Cred. Int.", "Signficance")) %>% 
  `rownames<-`(c("(Intercept)", "Mapping Value", "Age (yrs)", "Female (0) vs Male (1)")) %>% 
  kable() %>% 
  kable_classic(html_font = 'times', full_width = F)



## Plots 
dat_labr_p <- dat_labr
dat_labr_p$recon <- factor(dat_labr_p$recon, labels = c("No Reconstruction", "Needed Reconstruction"))

dat_labr_p_sum <- dat_labr_p %>% 
  group_by(patient_id, recon, age_yrs) %>% 
  summarize(
    mean_map_value = mean(mapping_value)
  )

labr_map_p <- ggplot(
  data = dat_labr_p_sum,
  aes(
    x = recon,
    y = mean_map_value,
    fill = recon
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  labs(
    x = "",
    y = "Mean Labral Mapping Value",
    subtitle = "(Averaged per Patient)"
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "none"
  )


ggplot(
  data = dat_labr_p_sum,
  aes(
    y = mean_map_value,
    x = age_yrs,
    color = recon
  )
) + 
  geom_point(
    
  ) + 
  geom_smooth(
    method = "lm"
  ) + 
  labs(
    x = "Age (yrs)",
    y = "Mean Labral Mapping Value",
    subtitle = "(Averaged per Patient)",
    color = ""
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  )


## Conditional Effects 
labr_effects <- conditional_effects(mod_labr2)

labr_map_value_effect <- as.data.frame(labr_effects$mapping_value)
labr_age_effect <- as.data.frame(labr_effects$age_yrs)
labr_sex_effect <- as.data.frame(labr_effects$sex)

## Mapping Value: Logistic Plot 
labr_mv_p <- ggplot(
  data = labr_map_value_effect, 
  aes(
    x = mapping_value, 
    y = estimate__ * 100
    )
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_ribbon(
    aes(
      ymin = lower__*100, 
      ymax = upper__*100
      ), 
    alpha = 0.2, 
    fill = "blue") +
  labs(
    x = "Labral Mapping Value", 
    y = "Estimated Probability of Reconstruction"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1)
    ) +
  theme_bw(
    
  )

ggsave(filename = "labr_mv_p.tiff", path = "HiRes Figures", width = 4, height = 2, device='tiff', dpi=600, scaling = .5, units = "in")



## Age: Logistic Plot, THIS FIGURE IN POSTER 9/23/24
labr_age_p <- ggplot(
  data = labr_age_effect, 
  aes(
    x = age_yrs, 
    y = estimate__ * 100
  )
) +
  geom_line(
    linewidth = 1
  ) +
  geom_ribbon(
    aes(
      ymin = lower__*100, 
      ymax = upper__*100
    ), 
    alpha = 0.2, 
    fill = "blue") +
  labs(
    x = "Age (yrs)", 
    y = "Estimated Probability of Reconstruction"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1)
  ) +
  theme_bw(
    
  )

ggsave(filename = "labr_age_p.tiff", path = "HiRes Figures", width = 4, height = 2, device='tiff', dpi=600, scaling = .5, units = "in")



## Sex: Logistic Plot 
labr_sex_p <- ggplot(
  data = labr_sex_effect, 
  aes(
    x = factor(sex, labels = c("Female", "Male")), 
    y = estimate__ * 100,
    color = factor(sex, labels = c("Female", "Male")), 
  )
) + 
  geom_errorbar(
    aes(
      ymin = lower__*100, 
      ymax = upper__*100
      ),
    size = 1
    ) +
  geom_point(
    size = 4,
    color = "black"
  ) +
  labs(
    x = "", 
    y = "Estimated Probability of Reconstruction"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1)
  ) +
  theme_bw(
    
  ) + 
  theme(
    legend.position = "none"
  )

ggsave(filename = "labr_sex_p.tiff", path = "HiRes Figures", width = 4, height = 2, device='tiff', dpi=600, scaling = .5, units = "in")

