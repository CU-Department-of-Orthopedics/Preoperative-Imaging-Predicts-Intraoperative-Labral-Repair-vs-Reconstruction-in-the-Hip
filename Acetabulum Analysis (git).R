### Analysis 

source("Data Clean.R")

library(kableExtra)

# Models (try)

## Bayesian Mixed Models 
library(brms)

priors <- c(
  set_prior("normal(0, 5)", class = "Intercept"),
  set_prior("student_t(10, 0, 1)", class = "b"), 
  set_prior("gamma(1, 2)", class = "sd")
)

# mod_acet2 <- brm(recon ~ mapping_value + age_yrs + sex +
#                    (1|patient_id:rater:pass_num:measurement_num:zone),
#                 data = dat_acet,
#                 family = bernoulli(link = "logit"),
#                 prior = priors,
#                 chains = 4,
#                 warmup = 1000,
#                 iter = 6000,
#                 control = list(adapt_delta = 0.9))
# 
# saveRDS(mod_acet2, "acet_mod2.RDS")

mod_acet2 <- readRDS("acet_mod2.RDS")

acet_posterior_samples <- posterior_samples(mod_acet2)[, 1:4]

# Exponentiate coefficients
acet_exp_samples <- exp(acet_posterior_samples)

# CIs for Odds 
quantile(1-acet_exp_samples$b_mapping_value, c(0.025, 0.975))*100
quantile(acet_exp_samples$b_age_yrs, c(0.025, 0.975))*100
quantile(acet_exp_samples$b_sexM, c(0.025, 0.975))*100



mod_acet2_sum <- as.data.frame(fixef(mod_acet2)) %>% 
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
dat_acet_p <- dat_acet
dat_acet_p$recon <- factor(dat_acet_p$recon, labels = c("No Reconstruction", "Needed Reconstruction"))

dat_acet_p_sum <- dat_acet_p %>% 
  group_by(patient_id, recon) %>% 
  summarize(
    mean_map_value = mean(mapping_value)
  )

acet_p1 <- ggplot(
  data = dat_acet_p_sum,
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
    y = "Acetabulum Mapping Value"
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "none"
  )


## Conditional Effects 
acet_effects <- conditional_effects(mod_acet2)

acet_map_value_effect <- as.data.frame(acet_effects$mapping_value)
acet_age_effect <- as.data.frame(acet_effects$age_yrs)
acet_sex_effect <- as.data.frame(acet_effects$sex)

## Mapping Value: Logistic Plot 
acet_mv_p <- ggplot(
  data = acet_map_value_effect, 
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
    x = "Acetabular Mapping Value", 
    y = "Estimated Probability of Reconstruction"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1)
  ) +
  theme_bw(
    
  )

ggsave(filename = "acet_mv_p.tiff", path = "HiRes Figures", width = 4, height = 2, device='tiff', dpi=600, scaling = .5, units = "in")



## Age: Logistic Plot 
acet_age_p <- ggplot(
  data = acet_age_effect, 
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

ggsave(filename = "acet_age_p.tiff", path = "HiRes Figures", width = 4, height = 2, device='tiff', dpi=600, scaling = .5, units = "in")


## Sex: Logistic Plot 
acet_sex_p <- ggplot(
  data = acet_sex_effect, 
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

ggsave(filename = "acet_sex_p.tiff", path = "HiRes Figures", width = 4, height = 2, device='tiff', dpi=600, scaling = .5, units = "in")




