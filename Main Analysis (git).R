### Analysis 

source("Data Clean.R")

library(kableExtra)

# Models (try)

## GLMER 
library(lme4)

mod1 <- glmer(recon ~ mapping_value + (1|patient_id) + (1|rater) + (1|pass_num) + (1|measurement_num), 
              data = dat_labr, family = binomial(link = "logit"), 
              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5)))

summary(mod1)

mod2 <- glmer(recon ~ mapping_value + (1|patient_id) + (1|rater), 
              data = dat_labr, family = binomial(link = "logit"), 
              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5)))

summary(mod2)

mod3 <- glmer(recon ~ mapping_value + (1|patient_id:rater), 
              data = dat_labr, family = binomial(link = "logit"), 
              control = glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 2e5)))

summary(mod3)

AIC(mod1, mod2, mod3)

## Bayesian Mixed Models 
library(brms)

priors <- c(
  set_prior("normal(0, 1)", class = "Intercept"),
  set_prior("student_t(10, 0, 1)", class = "b"), 
  set_prior("cauchy(0, 1)", class = "sd")
)

# mod_labr1 <- brm(recon ~ mapping_value +
#                  (1|patient_id) + (1|rater),
#                data = dat_labr,
#                family = bernoulli(link = "logit"),
#                prior = priors,
#                chains = 4,
#                warmup = 1000,
#                iter = 8000,
#                control = list(adapt_delta = 0.9))
# 
# saveRDS(mod_labr1, "bays_mod1.RDS")
# 
# mod_labr2 <- brm(recon ~ mapping_value + age_yrs +
#                   (1|patient_id) + (1|rater), 
#                 data = dat_labr, 
#                 family = bernoulli(link = "logit"),
#                 prior = priors,
#                 chains = 4,
#                 warmup = 1000,
#                 iter = 8000,
#                 control = list(adapt_delta = 0.9))

# saveRDS(mod_labr2, "bays_mod2.RDS")

mod_labr1 <- readRDS("bays_mod1.RDS")

mod_bay1_sum <- as.data.frame(fixef(mod_labr1)) %>% 
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
  `rownames<-`(c("(Intercept)", "Mapping Value")) %>% 
  kable() %>% 
  kable_classic(html_font = 'times', full_width = F)



mod_labr2 <- readRDS("bays_mod2.RDS")

mod_bay2_sum <- as.data.frame(fixef(mod_labr2)) %>% 
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
  `rownames<-`(c("(Intercept)", "Mapping Value", "Age (yrs)")) %>% 
  kable() %>% 
  kable_classic(html_font = 'times', full_width = F)



## Plots 
dat_labr_p <- dat_labr
dat_labr_p$recon <- factor(dat_labr_p$recon, labels = c("No Reconstruction", "Needed Reconstruction"))

dat_labr_p_sum <- dat_labr_p %>% 
  group_by(patient_id, recon) %>% 
  summarize(
    mean_map_value = mean(mapping_value)
  )

ggplot(
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
    y = "Labrum Mapping Value"
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "none"
  )






