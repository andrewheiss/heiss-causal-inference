library(tidyverse)
library(scales)
library(broom)

# Mosquito net example ----------------------------------------------------

# Create confounder variables that are related to each other
mu <- c(income = 900, temperature = 75, health = 50)
stddev <- c(income = 200, temperature = 10, health = 20)
lower <- c(income = 100, temperature = 60, health = 5)
upper <- c(income = 2000, temperature = 90, health = 100)

# https://stackoverflow.com/a/46563034/120898
correlations_confounders <- tribble(
  ~var1, ~var2, ~correlation,
  "income", "temperature", 0.2,
  "income", "health", 0.8,
  # "temperature", "health", 0.6,
  "temperature", "health", 0.2,
) %>%
  mutate_at(vars(starts_with("var")),
            ~factor(., levels = c("income", "temperature", "health"))) %>%
  xtabs(correlation ~ var1 + var2, ., drop.unused.levels = FALSE) %>%
  '+'(., t(.)) %>%
  `diag<-`(1) %>%
  as.data.frame.matrix() %>% as.matrix()

# Convert correlation matrix to covariance matrix using fancy math
cov_matrix_confounders <- stddev %*% t(stddev) * correlations_confounders

# Force the covariance matrix to be positive definite and symmetric
# https://stats.stackexchange.com/q/153166/3025
sigma <- as.matrix(Matrix::nearPD(cov_matrix_confounders)$mat)


# Make observational data -------------------------------------------------

num <- 1752

set.seed(123)
confounders <- tmvtnorm::rtmvnorm(num, mean = mu, sigma = sigma,
                                  lower = lower, upper = upper) %>%
  magrittr::set_colnames(names(mu)) %>% as_tibble() %>%
  mutate(health = round(health, 0),
         temperature = round(temperature, 1))

set.seed(123)
mosquito_nets <- tibble(id = 1:num) %>%
  bind_cols(confounders) %>%
  mutate(household = rpois(n(), 2) + 1) %>% 
  mutate(enrolled = household > 4 & income < 700) %>% 
  mutate(resistance = rescale(rnorm(n(), 0, 1), to = c(5, 95))) %>% 
  # Simulate data from a logit model: https://stats.stackexchange.com/a/46525/3025
  # But then do all sorts of weird distortion to change the likelihood of using a net
  mutate(net_effect = (1.85 * income / 10) + (-1.7 * temperature) + (1.8 * health / 10) + 
           (150 * enrolled) + (2.9 * household),
         net_diff = net_effect - mean(net_effect),
         net_effect = ifelse(net_diff < 0, net_effect - (net_diff / 2), net_effect),
         net_effect_rescaled = rescale(net_effect, to = c(-2.2, 2.2)),
         inv_logit = 1 / (1 + exp(-net_effect_rescaled)),
         net_num = rbinom(n(), 1, inv_logit),
         net = net_num == 1) %>% 
  mutate(malaria_risk_effect = (-5 * income / 10) + (3.9 * temperature) + 
           (1.4 * resistance) + (9 * health / 10) + (-80 * net_num),
         malaria_risk_diff = malaria_risk_effect - mean(malaria_risk_effect),
         malaria_risk_effect = ifelse(malaria_risk_diff < 0, 
                                      malaria_risk_effect - (malaria_risk_diff / 2), 
                                      malaria_risk_effect),
         malaria_risk_effect_rescaled = rescale(malaria_risk_effect, to = c(-2.2, 2.2)),
         malaria_risk = 1 / (1 + exp(-malaria_risk_effect_rescaled)),
         malaria_risk = round(malaria_risk * 100, 0)) %>% 
  mutate_at(vars(income, resistance), ~round(., 0)) %>% 
  mutate(temperature = (temperature - 32) * 5/9,
         temperature = round(temperature, 1)) %>% 
  mutate(malaria_risk = malaria_risk)

mosquito_nets_final <- mosquito_nets %>% 
  select(id, net, net_num, malaria_risk, income, health, household, 
         eligible = enrolled, temperature, resistance)

write_csv(mosquito_nets_final, "data/mosquito_nets.csv")


# Education and age example -----------------------------------------------

set.seed(1234)
edu_age_treatment <- tibble(education = rnorm(30, 20, 3),
                            age = rnorm(30, 50, 8),
                            treatment = "Treatment")

edu_age_control <- tibble(education = runif(70, 
                                            min(edu_age_treatment$education) - 2, 
                                            max(edu_age_treatment$education) + 2),
                          age = runif(70,
                                      min(edu_age_treatment$age) - 5, 
                                      max(edu_age_treatment$age) + 5),
                          treatment = "Control")

edu_age <- bind_rows(edu_age_treatment, edu_age_control) %>% 
  mutate(treatment_num = recode(treatment, Treatment = 1, Control = 0)) %>% 
  mutate(id = 1:n()) %>% 
  select(id, treatment, treatment_num, education, age)

write_csv(edu_age, "data/edu_age.csv")
