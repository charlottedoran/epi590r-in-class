#Load libraries
library(tidyverse)
library(broom)
library(tidycat)
library(sandwich)
library(lmtest)

#Prep dataset
nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")))

#Create models
poisson_mod <- glm(data = nlsy,
										glasses ~ eyesight_cat + sex_cat,
										family = poisson()) %>%
	coeftest(vcov = vcovHC,
					 type = "HC1")

logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy, family = binomial(link = "log"))


tidy_poisson_mod <- tidy(poisson_mod, conf.int = TRUE)
tidy_logistic_mod <- tidy(logistic_model, conf.int = TRUE)

#Combine models
bind_rows(tidy_poisson_mod, tidy_logistic_mod,
					.id = "Model")
