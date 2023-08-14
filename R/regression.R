#Load libraries
library(tidyverse)
library(gtsummary)

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
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))

#Univariate regression
nlsy %>%
	tbl_uvregression(
		x = sex_cat,
		include = c(starts_with("sleep"), income, nsibs),
		method = lm
	)

#Poisson regression
poisson_reg <- glm(data = nlsy, nsibs ~
									 	sex_cat +
									  race_eth_cat +
									  sleep_wkdy, family = poisson())

tbl_regression(
	poisson_reg,
	label = list(sex_cat ~ "Sex",
							 race_eth_cat ~ "Race/ethnicity",
							 sleep_wkdy ~ "Weekday sleep duration"),
	exponentiate = TRUE
	)

#Logistic regression with RR
logistic_model <- glm(glasses ~ eyesight_cat + sex_cat,
											data = nlsy, family = binomial(link = "log"))

tbl_log_binomial <- tbl_regression(
	logistic_model,
	label = list(eyesight_cat ~ "Eyesight category",
							 sex_cat ~ "Sex"),
	exponentiate = TRUE
)

#Poisson regression with RR
poisson_reg2 <- glm(data = nlsy,
										glasses ~ eyesight_cat + sex_cat,
										family = poisson())
poisson_reg2

tbl_log_poisson <- tbl_regression(
	poisson_reg2,
	label = list(eyesight_cat ~ "Eyesight category",
							 sex_cat ~ "Sex"),
	exponentiate = TRUE,
	tidy_fun = partial(tidy_robust, vcov = "HC1")
)

#Compare models
tbl_merge(list(tbl_log_binomial, tbl_log_poisson),
					tab_spanner = c("**Log Binomial**", "**Poisson**"))
