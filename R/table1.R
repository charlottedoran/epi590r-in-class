#Load libraries
library(tidyverse)
library(gtsummary)

#Define objects and data
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

#Create table summary
tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(region_cat,
							race_eth_cat,
							starts_with("sleep"),
							income),
	label = list(sex_cat ~ "Sex",
							 region_cat ~ "Region",
							 race_eth_cat ~ "Race/ethnicity",
							 income ~ "Income (10th percentile, 90th percentile)",
							 sleep_wkdy ~ "Weekday sleep duration (min, max hours)",
							 sleep_wknd ~ "Weekend sleep duration (min, max hours)"),
	missing_text = "Missing",
	statistic = list(income ~ "{p10}, {p90}",
									 starts_with("sleep") ~ "{min} - {max}"),
	digits = list(income ~ c(3,3),
								starts_with("sleep") ~ c(1,1))
	) |>
	add_p(all_categorical() ~ "chisq.test",
				all_continuous() ~ "t.test")
