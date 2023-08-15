usethis::use_git_config(user.name = "Suprithi Pingle", user.email = "suprithi.pingle@gmail.com")
usethis::create_github_token()
gitcreds::gitcreds_set()

library(dplyr)
nlsy <- nlsy |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))

nlsy <- na.omit(nlsy)

setwd("../clean/")
write_rds(nlsy, "nlsy-complete-cases.rds")

library(tidyverse)

# column names
nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")

# read in raw data, replacing missing values with NA
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
				 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
				 skip = 1, col_names = nlsy_cols)

# create factors for categorical variables
nlsy_cats <- nlsy |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))

# check to make sure coding is correct
count(nlsy_cats, eyesight, eyesight_cat)

# remove observations with any missing data
nlsy_cc <- na.omit(nlsy_cats)

# check to make sure it worked
count(nlsy_cc, eyesight_cat)

# create data/clean folder if it doesn't already exist
if (!dir.exists(here::here("data", "clean"))) {
	dir.create(here::here("data", "clean"))
}

# save the complete-case data
write_rds(nlsy_cc, here::here("data", "clean", "nlsy-complete-cases.rds"))


nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")

library(tidyverse)
#not everyone has the same directory so it doesn't work
setwd("~/Documents/Teaching/Emory/epi590r-in-class/data/raw/")
nlsy <- read_csv("nlsy.csv",
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols)

library(tidyverse)
library(gtsummary)

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


# Univariate regression

tbl_uvregression(
	nlsy,
	y = income,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, income, age_bir),
	method = lm)


tbl_uvregression(
	nlsy,
	y = glasses,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	method = glm,
	method.args = list(family = binomial()),
	exponentiate = TRUE)


## Multivariable regressions

## Some regressions

linear_model <- lm(income ~ sex_cat + age_bir + race_eth_cat,
									 data = nlsy)


linear_model_int <- lm(income ~ sex_cat*age_bir + race_eth_cat,
											 data = nlsy)


logistic_model <- glm(glasses ~ eyesight_cat + sex_cat + income,
											data = nlsy, family = binomial())


## Tables

tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))


tbl_regression(
	logistic_model,
	exponentiate = TRUE,
	label = list(
		sex_cat ~ "Sex",
		eyesight_cat ~ "Eyesight",
		income ~ "Income"
	))


tbl_no_int <- tbl_regression(
	linear_model,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth"
	))

tbl_int <- tbl_regression(
	linear_model_int,
	intercept = TRUE,
	label = list(
		sex_cat ~ "Sex",
		race_eth_cat ~ "Race/ethnicity",
		age_bir ~ "Age at first birth",
		`sex_cat:age_bir` ~ "Sex/age interaction"
	))

## Table comparing the models with and without interaction

tbl_merge(list(tbl_no_int, tbl_int),
					tab_spanner = c("**Model 1**", "**Model 2**"))

#exercises
#3
tbl_uvregression(
	nlsy,
	x = sex_cat,
	include = c(nsibs, sleep_wkdy, sleep_wknd, income),
	method = lm)

#4
tbl_regression(
	poisson_model<-glm(nsibs~sex_cat+region+race_eth_cat, data=nlsy, family=poisson())
)

#5
tbl_regression(
	binomialRR<-glm(glasses~sex_cat+eyesight_cat, data=nlsy, family=binomial(link="log"))
)

#6
tbl_regression(
	poissonRR<-glm(glasses~sex_cat+eyesight_cat, data=nlsy, family=poisson(link="log")),
	tidy_fun = partial(tidy_robust, vcov = "HC1")
)

#7
tbl_merge(list(binomialRR, poissonRR),
					tab_spanner = c("**binomial**", "**poisson**"))
library(tidyverse)
library(broom)


library(tidyverse)
library(gtsummary)

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


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")


tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing") |>
	add_p(test = list(all_continuous() ~ "t.test",
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |>
	modify_header(label = "**Variable**", p.value = "**P**")

tbl_summary(
	nlsy,
	by= sex_cat,
	include=c(region_cat,race_eth_cat,income,starts_with("sleep")),
	label=list(
		race_eth_cat~"Race and Ethincity",
		region_cat~"Categorical Region",
		income~"Income",
		sleep_wkdy~"Weekday Sleep",
		sleep_wknd~"Weekend Sleep"),
	statistic=list(
		income~"10th{p10}-90th{p90}",
		starts_with("sleep")~"min={min};max={max}"),
	digits=list(
		income~c(3,3),
		starts_with("sleep")~c(1,1)
	))%>%
	add_p(test=list(all_continuous()~"t.test",
									all_categorical()~"chisq.test"))|>
	)

add_overall(col_label="**Total**")

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")))

mod_sex_cat <- lm(income ~ sex_cat, data = nlsy)
mod_race_eth_cat <- lm(income ~ race_eth_cat, data = nlsy)
mod_eyesight_cat <- lm(income ~ eyesight_cat, data = nlsy)
mod_age_bir <- lm(income ~ age_bir, data = nlsy)

tidy_sex_cat <- tidy(mod_sex_cat, conf.int = TRUE)
tidy_race_eth_cat <- tidy(mod_race_eth_cat, conf.int = TRUE)
tidy_eyesight_cat <- tidy(mod_eyesight_cat, conf.int = TRUE)
tidy_age_bir <- tidy(mod_age_bir, conf.int = TRUE)

bind_rows(
	sex_cat = tidy_sex_cat,
	race_eth_cat = tidy_race_eth_cat,
	eyesight_cat = tidy_eyesight_cat,
	age_bir = tidy_age_bir, .id = "model") |>
	mutate(
		term = str_remove(term, model),
		term = ifelse(term == "", model, term))

tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) |>
	tidycat::tidy_categorical(logistic_model, exponentiate = TRUE) |>
	select(-c(3:5))

tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) |>
	tidycat::tidy_categorical(logistic_model, exponentiate = TRUE) |>
	slice(-1) |> # remove intercept
	ggplot(mapping = aes(x = level, y = estimate,
											 ymin = conf.low, ymax = conf.high)) +
	geom_point() +
	geom_errorbar() +
	facet_grid(cols = vars(variable), scales = "free", space = "free") +
	scale_y_log10()

#exercise
mod_sex_cat <- lm(glasses ~ sex_cat, data = nlsy)
mod_eyesight_cat <- lm(income ~ glasses, data = nlsy)

tidy_sex_cat <- tidy(mod_sex_cat, conf.int = TRUE)
tidy_eyesight_cat <- tidy(mod_eyesight_cat, conf.int = TRUE)

bind_rows(
	sex_cat = tidy_sex_cat,
	eyesight_cat = tidy_eyesight_cat, .id = "model") |>
	mutate(
		term = str_remove(term, model),
		term = ifelse(term == "", model, term))

tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) |>
	tidycat::tidy_categorical(logistic_model, exponentiate = TRUE) |>
	select(-c(3:5))

tidy(logistic_model, conf.int = TRUE, exponentiate = TRUE) |>
	tidycat::tidy_categorical(logistic_model, exponentiate = TRUE) |>
	slice(-1) |> # remove intercept
	ggplot(mapping = aes(x = level, y = estimate,
											 ymin = conf.low, ymax = conf.high)) +
	geom_point() +
	geom_errorbar() +
	facet_grid(cols = vars(variable), scales = "free", space = "free") +
	scale_y_log10()
