library(tidyverse)
library(magrittr)
options(dplyr.width = Inf)
library(lubridate)
library(here)
library(janitor)
library(gtsummary)
library(Hmisc)

source("0-functions.R")

#Demos
data_comb_50k <- read_csv(here("data_filtered_50k.csv"))

key_stats <-data_comb_50k %>%
  select(gender, yearOfBirth, inMarket, new_or_used, income) %>%
  mutate(
    yearOfBirth_date = as.Date(yearOfBirth, "%Y"),
    age = trunc(yearOfBirth_date %--% Sys.Date() / years(1))
  ) %>%
  select(-yearOfBirth, -yearOfBirth_date) %>%
  mutate(
    gender = recode(gender, "male" = "Male", "female" = "Female", "other" = "Other", "prefer_not_say" = "Prefer not to say"),
    inMarket = recode(inMarket, "0-3months" = "0-3 months" , "1year" = "1 year", "no_timeline" = "No timeline", "not_in_market" = "NA"),
    new_or_used = recode(new_or_used, "1" = "New", "2" = "Used / both / not sure", "3" = "Used / both / not sure", "4" = "Used / both / not sure"),
    income = ifelse(((income== "under25") | (income == "inc_25to35") | (income == "inc_35to50")), "< $50k", "> $50k")
  ) %>%
  select(age, gender, inMarket, new_or_used, income) %>%
  rename(
    "Age" = age,
    "Gender Identity" = gender,
    "Timeframe for Purchase" = inMarket,
    "New or Used" = new_or_used,
    "Income" = income
  )

table <- key_stats %>%
  tbl_summary(
    type = Age ~ "continuous",
    digits = starts_with("Age") ~ 0,
    statistic = Age ~ "{min} {max} {mean}",
    missing_text = "(NA)",
    sort = list(everything() ~ "frequency")
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Key Sample Stats**")
table

table <- table %>%
  gtsummary::as_flex_table()
print(table, preview = "docx")


EV_data <-data_comb_50k %>%
  select(knowledgeSub, neighborEV, considerPHEV, considerBEV) %>%
  mutate(
    knowledgeSub = recode(knowledgeSub, "not_sure" = "Not sure"),
    considerPHEV = recode(considerPHEV, "definitelyNot" = "Definitely Not", "probablyNot" = "Probably Not", "maybe" = "Maybe / Not sure", 
                          "probablyYes" = "Probably Yes", "definitelyYes" = "Definitely Yes"),
    considerBEV = recode(considerBEV, "definitelyNot" = "Definitely Not", "probablyNot" = "Probably Not", "maybe" = "Maybe / Not sure", 
                         "probablyYes" = "Probably Yes", "definitelyYes" = "Definitely Yes"),
    neighborEV = recode(neighborEV, "1" = "Yes", "2" = "No", "3" = "Not sure")
  ) %>%
  rename(
    "What is the current maximum subsidy available fromn/ the US federal government for purchasing an electric vehicle?" = knowledgeSub,
    "Would you consider a Plug-in Hybrid Electric Vehicle as your next vehicle?" = considerPHEV,
    "Would you consider a Pure Electric Vehicle as your next vehicle?" = considerBEV,
    "Do any neighbors own / lease a plug-in hybrid or pure electric vehicle?" = neighborEV
  )


table2 <- EV_data %>%
  tbl_summary(
    missing_text = "(NA)",
    sort = list(everything() ~ "alphanumeric")
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Sample - PEV related questions**")
table2

table2 <- table2 %>%
  gtsummary::as_flex_table()
print(table2, preview = "docx")

#Model Table

#load models
load(here::here('models', 'model_comb_wtp_m1_50k.RData'))
load(here::here('models', 'model_comb_wtp_mxl_m1_50k.RData'))

#Subgroups

#load models
load(here::here('models', 'model_comb_inc_high_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_low_m1_50k.RData'))
load(here::here('models', 'model_comb_new_m1_50k.RData'))
load(here::here('models', 'model_comb_used_m1_50k.RData'))
load(here::here('models', 'model_comb_carBudgethigh_m1_50k.RData'))
load(here::here('models', 'model_comb_carBudgetlow_m1_50k.RData'))

#flextable

library(officer)
library(flextable)

summary_sl2 <- make_coef_table2(m1_comb_50k)
summary_mxl2 <- make_coef_table2(mxl_comb_50k) %>% mutate(coefficients = str_remove(coefficients, "_mu"))
summary_high2 <- make_coef_table2(m1_high_50k) 
summary_low2 <- make_coef_table2(m1_low_50k) 
summary_new2 <- make_coef_table2(m1_new_50k) 
summary_used2 <- make_coef_table2(m1_used_50k)
summary_carBud1 <- make_coef_table2(m1_carBudgethigh_50k)
summary_carBud2 <- make_coef_table2(m1_carBudgetlow_50k)

summary2 <- summary_sl2 %>%
  full_join(summary_mxl2, by = "coefficients") %>%
  left_join(summary_high2, by = "coefficients") %>%
  left_join(summary_low2, by = "coefficients") %>%
  left_join(summary_new2, by = "coefficients") %>%
  left_join(summary_used2,by = "coefficients") %>%
  left_join(summary_carBud1, by = "coefficients") %>%
  left_join(summary_carBud2, by = "coefficients") %>%
  mutate(
    coefficients = recode(coefficients, 
                          "type_salesTax" = "Sales Tax", 
                          "type_taxCredit" = "Tax Credit",
                          "type_taxDeduction" = "Tax Deduction",
                          "timing_taxCredit_immediate" = "Tax Credit – Timing Immediate",
                          "timing_rebate_2" = "Rebate – Timing 2 weeks",
                          "timing_rebate_6" = "Rebate – Timing 6 weeks",
                          "source_rebate_oem" = "Rebate – Source OEM",
                          "source_rebate_dealer" = "Rebate – Source Dealer"))


summary1 <- summary2


summary1 <- flextable(summary1)
theme_vanilla(summary1)
summary1 <- add_header_row(
  summary1, values = c("", "Simple Logit", "Mixed Logit", "High Income", "Low Income", "New Buyers", "Used Buyers", "Greater than $30k budget", "Less than $30k budget"),
  colwidths = c(1,3,3,3,3,3,3)
)
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")
