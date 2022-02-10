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


carType_data <- data_comb_50k %>%
  mutate(
    hatch = ifelse(str_detect(carType, "hatch"),1,0),
    sedan = ifelse(str_detect(carType, "sedan"),1,0),
    suv = ifelse(str_detect(carType, "suv"),1,0),
    truck = ifelse(str_detect(carType, "truck"),1,0),
    van = ifelse(str_detect(carType, "van"),1,0)) %>%
  select(hatch, sedan, suv, truck, van) %>%
  colSums(na.rm = TRUE)

survey_data <- data_comb_50k %>%
  select(inMarket, new_or_used, lease_or_purchase, carType, carBudget) %>%
  mutate(
    carType_suv = ifelse(str_detect(carType, "suv") == 1, 1,0)) %>%
  mutate(
    inMarket = recode(inMarket, "0-3months" = "0-3 months" , "1year" = "1 year", "no_timeline" = "No timeline", "not_in_market" = "NA"),
    new_or_used = recode(new_or_used, "1" = "New", "2" = "Used", "3" = "Both", "4" = "Not sure"),
    lease_or_purchase = recode(lease_or_purchase, "1" = "Lease", "2" = "Purchase", "3" = "Not sure")
  ) %>%
  rename(
    "New or Used" = new_or_used,
    "Lease or Purchase" = lease_or_purchase,
    "Car Type Shopping" = carType,
    "Car Budget" = carBudget,
    "Timeframe for Purchase" = inMarket
    )

table1 <- survey_data %>%
  tbl_summary(
    missing_text = "(NA)",
    sort = list(everything() ~ "alphanumeric")
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Sample - Vehicle Purchase questions**")
table1

table1 <- table1 %>%
  gtsummary::as_flex_table()
print(table1, preview = "docx")


EV_data <-data_comb_50k %>%
  select(knowledgeFuelGas, knowledgeFuelElec, knowledgeSub, neighborEV, householdVehicles, currentVehType, considerPHEV, considerBEV) %>%
  mutate(
    knowledgeFuelGas = toupper(knowledgeFuelGas),
    knowledgeFuelElec = toupper(knowledgeFuelElec),
    knowledgeSub = recode(knowledgeSub, "not_sure" = "Not sure"),
    currentVehType = str_replace_all(currentVehType, c("Gasoline" = "Gas", "Electric" = "BEV", "Plug-In Hybrid" = "PHEV", "Hybrid" = "HEV")),
    considerPHEV = recode(considerPHEV, "definitelyNot" = "Definitely Not", "probablyNot" = "Probably Not", "maybe" = "Maybe / Not sure", 
                          "probablyYes" = "Probably Yes", "definitelyYes" = "Definitely Yes"),
    considerBEV = recode(considerBEV, "definitelyNot" = "Definitely Not", "probablyNot" = "Probably Not", "maybe" = "Maybe / Not sure", 
                          "probablyYes" = "Probably Yes", "definitelyYes" = "Definitely Yes"),
    neighborEV = recode(neighborEV, "1" = "Yes", "2" = "No", "3" = "Not sure")
  ) %>%
  rename(
     "Please select which vehicle(s) can run on gasoline:" = knowledgeFuelGas,
     "Please select which vehicle(s) can be plugged-in:" = knowledgeFuelElec,
     "What is the current maximum subsidy available fromn/ the US federal government for purchasing an electric vehicle?" = knowledgeSub,
     "Household Vehicles" = householdVehicles,
     "Household Vehicle Types (ie Gas, HEV, PHEV, BEV)" = currentVehType,
     "Would you consider a Plug-in Hybrid Electric Vehicle as your next vehicle?" = considerPHEV,
     "Would you consider a Pure Electric Vehicle as your next vehicle?" = considerBEV,
     "Do any neighbors own / lease a plug-in hybrid or pure electric vehicle?" = neighborEV
  )


currentveh_data <-data_comb_50k %>%
  select( currentVehType) %>%
  mutate(
    currentVehType = str_replace_all(currentVehType, c("Gasoline" = "Gas", 
                                                       "Electric" = "BEV", 
                                                       "Plug-In Hybrid" = "PHE", 
                                                       "Hybrid" = "HEV"))) %>%
  mutate(
    BEV = ifelse(str_detect(currentVehType, "BEV"),1,0),
    PHEV = ifelse(str_detect(currentVehType, "PHE"),1,0),
    HEV = ifelse(str_detect(currentVehType, "HEV"),1,0),
    Gas= ifelse(str_detect(currentVehType, "Gas"),1,0))  %>%
  select(BEV, PHEV, HEV, Gas) %>%
  colSums(na.rm = TRUE)

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

demos <- data_comb_50k %>%
  select(gender, income, education, work, housingType, housingOwner, rural, political,covidEmployment)%>%
  mutate(
    gender = recode(gender, "male" = "Male", "female" = "Female", "other" = "Other", "prefer_not_say" = "Prefer not to say"),
    rural = ifelse(rural == 0, "Rural", "Urban/suburban"),
    income = ifelse(((income== "under25") | (income == "inc_25to35") | (income == "inc_35to50")), "< $50k", "> $50k"),
    education = recode(education, "no_hs" = "Less than a high school diploma", 
                       "hs" = "High school degree or equivalent (e.g. GED)",
                       "college_some" = "Some college or university, no college degree", 
                       "vocational" = "Trade/technical/vocational training, no degree awarded",
                       "degree_associate" = "Associate degree (e.g. AA, AS)",
                       "degree_bs" = "Bachelor’s degree (e.g. BA, BS)",
                       "degree_ms" = "Master's degree (e.g. MA, MS, MEd)",
                       "degree_md" = "Professional degree (e.g. MD, DDS, DVM)",
                       "degree_phd" = "Doctorate (e.g. PhD, EdD)",
                       "prefer_not_say" = "Prefer not to say"),
    work = recode(work, "student" = "Full time student",
                  "employed_under40" = "Employed, working 1-39 hours per week",
                  "employed_over40" = "Employed, working 40 or more hours per week",
                  "not_employed_yes_looking" = "Not employed, looking for work",
                  "not_employed_not_looking" = "Not employed, NOT looking for work",
                  "retired" = "Retired",
                  "disabled" = "Disabled, not able to work",
                  "prefer_not_say" = "Prefer not to say"),
    housingType = recode(housingType, "prefer_not_say" = "Prefer not to say"),
    housingType = capitalize(housingType),
    housingOwner = recode(housingOwner, "prefer_not_say" = "Prefer not to say"),
    housingOwner = capitalize(housingOwner),
    political = recode(political, "very_conservative" = "Very Conservative", "very_liberal" = "Very Liberal", "prefer_not_say" = "Prefer not to say"),
    political = capitalize(political),
    covidEmployment = recode(covidEmployment, "prefer_not_say" = "Prefer not to say"),
    covidEmployment = capitalize(covidEmployment)
  ) %>%
  rename(
    "Gender Identity" = gender,
    "Income" = income,
    "Highest Degree of Education" = education,
    "Current Employment Status" = work,
    "Current Housing" = housingType,
    "Ownership of Housing" = housingOwner,
    "Geographical Location" = rural,
    "Political Views" = political,
    "Did COVID-19 impact employment status?" = covidEmployment
  )

table3 <- demos %>%
  tbl_summary(
    missing_text = "(NA)",
    sort = list(everything() ~ "alphanumeric")
  ) %>%
  modify_header(label ~ "**Sample - Demographics**") %>%
  bold_labels()
table3

table3 <- tbl_summary(demos) %>%
  gtsummary::as_flex_table()
print(table3, preview = "docx")

#Testing tables

#load models
load(here::here('models', 'model_comb_wtp_m1_50k.RData'))
load(here::here('models', 'model_comb_wtp_mxl_m1_50k.RData'))

# #save csv - Simple Logit
# 
# summary_sl <- make_coef_table(m1_comb_50k)
# write_csv(summary_sl, here::here("tables","simple_logit.csv"))
# 
# #save csv - Mixed Logit
# 
# summary_mxl <- make_coef_table(mxl_comb_50k)
# write_csv(summary_mxl, here::here("tables","mixed_logit.csv"))

#Subgroups

#load models
load(here::here('models', 'model_comb_inc_high_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_low_m1_50k.RData'))
load(here::here('models', 'model_comb_new_m1_50k.RData'))
load(here::here('models', 'model_comb_used_m1_50k.RData'))

#save csv - income/new used subgroups
summary_high <- make_coef_table(m1_high_50k) %>% rename("Model - High Income" = "table_format2")
write_csv(summary_high, here::here("tables","model_inc_high.csv"))

summary_low <- make_coef_table(m1_low_50k) %>% rename("Model - Low Income" = "table_format2")
write_csv(summary_low, here::here("tables","model_inc_low.csv"))

summary_new <- make_coef_table(m1_new_50k) %>% rename("Model - New Car Shopper" = "table_format2")
write_csv(summary_new, here::here("tables","model_new.csv"))

summary_used <- make_coef_table(m1_used_50k) %>% rename("Model - Used Car Shopper" = "table_format2")
write_csv(summary_used, here::here("tables","model_used.csv"))

summary_subgroup1 <- summary_high %>%
  left_join(summary_low, by = "coefficients") %>%
  left_join(summary_new, by = "coefficients") %>%
  left_join(summary_used,by = "coefficients")
write_csv(summary_subgroup1, here::here("tables","model_subgroup1.csv"))

#flextable

library(officer)
library(flextable)

summary_sl2 <- make_coef_table2(m1_comb_50k)
summary_mxl2 <- make_coef_table2(mxl_comb_50k) %>% mutate(coefficients = str_remove(coefficients, "_mu"))
summary_high2 <- make_coef_table2(m1_high_50k) 
summary_low2 <- make_coef_table2(m1_low_50k) 
summary_new2 <- make_coef_table2(m1_new_50k) 
summary_used2 <- make_coef_table2(m1_used_50k)

summary2 <- summary_sl2 %>%
  full_join(summary_mxl2, by = "coefficients") %>%
  left_join(summary_high2, by = "coefficients") %>%
  left_join(summary_low2, by = "coefficients") %>%
  left_join(summary_new2, by = "coefficients") %>%
  left_join(summary_used2,by = "coefficients") 
 #%>% rename_with(~ gsub('[[:punct:]].*', '', .x))


# summary_subgroup2 <- summary_high2 %>%
#   left_join(summary_low2, by = "coefficients") %>%
#   left_join(summary_new2, by = "coefficients") %>%
#   left_join(summary_used2,by = "coefficients")

summary1 <- summary2


summary1 <- flextable(summary1)
theme_vanilla(summary1)
# summary1 <- set_header_labels(summary1,
#                               values = list(
#                                 
#                               ))
summary1 <- add_header_row(
  summary1, values = c("", "Simple Logit", "Mixed Logit", "High Income", "Low Income", "New Buyers", "Used Buyers"),
  colwidths = c(1,3,3,3,3,3,3)
)
#summary1 <- add_header_lines( summary1, values = c("Simple Logit Model"))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
# summary1 <- align(summary1, j= c("(Error)"), align = "center", part = "all")
# summary1 <- align(summary1, i = c("Simple Logit Model"), align = "center")
print(summary1, preview = "docx")
