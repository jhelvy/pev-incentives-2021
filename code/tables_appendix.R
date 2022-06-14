library(tidyverse)
library(magrittr)
options(dplyr.width = Inf)
library(lubridate)
library(here)
library(janitor)
library(gtsummary)
library(Hmisc)
library(officer)
library(flextable)

#Appendix

#Demo tables

data_comb_50k <- read_csv(here("data_filtered_50k.csv"))

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

#unadjusted model summary table
#flextable

source("code/0-functions.R")

library(officer)
library(flextable)

load(here::here('models', 'model_comb_wtp_m1_50k.RData'))
load(here::here('models', 'model_comb_wtp_mxl_m1_50k.RData'))

#Subgroups
load(here::here('models', 'model_comb_inc_high_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_low_m1_50k.RData'))
load(here::here('models', 'model_comb_new_m1_50k.RData'))
load(here::here('models', 'model_comb_used_m1_50k.RData'))
load(here::here('models', 'model_comb_carBudgethigh_m1_50k.RData'))
load(here::here('models', 'model_comb_carBudgetlow_m1_50k.RData'))

summary_sl2 <- make_coef_table3(m1_comb_50k)
summary_mxl2 <- make_coef_table3(mxl_comb_50k) #%>% mutate(coefficients = str_remove(coefficients, "_mu"))
summary_high2 <- make_coef_table3(m1_high_50k) 
summary_low2 <- make_coef_table3(m1_low_50k) 
summary_new2 <- make_coef_table3(m1_new_50k) 
summary_used2 <- make_coef_table3(m1_used_50k)
summary_more30k <- make_coef_table3(m1_carBudgethigh_50k)
summary_less30k <- make_coef_table3(m1_carBudgetlow_50k)


summary1 <- summary_sl2 %>%
  full_join(summary_mxl2, by = "coefficients") %>%
  left_join(summary_high2, by = "coefficients") %>%
  left_join(summary_low2, by = "coefficients") %>%
  left_join(summary_new2, by = "coefficients") %>%
  left_join(summary_used2,by = "coefficients") %>%
  left_join(summary_more30k,by = "coefficients") %>%
  left_join(summary_less30k,by = "coefficients") 


summary1 <- flextable(summary1)
theme_vanilla(summary1)
# summary1 <- set_header_labels(summary1,
#                               values = list(
#                                 
#                               ))
summary1 <- add_header_row(
  summary1, values = c("", "Simple Logit", "Mixed Logit", "High Income", "Low Income", "New Car Buyers", "Used Car Buyers", "Budget >$30k", "Budget <$30k"),
  colwidths = c(1,1,1,1,1,1,1,1,1)
)
#summary1 <- add_header_lines( summary1, values = c("Simple Logit Model"))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
# summary1 <- align(summary1, j= c("(Error)"), align = "center", part = "all")
# summary1 <- align(summary1, i = c("Simple Logit Model"), align = "center")
print(summary1, preview = "docx")

#table of income brackets
#flextable

source("code/0-functions.R")

library(officer)
library(flextable)

load(here::here('models', 'model_comb_inc_lessthan25_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_25-35_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_35-50_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_50-75_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_75-100_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_100-150_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_150-200_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_200-250_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_250-300_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_300-400_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_400plus_m1_50k.RData'))

summary_1 <- make_coef_table3(m1_inclessthan25_50k)
summary_2 <- make_coef_table3(m1_inc25_35_50k)
summary_3 <- make_coef_table3(m1_inc35_50_50k) 
summary_4 <- make_coef_table3(m1_inc50_75_50k) 
summary_5 <- make_coef_table3(m1_inc75_100_50k) 
summary_6 <- make_coef_table3(m1_inc100_150_50k)
summary_7 <- make_coef_table3(m1_inc150_200_50k)
summary_8 <- make_coef_table3(m1_inc200_250_50k)
summary_9 <- make_coef_table3(m1_inc250_300_50k)
summary_10 <- make_coef_table3(m1_inc300_400_50k)
summary_11 <- make_coef_table3(m1_inc400plus_50k)


summary1 <- summary_1 %>%
  left_join(summary_2, by = "coefficients") %>%
  left_join(summary_3, by = "coefficients") %>%
  left_join(summary_4, by = "coefficients") %>%
  left_join(summary_5,by = "coefficients") %>%
  left_join(summary_6,by = "coefficients") %>%
  left_join(summary_7,by = "coefficients") %>%
  left_join(summary_8,by = "coefficients") %>%
  left_join(summary_9,by = "coefficients") %>%
  left_join(summary_10,by = "coefficients") %>%
  left_join(summary_11,by = "coefficients") 


summary1 <- flextable(summary1)
theme_vanilla(summary1)
# summary1 <- set_header_labels(summary1,
#                               values = list(
#                                 
#                               ))
summary1 <- add_header_row(
  summary1, values = c("", "<25", "25-35", "35-50", "50-75", "75-100", "100-150", "150-200", "200-250" , "250-300", "300-400", "400+"),
  colwidths = c(1,1,1,1,1,1,1,1,1,1,1,1)
)
#summary1 <- add_header_lines( summary1, values = c("Simple Logit Model"))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
# summary1 <- align(summary1, j= c("(Error)"), align = "center", part = "all")
# summary1 <- align(summary1, i = c("Simple Logit Model"), align = "center")
print(summary1, preview = "docx")

#load coef table function
source("0-functions.R")


#Knowledge Sub and consider PEV

#load models
load(here::here('models', 'model_comb_knowSubyes_m1_50k.RData'))
load(here::here('models', 'model_comb_knowSubno_m1_50k.RData'))
load(here::here('models', 'model_comb_yesEV_m1_50k.RData'))
load(here::here('models', 'model_comb_noEV_m1_50k.RData'))
load(here::here('models', 'model_comb_neighborEVyes_m1_50k.RData'))
load(here::here('models', 'model_comb_neighborEVno_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table3(m1_knowSubyes_50k)
summary2 <- make_coef_table3(m1_knowSubno_50k)
summary3 <- make_coef_table3(m1_yesEV_50k) 
summary4 <- make_coef_table3(m1_noEV_50k) 
summary5 <- make_coef_table3(m1_neighborEVyes_50k)
summary6 <- make_coef_table3(m1_neighborEVno_50k)

#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
  left_join(summary4, by = "coefficients") %>%
  left_join(summary5, by = "coefficients") %>%
  left_join(summary6, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Knowledge Subsidy Yes", "Knowledge Subsidy No", "Consider PEV Yes", "Consider PEV No", "Neighbor PEV Yes", "Neighbor PEV No"),
  colwidths = c(1,1,1,1,1,1,1))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Dedicated parking

#load models

load(here::here('models', 'model_comb_homepark_m1_50k.RData'))
load(here::here('models', 'model_comb_homeparkNo_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_homepark_50k) 
summary2 <- make_coef_table2(m1_homeparkNo_50k) 

#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Dedicated Parking for charging", "No dedicated parking for charging"),
  colwidths = c(1,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Rest of knowledge

#load models
load(here::here('models', 'model_comb_knowFuelGasyes_m1_50k.RData'))
load(here::here('models', 'model_comb_knowFuelGasno_m1_50k.RData'))
load(here::here('models', 'model_comb_knowFuelElecyes_m1_50k.RData'))
load(here::here('models', 'model_comb_knowFuelElecno_m1_50k.RData'))


#make coefficient tables
summary1 <- make_coef_table2(m1_knowFuelGasyes_50k)
summary2 <- make_coef_table2(m1_knowFuelGasno_50k)
summary3 <- make_coef_table2(m1_knowFuelElecyes_50k) 
summary4 <- make_coef_table2(m1_knowFuelElecno_50k) 

#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
  left_join(summary4, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Knowledge Gas Yes", "Knowledge Gas No", "Knowledge Electric Yes", "Knowledge Electric No"),
  colwidths = c(1,3,3,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#purchase choices - lease/purchase

#load models
load(here::here('models', 'model_comb_purchase_m1_50k.RData'))
load(here::here('models', 'model_comb_lease_m1_50k.RData'))


#make coefficient tables
summary1 <- make_coef_table2(m1_purchase_50k) 
summary2 <- make_coef_table2(m1_lease_50k) 


#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Purchase", "Lease"),
  colwidths = c(1,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#cartype shopping

#load models
load(here::here('models', 'model_comb_hatch_m1_50k.RData'))
load(here::here('models', 'model_comb_sedan_m1_50k.RData'))
load(here::here('models', 'model_comb_SUV_m1_50k.RData'))
load(here::here('models', 'model_comb_truck_m1_50k.RData'))
load(here::here('models', 'model_comb_van_m1_50k.RData'))


#make coefficient tables
summary1 <- make_coef_table2(m1_hatch_50k)
summary2 <- make_coef_table2(m1_sedan_50k)
summary3 <- make_coef_table2(m1_SUV_50k) 
summary4 <- make_coef_table2(m1_truck_50k) 
summary5 <- make_coef_table2(m1_van_50k)


#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
  left_join(summary4, by = "coefficients") %>%
  left_join(summary5, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Hatch", "Sedan", "SUV", "Truck", "Van"),
  colwidths = c(1,3,3,3,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#HH vehicles and type

#load models
load(here::here('models', 'model_comb_lessthan2veh_m1_50k.RData'))
load(here::here('models', 'model_comb_morethan2veh_m1_50k.RData'))
load(here::here('models', 'model_comb_currentVehElec_m1_50k.RData'))
load(here::here('models', 'model_comb_currentVehGas_m1_50k.RData'))


#make coefficient tables
summary1 <- make_coef_table2(m1_lessthan2veh_50k)
summary2 <- make_coef_table2(m1_morethan2veh_50k)
summary3 <- make_coef_table2(m1_currentVehElec_50k) 
summary4 <- make_coef_table2(m1_currentVehGas_50k) 


#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
  left_join(summary4, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "0 or 1 household veh", "2 or more household veh", "Current Veh Electrified", "Current Veh Gas"),
  colwidths = c(1,3,3,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Gender

#load models
load(here::here('models', 'model_comb_male_m1_50k.RData'))
load(here::here('models', 'model_comb_female_m1_50k.RData'))
load(here::here('models', 'model_comb_nonbinary_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_male_50k)
summary2 <- make_coef_table2(m1_female_50k)
summary3 <- make_coef_table2(m1_nonbinary_50k) 


#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Male", "Female", "Non-binary"),
  colwidths = c(1,3,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Ethnicity

#load models
load(here::here('models', 'model_comb_hispanic_m1_50k.RData'))
load(here::here('models', 'model_comb_asian_m1_50k.RData'))
load(here::here('models', 'model_comb_black_m1_50k.RData'))
load(here::here('models', 'model_comb_native_m1_50k.RData'))
load(here::here('models', 'model_comb_pacific_m1_50k.RData'))
load(here::here('models', 'model_comb_white_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_asian_50k)
summary2 <- make_coef_table2(m1_black_50k)
summary3 <- make_coef_table2(m1_hispanic_50k) 
summary4 <- make_coef_table2(m1_native_50k) 
summary5 <- make_coef_table2(m1_pacific_50k) 
summary6 <- make_coef_table2(m1_white_50k) 

#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
  left_join(summary4, by = "coefficients") %>%
  left_join(summary5, by = "coefficients") %>%
  left_join(summary6, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Asian", "Black", "Hispanic", "Native", "Pacific Islander", "White"),
  colwidths = c(1,3,3,3,3,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Education 1 - no HS through associates

#load models
load(here::here('models', 'model_comb_no_hs_m1_50k.RData'))
load(here::here('models', 'model_comb_hs_m1_50k.RData'))
load(here::here('models', 'model_comb_some_college_m1_50k.RData'))
load(here::here('models', 'model_comb_vocational_m1_50k.RData'))
load(here::here('models', 'model_comb_associates_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_no_hs_50k)
summary2 <- make_coef_table2(m1_hs_50k)
summary3 <- make_coef_table2(m1_some_college_50k) 
summary4 <- make_coef_table2(m1_vocational_50k) 
summary5 <- make_coef_table2(m1_associates_50k) 

#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
  left_join(summary4, by = "coefficients") %>%
  left_join(summary5, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "No High School", "Some High School", "Some College", "Vocational", "Associates"),
  colwidths = c(1,3,3,3,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Education 2 - BS through PhD

#load models
load(here::here('models', 'model_comb_bs_m1_50k.RData'))
load(here::here('models', 'model_comb_ms_m1_50k.RData'))
load(here::here('models', 'model_comb_md_m1_50k.RData'))
load(here::here('models', 'model_comb_phd_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_bs_50k)
summary2 <- make_coef_table2(m1_ms_50k)
summary3 <- make_coef_table2(m1_md_50k) 
summary4 <- make_coef_table2(m1_phd_50k) 

#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
  left_join(summary4, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "BS", "MS", "MD", "Doctorate"),
  colwidths = c(1,3,3,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Work

#load models
load(here::here('models', 'model_comb_student_m1_50k.RData'))
load(here::here('models', 'model_comb_parttime_m1_50k.RData'))
load(here::here('models', 'model_comb_fulltime_m1_50k.RData'))
load(here::here('models', 'model_comb_not_employed_looking_m1_50k.RData'))
load(here::here('models', 'model_comb_not_employed_not_looking_m1_50k.RData'))
load(here::here('models', 'model_comb_retired_m1_50k.RData'))
load(here::here('models', 'model_comb_disabled_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_student_50k)
summary2 <- make_coef_table2(m1_parttime_50k)
summary3 <- make_coef_table2(m1_fulltime_50k) 
summary4 <- make_coef_table2(m1_not_employed_looking_50k) 
summary5 <- make_coef_table2(m1_not_employed_not_looking_50k)
summary6 <- make_coef_table2(m1_retired_50k)
summary7 <- make_coef_table2(m1_disabled_50k)

#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
  left_join(summary4, by = "coefficients") %>%
  left_join(summary5, by = "coefficients") %>%
  left_join(summary6, by = "coefficients") %>%
  left_join(summary7, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Student", "Less than 40 hrs", "40 hrs or more", "Not Employed, Looking", "Not Employed, Not", "Retired", "Disabled"),
  colwidths = c(1,3,3,3,3,3,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Housing Type

#load models
load(here::here('models', 'model_comb_mobile_home_m1_50k.RData'))
load(here::here('models', 'model_comb_apartment_m1_50k.RData'))
load(here::here('models', 'model_comb_townhome_m1_50k.RData'))
load(here::here('models', 'model_comb_condo_m1_50k.RData'))
load(here::here('models', 'model_comb_SFH_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_mobile_home_50k)
summary2 <- make_coef_table2(m1_apartment_50k)
summary3 <- make_coef_table2(m1_townhome_50k) 
summary4 <- make_coef_table2(m1_condo_50k) 
summary5 <- make_coef_table2(m1_SFH_50k)


#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
  left_join(summary4, by = "coefficients") %>%
  left_join(summary5, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Mobile Home", "Apartment", "Townhome", "Condo", "Single Family Home"),
  colwidths = c(1,3,3,3,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Housing Ownership

#load models
load(here::here('models', 'model_comb_rent_m1_50k.RData'))
load(here::here('models', 'model_comb_own_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_rent_50k)
summary2 <- make_coef_table2(m1_own_50k)

#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Rent", "Own"),
  colwidths = c(1,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")



#Household size

#load models
load(here::here('models', 'model_comb_HHone_m1_50k.RData'))
load(here::here('models', 'model_comb_HHtwo_m1_50k.RData'))
load(here::here('models', 'model_comb_HHthree_m1_50k.RData'))
load(here::here('models', 'model_comb_HHfour_m1_50k.RData'))
load(here::here('models', 'model_comb_HHfive_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_HHone_50k)
summary2 <- make_coef_table2(m1_HHtwo_50k)
summary3 <- make_coef_table2(m1_HHthree_50k) 
summary4 <- make_coef_table2(m1_HHfour_50k) 
summary5 <- make_coef_table2(m1_HHfive_50k)


#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
  left_join(summary4, by = "coefficients") %>%
  left_join(summary5, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "HH of 1", "HH of 2", "HH of 3", "HH of 4", "HH 5 or more"),
  colwidths = c(1,3,3,3,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Rural/non-rural

#load models
load(here::here('models', 'model_comb_rural_m1_50k.RData'))
load(here::here('models', 'model_comb_nonrural_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_rural_50k)
summary2 <- make_coef_table2(m1_nonrural_50k)

#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Rural", "Non-Rural"),
  colwidths = c(1,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Political

#load models
load(here::here('models', 'model_comb_conservative_m1_50k.RData'))
load(here::here('models', 'model_comb_liberal_m1_50k.RData'))
load(here::here('models', 'model_comb_moderate_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_conservative_50k)
summary2 <- make_coef_table2(m1_liberal_50k)
summary3 <- make_coef_table2(m1_moderate_50k) 


#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Conservative", "Liberal", "Moderate"),
  colwidths = c(1,3,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Generations (age)

#load models
load(here::here('models', 'model_comb_gen_silent_m1_50k.RData'))
load(here::here('models', 'model_comb_gen_boomer_m1_50k.RData'))
load(here::here('models', 'model_comb_gen_x_m1_50k.RData'))
load(here::here('models', 'model_comb_gen_mill_m1_50k.RData'))
load(here::here('models', 'model_comb_gen_z_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_gen_silent_50k)
summary2 <- make_coef_table2(m1_gen_boomer_50k)
summary3 <- make_coef_table2(m1_gen_x_50k) 
summary4 <- make_coef_table2(m1_gen_mill_50k) 
summary5 <- make_coef_table2(m1_gen_z_50k)


#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
  left_join(summary3, by = "coefficients") %>%
  left_join(summary4, by = "coefficients") %>%
  left_join(summary5, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "Silent", "Boomer", "Gen X", "Millennial", "Gen Z"),
  colwidths = c(1,3,3,3,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")

#Rural/non-rural

#load models
load(here::here('models', 'model_comb_COVIDyes_m1_50k.RData'))
load(here::here('models', 'model_comb_COVIDno_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_COVIDyes_50k)
summary2 <- make_coef_table2(m1_COVIDno_50k)

#join and rename rows
summary_final <- summary1 %>%
  left_join(summary2, by = "coefficients") %>%
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

#FLEXTABLE
summary1 <- flextable(summary_final)
theme_vanilla(summary1)
#rename header rows based on models
summary1 <- add_header_row(
  summary1, values = c("", "COVID yes", "COVID no"),
  colwidths = c(1,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")

