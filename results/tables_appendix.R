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

#load coef table function
source("0-functions.R")


# Car Budget

#load models
load(here::here('models', 'model_comb_carBudgethigh_m1_50k.RData'))
load(here::here('models', 'model_comb_carBudgetlow_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_carBudgethigh_50k)
summary2 <- make_coef_table2(m1_carBudgetlow_50k)

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
  summary1, values = c("", "Greater than $30k budget", "Less than $30k budget"),
  colwidths = c(1,3,3))
summary1<- autofit(summary1)
summary1 <- add_footer_lines(summary1, values = "Signif. codes:  '***' = 0.001, '**' = 0.01, '*' = 0.05, '.' = 0.1, ' ' = 1")
summary1 <- align(summary1, align = "right", part = "body")
print(summary1, preview = "docx")


#Knowledge Sub and consider PEV

#load models
load(here::here('models', 'model_comb_knowSubyes_m1_50k.RData'))
load(here::here('models', 'model_comb_knowSubno_m1_50k.RData'))
load(here::here('models', 'model_comb_yesEV_m1_50k.RData'))
load(here::here('models', 'model_comb_noEV_m1_50k.RData'))
load(here::here('models', 'model_comb_neighborEVyes_m1_50k.RData'))
load(here::here('models', 'model_comb_neighborEVno_m1_50k.RData'))

#make coefficient tables
summary1 <- make_coef_table2(m1_knowSubyes_50k)
summary2 <- make_coef_table2(m1_knowSubno_50k)
summary3 <- make_coef_table2(m1_yesEV_50k) 
summary4 <- make_coef_table2(m1_noEV_50k) 
summary5 <- make_coef_table2(m1_neighborEVyes_50k)
summary6 <- make_coef_table2(m1_neighborEVno_50k)

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
  colwidths = c(1,3,3,3,3,3,3))
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

