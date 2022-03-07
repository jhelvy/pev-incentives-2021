#prep choice data for modeling and baseline models

source("1-load_data.R")

library(data.table)
library(cowplot)
library(logitr)
library(mlogit)
library(fastDummies)
library(gtsummary)
options(scipen=999)


#read in choice data from combined model +50k
choiceData_comb_50k <- read_csv(here("choiceData_comb_50k.csv"))

#add data for model
choiceData_comb_50k <- choiceData_comb_50k %>%
  # Fix amount and add 0s to attribute-specific levels
  fastDummies::dummy_cols(c(
    'type', 'timing_taxCredit', 'timing_rebate', 'source_rebate')) %>%
  # Add 0s to type-specific dummy-coded variables
  mutate(
    timing_taxCredit_immediate = type_taxCredit*timing_taxCredit_immediate,
    timing_taxCredit_tax_filing = type_taxCredit*timing_taxCredit_tax_filing,
    timing_rebate_0 = type_rebate*timing_rebate_0,
    timing_rebate_2 = type_rebate*timing_rebate_2,
    timing_rebate_6 = type_rebate*timing_rebate_6,
    source_rebate_dealer = type_rebate*source_rebate_dealer,
    source_rebate_government = type_rebate*source_rebate_government,
    source_rebate_oem = type_rebate*source_rebate_oem)

choiceData_comb_50k$amount <- -1*choiceData_comb_50k$amount 

# Model 1 - baselines:
# Type: Rebate
# Tax Credit Timing: Tax Filling
# Rebate Timing: 0 Weeks 
# Rebate Source: Government

m1_comb_50k <- logitr::logitr(
  data = choiceData_comb_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)
summary(m1_comb_50k)


# Save results
save(m1_comb_50k, file = here::here('models', 'model_comb_wtp_m1_50k.RData'))


#Estimate MXL model--------------------------------------------------------


#Estimate the model
mxl_comb_50k <- logitr::logitr(
  data   = choiceData_comb_50k,
  outcome = 'choice',
  obsID  = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer'),
  randPars = c(type_salesTax = 'n', type_taxCredit = 'n', type_taxDeduction = 'n', 
               timing_taxCredit_immediate = 'n',
               timing_rebate_2 = 'n', timing_rebate_6 = 'n',
               source_rebate_oem = 'n', source_rebate_dealer = 'n')
)

summary(mxl_comb_50k)
save(mxl_comb_50k, file = here::here('models', 'model_comb_wtp_mxl_m1_50k.RData'))

