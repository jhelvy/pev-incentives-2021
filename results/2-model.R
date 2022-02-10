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

# Model 2 - baselines:
# Type: salesTax 
# Tax Credit Timing: Tax Filling
# Rebate Timing: 6 Weeks 
# Rebate Source: Dealership


m2_comb_50k <- logitr::logitr(
  data = choiceData_comb_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  pars = c(
    'type_taxCredit', 'type_taxDeduction', 'type_rebate',
    'timing_taxCredit_immediate',
    'timing_rebate_0', 'timing_rebate_2',
    'source_rebate_oem', 'source_rebate_government')
)

summary(m2_comb_50k)

# Save results
save(m1_comb_50k, file = here::here('models', 'model_comb_wtp_m1_50k.RData'))
save(m2_comb_50k, file = here::here('models', 'model_comb_wtp_m2_50k.RData'))

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

# #with only softLaunch1 data-----------------
# choiceData <- read_csv(here("choiceData.csv"))
# choiceData_sf1 <- read_csv(here("softLaunch1", "choiceData.csv"))
# 
# #bind rows to softLaunch2 choiceData
# choiceData_comb <- choiceData %>%
#   bind_rows(choiceData_sf1)
# 
# #renumber obsID
# choiceData_comb$obsID = rep(seq(nrow(choiceData_comb) / 4), each = 4)
# 
# choiceData_comb <- choiceData_comb %>%
#   mutate(
#     maxSalestax = as.numeric(maxSalestax),
#     amount_salesTax = (round((amount_salesTax * maxSalestax), -2))) %>%
#   select(-maxSalestax) %>%
#   mutate(
#     amount = ifelse(
#       type == "salesTax", as.numeric(amount_salesTax),
#       as.numeric(as.character(amount_nonSalesTax)))) %>%
#   select(-starts_with("amount_"))
# 
# # Save combined model formatted data
# write_csv(choiceData_comb, here::here('choiceData_comb.csv'))
# 
# 
# #add data for model
# choiceData_comb <- choiceData_comb %>%
#   # Fix amount and add 0s to attribute-specific levels
#   fastDummies::dummy_cols(c(
#     'type', 'timing_taxCredit', 'timing_rebate', 'source_rebate')) %>%
#   # Add 0s to type-specific dummy-coded variables
#   mutate(
#     timing_taxCredit_immediate = type_taxCredit*timing_taxCredit_immediate,
#     timing_taxCredit_tax_filing = type_taxCredit*timing_taxCredit_tax_filing,
#     timing_rebate_0 = type_rebate*timing_rebate_0,
#     timing_rebate_2 = type_rebate*timing_rebate_2,
#     timing_rebate_6 = type_rebate*timing_rebate_6,
#     source_rebate_dealer = type_rebate*source_rebate_dealer,
#     source_rebate_government = type_rebate*source_rebate_government,
#     source_rebate_oem = type_rebate*source_rebate_oem)
# 
# choiceData_comb$amount <- -1*choiceData_comb$amount
# 
# # Model 1 - baselines:
# # Type: Rebate
# # Tax Credit Timing: Tax Filling
# # Rebate Timing: 0 Weeks
# # Rebate Source: Government
# 
# m1_comb <- logitr::logitr(
#   data = choiceData_comb,
#   choice = 'choice',
#   obsID = 'obsID',
#   price = 'amount',
#   modelSpace = 'wtp',
#   pars = c(
#     'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
#     'timing_taxCredit_immediate',
#     'timing_rebate_2', 'timing_rebate_6',
#     'source_rebate_oem', 'source_rebate_dealer')
# )
# summary(m1_comb)
# 
# # Model 2 - baselines:
# # Type: salesTax
# # Tax Credit Timing: Tax Filling
# # Rebate Timing: 6 Weeks
# # Rebate Source: Dealership
# 
# 
# m2_comb <- logitr::logitr(
#   data = choiceData_comb,
#   choice = 'choice',
#   obsID = 'obsID',
#   price = 'amount',
#   modelSpace = 'wtp',
#   pars = c(
#     'type_taxCredit', 'type_taxDeduction', 'type_rebate',
#     'timing_taxCredit_immediate',
#     'timing_rebate_0', 'timing_rebate_2',
#     'source_rebate_oem', 'source_rebate_government')
# )
# 
# summary(m2_comb)
# 
# # Save results
# save(m1_comb, file = here::here('models', 'model_comb_wtp_m1.RData'))
# save(m2_comb, file = here::here('models', 'model_comb_wtp_m2.RData'))


# #SF2 + main sample------------------------------------------
# #format choice data for model
# 
# choiceData <- read_csv(here("choiceData.csv"))
# 
# choiceData <- choiceData %>%
#   mutate(
#     maxSalestax = as.numeric(maxSalestax),
#     amount_salesTax = (round((amount_salesTax * maxSalestax), -2))) %>%
#   select(-maxSalestax)
# 
# choiceData <- choiceData %>%
#   mutate(
#     amount = ifelse(
#       type == "salesTax", as.numeric(amount_salesTax),
#       as.numeric(as.character(amount_nonSalesTax)))) %>%
#   select(-starts_with("amount_")) %>%
#   # Fix amount and add 0s to attribute-specific levels
#   fastDummies::dummy_cols(c(
#     'type', 'timing_taxCredit', 'timing_rebate', 'source_rebate')) %>%
#   # Add 0s to type-specific dummy-coded variables
#   mutate(
#     timing_taxCredit_immediate = type_taxCredit*timing_taxCredit_immediate,
#     timing_taxCredit_tax_filing = type_taxCredit*timing_taxCredit_tax_filing,
#     timing_rebate_0 = type_rebate*timing_rebate_0,
#     timing_rebate_2 = type_rebate*timing_rebate_2,
#     timing_rebate_6 = type_rebate*timing_rebate_6,
#     source_rebate_dealer = type_rebate*source_rebate_dealer,
#     source_rebate_government = type_rebate*source_rebate_government,
#     source_rebate_oem = type_rebate*source_rebate_oem)
# 
# 
# # Compare different encodings:
# 
# choiceData$amount <- -1*choiceData$amount
# 
# # Model 1 - baselines:
# # Type: Rebate
# # Tax Credit Timing: Tax Filling
# # Rebate Timing: 0 Weeks 
# # Rebate Source: Government
# 
# m1 <- logitr::logitr(
#   data = choiceData, 
#   choice = 'choice',
#   obsID = 'obsID',
#   price = 'amount',
#   modelSpace = 'wtp',
#   pars = c(
#     'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
#     'timing_taxCredit_immediate',
#     'timing_rebate_2', 'timing_rebate_6',
#     'source_rebate_oem', 'source_rebate_dealer')
# )
# 
# # Model 2 - baselines:
# # Type: salesTax 
# # Tax Credit Timing: Tax Filling
# # Rebate Timing: 6 Weeks 
# # Rebate Source: Dealership
# 
# m2 <- logitr::logitr(
#   data = choiceData, 
#   choice = 'choice',
#   obsID = 'obsID',
#   price = 'amount',
#   modelSpace = 'wtp',
#   pars = c(
#     'type_taxCredit', 'type_taxDeduction', 'type_rebate',
#     'timing_taxCredit_immediate',
#     'timing_rebate_0', 'timing_rebate_2',
#     'source_rebate_oem', 'source_rebate_government')
# )
# 
# summary(m1)
# summary(m2)
# 


# #data with SF1 & rebate timing as continuous -----------------
# choiceData <- read_csv(here("choiceData.csv"))
# choiceData_sf1 <- read_csv(here("softLaunch1", "choiceData.csv"))
# 
# #bind rows to softLaunch2 choiceData
# choiceData_comb <- choiceData %>%
#   bind_rows(choiceData_sf1)
# 
# #renumber obsID
# choiceData_comb$obsID = rep(seq(nrow(choiceData_comb) / 4), each = 4)
# 
# choiceData_comb <- choiceData_comb %>%
#   mutate(
#     maxSalestax = as.numeric(maxSalestax),
#     amount_salesTax = (round((amount_salesTax * maxSalestax), -2))) %>%
#   select(-maxSalestax)
# 
# choiceData_comb <- choiceData_comb %>%
#   mutate(
#     amount = ifelse(
#       type == "salesTax", as.numeric(amount_salesTax),
#       as.numeric(as.character(amount_nonSalesTax)))) %>%
#   select(-starts_with("amount_")) %>%
#   # Fix amount and add 0s to attribute-specific levels
#   fastDummies::dummy_cols(c(
#     'type', 'timing_taxCredit', 'source_rebate')) %>%
#   # Add 0s to type-specific dummy-coded variables
#   mutate(
#     timing_taxCredit_immediate = type_taxCredit*timing_taxCredit_immediate,
#     timing_taxCredit_tax_filing = type_taxCredit*timing_taxCredit_tax_filing,
#     source_rebate_dealer = type_rebate*source_rebate_dealer,
#     source_rebate_government = type_rebate*source_rebate_government,
#     source_rebate_oem = type_rebate*source_rebate_oem, 
#     timing_rebate = type_rebate*timing_rebate, 
#     timing_rebate_sq = timing_rebate^2)
# 
# 
# # Model 3 - baselines:
# # Type: salesTax 
# # Tax Credit Timing: Tax Filling
# # Rebate Source: Dealership
# 
# choiceData_comb$amount <- -1*choiceData_comb$amount 
# 
# 
# m3_comb <- logitr::logitr(
#   data = choiceData_comb, 
#   choice = 'choice',
#   obsID = 'obsID',
#   price = 'amount',
#   modelSpace = 'wtp',
#   pars = c(
#     'type_taxCredit', 'type_taxDeduction', 'type_rebate',
#     'timing_taxCredit_immediate',
#     'timing_rebate', 'timing_rebate_sq', 
#     'source_rebate_oem', 'source_rebate_government')
# )
# 
# summary(m3_comb)
# 
# # Save results
# save(m3_comb, file = here::here('models','model_comb_wtp_rebate_cont.RData'))
# 
