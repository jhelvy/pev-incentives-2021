source("1-load_data.R")

library(data.table)
library(cowplot)
library(logitr)
library(mlogit)
library(fastDummies)
library(gtsummary)

choiceData <- read_csv(here("choiceData.csv"))
# Re-code choiceData
#bring in sales tax amount based on budget
#salesTax <- data %>%
 # select(respondentID, maxSalestax) 

#choiceData <- choiceData %>%
  #left_join(salesTax, by = "respondentID")

choiceData <- choiceData %>%
  mutate(
    maxSalestax = as.numeric(maxSalestax),
    amount_salesTax = (round((amount_salesTax * maxSalestax), -2))) %>%
  select(-maxSalestax)

choiceData <- choiceData %>%
  mutate(
    amount = ifelse(
      type == "salesTax", as.numeric(amount_salesTax),
      as.numeric(as.character(amount_nonSalesTax)))) %>%
  select(-starts_with("amount_")) %>%
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

# Partworth model
parNames <- c(
  'amount',
  'type_taxCredit', 'type_taxDeduction', 'type_rebate',
  'timing_taxCredit_immediate',
  'timing_rebate_0', 'timing_rebate_2',
  'source_rebate_oem', 'source_rebate_government')

partworth <- logitr::logitr(
  data = choiceData, parNames = parNames, randPars = NULL,
  choiceName = 'choice', obsIDName = 'obsID', priceName = NULL,
  randPrice = NULL, modelSpace = 'pref', weights = NULL)

# Save results
saveRDS(partworth, here::here('partworth.Rds'))

summary(partworth)
wtp(partworth, "amount")






# Compare different encodings:

choiceData$amount <- -1*choiceData$amount

# Model 1 - baselines:
# Type: salesTax 
# Tax Credit Timing: Tax Filling
# Rebate Timing: 0 Weeks 
# Rebate Source: Government

m1 <- logitr::logitr(
  data = choiceData, 
  choiceName = 'choice',
  obsIDName = 'obsID',
  priceName = 'amount',
  modelSpace = 'wtp',
  parNames = c(
    'type_taxCredit', 'type_taxDeduction', 'type_rebate',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

# Model 2 - baselines:
# Type: salesTax 
# Tax Credit Timing: Tax Filling
# Rebate Timing: 6 Weeks 
# Rebate Source: Dealership

m2 <- logitr::logitr(
  data = choiceData, 
  choiceName = 'choice',
  obsIDName = 'obsID',
  priceName = 'amount',
  modelSpace = 'wtp',
  parNames = c(
    'type_taxCredit', 'type_taxDeduction', 'type_rebate',
    'timing_taxCredit_immediate',
    'timing_rebate_0', 'timing_rebate_2',
    'source_rebate_oem', 'source_rebate_government')
)

summary(m1)
summary(m2)
