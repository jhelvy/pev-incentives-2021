library(tidyverse)
library(magrittr)
options(dplyr.width = Inf)
library(here)
library(readxl)
library(data.table)
library(cowplot)
library(logitr)
library(mlogit)
library(fastDummies)
options(scipen=999)

#Building Cost of Living (COL) data table--------------------------------

data_comb_50k <- read_csv(here("data_filtered_50k.csv"))
choiceData_comb_50k <- read_csv(here("choiceData_comb_50k.csv"))

#filter zips from data
data_comb_50k_zip <- data_comb_50k %>%
  select(session, zipcode)

#Regional Price Parity - BEA 2020
rpp <- read_excel(here("rpp1221.xlsx"), sheet = "Table 4", skip = 3)
head(rpp)

#clean up rpp import col names
rpp <- rpp %>% 
  rename(
    area = ...1,
    rpp = ...2,
    goods = ...3,
    regional_price_deflators_all = ...7,
    rpd_yoy = ...8)

#pull in city (zipcodeR)
#
# geo_data_comb <- reverse_zipcode(data_comb_50k_zip$zipcode) %>%
#   select(zipcode, post_office_city, lat,lng)
# 
# data_comb_50k_zip <- data_comb_50k_zip %>%
#   left_join(geo_data_comb, by = "zipcode") %>%
#   rename(
#     area = "post_office_city")
# 
# data_comb_50k_zip <- data_comb_50k_zip %>%
#   left_join(rpp, by = "area")

#HUD-USPS cross walk ZIP to CBSA Q4 2021 US Dept of Housing and Urban Dev https://www.huduser.gov/portal/datasets/usps_crosswalk.html#data 
cbsa <- read_excel(here("ZIP_CBSA_122021.xlsx"))
head(cbsa)
# length(unique(cbsa$zip))

# dups <- cbsa %>%
#   filter(duplicated(zipcode)) %>%
#   arrange(zipcode)

#clean up cbsa file
cbsa <- cbsa %>%
  #for zips with multiples cbsas, choose cbsa containing over 50% ratio of residential addresses in that zip 
  #res_raito - The ratio of residential addresses in the ZIP – Tract, County, or CBSA part to the total number of residential addresses in the entire ZIP https://www.huduser.gov/portal/datasets/usps_crosswalk.html#codebook
  filter(res_ratio > .5) %>%
  rename(
    zipcode = zip
  ) %>%
  select(zipcode, cbsa)

#CBSA reference w/ cbsa codes and names Census Bureau March 2020 https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html
cbsa_ref <- read_excel(here("cbsa_2020.xls"), skip = 2)
head(cbsa_ref)

cbsa_ref <- cbsa_ref %>%
  rename(
    cbsa = 'CBSA Code',
    cbsa_name = 'CBSA Title'
  ) %>% 
  select(cbsa, cbsa_name)


#join cbsa codes and names
cbsa <- cbsa %>% 
  left_join(cbsa_ref, by = "cbsa") 

#join data to cbsa info
data_cbsa <- data_comb_50k_zip %>%
  left_join(cbsa, by = c("zipcode")) %>%
  distinct(session, .keep_all = TRUE)

data_cbsa <- data_cbsa %>%
  rename(
    area = cbsa_name
  ) %>%
  left_join(rpp, by = "area") %>%
  select(session, zipcode, area, rpp)
tally(data_cbsa %>% filter(is.na(rpp)))

#NA data frame
data_cbsa_NA <- data_cbsa %>% 
  filter(is.na(rpp))
tally(data_cbsa_NA %>% filter(is.na(area)))

#calculate adj factor %
data_cbsa <- data_cbsa %>%
  mutate(
    rpp_adj = (100-rpp)/100
  )

#join to choice data
choiceData_comb_50k_col <- choiceData_comb_50k %>%
  left_join(data_cbsa, by = "session")

#adjust amounts
choiceData_comb_50k_col <- choiceData_comb_50k_col %>%
  mutate(
    amount_rpp_adj = round(amount+(amount*rpp_adj),0)
  ) %>%
  filter(!is.na(amount_rpp_adj)) %>%
  mutate(
    obsID = rep(seq(n() / 4), each = 4))

#add data for model
choiceData_comb_50k_col <- choiceData_comb_50k_col %>%
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

choiceData_comb_50k_col$amount_rpp_adj <- -1*choiceData_comb_50k_col$amount_rpp_adj
length(unique(choiceData_comb_50k_col$session))

# Model 1 - baselines:
# Type: Rebate
# Tax Credit Timing: Tax Filling
# Rebate Timing: 0 Weeks 
# Rebate Source: Government

m1_comb_50k_col <- logitr::logitr(
  data = choiceData_comb_50k_col, 
  outcome = "choice",
  obsID = "obsID",
  price = "amount_rpp_adj",
  modelSpace = "wtp",
  pars = c(
    "type_salesTax", "type_taxCredit", "type_taxDeduction",
    "timing_taxCredit_immediate",
    "timing_rebate_2", "timing_rebate_6",
    "source_rebate_oem", "source_rebate_dealer")
)
summary(m1_comb_50k_col)


# Save results
save(m1_comb_50k_col, file = here::here('models', 'model_comb_wtp_m1_50k_col.RData'))


#Estimate MXL model--------------------------------------------------------


#Estimate the model
mxl_comb_50k_col <- logitr::logitr(
  data   = choiceData_comb_50k_col,
  outcome = 'choice',
  obsID  = 'obsID',
  price = 'amount_rpp_adj',
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

summary(mxl_comb_50k_col)
save(mxl_comb_50k_col, file = here::here('models', 'model_comb_wtp_mxl_m1_50k_col.RData'))

#1 - Income (high/low)

#Low Income

#prep/filter choiceData_comb
choiceData_low_50k_col <- choiceData_comb_50k_col %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income == "under25" | income == "inc_25to35" | income == "inc_35to50") %>%
  mutate(
    obsID = rep(seq(n() / 4), each = 4)) %>%
  select(-id)
length(unique(choiceData_low_50k_col$session))

#m1 model low (rebate 0wks govt)

m1_low_50k_col <- logitr::logitr(
  data = choiceData_low_50k_col, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount_rpp_adj',
  modelSpace = 'wtp',
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_low_50k_col)
save(m1_low_50k_col, file = here::here('models', 'model_comb_inc_low_m1_50k_col.RData'))


#high income 

#prep/filter choiceData_comb
choiceData_high_50k_col <- choiceData_comb_50k_col %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income != "under25" & income != "inc_25to35" & income != "inc_35to50") %>%
  mutate(
    obsID = rep(seq(n() / 4), each = 4))
length(unique(choiceData_high_50k_col$session))

#m1 model high (rebate 0wks govt)

m1_high_50k_col <- logitr::logitr(
  data = choiceData_high_50k_col, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount_rpp_adj',
  modelSpace = 'wtp',
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_high_50k_col)
save(m1_high_50k_col, file = here::here('models', 'model_comb_inc_high_m1_50k_col.RData'))



#2 - Exclusively New vs. Used/both

#New (choice 1)

#prep/filter data from choiceData_comb
choiceData_new_50k_col <- choiceData_comb_50k_col %>% 
  left_join(
    data_comb_50k %>% 
      select(session, new_or_used),
    by = "session") %>%
  filter(new_or_used == 1) %>%
  mutate(
    obsID = rep(seq(n() / 4), each = 4))
length(unique(choiceData_new_50k_col$session))

#m1 model new (rebate 0wks govt)

m1_new_50k_col <- logitr::logitr(
  data = choiceData_new_50k_col, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount_rpp_adj',
  modelSpace = 'wtp',
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_new_50k_col)
save(m1_new_50k_col, file = here::here('models', 'model_comb_new_m1_50k_col.RData'))


#used and other (used, both and not sure)

#prep/filter choice data_comb 
choiceData_used_50k_col <- choiceData_comb_50k_col %>% 
  left_join(
    data_comb_50k %>% 
      select(session, new_or_used),
    by = "session") %>%
  filter(new_or_used != 1) %>%
  mutate(
    obsID = rep(seq(n() / 4), each = 4))
length(unique(choiceData_used_50k_col$session))

#m1 model used (rebate 0wks govt)

m1_used_50k_col <- logitr::logitr(
  data = choiceData_used_50k_col, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount_rpp_adj',
  modelSpace = 'wtp',
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_used_50k_col)
save(m1_used_50k_col, file = here::here('models', 'model_comb_used_m1_50k_col.RData'))


#Car budget

#<30k
#prep/filter choiceData_comb
choiceData_carBudgetlow_50k_col <- choiceData_comb_50k_col %>% 
  left_join(
    data_comb_50k %>% 
      select(session, carBudget),
    by = "session") %>%
  filter(carBudget == "under_10" | carBudget == "10-15" | carBudget == "15-20" | carBudget == "20-25" | carBudget == "25-30") %>%
  mutate(
    obsID = rep(seq(n() / 4), each = 4))
length(unique(choiceData_carBudgetlow_50k_col$session))

#m1 model (rebate 0wks govt)

m1_carBudgetlow_50k_col <- logitr::logitr(
  data = choiceData_carBudgetlow_50k_col, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount_rpp_adj',
  modelSpace = 'wtp',
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_carBudgetlow_50k_col)
save(m1_carBudgetlow_50k_col, file = here::here('models', 'model_comb_carBudgetlow_m1_50k_col.RData'))

#>30k
#prep/filter choiceData_comb
choiceData_carBudgethigh_50k_col <- choiceData_comb_50k_col %>% 
  left_join(
    data_comb_50k %>% 
      select(session, carBudget),
    by = "session") %>%
  filter(carBudget != "under_10" & carBudget != "10-15" & carBudget != "15-20" & carBudget != "20-25" & carBudget != "25-30") %>%
  mutate(
    obsID = rep(seq(n() / 4), each = 4))
length(unique(choiceData_carBudgethigh_50k_col$session))

#m1 model (rebate 0wks govt)

m1_carBudgethigh_50k_col <- logitr::logitr(
  data = choiceData_carBudgethigh_50k_col, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount_rpp_adj',
  modelSpace = 'wtp',
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_carBudgethigh_50k_col)
save(m1_carBudgethigh_50k_col, file = here::here('models', 'model_comb_carBudgethigh_m1_50k_col.RData'))

#flextable

library(officer)
library(flextable)

summary_sl2 <- make_coef_table3(m1_comb_50k_col)
summary_mxl2 <- make_coef_table3(mxl_comb_50k_col) %>% mutate(coefficients = str_remove(coefficients, "_mu"))
summary_high2 <- make_coef_table3(m1_high_50k_col) 
summary_low2 <- make_coef_table3(m1_low_50k_col) 
summary_new2 <- make_coef_table3(m1_new_50k_col) 
summary_used2 <- make_coef_table3(m1_used_50k_col)
summary_more30k <- make_coef_table3(m1_carBudgethigh_50k_col)
summary_less30k <- make_coef_table3(m1_carBudgetlow_50k_col)


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
