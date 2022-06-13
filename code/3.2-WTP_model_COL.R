#install.packages("remotes")
#remotes::install_github("emse-madd-gwu/maddTools")

#estimate WTP space model COL

# Load libraries
library(tidyverse)
library(magrittr)
options(dplyr.width = Inf)
library(lubridate)
library(here)
library(janitor)
library(logitr)
library(mlogit)
options(scipen=999)
library(maddTools)
library(forcats)
library(data.table)

#function for running wtp calcs------------------------------------------------------------------


wtp_calcs_m1 <- function(m1_comb){
  # Method 2: Estimate WTP in WTP space model:
  coefs <- coef(m1_comb)
  covariance <- vcov(m1_comb)
  wtp_draws <- as.data.frame(mvrnorm(10^4, coefs, covariance))
  
  #rename columns
  setnames(wtp_draws, "type_taxCredit", "type_taxCredit_taxFiling")
  
  #create data frame for options from draws
  wtp_draws <- wtp_draws %>% 
    mutate(
      type_taxCredit_immediate = type_taxCredit_taxFiling + timing_taxCredit_immediate,
      type_rebate_6wks_dealer = timing_rebate_6 + source_rebate_dealer,
      type_rebate_2wks_dealer = timing_rebate_2 + source_rebate_dealer,
      type_rebate_0wks_dealer = source_rebate_dealer,
      type_rebate_6wks_oem = timing_rebate_6 + source_rebate_oem, 
      type_rebate_2wks_oem = timing_rebate_2 + source_rebate_oem,
      type_rebate_0wks_oem = source_rebate_oem,
      type_rebate_6wks_govt = timing_rebate_6,
      type_rebate_2wks_govt = timing_rebate_2
    ) %>%
    select(contains("type"))
  
  # CI of draws 
  wtp_ci <- ci(wtp_draws)
  
  # Convert to dollars (model was estimated in thousands of dollars)
  wtp_ci <- wtp_ci %>% 
    mutate(
      mean = mean*1000,
      lower = lower*1000,
      upper = upper*1000,
    )
  
  # add type facet
  wtp_ci$par <- row.names(wtp_ci)
  
  #add baseline row
  wtp_ci <- wtp_ci %>%
    add_row(tibble_row(mean = 0, lower = 0, upper = 0, par = "type_rebate_0wks_govt"))
  
  # add type timing and source columns for faceting/plot
  wtp_ci <- wtp_ci %>%
    mutate(
      type = ifelse(str_detect(par, "rebate") == "TRUE","Rebate",
                    ifelse(str_detect(par, "taxCredit") == "TRUE","Tax Credit",
                           ifelse(str_detect(par, "taxDeduction")== "TRUE","Tax\nDeduction",
                                  ifelse(str_detect(par, "salesTax") == "TRUE", "Sales Tax",0)))),
      timing = ifelse(str_detect(par, "rebate_6wks") == "TRUE","6wks",
                      ifelse(str_detect(par, "rebate_2wks") == "TRUE","2wks",
                             ifelse(str_detect(par, "rebate_0wks")== "TRUE","0wks",
                                    ifelse(str_detect(par, "taxCredit_immediate") == "TRUE", "0wks",
                                           ifelse(str_detect(par, "salesTax") == "TRUE", "0wks",1))))),
      source = ifelse(str_detect(par, "dealer") == "TRUE", "dealer",
                      ifelse(str_detect(par, "oem") == "TRUE", "oem",
                             ifelse(str_detect(par,"govt") == "TRUE", "govt",0)))
      
    ) %>%
    mutate(source = factor(source, levels = c("govt", "oem", "dealer",0))) %>%
    arrange(type, timing, source)
  
  #wtp_ci <- wtp_ci[-1,] # Drop lambda (we won't plot this)
  return(wtp_ci)
}


#------------------------------------------------------------------------------------------------

# MNL COL - m1_comb with rebate as baseline

load(here::here('models', 'model_comb_wtp_m1_50k_col.RData'))

#wtp calcs
wtp_ci_col <- wtp_calcs_m1(m1_comb = m1_comb_50k_col)

wtp_ci_col
save(wtp_ci_col, file = here::here("wtp", "wtp_ci_col.RData"))


# MXL COL - m1_comb with rebate as baseline

load(here::here('models', 'model_comb_wtp_mxl_m1_50k_col.RData'))


# Get WTP estimates with 95% CI

# Method 2: Estimate WTP in WTP space model:
coefs <- coef(mxl_comb_50k_col)
covariance <- vcov(mxl_comb_50k_col)
wtp_draws <- as.data.frame(mvrnorm(10^4, coefs, covariance))

#rename columns
setnames(wtp_draws, "type_taxCredit", "type_taxCredit_taxFiling")
setnames(wtp_draws, "sd_type_taxCredit", "sd_type_taxCredit_taxFiling")

#create data frame for options from draws
wtp_draws <- wtp_draws %>% 
  mutate(
    type_taxCredit_immediate = type_taxCredit_taxFiling + timing_taxCredit_immediate,
    type_rebate_6wks_dealer = timing_rebate_6 + source_rebate_dealer,
    type_rebate_2wks_dealer = timing_rebate_2 + source_rebate_dealer,
    type_rebate_0wks_dealer = source_rebate_dealer,
    type_rebate_6wks_oem = timing_rebate_6 + source_rebate_oem, 
    type_rebate_2wks_oem = timing_rebate_2 + source_rebate_oem,
    type_rebate_0wks_oem = source_rebate_oem,
    type_rebate_6wks_govt = timing_rebate_6,
    type_rebate_2wks_govt = timing_rebate_2
  ) %>%
  select(contains("type"), -contains("sd")) 

# CI of draws 
wtp_ci_mxl_col <- ci(wtp_draws)

# Convert to dollars (model was estimated in thousands of dollars)
wtp_ci_mxl_col <- wtp_ci_mxl_col %>% 
  mutate(
    mean = mean*1000,
    lower = lower*1000,
    upper = upper*1000,
  )

# add type facet
wtp_ci_mxl_col$par <- row.names(wtp_ci_mxl_col)

#add baseline row
wtp_ci_mxl_col <- wtp_ci_mxl_col %>%
  add_row(tibble_row(mean = 0, lower = 0, upper = 0, par = "type_rebate_0wks_govt"))

# add type timing and source columns for faceting/plot
wtp_ci_mxl_col <- wtp_ci_mxl_col %>%
  mutate(
    type = ifelse(str_detect(par, "rebate") == "TRUE","Rebate",
                  ifelse(str_detect(par, "taxCredit") == "TRUE","Tax Credit",
                         ifelse(str_detect(par, "taxDeduction")== "TRUE","Tax\nDeduction",
                                ifelse(str_detect(par, "salesTax") == "TRUE", "Sales Tax",0)))),
    timing = ifelse(str_detect(par, "rebate_6wks") == "TRUE","6wks",
                    ifelse(str_detect(par, "rebate_2wks") == "TRUE","2wks",
                           ifelse(str_detect(par, "rebate_0wks")== "TRUE","0wks",
                                  ifelse(str_detect(par, "taxCredit_immediate") == "TRUE", "0wks",
                                         ifelse(str_detect(par, "salesTax") == "TRUE", "0wks",1))))),
    source = ifelse(str_detect(par, "dealer") == "TRUE", "dealer",
                    ifelse(str_detect(par, "oem") == "TRUE", "oem",
                           ifelse(str_detect(par,"govt") == "TRUE", "govt",0)))
    
  ) %>%
  mutate(source = factor(source, levels = c("govt", "oem", "dealer",0))) %>%
  arrange(type, timing, source)

wtp_ci_mxl_col
save(wtp_ci_mxl_col, file = here::here("wtp", "wtp_ci_mxl_col.RData"))


#1 - Low/high income breakdown

#load models
load(here::here('models', 'model_comb_inc_low_m1_50k_col.RData'))
load(here::here('models', 'model_comb_inc_high_m1_50k_col.RData'))

#wtp calcs for low income
wtp_ci_inc_low_col <- wtp_calcs_m1(m1_comb = m1_low_50k_col)


wtp_ci_inc_low_col <- wtp_ci_inc_low_col %>%
  mutate(
    demo_label = "<$50k Income"
  )

#wtp calcs for high income
wtp_ci_inc_high_col <- wtp_calcs_m1(m1_comb = m1_high_50k_col)


wtp_ci_inc_high_col <- wtp_ci_inc_high_col %>%
  mutate(
    demo_label = ">$50k Income"
  )

#bind wtp tables together
wtp_ci_inc_col <- wtp_ci_inc_low_col %>%
  bind_rows(wtp_ci_inc_high_col)

wtp_ci_inc_col
save(wtp_ci_inc_col, file = here::here("wtp", "wtp_ci_inc_col.RData"))


