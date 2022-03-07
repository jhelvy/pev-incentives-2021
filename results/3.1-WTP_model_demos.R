#install.packages("remotes")
#remotes::install_github("emse-madd-gwu/maddTools")

#estimate WTP space model

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

#1 - Low/high income breakdown

#load models
load(here::here('models', 'model_comb_inc_low_m1_50k.RData'))
load(here::here('models', 'model_comb_inc_high_m1_50k.RData'))

#wtp calcs for low income
wtp_ci_inc_low <- wtp_calcs_m1(m1_comb = m1_low_50k)


wtp_ci_inc_low <- wtp_ci_inc_low %>%
  mutate(
    demo_label = "<$50k Income"
  )

#wtp calcs for high income
wtp_ci_inc_high <- wtp_calcs_m1(m1_comb = m1_high_50k)


wtp_ci_inc_high <- wtp_ci_inc_high %>%
  mutate(
    demo_label = ">$50k Income"
  )

#bind wtp tables together
wtp_ci_inc <- wtp_ci_inc_low %>%
  bind_rows(wtp_ci_inc_high)

wtp_ci_inc
save(wtp_ci_inc, file = here::here("wtp", "wtp_ci_inc.RData"))


#New/Used

#load models
load(here::here('models', 'model_comb_new_m1_50k.RData'))
load(here::here('models', 'model_comb_used_m1_50k.RData'))

#wtp calcs for new
wtp_ci_new <- wtp_calcs_m1(m1_comb = m1_new_50k)
wtp_ci_new <- wtp_ci_new %>%
  mutate(
    demo_label = "Exclusively New"
  )

#wtp calcs for used/both/not sure
wtp_ci_used <- wtp_calcs_m1(m1_comb = m1_used_50k)
wtp_ci_used <- wtp_ci_used %>%
  mutate(
    demo_label = "Used / Both / Not Sure"
  )

#bind wtp tables together
wtp_ci_vehicle <- wtp_ci_new %>%
  bind_rows(wtp_ci_used)

wtp_ci_inc
save(wtp_ci_vehicle, file = here::here("wtp", "wtp_ci_vehicle.RData"))


#Car Budget


#load models
load(here::here('models', 'model_comb_carBudgetlow_m1_50k.RData'))
load(here::here('models', 'model_comb_carBUdgethigh_m1_50k.RData'))

#wtp calcs for <$30k car budget
wtp_ci_carBudgetlow <- wtp_calcs_m1(m1_comb = m1_carBudgetlow_50k)

wtp_ci_carBudgetlow <- wtp_ci_carBudgetlow %>%
  mutate(
    demo_label = "<$30k budget"
  )

#wtp calcs for > $30k car budget
wtp_ci_carBudgethigh <- wtp_calcs_m1(m1_comb = m1_carBudgethigh_50k)

wtp_ci_carBudgethigh <- wtp_ci_carBudgethigh %>%
  mutate(
    demo_label = ">$30k budget"
  )

#bind wtp tables together
wtp_ci_carBudget <- wtp_ci_carBudgetlow %>%
  bind_rows(wtp_ci_carBudgethigh)

wtp_ci_carBudget
save(wtp_ci_carBudget, file = here::here("wtp", "wtp_ci_carBudget.RData"))



