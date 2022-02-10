#install.packages("remotes")
#remotes::install_github("emse-madd-gwu/maddTools")

#estimate WTP space model

# Load libraries
library(logitr)
library(maddTools)
library(forcats)
library(data.table)

#Model m1 (rebate, 0wks, govt) full sample------------------

# Read in the estimated models with 50k added - m1_comb with rebate as baseline

load(here::here('models', 'model_comb_wtp_m1_50k.RData'))

# Get WTP estimates with 95% CI

# Method 2: Estimate WTP in WTP space model:
coefs <- coef(m1_comb_50k)
covariance <- vcov(m1_comb_50k)
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

wtp_ci
save(wtp_ci, file = here::here("wtp", "wtp_ci.RData"))

#MXL

load(here::here('models', 'model_comb_wtp_mxl_m1_50k.RData'))

# Get WTP estimates with 95% CI

# Method 2: Estimate WTP in WTP space model:
coefs <- coef(mxl_comb_50k)
covariance <- vcov(mxl_comb_50k)
wtp_draws <- as.data.frame(mvrnorm(10^4, coefs, covariance))

#rename columns
setnames(wtp_draws, "type_taxCredit_mu", "type_taxCredit_taxFiling_mu")
setnames(wtp_draws, "type_taxCredit_sigma", "type_taxCredit_taxFiling_sigma")

#create data frame for options from draws
wtp_draws <- wtp_draws %>% 
  mutate(
    type_taxCredit_immediate_mu = type_taxCredit_taxFiling_mu + timing_taxCredit_immediate_mu,
    type_rebate_6wks_dealer_mu = timing_rebate_6_mu + source_rebate_dealer_mu,
    type_rebate_2wks_dealer_mu = timing_rebate_2_mu + source_rebate_dealer_mu,
    type_rebate_0wks_dealer_mu = source_rebate_dealer_mu,
    type_rebate_6wks_oem_mu = timing_rebate_6_mu + source_rebate_oem_mu, 
    type_rebate_2wks_oem_mu = timing_rebate_2_mu + source_rebate_oem_mu,
    type_rebate_0wks_oem_mu = source_rebate_oem_mu,
    type_rebate_6wks_govt_mu = timing_rebate_6_mu,
    type_rebate_2wks_govt_mu = timing_rebate_2_mu
  ) %>%
  select(contains("type"), -contains("sigma")) 

# CI of draws 
wtp_ci_mxl <- ci(wtp_draws)


# add type facet
wtp_ci_mxl$par <- row.names(wtp_ci_mxl)

#add baseline row
wtp_ci_mxl <- wtp_ci_mxl %>%
  add_row(tibble_row(mean = 0, lower = 0, upper = 0, par = "type_rebate_0wks_govt"))

# add type timing and source columns for faceting/plot
wtp_ci_mxl <- wtp_ci_mxl %>%
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

wtp_ci_mxl
save(wtp_ci_mxl, file = here::here("wtp", "wtp_ci_mxl.RData"))


# #WTP m1 no 50K-----------------------------------
# 
# # Read in the estimated models - m1_comb with rebate as baseline
# 
# load(here::here('models', 'model_comb_wtp_m1.RData'))
# 
# 
# # Get WTP estimates with 95% CI
# 
# # Method 2: Estimate WTP in WTP space model:
# coefs <- coef(m1_comb)
# covariance <- vcov(m1_comb)
# wtp_draws <- as.data.frame(mvrnorm(10^4, coefs, covariance))
# 
# #rename columns
# setnames(wtp_draws, "type_taxCredit", "type_taxCredit_taxFiling")
# 
# #create data frame for options from draws
# wtp_draws <- wtp_draws %>% 
#   mutate(
#     type_taxCredit_immediate = type_taxCredit_taxFiling + timing_taxCredit_immediate,
#     type_rebate_6wks_dealer = timing_rebate_6 + source_rebate_dealer,
#     type_rebate_2wks_dealer = timing_rebate_2 + source_rebate_dealer,
#     type_rebate_0wks_dealer = source_rebate_dealer,
#     type_rebate_6wks_oem = timing_rebate_6 + source_rebate_oem, 
#     type_rebate_2wks_oem = timing_rebate_2 + source_rebate_oem,
#     type_rebate_0wks_oem = source_rebate_oem,
#     type_rebate_6wks_govt = timing_rebate_6,
#     type_rebate_2wks_govt = timing_rebate_2
#   ) %>%
#   select(contains("type"))
# 
# # CI of draws 
# wtp_ci <- ci(wtp_draws)
# 
# 
# # add type facet
# wtp_ci$par <- row.names(wtp_ci)
# 
# #add baseline row
# wtp_ci <- wtp_ci %>%
#   add_row(tibble_row(mean = 0, lower = 0, upper = 0, par = "type_rebate_0wks_govt"))
# 
# # add type timing and source columns for faceting/plot
# wtp_ci <- wtp_ci %>%
#   mutate(
#     type = ifelse(str_detect(par, "rebate") == "TRUE","Rebate",
#                   ifelse(str_detect(par, "taxCredit") == "TRUE","Tax Credit",
#                          ifelse(str_detect(par, "taxDeduction")== "TRUE","Tax\nDeduction",
#                                 ifelse(str_detect(par, "salesTax") == "TRUE", "Sales Tax",0)))),
#     timing = ifelse(str_detect(par, "rebate_6wks") == "TRUE","6wks",
#                     ifelse(str_detect(par, "rebate_2wks") == "TRUE","2wks",
#                            ifelse(str_detect(par, "rebate_0wks")== "TRUE","0wks",
#                                   ifelse(str_detect(par, "taxCredit_immediate") == "TRUE", "0wks",
#                                          ifelse(str_detect(par, "salesTax") == "TRUE", "0wks",1))))),
#     source = ifelse(str_detect(par, "dealer") == "TRUE", "dealer",
#                     ifelse(str_detect(par, "oem") == "TRUE", "oem",
#                            ifelse(str_detect(par,"govt") == "TRUE", "govt",0)))
#   ) %>%
#   mutate(source = factor(source, levels = c("govt", "oem", "dealer",0))) %>%
#   arrange(type, timing, source)
# 
# #wtp_ci <- wtp_ci[-1,] # Drop lambda (we won't plot this)
# wtp_ci
# write_csv(wtp_ci, here::here("wtp_ci_total_m1.csv"))

# #WTP m2 no 50k--------------------------------------------------------------------------
# # Read in the estimated models - m2_comb with "worst" options as 0
# load(here::here('models', 'model_comb_wtp_m2.RData'))
# 
# 
# # Get WTP estimates with 95% CI
# 
# # Method 2: Estimate WTP in WTP space model:
# 
# coefs <- coef(m2_comb)
# covariance <- vcov(m2_comb)
# wtp_draws <- as.data.frame(mvrnorm(10^4, coefs, covariance))
# 
# #rename columns
# setnames(wtp_draws, "type_taxCredit", "type_taxCredit_taxFiling")
# setnames(wtp_draws, "type_rebate", "type_rebate_6wks_dealer")
# 
# #create data frame for options from draws
# wtp_draws <- wtp_draws %>% 
#   mutate(
#     type_taxCredit_immediate = type_taxCredit_taxFiling + timing_taxCredit_immediate,
#     type_rebate_2wks_dealer = type_rebate_6wks_dealer + timing_rebate_2,
#     type_rebate_0wks_dealer = type_rebate_6wks_dealer + timing_rebate_0,
#     type_rebate_6wks_oem = type_rebate_6wks_dealer + source_rebate_oem, 
#     type_rebate_2wks_oem = type_rebate_6wks_dealer + timing_rebate_2 + source_rebate_oem,
#     type_rebate_0wks_oem = type_rebate_6wks_dealer + timing_rebate_0 + source_rebate_oem,
#     type_rebate_6wks_govt = type_rebate_6wks_dealer + source_rebate_government,
#     type_rebate_2wks_govt = type_rebate_6wks_dealer + timing_rebate_2 + source_rebate_government,
#     type_rebate_0wks_govt = type_rebate_6wks_dealer + timing_rebate_0 + source_rebate_government
#   ) %>%
#   select(contains("type"))
# 
# # CI of draws 
# wtp_ci <- ci(wtp_draws)
# 
# # add type facet
# wtp_ci$par <- row.names(wtp_ci)
# wtp_ci <- wtp_ci %>%
#   mutate(
#     type = ifelse(str_detect(par, "rebate") == "TRUE","Rebate",
#                   ifelse(str_detect(par, "taxCredit") == "TRUE","Tax Credit",
#                          ifelse(str_detect(par, "taxDeduction")== "TRUE","Tax\nDeduction",0))),
#     timing = ifelse(str_detect(par, "rebate_6wks") == "TRUE","6wks",
#                     ifelse(str_detect(par, "rebate_2wks") == "TRUE","2wks",
#                            ifelse(str_detect(par, "rebate_0wks")== "TRUE","0wks",
#                                   ifelse(str_detect(par, "taxCredit_immediate") == "TRUE", "0wks",1))))
#     
#   ) %>%
#   arrange(type, timing)
# 
# #wtp_ci <- wtp_ci[-1,] # Drop lambda (we won't plot this)
# wtp_ci