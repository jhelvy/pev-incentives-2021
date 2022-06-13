#models incorporating demographic/knowledge differences

source("1-load_data.R")

library(data.table)
library(cowplot)
library(logitr)
library(mlogit)
library(fastDummies)
library(gtsummary)
options(scipen=999)

#read in choice data from combined model +50k
data_comb_50k <- read_csv(here( "data-processed", "data_filtered_50k.csv"))
choiceData_comb_50k <- read_csv(here("data-processed","choiceData_comb_50k.csv"))

#prep choice data for modeling
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

# Negative of amount as it's not a price you pay but rather a subsidy you get
choiceData_comb_50k$amount <- -1 * choiceData_comb_50k$amount / 1000

#1 - Income (high/low)

#Low Income

#prep/filter choiceData_comb
choiceData_low_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income == "under25" | income == "inc_25to35" | income == "inc_35to50") 
length(unique(choiceData_low_50k$session))

#fix id & obsID for low income
choiceData_low_50k <- choiceData_low_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_low_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))


#m1 model low (rebate 0wks govt)

m1_low_50k <- logitr::logitr(
  data = choiceData_low_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_low_50k)
save(m1_low_50k, file = here::here('models', 'model_comb_inc_low_m1_50k.RData'))


#high income 

#prep/filter choiceData_comb
choiceData_high_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income != "under25" & income != "inc_25to35" & income != "inc_35to50") 
length(unique(choiceData_high_50k$session))

#fix id and obsID for high income
choiceData_high_50k <- choiceData_high_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_high_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model high (rebate 0wks govt)

m1_high_50k <- logitr::logitr(
  data = choiceData_high_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_high_50k)
save(m1_high_50k, file = here::here('models', 'model_comb_inc_high_m1_50k.RData'))



#2 - Exclusively New vs. Used/both

tabyl(data_comb_50k$new_or_used)

#New (choice 1)

#prep/filter data from choiceData_comb
choiceData_new_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, new_or_used),
    by = "session") %>%
  filter(new_or_used == 1) 
length(unique(choiceData_new_50k$session))

#fix id & obsID for new buyers
choiceData_new_50k <- choiceData_new_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_new_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model new (rebate 0wks govt)

m1_new_50k <- logitr::logitr(
  data = choiceData_new_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_new_50k)
save(m1_new_50k, file = here::here('models', 'model_comb_new_m1_50k.RData'))


#used and other (used, both and not sure)

#prep/filter choice data_comb 
choiceData_used_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, new_or_used),
    by = "session") %>%
  filter(new_or_used != 1) 
length(unique(choiceData_used_50k$session))

#fix id & obsID for used/other buyers
choiceData_used_50k <- choiceData_used_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_used_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model used (rebate 0wks govt)

m1_used_50k <- logitr::logitr(
  data = choiceData_used_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_used_50k)
save(m1_used_50k, file = here::here('models', 'model_comb_used_m1_50k.RData'))


#Car budget

#<30k
#prep/filter choiceData_comb
choiceData_carBudgetlow_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, carBudget),
    by = "session") %>%
  filter(carBudget == "under_10" | carBudget == "10-15" | carBudget == "15-20" | carBudget == "20-25" | carBudget == "25-30")
length(unique(choiceData_carBudgetlow_50k$session))

#fix id & obsID for <$30k budget
choiceData_carBudgetlow_50k <- choiceData_carBudgetlow_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_carBudgetlow_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model (rebate 0wks govt)

m1_carBudgetlow_50k <- logitr::logitr(
  data = choiceData_carBudgetlow_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_carBudgetlow_50k)
save(m1_carBudgetlow_50k, file = here::here('models', 'model_comb_carBudgetlow_m1_50k.RData'))

#>30k
#prep/filter choiceData_comb
choiceData_carBudgethigh_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, carBudget),
    by = "session") %>%
  filter(carBudget != "under_10" & carBudget != "10-15" & carBudget != "15-20" & carBudget != "20-25" & carBudget != "25-30")
length(unique(choiceData_carBudgethigh_50k$session))

#fix id & obsID for >$30k budget
choiceData_carBudgethigh_50k <- choiceData_carBudgethigh_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_carBudgethigh_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model (rebate 0wks govt)

m1_carBudgethigh_50k <- logitr::logitr(
  data = choiceData_carBudgethigh_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_carBudgethigh_50k)
save(m1_carBudgethigh_50k, file = here::here('models', 'model_comb_carBudgethigh_m1_50k.RData'))


#---------------------------------Appendix-----------------------------------------

choiceData_inclessthan25_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income == "under25") 
length(unique(choiceData_inclessthan25_50k$session))

#fix id and obsID for high income
choiceData_inclessthan25_50k <- choiceData_inclessthan25_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_inclessthan25_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model high (rebate 0wks govt)

m1_inclessthan25_50k <- logitr::logitr(
  data = choiceData_inclessthan25_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_inclessthan25_50k)
save(m1_inclessthan25_50k, file = here::here('models', 'model_comb_inc_lessthan25_m1_50k.RData'))

#25-35
choiceData_inc25_35_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income == "inc_25to35") 
length(unique(choiceData_inc25_35_50k$session))

#fix id and obsID
choiceData_inc25_35_50k <- choiceData_inc25_35_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_inc25_35_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model (rebate 0wks govt)

m1_inc25_35_50k <- logitr::logitr(
  data = choiceData_inc25_35_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_inc25_35_50k)
save(m1_inc25_35_50k, file = here::here('models', 'model_comb_inc_25-35_m1_50k.RData'))

#35-50
choiceData_inc35_50_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income == "inc_35to50") 
length(unique(choiceData_inc35_50_50k$session))

#fix id and obsID
choiceData_inc35_50_50k <- choiceData_inc35_50_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_inc35_50_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model (rebate 0wks govt)

m1_inc35_50_50k <- logitr::logitr(
  data = choiceData_inc35_50_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_inc35_50_50k)
save(m1_inc35_50_50k, file = here::here('models', 'model_comb_inc_35-50_m1_50k.RData'))


#50-75
choiceData_inc50_75_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income == "inc_50to75") 
length(unique(choiceData_inc50_75_50k$session))

#fix id and obsID
choiceData_inc50_75_50k <- choiceData_inc50_75_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_inc50_75_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model (rebate 0wks govt)

m1_inc50_75_50k <- logitr::logitr(
  data = choiceData_inc50_75_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_inc50_75_50k)
save(m1_inc50_75_50k, file = here::here('models', 'model_comb_inc_50-75_m1_50k.RData'))


#75-100
choiceData_inc75_100_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income == "inc_75to100") 
length(unique(choiceData_inc75_100_50k$session))

#fix id and obsID
choiceData_inc75_100_50k <- choiceData_inc75_100_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_inc75_100_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model (rebate 0wks govt)

m1_inc75_100_50k <- logitr::logitr(
  data = choiceData_inc75_100_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_inc75_100_50k)
save(m1_inc75_100_50k, file = here::here('models', 'model_comb_inc_75-100_m1_50k.RData'))


#100-150
choiceData_inc100_150_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income == "inc_100to150") 
length(unique(choiceData_inc100_150_50k$session))

#fix id and obsID
choiceData_inc100_150_50k <- choiceData_inc100_150_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_inc100_150_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model (rebate 0wks govt)

m1_inc100_150_50k <- logitr::logitr(
  data = choiceData_inc100_150_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_inc100_150_50k)
save(m1_inc100_150_50k, file = here::here('models', 'model_comb_inc_100-150_m1_50k.RData'))


#150-200
choiceData_inc150_200_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income == "inc_150to200") 
length(unique(choiceData_inc150_200_50k$session))

#fix id and obsID
choiceData_inc150_200_50k <- choiceData_inc150_200_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_inc150_200_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model (rebate 0wks govt)

m1_inc150_200_50k <- logitr::logitr(
  data = choiceData_inc150_200_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_inc150_200_50k)
save(m1_inc150_200_50k, file = here::here('models', 'model_comb_inc_150-200_m1_50k.RData'))


#200-250
choiceData_inc200_250_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income == "inc_200to250") 
length(unique(choiceData_inc200_250_50k$session))

#fix id and obsID
choiceData_inc200_250_50k <- choiceData_inc200_250_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_inc200_250_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model (rebate 0wks govt)

m1_inc200_250_50k <- logitr::logitr(
  data = choiceData_inc200_250_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_inc200_250_50k)
save(m1_inc200_250_50k, file = here::here('models', 'model_comb_inc_200-250_m1_50k.RData'))


#250-300
choiceData_inc250_300_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, income),
    by = "session") %>%
  filter(income == "inc_250to300") 
length(unique(choiceData_inc250_300_50k$session))

#fix id and obsID
choiceData_inc250_300_50k <- choiceData_inc250_300_50k %>%
  mutate(
    id = rep(seq(1:length(unique(choiceData_inc250_300_50k$session))), each = 40),
    obsID = rep(seq(n() / 4), each = 4))

#m1 model (rebate 0wks govt)

m1_inc250_300_50k <- logitr::logitr(
  data = choiceData_inc250_300_50k, 
  outcome = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  clusterID = 'id',
  numMultiStarts = 30,
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_inc250_300_50k)
save(m1_inc250_300_50k, file = here::here('models', 'model_comb_inc_250-300_m1_50k.RData'))

#3 - Consider PEV

#Prob/Def Would

#prep/filter choiceData_comb
choiceData_yesEV_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, considerPHEV, considerBEV),
    by = "session") %>%
  filter(considerPHEV == "definitelyYes" | considerPHEV == "probablyYes" | considerBEV == "definitelyYes" | considerBEV == "probablyYes")
length(unique(choiceData_yesEV_50k$session))

#m1 model low (rebate 0wks govt)

m1_yesEV_50k <- logitr::logitr(
  data = choiceData_yesEV_50k, 
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

summary(m1_yesEV_50k)
save(m1_yesEV_50k, file = here::here('models', 'model_comb_yesEV_m1_50k.RData'))

#Maybe/Would not

#prep/filter choiceData_comb
choiceData_noEV_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, considerPHEV, considerBEV),
    by = "session") %>%
  filter(considerPHEV != "definitelyYes" & considerPHEV != "probablyYes" & considerBEV != "definitelyYes" & considerBEV != "probablyYes")
length(unique(choiceData_noEV_50k$session))

#m1 model low (rebate 0wks govt)

m1_noEV_50k <- logitr::logitr(
  data = choiceData_noEV_50k, 
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

summary(m1_noEV_50k)
save(m1_noEV_50k, file = here::here('models', 'model_comb_noEV_m1_50k.RData'))


#4 - Knowledge - Sub

#Correct

#prep/filter choiceData_comb
choiceData_knowSubyes_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, knowledgeSub),
    by = "session") %>%
  filter(knowledgeSub == 7500)
length(unique(choiceData_knowSubyes_50k$session))

#m1 model low (rebate 0wks govt)

m1_knowSubyes_50k <- logitr::logitr(
  data = choiceData_knowSubyes_50k, 
  choice = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_knowSubyes_50k)
save(m1_knowSubyes_50k, file = here::here('models', 'model_comb_knowSubyes_m1_50k.RData'))

#Incorrect

#prep/filter choiceData_comb
choiceData_knowSubno_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, knowledgeSub),
    by = "session") %>%
  filter(knowledgeSub != 7500)
length(unique(choiceData_knowSubno_50k$session))

#m1 model low (rebate 0wks govt)

m1_knowSubno_50k <- logitr::logitr(
  data = choiceData_knowSubno_50k, 
  choice = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_knowSubno_50k)
save(m1_knowSubno_50k, file = here::here('models', 'model_comb_knowSubno_m1_50k.RData'))


#4a - Knowledge - Fuel Gas

#Correct

#prep/filter choiceData_comb
choiceData_knowFuelGasyes_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, knowledgeFuelGas),
    by = "session") %>%
  filter(knowledgeFuelGas == "hev, phev")
length(unique(choiceData_knowFuelGasyes_50k$session))

#m1 model low (rebate 0wks govt)

m1_knowFuelGasyes_50k <- logitr::logitr(
  data = choiceData_knowFuelGasyes_50k, 
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

summary(m1_knowFuelGasyes_50k)
save(m1_knowFuelGasyes_50k, file = here::here('models', 'model_comb_knowFuelGasyes_m1_50k.RData'))

#Incorrect

#prep/filter choiceData_comb
choiceData_knowFuelGasno_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, knowledgeFuelGas),
    by = "session") %>%
  filter(knowledgeFuelGas != "hev, phev")
length(unique(choiceData_knowFuelGasno_50k$session))

#m1 model low (rebate 0wks govt)

m1_knowFuelGasno_50k <- logitr::logitr(
  data = choiceData_knowFuelGasno_50k, 
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

summary(m1_knowFuelGasno_50k)
save(m1_knowFuelGasno_50k, file = here::here('models', 'model_comb_knowFuelGasno_m1_50k.RData'))

#4b - Knowledge - Fuel Electric

#Correct

#prep/filter choiceData_comb
choiceData_knowFuelElecyes_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, knowledgeFuelElec),
    by = "session") %>%
  filter(knowledgeFuelElec == "phev, bev")
length(unique(choiceData_knowFuelElecyes_50k$session))

#m1 model low (rebate 0wks govt)

m1_knowFuelElecyes_50k <- logitr::logitr(
  data = choiceData_knowFuelElecyes_50k, 
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

summary(m1_knowFuelElecyes_50k)
save(m1_knowFuelElecyes_50k, file = here::here('models', 'model_comb_knowFuelElecyes_m1_50k.RData'))

#Incorrect

#prep/filter choiceData_comb
choiceData_knowFuelElecno_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, knowledgeFuelElec),
    by = "session") %>%
  filter(knowledgeFuelElec != "phev, bev")
length(unique(choiceData_knowFuelElecno_50k$session))

#m1 model low (rebate 0wks govt)

m1_knowFuelElecno_50k <- logitr::logitr(
  data = choiceData_knowFuelElecno_50k, 
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

summary(m1_knowFuelElecno_50k)
save(m1_knowFuelElecno_50k, file = here::here('models', 'model_comb_knowFuelElecno_m1_50k.RData'))


#5 - Neighbor EV

#Yes Neighbor has EV

#prep/filter choiceData_comb
choiceData_neighborEVyes_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, neighborEV),
    by = "session") %>%
  filter(neighborEV == 1)
length(unique(choiceData_neighborEVyes_50k$session))

#m1 model low (rebate 0wks govt)

m1_neighborEVyes_50k <- logitr::logitr(
  data = choiceData_neighborEVyes_50k, 
  choice = 'choice',
  obsID = 'obsID',
  price = 'amount',
  modelSpace = 'wtp',
  pars = c(
    'type_salesTax', 'type_taxCredit', 'type_taxDeduction',
    'timing_taxCredit_immediate',
    'timing_rebate_2', 'timing_rebate_6',
    'source_rebate_oem', 'source_rebate_dealer')
)

summary(m1_neighborEVyes_50k)
save(m1_neighborEVyes_50k, file = here::here('models', 'model_comb_neighborEVyes_m1_50k.RData'))

#No neighbor with EV

#prep/filter choiceData_comb
choiceData_neighborEVno_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, neighborEV),
    by = "session") %>%
  filter(neighborEV != 1)
length(unique(choiceData_neighborEVno_50k$session))

#m1 model low (rebate 0wks govt)

m1_neighborEVno_50k <- logitr::logitr(
  data = choiceData_neighborEVno_50k, 
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

summary(m1_neighborEVno_50k)
save(m1_neighborEVno_50k, file = here::here('models', 'model_comb_neighborEVno_m1_50k.RData'))

#Rural / non Rural
#RUral

#prep/filter choiceData_comb
choiceData_rural_50k <- choiceData_comb_50k %>% 
  left_join(
    zips_comb_50k %>% 
      select(session, rural),
    by = "session") %>%
  filter(rural == 1)
length(unique(choiceData_rural_50k$session))

#m1 model low (rebate 0wks govt)

m1_rural_50k <- logitr::logitr(
  data = choiceData_rural_50k, 
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

summary(m1_rural_50k)
save(m1_rural_50k, file = here::here('models', 'model_comb_rural_m1_50k.RData'))

#Non rural

#prep/filter choiceData_comb
choiceData_nonrural_50k <- choiceData_comb_50k %>% 
  left_join(
    zips_comb_50k %>% 
      select(session, rural),
    by = "session") %>%
  filter(rural == 0)
length(unique(choiceData_nonrural_50k$session))

#m1 model low (rebate 0wks govt)

m1_nonrural_50k <- logitr::logitr(
  data = choiceData_nonrural_50k, 
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

summary(m1_nonrural_50k)
save(m1_nonrural_50k, file = here::here('models', 'model_comb_nonrural_m1_50k.RData'))


#Household vehicles

#<2

#prep/filter choiceData_comb
choiceData_lessthan2veh_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, householdVehicles),
    by = "session") %>%
  filter(householdVehicles < 2)
length(unique(choiceData_lessthan2veh_50k$session))

#m1 model low (rebate 0wks govt)

m1_lessthan2veh_50k <- logitr::logitr(
  data = choiceData_lessthan2veh_50k, 
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

summary(m1_lessthan2veh_50k)
save(m1_lessthan2veh_50k, file = here::here('models', 'model_comb_lessthan2veh_m1_50k.RData'))

#>2 household veh

#prep/filter choiceData_comb
choiceData_morethan2veh_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, householdVehicles),
    by = "session") %>%
  filter(householdVehicles >= 2)
length(unique(choiceData_morethan2veh_50k$session))

#m1 model low (rebate 0wks govt)

m1_morethan2veh_50k <- logitr::logitr(
  data = choiceData_morethan2veh_50k, 
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

summary(m1_morethan2veh_50k)
save(m1_morethan2veh_50k, file = here::here('models', 'model_comb_morethan2veh_m1_50k.RData'))


#Gender

#male
#prep/filter choiceData_comb
choiceData_male_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, gender),
    by = "session") %>%
  filter(gender == "male")
length(unique(choiceData_male_50k$session))

#m1 model low (rebate 0wks govt)

m1_male_50k <- logitr::logitr(
  data = choiceData_male_50k, 
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

summary(m1_male_50k)
save(m1_male_50k, file = here::here('models', 'model_comb_male_m1_50k.RData'))

#female
#prep/filter choiceData_comb
choiceData_female_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, gender),
    by = "session") %>%
  filter(gender == "female")
length(unique(choiceData_female_50k$session))

#m1 model low (rebate 0wks govt)

m1_female_50k <- logitr::logitr(
  data = choiceData_female_50k, 
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

summary(m1_female_50k)
save(m1_female_50k, file = here::here('models', 'model_comb_female_m1_50k.RData'))

#non-binary
#prep/filter choiceData_comb
choiceData_nonbinary_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, gender),
    by = "session") %>%
  filter(gender == "other")
length(unique(choiceData_nonbinary_50k$session))

#m1 model low (rebate 0wks govt)

m1_nonbinary_50k <- logitr::logitr(
  data = choiceData_nonbinary_50k, 
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

summary(m1_nonbinary_50k)
save(m1_nonbinary_50k, file = here::here('models', 'model_comb_nonbinary_m1_50k.RData'))


#parking for household

#home parking
#prep/filter choiceData_comb
choiceData_homepark_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, parkingHousehold) %>%
      mutate(
        home_parking = ifelse(str_detect(parkingHousehold, "SFH"),1,
                                         ifelse(str_detect(parkingHousehold, "driveway"), 1,0))
      ),
    by = "session") %>%
  filter(home_parking == 1)
length(unique(choiceData_homepark_50k$session))

#m1 model (rebate 0wks govt)

m1_homepark_50k <- logitr::logitr(
  data = choiceData_homepark_50k, 
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

summary(m1_homepark_50k)
save(m1_homepark_50k, file = here::here('models', 'model_comb_homepark_m1_50k.RData'))

#NO home parking
#prep/filter choiceData_comb
choiceData_homeparkNo_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, parkingHousehold) %>%
      mutate(
        home_parking = ifelse(str_detect(parkingHousehold, "SFH"),1,
                              ifelse(str_detect(parkingHousehold, "driveway"), 1,0))
      ),
    by = "session") %>%
  filter(home_parking != 1)
length(unique(choiceData_homeparkNo_50k$session))

#m1 model (rebate 0wks govt)

m1_homeparkNo_50k <- logitr::logitr(
  data = choiceData_homeparkNo_50k, 
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

summary(m1_homeparkNo_50k)
save(m1_homeparkNo_50k, file = here::here('models', 'model_comb_homeparkNo_m1_50k.RData'))




#COVID employment

#Yes employment status changed
#prep/filter choiceData_comb
choiceData_COVIDyes_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, covidEmployment),
    by = "session") %>%
  filter(covidEmployment == "yes")
length(unique(choiceData_COVIDyes_50k$session))

#m1 model (rebate 0wks govt)

m1_COVIDyes_50k <- logitr::logitr(
  data = choiceData_COVIDyes_50k, 
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

summary(m1_COVIDyes_50k)
save(m1_COVIDyes_50k, file = here::here('models', 'model_comb_COVIDyes_m1_50k.RData'))

#No employment status changed
#prep/filter choiceData_comb
choiceData_COVIDno_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, covidEmployment),
    by = "session") %>%
  filter(covidEmployment == "no")
length(unique(choiceData_COVIDno_50k$session))

#m1 model (rebate 0wks govt)

m1_COVIDno_50k <- logitr::logitr(
  data = choiceData_COVIDno_50k, 
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

summary(m1_COVIDno_50k)
save(m1_COVIDno_50k, file = here::here('models', 'model_comb_COVIDno_m1_50k.RData'))

#Political

#Conservative & very conservative

#prep/filter choiceData_comb
choiceData_conservative_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, political),
    by = "session") %>%
  filter(political == "conservative" | political == "very_conservative")
length(unique(choiceData_conservative_50k$session))

#m1 model (rebate 0wks govt)

m1_conservative_50k <- logitr::logitr(
  data = choiceData_conservative_50k, 
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

summary(m1_conservative_50k)
save(m1_conservative_50k, file = here::here('models', 'model_comb_conservative_m1_50k.RData'))

#Liberal & very liberal

#prep/filter choiceData_comb
choiceData_liberal_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, political),
    by = "session") %>%
  filter(political == "liberal" | political == "very_liberal")
length(unique(choiceData_liberal_50k$session))

#m1 model (rebate 0wks govt)

m1_liberal_50k <- logitr::logitr(
  data = choiceData_liberal_50k, 
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

summary(m1_liberal_50k)
save(m1_liberal_50k, file = here::here('models', 'model_comb_liberal_m1_50k.RData'))

#moderate

#prep/filter choiceData_comb
choiceData_moderate_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, political),
    by = "session") %>%
  filter(political == "moderate")
length(unique(choiceData_moderate_50k$session))

#m1 model (rebate 0wks govt)

m1_moderate_50k <- logitr::logitr(
  data = choiceData_moderate_50k, 
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

summary(m1_moderate_50k)
save(m1_moderate_50k, file = here::here('models', 'model_comb_moderate_m1_50k.RData'))


#car Type 

#hatch
#prep/filter choiceData_comb
choiceData_hatch_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, carType) %>%
      mutate(
        hatch = ifelse(str_detect(carType, "hatch"),1,0)
      ),
    by = "session") %>%
  filter(hatch == 1)
length(unique(choiceData_hatch_50k$session))

#m1 model (rebate 0wks govt)

m1_hatch_50k <- logitr::logitr(
  data = choiceData_hatch_50k, 
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

summary(m1_hatch_50k)
save(m1_hatch_50k, file = here::here('models', 'model_comb_hatch_m1_50k.RData'))

#sedan
#prep/filter choiceData_comb
choiceData_sedan_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, carType) %>%
      mutate(
        sedan = ifelse(str_detect(carType, "sedan"),1,0)
      ),
    by = "session") %>%
  filter(sedan == 1)
length(unique(choiceData_sedan_50k$session))

#m1 model (rebate 0wks govt)

m1_sedan_50k <- logitr::logitr(
  data = choiceData_sedan_50k, 
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

summary(m1_sedan_50k)
save(m1_sedan_50k, file = here::here('models', 'model_comb_sedan_m1_50k.RData'))

#SUV
#prep/filter choiceData_comb
choiceData_SUV_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, carType) %>%
      mutate(
        suv = ifelse(str_detect(carType, "suv"),1,0)
      ),
    by = "session") %>%
  filter(suv == 1)
length(unique(choiceData_SUV_50k$session))

#m1 model (rebate 0wks govt)

m1_SUV_50k <- logitr::logitr(
  data = choiceData_SUV_50k, 
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

summary(m1_SUV_50k)
save(m1_SUV_50k, file = here::here('models', 'model_comb_SUV_m1_50k.RData'))

#truck
#prep/filter choiceData_comb
choiceData_truck_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, carType) %>%
      mutate(
        truck = ifelse(str_detect(carType, "truck"),1,0)
      ),
    by = "session") %>%
  filter(truck == 1)
length(unique(choiceData_truck_50k$session))

#m1 model (rebate 0wks govt)

m1_truck_50k <- logitr::logitr(
  data = choiceData_truck_50k, 
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

summary(m1_truck_50k)
save(m1_truck_50k, file = here::here('models', 'model_comb_truck_m1_50k.RData'))

#van
#prep/filter choiceData_comb
choiceData_van_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, carType) %>%
      mutate(
        van = ifelse(str_detect(carType, "van"),1,0)
      ),
    by = "session") %>%
  filter(van == 1)
length(unique(choiceData_van_50k$session))

#m1 model (rebate 0wks govt)

m1_van_50k <- logitr::logitr(
  data = choiceData_van_50k, 
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

summary(m1_van_50k)
save(m1_van_50k, file = here::here('models', 'model_comb_van_m1_50k.RData'))


#current veh type

#anything electrified
#prep/filter choiceData_comb
choiceData_currentVehElec_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, currentVehType) %>%
      mutate(
        currentVehElec = ifelse(str_detect(currentVehType, "Electric"),1,
                                ifelse(str_detect(currentVehType, "Plug-In Hybrid"),1,
                                       ifelse(str_detect(currentVehType, "Hybrid"),1,0)))
      ),
    by = "session") %>%
  filter(currentVehElec == 1)
length(unique(choiceData_currentVehElec_50k$session))

#m1 model (rebate 0wks govt)

m1_currentVehElec_50k <- logitr::logitr(
  data = choiceData_currentVehElec_50k, 
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

summary(m1_currentVehElec_50k)
save(m1_currentVehElec_50k, file = here::here('models', 'model_comb_currentVehElec_m1_50k.RData'))

#gas only
#prep/filter choiceData_comb
choiceData_currentVehGas_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, currentVehType) %>%
      mutate(
        currentVehElec = ifelse(str_detect(currentVehType, "Electric"),1,
                                ifelse(str_detect(currentVehType, "Plug-In Hybrid"),1,
                                       ifelse(str_detect(currentVehType, "Hybrid"),1,0)))
      ),
    by = "session") %>%
  filter(currentVehElec != 1)
length(unique(choiceData_currentVehGas_50k$session))

#m1 model (rebate 0wks govt)

m1_currentVehGas_50k <- logitr::logitr(
  data = choiceData_currentVehGas_50k, 
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

summary(m1_currentVehGas_50k)
save(m1_currentVehGas_50k, file = here::here('models', 'model_comb_currentVehGas_m1_50k.RData'))


#Ethinicity

#identify as hispanic 
#prep/filter choiceData_comb
choiceData_hispanic_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, ethnicity) %>%
      mutate(
        hispanic = ifelse(str_detect(ethnicity, "hispanic"),1,0)
      ),
    by = "session") %>%
  filter(hispanic == 1)
length(unique(choiceData_hispanic_50k$session))

#m1 model (rebate 0wks govt)

m1_hispanic_50k <- logitr::logitr(
  data = choiceData_hispanic_50k, 
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

summary(m1_hispanic_50k)
save(m1_hispanic_50k, file = here::here('models', 'model_comb_hispanic_m1_50k.RData'))

#identify as asian
#prep/filter choiceData_comb
choiceData_asian_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, ethnicity) %>%
      mutate(
        asian = ifelse(str_detect(ethnicity, "asian"),1,0)
      ),
    by = "session") %>%
  filter(asian == 1)
length(unique(choiceData_asian_50k$session))

#m1 model (rebate 0wks govt)

m1_asian_50k <- logitr::logitr(
  data = choiceData_asian_50k, 
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

summary(m1_asian_50k)
save(m1_asian_50k, file = here::here('models', 'model_comb_asian_m1_50k.RData'))

#identify as black 
#prep/filter choiceData_comb
choiceData_black_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, ethnicity) %>%
      mutate(
        black = ifelse(str_detect(ethnicity, "black"),1,0)
      ),
    by = "session") %>%
  filter(black == 1)
length(unique(choiceData_black_50k$session))

#m1 model (rebate 0wks govt)

m1_black_50k <- logitr::logitr(
  data = choiceData_black_50k, 
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

summary(m1_black_50k)
save(m1_black_50k, file = here::here('models', 'model_comb_black_m1_50k.RData'))

#identify as native
#prep/filter choiceData_comb
choiceData_native_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, ethnicity) %>%
      mutate(
        native = ifelse(str_detect(ethnicity, "native"),1,0)
      ),
    by = "session") %>%
  filter(native == 1)
length(unique(choiceData_native_50k$session))

#m1 model (rebate 0wks govt)

m1_native_50k <- logitr::logitr(
  data = choiceData_native_50k, 
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

summary(m1_native_50k)
save(m1_native_50k, file = here::here('models', 'model_comb_native_m1_50k.RData'))

#identify as pacific
#prep/filter choiceData_comb
choiceData_pacific_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, ethnicity) %>%
      mutate(
        pacific = ifelse(str_detect(ethnicity, "pacific"),1,0)
      ),
    by = "session") %>%
  filter(pacific == 1)
length(unique(choiceData_pacific_50k$session))

#m1 model (rebate 0wks govt)

m1_pacific_50k <- logitr::logitr(
  data = choiceData_pacific_50k, 
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

summary(m1_pacific_50k)
save(m1_pacific_50k, file = here::here('models', 'model_comb_pacific_m1_50k.RData'))

#identify as white
#prep/filter choiceData_comb
choiceData_white_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, ethnicity) %>%
      mutate(
        white = ifelse(str_detect(ethnicity, "white"),1,0)
      ),
    by = "session") %>%
  filter(white == 1)
length(unique(choiceData_white_50k$session))

#m1 model (rebate 0wks govt)

m1_white_50k <- logitr::logitr(
  data = choiceData_white_50k, 
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

summary(m1_white_50k)
save(m1_white_50k, file = here::here('models', 'model_comb_white_m1_50k.RData'))


#education

#no HS
#prep/filter choiceData_comb
choiceData_no_hs_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, education),
    by = "session") %>%
  filter(education == "no_hs")
length(unique(choiceData_no_hs_50k$session))

#m1 model (rebate 0wks govt)

m1_no_hs_50k <- logitr::logitr(
  data = choiceData_no_hs_50k, 
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

summary(m1_no_hs_50k)
save(m1_no_hs_50k, file = here::here('models', 'model_comb_no_hs_m1_50k.RData'))

#HS
#prep/filter choiceData_comb
choiceData_hs_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, education),
    by = "session") %>%
  filter(education == "hs")
length(unique(choiceData_hs_50k$session))

#m1 model (rebate 0wks govt)

m1_hs_50k <- logitr::logitr(
  data = choiceData_hs_50k, 
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

summary(m1_hs_50k)
save(m1_hs_50k, file = here::here('models', 'model_comb_hs_m1_50k.RData'))

#some college
#prep/filter choiceData_comb
choiceData_some_college_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, education),
    by = "session") %>%
  filter(education == "college_some")
length(unique(choiceData_some_college_50k$session))

#m1 model (rebate 0wks govt)

m1_some_college_50k <- logitr::logitr(
  data = choiceData_some_college_50k, 
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

summary(m1_some_college_50k)
save(m1_some_college_50k, file = here::here('models', 'model_comb_some_college_m1_50k.RData'))

#vocational
#prep/filter choiceData_comb
choiceData_vocational_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, education),
    by = "session") %>%
  filter(education == "vocational")
length(unique(choiceData_vocational_50k$session))

#m1 model (rebate 0wks govt)

m1_vocational_50k <- logitr::logitr(
  data = choiceData_vocational_50k, 
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

summary(m1_vocational_50k)
save(m1_vocational_50k, file = here::here('models', 'model_comb_vocational_m1_50k.RData'))

#associates degree
#prep/filter choiceData_comb
choiceData_associates_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, education),
    by = "session") %>%
  filter(education == "degree_associate")
length(unique(choiceData_associates_50k$session))

#m1 model (rebate 0wks govt)

m1_associates_50k <- logitr::logitr(
  data = choiceData_associates_50k, 
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

summary(m1_associates_50k)
save(m1_associates_50k, file = here::here('models', 'model_comb_associates_m1_50k.RData'))

#bachelors degree
#prep/filter choiceData_comb
choiceData_bs_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, education),
    by = "session") %>%
  filter(education == "degree_bs")
length(unique(choiceData_bs_50k$session))

#m1 model (rebate 0wks govt)

m1_bs_50k <- logitr::logitr(
  data = choiceData_bs_50k, 
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

summary(m1_bs_50k)
save(m1_bs_50k, file = here::here('models', 'model_comb_bs_m1_50k.RData'))

#masters degree
#prep/filter choiceData_comb
choiceData_ms_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, education),
    by = "session") %>%
  filter(education == "degree_ms")
length(unique(choiceData_ms_50k$session))

#m1 model (rebate 0wks govt)

m1_ms_50k <- logitr::logitr(
  data = choiceData_ms_50k, 
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

summary(m1_ms_50k)
save(m1_ms_50k, file = here::here('models', 'model_comb_ms_m1_50k.RData'))

#Professional degree
#prep/filter choiceData_comb
choiceData_md_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, education),
    by = "session") %>%
  filter(education == "degree_md")
length(unique(choiceData_md_50k$session))

#m1 model (rebate 0wks govt)

m1_md_50k <- logitr::logitr(
  data = choiceData_md_50k, 
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

summary(m1_md_50k)
save(m1_md_50k, file = here::here('models', 'model_comb_md_m1_50k.RData'))

#PhD degree
#prep/filter choiceData_comb
choiceData_phd_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, education),
    by = "session") %>%
  filter(education == "degree_phd")
length(unique(choiceData_phd_50k$session))

#m1 model (rebate 0wks govt)

m1_phd_50k <- logitr::logitr(
  data = choiceData_phd_50k, 
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

summary(m1_phd_50k)
save(m1_phd_50k, file = here::here('models', 'model_comb_phd_m1_50k.RData'))


#Work

#student
#prep/filter choiceData_comb
choiceData_student_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, work),
    by = "session") %>%
  filter(work == "student")
length(unique(choiceData_student_50k$session))

#m1 model (rebate 0wks govt)

m1_student_50k <- logitr::logitr(
  data = choiceData_student_50k, 
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

summary(m1_student_50k)
save(m1_student_50k, file = here::here('models', 'model_comb_student_m1_50k.RData'))

#employed <40 hours per week
#prep/filter choiceData_comb
choiceData_parttime_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, work),
    by = "session") %>%
  filter(work == "employed_under40")
length(unique(choiceData_parttime_50k$session))

#m1 model (rebate 0wks govt)

m1_parttime_50k <- logitr::logitr(
  data = choiceData_parttime_50k, 
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

summary(m1_parttime_50k)
save(m1_parttime_50k, file = here::here('models', 'model_comb_parttime_m1_50k.RData'))

#employed >= 40 hours per week
#prep/filter choiceData_comb
choiceData_fulltime_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, work),
    by = "session") %>%
  filter(work == "employed_over40")
length(unique(choiceData_fulltime_50k$session))

#m1 model (rebate 0wks govt)

m1_fulltime_50k <- logitr::logitr(
  data = choiceData_fulltime_50k, 
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

summary(m1_fulltime_50k)
save(m1_fulltime_50k, file = here::here('models', 'model_comb_fulltime_m1_50k.RData'))

#Not employed, looking
#prep/filter choiceData_comb
choiceData_not_employed_looking_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, work),
    by = "session") %>%
  filter(work == "not_employed_yes_looking")
length(unique(choiceData_not_employed_looking_50k$session))

#m1 model (rebate 0wks govt)

m1_not_employed_looking_50k <- logitr::logitr(
  data = choiceData_not_employed_looking_50k, 
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

summary(m1_not_employed_looking_50k)
save(m1_not_employed_looking_50k, file = here::here('models', 'model_comb_not_employed_looking_m1_50k.RData'))

#Not employed, not looking
#prep/filter choiceData_comb
choiceData_not_employed_not_looking_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, work),
    by = "session") %>%
  filter(work == "not_employed_not_looking")
length(unique(choiceData_not_employed_not_looking_50k$session))

#m1 model (rebate 0wks govt)

m1_not_employed_not_looking_50k <- logitr::logitr(
  data = choiceData_not_employed_not_looking_50k, 
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

summary(m1_not_employed_not_looking_50k)
save(m1_not_employed_not_looking_50k, file = here::here('models', 'model_comb_not_employed_not_looking_m1_50k.RData'))

# Retired
#prep/filter choiceData_comb
choiceData_retired_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, work),
    by = "session") %>%
  filter(work == "retired")
length(unique(choiceData_retired_50k$session))

#m1 model (rebate 0wks govt)

m1_retired_50k <- logitr::logitr(
  data = choiceData_retired_50k, 
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

summary(m1_retired_50k)
save(m1_retired_50k, file = here::here('models', 'model_comb_retired_m1_50k.RData'))

# Disabled
#prep/filter choiceData_comb
choiceData_disabled_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, work),
    by = "session") %>%
  filter(work == "disabled")
length(unique(choiceData_disabled_50k$session))

#m1 model (rebate 0wks govt)

m1_disabled_50k <- logitr::logitr(
  data = choiceData_disabled_50k, 
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

summary(m1_disabled_50k)
save(m1_disabled_50k, file = here::here('models', 'model_comb_disabled_m1_50k.RData'))


#Housing type

# Mobile home
#prep/filter choiceData_comb
choiceData_mobile_home_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, housingType),
    by = "session") %>%
  filter(housingType == "mobile")
length(unique(choiceData_mobile_home_50k$session))

#m1 model (rebate 0wks govt)

m1_mobile_home_50k <- logitr::logitr(
  data = choiceData_mobile_home_50k, 
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

summary(m1_mobile_home_50k)
save(m1_mobile_home_50k, file = here::here('models', 'model_comb_mobile_home_m1_50k.RData'))

# Apartment
#prep/filter choiceData_comb
choiceData_apartment_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, housingType),
    by = "session") %>%
  filter(housingType == "apartment")
length(unique(choiceData_apartment_50k$session))

#m1 model (rebate 0wks govt)

m1_apartment_50k <- logitr::logitr(
  data = choiceData_apartment_50k, 
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

summary(m1_apartment_50k)
save(m1_apartment_50k, file = here::here('models', 'model_comb_apartment_m1_50k.RData'))

# Townhome
#prep/filter choiceData_comb
choiceData_townhome_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, housingType),
    by = "session") %>%
  filter(housingType == "townhome")
length(unique(choiceData_townhome_50k$session))

#m1 model (rebate 0wks govt)

m1_townhome_50k <- logitr::logitr(
  data = choiceData_townhome_50k, 
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

summary(m1_townhome_50k)
save(m1_townhome_50k, file = here::here('models', 'model_comb_townhome_m1_50k.RData'))

# condo
#prep/filter choiceData_comb
choiceData_condo_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, housingType),
    by = "session") %>%
  filter(housingType == "condo")
length(unique(choiceData_condo_50k$session))

#m1 model (rebate 0wks govt)

m1_condo_50k <- logitr::logitr(
  data = choiceData_condo_50k, 
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

summary(m1_condo_50k)
save(m1_condo_50k, file = here::here('models', 'model_comb_condo_m1_50k.RData'))

# SFH
#prep/filter choiceData_comb
choiceData_SFH_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, housingType),
    by = "session") %>%
  filter(housingType == "detached")
length(unique(choiceData_SFH_50k$session))

#m1 model (rebate 0wks govt)

m1_SFH_50k <- logitr::logitr(
  data = choiceData_SFH_50k, 
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

summary(m1_SFH_50k)
save(m1_SFH_50k, file = here::here('models', 'model_comb_SFH_m1_50k.RData'))


#Housing Ownership

# Rent
#prep/filter choiceData_comb
choiceData_rent_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, housingOwner),
    by = "session") %>%
  filter(housingOwner == "rent")
length(unique(choiceData_rent_50k$session))

#m1 model (rebate 0wks govt)

m1_rent_50k <- logitr::logitr(
  data = choiceData_rent_50k, 
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

summary(m1_rent_50k)
save(m1_rent_50k, file = here::here('models', 'model_comb_rent_m1_50k.RData'))

# Own
#prep/filter choiceData_comb
choiceData_own_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, housingOwner),
    by = "session") %>%
  filter(housingOwner == "own")
length(unique(choiceData_own_50k$session))

#m1 model (rebate 0wks govt)

m1_own_50k <- logitr::logitr(
  data = choiceData_own_50k, 
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

summary(m1_own_50k)
save(m1_own_50k, file = here::here('models', 'model_comb_own_m1_50k.RData'))


#Household Size

# 1
#prep/filter choiceData_comb
choiceData_HHone_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, householdSize),
    by = "session") %>%
  filter(householdSize == 1)
length(unique(choiceData_HHone_50k$session))

#m1 model (rebate 0wks govt)

m1_HHone_50k <- logitr::logitr(
  data = choiceData_HHone_50k, 
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

summary(m1_HHone_50k)
save(m1_HHone_50k, file = here::here('models', 'model_comb_HHone_m1_50k.RData'))

# 2
#prep/filter choiceData_comb
choiceData_HHtwo_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, householdSize),
    by = "session") %>%
  filter(householdSize == 2)
length(unique(choiceData_HHtwo_50k$session))

#m1 model (rebate 0wks govt)

m1_HHtwo_50k <- logitr::logitr(
  data = choiceData_HHtwo_50k, 
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

summary(m1_HHtwo_50k)
save(m1_HHtwo_50k, file = here::here('models', 'model_comb_HHtwo_m1_50k.RData'))

# 3
#prep/filter choiceData_comb
choiceData_HHthree_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, householdSize),
    by = "session") %>%
  filter(householdSize == 3)
length(unique(choiceData_HHthree_50k$session))

#m1 model (rebate 0wks govt)

m1_HHthree_50k <- logitr::logitr(
  data = choiceData_HHthree_50k, 
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

summary(m1_HHthree_50k)
save(m1_HHthree_50k, file = here::here('models', 'model_comb_HHthree_m1_50k.RData'))

# 4
#prep/filter choiceData_comb
choiceData_HHfour_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, householdSize),
    by = "session") %>%
  filter(householdSize == 4)
length(unique(choiceData_HHfour_50k$session))

#m1 model (rebate 0wks govt)

m1_HHfour_50k <- logitr::logitr(
  data = choiceData_HHfour_50k, 
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

summary(m1_HHfour_50k)
save(m1_HHfour_50k, file = here::here('models', 'model_comb_HHfour_m1_50k.RData'))

# 5 or more
#prep/filter choiceData_comb
choiceData_HHfive_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, householdSize),
    by = "session") %>%
  filter(householdSize == "5 or more")
length(unique(choiceData_HHfive_50k$session))

#m1 model (rebate 0wks govt)

m1_HHfive_50k <- logitr::logitr(
  data = choiceData_HHfive_50k, 
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

summary(m1_HHfive_50k)
save(m1_HHfive_50k, file = here::here('models', 'model_comb_HHfive_m1_50k.RData'))


#Year of birth/generations per Pew research
#https://www.pewresearch.org/fact-tank/2019/01/17/where-millennials-end-and-generation-z-begins/

# Silent generation
#prep/filter choiceData_comb
choiceData_gen_silent_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, yearOfBirth),
    by = "session") %>%
  filter(between(yearOfBirth, 1928, 1945))
length(unique(choiceData_gen_silent_50k$session))

#m1 model (rebate 0wks govt)

m1_gen_silent_50k <- logitr::logitr(
  data = choiceData_gen_silent_50k, 
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

summary(m1_gen_silent_50k)
save(m1_gen_silent_50k, file = here::here('models', 'model_comb_gen_silent_m1_50k.RData'))

# Boomers generation
#prep/filter choiceData_comb
choiceData_gen_boomer_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, yearOfBirth),
    by = "session") %>%
  filter(between(yearOfBirth, 1946, 1964))
length(unique(choiceData_gen_boomer_50k$session))

#m1 model (rebate 0wks govt)

m1_gen_boomer_50k <- logitr::logitr(
  data = choiceData_gen_boomer_50k, 
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

summary(m1_gen_boomer_50k)
save(m1_gen_boomer_50k, file = here::here('models', 'model_comb_gen_boomer_m1_50k.RData'))

# X generation
#prep/filter choiceData_comb
choiceData_gen_x_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, yearOfBirth),
    by = "session") %>%
  filter(between(yearOfBirth, 1965, 1980))
length(unique(choiceData_gen_x_50k$session))

#m1 model (rebate 0wks govt)

m1_gen_x_50k <- logitr::logitr(
  data = choiceData_gen_x_50k, 
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

summary(m1_gen_x_50k)
save(m1_gen_x_50k, file = here::here('models', 'model_comb_gen_x_m1_50k.RData'))

# Millennials generation
#prep/filter choiceData_comb
choiceData_gen_mill_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, yearOfBirth),
    by = "session") %>%
  filter(between(yearOfBirth, 1981, 1996))
length(unique(choiceData_gen_mill_50k$session))

#m1 model (rebate 0wks govt)

m1_gen_mill_50k <- logitr::logitr(
  data = choiceData_gen_mill_50k, 
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

summary(m1_gen_mill_50k)
save(m1_gen_mill_50k, file = here::here('models', 'model_comb_gen_mill_m1_50k.RData'))

# Z generation
#prep/filter choiceData_comb
choiceData_gen_z_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, yearOfBirth),
    by = "session") %>%
  filter(between(yearOfBirth, 1997, 2012))
length(unique(choiceData_gen_z_50k$session))

#m1 model (rebate 0wks govt)

m1_gen_z_50k <- logitr::logitr(
  data = choiceData_gen_z_50k, 
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

summary(m1_gen_z_50k)
save(m1_gen_z_50k, file = here::here('models', 'model_comb_gen_z_m1_50k.RData'))

# Millennials + Z generation
#prep/filter choiceData_comb
choiceData_gen_millandz_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, yearOfBirth),
    by = "session") %>%
  filter(between(yearOfBirth, 1981, 2012))
length(unique(choiceData_gen_millandz_50k$session))

#m1 model (rebate 0wks govt)

m1_gen_millandz_50k <- logitr::logitr(
  data = choiceData_gen_millandz_50k, 
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

summary(m1_gen_millandz_50k)
save(m1_gen_millandz_50k, file = here::here('models', 'model_comb_gen_millandz_m1_50k.RData'))


#Lease or purchase

# Purchase (option 2)
#prep/filter choiceData_comb
choiceData_purchase_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, lease_or_purchase),
    by = "session") %>%
  filter(lease_or_purchase == 2)
length(unique(choiceData_purchase_50k$session))

#m1 model (rebate 0wks govt)

m1_purchase_50k <- logitr::logitr(
  data = choiceData_purchase_50k, 
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

summary(m1_purchase_50k)
save(m1_purchase_50k, file = here::here('models', 'model_comb_purchase_m1_50k.RData'))

# Lease / Not sure (option 1 & 3)
#prep/filter choiceData_comb
choiceData_lease_50k <- choiceData_comb_50k %>% 
  left_join(
    data_comb_50k %>% 
      select(session, lease_or_purchase),
    by = "session") %>%
  filter(lease_or_purchase != 2)
length(unique(choiceData_lease_50k$session))

#m1 model (rebate 0wks govt)

m1_lease_50k <- logitr::logitr(
  data = choiceData_lease_50k, 
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

summary(m1_lease_50k)
save(m1_lease_50k, file = here::here('models', 'model_comb_lease_m1_50k.RData'))

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
