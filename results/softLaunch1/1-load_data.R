#install.packages("devtools")
#devtools::install_github("rubenarslan/formr")

library(formr)
library(tidyverse)
library(magrittr)
options(dplyr.width = Inf)
library(lubridate)
library(here)
library(janitor)


raw_data_start <- read_csv(here("data", "Vehicle_Incentive_Study_Dynata_start.csv"))
raw_data_main <- read_csv(here("data", "Vehicle_Incentive_Study_Dynata.csv"))
raw_data_demos <- read_csv(here("data", "Vehicle_Incentive_Study_Dynata_Demos.csv"))


#Format start survey
raw_data_start <- raw_data_start %>%
  #calc time to do start survey
  mutate(
    created = as_datetime(created),
    ended = as_datetime(ended),
    sec_elapsed_start = as.numeric(ended - created, units = "secs")) %>%
  #select important col from start survey
  select(session, psid, inMarket, inMarketscreenout, sec_elapsed_start)
# select(respondentNumber, inMarket, inMarketscreenout, sec_elapsed_start)

#Format main survey
raw_data_main <- raw_data_main %>%
  select(-psid)

#Format demo survey
raw_data_demos <- raw_data_demos %>%
  #calc time to do start survey
  mutate(
    created = as_datetime(created),
    ended = as_datetime(ended),
    sec_elapsed_demos = as.numeric(ended - created, units = "secs")) %>%
  #select important col from demo survey
  select(-created, -modified, -ended, -expired, -respondentID)

# Join start and main data sets

raw_data <- raw_data_start %>%
  left_join(raw_data_main, by = "session") 

#raw_data <- raw_data_start %>%
# left_join(raw_data_main, by = "respondentID") 

raw_data <- raw_data %>%
  left_join(raw_data_demos, by = "session")


#Data Cleaning--------------------------------

data <- raw_data %T>%
  {print(paste("data no filter",nrow(.)))} %>%
  # 1 - Remove duplicate session IDs
  # distinct(session, .keep_all = TRUE) %T>%
  # {print(paste("duplicate session IDs",nrow(.)))} %>%
  # 2- Remove in Market NA
  filter(!is.na(inMarketscreenout)) %T>%
  {print(paste("inMarket NA",nrow(.)))} %>%
  #filter out tests
  filter(psid != "123") %T>%
  {print(paste("test",nrow(.)))} %>%
  # 3- Remove in Market screenouts
  filter(inMarketscreenout == 0) %T>%
  {print(paste("inMarket screenout",nrow(.)))} %>%
  # filter out bad responses
  # 4 - Remove cases that did not complete the CBC questions
  filter(!is.na(cbc1)) %>% 
  filter(!is.na(cbc2)) %>% 
  filter(!is.na(cbc3)) %>% 
  filter(!is.na(cbc4)) %>% 
  filter(!is.na(cbc5)) %>% 
  filter(!is.na(cbc6)) %>% 
  filter(!is.na(cbc7)) %>% 
  filter(!is.na(cbc8)) %>% 
  filter(!is.na(cbc9)) %>%
  filter(!is.na(cbc10))  %T>%
  {print(paste("cbc NAs",nrow(.)))} %>%
  # 4- Remove if all conjoint responses were the same
  filter(cbcAllSame == 0 ) %T>%
  {print(paste("cbc screenout",nrow(.)))} %>%
  # 5- practice
  filter(practice == 4 ) %T>%
  {print(paste("practice",nrow(.)))}

write_csv(data, here::here("data_filtered.csv"))

data <- data %>%
  #calc times for survey
  mutate(
    created = as_datetime(created),
    ended = as_datetime(ended),
    systime_cbc1_start = as_datetime(systime_cbc1_start),
    systime_cbc1_end = as_datetime(systime_cbc1_end),
    systime_cbc2_end = as_datetime(systime_cbc2_end),
    systime_cbc3_end = as_datetime(systime_cbc3_end),
    systime_cbc4_end = as_datetime(systime_cbc4_end),
    systime_cbc5_end = as_datetime(systime_cbc5_end),
    systime_cbc6_end = as_datetime(systime_cbc6_end),
    systime_cbc7_end = as_datetime(systime_cbc7_end),
    systime_cbc8_end = as_datetime(systime_cbc8_end),
    systime_cbc9_end = as_datetime(systime_cbc9_end),
    systime_cbc10_end = as_datetime(systime_cbc10_end),
    sec_elapsed_main = as.numeric(ended - created, units = "secs"),
    sec_elapsed_total = as.numeric(sec_elapsed_start + sec_elapsed_main, units = "secs"),
    sec_cbc1 = as.numeric(systime_cbc1_end - systime_cbc1_start, units = "secs"),
    sec_cbc2 = as.numeric(systime_cbc2_end - systime_cbc1_end, units = "secs"),
    sec_cbc3 = as.numeric(systime_cbc3_end - systime_cbc2_end, units = "secs"),
    sec_cbc4 = as.numeric(systime_cbc4_end - systime_cbc3_end, units = "secs"),
    sec_cbc5 = as.numeric(systime_cbc5_end - systime_cbc4_end, units = "secs"),
    sec_cbc6 = as.numeric(systime_cbc6_end - systime_cbc5_end, units = "secs"),
    sec_cbc7 = as.numeric(systime_cbc7_end - systime_cbc6_end, units = "secs"),
    sec_cbc8 = as.numeric(systime_cbc8_end - systime_cbc7_end, units = "secs"),
    sec_cbc9 = as.numeric(systime_cbc9_end - systime_cbc8_end, units = "secs"),
    sec_cbc10 = as.numeric(systime_cbc10_end - systime_cbc9_end, units = "secs"),
    avg_sec_cbc = as.numeric(((sec_cbc1 + sec_cbc2 + sec_cbc3 + sec_cbc4 + sec_cbc5 + sec_cbc6 + sec_cbc7 + sec_cbc8 + sec_cbc9 + sec_cbc10)/10), units = "secs")
    )
  #add some insights (histogram of time etc.)
  

  #  4- Remove short responses (add reasoning)
  # filter(sec_elapsed_total > 120)  %T>%
  # {print(paste("survey time under 2 min",nrow(.)))}
  

# Separate out feedback
feedback <- data %>%
  filter(!is.na(feedback)) %>%
  select(feedback)

write_csv(feedback, here::here("softLaunch1_feedback.csv"))


# Merge responses with survey designs to get choiceData ----------------------

# Read in survey for choice data
survey <- read_csv(here::here("survey.csv"))
survey_raw <- survey

#change respID to respondentID to match responses data
survey <- survey %>%
  rename(respondentID = respID) %>%
  mutate(
    respondentID = as.numeric(respondentID),
    type = as.factor(type),
    type = fct_relevel(type, c("salesTax", "taxCredit", "taxDeduction", "rebate"))) %>%
  arrange(obsID, type)

# Re-number altID 
survey$altID = rep(seq(4), nrow(survey) / 4)

#separate out systime stamps
timestamps <- data %>%
  select(contains("systime"))

data <- data %>%
  select(-contains("systime"))

choiceData <- data %>%
  select(session, respondentID, maxSalestax, cbc1:cbc10) %>% 
  arrange(respondentID, session) %>% 
  mutate(id = row_number()) %>% 
  select(id, everything()) %>% 
  gather(key = "qID", value = "choice", cbc1:cbc10) %>%
  separate(qID, into = c("drop", "qID"), sep = "cbc") %>%
  mutate(qID = as.numeric(qID)) %>%
  select(-drop) %>%
  arrange(id, qID) %>% 
  left_join(
    survey %>% select(-obsID, -contains("label")), 
    by = c("respondentID", "qID")) %>% 
  mutate(
    choice = ifelse(choice == altID, 1, 0), 
    obsID = rep(seq(n() / 4), each = 4)
  ) %>% 
  select(session, id, respondentID, obsID, qID, altID, choice, everything())
    
# Save formatted response data
write_csv(choiceData, here::here('choiceData.csv'))

#Basic analytics on responses---------------------------------------------------
choices <- choiceData %>%
  filter(
    choice == 1)
tabyl(choices$type)

tabyl(data$knowledgeFuelElec)

tabyl(data$knowledgeFuelGas)

tabyl(data$knowledgeSub)

tabyl(data$practice)

data <- data %>%
  mutate(
    prac_good = ifelse(practice == 4, 1,0)
  )
