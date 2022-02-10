#install.packages("devtools")
#devtools::install_github("rubenarslan/formr")

# library(formr)
library(tidyverse)
library(magrittr)
options(dplyr.width = Inf)
library(lubridate)
library(here)
library(janitor)
library(remotes)

## **Don't save with password entered in**
#password <- ""
#email <- "lar1223@gwmail.gwu.edu"

# Connect to formr
#formr_connect(email, password)

# Download raw data
#raw_data_start <- formr_raw_results(survey_name = 'Vehicle_Incentive_Study_Dynata_start')
#raw_data_main <- formr_raw_results(survey_name = 'Vehicle_Incentive_Study_Dynata')
raw_data_start <- read_csv(here("data", "Vehicle_Incentive_Study_Dynata_start.csv"))
raw_data_prac <- read_csv(here("data", "Vehicle_Incentive_Study_Dynata_prac.csv"))
raw_data_main <- read_csv(here("data", "Vehicle_Incentive_Study_Dynata.csv"))
raw_data_demos <- read_csv(here("data", "Vehicle_Incentive_Study_Dynata_Demos.csv"))

# #pull out psid list
# psid_raw <- raw_data_start %>% 
#   select(psid)
# 
# write_csv(psid_raw, here::here("psid_raw.csv"))
# 
# psid_unique <- psid_raw %>%
#   group_by(psid) %>%
#   count(psid)
# 
# write_csv(psid_unique, here::here("psid_unique.csv"))

#pull out zips 
zips <- raw_data_start %>%
  select(session, zip)

#Format start survey
raw_data_start <- raw_data_start %>%
  #calc time to do start survey
  mutate(
    created = as_datetime(created),
    ended = as_datetime(ended),
    sec_elapsed_start = as.numeric(ended - created, units = "secs")) %>%
  #select important col from start survey
  select(session, psid, respondentID, inMarket, inMarketscreenout, sec_elapsed_start) %>% 
  filter(!is.na(session))
# select(respondentNumber, inMarket, inMarketscreenout, sec_elapsed_start)

#Format prac survey
raw_data_prac <- raw_data_prac %>%
  #calc time to do start survey
  mutate(
    created = as_datetime(created),
    ended = as_datetime(ended),
    sec_elapsed_prac = as.numeric(ended - created, units = "secs")) %>%
  #select important col from prac survey
  select(-created, -modified, -ended, -expired, -psid, -respondentID, -salesTaxpath, -maxSalestax) %>% 
  filter(!is.na(session))

#Format main survey
raw_data_main <- raw_data_main %>%
  #calc time to do start survey
  mutate(
    created = as_datetime(created),
    ended = as_datetime(ended),
    sec_elapsed_main = as.numeric(ended - created, units = "secs")) %>%
  #select important col from main survey
  select(-respondentID, -created, -modified, -ended, -expired, -psid ) %>% 
  filter(!is.na(session))

#Format demo survey
raw_data_demos <- raw_data_demos %>%
  #calc time to do demo survey
  mutate(
    created = as_datetime(created),
    ended = as_datetime(ended),
    sec_elapsed_demos = as.numeric(ended - created, units = "secs")) %>%
  #select important col from demo survey
  select(-respondentID, -created, -modified, -ended, -expired) %>% 
  filter(!is.na(session))  

# Join start and main data sets

raw_data <- raw_data_start %>%
  left_join(raw_data_prac, by = "session") %>% 
  left_join(raw_data_main, by = "session") %>% 
  left_join(raw_data_demos, by = "session")


#Data Cleaning--------------------------------

data <- raw_data %T>%
  {print(paste("data no filter",nrow(.)))} %>%
  # Remove duplicate session IDs
  # distinct(session, .keep_all = TRUE) %T>%
  # {print(paste("duplicate session IDs",nrow(.)))} %>%
  # 1 - filter out psid NAs
  filter(!is.na(psid)) %T>%
  {print(paste("psid NA",nrow(.)))} %>%
  # 1 - filter out tests
  filter(psid != "123") %T>%
  {print(paste("test psid",nrow(.)))} %>%
  # 2 - Remove in Market NA
  filter(!is.na(inMarketscreenout)) %T>%
  {print(paste("inMarket NA",nrow(.)))} %>%
  # 3- Remove in Market screenouts
  filter(inMarketscreenout == 0) %T>%
  {print(paste("inMarket screenout",nrow(.)))} %>%
  # 4 - practice
  filter(practice == 4 ) %T>%
  {print(paste("practice screenout",nrow(.)))} %>%
  # filter out bad responses
  # 5 - Remove cases that did not complete the CBC questions
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
  {print(paste("cbc screenout",nrow(.)))}
  # 5- Didnt complete full survey
  #filter(!is.na(sec_elasped_demos)) %T>%
  #{print(paste("didnt complete demos",nrow(.)))}


write_csv(data, here::here("data_filtered.csv"))

data <- data %>%
  #calc times for survey
  mutate(
    sec_elapsed_total = as.numeric((sec_elapsed_start + sec_elapsed_prac + 
                                      sec_elapsed_main + sec_elapsed_demos),
                                   units = "secs"),
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
  
# Separate out feedback
feedback <- data %>%
  filter(!is.na(feedback)) %>%
  select(feedback)

write_csv(feedback, here::here("results_feedback.csv"))


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
  select(session,id, respondentID, obsID, qID, altID, choice, everything())
    
# Save formatted response data
write_csv(choiceData, here::here('choiceData.csv'))

#combine filtered data from SoftLaunch1 and 50k income sample ----------------------------------------------------------

data_sf1 <- read_csv(here("softLaunch1", "data_filtered.csv"))
data_sf1 <- data_sf1 %>%
  mutate(
    yearOfBirth = as.character(yearOfBirth))

data_50k <- read_csv(here("50kincome", "data_filtered.csv"))
data_50k <- data_50k %>%
  mutate(
    yearOfBirth = as.character(yearOfBirth))

#combine softLaunch1 data
data_comb <- data %>%
  bind_rows(data_sf1)

#combine softLaunch1 and 50k data
data_comb_50k <- data %>%
  bind_rows(data_sf1) %>%
  bind_rows(data_50k)

#save data filtered total + SoftLaunch1 and 50k income sample
write_csv(data_comb_50k, here::here("data_filtered_50k.csv"))



#combine choice data from SoftLaunch1 and 50k income sample ----------------------------------------------------------
choiceData <- read_csv(here("choiceData.csv"))
choiceData_sf1 <- read_csv(here("softLaunch1", "choiceData.csv"))
choiceData_50k <- read_csv(here("50kincome", "choiceData.csv"))

#bind rows to softLaunch2 choiceData

choiceData_comb_50k <-choiceData %>%
  bind_rows(choiceData_sf1) %>%
  bind_rows(choiceData_50k)

#choices breakdown
choices <- choiceData_comb_50k %>%
  filter(
    choice == 1)
tabyl(choices$type)


#renumber obsID
choiceData_comb_50k$obsID = rep(seq(nrow(choiceData_comb_50k) / 4), each = 4)

choiceData_comb_50k <- choiceData_comb_50k %>%
  mutate(
    maxSalestax = as.numeric(maxSalestax),
    amount_salesTax = (round((amount_salesTax * maxSalestax), -2))) %>%
  select(-maxSalestax) %>%
  mutate(
    amount = ifelse(
      type == "salesTax", as.numeric(amount_salesTax),
      as.numeric(as.character(amount_nonSalesTax)))) %>%
  select(-starts_with("amount_")) 

# Save combined model formatted data
write_csv(choiceData_comb_50k, here::here('choiceData_comb_50k.csv'))

#Basic analytics on responses---------------------------------------------------
print(paste(length(unique(data_comb_50k$respondentID)), "vs.", nrow(data_comb_50k)))

data_comb_50k %>% 
  group_by(respondentID) %>% 
  count(respondentID) %>%
  filter(n>1)

tabyl(data_comb_50k$carBudget)

tabyl(data_comb_50k$test_tax_credit)

tabyl(data_comb_50k$test_tax_deduct)

tabyl(data_comb_50k$knowledgeFuelGas)

tabyl(data_comb_50k$knowledgeFuelElec)

tabyl(data_comb_50k$knowledgeSub)

tabyl(data_comb_50k$considerBEV)

tabyl(data_comb_50k$householdVehicles)

tabyl(data_comb_50k$parkingHousehold)

tabyl(data_comb_50k$income)

tabyl(data_comb_50k$gender)

tabyl(data_comb_50k$ethnicity)

tabyl(data_comb_50k$education)

tabyl(data_comb_50k$new_or_used)


#Dynata Info (Income breakdown, psid file etc) ----------------------------------------------------------
#add buckets for Dynata
data_comb <- data_comb %>%
  mutate(
    income_dynata = ifelse(((income== "under25") | (income == "inc_25to35") | (income == "inc_35to50")), "<50",
                           ifelse((income== "inc_50to75"), "50_to_75",
                                  ifelse((income== "inc_75to100"), "75_to_100",
                                        ifelse((income== "inc_100to150"), "100_to_150",
                                                 ifelse(((income== "inc_150to200") | (income == "inc_200to250") | (income == "inc_250to300") |
                                                           (income == "inc_300to400")| (income == "inc_over400")), "150+",
                                                        ifelse((income == "prefer_not_say"), "prefer_not_say", 0)))))))

income_order <- c("<50", "50_to_75", "75_to_100", "100_to_150", "150+", "prefer_not_say", "NA")

data_comb %>%
  tabyl(income_dynata)%>%
  arrange(income_order)

tabyl(data_comb$gender)

tabyl(data_comb$ethnicity)

tabyl(data_comb$education)

tabyl(data_comb$new_or_used)

#pull out psid list
psid_valid <- data_comb %>% 
  select(psid)

write_csv(psid_valid, here::here("psid_valid.csv"))

#ZIPS------------------------------------------------------------------------

library(zipcodeR)

# #sf1 zips
data_sf1_zips <- read_csv(here("softLaunch1", "data", "Vehicle_Incentive_Study_Dynata_start.csv")) %>%
   select(session, zip)

# #50k zips
data_50k_zips <- read_csv(here("50kincome", "data", "Vehicle_Incentive_Study_Dynata_start_50k.csv")) %>%
  select(session, zip)

zips_comb_raw <- zips %>%
  bind_rows(data_sf1_zips) %>%
  bind_rows(data_50k_zips)

#add zips
zips_comb_50k <- data_comb_50k %>%
  left_join(zips_comb_raw, by = "session") %>%
  select(session,zip) %>%
  rename(zipcode = zip) %>%
  mutate(
    nchar = nchar(zipcode),
    zipcode = ifelse(nchar <5 , paste0("0",zipcode),zipcode)
  )

#zips_comb_geo <- geocode_zip(zips_comb_50k$zipcode) 

zips_comb_geo <- reverse_zipcode(zips_comb_50k$zipcode)

zips_comb_50k <- zips_comb_50k %>%
  left_join(zips_comb_geo) %>%
  select(session, zipcode, nchar, lat, lng, population, population_density) %>%
  mutate(
    #US dept of agriculture https://www.ers.usda.gov/topics/rural-economy-population/rural-classifications/what-is-rural.aspx
    rural = ifelse(population_density < 500,1,0)
  )

#add to filtered data

data_comb_50k <- data_comb_50k %>%
  left_join(zips_comb_50k)

#save data filtered total + SoftLaunch1 and 50k income sample with zips
write_csv(data_comb_50k, here::here("data_filtered_50k.csv"))

#zips_map <- ggplot() +
#  geom_polygon(
#    data = fifty_states, 
#    aes(x=long, y=lat, group = group),
#    color="white", fill="grey92") +
#  geom_point(
#    data = zips_comb,
#    aes(x = lng, y = lat),
#    size = 0.8, color = "blue", shape = 20) +
#  theme_void(base_size = 15)

#remotes::install_github("hrbrmstr/albersusa")

#Plot zips

library(ggplot2)
library(sf)
library(rgeos)
library(rnaturalearth)
library(rnaturalearthhires)

#filter for continental zips
zips_comb_cont <- zips_comb_50k %>%
  filter(lat > 22, lat < 50,
         lng > -150, lng < -66)

us_states_cont <- ne_states(
  country = 'united states of america',
  returnclass = 'sf') %>%
  filter(! name %in% c('Alaska', 'Hawaii'))

zips_map_cont <- ggplot() +
  geom_sf(data = us_states_cont) +
  geom_point(
    data = zips_comb_cont,
    aes(x = lng, y = lat),
    size = 0.8, color = "blue", shape = 20) +
  theme_void(base_size = 15)

# Save plot 
ggsave(
  filename = here('figs', 'zips_map.png'), 
  plot = zips_map_cont, 
  width = 5, height = 3
)  

#Alaska zips
zips_comb_AK <- zips_comb_50k %>%
  filter(lat > 50, lat < 80,
         lng > -190, lng < -130)

AK <- ne_states(
  country = 'united states of america',
  returnclass = 'sf') %>%
  filter(name == 'Alaska')

AK_map <- ggplot() +
  geom_sf(data = AK) +
  coord_sf(xlim = c(-190, -130), ylim = c(50, 80))+
  geom_point(
    data = zips_comb_AK,
    aes(x = lng, y = lat),
    size = 6, color = "blue", shape = 20) +
  theme_void()

# Save plot 
ggsave(
  filename = here('figs', 'AK_map.png'), 
  plot = AK_map, 
  width = 5, height = 3
)  


#Hawaii zips
zips_comb_HI <- zips_comb_50k %>%
  filter(lat > 18, lat < 22,
         lng > -162, lng < -152)

HI <- ne_states(
  country = 'united states of america',
  returnclass = 'sf') %>%
  filter(name == 'Hawaii')

HI_map <- ggplot() +
  geom_sf(data = HI) +
  coord_sf(xlim = c(-162, -152), ylim = c(18, 25))+
  geom_point(
    data = zips_comb_HI,
    aes(x = lng, y = lat),
    size = 6, color = "blue", shape = 20) +
  theme_void()

# Save plot 
ggsave(
  filename = here('figs', 'HI_map.png'), 
  plot = HI_map, 
  width = 5, height = 3
)  
