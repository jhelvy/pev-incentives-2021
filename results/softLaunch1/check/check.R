#check respondentID 462 ----------------------
#data check
raw_data_462 <- raw_data_main %>%
  filter(respondentID == 462) %>%
  select(contains("n1_"), contains("n2_"), contains("cbc"), -contains("systime")) %>%
  mutate(
    source = "raw_data"
  )

data_joined_462 <- data %>%
  filter(respondentID == 462) %>%
  select(contains("n1_"), contains("n2_"), contains("cbc"), -contains("systime"), -contains("sec")) %>%
  mutate(
    source = "data_joined_filtered"
  )

compare_survey462 <- bind_rows(raw_data_462, data_joined_462)

write_csv(compare_survey462, here::here('check', 'data_check_462.csv'))

#choice data processing check
raw_survey_462 <- survey_raw %>%
  filter(respID == 462 & ((qID == 1) | (qID == 2))) %>%
  select(-obsID, -contains("label")) %>%
  mutate(
    source = "raw_survey"
  )

survey_462 <- survey %>%
  filter(respondentID == 462 & ((qID == 1) | (qID == 2))) %>%
  select(-obsID, -contains("label")) %>%
  mutate(
    source = "survey_refac"
  )

choiceData_462 <- choiceData %>%
  filter(respondentID == 462 & ((qID == 1) | (qID == 2))) %>%
  select(-obsID) %>%
  mutate(
    source = "choiceData"
  )

compare_choice462 <- bind_rows(raw_survey_462, survey_462, choiceData_462) %>%
  arrange(qID, type)

write_csv(compare_choice462, here::here('check', 'choice_check_462.csv'))

# check respondentID 548 ----------------------------
# data check
raw_data_548 <- raw_data_main %>%
  filter(respondentID == 548) %>%
  select(session, contains("n1_"), contains("n2_"),maxSalestax, contains("cbc"), -contains("systime")) %>%
  mutate(
    source = "raw_data"
  )

data_joined_548 <- data %>%
  filter(respondentID == 548) %>%
  select(session, contains("n1_"), contains("n2_"), maxSalestax, contains("cbc"), -contains("systime"), -contains("sec")) %>%
  mutate(
    source = "data_joined_filtered"
  )

compare_survey548 <- bind_rows(raw_data_548, data_joined_548) %>%
  arrange(session)

write_csv(compare_survey548, here::here('check', 'data_check_548.csv'))

#choice data processing check
raw_survey_548 <- survey_raw %>%
  filter(respID == 548 & ((qID == 1) | (qID == 2))) %>%
  select(-obsID, -contains("label")) %>%
  mutate(
    source = "raw_survey"
  )

survey_548 <- survey %>%
  filter(respondentID == 548 & ((qID == 1) | (qID == 2))) %>%
  select(-obsID, -contains("label")) %>%
  mutate(
    source = "survey_refac"
  )

choiceData_548 <- choiceData %>%
  filter(respondentID == 548 & ((qID == 1) | (qID == 2))) %>%
  select(-obsID) %>%
  mutate(
    source = "choiceData"
  )

compare_choice548 <- bind_rows(raw_survey_548, survey_548, choiceData_548) %>%
  arrange(qID, type)

write_csv(compare_choice548, here::here('check', 'choice_check_548.csv'))
