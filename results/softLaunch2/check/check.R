#check respondentID 3559 ----------------------
#data check
raw_data_3559 <- raw_data_main %>%
  filter(respondentID == 3559) %>%
  select(contains("n1_"), contains("n2_"), maxSalestax, contains("cbc"), -contains("systime")) %>%
  mutate(
    source = "raw_data"
  )

data_joined_3559 <- data %>%
  filter(respondentID == 3559) %>%
  select(contains("n1_"), contains("n2_"), maxSalestax, contains("cbc"), -contains("systime"), -contains("sec")) %>%
  mutate(
    source = "data_joined_filtered"
  )

compare_survey3559 <- bind_rows(raw_data_3559, data_joined_3559)

write_csv(compare_survey3559, here::here('check', 'data_check_3559.csv'))

#choice data processing check
raw_survey_3559 <- survey_raw %>%
  filter(respID == 3559 & ((qID == 1) | (qID == 2))) %>%
  select(-obsID, -contains("label")) %>%
  mutate(
    source = "raw_survey"
  )

survey_3559 <- survey %>%
  filter(respondentID == 3559 & ((qID == 1) | (qID == 2))) %>%
  select(-obsID, -contains("label")) %>%
  mutate(
    source = "survey_refac"
  )

choiceData_3559 <- choiceData %>%
  filter(respondentID == 3559 & ((qID == 1) | (qID == 2))) %>%
  select(-obsID) %>%
  mutate(
    source = "choiceData"
  )

compare_choice3559 <- bind_rows(raw_survey_3559, survey_3559, choiceData_3559) %>%
  arrange(qID, type)

write_csv(compare_choice3559, here::here('check', 'choice_check_3559.csv'))

# check respondentID 9024 ----------------------------
# data check
raw_data_9024 <- raw_data_main %>%
  filter(respondentID == 9024) %>%
  select(session, contains("n1_"), contains("n2_"),maxSalestax, contains("cbc"), -contains("systime")) %>%
  mutate(
    source = "raw_data"
  )

data_joined_9024 <- data %>%
  filter(respondentID == 9024) %>%
  select(session, contains("n1_"), contains("n2_"), maxSalestax, contains("cbc"), -contains("systime"), -contains("sec")) %>%
  mutate(
    source = "data_joined_filtered"
  )

compare_survey9024 <- bind_rows(raw_data_9024, data_joined_9024) %>%
  arrange(session)

write_csv(compare_survey9024, here::here('check', 'data_check_9024.csv'))

#choice data processing check
raw_survey_9024 <- survey_raw %>%
  filter(respID == 9024 & ((qID == 1) | (qID == 2))) %>%
  select(-obsID, -contains("label")) %>%
  mutate(
    source = "raw_survey"
  )

survey_9024 <- survey %>%
  filter(respondentID == 9024 & ((qID == 1) | (qID == 2))) %>%
  select(-obsID, -contains("label")) %>%
  mutate(
    source = "survey_refac"
  )

choiceData_9024 <- choiceData %>%
  filter(respondentID == 9024 & ((qID == 1) | (qID == 2))) %>%
  select(-obsID) %>%
  mutate(
    source = "choiceData"
  )

compare_choice9024 <- bind_rows(raw_survey_9024, survey_9024, choiceData_9024) %>%
  arrange(qID, type)

write_csv(compare_choice9024, here::here('check', 'choice_check_9024.csv'))
