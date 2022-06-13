#R file to run incentive spend agregated over time and other computations

library(tidyverse)
library(magrittr)
options(dplyr.width = Inf)
library(lubridate)
library(here)
library(janitor)
library(cowplot)

#Implicit Discount Rate

#load wtp space model
load(here::here("wtp", "wtp_ci_col.RData"))
load(here::here("wtp", "wtp_ci_mxl_col.RData"))

get_discount_rate <- function(value, time) {
  # Value at time t
  V_t <- 7500
  # Present value: Using "+" because "value" is the estimated *difference*,
  # which is a negative number
  V_0 <- V_t + value
  rate <- ((V_t / V_0)^(1 / time)) - 1
  return(100*rate)
}

# Tax credit
# timeframe (fielding in Oct/Nov to April tax filing ~6months)

# MNL
taxcred_taxfile_value <- filter(wtp_ci_col, par == "type_taxCredit_taxFiling")$mean
timeframe_taxcred <- 1/2
get_discount_rate(taxcred_taxfile_value, timeframe_taxcred)

# MXL
taxcred_taxfile_value_mxl <- filter(wtp_ci_mxl_col, par == "type_taxCredit_taxFiling")$mean
get_discount_rate(taxcred_taxfile_value_mxl, timeframe_taxcred)

# 6-week rebate

# MNL
rebate6wks_value <- filter(wtp_ci_col, par == "type_rebate_6wks_govt")$mean
timefame_rebate <- 6/52
get_discount_rate(rebate6wks_value, timefame_rebate)

# MXL
rebate6wks_value_mxl <- filter(wtp_ci_mxl_col, par == "type_rebate_6wks_govt")$mean
get_discount_rate(rebate6wks_value_mxl, timefame_rebate)

# 2-week rebate

# MNL
rebate2wks_value <- filter(wtp_ci_col, par == "type_rebate_2wks_govt")$mean
timefame_rebate_2 <- 2/52
get_discount_rate(rebate2wks_value, timefame_rebate_2)

# MXL
rebate2wks_value_mxl <- filter(wtp_ci_mxl_col, par == "type_rebate_2wks_govt")$mean
get_discount_rate(rebate2wks_value_mxl, timefame_rebate_2)




#Tax credit spend based on EV sales vs. customer value

evsales_2010to2019 <- raw_data_start <- read_csv(here("data", "usEvSales.csv"))
load(here::here("wtp", "wtp_ci_col.RData"))
wtp_ci_col <- wtp_ci_col %>% filter(str_detect(wtp_ci_col$par, "taxCredit_taxFiling"))
evsales_phev <- read_csv(here("data" , "evsales_phev_vehicle_list.csv"))
ev_phaseout <- read_csv(here( "data", "evphaseout_list.csv")) %>% select(vehicle, year, month, taxcredit)

#create data frame with accurate tax credit amounts for phev, phaseouts
evsales_taxcred <- evsales_2010to2019 %>%
  left_join(evsales_phev, by = c("vehicle", "category", "year")) %>%
  mutate(
    taxcredit = replace(taxcredit, category == "bev", 7500))%>%
  left_join(ev_phaseout, by = c("vehicle", "year", "month")) %>%
  mutate(
    taxcredit = if_else(is.na(taxcredit.y)== "TRUE",taxcredit.x, taxcredit.y)
  ) %>%
  select(-taxcredit.x, -taxcredit.y)
  

evsales_taxcred <- evsales_taxcred %>%
  mutate(
    taxcred_adj = taxcredit + (wtp_ci_col$mean),
    taxcred_spend = sales * taxcredit,
    taxcred_spend_adj = sales * (taxcredit + (wtp_ci_col$mean)),
    diff = taxcred_spend - taxcred_spend_adj
  ) %>%
  group_by(year) %>%
  summarise(sales_sum = sum(sales), 
            taxcred_spend = sum(taxcred_spend),
            taxcred_spend_adj = sum(taxcred_spend_adj)) %>%
  mutate(
    taxcred_spend = round(taxcred_spend / 10^9,2),
    taxcred_spend_adj = round(taxcred_spend_adj / 10^9,2),
    diff = taxcred_spend - taxcred_spend_adj,
    year = as.character(year)
  )

library(showtext)

font_add_google("Fira Sans Condensed", "Fira Sans Condensed")
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto() 

mainFont <- "Fira Sans Condensed"
annFont <- "Roboto Condensed"
col_value <- "grey42"
col_loss <- "#FF3B3F"
evsales_taxcred_plot_col <- evsales_taxcred %>% 
  filter(year != '2010') %>% 
  ggplot() +
  geom_col(aes(x = year, y = taxcred_spend), width = 0.5, fill = col_loss) +
  geom_text(aes(x = year, y = taxcred_spend, label = taxcred_spend),
            vjust = -1,color = 'black', family = annFont,
            size = 3, nudge_y = -0.02)+
  geom_col(aes(x = year, y = taxcred_spend_adj), width = 0.5, fill = col_value) +
  geom_text(aes(x = year, y = taxcred_spend_adj, label = taxcred_spend_adj),
            vjust = 1,color = 'white', family = annFont,
            size = 3, nudge_y = -0.02) +
  scale_x_discrete(breaks = as.character(seq(2011, 2019))) +
  scale_y_continuous(
    expand = expansion(mult = c(0,0.05)), 
    labels = scales::dollar)+
  labs(x = 'Year',  
       y = "Tax Credit Spend (USD $ Billion)") +
  annotate(
    'text', x = "2011", y = 2.6,
    color = col_value, vjust = 1, hjust = 0, fontface = 2, family = annFont,
    label = "The $8.65 billion in tax credits allocated between 2011 - 2019\nonly generated $6.58 billion in value to consumers.")+
  annotate(
    'text', x = "2011", y = 2.3,
    color = col_loss, vjust = 1, hjust = 0, fontface = 2, family = annFont,
    label = "If the subsidy had been delivered as an immediate rebate,\nthe federal government could have saved $2.07 billion.") +
  theme_cowplot(font_family = mainFont)

evsales_taxcred_plot_col

# Save plots 
ggsave(
  filename = here('figs', 'fed_taxcred_spend_col.pdf'), 
  plot = evsales_taxcred_plot_col, 
  width = 7, height = 5, device = cairo_pdf
) 

