# Visualize results of estimated WTP space model - demos

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)
library(maddTools)
library(forcats)
library(ggrepel)

#Low/High Income

load(here::here('wtp', "wtp_ci_inc.RData"))

#--plot--

#choose data & add labels for plotting
df_compare <- wtp_ci_inc
cols <- c('mean', 'lower', 'upper')

#remove 2 & 6 weeks
df_compare <- df_compare %>% 
  filter(timing != "2wks" & timing != "6wks")

df_compare$label <- c(
  "Gov't      Time of Sale",
  "OEM      Time of Sale",
  "Dealer     Time of Sale", 
  "Gov't      Time of Sale",
  "Gov't      Time of Sale",
  "Gov't           Tax Filing",
  "Gov't           Tax Filing"
)

# visual 2 - facet
barplot_wtp2_inc <- df_compare %>%
  #  arrange(desc(type), desc(timing), desc(label)) %>%
  mutate(
    label = factor(label, levels = rev(unique(label)))
  ) %>%
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper, group = demo_label)) +
  geom_col(width = 0.8, aes(fill=demo_label), position = position_dodge2(width=1, reverse = TRUE)) +
  geom_errorbar(width = 0.8, position = position_dodge2(width=1, reverse = TRUE)) +
  #facet_wrap(~type, scales = "free_y", ncol = 1) +
  facet_grid(type~., scales = "free_y", space = "free") +
  scale_fill_manual(values = c('sienna', 'grey80')) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = 'Value ($)', title = "Value of Incentive Relative to Rebate (Time of Sale, gov't)", 
       tag = "Source      Timing",
       fill = "Income Breakdown") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.title.y = element_blank(),
        plot.tag.position = c(.1, .95),
        plot.tag = element_text(size=9, face = "italic"),
        legend.position="bottom") + 
  coord_cartesian(clip = "off") 
barplot_wtp2_inc

ggsave(
  filename = here('figs', 'wtp_barplot2_inc_50k.png'), 
  plot = barplot_wtp2_inc, 
  width = 6.5, height = 4
)

#2 new or used plot

load(here::here("wtp", "wtp_ci_vehicle.RData"))

#--plot--

#choose data & add labels for plotting
df_compare <- wtp_ci_vehicle
cols <- c('mean', 'lower', 'upper')

#remove 2 & 6 weeks
df_compare <- df_compare %>% 
  filter(timing != "2wks" & timing != "6wks")

df_compare$label <- c(
  "Gov't      Time of Sale",
  "OEM      Time of Sale",
  "Dealer     Time of Sale", 
  "Gov't      Time of Sale",
  "Gov't      Time of Sale",
  "Gov't           Tax Filing",
  "Gov't           Tax Filing"
)

# visual 2 - facet
barplot_wtp2_veh <- df_compare %>%
  #  arrange(desc(type), desc(timing), desc(label)) %>%
  mutate(
    label = factor(label, levels = rev(unique(label)))
  ) %>%
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper, group = demo_label)) +
  geom_col(width = 0.8, aes(fill=demo_label), position = position_dodge2(width=1, reverse = TRUE)) +
  geom_errorbar(width = 0.8, position = position_dodge2(width=1, reverse = TRUE)) +
  #facet_wrap(~type, scales = "free_y", ncol = 1) +
  facet_grid(type~., scales = "free_y", space = "free") +
  scale_fill_manual(values = c('grey80', 'sienna')) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = 'Value ($)', title = "Value of Incentive Relative to Rebate (Time of Sale, gov't)", 
       tag = "Source      Timing",
       fill = "Shopping \nNew vs. Used") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.title.y = element_blank(),
        plot.tag.position = c(.1, .95),
        plot.tag = element_text(size=9, face = "italic"),
        legend.position="bottom") + 
  coord_cartesian(clip = "off") 
barplot_wtp2_veh

# Save plot 
ggsave(
  filename = here('figs', 'wtp_barplot2_veh.png'), 
  plot = barplot_wtp2_veh, 
  width = 6.5, height = 4
)


#3 Consider PEV

load(here::here('wtp', "wtp_ci_EVconsider.RData"))

#--plot--

#choose data & add labels for plotting
df_compare <- wtp_ci_EVconsider
cols <- c('mean', 'lower', 'upper')

#remove 2 & 6 weeks
df_compare <- df_compare %>% 
  filter(timing != "2wks" & timing != "6wks")

df_compare$label <- c(
  "Gov't      Time of Sale",
  "OEM      Time of Sale",
  "Dealer     Time of Sale", 
  "Gov't      Time of Sale",
  "Gov't      Time of Sale",
  "Gov't           Tax Filing",
  "Gov't           Tax Filing"
)

# visual 2 - facet
barplot_wtp2_EVconsider <- df_compare %>%
  #  arrange(desc(type), desc(timing), desc(label)) %>%
  mutate(
    label = factor(label, levels = rev(unique(label)))
  ) %>%
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper, group = demo_label)) +
  geom_col(width = 0.8, aes(fill=demo_label), position = position_dodge2(width=1, reverse = TRUE)) +
  geom_errorbar(width = 0.8, position = position_dodge2(width=1, reverse = TRUE)) +
  #facet_wrap(~type, scales = "free_y", ncol = 1) +
  facet_grid(type~., scales = "free_y", space = "free") +
  scale_fill_manual(values = c('grey80', 'sienna')) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = 'Value ($)', title = "Value of Incentive Relative to Rebate (Time of Sale, gov't)", 
       tag = "Source      Timing",
       fill = "PEV Consideration") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.title.y = element_blank(),
        plot.tag.position = c(.1, .95),
        plot.tag = element_text(size=9, face = "italic"),
        legend.position="bottom") + 
  coord_cartesian(clip = "off") 
barplot_wtp2_EVconsider

ggsave(
  filename = here('figs', 'wtp_barplot2_EVconsider_50k.png'), 
  plot = barplot_wtp2_EVconsider, 
  width = 6.5, height = 4
)


#3 Knowledge of subsidy

load(here::here('wtp', "wtp_ci_knowSub.RData"))

#--plot--

#choose data & add labels for plotting
df_compare <- wtp_ci_knowSub
cols <- c('mean', 'lower', 'upper')

#remove 2 & 6 weeks
df_compare <- df_compare %>% 
  filter(timing != "2wks" & timing != "6wks")

df_compare$label <- c(
  "Gov't      Time of Sale",
  "OEM      Time of Sale",
  "Dealer     Time of Sale", 
  "Gov't      Time of Sale",
  "Gov't      Time of Sale",
  "Gov't           Tax Filing",
  "Gov't           Tax Filing"
)

# visual 2 - facet
barplot_wtp2_knowSub <- df_compare %>%
  #  arrange(desc(type), desc(timing), desc(label)) %>%
  mutate(
    label = factor(label, levels = rev(unique(label)))
  ) %>%
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper, group = demo_label)) +
  geom_col(width = 0.8, aes(fill=demo_label), position = position_dodge2(width=1, reverse = TRUE)) +
  geom_errorbar(width = 0.8, position = position_dodge2(width=1, reverse = TRUE)) +
  #facet_wrap(~type, scales = "free_y", ncol = 1) +
  facet_grid(type~., scales = "free_y", space = "free") +
  scale_fill_manual(values = c('grey80', 'sienna')) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = 'Value ($)', title = "Value of Incentive Relative to Rebate (Time of Sale, gov't)", 
       tag = "Source      Timing",
       fill = "Knowledge of today's EV tax credit") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.title.y = element_blank(),
        plot.tag.position = c(.1, .95),
        plot.tag = element_text(size=9, face = "italic"),
        legend.position="bottom") + 
  coord_cartesian(clip = "off") 
barplot_wtp2_knowSub

ggsave(
  filename = here('figs', 'wtp_barplot2_knowSub_50k.png'), 
  plot = barplot_wtp2_knowSub, 
  width = 6.5, height = 4
)


#3 Car Budget

load(here::here('wtp', "wtp_ci_carBudget.RData"))

#--plot--

#choose data & add labels for plotting
df_compare <- wtp_ci_carBudget
cols <- c('mean', 'lower', 'upper')

#remove 2 & 6 weeks
df_compare <- df_compare %>% 
  filter(timing != "2wks" & timing != "6wks")

df_compare$label <- c(
  "Gov't      Time of Sale",
  "OEM      Time of Sale",
  "Dealer     Time of Sale", 
  "Gov't      Time of Sale",
  "Gov't      Time of Sale",
  "Gov't           Tax Filing",
  "Gov't           Tax Filing"
)

# visual 2 - facet
barplot_wtp2_carBudget <- df_compare %>%
  #  arrange(desc(type), desc(timing), desc(label)) %>%
  mutate(
    label = factor(label, levels = rev(unique(label)))
  ) %>%
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper, group = demo_label)) +
  geom_col(width = 0.8, aes(fill=demo_label), position = position_dodge2(width=1, reverse = TRUE)) +
  geom_errorbar(width = 0.8, position = position_dodge2(width=1, reverse = TRUE)) +
  #facet_wrap(~type, scales = "free_y", ncol = 1) +
  facet_grid(type~., scales = "free_y", space = "free") +
  scale_fill_manual(values = c('sienna', 'grey80')) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = 'Value ($)', title = "Value of Incentive Relative to Rebate (Time of Sale, gov't)", 
       tag = "Source      Timing",
       fill = "Car Budget") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.title.y = element_blank(),
        plot.tag.position = c(.1, .95),
        plot.tag = element_text(size=9, face = "italic"),
        legend.position="bottom") + 
  coord_cartesian(clip = "off") 
barplot_wtp2_carBudget

ggsave(
  filename = here('figs', 'wtp_barplot2_carBudget_50k.png'), 
  plot = barplot_wtp2_carBudget, 
  width = 6.5, height = 4
)


#3 Home parking

load(here::here('wtp', "wtp_ci_homepark.RData"))

#--plot--

#choose data & add labels for plotting
df_compare <- wtp_ci_homepark
cols <- c('mean', 'lower', 'upper')

#remove 2 & 6 weeks
df_compare <- df_compare %>% 
  filter(timing != "2wks" & timing != "6wks")

df_compare$label <- c(
  "Gov't      Time of Sale",
  "OEM      Time of Sale",
  "Dealer     Time of Sale", 
  "Gov't      Time of Sale",
  "Gov't      Time of Sale",
  "Gov't           Tax Filing",
  "Gov't           Tax Filing"
)

# visual 2 - facet
barplot_wtp2_homepark <- df_compare %>%
  #  arrange(desc(type), desc(timing), desc(label)) %>%
  mutate(
    label = factor(label, levels = rev(unique(label)))
  ) %>%
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper, group = demo_label)) +
  geom_col(width = 0.8, aes(fill=demo_label), position = position_dodge2(width=1, reverse = TRUE)) +
  geom_errorbar(width = 0.8, position = position_dodge2(width=1, reverse = TRUE)) +
  #facet_wrap(~type, scales = "free_y", ncol = 1) +
  facet_grid(type~., scales = "free_y", space = "free") +
  scale_fill_manual(values = c('sienna', 'grey80')) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = 'Value ($)', title = "Value of Incentive Relative to Rebate (Time of Sale, gov't)", 
       tag = "Source      Timing",
       fill = "Access to home parking") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.title.y = element_blank(),
        plot.tag.position = c(.1, .95),
        plot.tag = element_text(size=9, face = "italic"),
        legend.position="bottom") + 
  coord_cartesian(clip = "off") 
barplot_wtp2_homepark

ggsave(
  filename = here('figs', 'wtp_barplot2_homepark_50k.png'), 
  plot = barplot_wtp2_homepark, 
  width = 6.5, height = 4
)


#3 Housing Ownership

load(here::here('wtp', "wtp_ci_housingOwn.RData"))

#--plot--

#choose data & add labels for plotting
df_compare <- wtp_ci_housingOwn
cols <- c('mean', 'lower', 'upper')

#remove 2 & 6 weeks
df_compare <- df_compare %>% 
  filter(timing != "2wks" & timing != "6wks")

df_compare$label <- c(
  "Gov't      Time of Sale",
  "OEM      Time of Sale",
  "Dealer     Time of Sale", 
  "Gov't      Time of Sale",
  "Gov't      Time of Sale",
  "Gov't           Tax Filing",
  "Gov't           Tax Filing"
)

# visual 2 - facet
barplot_wtp2_housingOwn <- df_compare %>%
  #  arrange(desc(type), desc(timing), desc(label)) %>%
  mutate(
    label = factor(label, levels = rev(unique(label)))
  ) %>%
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper, group = demo_label)) +
  geom_col(width = 0.8, aes(fill=demo_label), position = position_dodge2(width=1, reverse = TRUE)) +
  geom_errorbar(width = 0.8, position = position_dodge2(width=1, reverse = TRUE)) +
  #facet_wrap(~type, scales = "free_y", ncol = 1) +
  facet_grid(type~., scales = "free_y", space = "free") +
  scale_fill_manual(values = c('grey80', 'sienna')) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = 'Value ($)', title = "Value of Incentive Relative to Rebate (Time of Sale, gov't)", 
       tag = "Source      Timing",
       fill = "Housing Ownership") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.title.y = element_blank(),
        plot.tag.position = c(.1, .95),
        plot.tag = element_text(size=9, face = "italic"),
        legend.position="bottom") + 
  coord_cartesian(clip = "off") 
barplot_wtp2_housingOwn

ggsave(
  filename = here('figs', 'wtp_barplot2_housingOwn_50k.png'), 
  plot = barplot_wtp2_housingOwn, 
  width = 6.5, height = 4
)

# APPENDIX---------------------------------------------------------------------

#Rural / Non Rural

load(here::here('wtp', "wtp_ci_geo.RData"))

#--plot--

#choose data & add labels for plotting
df_compare <- wtp_ci_geo
cols <- c('mean', 'lower', 'upper')

# #remove 2 & 6 weeks
# df_compare <- df_compare %>%
#   filter(timing != "2wks" & timing != "6wks")
# 
# df_compare$label <- c(
#   "Gov't      Time of Sale",
#   "OEM      Time of Sale",
#   "Dealer     Time of Sale",
#   "Gov't      Time of Sale",
#   "Gov't      Time of Sale",
#   "Gov't           Tax Filing",
#   "Gov't           Tax Filing"
# )

df_compare$label <- c(
  "Gov't      Time of Sale",
  "OEM      Time of Sale",
  "Dealer     Time of Sale",
  "Gov't      2 weeks later",
  "OEM      2 weeks later",
  "Dealer     2 weeks later",
  "Gov't      6 weeks later",
  "OEM      6 weeks later",
  "Dealer     6 weeks later",
  "Gov't      Time of Sale",
  "Gov't      Time of Sale",
  "Gov't           Tax Filing",
  "Gov't           Tax Filing"
)

# visual 2 - facet
barplot_wtp2_geo <- df_compare %>%
  #  arrange(desc(type), desc(timing), desc(label)) %>%
  mutate(
    label = factor(label, levels = rev(unique(label)))
  ) %>%
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper, group = demo_label)) +
  geom_col(width = 0.8, aes(fill=demo_label), position = position_dodge2(width=1, reverse = TRUE)) +
  geom_errorbar(width = 0.8, position = position_dodge2(width=1, reverse = TRUE)) +
  #facet_wrap(~type, scales = "free_y", ncol = 1) +
  facet_grid(type~., scales = "free_y", space = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = 'Value ($)', title = "Value of Incentive Relative to Rebate (Time of Sale, gov't)", 
       tag = "Source      Timing",
       fill = "Income Breakdown") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.title.y = element_blank(),
        plot.tag.position = c(.1, .95),
        plot.tag = element_text(size=9, face = "italic"),
        legend.position="bottom") + 
  coord_cartesian(clip = "off") 
barplot_wtp2_geo

ggsave(
  filename = here('figs', 'wtp_barplot2_geo_50k.png'), 
  plot = barplot_wtp2_geo, 
  width = 6.5, height = 4
)


# Ethnicity

load(here::here('wtp', "wtp_ci_eth.RData"))

#--plot--

#choose data & add labels for plotting
df_compare <- wtp_ci_eth
cols <- c('mean', 'lower', 'upper')

#remove 2 & 6 weeks
df_compare <- df_compare %>% 
  filter(timing != "2wks" & timing != "6wks")

df_compare$label <- c(
  "Gov't      Time of Sale",
  "OEM      Time of Sale",
  "Dealer     Time of Sale", 
  "Gov't      Time of Sale",
  "Gov't      Time of Sale",
  "Gov't           Tax Filing",
  "Gov't           Tax Filing"
)

# visual 2 - facet
barplot_wtp2_eth <- df_compare %>%
  #  arrange(desc(type), desc(timing), desc(label)) %>%
  mutate(
    label = factor(label, levels = rev(unique(label)))
  ) %>%
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper, group = demo_label)) +
  geom_col(width = 0.8, aes(fill=demo_label), position = position_dodge2(width=1, reverse = TRUE)) +
  geom_errorbar(width = 0.8, position = position_dodge2(width=1, reverse = TRUE)) +
  #facet_wrap(~type, scales = "free_y", ncol = 1) +
  facet_grid(type~., scales = "free_y", space = "free") +
  scale_fill_manual(values = c('sienna', 'grey80')) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = 'Value ($)', title = "Value of Incentive Relative to Rebate (Time of Sale, gov't)", 
       tag = "Source      Timing",
       fill = "Ethnicity") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.title.y = element_blank(),
        plot.tag.position = c(.1, .95),
        plot.tag = element_text(size=9, face = "italic"),
        legend.position="bottom") + 
  coord_cartesian(clip = "off") 
barplot_wtp2_eth

ggsave(
  filename = here('figs', 'wtp_barplot2_eth_50k.png'), 
  plot = barplot_wtp2_eth, 
  width = 6.5, height = 4
)