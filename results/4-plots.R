#Plots for WTP models

source("3-WTP_model.R")

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)
library(maddTools)
library(forcats)
library(ggrepel)

# -----------------------------------------------------------------------------
# Plot WTP for changes in all attributes 

#plot
df_compare <- wtp_ci
cols <- c('mean', 'lower', 'upper')


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
barplot_wtp2_m1_50k <- df_compare %>%
  #  arrange(desc(type), desc(timing), desc(label)) %>%
  mutate(
    label = factor(label, levels = rev(unique(label)))
  ) %>%
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper)) +
  geom_col(width = 0.5, fill = "grey") +
  geom_errorbar(width = 0.3) +
  #facet_wrap(~type, scales = "free_y", ncol = 1) +
  facet_grid(type~., scales = "free_y", space = "free") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = 'Value ($)', title = "Value of Incentive Relative to Rebate (Time of Sale, gov't)", 
       tag = "Source      Timing") +
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        axis.title.y = element_blank(),
        plot.tag.position = c(.1, .95),
        plot.tag = element_text(size=9, face = "italic")) + 
  coord_cartesian(clip = "off") 
barplot_wtp2_m1_50k

# Save plots 
ggsave(
  filename = here('figs', 'wtp_barplot2_m1_50k.png'), 
  plot = barplot_wtp2_m1_50k, 
  width = 6.5, height = 4
) 



#visual 1 - fill type
#barplot_wtp1 <- df_compare %>%
#  arrange(desc(type), desc(timing)) %>%
#  mutate(
#    label = factor(label, levels = unique(label))
#    ) %>%
#  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper, fill = type)) +
#  geom_col(width = 0.5, position = position_dodge()) +
# geom_errorbar(width = 0.3, position = position_dodge()) +
#  #facet_wrap(~type, scales = "free_y") +
#  labs(x = 'WTP ($1,000)', y = 'Attribute', title = 'Willingness to pay vs. Sales Tax Exemption') +
#  theme_bw()
#barplot_wtp1

# Save plots 
#ggsave(
#  filename = here('figs', 'wtp_barplot1.png'), 
#  plot = barplot_wtp1, 
#  width = 5, height = 3
#)


# #m2 plot--------------------------------------
# df_compare <- wtp_ci
# cols <- c('mean', 'lower', 'upper')
# df_compare$label <- c(
#   "Dealer     Time of Sale", 
#   "OEM      Time of Sale",
#   "Gov't      Time of Sale",
#   "Dealer     2 weeks later",
#   "OEM      2 weeks later",
#   "Gov't      2 weeks later",
#   "Dealer     6 weeks later",
#   "OEM      6 weeks later",
#   "Gov't      6 weeks later",
#   "Gov't      Time of Sale",
#   "Gov't           Tax Filing",
#   "Gov't           Tax Filing"
# )
# 
# # visual 2 - facet
# barplot_wtp2 <- df_compare %>%
#   #  arrange(desc(type), desc(timing), desc(label)) %>%
#   mutate(
#     label = factor(label, levels = rev(unique(label)))
#   ) %>%
#   ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper)) +
#   geom_col(width = 0.5) +
#   geom_errorbar(width = 0.3) +
#   #facet_wrap(~type, scales = "free_y", ncol = 1) +
#   facet_grid(type~., scales = "free_y", space = "free") +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   labs(x = 'WTP ($1,000)', title = 'Willingness to pay vs. Sales Tax Exemption', 
#        tag = "Source      Timing") +
#   theme_bw() +
#   theme(strip.text.y = element_text(angle = 0),
#         axis.title.y = element_blank(),
#         plot.tag.position = c(.1, .95),
#         plot.tag = element_text(size=9, face = "italic")) + 
#   coord_cartesian(clip = "off") 
# barplot_wtp2
# 
# # Save plots 
# ggsave(
#   filename = here('figs', 'wtp_barplot2.png'), 
#   plot = barplot_wtp2, 
#   width = 5, height = 3
# )

#m1 no 50k-------------------------------------------------------------

# #plot
# df_compare <- wtp_ci
# cols <- c('mean', 'lower', 'upper')
# 
# 
# df_compare$label <- c(
#   "Gov't      Time of Sale",
#   "OEM      Time of Sale",
#   "Dealer     Time of Sale", 
#   "Gov't      2 weeks later",
#   "OEM      2 weeks later",
#   "Dealer     2 weeks later",
#   "Gov't      6 weeks later",
#   "OEM      6 weeks later",
#   "Dealer     6 weeks later",
#   "Gov't      Time of Sale",
#   "Gov't      Time of Sale",
#   "Gov't           Tax Filing",
#   "Gov't           Tax Filing"
# )
# 
# # visual 2 - facet
# barplot_wtp2_m1 <- df_compare %>%
#   #  arrange(desc(type), desc(timing), desc(label)) %>%
#   mutate(
#     label = factor(label, levels = rev(unique(label)))
#   ) %>%
#   ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper)) +
#   geom_col(width = 0.5, fill = "grey") +
#   geom_errorbar(width = 0.3) +
#   #facet_wrap(~type, scales = "free_y", ncol = 1) +
#   facet_grid(type~., scales = "free_y", space = "free") +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   labs(x = 'Value ($)', title = "Value of Incentive Relative to Rebate (Time of Sale, gov't)", 
#        tag = "Source      Timing") +
#   theme_bw() +
#   theme(strip.text.y = element_text(angle = 0),
#         axis.title.y = element_blank(),
#         plot.tag.position = c(.1, .95),
#         plot.tag = element_text(size=9, face = "italic")) + 
#   coord_cartesian(clip = "off") 
# barplot_wtp2_m1
# 
# # Save plots 
# ggsave(
#   filename = here('figs', 'wtp_barplot2_m1.png'), 
#   plot = barplot_wtp2_m1, 
#   width = 6.5, height = 4
# )
# # Unused plots----------------------------------------------------------------------------
# # Plot results
# 
# # Separate coefficient CIs by attribute 
# wtp_taxCredit <- wtp_ci %>% filter((str_detect(par,'taxCredit')))
# wtp_taxDeduction <- wtp_ci %>% filter((str_detect(par,'taxDeduction')))
# wtp_rebate <- wtp_ci %>% filter((str_detect(par,'rebate')))

# # Compare WTP for changes in all attributes 
# 
# # WTP for:
# #tax credit
# 
# df_compare_taxCredit <- wtp_taxCredit
# cols <- c('mean', 'lower', 'upper')
# df_compare_taxCredit$label <- c(
#   "Incentive Type:\n Sales Tax Exemption vs. Tax Credit \n(at tax filing)", "Time of Sale"
# )
# 
# barplot_wtp_taxCredit <- df_compare_taxCredit %>% 
#   ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper)) +
#   geom_col(width = 0.5, fill = 'gray') +
#   geom_errorbar(width = 0.3) +
#   labs(x = 'WTP ($1,000)', y = 'Attriubte') +
#   theme_bw()
# 
# # Save plots 
# ggsave(
#   filename = here('figs', 'wtp_barplot_taxCredit.png'), 
#   plot = barplot_wtp_taxCredit, 
#   width = 5, height = 3
# )
