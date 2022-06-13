# Visualize results of estimated WTP space model - COL

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)
library(maddTools)
library(forcats)
library(ggrepel)

# Plot WTP for changes in all attributes

load(here::here("wtp", "wtp_ci_col.RData"))

# plot
df_compare <- wtp_ci_col
cols <- c("mean", "lower", "upper")


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

library(showtext)

font_add_google("Fira Sans Condensed", "Fira Sans Condensed")
showtext_auto() 
# windows()

# showtext_opts(dpi = 300)
# showtext_auto(enable = TRUE)
col_value <- "grey70"
font_main <- "Fira Sans Condensed"
# col_loss <- "#FF3B3F"

barplot_wtp2_m1_50k_col <- df_compare %>%
  mutate(
    label = factor(label, levels = rev(unique(label))),
    type = factor(type, levels = c(
      "Rebate", "Sales Tax", "Tax Credit", "Tax\nDeduction"
    ))
  ) %>%
  ggplot(aes(x = mean, y = label, xmin = lower, xmax = upper)) +
  geom_col(width = 0.5, fill = col_value) +
  geom_errorbar(width = 0.3, color = "grey30") +
  facet_grid(type ~ ., scales = "free_y", space = "free") +
  geom_vline(xintercept = 0, size = 0.1) +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    x = "Value of Incentive Relative to Immediate Rebate from Gov't ($USD)",
    title = " ",
    tag = "Source          Timing"
  ) +
  theme_bw(base_family = font_main) +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.grid.major = element_line(size = 0.3, colour = "grey90"),
    strip.text.y = element_text(angle = 0),
    axis.title.y = element_blank(),
    plot.tag.position = c(0.09, 0.95),
    plot.tag = element_text(size = 9, face = "italic", family = font_main)
  ) +
  coord_cartesian(clip = "off")

barplot_wtp2_m1_50k_col

# Save plots
ggsave(
  filename = here("figs", "wtp_barplot2_m1_50k_col.pdf"),
  plot = barplot_wtp2_m1_50k_col,
  width = 6.5, height = 4, device = cairo_pdf
)



# Low/High Income

load(here::here("wtp", "wtp_ci_inc_col.RData"))

#--plot--

# choose data & add labels for plotting
df_compare <- wtp_ci_inc_col
cols <- c("mean", "lower", "upper")

# remove 2 & 6 weeks
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

# library(showtext)

# font_add_google("Fira Sans Condensed", "fira")

# showtext_opts(dpi = 300)
# showtext_auto(enable = TRUE)
col_value <- "grey70"
col_loss <- "#FF3B3F"
font_main <- "Fira Sans Condensed"

barplot_wtp2_inc_col <- df_compare %>%
  mutate(
    label = factor(label, levels = rev(unique(label))),
    demo_label = factor(demo_label, levels = rev(unique(demo_label))),
    type = factor(type, levels = c(
      "Rebate", "Sales Tax", "Tax Credit", "Tax\nDeduction"
    ))
  ) %>%
  ggplot(
    aes(x = mean, y = label, xmin = lower, xmax = upper, group = demo_label)) +
  geom_col(
    aes(fill = demo_label),
    width = 0.7, 
    position = position_dodge2(width = 0.9, reverse = TRUE)
  ) +
  geom_errorbar(
    width = 0.7, color = "grey30",
    position = position_dodge2(width = 0.9, reverse = TRUE)
  ) +
  facet_grid(type ~ ., scales = "free_y", space = "free") +
  scale_fill_manual(values = c(col_value, col_loss)) +
  scale_x_continuous(labels = scales::dollar) +
  geom_vline(xintercept = 0, size = 0.1) +
  labs(
    x = "Value of Incentive Relative to Immediate Rebate from Gov't ($ USD)",
    title = " ",
    tag = "Source          Timing",
    fill = "Annual Household Income"
  ) +
  theme_bw(base_family = font_main) +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    panel.grid.major = element_line(size = 0.3, colour = "grey90"),
    strip.text.y = element_text(angle = 0),
    axis.title.y = element_blank(),
    plot.tag.position = c(0.08, .95),
    plot.tag = element_text(size = 9, face = "italic", family = font_main),
    legend.position = "bottom"
  ) +
  coord_cartesian(clip = "off")

barplot_wtp2_inc_col

ggsave(
  filename = here("figs", "wtp_barplot2_inc_50k_col.pdf"),
  plot = barplot_wtp2_inc_col,
  width = 6.5, height = 4, device = cairo_pdf
)

