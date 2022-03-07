# Visualize results of estimated WTP space model - demos

# Load libraries
library(logitr)
library(tidyverse)
library(here)
library(cowplot)
library(maddTools)
library(forcats)
library(ggrepel)

# Low/High Income

load(here::here("wtp", "wtp_ci_inc.RData"))

#--plot--

# choose data & add labels for plotting
df_compare <- wtp_ci_inc
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

barplot_wtp2_inc <- df_compare %>%
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

ggsave(
  filename = here("figs", "wtp_barplot2_inc_50k.pdf"),
  plot = barplot_wtp2_inc,
  width = 6.5, height = 4, device = cairo_pdf
)

