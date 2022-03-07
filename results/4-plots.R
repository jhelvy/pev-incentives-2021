# Plots for WTP models

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

load(here::here("wtp", "wtp_ci.RData"))

# plot
df_compare <- wtp_ci
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

# library(showtext)

# font_add_google("Fira Sans Condensed", "fira")

# showtext_opts(dpi = 300)
# showtext_auto(enable = TRUE)
col_value <- "grey70"
font_main <- "Fira Sans Condensed"
# col_loss <- "#FF3B3F"

barplot_wtp2_m1_50k <- df_compare %>%
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

# Save plots
ggsave(
  filename = here("figs", "wtp_barplot2_m1_50k.pdf"),
  plot = barplot_wtp2_m1_50k,
  width = 6.5, height = 4, device = cairo_pdf
)



