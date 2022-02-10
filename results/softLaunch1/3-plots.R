source("2-model.R")

# Read in the estimated models
partworth <- readRDS(here::here('partworth.Rds'))

# Get standard error data frame
se <- do.call(rbind, lapply(partworth, getSE)) %>%
  mutate(type = case_when(
    str_detect(coef, "type_") ~ "type",
    str_detect(coef, "source_") ~ "source_rebate",
    str_detect(coef, "timing_rebate") ~ "timing_rebate",
    str_detect(coef, "timing_taxCredit_") ~ "timing_taxCredit",
    TRUE ~ coef),
    size = size / 10^3
  )

# Plot 

error <- se %>%
  ggplot() +
  geom_point(
    aes(x = size, y = se, group = coef, color = coef),
    size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
  facet_wrap(~type, nrow = 1) +
  theme_minimal_grid() +
  expand_limits(x = 0, y = 0) +
  labs(
    color = "Model type",
    x = "Sample size (1,000)",
    y = "Standard error"
  )

ggsave("error.png", error, width = 12, height = 3)
