library(logitr)
library(mlogit)

make_coef_table <- function(model) {

  summary <- coef(summary(model)) %>%
    round(3) %>%
    rownames_to_column() %>%
    rename(
      "coefficients" = "rowname",
      "prob" = "Pr(>|z|)") %>%
    mutate(
      sig = ifelse(
        prob <= 0.001,'***', ifelse(
        prob > 0.001 & prob <= 0.01, '**', ifelse(
        prob > 0.01 & prob <= 0.05, '*', ifelse(
        prob > 0.05 & prob <= 0.1, '.', '   ')))),
      sig2 = ifelse(
        prob <= 0.001,'***', ifelse(
          prob > 0.001 & prob <= 0.01, '&nbsp;**', ifelse(
            prob > 0.01 & prob <= 0.05, '&nbsp;&nbsp;*', ifelse(
              prob > 0.05 & prob <= 0.1, '&nbsp;&nbsp;&nbsp;.', "&nbsp;&nbsp;&nbsp;&nbsp;"))))
    ) %>%
    mutate(
      Estimate = sprintf("%.3f", Estimate),
      `Std. Error` = sprintf("%.3f", `Std. Error`),
      table_format = ifelse(nchar(`Std. Error`) == 6 ,paste0(Estimate, " (", `Std. Error`, ") ", sig),
                            paste0(Estimate, "  (", `Std. Error`, ") ", sig)),
      table_format2 = ifelse(nchar(`Std. Error`) == 6 ,paste0(Estimate, " (", `Std. Error`, ") ", sig2),
                             paste0(Estimate, " (", `Std. Error`, ") ", sig2))
    ) %>%
    select("coefficients", "table_format2" )

    return(summary)
}

make_coef_table2 <- function(model) {
  
  summary <- coef(summary(model)) %>%
    round(3) %>%
    rownames_to_column() %>%
    rename(
      "coefficients" = "rowname",
      "prob" = "Pr(>|z|)") %>%
    mutate(
      sig = ifelse(
        prob <= 0.001,'***', ifelse(
          prob > 0.001 & prob <= 0.01, '**', ifelse(
            prob > 0.01 & prob <= 0.05, '*', ifelse(
              prob > 0.05 & prob <= 0.1, '.', '   ')))),
      sig2 = ifelse(
        prob <= 0.001,'***', ifelse(
          prob > 0.001 & prob <= 0.01, '**', ifelse(
            prob > 0.01 & prob <= 0.05, '*', ifelse(
              prob > 0.05 & prob <= 0.1, '.', "XXX"))))
    ) %>%
    mutate(
      Estimate = sprintf("%.3f", Estimate),
      `Std. Error` = sprintf("%2.3f", `Std. Error`),
      `Std. Error` = paste0("(", `Std. Error`, ")"),
      table_format = ifelse(nchar(`Std. Error`) == 6 ,paste0(Estimate, " (", `Std. Error`, ") ", sig),
                            paste0(Estimate, "  (", `Std. Error`, ") ", sig)),
      table_format2 = ifelse(nchar(`Std. Error`) == 6 ,paste0(Estimate, " (", `Std. Error`, ") ", sig2),
                             paste0(Estimate, " (", `Std. Error`, ") ", sig2))
    ) %>%
    select("coefficients", "Estimate", "Std. Error", "sig" )
  
  return(summary)
}



