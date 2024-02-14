# SPDX-License-Identifier: MIT
# MPC by Liquidity
# Date: 17 October, 2023
# Author: Rupsha Debnath
# Create a new version of Table 2 with Std Errors and count of observations
options(scipen=999)
# Read in the apc_by_liquidity files ----
apc_high_liq <- read.csv(str_c(path_out, 
                               'apcs_table_ivreg_high_liquidity.csv')) %>%
  filter(numerator == "Spend (total)", denominator == "income",
         design != "LWA") %>%
  select(-c(numerator, denominator)) %>% 
  mutate(liquidity = "high")
apc_low_liq <- read.csv(str_c(path_out, 
                              'apcs_table_ivreg_low_liquidity.csv')) %>%
  filter(numerator == "Spend (total)", denominator == "income",
         design != "LWA")%>%
  select(-c(numerator, denominator)) %>%
  mutate(liquidity = "low") 

# Create output tex table ----
# VERSION 2:
calc_se <- function(se1, se2){
  # std error of the difference
  std_error_diff <- sqrt((se1^2) + (se2^2))
  #return standard error
  return(round(std_error_diff,3))
}

mpc_output_v2 <- apc_low_liq %>%
  rbind(., apc_high_liq) %>%
  pivot_wider(names_from = "liquidity",
              values_from = c("apc", "se", "n_obs")) %>%
  select(design, apc_high, se_high, n_obs_high, apc_low, se_low, n_obs_low) %>%
  mutate(diff_mpc = round(as.numeric(apc_low) - as.numeric(apc_high), 2),
         std_error = calc_se(se1 = se_low, se2 = se_high),
         design = as.character(design)) %>%
  select(-c(n_obs_high, n_obs_low))

edited_mpc_output_v2 <- mpc_output_v2 %>%
  mutate(high_mpc_se = paste0(round(apc_high,2), " (", round(se_high,2), ")"),
         low_mpc_se = paste0(round(apc_low,2), " (", round(se_low, 2), ")"),
         diff_mpc_se = paste0(round(diff_mpc,2), " (", round(std_error,2), ")"),
         design = c("Waiting for benefits", "$600 expiration",
                    "$300 onset",
                    "$300 expiration Sept states",
                    "$300 expiration June states",
                    "$300 expiration June vs. Sept states")) %>%
  select(design, high_mpc_se, low_mpc_se, diff_mpc_se)

colnames(edited_mpc_output_v2) <- c("Design", "High Liquidity MPC",
                             "Low Liquidity MPC", "Difference")

write.csv(n_values, (str_c(path_out,"n_obs_by_liq.csv")))

# Version 2 ----
# html tex files:
table_apcs_by_liquidity_tex_file_V2 <- edited_mpc_output_v2 %>%
  stargazer(., digits=2, summary=FALSE, type="latex", rownames = FALSE) 

edited_mpc_output_V2 <- table_apcs_by_liquidity_tex_file_V2 %>%
  str_subset("\\{table\\}|caption|label", negate = TRUE) %>%
  str_replace("cccc", "lccc") %>%
  str_replace_all("hline", c("toprule","toprule", "midrule", "bottomrule")) %>%
  writeLines(str_c(path_out, "mpc_table_liq_w_stderr_V2.tex"))

table_apcs_by_liquidity_tex_file_V2 <- edited_mpc_output_v2 %>%
  stargazer(., digits=2, summary=FALSE, type="html", rownames = FALSE) %>%
  str_replace_all(fixed("expiration "), "$300 expiration ") %>%
  str_replace_all(fixed("expiration</td>"), "$600 expiration</td>") %>%
  str_replace_all(fixed("onset"), "$300 onset") %>%
  writeLines(str_c(path_out, "mpc_table_liq_w_stderr_V2.html"))

# Aggregation standards ----
agg_std <- data.frame("Item" = "Table 2 Version",
                      "Title" = "Marginal Propensity to Consume out of Unemployment Benefits by Liquidity",
                      "Output file" = "mpc_table_liq_w_stderr" ,
                      "Min Cell Size" = min(n_values$value), "Code File" = "pgm/table2_V2.R")
write.csv(agg_std, str_c(path_out, "agg_std_table2.csv"))
