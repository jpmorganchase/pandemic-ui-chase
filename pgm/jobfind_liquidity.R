# jobfind_liquidity.R
# Author: Katie Zhang
# This runs robustness checks on a number of controls. 
# This runs regressions interacting with liquidity variable, which is measured
# as pre-period balance. 
# Note that the dfs/output names are "trim_5/wins5". It is actually winsorized
# at 10% not 5%, but we previously did it with 5% so this is just legacy code

# INS
base_reg <- "exit_ui ~ PctChange * SuppAvail"

dfs_used_throughout_jobfind_liquidity <-
  # dataframe name + script where it was last generated in the current form
  tribble(~df_name, ~script_where_generated,
          "all_states_pandemic_ctrls", "rep_rate_prep.R",
          "all_states_december_ctrls", "rep_rate_prep.R",
          "asset_terciles_exp", "control_prep.R",
          "asset_terciles_ons", "control_prep.R")

# OUTS
reg_output_liq_ctrls_outputs_list <-
  c(str_c(path_out, "table_liq_ctrls.txt"),
    str_c(path_out, "table_liq_ctrls_exp_wins5.tex"),
    str_c(path_out, "table_liq_ctrls_ons_wins5.tex"),
    str_c(path_out, "table_liq_ctrls_exp_wins5.html"),
    str_c(path_out, "table_liq_ctrls_ons_wins5.html"))

collapsed_dliq_reg_tables_for_tablebook <-
  c("reg_output_liq_ctrls_exp_collapsed",
    "reg_output_liq_ctrls_ons_collapsed")

################################################################################
# 0. SET UP ----
liq_bal_df <- function(df, nominal_var, terciles_df) {
  # this function brings in the tercile of each customer in terms of pre-period
  # balance, then also standardizes so that when we ctrl for the (std) pre-bal
  # in a regression we can interpret it in standard deviation changes
  df %>%
    transmute(week_start_date, cust_number, per_change, SuppAvail, post,
              exit_labor, exit_ui, start_recall, vin_all, pan_ben,
              pre_bal = !!sym(nominal_var)) %>%
    left_join(terciles_df %>%
                select(cust_number, contains("quantile")),
              by = "cust_number") %>%
    mutate(PctChange = per_change,
           # asset measure: (std) pre-bal
           std_pre_bal = (pre_bal - mean(pre_bal, na.rm = TRUE)) / sd(pre_bal, na.rm = TRUE)) %>%
    ungroup()
}

exp_liq_bal_trim_5 <- all_states_pandemic_ctrls %>%
  liq_bal_df(., "pre_bal_exp", asset_terciles_exp) 

ons_liq_bal_trim_5 <- all_states_december_ctrls %>%
  liq_bal_df(., "pre_bal_ons", asset_terciles_ons)

# 1. REGRESSIONS ----
liq_ctrl_regs <- function(data_name, ctrl, output) {
  # Note: only running on 'new job' sample
  df <- get(data_name) %>%
    filter(exit_labor == 1) %>%
    mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
    na.omit()
  
  # Generate summary stats to add in rows at end of table
  row_vals <- df %>%
    ungroup() %>%
    summarise(ctrl_mean = mean(pre_bal, na.rm = TRUE),
              ctrl_min = min(pre_bal, na.rm = TRUE),
              ctrl_max = max(pre_bal, na.rm = TRUE))
  
  ctrl_mean <- row_vals %>% pull(ctrl_mean)
  ctrl_min  <- row_vals %>% pull(ctrl_min)
  ctrl_max  <- row_vals %>% pull(ctrl_max)
  
  # We want to run 3 regressions:
  # 1. the base regression
  # 2. base + ctrl for assets
  # 3. triple diff
  reg1 <- felm(as.formula(str_c(base_reg, " |0|0| cust_number")), df)
  reg2 <- felm(as.formula(str_c(base_reg, " + ", ctrl, " |0|0| cust_number")), df)
  reg3 <- felm(as.formula(str_c(base_reg, " + SuppAvail * PctChange * ", ctrl, " |0|0| cust_number")), df)
  
  # Generate stats for above/below mean magnitudes
  disincentive_coef <- reg3 %>%
    broom::tidy() %>%
    filter(term == "PctChange:SuppAvailTRUE") %>%
    pull(estimate)
  
  triple_coef <- reg3 %>%
    broom::tidy() %>%
    filter(term == "PctChange:SuppAvailTRUE:std_pre_bal") %>%
    pull(estimate)
  
  # Generate table
  table <- mod_stargazer(reg1, reg2, reg3,
                         type = output,
                         dep.var.labels.include = FALSE,
                         digits = 4,
                         keep.stat = "n",
                         column.labels = c("Base", "Control for liquidity", "Triple difference"),
                         add.lines = list(c("Mean pre-period balance", "-", round(ctrl_mean, 2), "-"),
                                          c("Min pre-period balance", "-", round(ctrl_min, 2), "-"),
                                          c("Winsorized Max pre-period balance", "-", round(ctrl_max, 2), "-"),
                                          c("PctChange*SuppAvail if balance 1 sd above mean", "", "",
                                            round(disincentive_coef + triple_coef, 4)),
                                          c("PctChange*SuppAvail if balance 1 sd below mean", "", "",
                                            round(disincentive_coef - triple_coef, 4)),
                                          c("Number of Households", 
                                            reg_cluster_count(reg1),
                                            reg_cluster_count(reg2),
                                            reg_cluster_count(reg3))),
                         notes.label = "",
                         float = FALSE,
                         omit.table.layout = "n")
  
  list(table, disincentive_coef, triple_coef)
}

reg_setup_liq_ctrls <-
  cross_df(list(data = list("exp_liq_bal_trim_5",
                            "ons_liq_bal_trim_5"),
                ctrl_var = list("std_pre_bal"),
                output = list("text",
                              "html",
                              "latex"))) %>%
  arrange(desc(output), data)

# dataframe with all regression results
reg_output_liq_ctrls <-
  reg_setup_liq_ctrls %>%
  mutate(fitted = pmap(list(data, ctrl_var, output), liq_ctrl_regs),
         sg_table = map(fitted, 1), 
         disincentive_coef = map(fitted, 2),
         triple_diff_coef = map(fitted, 3),
         fitted = NULL)

# output to txt
table_labels <- c("Table 1: Expiration, standardized pre-period balance, winsorizing top 10%\n",
                  "\n\nTable 2: Onset, standardized pre-period balance, winsorizing top 10%\n")

raw_text <- "SuppAvailTRUE:Std Balance"
replacement_text <-"SuppAvail*Std Balance"
spaces_num <- nchar(raw_text) - nchar(replacement_text)

reg_output_liq_ctrls$sg_table[1:2] %>%
  map_chr( ~ str_c(.x, collapse = "\n")) %>%
  str_c(table_labels, .) %>%
  str_c(collapse = "\n") %>%
  str_replace_all("std_pre_bal",
                  "Std Balance") %>%
  str_replace_all("PctChange:",
                  "PctChange*") %>%
  str_replace_all(fixed(raw_text),
                  str_c(replacement_text,
                        strrep(" ", spaces_num))) %>%
  writeLines(str_c(path_out, "table_liq_ctrls.txt"))

### output to tex
##expiry
modify_liq_ctrls_exp <- reg_output_liq_ctrls$sg_table[[3]] %>%
  str_replace_all(fixed("std\\_pre\\_bal"), "Std Balance") %>%
  str_replace_all(fixed("SuppAvailTRUE:"), "SuppAvail*") %>%
  str_replace_all("PctChange:", "PctChange*") %>%
  str_replace("\\[-1.8ex]\\\\hline", "\\[-1.8ex]\\\\toprule") %>%
  str_replace("\\\\cline\\{2-4\\}", "\\\\cmidrule\\{2-4}") 
# Delete an extra hline and a couple of other stats that are not used:
modify_liq_ctrls_exp <- modify_liq_ctrls_exp[-c(37:39, 44)]
#Edit the hlines to fit the booktabs format
modify_liq_ctrls_exp[c(6, 11, 36, 41)] <- str_replace(modify_liq_ctrls_exp[c(6, 11, 36, 41)], 
                                                      "hline", c("toprule", "midrule", "midrule", "bottomrule"))
modify_liq_ctrls_exp  %>%
  writeLines(str_c(path_out, "table_liq_ctrls_exp_wins5.tex"))

##onset
modify_liq_ctrls_onset <- reg_output_liq_ctrls$sg_table[[4]] %>%
  str_replace_all(fixed("std\\_pre\\_bal"), "Std Balance") %>%
  str_replace_all(fixed("SuppAvailTRUE:"), "SuppAvail*") %>%
  str_replace_all("PctChange:", "PctChange*") %>%
  str_replace("\\[-1.8ex]\\\\hline", "\\[-1.8ex]\\\\toprule") %>%
  str_replace("\\\\cline\\{2-4\\}", "\\\\cmidrule\\{2-4}") 
# Delete an extra hline and a couple of other stats that are not used:
modify_liq_ctrls_onset <- modify_liq_ctrls_onset[-c(37:39, 44)]
#Edit the hlines to fit the booktabs format
modify_liq_ctrls_onset[c(6, 11, 36, 41)] <- str_replace(modify_liq_ctrls_onset[c(6, 11, 36, 41)], 
                                                        "hline", c("toprule", "midrule", "midrule", "bottomrule"))
modify_liq_ctrls_onset %>%
  writeLines(str_c(path_out, "table_liq_ctrls_ons_wins5.tex"))

### output to html
reg_output_liq_ctrls$sg_table[[5]] %>%
  str_replace_all(fixed("std_pre_bal"), "Std Balance") %>%
  str_replace_all(fixed("SuppAvailTRUE:"), "SuppAvail*") %>%
  str_replace_all("PctChange:", "PctChange*") %>%
  writeLines(str_c(path_out, "table_liq_ctrls_exp_wins5.html"))

reg_output_liq_ctrls$sg_table[[6]] %>%
  str_replace_all(fixed("std_pre_bal"), "Std Balance") %>%
  str_replace_all(fixed("SuppAvailTRUE:"), "SuppAvail*") %>%
  str_replace_all("PctChange:", "PctChange*") %>%
  writeLines(str_c(path_out, "table_liq_ctrls_ons_wins5.html"))

# formatting for inclusion in tablebook
reg_output_liq_ctrls_exp_collapsed <-
  c("\n\nTable X: Disincentive by Liquidity (Expiration)",
    reg_output_liq_ctrls$sg_table[1] %>%
      map_chr( ~ str_c(.x, collapse = "\n"))) %>% 
  str_c(collapse = "\n")

reg_output_liq_ctrls_ons_collapsed <-
  c("\n\nTable X: Disincentive by Liquidity (Onset",
    reg_output_liq_ctrls$sg_table[2] %>%
      map_chr( ~ str_c(.x, collapse = "\n"))) %>% 
  str_c(collapse = "\n")

# for minimum aggregation
n_table_liq_ctrls_exp_wins5 <- exp_liq_bal_trim_5 %>%
  filter(exit_labor == 1) %>%
  nrow()
n_table_liq_ctrls_ons_wins5 <- ons_liq_bal_trim_5 %>%
  filter(exit_labor == 1) %>%
  nrow()

# 2. BINSCATTERS ----
# create df with values used in producing binscatter plots
produce_binscatter_df <- function(reg_df, tercile_n) {
  tercile_subset_df <- get(reg_df) %>% 
    filter(pre_bal_quantile == tercile_n)
  
  temp_agg_df <- tercile_subset_df %>%
    filter(exit_labor == 1) %>%
    mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
    make_aggregate()
  
  if (grepl("exp", reg_df)) {
    temp_agg_df <- temp_agg_df %>%
      mutate(per_change = -per_change)
  } else {
    temp_agg_df <- temp_agg_df
  }
  
  for_intercept <- temp_agg_df %>%
    ungroup() %>%
    summarise(x = mean(per_change),
              y = mean(estimate))
  
  grand_mean <- tercile_subset_df %>% 
    filter(exit_labor == 1,
           !(exit_ui == 1 & start_recall == 1)) %>%
    lm(exit_ui ~ post, .) %>%
    broom::tidy() %>%
    slice(2) %>%
    pull(estimate)
  
  base_reg_tercile <- tercile_subset_df %>%
    mutate(PctChange = per_change) %>%
    filter(exit_labor == 1) %>%
    mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
    lm(base_reg, .) %>%
    broom::tidy() %>%
    dplyr::filter(term == "PctChange:SuppAvailTRUE")
  
  slope <- base_reg_tercile %>% pull(estimate)
  new_job_se <- base_reg_tercile %>% pull(std.error)
  
  # make_binscatter_aggregate
  temp_agg_df %>%
    mutate(estimate = estimate - for_intercept$y + grand_mean) %>%
    # store values in df
    mutate(tercile_num = tercile_n,
           intercept_val = grand_mean - slope * for_intercept$x,
           slope = slope,
           new_job_se = new_job_se)
}


expiry_binscatter_df <- pmap_dfr(list("exp_liq_bal_trim_5", c(1, 2, 3)),
                                  produce_binscatter_df)

onset_binscatter_df <- pmap_dfr(list("ons_liq_bal_trim_5", c(1, 2, 3)),
                                 produce_binscatter_df)

# stick three lines + three sets of scatter dots on one plot
plot_three_binscatter_fn <- function(df) {
  df %>%
    ggplot() +
    aes(per_change, estimate, colour = as.factor(tercile_num)) +
    geom_point(size = 2) +
    labs(subtitle = "Change in average exit rate to new job") +
    geom_abline(slope = df %>% filter(tercile_num == 1) %>% distinct(slope) %>% pull(),
                intercept = df %>% filter(tercile_num == 1) %>% distinct(intercept_val) %>% pull(),
                colour = jpmci_colors[1]) +
    geom_abline(slope = df %>% filter(tercile_num == 2) %>% distinct(slope) %>% pull(),
                intercept = df %>% filter(tercile_num == 2) %>% distinct(intercept_val) %>% pull(),
                colour = jpmci_colors[2]) +
    geom_abline(slope = df %>% filter(tercile_num == 3) %>% distinct(slope) %>% pull(),
                intercept = df %>% filter(tercile_num == 3) %>% distinct(intercept_val) %>% pull(),
                colour = jpmci_colors[3]) +
    fte_theme() +
    scale_color_manual(name = "Pre-treatment balance tercile",
                       values = jpmci_colors[1:3]) +
    theme(legend.title = element_text(colour = greys[8]),
          legend.position = c(0, 0),
          legend.justification = c("left", "bottom"),
          legend.key = element_rect(colour = NA, fill = "transparent"),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA))
}
