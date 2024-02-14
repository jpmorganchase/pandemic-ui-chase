# SPDX-License-Identifier: MIT
# INS
dfs_used_throughout_rep_rate_tables <-
  # dataframe name + script where it was last generated in the current form
  tribble(~df_name, ~script_where_generated,
          "all_states_pandemic", "rep_rate_prep.R",
          "all_states_december", "rep_rate_prep.R",
          "all_states_pandemic_ctrls", "rep_rate_prep.R",
          "all_states_december_ctrls", "rep_rate_prep.R")

last_date_run_file <- "2023-02-23"
path_totables <- str_c(path_out)

# OUTS
table_effects_summary_csv <- str_c(path_out,  "table_effects_summary.csv")
table_effects_summary_tex <- str_c(path_out,  "table_effects_summary.tex")
table_main_coefficients_txt <- str_c(path_out,  "table_main_coefficients.txt")
table_main_coefficients_tex <- str_c(path_out,  "table_main_coefficients.tex")
table_regression_alt_txt <- str_c(path_out,  "table_regression_alternative_outcome.txt")
table_regression_alt_exp_tex <- str_c(path_out,  "table_regression_alternative_outcome_expiry.tex")
table_regression_alt_ons_tex <- str_c(path_out,  "table_regression_alternative_outcome_onset.tex")
table_ctrl_exp_txt <- str_c(path_out,  "table_ctrl_expiration.txt")
table_ctrl_exp_tex <- str_c(path_out,  "table_ctrl_expiration.tex")
table_ctrl_ons_txt <- str_c(path_out,  "table_ctrl_onset.txt")
table_ctrl_ons_tex <- str_c(path_out,  "table_ctrl_onset.tex")
table_all_txt <- str_c(path_out,  last_date_run_file, "table_all.txt")

collapsed_reg_tables_for_tablebook <-
  c("regs_1_collapsed",
    "regressions_collapsed",
    "regressions_alt",
    "reg_control_expire_collapsed",
    "reg_control_onset_collapsed")

################################################################################
# summary table of micro & macro effects ----
make_row_table_2 <- function(df, size) {
  average_per_change <- df %>%
    group_by(cust_number) %>%
    slice(1) %>%
    ungroup() %>%
    summarise(mean(per_change)) %>%
    pull()
  
  chg_per_100 <- df %>%
    ungroup() %>%
    summarise(mean(0.9 * 2 * 100 / (no_sup_stat_benefits * 2 + 0.9 * 100))) %>%
    pull()
  
  if (min(df$week_start_date) <= as.Date("2020-06-07")) {
    # NOTE: estimate function is from `pgm/funcs/ui_functions.R`
    time_series <- -estimate(as.Date("2020-07-26"))
  } else {
    time_series <- estimate(as.Date("2020-12-27"), donut = 14)
  }
  
  if(size == "300 expire"){
    time_series <- NA
  } else {
    time_series <- time_series
  }
  
  cross_section <- lm(exit_ui ~ SuppAvail * per_change, df) %>% 
    broom::tidy() %>%
    filter(str_detect(term, "SuppAvailTRUE:per_change")) %>%
    pull(estimate)
  
  tibble(event = size,
         ts_exit = time_series,
         ts_p100 = ts_exit / (average_per_change / chg_per_100),
         average_per_change = average_per_change,
         cs_exit = cross_section * average_per_change,
         cs_p100 = cross_section * chg_per_100,
         chg_per_100 = chg_per_100)
}

regs_1 <- list(
  list(all_states_pandemic %>% 
         filter(exit_labor == 1) %>%
         mutate(exit_ui = (exit_ui == 1 & start_recall == 0)),
       all_states_december %>% 
         filter(exit_labor == 1) %>%
         mutate(exit_ui = (exit_ui == 1 & start_recall == 0))),
  c("600 expire", "300 onset")) %>%
  pmap_dfr(make_row_table_2)

write_csv(regs_1, str_c(path_out,  "table_effects_summary.csv"))

regs_1_temp <- read.csv(str_c(path_out,  
                              "table_effects_summary.csv")) %>% # for latex + outputting to table_all later
  select(-average_per_change, -chg_per_100) %>%
  mutate_at(vars(-("event")), ~ . * 100) %>%
  mutate_at(vars(-("event")), ~ round(., 2)) %>%
  mutate(event = str_c("$", event))
colnames(regs_1_temp) <- c("Effect of...", "Entire supplement", "per $100",
                           "Entire supplement", "per $100")
rownames(regs_1_temp) <- c()

regs_1_latex <-
    capture.output(print(xtable(regs_1_temp, digits = 2),
                         floating = FALSE, include.rownames=FALSE)) %>%
  str_replace("lrrrr", "lcccc") %>%
  str_replace(" \\\\hline", "\\\\hline \\\\\\\\[-1.8ex]") %>%
  append("\\\\[-1.8ex]\\toprule ", 3) %>%
  append(" & \\multicolumn{2}{c}{Macro effects} & \\multicolumn{2}{c}{Micro effects} \\\\", 5)

regs_1_latex[c(5, 8, 11)] <- str_replace(regs_1_latex[c(5, 8, 11)], 
                                         "hline", c("toprule", "midrule", "bottomrule"))

regs_1_latex %>% writeLines(str_c(path_out,  "table_effects_summary.tex"))

# main regression coefficients ----
expire_reg_df <- all_states_pandemic %>% 
  filter(exit_labor == 1) %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  mutate(PctChange = per_change,
         SuppAvail = !post)

reg_xsec_expire <-
  felm(exit_ui ~ PctChange * SuppAvail |0|0| cust_number , 
       expire_reg_df)

onset_reg_df <- all_states_december %>%
  filter(exit_labor == 1) %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  rename(PctChange = per_change,
         SuppAvail = post)

reg_xsec_onset <-
  felm(exit_ui ~ PctChange * SuppAvail |0|0| cust_number , 
       onset_reg_df)

(regressions <- list(reg_xsec_expire, reg_xsec_onset) %>%
    mod_stargazer(type = "text", digits = 4,
                  keep.stat = "n",
                  notes.label = "",
                  dep.var.labels = "Exit to new job",
                  add.lines = list(c("Event", "Expiration of $600", "Onset of $300"))) %>% # couldn't escape $
    str_replace(., "e:S", "e*S") %>%
    str_replace(., "600", "$600") %>%
    str_replace(., "300", "$300")) %>% 
  writeLines(str_c(path_out,  "table_main_coefficients.txt"))

#modify the latex output for table_main_coefficients to make it reflective of changes on the outside
modifytex_main_coeff <- mod_stargazer(reg_xsec_expire, reg_xsec_onset,
                   keep.stat = "n", 
                   notes.label = "",
                   dep.var.labels = "Exit to new job",
                   column.labels = c("Expiration of \\$600", "Onset of \\$300"),
                   add.lines = list(c("Number of Households", 
                                      reg_cluster_count(reg_xsec_expire),
                                      reg_cluster_count(reg_xsec_onset))),
                   float = FALSE,
                   omit.table.layout = "n") %>%
  str_replace("\\[-1.8ex]\\\\hline", "\\[-1.8ex]\\\\toprule") %>%
  str_replace("\\\\cline\\{2-3\\}", "\\\\cmidrule\\{2-3}")
# Delete an extra hline
modifytex_main_coeff <- modifytex_main_coeff[-c(28)]
# Integer subsetting to replace the hlines in the latex code with the proper booktabs version used in overleaf 
modifytex_main_coeff[c(6, 12, 25, 28)] <- str_replace(modifytex_main_coeff[c(6, 12, 25, 28)], 
                                                      "hline", c("toprule", "midrule", "midrule", "bottomrule"))
modifytex_main_coeff %>%
  writeLines(str_c(path_out,  "table_main_coefficients.tex"))

# alternative regression outcomes ----
#"exit_ui ~ state * SuppAvail + per_change * SuppAvail",
fit_4_samples <- function(df, formula){
  # This function fits a model defined in formula
  # to the 3 relevant samples:
  # exits with observed seperation
  # Exit to recall and Exit not to recall
  df1 <- df %>%
    filter(exit_labor == 1) %>%
    mutate(exit_ui = (exit_ui == 1 & start_recall == 0))
  
  df2 <-  df %>%
    filter(exit_labor == 1) %>%
    mutate(exit_ui = (exit_ui == 1 & start_recall == 1))
  
  list(not_recall = felm(as.formula(formula), df1),
       recall = felm(as.formula(formula), df2),
       sep = felm(as.formula(formula),
                  filter(df, exit_labor == 1)))
}

extract_model_reg <- function(data_name, formula, name, output_type){
  #produces a stargazer table for a given model and dataset
  # and extracts the relevant coefficient for use in plots
  
  models <- fit_4_samples(get(data_name), formula) 
  table <- models %>%
    mod_stargazer(type = output_type,
                  digits = 4, 
                  keep = "SuppAvailTRUE\\:per_change",
                  keep.stat = "n",
                  notes.label = "",
                  dep.var.labels.include = FALSE,
                  column.labels = c("New job", "Recall", "Total"),
                  add.lines = list(c("Number of Households", 
                                     reg_cluster_count(.$not_recall),
                                     reg_cluster_count(.$recall),
                                     reg_cluster_count(.$sep))),
                  float = FALSE,
                  omit.table.layout = "n")
  
  coeffs <- models %>%
    map("coefficients") %>% 
    as_tibble() %>%
    slice(4:n())
  
  new_job_se <- 
    coef(summary(models$not_recall))[ , "Cluster s.e."]["SuppAvailTRUE:per_change"] # this is janky, fix later
  colnames(coeffs) <- c("not_recall", "recall", "total")
  coeffs <- as_list(coeffs)
  min_n <- models$not_recall$N # notes that cols 1-3 have same N bc conditioning on separation
  list(table, coeffs, new_job_se, min_n) #want to avoid storing data in model object
}

regressions_output_df <- cross_df(list(formula = list("exit_ui ~ SuppAvail * per_change |0|0| cust_number"),
                                       data = list("all_states_pandemic",
                                                   "all_states_december"),
                                       output = list("text",
                                                     "latex"))) %>% 
  mutate(name = if_else(str_detect(data,
                                   "december"),
                        "Onset design (January 2021)",
                        "Expiry design (August 2020)"))

regressions_output <- regressions_output_df %>%
  mutate(fitted = pmap(list(data, formula, name, output), extract_model_reg),
         sg_table = map(fitted, 1), 
         coefficients = map(fitted, 2),
         new_job_se = map(fitted, 3),
         min_n = map(fitted, 4),
         fitted = NULL)

table_numbers <- str_c("Table ", seq(1:2), "\n") 

raw_text <- "SuppAvailTRUE:per_change"
replacement_text <-"SuppAvail*PctChange"
spaces_num <- nchar(raw_text) - nchar(replacement_text)

(regressions_alt <- regressions_output$sg_table[1:2] %>% 
    map_chr( ~ str_c(.x, collapse = "\n")) %>% 
    str_c(table_numbers, .) %>%
    str_c(collapse = "\n") %>%
    str_replace_all(fixed(raw_text),
                    str_c(replacement_text,
                          strrep(" ", spaces_num)))) %>% # keep old spacing
  writeLines(str_c(path_out,  "table_regression_alternative_outcome.txt"))

# Expiry .tex file modified
modify_reg_alt_expiry <- regressions_output$sg_table[[3]] %>% 
  str_replace_all(fixed("SuppAvailTRUE:per\\_change"), replacement_text) %>%
  str_replace("\\[-1.8ex]\\\\hline", "\\[-1.8ex]\\\\toprule") %>%
  str_replace("\\\\cline\\{2-5\\}", "\\\\cmidrule\\{2-5}")
# Delete an extra hline
modify_reg_alt_expiry <- modify_reg_alt_expiry[-c(18)]
#Edit the hlines to fit the booktabs format
modify_reg_alt_expiry[c(6, 11, 15, 18)] <- str_replace(modify_reg_alt_expiry[c(6, 11, 15, 18)], 
                                                       "hline", c("toprule", "midrule", "midrule", "bottomrule"))
modify_reg_alt_expiry %>%
  writeLines(str_c(path_out,  "table_regression_alternative_outcome_expiry.tex"))

# Onset .tex file modified
modify_reg_alt_onset  <- regressions_output$sg_table[[4]] %>% 
  str_replace_all(fixed("SuppAvailTRUE:per\\_change"), replacement_text)%>%
  str_replace("\\[-1.8ex]\\\\hline", "\\[-1.8ex]\\\\toprule") %>%
  str_replace("\\\\cline\\{2-5\\}", "\\\\cmidrule\\{2-5}") 
# Delete an extra hline
modify_reg_alt_onset <- modify_reg_alt_onset[-c(18)]
#Edit the hlines to fit the booktabs format
modify_reg_alt_onset[c(6, 11, 15, 18)] <- str_replace(modify_reg_alt_onset[c(6, 11, 15, 18)], 
                                                      "hline", c("toprule", "midrule", "midrule", "bottomrule"))
modify_reg_alt_onset %>%
  writeLines(str_c(path_out,  "table_regression_alternative_outcome_onset.tex"))

# add controls ----
fit_ctrls <- function(df, base_reg, output_type) {
  df <- df %>%
    filter(exit_labor == 1) %>%
    mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
    rename(PctChange = per_change)
  list(base = felm(as.formula(str_c(base_reg, " |0|0| cust_number")), df),
       state = felm(as.formula(str_c(base_reg, " + state * post_bin |0|0| cust_number")), df),
       age = felm(as.formula(str_c(base_reg, " + state * post_bin + age_bin * post_bin |0|0| cust_number")), df),
       ind = felm(as.formula(str_c(base_reg, " + state * post_bin + age_bin * post_bin + ind_factor * post_bin |0|0| cust_number")), df))  %>%
    mod_stargazer(type = output_type, 
              keep =  "PctChange\\:",
              digits = 4,
              keep.stat = "n",
              notes.label = "",
              column.labels = c("Exit to New Job"),
              column.separate = c(4),
              dep.var.labels.include = FALSE,
              add.lines = list(c("PctChange", rep("X", 4)),
                               c("SuppAvail", rep("X", 4)),
                               c("State*SuppAvail FE", " ", rep("X", 3)),
                               c("Age*SuppAvail FE", rep(" ", 2), rep("X", 2)),
                               c("Industry*SuppAvail FE", rep(" ", 3), "X"),
                               c("Number of Households", reg_cluster_count(.$base),
                                 reg_cluster_count(.$state), reg_cluster_count(.$age),
                                 reg_cluster_count(.$ind))),
              notes.align = "l",
              float = FALSE,
              omit.table.layout = "n") 
}

# robustness to controls ----
# expiration
(reg_control_expire <- all_states_pandemic_ctrls %>% 
   mutate(post_bin = SuppAvail) %>% 
   fit_ctrls("exit_ui ~ PctChange * SuppAvail", output_type = "text") %>%
   str_replace("PctChange\\:", "PctChange*")) %>% 
  writeLines(str_c(path_out,  "table_ctrl_expiration.txt"))

modify_expire_tex <- all_states_pandemic_ctrls %>% 
  mutate(post_bin = SuppAvail) %>%
  fit_ctrls("exit_ui ~ PctChange * SuppAvail", output_type = "latex") %>%
  str_replace("PctChange\\:SuppAvail", "PctChange*SuppAvail") %>%
  str_replace("\\[-1.8ex]\\\\hline", "\\[-1.8ex]\\\\toprule") %>%
  str_replace("\\\\cline\\{2-5\\}", "\\\\cmidrule\\{2-5}")
# Delete an extra hline
modify_expire_tex <- modify_expire_tex[-c(23)]
#integer subsetting to replace hlines with the appropriate booktabs version
modify_expire_tex[c(6, 11, 15, 23)] <- str_replace(modify_expire_tex[c(6, 11, 15, 23)], 
                                    "hline", c("toprule", "midrule", "midrule", "bottomrule"))
modify_expire_tex %>% writeLines(str_c(path_out,  "table_ctrl_expiration.tex"))

# onset
(reg_control_onset <- all_states_december_ctrls %>%
    filter(week_start_date <= end_window_reg_onset) %>%
  fit_ctrls("exit_ui ~ PctChange * SuppAvail", output_type = "text")%>%
    str_replace("PctChange\\:", "PctChange*")) %>% 
  writeLines(str_c(path_out,  "table_ctrl_onset.txt"))

modify_onset_tex <- all_states_december_ctrls %>%
  filter(week_start_date <= end_window_reg_onset) %>%
  fit_ctrls("exit_ui ~ PctChange * SuppAvail", output_type = "latex") %>%
  str_replace("PctChange\\:SuppAvail", "PctChange*SuppAvail")%>%
  str_replace("\\[-1.8ex]\\\\hline", "\\[-1.8ex]\\\\toprule") %>%
  str_replace("\\\\cline\\{2-5\\}", "\\\\cmidrule\\{2-5}")
# Delete an extra hline
modify_onset_tex <- modify_onset_tex[-c(23)]
#integer subsetting to replace hlines with the appropriate booktabs version
modify_onset_tex[c(6, 11, 15, 23)] <- str_replace(modify_onset_tex[c(6, 11, 15, 23)], 
                                                  "hline", c("toprule", "midrule", "midrule", "bottomrule"))
modify_onset_tex %>% writeLines(str_c(path_out,  "table_ctrl_onset.tex"))

# formatting for inclusion in tablebook ----
big_header <- c(strrep(" ", nchar(colnames(regs_1_temp)[1]) + 20), 
                format("Macro effects",
                       width = ( nchar(colnames(regs_1_temp)[2]) +
                                   nchar(colnames(regs_1_temp)[3]) + 3 ),
                       justify = "centre"),
                format("Micro effects",
                       width = (nchar(colnames(regs_1_temp)[4]) +
                                  nchar(colnames(regs_1_temp)[5]) + 3 ), 
                       justify = "centre")) %>% 
  str_c(collapse = "")

regs_1_collapsed <-
  c("Table X: Micro and macro disincentives\n",
    big_header,
    capture.output(print(regs_1_temp, print.gap = 5))[1],
    strrep("-", nchar(big_header)),
    capture.output(print(regs_1_temp, print.gap = 5, row.names = FALSE))[2],
    capture.output(print(regs_1_temp, print.gap = 5, row.names = FALSE))[3])


regressions_collapsed <-
  c("\n\nTable X: Regression Estimates for Effect of Expanded Benefits on Job-Finding",
    regressions %>%
      map_chr( ~ str_c(.x, collapse = "\n"))) %>% 
  str_c(collapse = "\n")

reg_control_expire_collapsed <-
  c("\n\nTable X(a): Micro Effect of Expanded Benefits: Robustness to Controls (Expiration)",
    reg_control_expire%>%
      map_chr( ~ str_c(.x, collapse = "\n"))) %>% 
  str_c(collapse = "\n")

reg_control_onset_collapsed <-
  c("\n\nTable X(b): Micro Effect of Expanded Benefits: Robustness to Controls (Onset)",
  reg_control_onset %>%
    map_chr( ~ str_c(.x, collapse = "\n"))) %>% 
  str_c(collapse = "\n")

