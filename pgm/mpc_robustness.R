# SPDX-License-Identifier: MIT
# mpc_robustness.R

# INS
dfs_used_throughout_mpc_robustness <-
  # dataframe name + script where it was last generated in the current form
  tribble(~df_name, ~script_where_generated,
          "df_demo_src_wins", "ui_eip_data_build.R", # read in from pgm/spend_build.R
          "cust_ui_onset", "spend_build.R",
          "cust_nonui", "spend_build.R",
          "cust_thru_aug", "spend_build.R",
          "cust_thru_jan", "spend_build.R",
          "cust_nonui", "spend_build.R",
          "cust_get_lwa", "spend_build.R",
          "cust_eip_touse", "spend_build.R",
          "cust_eip_rounds_touse", "spend_build.R",
          "cust_ui_thru_sept_2021", "spend_build.R",
          "cust_ui_thru_jul_2021","spend_build.R",
          "state_treatment_control_lwa", "spend_build.R")
# Check all the dataframes are present:
test_that(dfs_used_throughout_mpc_robustness %>%
            select(df_name) %>%
            pull() %>% 
            exists() %>% 
            expect_equal(TRUE))

# Must run also spend_build.R first if these dataframes are missing:
if (!(dfs_used_throughout_mpc_robustness %>% select(df_name) %>% pull() %>% exists())){
  source("pgm/spend_build.R")
}

# OUTS
robustness_tables <-
  c("mpc_robust_df",
    "mpc_elas_all_means",
    "inc_spend_avg") # contains useful stats, might drop later
#from the run_apc_calculations function --
#csv_apcs: APC results from dplyr operations
#csv_apcs_ivreg: APC results from IV regressionsn

################################################################################

# function that can run select lines
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

orig_df_demo_src_wins <- df_demo_src_wins

# 1. CONSTRUCT FILTERS ----
# filter for non-chase credit cards ----
# customers who in 2019 have (non-chase credit cards) / (total outflows) = 0
tmp_df_demo_src_wins <-
  df_demo_src_wins %>%
  mutate(year_num = str_sub(periodid, 1, 4) %>% as.numeric()) %>%
  filter(year_num == 2019) %>%
  group_by(cust_number) %>%
  summarise_at(vars(off_us_creditcard_pmts, 
                    on_us_creditcard_pmts,
                    total_outflows),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(pct_cc_of_tot_outflows = if_else(is.na(off_us_creditcard_pmts),
                                          NA_real_,
                                          off_us_creditcard_pmts / total_outflows))

test_that("",
          expect_equal((tmp_df_demo_src_wins %>%
                filter(pct_cc_of_tot_outflows == 0) %>%
                nrow()) /
               nrow(tmp_df_demo_src_wins),
             0.303,
             tol = 0.01))

subsample_cust_non_chase_cc_filter <- # people we are keeping in subsample
  tmp_df_demo_src_wins %>%
  filter(pct_cc_of_tot_outflows == 0) %>%
  pull(cust_number)

# debt ach subsample ----
debt_ach_cust_list <-
  df_demo_src_wins %>%
  distinct(cust_number, periodid, debt_payments, total_spend_expanded) %>%
  mutate(mth_date = str_c(periodid, "01") %>% ymd()) %>%
  filter(mth_date < ymd("2020-01-01"),
         debt_payments > 0) %>%
  group_by(cust_number) %>%
  count() %>%
  filter(n == 12) %>%
  pull(cust_number)

if(!small_samp){
test_that("length of debt_ach_cust_list", 
          expect_equal(length(debt_ach_cust_list), 589352))} # was 762103 for >= 10 months
  
# 2. SET UP FUNCTIONS TO ITERATE ----
source("pgm/funcs/mpc_robustness_funcs.R")

# 3. GET APCS FOR ROBUSTNESS TABLES ----
#This produces: table_apcs_combined_all, table_apcs_combined_no_other_cc, table_apcs_combined_debt_ach 
samples <- c("all", "no_other_cc", "debt_ach")
table_combined <- samples %>% 
  purrr::map(~ filter_mpc_sample(.x) %>% 
               construct_weighted_controls() %>%
               run_apc_calculations(df_list=., sample=.x) %>%
               as_tibble) %>%
  set_names(str_c("table_apcs_combined_", samples)) %>%
  list2env(.GlobalEnv)
# This also produces .csv files of APC tables and the IV regressions 
# Save apcs_table_ivreg as a formatted .tex and .html file
mpc_ivreg <- read.csv(str_c(path_out, "apcs_table_ivreg.csv"))

mpc_tex <- mpc_ivreg %>%
  filter(numerator == "Spend (total)",
         denominator == "income",
         design != "LWA") %>%
  select(-c(numerator, denominator, n_obs)) %>%
  slice(match(c("UI onset (waiting)","FPUC expiration", "$300 onset",
                "$300 expiration June states", "$300 expiration Sept states",
                "$300 expiration June vs. Sept states"), design)) %>%
  transmute(design = (c("Waiting for benefits", 
                          "Expiration of 600 supplement",
                          "Onset of 300 supplement",
                          "Expiration of 300 supplement (June states)",
                          "Expiration of 300 supplement (September states)",
                          "Expiration of 300 supplement (June vs. September states)")),
            mpc = paste0(round(apc,2), " (", round(se,2), ")")) %>%
  `colnames<-`(c("Research Design", "One Month MPC"))

mpc_tex_file <- mpc_tex %>%
  stargazer(., summary=FALSE, type="latex") %>%
  str_subset("\\{table\\}|caption|label", negate = TRUE)%>%
  str_replace("ccc", "clc") %>%
  str_replace_all("hline", c("toprule", "midrule", "bottomrule")) %>%
  str_replace_all("600", "\\\\$600") %>%
  str_replace_all("300", "\\\\$300") %>%
  append("& & (Total Spending) \\\\", 7) %>%
  writeLines(str_c(path_out, "mpc_out_of_ui.tex"))

mpc_html_file <- mpc_tex %>%
  stargazer(.,  digits=2, summary=FALSE, type="html", rownames = FALSE) 

mpc_html_file %>%
  str_replace_all("One month MPC ", "One month MPC<br>")%>%
  str_replace_all("600", "$600") %>%
  str_replace_all("300", "$300") %>%
  writeLines(str_c(path_out, "mpc_out_of_ui.html"))

# 4. FORMAT ROBUSTNESS TABLES ----
make_col_df <- function(df, stat = "mean_weighted", apc_var = "apc_spend_exp") {
  df %>%
    mutate(row_num = case_when(grepl("waiting", design) ~ 1,
                               grepl("FPUC", design) ~ 2,
                               design == "$300 onset" ~ 3,
                               grepl("total income", design) ~ 4)) %>%
    filter(method == stat,
           !is.na(row_num)) %>%
    arrange(row_num) %>%
    transmute(design, !!sym(apc_var))
}

mpc_dfs <- mget(paste0("table_apcs_combined_", c("all", samples)))
mpc_df_stat <- as.list(c("mean_weighted", "median_weighted", rep("mean_weighted",2)))
mpc_episode_column <- c("Waiting for benefits", "Expiration of $600 supplement",
                         "Onset of 8-month $300 supplement", "Lost Wages Assistance")

mpc_robust_df <- map2_dfc(mpc_dfs, mpc_df_stat, 
                          ~.x %>% 
                            make_col_df(stat = .y) %>% 
                            getElement('apc_spend_exp') %>%
                            round(digits = 2)) %>%
  `colnames<-`(str_c("(", 1:4, ")")) %>%
  add_column( "Episode" = mpc_episode_column, 
              .before = "(1)")%>%
  filter(Episode != "Lost Wages Assistance")

capture.output(print(xtable(mpc_robust_df, digits = 2), include.rownames=FALSE,
                     floating=FALSE, booktabs=TRUE)) %>%
  str_replace("\\{lrrrr\\}", "\\{lcccc\\}") %>%
  str_replace("\\\\toprule", "\\\\toprule \\\\toprule") %>%
  append("Summary Statistic & Mean & Median & Mean & Mean \\\\", 9) %>%
  append("\\midrule", 10) %>%
  append("Sample & All & All & No non-Chase credit card & Make ACH debt payments \\\\", 11) %>%
  writeLines(str_c(path_out, "mpc_robustness.tex"))

capture.output(print(xtable(mpc_robust_df), type = "html", include.rownames = FALSE)) %>%
  writeLines(str_c(path_out, "mpc_robustness.html"))

# 5. FORMAT ELASTICITY TABLES ----
# get inflows_ex_transfers, spend_cardcash, spend_total
get_sum_stats_table <- function(df) {
  df_demo_src_wins <- df
  
  cust_for_sumstats <-
    bind_rows(cust_nonui,
              #restrict to customers who have gotten UI since April
              cust_ui_onset %>% dplyr::filter(exit_ui_date >= as.Date('2020-04-05'))) %>% 
    distinct(cust_number, .keep_all = TRUE)
  
  tmp_sumstats_data <-
    df_demo_src_wins %>% 
    inner_join(cust_for_sumstats, by = 'cust_number') %>% 
    distinct(cust_number, periodid, .keep_all = TRUE) %>% 
    mutate(period = ifelse(periodid %in% c(202001, 202002), 'jan_feb_2020',
                           ifelse(between(periodid, 202004, 202010), 'apr_oct_2020', 'other')),
           inflows_ex_transfers = total_inflows - transfer_inflows) %>%
    rename(spend_cardcash = total_spend_narrow,
           spend_total = total_spend_expanded) %>% 
    dplyr::filter(period %in% c('jan_feb_2020', 'apr_oct_2020'))
  
  spend_sumstats <- 
    tmp_sumstats_data %>% 
    group_by(group, period) %>% 
    summarise_at(.vars = vars(inflows_ex_transfers, spend_cardcash, spend_total, total_ui_inflows, checking_acct_balance),
                 .funs = list(mean = mean, median = xtile_ten, sd = sd)) %>% 
    left_join(tmp_sumstats_data %>% group_by(group, period) %>% summarise(ct_cust = n()))
  
  return(spend_sumstats)
}

# By samples and sub-samples ----
sub_sample <- c("all", "no_other_cc", "debt_ach")
spend_sumstats_subsamples <- sub_sample %>%
  map(~filter_mpc_sample(mpc_subsample = .x) %>%
        get_sum_stats_table()) %>%
  set_names(str_c("spend_sumstats_", sub_sample)) %>%
  list2env(.GlobalEnv)

get_sum_stats_stub <- function(mpc_subsample, stat) {
  df <- get(str_c("spend_sumstats_", mpc_subsample))
  df %>%
    ungroup() %>%
    select(!!sym(str_c("inflows_ex_transfers_", stat)):!!sym(str_c("spend_total_", stat))) %>%
    slice(4) %>%
    mutate(subsample = str_c(mpc_subsample, ", ", stat)) %>%
    purrr::set_names(~ str_replace_all(., str_c("_", stat), ""))
}

stats <- c("mean", "median")
spend_sumstats_names <- c(str_c("spend_sumstats_all_stub_", stats),
                          str_c("spend_sumstats_", c("no_other_cc", "debt_ach"), "_stub"))
stats_sub_sample_list <- list(sub_sample = as.list(c(rep("all",2), sub_sample[2:3])),
                              stat = as.list(c(stats, rep("mean",2))))
spend_sumstats_tables <- map(seq_along(spend_sumstats_names),
                             ~get_sum_stats_stub(stats_sub_sample_list$sub_sample[[.x]],
                                                 stats_sub_sample_list$stat[[.x]])) %>%
  set_names(spend_sumstats_names)

# generate table ----
# gather all the inc/spend averages
inc_spend_avg <-
  bind_rows(spend_sumstats_tables) %>%
  mutate(inc_over_spend_total = inflows_ex_transfers / spend_total,
         inc_over_spend_cardcash = inflows_ex_transfers / spend_cardcash) %>%
  select(subsample, everything())

# all, means
mpc_elas_all_means <- table_apcs_combined_all %>%
  make_col_df() %>%
  rename(`(1)` = apc_spend_exp,
         Episode = design) %>%
  mutate("Episode" = mpc_episode_column,
         `(2)` = filter(inc_spend_avg, subsample == "all, mean")$inc_over_spend_total * `(1)`,
         `(3)` = make_col_df(table_apcs_combined_all, 
                             apc_var = "apc_spend_cardcash")$apc_spend_cardcash,
         `(4)` = filter(inc_spend_avg, subsample == "all, mean")$inc_over_spend_cardcash * `(3)`) %>%
  mutate_if(is.numeric, round, digits=2)

capture.output(print(xtable(mpc_elas_all_means), 
                     floating = FALSE, include.rownames = FALSE,
                     booktabs=TRUE)) %>%
  str_replace("\\{lrrrr\\}", "\\{lcccc\\}") %>%
  str_replace("\\\\toprule", "\\\\toprule \\\\toprule") %>%
  str_replace("Episode & \\(1\\) & \\(2\\) & \\(3\\) & \\(4\\) ", 
              "& \\\\multicolumn\\{2\\}\\{c\\}\\{Spend Total\\} & \\\\multicolumn\\{2\\}\\{c\\}\\{Spend card and cash\\}") %>%
  append("\\cmidrule(r){2-3} \\cmidrule(r){4-5}", 5) %>%
  append("Episode & MPC & Elasticity & MPC & Elasticity \\\\", 6) %>%
  writeLines(str_c(path_out, "mpc_elas_all_means.tex"))

capture.output(print(xtable(mpc_elas_all_means), type = "html", include.rownames = FALSE)) %>%
  writeLines(str_c(path_out, "mpc_elas_all_means.html"))

