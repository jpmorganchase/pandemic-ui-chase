# SPDX-License-Identifier: MIT
# Liquidity Changes
# Author: Rupsha Debnath
# Date: 30 October 2023
# Function to produce liquidity change outputs for different samples.

sample <- c("all", "high_liquidity", "low_liquidity") # some functions use sample and some samples so we need to assign both
samples <- sample

orig_df_demo_src_wins <- df_demo_src_wins

cust_list_low_liquidity_full <- customer_buffer_quantiles_2018 %>%
  filter(med_buffer_quantile == 1)

cust_list_high_liquidity_full <- customer_buffer_quantiles_2018 %>%
  filter(med_buffer_quantile == 2)

# Function to combine both scripts----

liquidity_outputs <- function(samples, sample){
  # Run spend_build.R prior to running this
  source("pgm/funcs/mpc_robustness_funcs.R")
  
  # Run parts of the mpc_robustness_funcs script:
  df_list <- samples %>% 
    purrr::map(~ filter_mpc_sample(.x) %>% 
                 construct_weighted_controls())
  
  df_demo_src_wins_touse_eipwks <- df_list[[1]]$df1
  customer_inc_quintiles <- df_list[[1]]$df2
  
  df_demo_src_wins_touse_eipwks_summer <- df_demo_src_wins_touse_eipwks %>%
    filter(is.na(eip_week) | eip_week %in% eip1_week_list ) %>%
    filter(is.na(eip_week) | eip2_week %in% eip2_week_list,
           is.na(eip_week) | eip3_week %in% eip3_week_list)
  
  # cust_list ####
  # The following is split up in the following steps:
  ##STEP 1: get list of customers to include in analysis
  # Note: For step 1,  Waiting cannot be coalesced since the control group is other unemployed workers
  # 1. Waiting ----
  waiting_apc_customers <-
    cust_ui_onset %>%
    #compare customers who start UI week of 4/5 (Treatment) vs start UI week of 5/31 (Control)
    dplyr::filter(first_ui_week %in% c(as.Date('2020-04-05'), as.Date('2020-05-31'))) %>%
    transmute(cust_number, group = ifelse(first_ui_week == '2020-04-05',
                                          'treatment', 'control'))
  
  # 2., 3., 4.: Expiration, $300 Onset: List of customers----
  #This output dfs: "ui_expiration_customers", "ui_onset_customers"
  customer_input_dfs <- c((list(cust_thru_aug, cust_thru_jan,
                                cust_ui_thru_sept_2021,
                                cust_ui_thru_jul_2021) %>%
                             map(~distinct(.x, cust_number, group)) %>%
                             map(~bind_rows(cust_nonui, .x))))
  env = environment()
  ui_customers <- map(customer_input_dfs,
                      ~.x %>% mutate(group = ifelse(group %in% c('Employed','Control (receive in October)'),
                                                    'control', 'treatment'))) %>%
    set_names(c(str_c("ui_", c("expiration", "onset", "puc_sep","puc_jun"), "_customers")))%>%
    list2env(set_names(c(str_c("ui_", c("expiration", "onset", "puc_sep","puc_jun"), "_customers"))),
             envir = env)
  
  #June vs sep
  ui_puc_sep_customers_only_u <- ui_puc_sep_customers %>%
    filter(group=='treatment') %>%
    mutate(group='control')
  
  ui_puc_jun_customers_only_u <- ui_puc_jun_customers %>%
    filter(group=='treatment')
  
  ui_puc_customers_only_u <- rbind(ui_puc_jun_customers_only_u,ui_puc_sep_customers_only_u)
  
  # Continuously unemployed sample: get list of customers to include in analysis
  combined_cust_list <-
    bind_rows(cust_nonui ,
              cust_thru_feb %>%
                dplyr::filter(job_sep_date %in% c(as.Date('2020-03-29'), as.Date('2020-04-05'))) %>%
                dplyr::filter(first_ui_week %in% c(as.Date('2020-04-05'), as.Date('2020-04-12'))) %>%
                distinct(cust_number, .keep_all = TRUE)) %>%
    mutate(group = ifelse(group == 'Employed', 'control', 'treatment'))
  
  # Dataset for use ####
  ##STEP 3: get cust-by-month df, with weights
  # 1., 3., 2. Waiting, Onset, Expiration---
  if(sample=="all"){
    datasets <- list(
      # Continuously unemployed--
      df_demo_src_wins,
      # Waiting--
      df_demo_src_wins_touse_eipwks %>% dplyr::filter(cust_type == '202021_ui_recipient'),
      # Onset--
      df_demo_src_wins_touse_eipwks %>% dplyr::filter((is.na(eip2_week) | eip2_week %in% eip2_week_list)),
      # Expiration
      df_demo_src_wins_touse_eipwks,
      df_demo_src_wins_touse_eipwks_summer,
      df_demo_src_wins_touse_eipwks_summer,
      df_demo_src_wins_touse_eipwks_summer)
  } else {
    datasets <- list(
      # Continuously unemployed--
      df_demo_src_wins_touse_eipwks,
      # Waiting--
      df_demo_src_wins_touse_eipwks %>% dplyr::filter(cust_type == '202021_ui_recipient'),
      # Onset--
      df_demo_src_wins_touse_eipwks %>% dplyr::filter((is.na(eip2_week) | eip2_week %in% eip2_week_list)),
      # Expiration
      df_demo_src_wins_touse_eipwks,
      df_demo_src_wins_touse_eipwks_summer,
      df_demo_src_wins_touse_eipwks_summer,
      df_demo_src_wins_touse_eipwks_summer)
  }
  
  periodid_filters <- list(c(202007, 202008),
                           c(202003, 202005),
                           c(202012, 202101),
                           c(202007, 202008),
                           c(202108, 202110),
                           c(202106, 202108),
                           c(202106, 202108))
  
  innerjoin_dfs <- list(combined_cust_list, waiting_apc_customers, # continuously unemp; waiting
                        ui_onset_customers, ui_expiration_customers, # onset; expiration
                        ui_puc_sep_customers, ui_puc_jun_customers, # sep and june summer expiration
                        ui_puc_customers_only_u) # sep vs jun
  
  post_treat <- list(202008, 202003, 202012, 202008, 202110, 202108, 202108)
  
  design_names <- list("Continuous unemployed during the pandemic",
                       "UI onset (waiting)","$300 onset","FPUC expiration", 
                       "$300 expiration Sept states", "$300 expiration June states",
                       "$300 expiration June vs. Sept states")
  
  # Buffer variable definition: (checking_acct_balance - 0.5 * spend_jan2020) / spend_jan2020
  filtered_datasets <- map(seq_along(datasets),
                           ~ datasets[[.x]] %>%
                             group_by(cust_number) %>%
                             mutate(spend_jan2020 = sum(total_spend_expanded[periodid==202001])) %>%
                             ungroup() %>%
                             #ID strategy: diff-in-diff of 202005 vs 202003 spending
                             dplyr::filter(periodid %in% periodid_filters[[.x]]) %>%
                             inner_join(innerjoin_dfs[[.x]], by = "cust_number") %>%
                             distinct(cust_number, periodid, .keep_all = TRUE) %>%
                             mutate(post = ifelse(periodid == post_treat[.x], 1, 0)))
  
  liq_buffer <- map(seq_along(filtered_datasets),
                    ~ filtered_datasets[[.x]] %>%
                      mutate(buffer_spend = ((checking_acct_balance - 0.5 * spend_jan2020) / spend_jan2020),
                             buffer_spend = ifelse(buffer_spend==Inf, NA, buffer_spend)) %>%
                      filter(group=="treatment")%>%
                      group_by(post)  %>%
                      summarise(median_buffer_spend = xtile_ten(buffer_spend),
                                median_check_bal = xtile_ten(checking_acct_balance)) %>%
                      ungroup() %>%
                      transmute(post = ifelse(post==1, "post", "pre"),
                                median_buffer_spend,
                                median_check_bal,
                                design = design_names[.x]) %>%
                      pivot_wider(names_from = c(post), 
                                  values_from = c("median_buffer_spend", "median_check_bal"))) %>%
    bind_rows()
  
  filtered_dfs_jan2020 <- map(seq_along(datasets),
                              ~ datasets[[.x]] %>%
                                dplyr::filter(periodid == 202001) %>%
                                inner_join(innerjoin_dfs[[.x]], by = "cust_number") %>%
                                distinct(cust_number, periodid, .keep_all = TRUE))
  
  # BUFFER CALCULATIONS ----
  # Percentile rank of this mean POST-period checking account balance in
  # the (unweighted) checking account balance distribution for Jan 2020 of
  # households in the employed control group
  percentile_dist_jan2020_buffer <- df_demo_src_wins %>%
    filter(cust_number %in% cust_nonui$cust_number) %>%
    filter(periodid==202001) %>%
    mutate(buffer_spend = (checking_acct_balance - 0.5 * total_spend_expanded) / total_spend_expanded) %>%
    pull(buffer_spend) %>%
    ecdf()
  
  median_pre_buffer <- liq_buffer$median_buffer_spend_pre
  
  df_buffer_rank_jan2020 <- map_df(seq_along(median_pre_buffer),
                                   ~ data.frame(percent_rank_pre_median_jan2020_buffer = 
                                                  percentile_dist_jan2020_buffer(median_pre_buffer[[.x]]))) %>%
    mutate(design = design_names) 
  
  # "what are their average pre-pandemic liquidity levels?"
  # Mean checking account balance in dollars in January 2020 for this treatment group
  jan20_buffer <- map(seq_along(filtered_dfs_jan2020),
                      ~ filtered_dfs_jan2020[[.x]] %>%
                        mutate(buffer_spend = ((checking_acct_balance - 0.5 * total_spend_expanded) / total_spend_expanded),
                               buffer_spend = ifelse(buffer_spend==Inf, NA, buffer_spend)) %>%
                        filter(group=="treatment") %>%
                        summarise(median_buffer_spend_jan20 = median(buffer_spend, na.rm = T)) %>%
                        mutate(design = design_names[.x])) %>%
    bind_rows()
  
  # Percentile rank of this January 2020 mean checking account balance
  # in the (unweighted) checking account balance distribution for Jan
  # 2020 of households in the employed control group.
  jan20_buffer_median <- jan20_buffer$median_buffer_spend_jan20
  
  df_jan20_comparison_buffer <- map_df(seq_along(jan20_buffer_median),
                                       ~ data.frame(median_perc = percentile_dist_jan2020_buffer(jan20_buffer_median[[.x]]))) %>%
    mutate(design = design_names) %>%
    rename(percent_rank_median_jan20 = median_perc)
  
  # CHECKING BALANCE----
  checking_bal_jan2020 <- map(seq_along(filtered_dfs_jan2020),
                              ~ filtered_dfs_jan2020[[.x]] %>%
                                filter(group=="treatment") %>%
                                summarise(median_check_bal_jan20 = median(checking_acct_balance)) %>%
                                mutate(design = design_names[.x])) %>%
    bind_rows()
  
  # Quantile function:
  percentile_dist_jan2020 <- df_demo_src_wins %>%
    filter(cust_number %in% cust_nonui$cust_number) %>%
    filter(periodid==202001) %>%
    pull(checking_acct_balance) %>%
    ecdf()
  
  # Percentile rank of this mean POST-period checking account balance in
  # the (unweighted) checking account balance distribution for Jan 2020 of
  # households in the employed control group
  # Repeat each percentile rank for the PRE period checking account balance
  median_check_bal_pre <- liq_buffer$median_check_bal_pre

  df_check_bal_perc_jan20 <- map_df(seq_along(median_check_bal_pre),
                                    ~ data.frame(percent_rank_pre_median_jan2020 = percentile_dist_jan2020(median_check_bal_pre[[.x]]))) %>%
    mutate(design = design_names)
  
  # Percentile rank of this January 2020 mean checking account balance
  # in the (unweighted) checking account balance distribution for Jan
  # 2020 of households in the employed control group.
  jan2020_checkbal_median <- checking_bal_jan2020$median_check_bal_jan20
  
  df_jan20_comparison <- map_df(seq_along(jan2020_checkbal_median),
                                ~ data.frame(percent_rank_median_jan20 = percentile_dist_jan2020(jan2020_checkbal_median[[.x]]))) %>%
    mutate(design = design_names)
 
  ################################################################################################################################################
  
  # OUTPUTS ----
  
  checking_balance_output <- data.frame(design = unlist(design_names),
                                        checking_bal_pre = liq_buffer$median_check_bal_pre,
                                        check_bal_perc_jan20_pre = df_check_bal_perc_jan20$percent_rank_pre_median_jan2020,
                                        jan2020_checkbal_median = jan2020_checkbal_median,
                                        perc_rank_jan20 = df_jan20_comparison$percent_rank_median_jan20)
  
  checking_balance_output %>%
    write.csv(str_c(path_out, "liquidity_by_checkbal_", sample, ".csv"))
  
  buffer_output <- data.frame(design = unlist(design_names),
                              buffer_pre = liq_buffer$median_buffer_spend_pre,
                              buffer_perc_jan20_pre = df_buffer_rank_jan2020$percent_rank_pre_median_jan2020_buffer,
                              jan2020_buffer_median = jan20_buffer_median,
                              perc_rank_jan20 = df_jan20_comparison_buffer$percent_rank_median_jan20) 
  
  buffer_output %>%
    write.csv(str_c(path_out, "liquidity_by_buffer_", sample, ".csv"))
  
  # Aggregation standards:
  liq_change_agg_stds <<- data.frame(design = c("Continuous unemployed during the pandemic", "UI onset (waiting)",
                                    "$300 onset", "FPUC expiration", 
                                    "$300 expiration Sept states", "$300 expiration June states", 
                                    "$300 expiration June vs. Sept states"),
                         ct_cust = min(unlist(map(seq_along(innerjoin_dfs),
                                                  ~ length(unique(innerjoin_dfs[[.x]]$cust_number))))),
                         bind_rows(map_df(seq_along(innerjoin_dfs),
                                          ~ innerjoin_dfs[[.x]] %>%
                                            select(cust_number, group) %>%
                                            unique() %>%
                                            count(group) %>% 
                                            pivot_wider(names_from = "group",
                                                        values_from = "n"))),
                         row_count_df = (unlist(map(seq_along(datasets),
                                                    ~ nrow(datasets[[.x]])))))
  write.csv(liq_change_agg_stds, str_c(path_out, "liq_change_agg_stds_", sample, ".csv"))
}

# Produce outputs:
liq_outputs <- map(seq_along(sample), 
         ~ liquidity_outputs(samples = samples[[.x]],
                             sample = sample[[.x]]))

# Format tables for paper ----

# Function for tex and html table:
write_tex_html_tables <- function(sample_read){
  
  checkbal <- read.csv(str_c(path_out, "liquidity_by_checkbal_", sample_read, ".csv"))[-1] %>%
    mutate(design = factor(design, levels = design_names)) %>%
    arrange(design)
  
  buffer <- read.csv(str_c(path_out, "liquidity_by_buffer_", sample_read, ".csv"))[-1]%>%
    mutate(design = factor(design, levels = design_names)) %>%
    arrange(design)
  
  table <- data.frame(Design = as.character(checkbal$design),
                      "mediancheckbal.PrePandemic" = paste0("$",number(round(checkbal$jan2020_checkbal_median), big.mark = ",")),
                      "mediancheckbal.MonthofMPC" = paste0("$",number(round(checkbal$checking_bal_pre), big.mark = ",")),
                      "percrankcheckbal.PrePandemic" = round(checkbal$perc_rank_jan20, 2),
                      "percrankcheckbal.MonthofMPC" = round(checkbal$check_bal_perc_jan20_pre, 2),
                      "percrankbuff.PrePandemic" = round(buffer$perc_rank_jan20, 2),
                      "percrankbuff.MonthofMPC" = round(buffer$buffer_perc_jan20_pre, 2)) 
  
  textable <- table %>%
    stargazer(., summary=FALSE, type="latex", rownames = FALSE, digits=2) %>%
    str_remove_all("mediancheckbal.") %>%
    str_remove_all("percrankcheckbal.") %>%
    str_remove_all("percrankbuff.") %>%
    str_subset("\\{table\\}|caption|label", negate = TRUE)%>%
    str_replace("ccccccc", "lcccccc") %>%
    str_replace_all("FPUC", "\\\\$600") %>%
    str_replace_all("PrePandemic", "Pre-Pandemic") %>%
    str_replace_all("MonthofMPC", "Month of MPC") %>%
    str_replace_all("Continuous", "Continuously") %>%
    str_replace_all("hline", c("toprule","toprule", "midrule", "bottomrule")) %>%
    append("& (1) & (2) & (3) & (4) & (5) & (6) \\\\", 7) %>%
    append(" & \\multicolumn{2}{c}{Median checking account balance} & \\multicolumn{2}{c}{Percentile ranking of median checking account balance} & \\multicolumn{2}{c}{Percentile ranking of median checking account buffer} \\\\",
           6)  %>%
    append("\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}", 7) %>%
    writeLines(str_c(path_out, "liquidity_", sample_read, ".tex"))
  
  htmltable <- capture.output(print(xtable(table), type = "html", include.rownames = FALSE)) %>%
    str_remove_all("mediancheckbal.") %>%
    str_remove_all("percrankcheckbal.") %>%
    str_remove_all("percrankbuff.") %>%
    str_replace_all("PrePandemic", "Pre-Pandemic") %>%
    str_replace_all("MonthofMPC", "Month of MPC") %>%
    str_replace_all("Continuous", "Continuously") %>%
    str_replace_all("FPUC", "$600") %>%
    str_replace_all("<th> Design </th>", "") %>%
    append('<tr> <th rowspan="3"> Design </th> <th colspan="2"> Median checking account balance </th> <th colspan="2"> Percentile ranking of median checking account balance </th> <th colspan="2"> Percentile ranking of median checking account buffer </th> </tr>',
           3) %>%
    append('<tr> <th> (1) </th> <th> (2) </th> <th> (3) </th> <th> (4) </th> <th> (5) </th> <th> (6) </th> </tr>',
           5) %>%
    writeLines(str_c(path_out, "liquidity_", sample_read, ".html"))
  
}

# Write the tables:
sample_read <- sample
liquidity_tables <- map(seq_along(sample_read),
                        ~ write_tex_html_tables(sample_read[[.x]]))
