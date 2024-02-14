# SPDX-License-Identifier: MIT
# ui_eip_data_build.R
# Author: Peter Ganong
# Objective: Clean up some of the data from ui_eip_data_read_in.R so that it is
# in the form we want for analysis.

#INS
eip_week_list <- as.Date(c("2020-04-12", "2020-04-26", "2020-05-03", "2020-05-10", "2020-05-17", "2020-05-24"))

if(run_categories){
  cats_vector_to_sum <- c(durables,
                          non_durables)
}

#OUTS (list of R dfs)
##fpuc_rollout: list of FPUC ($600 UI supplement) rollout dates by state
##cust_eip_touse: list of customers with EIP payments (from round 1) 
##df_demo_src_wins: winsorizes the top 10% of a bunch of variables (checking account balance, 
##                  total liquid balances, debt payments, labor inflows, total outflows, 
##                  transfer inflows, total spend, non-chase credit card payments, chase 
##                  credit card payments) and top 1% of ui_inflows and tax refund inflows. 
##cust_eip_rounds_touse: gets a list of EIPs from round 2 and 3
##df_lwa_cust_week_src (spend): customer-week level LWA data

#fpuc ----
## Read in list of FPUC (federal pandemic unemployment compensation, the $600 UI supplement) rollout dates
## this list of dates is from JIRA68 
fpuc_rollout <- 
  readxl::read_excel(str_c(path_repo, 'src/', "fpuc_rollout.xlsx")) %>%
  filter(!is.na(cust_state)) %>%
  transmute(cust_state, puc_start_week = as.Date(start_week))

#eip ----
## get list of EIP1s to use
cust_eip_touse <- 
  df_eips %>%
  mutate(eip_week = floor_date(eip_date, unit = "week")) %>%
  #only include customers who get 1 EIP: getting multiple EIPs means something is weird with the customer
  filter(num_eips_for_customer == 1,
         #only count EIPs at "normal" EIP weeks
         eip_week %in% eip_week_list)

# clean EIP2 and EIP3
cust_eip_rounds_touse <- 
  df_eip_rounds %>%
  mutate_at(vars(eip1_week = eip1_date, eip2_week = eip2_date, eip3_week = eip3_date), 
            ~ floor_date(., unit = "week")) %>%
  select(-eip1_date, -eip2_date, -eip3_date)


#demographics ----
## add variables to demographics df so that it is in the form we want to use
## takes ~2 min to run
df_demo_src_wins <-
  df_demog_touse %>%
  #drop AR, AK from analysis because Chase benchmarks poorly there
  dplyr::filter(!(cust_state %in% c('AR', 'AK'))) %>% 
  #produce month as date (rather than periodid yyyymm)
  mutate(month_start_date = ymd(periodid, truncated = 1)) %>%
  { if (run_categories) mutate(., 
                               total_categorized_spending_pre_winsor = select(., cats_vector_to_sum) %>% rowSums(na.rm = TRUE),
                               uncatergorized_spend_cardcash = total_spend_narrow - total_categorized_spending_pre_winsor,
                               uncatergorized_spend = total_spend_expanded - total_categorized_spending_pre_winsor) else . } %>%
  #winsorize monthly flows
  mutate_at(vars(contains('balance'), "total_inflows", "labor_inflows",
                 contains('outflows'), contains('spend'), contains('debt'),
                 contains('transfer'), contains('pmts')),
            funs(winsor(., fraction_high = 0.90))) %>%
  mutate_at(vars("ui_inflows", "tax_refund_inflows"), funs(winsor(., fraction_high = 0.99))) %>%
  mutate(outflows_ex_transfers = total_spend_expanded + debt_payments, #total_outflows - transfers,
         inflows_ex_transfers = total_inflows - transfer_inflows,
         pct_cc_of_tot_outflows = if_else(is.na(off_us_creditcard_pmts) | is.na(total_outflows),
                                          NA_real_,
                                          off_us_creditcard_pmts / total_outflows))

test_that('N rows demographics',
          expect_equal(
            df_demo_src_wins %>% nrow(),
            ifelse(small_samp == TRUE, 644249, 43332411)
          ))

#LWA read-in ----
##first, get a unique cust type for each customer (there can be overlap, eg, customers who are both nonui_2020 and nonui_2019)
cust_types <- df_demo_src_wins %>%
  dplyr::filter(cust_type %in% c('202021_ui_recipient', '2019_ui_recipient')) %>% 
  distinct(cust_number, cust_type)
test_that('No customer should be classified as both 202021_ui_recipient and 2019_ui_recipient',
          expect_equal(cust_types %>% group_by(cust_number) %>% summarise(ct = n()) %>% distinct(ct) %>% pull(), 1))
# read in cust-week LWA data
df_lwa_cust_week_src <- 
  df_cp %>%
  filter(inflow_type %in% c("lwa", "normal_ui", "PUA")) %>%
  transmute(week_start_date = as.Date(week_start_date),
           inflow_type = if_else(inflow_type == "lwa",
                                 "lwa",
                                 "ui_inflows_mainyear"),
          inflow_amount = as.numeric(inflow_amount),
          cust_type,
          cust_state = state,
          cust_number) %>%
  group_by(cust_number, cust_state, week_start_date, inflow_type, cust_type) %>%
  summarise(inflow_amount = sum(inflow_amount)) %>%
  slice(1) %>%
  select(-cust_type) %>%
  pivot_wider(names_from = inflow_type,
             values_from = inflow_amount,
             values_fill = list(inflow_amount = 0))

lwa_states <- df_lwa_cust_week_src %>% distinct(cust_state) %>% pull()


# output files needed for rep_rate_prep, df_demo_src_wins too large to write
df_demo_src_wins %>%
  select(cust_number, periodid, age, gender) %>%
  saveRDS(str_c(data_path, "tmp/build_sp_jf/", Sys.Date(), 
                ifelse(run_categories == TRUE, 
                       "df_demo_src_wins_att_disagg.rds",
                       "df_demo_src_wins_att.rds")))

df_demo_src_wins_2019 <- df_demo_src_wins %>%
  dplyr::filter(between(periodid, 201901, 201912))

df_demo_src_wins_2019 %>%
  saveRDS(str_c(data_path, "tmp/build_sp_jf/", Sys.Date(), 
                ifelse(run_categories == TRUE, 
                       "df_demo_src_wins_2019_disagg.rds",
                       "df_demo_src_wins_2019.rds")))

#Save rds files needed to run the spend build without re-running entire build
df_demo_src_wins %>%
  saveRDS(str_c(data_path, "tmp/build_sp_jf/", Sys.Date(), 
                ifelse(run_categories == TRUE, 
                       "df_demo_src_wins_disagg.rds",
                       "df_demo_src_wins.rds"))) 
df_lwa_cust_week_src %>%
  saveRDS(str_c(data_path, "tmp/build_sp_jf/", Sys.Date(), 
                ifelse(run_categories == TRUE, 
                       "df_lwa_cust_week_src_disagg.rds",
                       "df_lwa_cust_week_src.rds"))) 
cust_eip_touse %>% 
  saveRDS(str_c(data_path, "tmp/build_sp_jf/", Sys.Date(), 
                ifelse(run_categories == TRUE, 
                       "cust_eip_touse_disagg.rds",
                       "cust_eip_touse.rds"))) 
cust_eip_rounds_touse %>%
  saveRDS(str_c(data_path, "tmp/build_sp_jf/", Sys.Date(), 
                ifelse(run_categories == TRUE, 
                       "cust_eip_rounds_touse_disagg.rds",
                       "cust_eip_rounds_touse.rds")))

