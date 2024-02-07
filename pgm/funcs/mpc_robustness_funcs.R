# mpc_robustness_funcs.R

# Each of these functions are run eight times - full sample, no non-chase cc, debt ach sample,
# high liquidity vs. low liquidity, and then people who exit to recall, exit to non-recall, 
# and exit to payroll (the union of recall and non-recall)
# i. Filter (differs by subsample)
# ii. Compute quintiles and construct employed sample
# iii. Run regressions

ui_recip <- c("202021_ui_recipient", "2019_ui_recipient")
non_ui_recip <- c("nonui_2020", "nonui_")

# i. Filter by subsample ----
filter_mpc_sample <- function(mpc_subsample = "all") {
  if (mpc_subsample == "no_other_cc") {
    df_demo_src_wins <-
      orig_df_demo_src_wins %>%
      filter(cust_number %in% subsample_cust_non_chase_cc_filter)
    
    print("Running subsample with 0% non-chase credit card payments")
  } else if (mpc_subsample == "debt_ach") {
    df_demo_src_wins <-
      orig_df_demo_src_wins %>%
      filter(cust_number %in% debt_ach_cust_list)
    
    print("Running subsample with people who pay debt with ACH")
  } else if (mpc_subsample == "recalled") {
    df_demo_src_wins <-
      orig_df_demo_src_wins %>%
      filter(cust_number %in% cust_list_exit_to_recall$cust_number | cust_type == "nonui_2020")
    
    print("Running subsample with people who are recalled")
  } else if (mpc_subsample == "not_recalled") {
    df_demo_src_wins <-
      orig_df_demo_src_wins %>%
      filter(cust_number %in% cust_list_exit_to_not_recall$cust_number  | cust_type == "nonui_2020")
    
    print("Running subsample with people who are not recalled")
  } else if (mpc_subsample == "to_payroll") {
    df_demo_src_wins <-
      orig_df_demo_src_wins %>%
      filter(cust_number %in% cust_list_exit_to_payroll$cust_number  | cust_type == "nonui_2020")
    
    print("Running subsample with people who exit to payroll")  
  } else if (mpc_subsample == "low_liquidity"){
    df_demo_src_wins <-
      orig_df_demo_src_wins %>%
      filter(cust_number %in% cust_list_low_liquidity_full$cust_number)
    
    print("Running subsample with people who are low liquidity") 
  } else if (mpc_subsample == "high_liquidity"){
    df_demo_src_wins <-
      orig_df_demo_src_wins %>%
      filter(cust_number %in% cust_list_high_liquidity_full$cust_number)
    
    print("Running subsample with people who are high liquidity") 

  } else {
    df_demo_src_wins <- orig_df_demo_src_wins
    
    print("Running full sample")
  }
  return(df_demo_src_wins)
}

# function that can run select lines
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}

# ii. Compute quintiles and construct employed sample ----
construct_weighted_controls <- function(df) {
  #build monthly df to show comparison of unemployed vs employed throughout the pandemic
  ## calculate income quintiles based on 2019 income
  customer_inc_quintiles <-
    # source2("pgm/spend_build.R", 183, 191)
    df %>%
    dplyr::filter(between(periodid, 201901, 201912)) %>%
    mutate(inflows_ex_transfers = total_inflows - transfer_inflows) %>%
    distinct(cust_number, periodid, .keep_all = TRUE) %>%
    group_by(cust_number) %>%
    summarise(inc_2019 = sum(inflows_ex_transfers)) %>%
    mutate(inc_2019_quintile = cut_number(inc_2019, n = 5))
  
  #join 2019 income and EIP receipt information to monthly customer-level df
  df_demo_src_wins_touse <- # source2("pgm/spend_build.R", 195, 200)
    df %>%
    left_join(customer_inc_quintiles, by = 'cust_number') %>%
    left_join(cust_eip_touse, by = 'cust_number') %>%
    left_join(cust_eip_rounds_touse, by = 'cust_number') %>%
    rename(total_spend_cardcash = total_spend_narrow)
  #tiny number of rows has null 2019 income -- drop these
  print(str_c(
      "Number of rows in df_demo_src_wins_touse with null 2019 income: ",
      df_demo_src_wins_touse %>% dplyr::filter(is.na(inc_2019_quintile)) %>% nrow()))
  df_demo_src_wins_touse <-
    df_demo_src_wins_touse %>% dplyr::filter(!is.na(inc_2019_quintile))
  
  #Drop customers with multiple EIPs for rounds 1 or 2 (3 is outside current sample frame)
  df_demo_src_wins_touse <- # "pgm/spend_build.R" L209-212
    df_demo_src_wins_touse %>%
    dplyr::filter(
      is.na(eip1_txn_count) | eip1_txn_count <= 1,
      is.na(eip2_txn_count) | eip2_txn_count <= 1)
  
  #Version for full time-series filters just on EIP1 weeks
  df_demo_src_wins_touse_eipwks <-
    # source2("pgm/spend_build.R", 231, 233)
    df_demo_src_wins_touse %>%
    dplyr::filter(is.na(eip_week) | eip_week %in% eip1_week_list)
  print(str_c(
      "Number of rows in df_demo_src_wins_touse_eipwks: ",
      df_demo_src_wins_touse_eipwks %>% distinct(cust_number) %>% nrow()))
  
  list(df1 = df_demo_src_wins_touse_eipwks,
       df2 = customer_inc_quintiles)
}

# iii. Run the APC regressions ----

#compute apc
compute_apcs <- function(df_collapsed){
  
  #' IN 
  #' Dataframe collapsed to the group-by-flow category-by-period level with 
  #' additional columns 'mean_weighted' and 'median_weighted'.
  #' Remark: 'group' refers to treatment/control,  
  #' 'flow category' refers to the category of flow, such as income or total_outflows; and 
  #' 'period' refers to pre or post in the diff-in-diff.
  #' OUT 
  #' Table with diff-in-diff APC estimates

  `+` <- possibly(`+`, NA_real_)
  `-` <- possibly(`-`, NA_real_)
  `/` <- possibly(`/`, NA_real_)
  
  df_out <- 
    df_collapsed %>% 
    pivot_longer(cols = c('mean_weighted', 'median_weighted'), names_to = 'method') %>% 
    pivot_wider(names_from = c('name', 'group', 'period'), values_from = 'value') %>% 
    transmute(method,
              income_diff = (income_treatment_post - income_treatment_pre) - (income_control_post - income_control_pre),
              spend_exp_diff = (total_spend_expanded_treatment_post - total_spend_expanded_treatment_pre) - 
                (total_spend_expanded_control_post - total_spend_expanded_control_pre),
              spend_cardcash_diff = (total_spend_cardcash_treatment_post - total_spend_cardcash_treatment_pre) - 
                (total_spend_cardcash_control_post - total_spend_cardcash_control_pre),
              total_outflows_diff = (total_outflows_treatment_post - total_outflows_treatment_pre) - 
                (total_outflows_control_post - total_outflows_control_pre),
              apc_spend_exp = spend_exp_diff/income_diff, 
              apc_spend_cardcash = spend_cardcash_diff/income_diff,
              apc_total_outflows = total_outflows_diff/income_diff
    )
  return(df_out)
}

#function for iv regressions
ivregression <- function(df) {
  
  #' INS
  #' Dataframe with post, treat and post*treat column.
  #' OUTS
  #' Table of IV regression results. 
  #' We run 4 IV regressions: spend vs income (weighted), spend vs ui (weighted).
  #' Note the regression is a normal 2SLS with the following specification
  #' (let y = some measure of income, c = some measure of consumption):
  #' First stage: y = alpha + beta*(post*treat) + b1*post + b2*treat
  #' Second stage: c = delta + gamma*y_hat + b3*post + b4*treat
  #'
  #' post and treat are controls, and (post*treat) is the instrument. 
  #' In other words, the instrument is being in the treatment group in the post period. 
  #' Generally, treatment group means you are a UI recipient; post period means you 
  #' are in the period of UI receipt, as opposed to the pre period, which is pre-UI receipt.
  #' Put differently, this is a simple diff-in-diff between a treatment and control group across a
  #' post period (when treatment occurs) and a pre period (before treatment has occurred).
  
  reg <- function(y, x, df) {
    #generic IV regression of y vs x; instrument post_treat_ix is always the same; cluster SEs by cust_number
    reg_result <-
      felm(as.formula(str_c(y, " ~ post + treat |0| (", x, " ~ post_treat_ix) | cust_number")),
        data = df,
        weights = df$weight,
        # we get issues with negative eigenvals in multiway clustered variance matrix
        # setting psdef to FALSE prevents forcing the values to 0
        psdef = FALSE)
    coefs <- reg_result %>% summary() %>% coefficients()
    n_obs <- reg_result$N
    return(list(coefs, n_obs))
  }
  
  #regress total spend vs income
  reg_y <- c(rep("total_spend_expanded", 2), rep("total_spend_cardcash", 2))
  reg_x <- rep(c("income", "total_ui_inflows"), 2)
  #list of regressions
  iv_list <- map2(reg_y, reg_x, 
                  ~reg(.x, .y, df)) %>%
    set_names(str_c("iv_", reg_y, "_", reg_x))
  
  tbl_out <- data.frame(
    numerator = c(rep('Spend (total)', 2), rep('Spend (cardcash)', 2)),
    denominator = rep(c('income', 'ui'), 2),
    #[4] extracts coefficient estimate
    apc = unlist(map(iv_list, ~ .x[[1]][4] %>% round(3))),
    #[8] extracts SE estimate
    se = unlist(map(iv_list, ~ .x[[1]][8] %>% round(3))),
    n_obs = unlist(map(iv_list, ~ .x[[2]])),
    row.names = c(1:4))
  
  return(tbl_out)
}


#This function merges in the relevant re-weights defined in spend_by_liquidity_buffer and 
#spend_by_ever_recall when weights are multiplied by re_weights, the new weights will match 
#income and EIP week not just for treatment and control (E and U) but also across the observable 
#splits like high and low liquidity, so that we will be comparing low liquidity and high liquidity households 
#with the same income.
#This is re_weight is a "wrapper" outside the weighting of treatment and control, so reweights
#both the U and E groups accordingly, i.e. everywhere below now uses weight=weight*re_weight 
#but re_weight will be set equal to 1 except in the cases of recall and liquidity where we want reweighting
#browser()
join_weights_with_re_weights <- function(df_weights, sample, liq_reweighting = TRUE) {
  if (sample == "not_recalled" | sample =="to_payroll") {
    df_weights <- 
      df_weights %>% 
      left_join(weights_start_no_recall,
                by=c("inc_2019_quintile_index", "eip_week")) %>%
      replace_na(list(re_weight = 0))
  } else if (sample == "high_liquidity" | sample =="low_liquidity") {
    df_weights <- 
      df_weights %>% 
      left_join(reweights_to_use_for_liquidity,
                by=c("inc_2019_quintile_index", "eip_week")) %>%
      replace_na(list(re_weight = 0))
    if (sample == "high_liquidity") {
      if (liq_reweighting){
        #use reweight where income of high liquidity group matches overall pop
        df_weights<-df_weights %>% mutate(re_weight=re_weight_high)
      } else {
        #use reweight where income of high liquidity group matches overall pop
        df_weights<-df_weights %>% mutate(re_weight=1)
      }
    } else { 
      if (liq_reweighting){
        #use reweight where income of high liquidity group matches overall pop
        df_weights<-df_weights %>% mutate(re_weight=re_weight_low)
      } else {
        #use reweight where income of high liquidity group matches overall pop
        df_weights<-df_weights %>% mutate(re_weight=1)
      }
    }
  } else { #no reweighting except in the cases defined above
    df_weights<-df_weights %>% mutate(re_weight=1)
  }
}


run_apc_calculations <- function(df_list, sample, category=NA, liq_reweighting = TRUE) {
  
  print(sample)
  print(category)
  # this function uses the following inputs:
  # cust_ui_onset, cust_nonui, cust_thru_aug, cust_thru_jan, state_treatment_control_lwa
  if(run_categories){
    if (category == "total_spend_cardcash"){
      df_demo_src_wins_touse_eipwks <- df_list$df1
    } else{
      df_demo_src_wins_touse_eipwks <- df_list$df1 %>% 
        select(-total_spend_cardcash) %>% 
        mutate(spending_category = category) %>% 
        rename(total_spend_cardcash = category)
    }
  } else {
    df_demo_src_wins_touse_eipwks <- df_list$df1
  }
  
  customer_inc_quintiles <- df_list$df2
  
  df_demo_src_wins_touse_eipwks_summer <- df_demo_src_wins_touse_eipwks %>%
    filter(is.na(eip_week) | eip_week %in% eip1_week_list ) %>%
    filter(is.na(eip_week) | eip2_week %in% eip2_week_list,
           is.na(eip_week) | eip3_week %in% eip3_week_list)
  
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
  
  # 2., 3., 4.: Expiration, $300 Onset, LWA: List of customers----
  #This output dfs: "ui_expiration_customers", "ui_onset_customers"
  customer_input_dfs <- c((list(cust_thru_aug, cust_thru_jan,
                                cust_ui_thru_sept_2021,
                                cust_ui_thru_jul_2021) %>%
                             map(~distinct(.x, cust_number, group)) %>%
                             map(~bind_rows(cust_nonui, .x))),
                          list(state_treatment_control_lwa = distinct(state_treatment_control_lwa, cust_number, group)))
  env = environment()
  ui_customers <- map(customer_input_dfs, 
                      ~.x %>% mutate(group = ifelse(group %in% c('Employed','Control (receive in October)'), 
                                                    'control', 'treatment'))) %>%
    set_names(c(str_c("ui_", c("expiration", "onset", "puc_sep","puc_jun"), "_customers"), "lwa_customers"))%>%
    list2env(set_names(c(str_c("ui_", c("expiration", "onset", "puc_sep","puc_jun"), "_customers"), "lwa_customers")), 
             envir = env)
  
  ##STEP 2: compute inc quintile-by-eip date weights: we want to weight the control group
  ##        so that it matches the treatment group in terms of (inc quintile-eip date)
  # Note: For step 2,  $300 onset cannot be coalesced since we match on both rounds of EIP while other analyses match on just a single round of EIP
  # 3. $300 onset ----
  weights_onset_apc <-
    df_demo_src_wins_touse_eipwks %>%
    dplyr::filter(is.na(eip2_week) | eip2_week %in% eip2_week_list) %>% #only filters on eip1_week in spend_build.R
    #get a customer-by-inc quintile-by-eip date df
    distinct(cust_number, inc_2019_quintile, eip_week, eip2_week) %>%
    inner_join(ui_onset_customers, by = "cust_number") %>%
    group_by(inc_2019_quintile, eip_week, eip2_week, group) %>%
    summarise(ct = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = 'group', values_from = 'ct') %>%
    mutate(weight = treatment/control) %>%
    select(inc_2019_quintile, eip_week, eip2_week, weight) %>% 
    mutate(inc_2019_quintile_index=as.numeric(inc_2019_quintile)) %>%
    replace_na(list(weight = 0))
  
  weights_puc_sep_apc <-
    df_demo_src_wins_touse_eipwks_summer %>%
    distinct(cust_number, inc_2019_quintile, eip_week, eip2_week, eip3_week) %>%
    inner_join(ui_puc_sep_customers, by = "cust_number") %>%
    group_by(inc_2019_quintile, eip_week, eip2_week, eip3_week, group) %>%
    summarise(ct = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = 'group', values_from = 'ct') %>%
    mutate(weight = treatment/control) %>%
    select(inc_2019_quintile, eip_week, eip2_week, eip3_week, weight) %>% 
    mutate(inc_2019_quintile_index=as.numeric(inc_2019_quintile)) %>%
    replace_na(list(weight = 0))
  
  weights_puc_jun_apc <-
    df_demo_src_wins_touse_eipwks_summer %>%
    distinct(cust_number, inc_2019_quintile, eip_week, eip2_week, eip3_week) %>%
    inner_join(ui_puc_jun_customers, by = "cust_number") %>%
    group_by(inc_2019_quintile, eip_week, eip2_week, eip3_week, group) %>%
    summarise(ct = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = 'group', values_from = 'ct') %>%
    mutate(weight = treatment/control) %>%
    select(inc_2019_quintile, eip_week, eip2_week, eip3_week, weight) %>% 
    mutate(inc_2019_quintile_index=as.numeric(inc_2019_quintile)) %>%
    replace_na(list(weight = 0))
  
  ui_puc_sep_customers_only_u <- ui_puc_sep_customers %>%
    filter(group=='treatment') %>%
    mutate(group='control')
  
  ui_puc_jun_customers_only_u <- ui_puc_jun_customers %>%
    filter(group=='treatment')
  
  ui_puc_customers_only_u <- rbind(ui_puc_jun_customers_only_u,ui_puc_sep_customers_only_u)
  
  weights_puc_jun_vs_sep_apc <-
    df_demo_src_wins_touse_eipwks_summer %>%
    distinct(cust_number, inc_2019_quintile, eip_week, eip2_week, eip3_week) %>%
    inner_join(ui_puc_customers_only_u, by = "cust_number") %>%
    group_by(inc_2019_quintile, eip_week, eip2_week, eip3_week, group) %>%
    summarise(ct = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = 'group', values_from = 'ct') %>%
    mutate(weight = treatment/control) %>%
    select(inc_2019_quintile, eip_week, eip2_week, eip3_week, weight) %>% 
    mutate(inc_2019_quintile_index=as.numeric(inc_2019_quintile)) %>%
    replace_na(list(weight = 0))
  
  # 1., 2., 4.: Waiting, Expiration, LWA: Weights ----
  join_df_list <- list(ui_expiration_customers, lwa_customers, waiting_apc_customers)
  weight_df_names <- c("expiration", "lwa", "waiting")
  weights_trial <- map(join_df_list, ~df_demo_src_wins_touse_eipwks %>% 
                         #get a customer-by-inc quintile-by-eip date df
                         distinct(cust_number, inc_2019_quintile, eip_week) %>% 
                         inner_join(.x, by = "cust_number") %>% 
                         group_by_at(vars(one_of(c("inc_2019_quintile", "eip_week", "group")))) %>% 
                         summarise(ct = n()) %>% 
                         ungroup() %>% 
                         pivot_wider(names_from = 'group', values_from = 'ct') %>%
                         mutate(weight = treatment/control) %>%
                         select(inc_2019_quintile, eip_week, weight) %>%
                         mutate(inc_2019_quintile_index=as.numeric(inc_2019_quintile))) %>%
    set_names(str_c("weights_", weight_df_names, "_apc"))%>%
    list2env(set_names(str_c("weights_", weight_df_names, "_apc")),
             envir = env)
  
  #merge in the re-weights using the function defined above (note for cases without reweighting this reweight is just 1)
  weights_waiting_apc <- join_weights_with_re_weights(weights_waiting_apc,sample, liq_reweighting)
  weights_lwa_apc <- join_weights_with_re_weights(weights_lwa_apc,sample, liq_reweighting)
  weights_expiration_apc <- join_weights_with_re_weights(weights_expiration_apc,sample, liq_reweighting)
  weights_onset_apc <- join_weights_with_re_weights(weights_onset_apc,sample, liq_reweighting)
  weights_puc_sep_apc <- join_weights_with_re_weights(weights_puc_sep_apc,sample, liq_reweighting)
  weights_puc_jun_apc <- join_weights_with_re_weights(weights_puc_jun_apc,sample, liq_reweighting)
  weights_puc_jun_vs_sep_apc <- join_weights_with_re_weights(weights_puc_jun_vs_sep_apc,sample, liq_reweighting)
  
  ##STEP 3: get cust-by-month df, with weights
  # Note: For step 3, LWA cannot be coalesced because it uses weekly data because the policy change happens in the middle of a month
  # 4. LWA (income denom) ----
  df_to_compute_lwa_apcs <- 
    state_treatment_control_lwa %>% 
    left_join(customer_inc_quintiles, by = 'cust_number') %>% 
    left_join(cust_eip_touse, by = 'cust_number') %>% 
    select(-group) %>% 
    #ID strategy: diff-in-diff over weeks of 9/6 thru 10/11 (6 weeks) compared with weeks of 7/26 thru 8/30 (6 weeks)
    dplyr::filter(between(week_start_date, as.Date('2020-08-02'), as.Date('2020-10-04'))) %>% 
    inner_join(lwa_customers, by = "cust_number") %>%
    left_join(weights_lwa_apc, by = c('inc_2019_quintile', 'eip_week')) %>% 
    #weight should be 1 for treatment group
    mutate(weight = ifelse(group == 'treatment', 1*re_weight, weight*re_weight)) %>% 
    #a small number of obs dont get weights b/c they have no 2019 inc quintile
    dplyr::filter(!is.na(weight)) %>% 
    transmute(cust_number, cust_state, group, week_start_date, weight, 
              total_outflows = total_deposit_outflows_mainyear, 
              total_spend_cardcash = spend_mainyear, 
              total_spend_expanded = spend_expanded_mainyear,
              income = total_inflows_mainyear - transfer_inflows_mainyear,
              #For this ID strategy, our diff-in-diff measure of UI inflows is LWA inflows
              total_ui_inflows = lwa) %>%
    pivot_longer(-one_of(c('cust_number', 'cust_state', 'group', 'week_start_date', 'weight'))) %>% 
    mutate(period = ifelse(between(week_start_date, as.Date('2020-08-02'), as.Date('2020-08-31')), 'pre', 'post')) %>%
    # maybe multiply the value with 1 or 3/7ths depending on the week... so add an ifelse statement
    # so it would show up...
    mutate(time_adjust = ifelse(between(week_start_date, as.Date('2020-08-09'), as.Date('2020-09-27')), 1, 3/7), 
           value = value*time_adjust) %>%
    select(-time_adjust)
  
  # 1., 3., 2. Waiting, Onset, Expiration---
  datasets <- list(# Waiting--
    df_demo_src_wins_touse_eipwks %>% dplyr::filter(cust_type == '202021_ui_recipient'),
    # Onset--
    df_demo_src_wins_touse_eipwks %>% dplyr::filter((is.na(eip2_week) | eip2_week %in% eip2_week_list)),
    # Expiration
    df_demo_src_wins_touse_eipwks,
    df_demo_src_wins_touse_eipwks_summer,
    df_demo_src_wins_touse_eipwks_summer,
    df_demo_src_wins_touse_eipwks_summer)
  
  periodid_filters <- list(c(202003, 202005),
                           c(202012, 202101), 
                           c(202007, 202008),
                           c(202108, 202110),
                           c(202106, 202108),
                           c(202106, 202108))
  
  innerjoin_dfs <- list(waiting_apc_customers, ui_onset_customers, 
                        ui_expiration_customers, ui_puc_sep_customers,
                        ui_puc_jun_customers, ui_puc_customers_only_u)
  leftjoin_dfs <- mget(paste0("weights_", c("waiting", "onset", "expiration","puc_sep","puc_jun","puc_jun_vs_sep"), "_apc"))
  left_join_conditions <- list(c('inc_2019_quintile', 'eip_week'),
                               c('inc_2019_quintile', 'eip_week', 'eip2_week'),
                               c('inc_2019_quintile', 'eip_week'),
                               c('inc_2019_quintile', 'eip_week', 'eip2_week','eip3_week'),
                               c('inc_2019_quintile', 'eip_week', 'eip2_week','eip3_week'),
                               c('inc_2019_quintile', 'eip_week', 'eip2_week','eip3_week'))
  df_apcs_compute <- map(seq_along(datasets),
                         ~ datasets[[.x]] %>%
                           #ID strategy: diff-in-diff of 202005 vs 202003 spending
                           dplyr::filter(periodid %in% periodid_filters[[.x]]) %>% 
                           inner_join(innerjoin_dfs[[.x]], by = "cust_number") %>% 
                           distinct(cust_number, periodid, .keep_all = TRUE) %>%
                           left_join(leftjoin_dfs[[.x]], by = left_join_conditions[[.x]]) %>% 
                           #weight should be 1 for 4/5 UI starters
                           mutate(weight = ifelse(group == 'treatment', 1*re_weight, weight*re_weight)) %>% 
                           transmute(cust_number, cust_state, group, periodid, weight, 
                                     total_outflows, total_spend_cardcash, total_spend_expanded,
                                     income = total_inflows - transfer_inflows,
                                     total_ui_inflows) %>% 
                           pivot_longer(-one_of(c('cust_number', 'cust_state', 'group', 'periodid', 'weight')))) %>%
    set_names(str_c("df_to_compute_", c("waiting", "onset", "expiration","puc_sep","puc_jun","puc_jun_vs_sep"), "_apcs")) %>%
    list2env(set_names(str_c("df_to_compute_", c("waiting", "onset", "expiration","puc_sep","puc_jun","puc_jun_vs_sep"), "_apcs")),
             envir = env)
  
  #APC Tables####
  apcs_mutate_df <- mget(paste0("df_to_compute_", c("waiting", "expiration", "onset","puc_sep","puc_jun","puc_jun_vs_sep"), "_apcs"))
  apcs_periodid <- list(202003, 202007, 202012, 202108, 202106, 202106)
  mutated_df_apcs <- map(seq_along(apcs_mutate_df), 
                         ~apcs_mutate_df[[.x]] %>%
                           mutate(period = ifelse(apcs_mutate_df[[.x]]$periodid == apcs_periodid[[.x]], 
                                                  'pre', 'post')))
  #going to add the additional dfs in the list
  df_apcs_all <- append(mutated_df_apcs, list(df_to_compute_lwa_apcs %>% 
                                                      group_by(cust_number, weight, period, group, name) %>% 
                                                      summarise(value = sum(value)) %>% ungroup()), 2)
  apc_table_names <- c("waiting", "expiration", "lwa_incomedenom", "onset", "puc_sep","puc_sep","puc_jun_vs_sep")
  apc_table_design <- c('UI onset (waiting)', 'FPUC expiration', 
                        'LWA (denominator = total income)', '$300 onset','$300 expiration Sept states','$300 expiration June states','$300 expiration June vs. Sept states')
  apcs <- map(seq_along(df_apcs_all), 
              ~ df_apcs_all[[.x]] %>%
                group_by(period, name, group) %>%
                summarise(mean_weighted = weighted.mean(value, weight),
                          median_weighted = xtile_ten(value, weight)) %>%
                ungroup() %>%
                select(name, mean_weighted, median_weighted, group, period) %>%
                compute_apcs() %>%
                mutate(design = apc_table_design[.x])) %>%
    set_names(str_c("table_apcs_", apc_table_names))
  # COMBINED ALL 5 APCS#####
  table_apcs_combined <- bind_rows(apcs)
  
  if(run_categories){
    table_apcs_combined <- table_apcs_combined %>%
      mutate(spending_category = category)
  }
  #####
  
  if(sample == "all" | sample=="recalled" | sample=="not_recalled" | sample=="to_payroll" | sample=="low_liquidity" | sample=="high_liquidity"){ 
    #IV Regressions####
    #clean up df_to_compute_lwa_apcs a bit more
    df_to_compute_lwa_apcs <- df_to_compute_lwa_apcs %>% 
      group_by(cust_number, cust_state,group, period, name, weight) %>% 
      summarise(value = sum(value)) %>% ungroup() %>%
      rename(periodid = period)
    #set up variables
    iv_names <- c("waiting", "expiration", "lwa", "onset", "puc_sep","puc_jun","puc_jun_vs_sep")
    dfs_iv <- mget(paste0("df_to_compute_", iv_names, "_apcs"))
    period_id_iv <- list(202005, 202008, 'post', 202101, 202110, 202108, 202108)
    iv_design <- c('UI onset (waiting)', 'FPUC expiration', 'LWA', '$300 onset','$300 expiration Sept states','$300 expiration June states','$300 expiration June vs. Sept states')
    iv_reg_all <- map(seq_along(dfs_iv), 
                      ~dfs_iv[[.x]] %>%
                        # Need to ensure that the column is called periodid in the dataframes
                        mutate(post = ifelse(periodid == period_id_iv[.x], 1, 0)) %>%
                        mutate(treat = as.integer(group == 'treatment'),
                               post_treat_ix = post*treat) %>% 
                        select(cust_number, cust_state, name, value, post, treat, post_treat_ix, weight) %>%
                        pivot_wider(names_from = 'name', values_from = 'value') %>%
                        ivregression() %>% mutate(design = iv_design[.x])) %>%
      set_names(str_c("iv_reg_", iv_names))
    #relabel 'denominator' column of iv_reg_lwa to clarify that we the denominator is LWA, not normal UI
    iv_reg_all$iv_reg_lwa <- iv_reg_all$iv_reg_lwa %>%
      mutate(denominator = as.character(denominator),
             denominator = ifelse(denominator == 'ui', 'lwa', denominator))
    table_iv_apcs_combined <- bind_rows(iv_reg_all)
    if(run_categories){
      table_iv_apcs_combined <- table_iv_apcs_combined %>% 
        mutate(spending_category = category)
    }
    #####
    
    # Categories Spending additional MPC calculations####
    if(run_categories){
      period_id_iv_emp <- list(202005, 202008, 'post', 202101, 202110, 202108, 202108)
      period_id_iv_unemp <- list(201905, 201908, 'post', 201901, 201910, 201908, 201908)
      
      periodid_filters <- list(c(202003, 202005, 201905),
                               c(202012, 202101, 201901),
                               c(202007, 202008, 201908),
                               c(202108, 202110, 201910),
                               c(202106, 202108, 201908),
                               c(202106, 202108, 201908))
      
      df_apcs_compute <- map(seq_along(datasets),
                             ~ datasets[[.x]] %>%
                               #ID strategy: diff-in-diff of 202005 vs 202003 spending
                               dplyr::filter(periodid %in% periodid_filters[[.x]]) %>% 
                               inner_join(innerjoin_dfs[[.x]], by = "cust_number") %>% 
                               distinct(cust_number, periodid, .keep_all = TRUE) %>%
                               left_join(leftjoin_dfs[[.x]], by = left_join_conditions[[.x]]) %>% 
                               #weight should be 1 for 4/5 UI starters
                               mutate(weight = ifelse(group == 'treatment', 1*re_weight, weight*re_weight)) %>% 
                               transmute(cust_number, cust_state, group, periodid, weight, 
                                         total_outflows, total_spend_cardcash, total_spend_expanded,
                                         income = total_inflows - transfer_inflows,
                                         total_ui_inflows) %>% 
                               pivot_longer(-one_of(c('cust_number', 'cust_state', 'group', 'periodid', 'weight')))) %>%
        set_names(str_c("df_to_compute_", c("waiting", "onset", "expiration","puc_sep","puc_jun","puc_jun_vs_sep"), "_apcs")) %>%
        list2env(set_names(str_c("df_to_compute_", c("waiting", "onset", "expiration","puc_sep","puc_jun","puc_jun_vs_sep"), "_apcs")),
                 envir = env)
      
      dfs_iv <- mget(paste0("df_to_compute_", iv_names, "_apcs"))
      # map(dfs_iv, ~summary(.x$periodid))
      if (category == "total_spend_cardcash"){
        spend_levels <- map(seq_along(dfs_iv), 
                            ~ dfs_iv[[.x]] %>%
                              mutate(period_class = if_else(group == "control", 
                                                            period_id_iv_emp[[.x]],
                                                            period_id_iv_unemp[[.x]]),
                                     post = ifelse(periodid == period_class, 1, 0)) %>% 
                              filter(post == 1,
                                     name == "total_spend_cardcash" | name == "total_spend_expanded") %>% 
                              group_by(name, group) %>%
                              summarise(mean_weighted = weighted.mean(value, weight),
                                        median_weighted = xtile_ten(value, weight)) %>%
                              ungroup() %>%
                              mutate(design = apc_table_design[.x]) %>% 
                              select(-c(median_weighted))
                            
        )
        
        table_spend_levels_combined <- bind_rows(spend_levels)
        
        spend_wide <-    table_spend_levels_combined %>% 
          pivot_wider(names_from = group,
                      values_from = mean_weighted) %>% 
          rename(spend_category_unemp = control,
                 spend_category_emp = treatment)
        
        table_apcs_combined_joined <- table_apcs_combined %>% 
          left_join(spend_wide, 
                    by = "design")
        
        
        table_iv_apcs_combined_joined <- table_iv_apcs_combined %>% 
          left_join(spend_wide, 
                    by = "design") %>% 
          select(-spending_category) %>% 
          rename(spending_category = name)
        
      } else{
        spend_levels <- map(seq_along(dfs_iv), 
                            ~ dfs_iv[[.x]] %>%
                              mutate(period_class = if_else(group == "control", 
                                                            period_id_iv_emp[[.x]],
                                                            period_id_iv_unemp[[.x]]),
                                     post = ifelse(periodid == period_class, 1, 0)) %>% 
                              filter(post == 1,
                                     name == "total_spend_cardcash") %>% 
                              group_by(name, group) %>%
                              summarise(mean_weighted = weighted.mean(value, weight),
                                        median_weighted = xtile_ten(value, weight)) %>%
                              ungroup() %>%
                              mutate(design = apc_table_design[.x]) %>% 
                              select(-c(name, median_weighted))
                            
        )
        
        table_spend_levels_combined <- bind_rows(spend_levels) %>% 
          mutate(spending_category = category)
        
        spend_wide <-    table_spend_levels_combined %>% 
          pivot_wider(names_from = group,
                      values_from = mean_weighted) %>% 
          rename(spend_category_unemp = control,
                 spend_category_emp = treatment)
        
        table_apcs_combined_joined <- table_apcs_combined %>% 
          left_join(spend_wide, 
                    by = "design")
        
        
        table_iv_apcs_combined_joined <- table_iv_apcs_combined %>% 
          left_join(spend_wide, 
                    by = "design")
      }
    }
    #####
    
    #Write APC and IV tables as csvs####
    if(sample == "all"){
      if(run_categories){
        table_apcs_combined_joined %>% write_csv(str_c(path_out, 
                                                       'apcs_table_full_', category, '.csv')) #APC results from dplyr operations
        table_iv_apcs_combined_joined %>% write_csv(str_c(path_out, 
                                                          'apcs_table_ivreg_', category, '.csv'))
      } else {
        table_apcs_combined %>% write_csv(str_c(path_out, 
                                                'apcs_table_full.csv')) #APC results from dplyr operations
        table_iv_apcs_combined %>% write_csv(str_c(path_out, 
                                                   'apcs_table_ivreg.csv')) #APC results from IV regressions
      }
    }
    
    
    if(sample == "recalled"){
      if(run_categories){
        table_iv_apcs_combined_joined %>% write_csv(str_c(path_out, 
                                                          "apcs_table_ivreg_recalled_", category, ".csv")) #APC results from IV regressions
      } else {
        table_iv_apcs_combined %>% write_csv(str_c(path_out, 
                                                   "apcs_table_ivreg_recalled.csv")) #APC results from IV regressions
      }
    }
    if(sample == "not_recalled"){
      if(run_categories){
        table_iv_apcs_combined_joined %>% write_csv(str_c(path_out, 
                                                          "apcs_table_ivreg_not_recalled_", category, ".csv")) #APC results from IV regressions
      } else {
        table_iv_apcs_combined %>% write_csv(str_c(path_out, 
                                                   "apcs_table_ivreg_not_recalled.csv")) #APC results from IV regressions
      }
    }
    
    
    if(sample == "to_payroll"){
      if(run_categories){
        table_iv_apcs_combined_joined %>% write_csv(str_c(path_out, 
                                                          "apcs_table_ivreg_to_payroll_", category, ".csv")) #APC results from IV regressions
      } else {
        table_iv_apcs_combined %>% write_csv(str_c(path_out, 
                                                   "apcs_table_ivreg_to_payroll.csv")) #APC results from IV regressions
      }
    }
    
    
    if(sample == "high_liquidity"){
      
      if(liq_reweighting){
        if(run_categories){
          table_iv_apcs_combined_joined %>% write_csv(str_c(path_out, 
                                                            "apcs_table_ivreg_high_liquidity_", category, ".csv")) #APC results from IV regressions
        } else {
          table_iv_apcs_combined %>% write_csv(str_c(path_out, 
                                                     "apcs_table_ivreg_high_liquidity.csv")) #APC results from IV regressions
        }
      } else {
        if(run_categories){
          table_iv_apcs_combined_joined %>% write_csv(str_c(path_out, 
                                                            "apcs_table_ivreg_high_liquidity_noreweight_", category, ".csv")) #APC results from IV regressions
        } else {
          table_iv_apcs_combined %>% write_csv(str_c(path_out, 
                                                     "apcs_table_ivreg_high_liquidity_noreweight.csv")) #APC results from IV regressions
        }
      }
      
    }
    
    if(sample == "low_liquidity"){
      
      if(liq_reweighting){
        if(run_categories){
          table_iv_apcs_combined_joined %>% write_csv(str_c(path_out, 
                                                            "apcs_table_ivreg_low_liquidity_", category, ".csv")) #APC results from IV regressions
        } else {
          table_iv_apcs_combined %>% write_csv(str_c(path_out, 
                                                     "apcs_table_ivreg_low_liquidity.csv")) #APC results from IV regressions
        }
      } else {
        if(run_categories){
          table_iv_apcs_combined_joined %>% write_csv(str_c(path_out, 
                                                            "apcs_table_ivreg_low_liquidity_noreweight_", category, ".csv")) #APC results from IV regressions
        } else {
          table_iv_apcs_combined %>% write_csv(str_c(path_out, 
                                                     "apcs_table_ivreg_low_liquidity_noreweight.csv")) #APC results from IV regressions
        }
      }      
    }
    
    }
  
  if(run_categories){
    return(table_iv_apcs_combined_joined)
  } else {
    return(table_apcs_combined)
  }
  
  
}

