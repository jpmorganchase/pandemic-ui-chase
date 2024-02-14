# SPDX-License-Identifier: MIT
#ui_data_readin_functions.R
source(str_c(path_repo, 'pgm/funcs/prelim.R'))


#### Categorization functions ####
func_total_deposit_outflows_newmethod<- function(){
  return("auto_loan_pmts + cash + cashback + debitcard + utils_telecoms_schools + cc_spend_total + mortgage_pmts + 
         off_us_creditcard_pmts + on_us_creditcard_pmts + other_loan_pmts + student_loan_pmts + transfers + other + paper_check")
  }

func_spend <- function(){
  return("cash + cashback + debitcard + utils_telecoms_schools + cc_spend_total")
  }

func_spend_expanded <- function(){
  return("cash + cashback + debitcard +utils_telecoms_schools + cc_spend_total + paper_check + other")
  }

func_debt_pmts <- function(){
  return("auto_loan_pmts + mortgage_pmts + off_us_creditcard_pmts + other_loan_pmts + student_loan_pmts")
  }

#### weekly data cleaner ####
microdata_cleaner <- function(df, main_year, preceding_year, ui_group_name, nonui_group_name){
  ## Function data_cleaner converts a customer-by-week-by-year dataframe into two dataframes, one for UI recipients and one for non-UI recipients,
  ## both in wide format for analysis.
  ####INS:
  ####    df: customer-by-week-by-year dataframe with flows and a 'cust_type' column
  ####    main_year: integer, the main year that we are interested in. (For the purposes of this project, it will be either 2020 or 2019.)
  ####    preceding_year: integer, the year preceding the main year. (For the purposes of this project, it will be either 2019 or 2018.)
  ####    ui_group_name: string, the name of the UI group in cust_type column
  ####    nonui_group_name: string, the name of the non-UI group in cust_type column
  ####OUTS: r dataframe in wide format.
  
  ##combine categories into more usable ones
  df_condensed_categories <- df %>% 
    transmute_('cust_number', 'week_year', 'week_num', 'week_start_date', 'cust_type',
               'labor_inflows', 
               'ui_inflows',
              'total_inflows', 'tax_refund_inflows',
               total_deposit_outflows = "total_outflows",
               spend = "total_spend_narrow",
               spend_expanded = "total_spend_expanded",
               debt_pmts = "debt_payments",
               "transfers" = "transfer_inflows",
              'transfer_inflows') %>% 
    ###winsorize
    mutate_at(.vars = vars(contains("inflows"), contains('outflows'), contains('spend'), contains('debt_pmts'), contains('transfer')),
              funs(winsor(., fraction_high = 0.98)))
    
  
  print(names(df_condensed_categories))
  ##join preceding year spending to main year spending at the customer-week level
  df_mainyear <- df_condensed_categories %>% 
    dplyr::filter(week_year == main_year) %>% 
    select(cust_number, week_num, week_start_date,  cust_type,
           labor_inflows, ui_inflows, total_inflows, tax_refund_inflows, transfer_inflows,
           total_deposit_outflows, spend, spend_expanded, debt_pmts, transfers) %>% 
    rename_at(vars(-contains('cust'), -contains('week')), function(x){paste0(x, '_mainyear')})
  df_prec_year <- df_condensed_categories %>% 
    dplyr::filter(week_year == preceding_year) %>% 
    select(cust_number, week_num,
           total_deposit_outflows, spend, spend_expanded, debt_pmts, transfers) %>% 
    rename_at(vars(-cust_number, -week_num), function(x){paste0(x, '_precyear')})
  df_wide <- df_mainyear %>% 
    left_join(df_prec_year, by = c('cust_number', 'week_num'))
  
  ##separate UI and non-UI 
  df_wide_nonui <- df_wide %>% 
    dplyr::filter(cust_type == nonui_group_name)
  df_wide_ui <- df_wide %>%
    dplyr::filter(cust_type == ui_group_name)
  
  #get first UI week for UI recipients
  df_first_last_ui <- 
    df_wide_ui %>% 
    dplyr::filter(ui_inflows_mainyear > 0) %>% 
    group_by(cust_number) %>% 
    summarise(first_ui_week = min(week_start_date),
              last_ui_week_overall = max(week_start_date),
              n_ui_weeks_paid = n())
  df_wide_ui_out <- 
    df_wide_ui %>% 
    left_join(df_first_last_ui, by = 'cust_number')
  
  list_out <- list("ui" = df_wide_ui_out, "nonui" = df_wide_nonui)
  return(list_out)
}



#### UI backpay ####
backpay_identifier <- function(df, modal_ui_threshold){
  df_modal_ui <- df %>% 
    dplyr::filter(ui_inflows > 0) %>% 
    group_by(cust_number) %>% 
    mutate(modal_ui = mode_func(ui_inflows),
           ct_weeks_modal_ui = sum(ui_inflows == modal_ui)) %>% 
    #set modal UI = NA if each week has unique UI value
    mutate(modal_ui = ifelse(ct_weeks_modal_ui > 1, modal_ui, NA)) %>% 
    ungroup()
  df_out <- df_modal_ui %>% 
    group_by(cust_number) %>% 
    mutate(gets_backpay = max(ui_inflows >= modal_ui_threshold*modal_ui),
           #convert to boolean
           gets_backpay = ifelse(gets_backpay == 0, FALSE, TRUE)) %>% 
    ungroup() %>% 
    select(cust_number, cust_state, week_start_date, ui_inflows, first_ui_week, modal_ui, ct_weeks_modal_ui, gets_backpay) %>% 
    arrange(cust_number, week_start_date)
  #distinct()
  return(df_out)
}
mode_func <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#### Continuous UI/customer characteristics ####
func_ui_continuous <- function(df,
                               share_expected_weeks_weekly,
                               share_expected_weeks_biweekly,
                               weekly_states,
                               biweekly_states,
                               customer_col = 'cust_number',
                               first_ui_week = 'first_ui_week', 
                               last_ui_week = 'last_ui_week_overall', 
                               ct_ui_weeks = 'n_ui_weeks_paid',
                               cust_state = 'cust_state'){
  #' Given a df with customers, customer states, and customers' first UI week, last UI week, and number weeks receiving UI, 
  #' output a customer-level df with two columns: customer_col (eg, cust_number) and continuous_ui (boolean of whether the customer receives continuous UI).
  #' 
  #' param share_expected_weeks_weekly (numeric in [0, 1]): minimum share of expected weeks of UI that must appear to be considered a 
  #'  continuous UI recipient in weekly UI states.
  #' param share_expected_weeks_biweekly (numeric in [0, 1]): same as share_expected_weeks_weekly, but in biweekly UI states.
  #' param weekly_states (vector): list of state abbrevs of weekly states
  #' param biweekly_states (vector): list of state abbrevs of biweekly states
  
  df_out <- df %>% 
    transmute_(customer_col, 'first_ui_week' = first_ui_week, 'last_ui_week' = last_ui_week, 'ct_ui_weeks' = ct_ui_weeks, 'cust_state' = cust_state) %>% 
    distinct() %>% 
    ##only consider customers in weekly or biweekly states
    dplyr::filter(cust_state %in% c(weekly_states, biweekly_states)) %>% 
    mutate(expected_weeks_ui = ifelse(cust_state %in% weekly_states,
                                      as.numeric(last_ui_week - first_ui_week)/7 + 1,
                                      ceiling((as.numeric(last_ui_week - first_ui_week)/7)/2)),
           
           share_of_expected_weeks = ifelse(cust_state %in% weekly_states, ct_ui_weeks/expected_weeks_ui,
                                            ##allow people in biweekly states to have (expected_weeks_ui + 1) and be counted as 100% of expected weeks
                                            ifelse(ct_ui_weeks == expected_weeks_ui + 1,
                                                   1.0,
                                                   ct_ui_weeks/expected_weeks_ui)),
           
           ##determine whether each customer
           continuous_ui = ifelse(cust_state %in% weekly_states,
                                  (share_of_expected_weeks >= share_expected_weeks_weekly),
                                  (between(share_of_expected_weeks, share_expected_weeks_biweekly, 1.0)))
    )
  
  expect_gte(df_out %>% dplyr::filter(continuous_ui == TRUE) %>% pull(share_of_expected_weeks) %>% min(),
             min(share_expected_weeks_weekly, share_expected_weeks_biweekly))
  
  df_out <- df_out %>% select_(customer_col, 'continuous_ui')
  
  return(df_out)
}

ct_weeks_present <- function(df){
  df_out <- df %>% 
    group_by(cust_number) %>% 
    summarise(n_weeks_present = n())
  return(df_out)
}
