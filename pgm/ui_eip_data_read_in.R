#ui_eip_data_read_in.R
#Max Liebeskind
#Sep 2020
#Note: this script takes ~40-60 min to run
#This script reads in and cleans rds files from pyspark build

#### Setup Categories (if running categories) ####
durables = c(
  'Retail_Durables',
  'Entertainment',
  'Home_Improvement',
  'Insurance',
  'Auto_Repair',
  'Organizations_&_Institutions',
  'schools')

non_durables = c(
  'Drug_Stores',
  'Groceries',
  'Miscellaneous_Nondurables',
  'Department_Stores',
  'Discount_Stores',
  'Telecom',
  'Hospitals',
  'Other_Healthcare',
  'Utilities',
  'Transit_&_Ground_Transportation',
  'Cash',
  'Professional_&_Personal_Services',
  'Other_Retail',
  'Flights',
  'Restaurant',
  'Clothing'
)

debt_disagg_cols = c('on_us_creditcard_pmts',
                     'off_us_creditcard_pmts',
                     'mortgage_pmts',
                     'student_loan_pmts',
                     'other_loan_pmts',
                     'auto_loan_pmts'
)

cats_vector <- c(durables, 
                 non_durables,
                 debt_disagg_cols)

#### SETUP ####
#PATHS [make sure to first activate uieip Rproject]
path_repo <- paste0('/data/jpmci/users/', user, '/uieip/')
if (user == "peter") {
  path_out <- paste0(path_repo, 'out/pub/')
} else {
  path_out <- paste0(path_repo, 'out/pub/')
}
data_path <- "/data/jpmci/teams/gnlab/ui_covid/"

source(str_c(path_repo, "pgm/funcs/ui_data_readin_functions.R"))

#INS ####
#last periodid in data on disk
latest_pid <- 202112
#last periodid analyzed
latest_pid_for_sample_screen <- 202112
#whether this iteration of the build will rebuild the spend-side dfs. 
#When I was only working on the job-finding side for a period of time, I would switch this to FALSE. 
inc_spend_results <- TRUE
#the demog RDS files come in batches of four months (for example if you look at the latest file it says 202105 to 202108). 
#Sometimes we don’t actually have data for the four months, for example for a while we only actually had data for 202105 to 
#202107 in the last file, so we would set increment_for_not_updated_demog to 1 to account for this missing month. 
#(this is used in the filter statement on ct_months when constructing cust_to_include)
increment_for_not_updated_demog <- 0

#parameters to reallocate spending from Mar 1 and 2 to Feb 29: our data is missing Feb 29, and the spending on Feb 29 has been incorrectly allocated to Mar 1 and 2
share_mar1st2nd_feb29 <- .402 #number from Tessa
share_weekmar1_mar1 <- .0586 #computed in Excel
share_weekmar1_mar2 <- .2957 #computed in Excel
share_shift_from_mar1week_to_feb23week = share_mar1st2nd_feb29*(share_weekmar1_mar1 + share_weekmar1_mar2)

feb29_adj_cols <- c("total_inflows" , 
                    "labor_inflows" ,
                    "tax_refund_inflows" , 
                    "transfer_inflows" ,
                    "ct_ui_txns" , 
                    "total_spend_narrow" , 
                    "total_spend_expanded" ,
                    "total_outflows" ,
                    "debt_payments" , 
                    "durable" , 
                    "non_durable" ,
                    "periodid" )

if(run_categories){
  feb29_adj_cols <- c(feb29_adj_cols, cats_vector)
}

##tables####
pids_2019_customers <- c(seq(201901, 201912)) #2019 ui recips and non-ui customers have data from 201801 thru 201912

if(small_samp == TRUE){
  weekly_flows_202021_latest <- "2023-02-06"
  weekly_flows_2019_latest <- "2023-02-06"
  rds_demog_latest <- "2023-02-06"
  eips_latest <- "2023-02-06"
  weekly_cp_latest <- "2023-02-06"
  data_1pct_path <- "/data/jpmci/teams/gnlab/ui_covid/1pct_data/"
  # prefixes
  pids_2020_customers <- c(201912, seq(202001, 202012), seq(202101, latest_pid)) 
  rds_weekly_prefix_2020 <- str_c(data_1pct_path,"weekly_flows/", weekly_flows_202021_latest, "weekly_flows_1pct_")
  prefix_2020_ui_recip <- paste0(rds_weekly_prefix_2020, "202021_ui_recipient_")
  prefix_2020_nonui_recip <- paste0(rds_weekly_prefix_2020, "2020_nonui_recipient_")
  rds_weekly_prefix_2019 <- str_c(data_1pct_path, "weekly_flows/", weekly_flows_2019_latest, "weekly_flows_1pct_")
  prefix_2019_ui_recip <- paste0(rds_weekly_prefix_2019, "2019_ui_recipient_")
  prefix_2019_nonui_recip <- paste0(rds_weekly_prefix_2019, "2019_nonui_recipient_")
  rds_demog_list <- paste0(data_1pct_path, "demog/",
                           list.files(path = paste0(data_1pct_path, "demog/"),
                                      pattern = rds_demog_latest))
  rds_eips <- paste0(data_1pct_path, "eips/", eips_latest, "eips_list_1pct.rds")
} else {
  pids_2020_customers <- c(201912, seq(202001, 202012), seq(202101, latest_pid)) #2020 ui recips and non-ui customers have data from 201901 thru last pid of 2020 data
  if(run_categories){
    weekly_flows_202021_latest <- "2023-06-08" #date must change too
    weekly_flows_2019_latest <-  "2023-06-08" # haven't re-built 2019
    rds_demog_latest <- "2023-06-11" # date implies disagg (cats)
    # read the files:
    rds_weekly_prefix_2020 <- str_c(data_path, "weekly_flows/", weekly_flows_202021_latest, "weekly_flows_100pct_disagg_") #CHANGE THIS EACH MONTH
    prefix_2020_ui_recip <- paste0(rds_weekly_prefix_2020, "202021_ui_recipient_")
    prefix_2020_nonui_recip <- paste0(rds_weekly_prefix_2020, "2020_nonui_recipient_")
    rds_weekly_prefix_2019 <- str_c(data_path, "weekly_flows/", weekly_flows_2019_latest, "weekly_flows_100pct_disagg_") ##prefix for 2019 UI recipients
    prefix_2019_ui_recip <- paste0(rds_weekly_prefix_2019, "2019_ui_recipient_")
    prefix_2019_nonui_recip <- paste0(rds_weekly_prefix_2019, "2019_nonui_recipient_")
  } else {
    weekly_flows_202021_latest <- "2023-02-20"
    weekly_flows_2019_latest <- "2021-09-26"
    rds_demog_latest <- "2023-02-22"
    # read the files:
    rds_weekly_prefix_2020 <- str_c(data_path, "weekly_flows/", weekly_flows_202021_latest, "weekly_flows_100pct_") #CHANGE THIS EACH MONTH
    prefix_2020_ui_recip <- paste0(rds_weekly_prefix_2020, "202021_ui_recipient_")
    prefix_2020_nonui_recip <- paste0(rds_weekly_prefix_2020, "2020_nonui_recipient_")
    rds_weekly_prefix_2019 <- str_c(data_path, "weekly_flows/", weekly_flows_2019_latest, "weekly_flows_100pct_") ##prefix for 2019 UI recipients
    prefix_2019_ui_recip <- paste0(rds_weekly_prefix_2019, "2019_ui_recipient_")
    prefix_2019_nonui_recip <- paste0(rds_weekly_prefix_2019, "2019_nonui_recipient_")
  }
  #CHANGE THIS EACH TIME NEW PYSPARK BUILD IS DONE
  eips_latest <- "2023-02-22"
  weekly_cp_latest <- "2023-02-22"
  # prefixes
  rds_demog_list <- paste0(data_path, "demog/",
                           list.files(path = paste0(data_path, "demog/"),
                                      pattern = rds_demog_latest)) %>%
    # Remove the 2018 assets. Only start reading in from 201901.
    str_subset(pattern = "2018", negate = TRUE)
  rds_eips <- paste0(data_path, "eips/", eips_latest, "eips_list_100pct.rds")
}

## OUTS ####
pdf_diagnostics <- paste0(path_out, 'ui_eip_data_readin_diagnostics.pdf')
###dfs created throughout the script
#df_demog_touse: customer-periodid table with demographic and monthly information (eg, balances)
#df_nonui_clean: customer-by-week df of non-ui customers
#df_ui_clean: customer-by-week df of ui customers
#df_ui_custlevel: customer-level df with info on number UI weeks paid, first/last UI week, backpay, check schedule (even/odd)
#even_odd_custs: customer-level df with list of even week, odd week customers
#even_odd_weekly: customer-weekly df with whether each week is an even or odd week
#rdf_eips: customer-level df with EIP info
#cust_to_include 
#df_cp 

################################################## EXECUTION ##################################################

#### DEMOGRAPHICS, EIPS, LIST OF CUSTOMERS TO INCLUDE####
tm <- Sys.time() ##takes ~20 min

df_demog_raw <- rds_demog_list %>% map_dfr( ~ read_rds(.))
if(run_categories){
  df_demog <- df_demog_raw %>%
    mutate_at(vars(periodid, total_inflows:Clothing, total_liquid_balances, 
                   checking_acct_balance, labor_inflows_2019), as.numeric)
} else {
  df_demog <- df_demog_raw %>% 
    mutate_at(vars(periodid, total_inflows:total_ui_inflows, total_liquid_balances, 
                   checking_acct_balance, labor_inflows_2019), as.numeric)
}

test_that('Customer-periodids is not unique! 
          Customer-periodids-cust_type is unique. cust_type can be nonui_2019 and nonui_2020',
          {
            nrow <- df_demog %>% filter(periodid == 202004) %>% nrow()
            expect_equal(nrow, df_demog %>% filter(periodid == 202004) %>% distinct(cust_number, cust_type) %>% nrow())
            expect_gt(nrow, df_demog %>% filter(periodid == 202004) %>% distinct(cust_number) %>% nrow())
          }
)

test_that("N rows df_demog",
          expect_equal(df_demog %>% nrow(), ifelse(small_samp == TRUE, 930268, 58239767)))

cust_to_include <- 
  df_demog %>% 
  #during [202001, 202103], included customers must #PN note: changed on 3/29 to be just through 202103 instead of latest_pid per JIRA-223. Led to 3.5% increase in sample.
  # (1) be present in every month,
  # (2) have non-0 inflows and non-0 outflows in every month,
  # (3) have non-null balance in every month
  dplyr::filter(between(periodid, 202001, latest_pid_for_sample_screen)) %>%
  group_by(cust_number, cust_type) %>% #group by (cust_number, cust_type) because a customer can have multiple types (eg, nonui 2019 and nonui 2020)
  mutate(ct_months = n(), 
         min_inflows = min(total_inflows), 
         min_outflows = min(total_outflows),
         ct_null_chkbal = sum(ifelse(is.na(checking_acct_balance), 1, 0)),
         ct_null_liqbal = sum(ifelse(is.na(total_liquid_balances), 1, 0))) %>% 
  ungroup() %>%
  dplyr::filter(ct_months == latest_pid_for_sample_screen-202101+13-increment_for_not_updated_demog, #present every month since 2020
                #positive inflows/outflows every month
                min_inflows > 0,
                min_outflows > 0,
                #non-null balance in every month
                ct_null_chkbal == 0,
                ct_null_liqbal == 0) %>% 
  #keep nonui_2020 2020 and 2019 ui recipients (since 2019_ui_recipient can also get UI in 2020)
  dplyr::filter(periodid == 202001) %>%  #determine state based on state in 202001
  distinct(cust_number, cust_state)

test_that('N customers to include',
          expect_equal(cust_to_include %>% nrow(), ifelse(small_samp == TRUE, 18882, 1170926)))

cust_to_include %>%
  saveRDS(str_c(ifelse(small_samp == TRUE, data_1pct_path, data_path), 
                "tmp/read_in/", Sys.Date(), 
                ifelse(run_categories==TRUE, 
                       "cust_to_include_disagg.rds",
                       "cust_to_include.rds")))

test_that('Customers should be uniquely associated with states in a single periodid',
          expect_equal(cust_to_include %>% nrow(), 
                       cust_to_include %>% distinct(cust_number) %>% nrow())
)

df_demog_touse <- 
  df_demog %>% 
  inner_join(cust_to_include %>% select(cust_number),
             by = 'cust_number') #mutate_at(vars(total_inflows:transfers, total_liquid_balances, checking_acct_balance), as.numeric)


test_that('N rows demog', expect_equal(df_demog_touse %>% nrow(), ifelse(small_samp == TRUE, 698472, 43346736)))
test_that('UI recipients in demographic table are unique',
          expect_equal(
            df_demog_touse %>% filter(cust_type == "202021_ui_recipient", periodid == 202004) %>% distinct(cust_number) %>% nrow(),
            df_demog_touse %>% filter(cust_type == "202021_ui_recipient", periodid == 202004) %>% nrow()
          ))

df_demog_touse %>%
  saveRDS(str_c(ifelse(small_samp == TRUE, data_1pct_path, data_path), 
                "tmp/read_in/", Sys.Date(), 
                ifelse(run_categories==TRUE, 
                       "df_demog_touse_disagg.rds",
                       "df_demog_touse.rds")))

rm(df_demog)

# read in EIP data
# warning: each row is one EIP. this means one customer can have multiple rows
df_eips <- read_rds(rds_eips) %>%
  mutate(eip_amt = as.numeric(eip_amt),
         eip_date = as.Date(eip_date)) %>%
  as_tibble()

print(Sys.time() - tm)

#### LABOR COUNTERPARTY DATA READ IN ####
tm <- Sys.time() #takes 20 min

df_cp_read_in <- list.files(str_c(ifelse(small_samp == TRUE, data_1pct_path, data_path), "weekly_cp"), 
                    pattern = weekly_cp_latest, full.names = TRUE) %>%
  map_dfr(read_rds) %>% 
  inner_join(cust_to_include %>% select(cust_number))

if(run_categories){
  df_cp_read_in <- df_cp_read_in%>%
    filter(periodid < 202109)
}

test_that('N rows counterparty',
          expect_equal(
            df_cp_read_in %>% nrow(),
            ifelse(small_samp == TRUE, 2141709, 133238620)
          ))

# To clean up post-March inflow issues, remove NA counterparties with non-zero inflow_amounts
df_cp <- df_cp_read_in %>%
  filter(!(is.na(at_counterparty_raw) & as.numeric(inflow_amount)>0))

test_that('N rows counterparty after filter',
          expect_equal(df_cp %>% nrow(), ifelse(small_samp == TRUE, 2013829, 125180041)))
print(Sys.time() - tm)

df_cp %>%
  saveRDS(str_c(
    ifelse(small_samp == TRUE, data_1pct_path, data_path),
    "tmp/read_in/",
    Sys.Date(),
    ifelse(run_categories==TRUE, 
           "df_cp_disagg.rds",
           "df_cp.rds")
  ))

#### WEEKLY DATA READIN ####
tm <- Sys.time() #takes ~20 min

# weekly data
if(run_categories){
  convert_old_data <- function(df) {
    df %>%
      select(cust_number, week_year, week_num, week_start_date, total_inflows, labor_inflows,
             tax_refund_inflows, transfer_inflows,
             ct_ui_txns,
             total_spend_narrow,
             total_spend_expanded,
             cats_vector,
             total_outflows,
             debt_payments,
             durable, non_durable, periodid, gets_ui_2019, gets_ui_202021) %>%
      mutate_at(vars(total_inflows:gets_ui_202021), as.numeric)
  }
} else {
  convert_old_data <- function(df) {
    df %>%
      transmute(cust_number, week_year, week_num, week_start_date, total_inflows, labor_inflows,
                tax_refund_inflows, transfer_inflows,
                ct_ui_txns,
                total_spend_narrow,
                total_spend_expanded,
                total_outflows, 
                debt_payments,
                durable, non_durable, periodid, gets_ui_2019, gets_ui_202021) %>%
      mutate_at(vars(total_inflows:gets_ui_202021), as.numeric)
  }
}

if(run_categories){
  ui_recip_raw <- read_rds(paste0(prefix_2019_ui_recip, "201912", '.rds'))
  # MAY 23 and June 13: RE RUN FROM HERE WITH REVERTED CATS
  ui_recip <- ui_recip_raw %>%
    convert_old_data()
  nonui_recip_raw <- read_rds(paste0(prefix_2019_nonui_recip, "201912", '.rds'))
  nonui_recip <- nonui_recip_raw %>%
    convert_old_data()
}

df_weekly_data <-
  bind_rows(
    pids_2020_customers %>%
      map_dfr(~ read_rds(paste0(prefix_2020_ui_recip, ., '.rds'))) %>%
      mutate_at(vars(total_inflows:gets_ui_202021), as.numeric),
    pids_2020_customers %>%
      map_dfr(~ read_rds(paste0(prefix_2020_nonui_recip, ., '.rds'))) %>%
      mutate_at(vars(total_inflows:gets_ui_202021), as.numeric),
    read_rds(paste0(prefix_2019_ui_recip, "201912", '.rds')) %>%
      convert_old_data(),
    read_rds(paste0(prefix_2019_nonui_recip, "201912", '.rds')) %>%
      convert_old_data()
  ) %>%
  inner_join(cust_to_include, by = 'cust_number') %>%  #only want weekly data for customers in cust_to_include
  mutate(week_start_date = as.Date(week_start_date))

test_that('N rows weekly data',
          expect_equal(df_weekly_data %>% nrow(), ifelse(small_samp == TRUE, 1292143, 97554296)))

print(Sys.time() - tm)


# #### FEB 29 ADJUSTMENT ####
tm <- Sys.time() #Takes ~ 6 mins
##separate out data from weeks of 2/23/20 and 3/1/20 from all other weeks
df_weekly_toadj <- df_weekly_data %>%
  dplyr::filter(week_start_date %in% as.Date(c('2020-03-01', '2020-02-23')))
df_weekly_noadj <- df_weekly_data %>%
  dplyr::filter((week_start_date %in% as.Date(c('2020-03-01', '2020-02-23'))) == FALSE)
##for weeks 2/23/20 and 3/1/20, apply adjustment
shifter <- function(colname){colname*share_shift_from_mar1week_to_feb23week}
df_weekly_toadj <- df_weekly_toadj %>%
  ##calculate amount to shift  from week of Mar 1  to week of Feb 23
  mutate_at(.vars = vars(feb29_adj_cols),
            .funs = list(shift = ~ ifelse((week_start_date == as.Date('2020-03-01')),
                                          shifter(.), NA))) %>%
  group_by(cust_number) %>%
  mutate_at(.vars = vars(contains('shift')),
            .funs = mean, na.rm = TRUE) %>% ##single 'shift' amount per column per customer
  ungroup()
df_weekly_toadj <- df_weekly_toadj %>%
  ##apply shift to weeks of Mar 1 and Feb 23: subtract shifter from Mar 1, add shifter to Feb 23
  mutate_at(.vars = vars(feb29_adj_cols),
            function(x, week_start_date){
              shift_var <- paste0(rlang::as_label(substitute(x)), "_shift")
              ifelse(week_start_date == as.Date('2020-03-01'),
                     x - df_weekly_toadj[[shift_var]],
                     ifelse(week_start_date == as.Date('2020-02-23'),
                            x + df_weekly_toadj[[shift_var]],
                            x)
              )
            },
            df_weekly_toadj$week_start_date
  ) %>%
  select(-contains('shift'))
##unit tests
expect_equal(df_weekly_noadj %>% colnames(),
             df_weekly_toadj %>% colnames())
expect_equal((df_weekly_noadj %>% nrow()) + (df_weekly_toadj %>% nrow()),
             df_weekly_data %>% nrow())
##recombine adjusted data for 2/23/20, 3/1/20 and data from all other weeks
df_weekly_data_adj <- bind_rows(df_weekly_noadj, df_weekly_toadj)
print(Sys.time() - tm)

#### CLEAN AND WINSORIZE ####
tm <- Sys.time() #takes ~ 12 mins
list_weekly_dfs_2020 <- microdata_cleaner(df_weekly_data_adj, 2020, 2019,
                                          '202021_ui_recipient', 'nonui_2020')
list_weekly_dfs_2021 <- microdata_cleaner(df_weekly_data_adj, 2021, 2020,
                                          '202021_ui_recipient', 'nonui_2020')
df_ui_clean_2020 <- list_weekly_dfs_2020[['ui']]
df_ui_clean_2021 <- list_weekly_dfs_2021[['ui']]
df_nonui_clean_2020 <- list_weekly_dfs_2020[['nonui']]
df_nonui_clean_2021 <- list_weekly_dfs_2021[['nonui']]

test_that("Nrow df_ui_clean_2020", expect_equal(nrow(df_ui_clean_2020), ifelse(small_samp == TRUE, 623949, 37912099)))
test_that("Nrow df_ui_clean_2021", expect_equal(nrow(df_ui_clean_2021), ifelse(small_samp == TRUE, 369192, 37925792)))
test_that("Ncust df_ui_clean_2020", 
          expect_equal(df_ui_clean_2020 %>% distinct(cust_number) %>% nrow(), 
                       ifelse(small_samp == TRUE, 12056, 731776)))
test_that("Ncust df_ui_clean_2021", 
          expect_equal(df_ui_clean_2021 %>% distinct(cust_number) %>% nrow(), 
                       ifelse(small_samp == TRUE, 12056, 731776)))
test_that("Ncust same",
          expect_equal(df_ui_clean_2020 %>% distinct(cust_number) %>% nrow(),
             df_ui_clean_2021 %>% distinct(cust_number) %>% nrow()))
test_that("Nrow df_nonui_clean_2020",
          expect_equal(nrow(df_nonui_clean_2020), 
                       ifelse(small_samp == TRUE, 147627, 8490152)))
test_that("Nrow df_nonui_clean_2021",
          expect_equal(nrow(df_nonui_clean_2021), 
                       ifelse(small_samp == TRUE, 87283, 8499387)))
test_that("Ncust df_nonui_clean_2020",
          expect_equal(df_nonui_clean_2020 %>% distinct(cust_number) %>% nrow(), 
             ifelse(small_samp == TRUE, 2862, 164346)))

df_ui_clean_combine <-
  bind_rows(df_ui_clean_2020,
            df_ui_clean_2021) %>%
  distinct()

# quick fix to uniqueness issue of binding rows
df_ui_clean_cust_distinct_df <- 
  df_ui_clean_combine %>%
  distinct(cust_number, first_ui_week,
           last_ui_week_overall, n_ui_weeks_paid) %>%
  group_by(cust_number) %>%
  summarise(first_ui_week = min(first_ui_week, na.rm = TRUE),
            last_ui_week_overall = max(last_ui_week_overall, na.rm = TRUE),
            n_ui_weeks_paid = sum(n_ui_weeks_paid, na.rm = TRUE))

df_ui_clean <-
  df_ui_clean_combine %>%
  select(-c(first_ui_week, last_ui_week_overall, n_ui_weeks_paid)) %>%
  distinct() %>%
  left_join(df_ui_clean_cust_distinct_df, by = "cust_number")

df_nonui_clean <- bind_rows(df_nonui_clean_2020,
                            df_nonui_clean_2021) %>%
  distinct()

rm(list_weekly_dfs_2020, list_weekly_dfs_2021,
   df_ui_clean_2020, df_ui_clean_2021,
   df_nonui_clean_2020, df_nonui_clean_2021,
   df_ui_clean_combine, df_ui_clean_cust_distinct_df)

#### Read EIP2 and EIP3 ####
if(small_samp == TRUE){
  rds_eip_rounds <- paste0(data_1pct_path, 'eips/', '2022-03-11eip_rounds_list_1pct.rds')
} else {
  rds_eip_rounds <- paste0(data_path, 'eips/', '2023-02-22eip_rounds_list_100pct.rds')
}

df_eip_rounds <- read_rds(rds_eip_rounds) %>%  
  mutate_at(vars(eip1_amount, eip2_amount, eip3_amount), ~ as.numeric(.)) %>%
  mutate_at(vars(eip1_date, eip2_date, eip3_date), ~ as.Date(.)) %>%
  as_tibble() %>%
  distinct()
test_that("df_eip_rounds nrow equal to ncust",
          expect_equal(df_eip_rounds %>% nrow(), 
                       df_eip_rounds %>% distinct(cust_number) %>% nrow()))
test_that("df_eip_rounds nrow",
          expect_equal(df_eip_rounds %>% nrow(), 
                       ifelse(small_samp == TRUE, 16641, 1080685)))

print(Sys.time() - tm)

df_nonui_clean %>% 
  saveRDS(str_c(ifelse(small_samp == TRUE, data_1pct_path, data_path), 
                "tmp/read_in/", Sys.Date(), 
                ifelse(run_categories==TRUE, 
                       "df_nonui_clean_disagg.rds",
                       "df_nonui_clean.rds")))
df_ui_clean %>% 
  saveRDS(str_c(ifelse(small_samp == TRUE, data_1pct_path, data_path), 
                "tmp/read_in/", Sys.Date(), 
                ifelse(run_categories==TRUE,
                       "df_ui_clean_disagg.rds",
                       "df_ui_clean.rds"))) 
rm(cust_to_include)
rm(df_weekly_data_adj)
rm(data_spike_diagnostic, df_weekly_data,
   data_backpay, data_for_even_odd, df_backpay_custlevel, df_backpay_maylater_start, df_backpay_precovidstart)
rm(even_odd_custs, even_odd_weekly)
