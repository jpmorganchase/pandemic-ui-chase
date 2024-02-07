# control_prep.R
# Author: Katie Zhang
# Date: 2022-01-11
# Constructs controls (employer, industry, age, gender, kids, marriage) for use
# in pgm/rep_rate_prep.R. Also creates the dataframe tmp_for_hazard_plot_expanded which 
# is used in just about everything in the job-finding analysis. 

# INS
rds_eips <- paste0(ifelse(small_samp==TRUE, data_1pct_path, data_path), 'eips/', '2023-02-22eips_list_', ifelse(small_samp==TRUE,1,100), 'pct.rds')
spell_level_df <- "df_ui_cust_spell" # should be in data image, generated in pgm/jobfind_build_2_of_2.R (takes a while to run)
df_balances_demog <- "df_demog_touse" # from pgm/ui_eip_data_read_in.R

# parameters and extract sample ----
states_rep_rate_expand <- c("NY", "IN", "WA", "GA", "NJ", "OH", "TX", "IL", "CA", "FL", "MI", "WI", "LA", "OR", "CT")
states_rep_rate_main_onset <- states_rep_rate_expand[!(states_rep_rate_expand %in% c("FL", "MI", "OR"))]
states_rep_rate_main_expire <-  states_rep_rate_expand[!(states_rep_rate_expand %in% c("FL", "TX", "WI", "LA", "CT"))]
states_rep_rate_timeseries <- states_rep_rate_expand[!(states_rep_rate_expand %in% c("FL", "TX", "MI", "WI", "LA", "CT"))]

start_window_expire <- as.Date("2020-06-01") #start date for jobfind expiration of $600
end_window_expire <- as.Date("2020-12-15") #end date for jobfind expiration of $600 plot
end_window_expire_reg <- as.Date("2020-09-30") #end date for jobfind expiration of $600 regression
start_window_onset <- as.Date("2020-11-01") #start date for jobfind onset of $300
end_window_onset_benefit <- as.Date("2020-12-31") #end date for measuring weekly benefit amount prior to onset of $300 supplement
end_window_onset <- as.Date("2021-05-09") #end date for jobfind onset of $300 plot
end_window_reg_onset <- as.Date("2021-03-14") #end date for jobfind onset of $300 regression 

last_reliable_date_payment_observed <- as.Date("2021-12-31") #last date that we see UI payments 
week_after_last_reliable_payment <- as.Date("2021-12-31") ##last_reliable_date_payment_observed + 7 days (example usage: if your last payment is on July 4, then your exit date is coded as July 7)
last_date_exit_timeseries <- as.Date("2021-12-26") #used as argument to weekly_summary

fortnight_states <- c("CA", "FL", "MI", "CO", "TX", "IL") 

# colours
greys <- RColorBrewer::brewer.pal(9, "Greys")
navyblue <- '#004577'
purple <-  '#8a89c4'
orange <- '#ffae5f'
colors_quant <- c(navyblue , '#2575ae', '#00a1df', '#a2dadb')
colors_qual <- c(navyblue , purple, "#ffae5f", "#bbd976",  "#a2dadb")

cyan0 <- "#00a1df"
green0 <- "#bbd976"
light_orange0 <- "#ffae5f"
red0 <- "#c84933"
navy0 <- navyblue

jpmci_colors <- c(cyan0, green0, light_orange0, red0, navyblue)

#OUTS
df_eip_inferred_kids_and_marriage <- "eip_kids"

################################################################################
# read in data if not already in data image ----
run_jf_build <- FALSE
if(!run_jf_build){
  date_scratch_jf_1_of_2 <- "2023-06-16"
  date_scratch_jf_2_of_2 <- "2023-06-16"
  date_scratch_read_in <- "2023-06-14"
  date_scratch_sp_jf <- "2023-06-15"
  
  df_demo_src_wins <- readRDS(str_c(data_path, "tmp/build_sp_jf/", date_scratch_sp_jf, 
                                    "df_demo_src_wins.rds"))
  df_ui_cust_week <- readRDS(str_c(data_path, "tmp/build_jf_2_of_2/", date_scratch_jf_2_of_2, 
                                   "df_ui_cust_week.rds"))
  df_eips <- read_rds(rds_eips) %>%
    mutate(eip_amt = as.numeric(eip_amt),
           eip_date = as.Date(eip_date)) %>%
    as_tibble()
  df_demog_touse <- readRDS(str_c(data_path, "tmp/read_in/", date_scratch_read_in, 
                                  "df_demog_touse.rds"))
  df_ui_cust_spell <- readRDS(str_c(data_path, "tmp/build_jf_2_of_2/", date_scratch_jf_2_of_2, 
                                    "df_ui_cust_spell.rds"))
  df_labor_cust_week_employer_2019 <- readRDS(str_c(data_path, "tmp/build_jf_1_of_2/", 
                                                    date_scratch_jf_1_of_2, 
                                                    "df_labor_cust_week_employer_2019.rds"))
  df_labor_cust_week_employer_2020 <- readRDS(str_c(data_path, "tmp/build_jf_1_of_2/", 
                                                    date_scratch_jf_1_of_2, 
                                                    "df_labor_cust_week_employer_2020.rds"))
  df_cp <- readRDS(str_c(data_path, "tmp/read_in/", date_scratch_read_in, 
                         "df_cp.rds"))
}

lwa_states <- (df_cp %>% filter(inflow_type == "lwa") %>% select(state) %>% distinct(state))$state
lwa_weekly_states <- lwa_states[!(lwa_states %in% fortnight_states)] 
#exclude FL b/c it appears to have a bug where no one leaves UI for a few weeks in Aug
job_search_states <- lwa_states[!(lwa_states %in% c("FL"))] 

# construct dataframe used in several plots downstream ----
# Notes:
# UI payments are not observed in Indiana prior to Feb 2020
# set start_month_cohort to one of the levels_cohort_pre_expire (if in those months), else NA
# spell_active is an identifier for each spell, = 1 from first to last week inclusive
# exit_labor is NA in very rare cases when a household does not have any labor market records
# exit_labor == 1 when we see an exit -8 to 2 weeks around UI starting (and there is labor market record)
# exit_ui == 1 the last week of UI inflows & exit_ui == NA when spell_active == FALSE
tmp_for_hazard_plot_expanded <- 
  df_ui_cust_week %>% 
  dplyr::filter(cust_state %in% states_rep_rate_expand,
                (cust_state != "IN" | week_start_date >= as.Date("2020-02-02"))) %>%
  mutate(exit_labor = ifelse(is.na(exit_labor), 0, exit_labor),
         exit_ui = ifelse(is.na(exit_ui), 0, exit_ui))

test_that("basic summary stats", {
  expect_equal(tmp_for_hazard_plot_expanded %>% nrow(), ifelse(small_samp == TRUE, 1801686, 125865375))
  expect_equal(tmp_for_hazard_plot_expanded %>% 
                 filter(spell_active) %>% 
                 summarise(mean(exit_ui)) %>% pull(), 
               0.0691, tol = 1e-3)
  expect_equal(tmp_for_hazard_plot_expanded %>% 
                 filter(exit_ui == 0) %>%
                 filter(start_recall %in% c(0, 1)) %>% # some obs with "-Inf"
                 summarise(mean(start_recall, na.rm = TRUE)) %>% pull(), 0.24414, tol = 1e-3)
})

# 1. SPELL LEVEL ATTRIBUTES ----
# age and gender ----
# define age as age at start of spell
age_gender_spell <- df_demog_touse %>%
  select(-cust_state) %>% # keep cust_state from df_ui_cust_spell instead
  left_join(df_ui_cust_spell %>%
              mutate(periodid = year(start_ui_date)*100 + month(start_ui_date)),
            by = c("cust_number", "periodid")) %>%
  filter(!is.na(inflow_type),
         cust_type == "202021_ui_recipient") %>%
  select(cust_number, age, gender, cust_state, ui_spell_number, start_ui_date)

test_that("Ncust age_gender_spell",
          expect_equal(age_gender_spell %>% 
                         distinct(cust_number) %>% 
                         nrow(), 
                       ifelse(small_samp == TRUE, 11940, 716737)))
test_that("Nrow age_gender_spell equal to distinct cust and ui spells numbers in age_gender_spell",
          expect_equal(age_gender_spell %>% distinct(cust_number, ui_spell_number) %>% nrow(),
             age_gender_spell %>% nrow()))

spell_level_attributes <-
  df_ui_cust_spell %>%
  filter(exit_labor == 1) %>%
  select(cust_number, start_ui_date, ui_spell_number) %>% 
  mutate(periodid = year(start_ui_date)*100 + month(start_ui_date)) %>%
  left_join(age_gender_spell, by = c("cust_number", "start_ui_date", "ui_spell_number"))

test_that("N customer-spells",
          expect_equal(nrow(spell_level_attributes %>% distinct(cust_number, ui_spell_number)),
                       ifelse(small_samp == TRUE, 11744, 651647) ))

test_that("Customer-spells is unique", 
          expect_equal(nrow(spell_level_attributes %>% distinct(cust_number, ui_spell_number)),
                       nrow(spell_level_attributes)) )


# 2. CUST-LEVEL ATTRIBUTES ----
# primary employer & industry ----
all_employers <- 
  bind_rows(df_labor_cust_week_employer_2019,
            df_labor_cust_week_employer_2020) %>%
  mutate(labor_amt = as.numeric(labor_amt)) %>%
  filter(labor_amt > 0,
         !is.na(labor_amt)) %>%
  select(-cust_type, -week_start_date_labor_lag, 
         -week_start_date_labor_lead)

customer_spell_dfs <- tmp_for_hazard_plot_expanded %>%
  filter(exit_labor == 1,
         between(start_ui_date, as.Date("2020-01-01"), as.Date("2021-06-30"))) %>%
  filter(exit_ui_date - start_ui_date != 0) %>%
  filter(start_ui) %>%
  group_by(cust_number) %>%
  slice(1) %>%
  ungroup() %>%
  select(cust_number, start_recall, start_ui_date, exit_ui_date)

primary_employer_dfs <- customer_spell_dfs %>%
  left_join(all_employers, by = "cust_number") %>%
  filter(between(start_ui_date - week_start_date, 0, 90)) %>%
  ungroup() %>%
  select(cust_number, at_counterparty_raw, labor_amt) %>%
  group_by(cust_number, at_counterparty_raw) %>%
  summarise(total = sum(labor_amt)) %>%
  arrange(desc(total)) %>%
  group_by(cust_number) %>%
  slice(1) %>%
  ungroup()

emp_industry_xwalk <- df_cp %>% distinct(at_counterparty_raw, naics_industry)

test_that("firm-industry xwalk is unique", 
          expect_equal(emp_industry_xwalk %>% distinct(at_counterparty_raw) %>% nrow(),
                       emp_industry_xwalk %>% nrow()))

# EIP-inferred marriage status ----
# generating nkids and filing_eip takes ~10 mins
eips_status <- df_eips %>% 
  arrange(cust_number, method, eip_date) %>% 
  group_by(cust_number) %>%
  slice(1) %>% # take DD over paper check and early over late
  mutate(remainder_single = (eip_amt - 1200) %% 500,
         remainder_couple = (eip_amt - 2400) %% 500,
         nkids = case_when(eip_amt <= 1200 ~ 0,
                           eip_amt %in% c(1200, 2400) ~ 0,
                           remainder_single == 0 & eip_amt > 1200 ~ (eip_amt - 1200) / 500,
                           remainder_couple == 0 & eip_amt > 2400 ~ (eip_amt - 2400) / 500,
                           TRUE ~ 0),
         filing_eip = case_when(eip_amt >= 1200 & remainder_single == 0 ~ "single",
                                eip_amt >= 2400 & remainder_couple == 0 ~ "married")) %>%
  select(-contains("remainder"), -num_eips_for_customer) %>%
  mutate(nkids = if_else(nkids > 5, # potentially mistake, some really really high eip amounts
                         5,
                         nkids))

# liquidity ----
pre_treat_bal_wins <- df_demo_src_wins %>% # ~ 3 mins
  filter(periodid %in% c(202003, 202007, 202010),
         grepl("ui_recipient", cust_type)) %>%
  select(cust_number, periodid, cust_type,
         checking_acct_balance) %>%
  pivot_wider(names_from = periodid,
              values_from = checking_acct_balance) %>%
  rename(pre_bal_exp = `202003`,
         pre_bal_jul = `202007`,
         pre_bal_ons = `202010`)

asset_quantile_df <- function(num_quant, pre_bal_mth) {
  pre_treat_bal_wins %>%
    mutate(pre_bal = !!sym(pre_bal_mth),
           pre_bal_quantile = ntile(pre_bal, num_quant)) %>%
    select(cust_number, !!sym(pre_bal_mth), pre_bal_quantile, cust_type)
}

asset_terciles_exp <- asset_quantile_df(3, "pre_bal_exp")
asset_terciles_jul <- asset_quantile_df(3, "pre_bal_jul")
asset_terciles_ons <- asset_quantile_df(3, "pre_bal_ons")

# join ----
cust_level_attributes <- eips_status %>%
  mutate(has_kids = nkids > 0) %>%
  select(-eip_date, -eip_amt, -method) %>%
  full_join(pre_treat_bal_wins, by = "cust_number") %>%
  full_join(primary_employer_dfs %>% select(-total), by = "cust_number") %>% 
  left_join(emp_industry_xwalk, by = "at_counterparty_raw")

test_that("N customers",
          expect_equal(nrow(cust_level_attributes %>% distinct(cust_number)),
                       ifelse(small_samp == TRUE, 20978, 1345454) ))

test_that("Unique cust-level data",
          expect_equal(nrow(cust_level_attributes %>% distinct(cust_number)),
                       nrow(cust_level_attributes) ))

# 3. JOIN SPELL-LEVEL AND CUST-LEVEL ----
attributes_covariates <- spell_level_attributes %>%
  full_join(cust_level_attributes, by = "cust_number") %>% 
  transmute(cust_number, start_ui_date, ui_spell_number, periodid,
            age = ifelse(age == "NaN", NA, age),
            age_bin = cut(age,
                          breaks = c(18, 24, 34, 44, 54, 107),
                          include.lowest = TRUE),
            gender,
            at_counterparty_raw, naics_industry,
            ind_factor = as.factor(naics_industry),
            filing_eip = factor(filing_eip,
                                levels = c("single",
                                           "married")),
            nkids, has_kids,
            pre_bal_ons, pre_bal_exp)

test_that("Ncust attributes_covariates", 
          expect_equal(attributes_covariates %>% distinct(cust_number) %>% nrow(), 
             ifelse(small_samp == TRUE, 20979, 1345479)))

test_that("Customer-spells is unique", 
          expect_equal(nrow(attributes_covariates %>% distinct(cust_number, ui_spell_number)),
                       nrow(attributes_covariates)) )

rm(df_eips, eips_status, age_gender_spell,
   all_employers, primary_employer_dfs,
   customer_spell_dfs, pre_treat_bal_wins,
   spell_level_attributes, cust_level_attributes)

