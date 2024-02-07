# mpc_more_controls.R

run_spend_build <- TRUE
if(!run_spend_build){
  # Note: It is more convenient if you just run spend_build.
  source("pgm/spend_build.R")
}

# Read in df_eips:
rds_eips <- paste0(ifelse(small_samp==TRUE, data_1pct_path, data_path), 
                   'eips/', '2023-02-22eips_list_', 
                   ifelse(small_samp==TRUE,1,100), 'pct.rds')
df_eips <- read_rds(rds_eips) %>%
  mutate(eip_amt = as.numeric(eip_amt),
         eip_date = as.Date(eip_date)) %>%
  as_tibble()

df_demog_touse <- readRDS(str_c(data_path, 
                                "tmp/read_in/", "2023-06-14", 
                                "df_demog_touse.rds"))
cust_age_state <- df_demog_touse %>%
  filter(cust_type=="202021_ui_recipient" | cust_type=="nonui_2020") %>%
  select(cust_number, age, cust_state) %>%
  distinct(cust_number,.keep_all = TRUE)

# 2. CUST-LEVEL ATTRIBUTES ----
# primary employer & industry ----
if(!exists(c("df_labor_cust_week_employer_2019", "df_labor_cust_week_employer_2020"))){
  date_scratch_jf_1_of_2 <- "2023-06-16"
  df_labor_cust_week_employer_2019 <- readRDS(str_c(data_path, "tmp/build_jf_1_of_2/", 
                                                    date_scratch_jf_1_of_2, 
                                                    "df_labor_cust_week_employer_2019.rds"))
  df_labor_cust_week_employer_2020 <- readRDS(str_c(data_path, "tmp/build_jf_1_of_2/", 
                                                    date_scratch_jf_1_of_2, 
                                                    "df_labor_cust_week_employer_2020.rds"))
}

all_employers <- 
  bind_rows(df_labor_cust_week_employer_2019,
            df_labor_cust_week_employer_2020) %>%
  mutate(labor_amt = as.numeric(labor_amt)) %>%
  filter(labor_amt > 0,
         !is.na(labor_amt)) %>%
  select(-cust_type, -week_start_date_labor_lag, 
         -week_start_date_labor_lead)

primary_employer_dfs <- all_employers %>%
  select(cust_number, at_counterparty_raw, labor_amt) %>%
  group_by(cust_number, at_counterparty_raw) %>%
  summarise(total = sum(labor_amt)) %>%
  arrange(desc(total)) %>%
  group_by(cust_number) %>%
  slice(1) %>%
  ungroup()

if(!exists("df_cp")){
  date_scratch_read_in <- "2023-06-14"
  df_cp <- readRDS(str_c(data_path, "tmp/read_in/", date_scratch_read_in, "df_cp.rds"))
}

emp_industry_xwalk <- df_cp %>% distinct(at_counterparty_raw, naics_industry)

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

cust_covariates <- eips_status %>%
  mutate(has_kids = nkids > 0) %>%
  select(-eip_date, -eip_amt, -method) %>%
  full_join(cust_age_state,by = "cust_number")%>%
  full_join(primary_employer_dfs %>% select(-total), by = "cust_number") %>% 
  left_join(emp_industry_xwalk, by = "at_counterparty_raw") %>%
  ungroup()

cust_covariates_binned <- cust_covariates %>%
  transmute(cust_number, 
            age_bin = cut(age,
                          breaks = c(18, 24, 34, 44, 54, 107),
                          include.lowest = TRUE),
            ind_factor = as.factor(naics_industry),
            has_kids_factor = as.factor(has_kids),
            state_factor = as.factor(cust_state))

#MPC calculations ----
source("pgm/funcs/mpc_robustness_funcs_more_controls.R")
orig_df_demo_src_wins <- df_demo_src_wins
samples <- c("all")
controls <- c("none","state","state_age","state_age_kids","state_age_kids_industry","age_kids")

table_combined <-  map2(samples,controls,~ filter_mpc_sample(.x) %>% 
                          construct_weighted_controls() %>%
                          run_apc_calculations(., .x,.y) %>%
                          as_tibble) %>%
  set_names(str_c("table_iv_apcs_combined_more_controls", controls)) %>%
  list2env(.GlobalEnv)

table_iv_apcs_none <- table_iv_apcs_combined_more_controlsnone %>%
  filter(numerator=="Spend (total)",denominator=="income") %>%
  rename(apc_none=apc,se_none=se,n_obs_none=n_obs)

table_iv_apcs_state <- table_iv_apcs_combined_more_controlsstate %>%
  filter(numerator=="Spend (total)",denominator=="income") %>%
  rename(apc_state=apc,se_state=se,n_obs_state=n_obs)

table_iv_apcs_state_age <- table_iv_apcs_combined_more_controlsstate_age %>%
  filter(numerator=="Spend (total)",denominator=="income") %>%
  rename(apc_state_age=apc,se_state_age=se,n_obs_state_age=n_obs)

table_iv_apcs_state_age_kids <- table_iv_apcs_combined_more_controlsstate_age_kids %>%
  filter(numerator=="Spend (total)",denominator=="income") %>%
  rename(apc_state_age_kids=apc,se_state_age_kids=se,n_obs_state_age_kids=n_obs)

table_iv_apcs_state_age_kids_industry <- table_iv_apcs_combined_more_controlsstate_age_kids_industry %>%
  filter(numerator=="Spend (total)",denominator=="income") %>%
  rename(apc_state_age_kids_industry=apc,se_state_age_kids_industry=se,n_obs_state_age_kids_industry=n_obs)

table_iv_apcs_age_kids <- table_iv_apcs_combined_more_controlsage_kids %>%
  filter(numerator=="Spend (total)",denominator=="income") %>%
  rename(apc_age_kids=apc,se_age_kids=se,n_obs_age_kids=n_obs)

apc_table_iv_controls <- table_iv_apcs_none %>% 
  inner_join(table_iv_apcs_state,by=c("design","numerator","denominator")) %>%
  inner_join(table_iv_apcs_state_age,by=c("design","numerator","denominator")) %>%
  inner_join(table_iv_apcs_state_age_kids,by=c("design","numerator","denominator")) %>%
  inner_join(table_iv_apcs_state_age_kids_industry,by=c("design","numerator","denominator")) %>%
  inner_join(table_iv_apcs_age_kids,by=c("design","numerator","denominator"))

col_order <- c("apc_none", "apc_state", "apc_state_age",
               "apc_state_age_kids", "apc_state_age_kids_industry","apc_age_kids","design",
               "se_none", "se_state", "se_state_age",
               "se_state_age_kids", "se_state_age_kids_industry","se_age_kids",
               "n_obs_none", "n_obs_state", "n_obs_state_age",
               "n_obs_state_age_kids", "n_obs_state_age_kids_industry","n_obs_age_kids","numerator","denominator")
apc_table_iv_controls <- apc_table_iv_controls[, col_order]

apc_table_iv_controls %>% write_csv(str_c(path_out,
                                           'apc_table_iv_controls.csv')) 

# MPC tex table formatting ----
apc_table_iv_controls <- read.csv(str_c(path_out, "apc_table_iv_controls.csv"))
apc_table_iv_controls <- apc_table_iv_controls %>%
  filter(!(design %in% c("LWA", "$300 expiration June vs. Sept states")))


apc <- apc_table_iv_controls %>%
  select(design,contains("apc_")) %>%
  mutate(group="apc") %>%
  rename_at(vars(starts_with("apc")), funs(sub("apc", "value", .))) %>%
  rbind(., (apc_table_iv_controls %>%
          select(design, contains("se_")) %>%
          mutate(group="se") %>%
          rename_at(vars(starts_with("se")), funs(sub("se", "value", .))))) %>%
  select(-c(value_state_age_kids_industry, value_age_kids)) %>%
  arrange(factor(design, 
                 levels = c("UI onset (waiting)","FPUC expiration", "$300 onset",
                            "$300 expiration June states", "$300 expiration Sept states"))) %>%
  mutate_at(vars(contains("value_")), 
            ~(ifelse(group == "se", 
                     str_c("(", format(round(., digits=2), nsmall = 2), ")"), 
                     format(round(., digits=2), nsmall = 2)))) %>%
  mutate(design = rep(c("Waiting for benefit", "Expiration of 600 supplement",
                        "Onset of 300 supplement", "Expiration of 300 supplement (June states)",
                        "Expiration of 300 supplement (September states)"), each = 2),
         design = ifelse(group == "se", "", design)) %>%
  select(-group) %>%
  rbind(c("hline", rep(NA, 4)),
        c("State*SuppAvail FE", "", rep("X", 3)),
        c("Age*SuppAvail FE", "", "", rep("X", 2)),
        c("HasKids*SuppAvail FE", rep("",3), "X")) %>%
  `colnames<-`(c("Research Design", "Total Spending MPC"))

apc_tex <- apc %>%
  stargazer(., summary=FALSE, type="latex", rownames = FALSE) 

apc_tex %>%
  str_replace_all("Total Spending MPC & NA & NA.1 & NA.2 ",
                  "\\\\multicolumn{4}{|c|}{Total Spending MPC}") %>%
  str_replace_all("hline &  &  &  &  \\\\\\\\", 
                  "\\\\hline \\\\\\\\[-1.8ex]") %>%
  str_replace_all("600", "$600") %>%
  str_replace_all("300", "$300") %>%
  str_subset("\\{table\\}|caption|label", negate = TRUE) %>%
  str_replace_all("ccccc", "lcccc") %>%
  writeLines(str_c(path_out, "apc_table_iv_controls.tex"))

apc_html_file <- apc %>%
  stargazer(., summary=FALSE, type="html", rownames = FALSE) 

apc_html_file %>%
  str_replace_all("<td>Total Spending MPC</td><td>NA</td><td>NA.1</td><td>NA.2</td></tr>", 
                  "<td colspan='4'>Total Spending MPC</td></tr>")%>%
  str_replace_all("600", "$600") %>%
  str_replace_all("300", "$300") %>%
  str_replace_all('<td style="text-align:left">hline</td><td></td><td></td><td></td><td></td>', 
                  "<td colspan='5' style='border-bottom: 1px solid black'></td>") %>%
  writeLines(str_c(path_out, "apc_table_iv_controls.html"))
