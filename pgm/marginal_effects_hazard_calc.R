# SPDX-License-Identifier: MIT
# Hazard Elasticity Calculations
# Author: Rupsha Debnath, Joe Vavra

# Set up:
# Run control_prep.R, rep_rate_prep.R and rep_rate_tables.R prior to running this script

# INS
# reg_xsec_expire, reg_xsec_onset (Produced in rep_rate_tables.R)
# all_states_pandemic, all_states_december (Produced in rep_rate_prep.R)

# OUT
marginal_effects_output <- "marginal_effects_hazard_calc_inputs.csv"

# Baseline Absolute PP: Regression Marginal Effects ----
# This is pulling just the coefficients from the output `table_main_coefficients`

baseline_pp_600_expire <- reg_xsec_expire$coefficients["PctChange:SuppAvailTRUE",]
baseline_pp_300_onset <- reg_xsec_onset$coefficients["PctChange:SuppAvailTRUE",]

# Logit Marginal Effects ----

logit_did <- function(df, episode = "600 expiry"){
  # Runs a logit regression and calculates the marginal effects and the SEs
  reg_df_setup <- df %>% 
    filter(exit_labor == 1) %>%
    mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
    mutate(PctChange = per_change)
  
  if(episode == "600 expiry"){
    reg_df <- mutate(reg_df_setup, SuppAvail = !post)
  } else {
    reg_df <- mutate(reg_df_setup, SuppAvail = post)
  }
  
  logit_model <- glm(exit_ui ~ PctChange * SuppAvail, 
                     reg_df,
                     family = binomial(logit))
  
  cluster_vcov <- vcovHC(logit_model, type = "HC1", cluster = "cust_number")
  
  model_estimate <- logit_model %>%
    tidy(., conf.int = TRUE) %>%
    column_to_rownames(var="term") %>%
    select("estimate") %>%
    t() %>%
    as.data.frame()
  
  # p_suppavail_low, p_suppavail_high, p_nosuppavail_low, p_nosuppavail_high
  low <- mean(reg_df$PctChange)
  high <- mean(reg_df$PctChange) + 0.0001
  
  # functions --
  logit_exponent <- function(input){exp(input)/(1+exp(input))}
  
  var_log_exp <- function(prob, input){prob/(1+exp(input))}
  
  var_logit_input_suppavail <- function(pctchange){
    (cluster_vcov["(Intercept)", "(Intercept)"] + cluster_vcov["SuppAvailTRUE", "SuppAvailTRUE"] +
       2*cluster_vcov["(Intercept)","SuppAvailTRUE"]) + 
      (cluster_vcov["PctChange", "PctChange"] + cluster_vcov["PctChange:SuppAvailTRUE", "PctChange:SuppAvailTRUE"] +
         2*cluster_vcov["PctChange","PctChange:SuppAvailTRUE"])*(pctchange^2) +
      2*(pctchange)*(cluster_vcov["(Intercept)","PctChange"] + cluster_vcov["(Intercept)","PctChange:SuppAvailTRUE"] +
                       cluster_vcov["SuppAvailTRUE","PctChange"] + 
                       cluster_vcov["SuppAvailTRUE","PctChange:SuppAvailTRUE"])
  }
  
  var_logit_input_nosupp <- function(pctchange){
    cluster_vcov["(Intercept)", "(Intercept)"] + 
      cluster_vcov["PctChange:SuppAvailTRUE", "PctChange:SuppAvailTRUE"]*(pctchange^2) +
      2*cluster_vcov["(Intercept)","PctChange:SuppAvailTRUE"]*(pctchange)
  }
  
  # Marginal effects:
  model_prob <- model_estimate %>% 
    mutate(# suppavail low
      p_suppavail_low = logit_exponent(`(Intercept)` + `PctChange`*low + 
                                         SuppAvailTRUE + `PctChange:SuppAvailTRUE`*low),
      var_p_suppavail_low = var_log_exp(p_suppavail_low, `(Intercept)` + `PctChange`*low + 
                                          SuppAvailTRUE + `PctChange:SuppAvailTRUE`*low),
      se_p_suppavail_low = sqrt((var_p_suppavail_low^2)*var_logit_input_suppavail(low)),
      # suppavail high
      p_suppavail_high = logit_exponent(`(Intercept)` + `PctChange`*high + 
                                          SuppAvailTRUE + `PctChange:SuppAvailTRUE`*high),
      var_p_suppavail_high = var_log_exp(p_suppavail_high, `(Intercept)` + `PctChange`*high + 
                                           SuppAvailTRUE + `PctChange:SuppAvailTRUE`*high),
      se_p_suppavail_high = sqrt((var_p_suppavail_high^2)*var_logit_input_suppavail(high)),
      # no suppavail low
      p_no_suppavail_low = logit_exponent(`(Intercept)` + `PctChange`*low),
      var_p_no_suppavail_low = var_log_exp(p_no_suppavail_low, `(Intercept)` + `PctChange`*low),
      se_p_no_suppavail_low = sqrt((var_p_no_suppavail_low^2)*var_logit_input_nosupp(low)),
      # no suppavail high
      p_no_suppavail_high = logit_exponent(`(Intercept)` + `PctChange`*high),
      var_p_no_suppavail_high = var_log_exp(p_no_suppavail_high, `(Intercept)` + `PctChange`*high),
      se_p_no_suppavail_high = sqrt((var_p_no_suppavail_high^2)*var_logit_input_nosupp(high)),
      # did
      did_p = ((p_suppavail_high - p_suppavail_low) - (p_no_suppavail_high - p_no_suppavail_low))/0.0001,
      se_did_supp = sqrt((se_p_suppavail_high)^2 + (se_p_suppavail_low)^2),
      se_did_nosupp = sqrt((se_p_no_suppavail_high)^2 + (se_p_no_suppavail_low)^2),
      se_did = sqrt((se_did_supp)^2 + (se_did_nosupp)^2)) %>%
    select(did_p, se_did)
  return(model_prob)
}

did_600 <- logit_did(all_states_pandemic, episode = "600 expiry")

did_300 <- logit_did(all_states_december, episode = "300 onset")

# Hazard Elasticity for binned data ----

## 600 expiration:
# exit to not recall
expiry_new_job_data_pre_policy <- all_states_pandemic %>% 
  filter(exit_labor == 1)  %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  group_by(vin_all, post) %>%
  summarise(estimate = mean(exit_ui),
            per_change = mean(per_change)) %>%
  mutate(group=ifelse(post==FALSE, "pre", "post")) %>%
  ungroup() %>%
  mutate(per_change = -per_change)

# total exit
expiry_total_exit_pre_policy <- all_states_pandemic %>% 
  filter(exit_labor == 1)  %>%
  group_by(vin_all, post) %>%
  summarise(estimate = mean(exit_ui),
            per_change = mean(per_change)) %>%
  mutate(group=ifelse(post==FALSE, "pre", "post")) %>%
  ungroup()%>%
  mutate(per_change = -per_change)

df_600_expiry <- rbind(mutate(expiry_new_job_data_pre_policy, exit_rate_type = "Exit not to Recall"),
                        mutate(expiry_total_exit_pre_policy, exit_rate_type = "Total Exit"))

## 300 onset:
# exit to not recall
onset_new_job_data_pre_policy <- all_states_december %>% 
  filter(exit_labor == 1)  %>%
  mutate(exit_ui = (exit_ui == 1 & start_recall == 0)) %>%
  group_by(vin_all, post) %>%
  summarise(estimate = mean(exit_ui),
            per_change = mean(per_change)) %>%
  mutate(group=ifelse(post==FALSE, "pre", "post")) %>%
  ungroup() %>%
  mutate(per_change = -per_change)

# total exit
onset_total_exit_pre_policy <- all_states_december %>% 
  filter(exit_labor == 1)  %>%
  group_by(vin_all, post) %>%
  summarise(estimate = mean(exit_ui),
            per_change = mean(per_change)) %>%
  mutate(group=ifelse(post==FALSE, "pre", "post")) %>%
  ungroup()%>%
  mutate(per_change = -per_change)

df_300_onset <- rbind(mutate(onset_new_job_data_pre_policy, exit_rate_type = "Exit not to Recall"),
                       mutate(onset_total_exit_pre_policy, exit_rate_type = "Total Exit"))

## Calculate the hazard elasticity:
hazard_elasticity <- function(binned_df, source_df, benefit_amt) {
  df_raw <- 
    binned_df %>%
    rename(benefit_spc = per_change) %>%
    select(-post) 
  
  if(benefit_amt==600){recall_rate_setup <- df_raw %>% filter(group == "post")
  } else {recall_rate_setup <- df_raw %>% filter(group == "pre")}
  
  recall_rate <- 
    recall_rate_setup %>% 
    group_by(exit_rate_type, vin_all) %>% 
    summarise(est = mean(estimate)) %>% 
    pivot_wider(names_from = exit_rate_type, values_from = est) %>% 
    mutate(recall_rate = `Total Exit` - `Exit not to Recall`) %>%
    select(-`Total Exit`, -`Exit not to Recall`)
  
  df <- 
    df_raw %>% 
    filter(exit_rate_type == "Exit not to Recall") %>%
    group_by(vin_all) %>%
    mutate(benefit_spc = mean(benefit_spc)) %>%
    pivot_wider(
      names_from = group, 
      values_from = estimate
    ) %>%
    inner_join(., recall_rate, by="vin_all") %>%
    mutate(
      absolute_change =        post - pre,
      hazard_ratio_w_recall = (post + recall_rate)/(pre + recall_rate)
    )
  
  temp_df <<- df
  
  hazard_spc <- lm("hazard_ratio_w_recall ~ benefit_spc", data = df) %>%
    broom::tidy() %>%
    filter(term == "benefit_spc") %>%
    pull(estimate)
  
  ratio_per_change <- source_df %>% 
    filter(exit_labor == 1) %>%
    summarise(mean_symm_per_change=mean(per_change), 
              mean_nonsymm_per_change=mean(benefit_amt/no_sup_stat_benefits)) %>%
    mutate(ratio=mean_symm_per_change/mean_nonsymm_per_change)%>%
    pull(ratio)
  
  perc_change_hazard <- hazard_spc*ratio_per_change
  output <- data.frame(perc_change_haz_elas = perc_change_hazard,
                       spc_haz_elas = hazard_spc)
  return(output)
}

hazard_elas_600_expiry <- hazard_elasticity(df_600_expiry, all_states_pandemic, 600)
hazard_elas_300_onset <- hazard_elasticity(df_300_onset, all_states_december, 300)

# Output dataframe ----

output_hazard <- data.frame(variable = c("Baseline Abs pp Marginal Effect",
                                "Logit Marginal Effect",
                                "Hazard Perc Change Binned Means"),
                            expiry_600 = c(baseline_pp_600_expire,
                                  did_600$did_p,
                                  hazard_elas_600_expiry$perc_change_haz_elas),
                            onset_300 = c(baseline_pp_300_onset,
                                 did_300$did_p, 
                                 hazard_elas_300_onset$perc_change_haz_elas))
write.csv(output_hazard, str_c(path_out, marginal_effects_output))
