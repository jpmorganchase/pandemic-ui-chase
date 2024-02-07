# Low Pre-Pandemic characteristics
# Author: Rupsha Debnath
# Date: 24 October 2023

# Set up -- 
# Run `spend_build.R` and `spend_by_liquidity_buffer.R` prior to this 

df_demo_src_wins_touse_eipwks_with_buffer <- 
  df_demo_src_wins_touse_eipwks %>%
  left_join(customer_buffer_quantiles_2018 %>%
              select(cust_number, med_buffer_quantile), 
            by = "cust_number") %>%
  # The next line ensures that the employed group doesn't have any NAs
  filter(!is.na(med_buffer_quantile)) %>%
  mutate(low_2018_liq = ifelse(med_buffer_quantile==1, 1, 0))

# regressions:
if(!exists("df_eips")) {
  rds_eips <- paste0(data_path, 'eips/', '2022-03-08eips_list_100pct.rds')
  df_eips <- read_rds(rds_eips) %>%
    mutate(eip_amt = as.numeric(eip_amt),
           eip_date = as.Date(eip_date)) %>%
    as_tibble()
}

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

df_eip_cleaned <- df_demo_src_wins_touse_eipwks_with_buffer %>%
  mutate(male = ifelse(gender == "M", 1, ifelse(gender == "F", 0, NA))) %>%
  left_join(., eips_status, by=c("cust_number", "eip_date", "eip_amt", "method")) %>%
  mutate(eip_dummy = ifelse(is.na(eip_date),0,1))

df_demeaned_eip <- df_eip_cleaned %>%
  mutate(inflows_ex_transfers_demeaned = inflows_ex_transfers - mean(inflows_ex_transfers),
         age_demeaned = age - mean(age, na.rm=TRUE),
         age_demeaned = ifelse(is.nan(age_demeaned), NA, age_demeaned),
         single_dummy = ifelse(filing_eip=="single",1,ifelse(filing_eip=="married", 0, NA))) %>%
  filter(eip_dummy==1) # Subset the df_demeaned to eip recipients

# full reg 
reg_all_eip_demeaned <- lm(low_2018_liq ~ inflows_ex_transfers_demeaned + age_demeaned + male + nkids + factor(filing_eip),
                          data=df_demeaned_eip)
summary(reg_all_eip_demeaned)
stargazer(reg_all_eip_demeaned, out = (str_c(path_out, "reg_all_eip_demeaned.html")),
          dep.var.labels=c("Low Liquidity in 2018"),
          covariate.labels=c("Income (Demeaned)","Age (Demeaned)",
                             "Share male","Number of Kids",
                             "Share single"),
          align=TRUE)
stargazer(reg_all_eip_demeaned, type="latex", out = str_c(path_out, "reg_all_eip_demeaned.tex"),
          dep.var.labels=c("Low Liquidity in 2018"),
          covariate.labels=c("Income (Demeaned)","Age (Demeaned)",
                             "Share male","Number of Kids",
                             "Share single"),
          align=TRUE)

#### Table of means ####

means_df <- df_demeaned_eip %>%
  group_by(low_2018_liq) %>%
  summarise(mean_income = mean(inflows_ex_transfers, na.rm = TRUE),
            mean_age = mean(age, na.rm = TRUE),
            mean_male = mean(male, na.rm = TRUE),
            mean_eip_dummy = mean(eip_dummy, na.rm = TRUE),
            mean_nkids = mean(nkids, na.rm = TRUE),
            mean_single_dummy = mean(single_dummy, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("mean_"), values_to="value", names_to = "variable") %>%
  mutate(low_2018_liq = ifelse(low_2018_liq==0, "high", "low")) %>%
  pivot_wider(names_from = low_2018_liq, values_from = value, names_prefix = "mean_") %>%
  mutate(variable = str_remove(variable,"mean_"),
         difference_means = mean_low - mean_high)

sd_df <- df_demeaned_eip %>%
  group_by(low_2018_liq) %>%
  summarise(sd_income = sd(inflows_ex_transfers, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            sd_male = sd(male, na.rm = TRUE),
            sd_eip_dummy = sd(eip_dummy, na.rm = TRUE),
            sd_nkids = sd(nkids, na.rm = TRUE),
            sd_single_dummy = sd(single_dummy, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("sd_"), values_to="value", names_to = "variable") %>%
  mutate(low_2018_liq = ifelse(low_2018_liq==0, "high", "low")) %>%
  pivot_wider(names_from = low_2018_liq, values_from = value, names_prefix = "sd_") %>%
  mutate(variable = str_remove(variable,"sd_"))

n_df <- df_demeaned_eip %>%
  group_by(low_2018_liq) %>%
  summarise(n_income = sum(!is.na(inflows_ex_transfers)),
            n_age = sum(!is.na(age)),
            n_male = sum(!is.na(male)),
            n_eip_dummy = sum(!is.na(eip_dummy)),
            n_nkids = sum(!is.na(nkids)),
            n_single_dummy = sum(!is.na(single_dummy))
  ) %>%
  pivot_longer(cols = starts_with("n_"), values_to="value", names_to = "variable") %>%
  mutate(low_2018_liq = ifelse(low_2018_liq==0, "high", "low")) %>%
  pivot_wider(names_from = low_2018_liq, values_from = value, names_prefix = "n_")%>%
  mutate(variable = str_remove(variable,"n_"))

n_low_df <- df_demeaned_eip %>%
  filter(low_2018_liq==1) %>%
  summarise(n_income = length(unique(.$cust_number[!is.na(.$inflows_ex_transfers)])),
            n_age = length(unique(.$cust_number[!is.na(.$age)])),
            n_male = length(unique(.$cust_number[!is.na(.$male)])),
            n_eip_dummy = length(unique(.$cust_number[!is.na(.$eip_dummy)])),
            n_nkids = length(unique(.$cust_number[!is.na(.$nkids)])),
            n_single_dummy = length(unique(.$cust_number[!is.na(.$single_dummy)]))) %>%
  pivot_longer(cols = starts_with("n_"), values_to="n_low", names_to = "variable") %>%
  mutate(variable = str_remove(variable,"n_"))

n_high_df <- df_demeaned_eip %>%
  filter(low_2018_liq==0) %>%
  summarise(n_income = length(unique(.$cust_number[!is.na(.$inflows_ex_transfers)])),
            n_age = length(unique(.$cust_number[!is.na(.$age)])),
            n_male = length(unique(.$cust_number[!is.na(.$male)])),
            n_eip_dummy = length(unique(.$cust_number[!is.na(.$eip_dummy)])),
            n_nkids = length(unique(.$cust_number[!is.na(.$nkids)])),
            n_single_dummy = length(unique(.$cust_number[!is.na(.$single_dummy)]))) %>%
  pivot_longer(cols = starts_with("n_"), values_to="n_high", names_to = "variable") %>%
  mutate(variable = str_remove(variable,"n_"))

full_df_for_p_val <- means_df %>%
  inner_join(., sd_df, by="variable") %>%
  inner_join(., n_low_df, by="variable") %>%
  inner_join(., n_high_df, by="variable")

p_df <- full_df_for_p_val %>%
  mutate(t_val = ((mean_low - mean_high)/sqrt((sd_high^2/n_high)+(sd_low^2/n_low))),
         deg_f = (((sd_high^2/n_high)+(sd_low^2/n_low))^2/((sd_high^2/n_high)^2/(n_high - 1) + (sd_low^2/n_low)^2/(n_low - 1))),
         p_value = (2*pt(-abs(t_val), deg_f))) %>%
  filter(variable != "eip_dummy") %>%
  select(variable, mean_low, mean_high,
         difference_means, p_value)%>% 
  mutate_if(is.numeric, round, digits=2) %>%
  mutate(mean_high = ifelse(variable=="income", as.integer(mean_high), round(mean_high,2)),
         mean_low = ifelse(variable=="income", as.integer(mean_low), round(mean_low,2)),
         difference_means = round((mean_low - mean_high), 2),
         variable = c("Income", "Age", "Share male", "Number of Kids", "Share single"))

colnames(p_df) <- c("Variable", "Low Liquidity", "High Liquidity", 
                     "Difference", "P-value")

write.csv(p_df, str_c(path_out, "table_means_liq.csv"))

mean_table_liq <- p_df %>%
  mutate(`P-value`="< 0.01") %>%
  mutate(`Low Liquidity` = ifelse(Variable == "Income", paste0("$", number(`Low Liquidity`, big.mark = ",")), 
                                  `Low Liquidity`),
         `High Liquidity` = ifelse(Variable == "Income", paste0("$", number(`High Liquidity`, big.mark = ",")), 
                                  `High Liquidity`),
         Difference = ifelse(Variable == "Income", paste0("$", number(Difference, big.mark = ",")), 
                             Difference))

mean_table_liq_edited <- mean_table_liq  %>%
  stargazer(., digits=2,summary=FALSE, type="latex", rownames = FALSE) %>%
  str_subset("\\{table\\}|caption|label", negate = TRUE) %>%
  str_replace("ccccc", "lcccc") %>%
  str_replace(fixed("\\$-"), "-\\$") %>%
  str_replace_all("hline", c("toprule","toprule", "midrule", "bottomrule")) %>%
  writeLines(str_c(path_out, "means_table_w_liq.tex"))

mean_table_liq_html <- capture.output(print(xtable(mean_table_liq), type="html", include.rownames=FALSE,
                                            floating=FALSE, booktabs=TRUE)) %>%
  str_replace_all(fixed("$-"), "-$") %>%
  writeLines(str_c(path_out, "means_table_w_liq.html"))

# Aggregation standards

agg_std <- data.frame("Item" = rep("Table", 2),
                      "Title" = c("Regression", "Table of Means"),
                      "Output file" = c("reg_all_eip_demeaned",
                                        "means_table_w_liq") ,
                      "Min Cell Size" = c(nobs(reg_all_eip_demeaned),
                                          min(c(n_df$n_low, n_df$n_low))), 
                      "Code File" = rep("pgm/low_prepand_liq.R",2))
write.csv(agg_std, str_c(path_out, "agg_std_low_prepand_liq.csv"))
