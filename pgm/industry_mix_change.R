# SPDX-License-Identifier: MIT
# industry_mix_change.R
# Author: Katie Zhang
# Date: 2022-01-22
# Purpose: run robustness checks on a number of controls. 
# Assess the quality of the industry variable tagging in JPMCI by
# comparing to an external benchmark (Department of Labor ETA form 203) which
# gives data on UI claims by industry

#INs
form_203_path <- # ETA form 203 from department of labor
  str_c(data_path, "scratch/2021-08-19claimant_industry.csv")

dfs_used_throughout_jobfind_plots <-
  # dataframe name + script where it was last generated in the current form
  tribble(~df_name, ~script_where_generated,
          "tmp_for_hazard_plot_expanded", "control_prep.R",
          "attributes_covariates", "control_prep.R")

#OUTS
last_date_run <- "2023-02-03"
compare_ind_comp_output <-
  c(str_c(path_out, last_date_run, "industry_prop_comparison.csv"),
    str_c(path_out, last_date_run, "industry_prop_comparison.tex"),
    str_c(path_out, last_date_run, "industry_prop_comparison.html"))

compare_industry_plot <- "scatter_d_ind_mix_symmetric"

################################################################################
# prepare data ----
# JPMCI data
jpmci_ind <-
  tmp_for_hazard_plot_expanded %>% # comes from pgm/control_prep.R, contains weeks of data for selected states in 2020/2021
  mutate(week_start_date = week_start_date + 7) %>% # indexing exits forward by a week since exit is week after last payment
  filter(spell_active) %>%
  left_join(attributes_covariates, # from pgm/control_prep.R
            by = c("cust_number", "ui_spell_number", "start_ui_date")) %>%
  filter(start_ui,
         inflow_type == "normal_ui") %>%
  transmute(week_start_date, cust_number, cust_state, ui_spell_number,
            start_ui, start_ui_date, exit_ui_date,
            at_counterparty_raw,
            jpmci_naics = naics_industry,
            # Note: using April cutoff instead of mid-March as DOL only has
            # monthly data, so this way the two timeframes can line up
            ind_period = case_when(week_start_date < as.Date("2020-04-01") ~ "Pre-COVID",
                                   week_start_date >= as.Date("2020-04-01") &
                                     week_start_date < as.Date("2021-01-01") ~ "Pandemic",
                                   TRUE ~ "2021")) %>%
  arrange(cust_number, week_start_date) %>%
  arrange(cust_number, ui_spell_number) %>%
  transmute(week_start_date, cust_number, cust_state,
            ui_spell_number, start_ui_date, ind_period, jpmci_naics)

# compute re-weighting values for DOL (JPMCI shares of UI recipients by state) 
state_period_weight <- jpmci_ind %>%
  filter(!is.na(jpmci_naics)) %>%
  group_by(ind_period, cust_state) %>%
  count() %>%
  group_by(ind_period) %>%
  transmute(state_abb = cust_state,
            st_period_wt = n / sum(n)) %>%
  ungroup()

# Department of Labor (DOL) data on Unemployment insurance claims by industry
claimants_industry <-
  read_csv(form_203_path) %>%
  rename(dol_naics = industry)

claimants_industry_period_summary <- claimants_industry %>%
  filter(year(month) >= 2019,
         !is.na(dol_naics)) %>%
  mutate(ind_period =
           case_when(month <= as.Date("2020-03-31") ~ "Pre-COVID",
                     month >= as.Date("2020-04-01") & month < as.Date("2021-01-01") ~ "Pandemic",
                     TRUE ~ "2021")) %>%
  inner_join(state_period_weight,
             by = c("state_abb", "ind_period")) %>% 
  # re-weighted claims, not real n, as we only care about proportion
  mutate(claims = st_period_wt * claims) %>%
  group_by(ind_period, dol_naics) %>%
  summarise(claims = sum(claims)) %>%
  group_by(ind_period) %>%
  mutate(total_claims = sum(claims),
         prop_dol = claims / total_claims)

# Crosswalk names
dol_naics <- unique(sort(claimants_industry$dol_naics))

jpmci_naics <- jpmci_ind %>%
  distinct(jpmci_naics) %>%
  arrange(jpmci_naics) %>%
  filter(!is.na(jpmci_naics)) %>%
  pull()

naics_match <- bind_cols("jpmci_naics" = jpmci_naics,
                         "dol_naics" = dol_naics)

naics_short_name <-
  c("Accommodation\nand Food Services",
    "Administrative\nand Support Services",
    "Agriculture, Forestry,\nFishing and Hunting",
    "Arts, Entertainment,\nand Recreation",
    "Construction",
    "Educational Services",
    "Finance and Insurance",
    "Health Care and\nSocial Assistance",
    "Information",
    "Management of\nCompanies",
    "Manufacturing",
    "Mining, Quarrying,\nExtraction",
    "Other Services",
    "Professional, Scientific,\nand Technical Services",
    "Public Administration",
    "Real Estate",
    "Retail Trade",
    "Transportation and\nWarehousing",
    "Utilities",
    "Wholesale Trade"
  )

naics_match$short_name <- naics_short_name

# compare JPMCI and DOL proportion of UI claims categorised under each NAICS industry ----
compare_ind_comp <-
  jpmci_ind %>%
  filter(!is.na(jpmci_naics)) %>%
  group_by(ind_period) %>%
  count(jpmci_naics) %>%
  mutate(total_n = sum(n),
         prop_jpmci = n / total_n) %>%
  ungroup() %>%
  left_join(naics_match, by = "jpmci_naics") %>%
  left_join(claimants_industry_period_summary, by = c("ind_period", "dol_naics")) %>%
  transmute(ind_period,
            naics = jpmci_naics,
            short_name,
            prop_jpmci, prop_dol,
            jpmci_dol_ratio = prop_jpmci / prop_dol)

# comparing between JPMCI and DOL data the change in industry mix as measured by
# symmetric pct change in the % of total UI claims belonging to each industry
d_prop <- compare_ind_comp %>%
  pivot_longer(starts_with("prop_"),
               names_to = "source",
               names_prefix = "prop_",
               values_to = "prop") %>%
  filter(ind_period != "2021") %>%
  select(-jpmci_dol_ratio) %>%
  pivot_wider(names_from = c(source, ind_period),
              values_from = prop) %>%
  rename(jpmci_pandemic = jpmci_Pandemic,
         jpmci_precovid = `jpmci_Pre-COVID`,
         dol_pandemic = dol_Pandemic,
         dol_precovid = `dol_Pre-COVID`) %>% # no janitor package
  mutate(jpmci_symmetric = 2 * ((jpmci_pandemic - jpmci_precovid) / (jpmci_pandemic + jpmci_precovid)),
         dol_symmetric = 2 * ((dol_pandemic - dol_precovid) / (dol_pandemic + dol_precovid)))

scatter_d_ind_mix_symmetric <- d_prop %>%
  select(short_name, contains("symmetric")) %>%
  ggplot() +
  aes(x = dol_symmetric, y = jpmci_symmetric, label = short_name) +
  geom_point(colour = navyblue) +
  geom_text_repel(size = 2.5) +
  geom_abline(slope = 1) +
  fte_theme() +
  labs(x = "Symmetric percentage change in industry mix (DOL)",
       y = "Symmetric percentage change in industry mix (JPMCI)") +
  theme(axis.title=element_text(size=9),
        axis.text=element_text(size=11))

