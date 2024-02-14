# SPDX-License-Identifier: MIT
# INS
in_dfs <-
  c("df_demo_src_wins", 
    "cust_eip_touse", 
    "cust_eip_rounds_touse", 
    "cust_ui_onset", 
    "cust_thru_aug", 
    "cust_thru_jan",
    "cust_ui_thru_sept_2021",
    "cust_ui_thru_jul_2021", 
    "state_treatment_control_lwa",
    "cust_nonui",
    "cust_thru_feb_first_weeks", #list of customers in Figure 1
    "df_2018") #2018 assets & spending

# Check all the input dfs are 
test_that("all input dfs are present", 
          expect_equal(all(in_dfs %>% map_lgl(exists)), TRUE))

#OUTS
out_dfs_ggs <- 
  c("plot_spend_by_liquidity",
    "df_monthly_collapsed_high_liquidity_for_plot",
    "df_monthly_collapsed_low_liquidity_for_plot",
    "medians_by_buffer_quantiles",
    "table_apcs_by_liquidity")

############################################################
# define liquidity cutoffs and classify accounts by 2018 liquidity
############################################################

# Need to add in 2018 demog data for 2018 buffer calculation
rds_demog_list_2018 <- paste0(data_path, "demog/",
                              list.files(path = paste0(data_path, "demog/"),
                                         pattern = "rupsha_"))
vars_to_numeric <- vars(periodid, total_inflows:total_ui_inflows, total_liquid_balances, checking_acct_balance, labor_inflows_2019)

df_2018_read_in <- rds_demog_list_2018 %>% map_dfr(~ read_rds(.)) %>%
  mutate_at(
    vars_to_numeric,
    as.numeric
  )

date_scratch_read_in <- "2023-06-14"
cust_to_include <- readRDS(str_c(data_path, "tmp/read_in/", 
                                 date_scratch_read_in, "cust_to_include.rds"))

df_2018 <- df_2018_read_in %>%
  inner_join(cust_to_include %>% select(cust_number))

# buffers relative to cutoffs are defined for everyone
customer_buffer_quantiles_2018 <-
  df_2018 %>%
  mutate(
    income = total_inflows - transfer_inflows, 
    buffer_income = checking_acct_balance / income, 
    buffer_spend = (checking_acct_balance - 0.5 * total_spend_expanded) / total_spend_expanded
  ) %>%
  filter(income > 50, total_spend_expanded > 50) %>%
  # drop customer-periodid duplicates 
  #(possible because a customer can have multiple types, eg, '202021_ui_recipient' and '2019_nonui')
  distinct(cust_number, periodid, .keep_all = TRUE) %>%
  group_by(cust_number, cust_type) %>% #this code block is slow to run
  summarise(
    med_buffer_spend_2018 = median(buffer_spend),
    #all three of these are only used for summary statistics later on
    med_buffer_income_2018 = median(buffer_income), 
    med_spend_2018 = median(total_spend_expanded), 
    med_income_2018 = median(income)
  ) %>%
  ungroup()

buffer_cutoff_for_unemployed_2018 <-
  customer_buffer_quantiles_2018 %>%
  filter(cust_type == "202021_ui_recipient") %>%
  summarise(buffer_spend_cutoff50 = median(med_buffer_spend_2018)) %>%
  pull()
test_that("buffer amt is", expect_equal(buffer_cutoff_for_unemployed_2018, -0.145775))

customer_buffer_quantiles_2018 <- 
  customer_buffer_quantiles_2018 %>%
  mutate(med_buffer_quantile = ifelse(med_buffer_spend_2018 <= buffer_cutoff_for_unemployed_2018, 1, 2))

medians_by_buffer_quantiles <- 
  customer_buffer_quantiles_2018 %>%
  group_by(med_buffer_quantile) %>%
  summarise(median_buffer_income_2018_by_quantile = median(med_buffer_income_2018), 
            median_buffer_spend_2018_by_quantile = median(med_buffer_spend_2018), 
            median_income_2018_by_quantile = median(med_income_2018), 
            median_spend_2018_by_quantile = median(med_spend_2018))

############################################################
# Perform calculations for plots
############################################################
# this is a smaller subset of households who become unemployed right around Apr 2020 which corresponds just to those for the time-series plot in Figure 1
test_that(
  "we observe 2018 liquidity for everyone in Figure 1",
  expect_equal(
    cust_thru_feb_first_weeks %>% 
      anti_join(customer_buffer_quantiles_2018, by = "cust_number") %>%
      nrow(),
    0
  )
)

cust_thru_feb_first_weeks_with_buffer <- 
  cust_thru_feb_first_weeks %>%
  left_join(
    customer_buffer_quantiles_2018 %>%
      select(cust_number, med_buffer_quantile), 
    by = "cust_number"
)

test_that(
  "no one in monthly file is missing a 2018 liquidity observation",
  expect_equal(
    df_demo_src_wins_touse_eipwks %>% 
      anti_join(customer_buffer_quantiles_2018, by = "cust_number") %>%
      nrow(),
    0
  )
)

df_demo_src_wins_touse_eipwks_with_buffer <- 
  df_demo_src_wins_touse_eipwks %>%
  left_join(customer_buffer_quantiles_2018 %>%
              select(cust_number, med_buffer_quantile), 
            by = "cust_number") %>%
  # The next line ensures that the employed group doesn't have any NAs
  filter(!is.na(med_buffer_quantile))

# these weights will re-weight the two liquidity groups to match the unconditional distribution of income and EIP weeks
# Note that depending on how groups are liquidity groups are defined, these re-weights don't need to add to average 1. This is not an issue because the average
# value of re-weight across the different observables groups within a liquidity group is irrelevant, the relative weights within a group are all that matters
reweights_to_use <-
  df_demo_src_wins_touse_eipwks_with_buffer %>%
  # get a customer-by-inc quintile-by-eip date df
  distinct(cust_number, inc_2019_quintile, eip_week, med_buffer_quantile) %>%
  count(inc_2019_quintile, eip_week, med_buffer_quantile) %>%
  mutate(med_buffer_quantile = ifelse(med_buffer_quantile == 1, "low_liquidity", "high_liquidity")) %>%
  pivot_wider(names_from = "med_buffer_quantile", values_from = "n") %>% 
  mutate(
    re_weight_high = (low_liquidity + high_liquidity) / high_liquidity,
    re_weight_low = (low_liquidity + high_liquidity) / low_liquidity
  ) %>%
  replace_na(list(re_weight_high = 0, re_weight_low = 0)) %>%
  select(-low_liquidity, -high_liquidity)

reweights_to_use_for_liquidity <-
  reweights_to_use %>%
  mutate(inc_2019_quintile_index = as.numeric(inc_2019_quintile)) %>%
  select(-inc_2019_quintile)

# note, this function is nearly identical to what is in spend build, but is adding a buffer to the match and then also needs to create a data set to merge
# which contains that variable (df_demo_src_wins_touse_eipwks_with_buffer)
# The alternative would be to pass both an unemp and emp cust_list or make a switch for what emp group is merged, so that we are only comparing unemp and emp
# households in the same liquidity quartile, but would then need to modify the original spend build code to be able to handle this
make_employed_vs_unemployed_collapsed_df_weighted_with_buffer_match <-
  function(unemp_cust_list,
             high_liquidity_indicator,
             matching_vars = c("inc_2019_quintile", "eip_week", "med_buffer_quantile"),
             ...) {

    ## step 1: get list of customers to include in analysis
    combined_cust_list <-
      bind_rows(
        cust_nonui,
        unemp_cust_list %>%
          mutate(group = "Unemployed") %>%
          distinct(cust_number, group)
      ) %>%
      mutate(group = ifelse(group == "Employed", "control", "treatment"))

    ## step 2: compute inc quintile-by-eip date weights: we want to weight the control group
    ##        so that it matches the treatment group in terms of (inc quintile-eip date).
    ##        This function uses tidy evaluation (...) to be flexible around the matching variables
    weights_to_use <-
      df_demo_src_wins_touse_eipwks_with_buffer %>%
      # get a customer-by-inc quintile-by-eip date df
      distinct(cust_number, ...) %>%
      inner_join(combined_cust_list, by = "cust_number") %>%
      group_by(..., group) %>%
      summarise(ct = n()) %>%
      ungroup() %>%
      pivot_wider(names_from = "group", values_from = "ct") %>%
      mutate(weight = treatment / control) %>%
      replace_na(list(
        control = 0,
        treatment = 0,
        weight = 0
      ))

    # weights match U and E groups (i.e. treatment and control)  the same on observables
    # re-weights then multiply weights to make the groups by liquidity match the overall population
    weights_to_use <- weights_to_use %>%
      select(-treatment, -control) %>% # deselect unnecessary colns
      inner_join(reweights_to_use, by = c("inc_2019_quintile", "eip_week")) %>%
      mutate(re_weight = ifelse(med_buffer_quantile == 1, re_weight_low, re_weight_high)) %>%
      mutate(weight_re_weight = weight * re_weight)


    ## step 3: get cust-by-month df, with weights
    df_to_collapse <-
      df_demo_src_wins_touse_eipwks_with_buffer %>%
      dplyr::filter(periodid >= 201901) %>%
      inner_join(combined_cust_list, by = "cust_number") %>%
      # do a distinct -- there are a few duplicates because customers can have multiple cust_type
      distinct(cust_number, periodid, .keep_all = TRUE) %>%
      left_join(weights_to_use, by = matching_vars) %>%
      mutate(weight = ifelse(group == "treatment", re_weight, weight_re_weight)) %>%
      transmute(cust_number, cust_state, group, periodid, weight,
        total_outflows,
        total_spend_cardcash,
        total_spend_expanded,
        income = total_inflows - transfer_inflows,
        total_inflows,
        transfer_inflows,
        labor_inflows,
        tax_refund_inflows,
        outflows_ex_transfers,
        chk_bal = checking_acct_balance
      ) %>%
      pivot_longer(-one_of(c("cust_number", "cust_state", "group", "periodid", "weight")),
        names_to = "category"
      )

    ## step 4: collapse to group-by-periodid level
    df_collapsed <-
      df_to_collapse %>%
      group_by(group, periodid, category) %>%
      summarise(
        mean = weighted.mean(value, weight),
        median = xtile_ten(value, weight),
        ct_cust = n()
      ) %>%
      ungroup() %>%
      pivot_longer(
        cols = c("mean", "median"),
        names_to = "measure",
        values_to = "value"
      ) %>%
      mutate(group = ifelse(group == "treatment", "unemployed", "employed")) %>%
      select(periodid, category, group, measure, value, ct_cust) %>%
      arrange(periodid, category, group, measure)

    test_that(
      "N rows (should be 2 groups x 36 periods x 10 names x 2 measures = 1440)",
      expect_equal(nrow(df_collapsed), 1440)
    ) # note that there's an extra period

    return(df_collapsed)
  }


# slow code to compute spending by liquidity ----
df_monthly_collapsed_low_liquidity <-
  cust_thru_feb_first_weeks_with_buffer %>%
  filter(med_buffer_quantile == 1) %>%
  make_employed_vs_unemployed_collapsed_df_weighted_with_buffer_match(.,
    high_liquidity = 0,
    matching_vars = c(
      "inc_2019_quintile",
      "eip_week",
      "med_buffer_quantile"
    ),
    inc_2019_quintile,
    eip_week,
    med_buffer_quantile
  ) %>%
  mutate(date = ymd(str_c(periodid, "01")))


df_monthly_collapsed_high_liquidity <-
  cust_thru_feb_first_weeks_with_buffer %>%
  filter(med_buffer_quantile == 2) %>%
  make_employed_vs_unemployed_collapsed_df_weighted_with_buffer_match(.,
    high_liquidity = 1,
    matching_vars = c(
      "inc_2019_quintile",
      "eip_week",
      "med_buffer_quantile"
    ),
    inc_2019_quintile,
    eip_week,
    med_buffer_quantile
  ) %>%
  mutate(date = ymd(str_c(periodid, "01")))

########################################################################
# Plots with spend, income and checking in % change as well as just spend
########################################################################

greys <- RColorBrewer::brewer.pal(9, "Greys")
source(str_c(path_repo, "pgm/funcs/ui_functions.R"))

# OUTS
pdf_out_spendplots <- str_c(path_repo, "issue_373_recall_spend/ui_spend.pdf")


# FACT 1 plot: spending, inflows relative to Jan 2020 ----
colors_fact1 <- c("Employed" = "#ffae5f", 
                  "Unemployed (get benefits from April 2020 through February 2021)" = "#004577")
shapes_fact1 <- c("Employed" = 19, 
                  "Unemployed (get benefits from April 2020 through February 2021)" = 15)

### arrange data
df_monthly_collapsed_low_liquidity_for_plot <- 
  df_monthly_collapsed_low_liquidity %>%
  dplyr::filter(category %in% c(
    "total_spend_cardcash", "income",
    "total_spend_expanded", "chk_bal"
  )) %>%
  mutate(date = ymd(periodid, truncated = 1)) %>%
  left_join(df_monthly_collapsed_low_liquidity %>%
    dplyr::filter(periodid == 202001) %>%
    transmute(group,
      category,
      measure,
      value_jan2020 = value
    ),
  by = c("group", "category", "measure")
  ) %>%
  mutate(
    percent_change = value / value_jan2020 - 1,
    category = ifelse(category == "total_spend_cardcash", "Spending (card and cash)",
      ifelse(category == "income", "Income",
        ifelse(category == "total_spend_expanded", "Spending (total)", "Checking account balance")
      )
    ),
    group = ifelse(group == "unemployed", "Unemployed (get benefits from April 2020 through February 2021)", "Employed")
  ) %>%
  mutate(
    name = ordered(category, levels = c("Income", "Spending (total)", "Spending (card and cash)", "Checking account balance")),
    group = ordered(group, levels = c("Unemployed (get benefits from April 2020 through February 2021)", "Employed")),
    liquidity = "Low 2018 Liquidity"
  )

df_monthly_collapsed_high_liquidity_for_plot <- 
  df_monthly_collapsed_high_liquidity %>%
  dplyr::filter(category %in% c(
    "total_spend_cardcash", "income",
    "total_spend_expanded", "chk_bal"
  )) %>%
  mutate(date = ymd(periodid, truncated = 1)) %>%
  left_join(df_monthly_collapsed_high_liquidity %>%
    dplyr::filter(periodid == 202001) %>%
    transmute(group,
      category,
      measure,
      value_jan2020 = value
    ),
  by = c("group", "category", "measure")
  ) %>%
  mutate(
    percent_change = value / value_jan2020 - 1,
    category = ifelse(category == "total_spend_cardcash", "Spending (card and cash)",
      ifelse(category == "income", "Income",
        ifelse(category == "total_spend_expanded", "Spending (total)", "Checking account balance")
      )
    ),
    group = ifelse(group == "unemployed", "Unemployed (get benefits from April 2020 through February 2021)", "Employed")
  ) %>%
  mutate(
    name = ordered(category, levels = c("Income", "Spending (total)", "Spending (card and cash)", "Checking account balance")),
    group = ordered(group, levels = c("Unemployed (get benefits from April 2020 through February 2021)", "Employed")),
    liquidity = "High 2018 Liquidity"
  )

df_monthly_collapsed_high_and_low_liq_for_plot <- 
  rbind(
    df_monthly_collapsed_high_liquidity_for_plot, 
    df_monthly_collapsed_low_liquidity_for_plot
  )


# create rectangular shaded box indicating when PUC is available
puc_available <-
  ## note: PUC expired on 7/31, we end the box at 7/10 because plot is monthly and this indicates that it was available thru July
  annotate("rect",
    xmin = as.Date("2020-03-28"), xmax = as.Date("2020-07-10"),
    ymax = Inf, ymin = -Inf, alpha = 0.1
  )

puc2_available <-
  annotate("rect",
    xmin = as.Date("2021-01-01"), xmax = as.Date("2021-02-10"),
    ymax = Inf, ymin = -Inf, alpha = 0.1
  )

plot_u_vs_e_onlyspend_by_liquidity <- function(df_in, measure_touse, subtitle_text,
                                               periodid_min = 201901,
                                               periodid_max = 202102,
                                               categories = c("Spending (total)"),
                                               nrow_facet = 1, ncol_facet = 2,
                                               leg_text_size = 12) {
  df_in %>%
    dplyr::filter(
      measure == measure_touse,
      periodid >= periodid_min,
      periodid <= periodid_max,
      category %in% categories
    ) %>%
    ggplot() +
    geom_line(aes(x = date, y = percent_change, color = group, shape=group)) +
    geom_point(aes(x = date, y = percent_change, color = group, shape=group)) +
    facet_wrap(vars(liquidity), scales = "free_y", nrow = nrow_facet, ncol = ncol_facet) +
    scale_y_continuous(labels = scales::percent_format(1), limits = c(-0.17, 0.25)) +
    scale_x_date(labels = date_format("%b %Y")) +
    puc_available +
    labs(
      x = "", y = "", subtitle = subtitle_text,
      color = ""
    ) +
    scale_color_manual("", values = colors_fact1) +
    scale_shape_manual("", values = shapes_fact1) +
    fte_theme("bottom") +
    theme(legend.text = element_text(size = 12), 
          legend.title = element_text(size=12, color = greys[7])) +
    guides(color = guide_legend(nrow=2, byrow = TRUE),
           shape = guide_legend(nrow=2, byrow = TRUE)) +
    geom_text(
      label = "$600\nsupplement\navailable",
      aes(x = date, y = y),
      color = greys[6],
      size = 3,
      data = data.frame(
        date = c(as.Date("2020-05-15")),
        y = c(-.15),
        name = c("Income")
      )
    ) +
    puc2_available +
    geom_text(
      label = "$300\nsupplement\navailable",
      aes(x = date, y = y),
      color = greys[6],
      size = 3,
      data = data.frame(
        date = c(as.Date("2021-01-15")),
        y = c(0.22),
        name = c("Income")
      )
    ) +
    scale_x_date(
      breaks = seq(as.Date("2019-01-01"), as.Date("2021-01-01"), by = "6 months"),
      labels = date_format("%b '%y")
    )
}


plot_spend_by_liquidity <- 
  (plot_u_vs_e_onlyspend_by_liquidity(
    df_monthly_collapsed_high_and_low_liq_for_plot, 
    "mean", 
    "Percent difference from January 2020 (mean total spending)",
    periodid_max = 202102,
    categories = c("Spending (total)")
  )
  ) %>%
    gg_walk_save("plot_spend_by_liquidity", height_touse = 6, width_touse = 12)

##############################################################
# save time-series and summary stats by group:
##############################################################
df_monthly_collapsed_high_liquidity_for_plot %>% write_csv(str_c(
  path_out,
  "df_monthly_collapsed_high_liquidity_for_plot.csv"
))
df_monthly_collapsed_low_liquidity_for_plot %>% write_csv(str_c(
  path_out,
  "df_monthly_collapsed_low_liquidity_for_plot.csv"
))
medians_by_buffer_quantiles %>% write_csv(str_c(
  path_out,
  "medians_by_buffer_quantiles.csv"
))

############################################################
# Compute APCS
############################################################
# This is the full sample, including both employed and unemployed over all dates not just the early april starts
cust_list_low_liquidity_full <- customer_buffer_quantiles_2018 %>%
  filter(med_buffer_quantile == 1)

cust_list_high_liquidity_full <- customer_buffer_quantiles_2018 %>%
  filter(med_buffer_quantile == 2)

source("pgm/funcs/mpc_robustness_funcs.R")

orig_df_demo_src_wins <- df_demo_src_wins

samples <- c("high_liquidity", "low_liquidity")
liq_reweighting_logic <- FALSE

samples %>%
  purrr::map(~filter_mpc_sample(.x) %>%
    construct_weighted_controls() %>%
    run_apc_calculations(., .x, liq_reweighting=liq_reweighting_logic) %>%
    as_tibble()) %>%
  set_names(str_c("table_apcs_combined_", samples)) %>%
  list2env(.GlobalEnv)

table_apcs_by_liquidity <-
  table_apcs_combined_high_liquidity %>%
  inner_join(table_apcs_combined_low_liquidity, 
             by = c("method", "design"), 
             suffix = c("_high_liquidity", "_low_liquidity")) %>%
  select(method, design, contains("apc_spend_exp"))

if (liq_reweighting_logic==TRUE){
  table_apcs_by_liquidity %>% write_csv(str_c(
    path_out,
    "apcs_table_by_liquidity.csv"
  ))
} else {
  table_apcs_by_liquidity %>% write_csv(str_c(
    path_out,
    "apcs_table_by_liquidity_noreweight.csv"
  ))
}

test_that("all output dfs are present", 
          expect_equal(all(out_dfs_ggs %>% map_lgl(exists)), TRUE))

# Edit apcs_table_by_liquidity table:
table_apcs_by_liquidity_df <- 
  read.csv(str_c(path_out, 
                 ifelse(liq_reweighting_logic == TRUE, 
                        "apcs_table_by_liquidity.csv",
                        "apcs_table_by_liquidity_noreweight.csv"))) %>%
  filter(method == "mean_weighted", 
         design != "LWA (denominator = total income)") %>%
  mutate(Difference = apc_spend_exp_low_liquidity - apc_spend_exp_high_liquidity,
         design = c("Waiting for benefits", "$600 expiration",
                    "$300 onset", 
                    "$300 expiration June states",
                    "$300 expiration Sept states",
                    "$300 expiration June vs. Sept states")) %>% 
  mutate_if(is.numeric, round, digits=2) %>%
  mutate(design = ifelse(design=="UI onset (waiting)", "Waiting for benefits", 
                         ifelse(design=="FPUC expiration", "$600 expiration",
                                as.character(design)))) %>%
  select(-method) %>%
  `colnames<-`(c("Design", "High Liquidity MPC", "Low Liquidity MPC",  "Difference"))

table_apcs_by_liquidity_tex_file <- table_apcs_by_liquidity_df %>%
  stargazer(., digits=2, summary=FALSE, type="latex", rownames = FALSE) 

# remove stargazer specific lines of code:
table_apcs_by_liquidity_tex_file <- table_apcs_by_liquidity_tex_file %>%
  str_subset("\\{table\\}|caption|label", negate = TRUE) %>%
  str_replace("cccc", "lccc") %>%
  str_replace_all("hline", c("toprule", "toprule", "bottomrule", "midrule")) %>%
  str_replace("UI onset (waiting)", "Waiting for Benefits") 

table_apcs_by_liquidity_tex_file %>%
  writeLines(str_c(path_out, ifelse(liq_reweighting_logic == TRUE, 
                                    "apcs_table_by_liquidity.tex",
                                    "apcs_table_by_liquidity_noreweight.tex")))

table_apcs_by_liquidity_html_file <- table_apcs_by_liquidity_df %>%
  stargazer(., digits=2, summary=FALSE, type="html", rownames = FALSE) 

table_apcs_by_liquidity_html_file %>%
  str_replace_all("onset</td>", "$300 onset</td>") %>%
  str_replace_all("expiration ", "$300 expiration ") %>%
  str_replace_all("expiration</td>", "$600 expiration</td>") %>%
  writeLines(str_c(path_out, ifelse(liq_reweighting_logic == TRUE, 
                                    "apcs_table_by_liquidity.html",
                                    "apcs_table_by_liquidity_noreweight.html")))
