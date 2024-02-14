# SPDX-License-Identifier: MIT
# mpc_cats.R
# Author: Tim Cejka
# This script loads a data image created in spend_build.R and
# computes MPCs for the main designs by spending category.

# INS ----
# Run spend_build.R prior to running this script.

# OUTS ----
name_df_cats_formatted_all_designs <- "df_cats_formatted_all_designs"
name_spending_categories_supp <- "spending_categories_supp"
name_spending_categories_main_1 <- "spending_categories_main_1"
name_spending_categories_main_2 <- "spending_categories_main_2"
name_stats_for_text <- "stats_for_text"
name_min_agg_standards <- "min_agg_standards"

#Setup ----
source("pgm/funcs/mpc_robustness_funcs.R")

# Definitions ----
durables = c(
  'Entertainment',
  'Home_Improvement',
  'Insurance',
  'Auto_Repair',
  "Hotels_&_Rental_cars",
  'Organizations_&_Institutions',
  'schools')
non_durables = c(
  'Drug_Stores',
  'Groceries',
  'Department_Stores',
  'Discount_Stores',
  'Telecom',
  'Healthcare',
  'Utilities',
  'Transit_&_Ground_Transportation',
  'Professional_&_Personal_Services',
  'Other_Retail',
  'Flights',
  'Restaurant',
  'Clothing')
debt_disagg_cols = c('on_us_creditcard_pmts',
                     'off_us_creditcard_pmts',
                     'mortgage_pmts',
                     'student_loan_pmts',
                     'other_loan_pmts',
                     'auto_loan_pmts')
cats_vector <- c(durables, non_durables, debt_disagg_cols)
cats_vector_to_sum <- c(durables, non_durables)

#from the run_apc_calculations function --
#csv_apcs: APC results from dplyr operations
#csv_apcs_ivreg: APC results from IV regressionsn
orig_df_demo_src_wins <- df_demo_src_wins

# 1. Clean ----
# CONSTRUCT FILTERS
# filter for non-chase credit cards
# customers who in 2019 have (non-chase credit cards) / (total outflows) = 0
tmp_df_demo_src_wins <-
  df_demo_src_wins %>%
  mutate(year_num = str_sub(periodid, 1, 4) %>% as.numeric()) %>%
  filter(year_num == 2019) %>%
  group_by(cust_number) %>%
  summarise_at(vars(off_us_creditcard_pmts, 
                    on_us_creditcard_pmts,
                    total_outflows),
               funs(sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(pct_cc_of_tot_outflows = if_else(is.na(off_us_creditcard_pmts),
                                          NA_real_,
                                          off_us_creditcard_pmts / total_outflows))

subsample_cust_non_chase_cc_filter <- # people we are keeping in subsample
  tmp_df_demo_src_wins %>%
  filter(pct_cc_of_tot_outflows == 0) %>%
  pull(cust_number)

# debt ach subsample
debt_ach_cust_list <-
  df_demo_src_wins %>%
  distinct(cust_number, periodid, debt_payments, total_spend_expanded) %>%
  mutate(mth_date = str_c(periodid, "01") %>% ymd()) %>%
  filter(mth_date < ymd("2020-01-01"),
         debt_payments > 0) %>%
  group_by(cust_number) %>%
  count() %>%
  filter(n == 12) %>%
  pull(cust_number)

# 3. GET APCS FOR ROBUSTNESS TABLES ----
#This produces: table_apcs_combined_all, table_apcs_combined_no_other_cc, table_apcs_combined_debt_ach 
samples <- c("all")
table_cleaned <- samples %>%
  map(~ filter_mpc_sample(.x) %>%
        construct_weighted_controls())

choose_category <- function(df_list, category){
  df_list <- table_cleaned[[1]]
  df <- df_list$df1
  if (category != "total_spend_cardcash") {
    df <- df %>%
      select(-total_spend_cardcash) %>%
      mutate(spending_category = category) %>%
      rename(total_spend_cardcash = category)
  }
  df_list_augmented <- list(df,
                            df_list$df2)
  return(df_list_augmented)
}

df1_original <- table_cleaned[[1]]$df1

# Clean categories ----
df_post_winsor <- df1_original %>% 
  mutate(`Transit_&_Ground_Transportation` = `Transit_&_Ground_Transportation` + `Fuel`,
         `Hotels_&_Rental_cars` = `Accomodation`,
         Healthcare = Hospitals + Other_Healthcare,
         Other_Retail = Other_Retail + Retail_Durables,
         `Professional_&_Personal_Services` = `Professional_&_Personal_Services` + Miscellaneous_Nondurables) %>%
  select(- c(`Fuel`,
             `Accomodation`,
             Hospitals,
             Other_Healthcare,
             Retail_Durables,
             Miscellaneous_Nondurables)) %>%
  mutate(total_categorized_spending_post_winsor = select(., cats_vector_to_sum) %>% rowSums(na.rm = TRUE))

table_cleaned[[1]]$df1 <- df_post_winsor

categories <- c("total_spend_cardcash", 
                cats_vector, "debt_payments",
                "total_categorized_spending_post_winsor",
                "uncatergorized_spend_cardcash", "uncatergorized_spend")

table_combined_mapped <- map2(table_cleaned, categories,
                              ~ run_apc_calculations(.x, c("all"), .y) %>%
                                as_tibble)

names(table_combined_mapped) <- categories

df_table_combined_mapped <- bind_rows(table_combined_mapped, 
                                      .id = "column_label") %>% 
  filter(design != "LWA") %>% 
  mutate(spending_category = if_else(column_label == "total_spend_cardcash",
                                     spending_category,
                                     spending_category.x)) %>% 
  select(-c(spending_category.x,
            spending_category.y,
            column_label)) %>% 
  mutate(keep = if_else((numerator == "Spend (cardcash)" & spending_category == "total_spend_cardcash") |
                          (numerator == "Spend (total)" & spending_category == "total_spend_expanded") |
                          ! spending_category %in% c("total_spend_cardcash", "total_spend_expanded"), 
                        1,
                        0)) %>% 
  filter(keep == 1) %>% 
  select(-keep)

write_csv(df_table_combined_mapped, str_c(path_out, 
                                          "df_table_combined_mapped", ".csv"))

# ANALYSIS
df_table_combined_mapped <- read_csv(str_c(path_out,  "df_table_combined_mapped", ".csv"))

df_cats_formatted <- df_table_combined_mapped %>% 
  mutate(spending_category = if_else(spending_category == "total_spend_expanded", 
                                     "total_spend",
                                     spending_category),
         to_keep = if_else(numerator != "Spend (total)" | spending_category == "total_spend",
                           1, 0)) %>% 
  filter(to_keep == 1) %>% 
  select(-c(numerator, to_keep)) %>% 
  select(spending_category,
         design, 
         everything()) %>% 
  mutate(spending_category = factor(spending_category, 
                                    levels= c("total_spend_cardcash","total_spend",
                                              cats_vector, "debt_payments",
                                              "uncatergorized_spend_cardcash", "uncatergorized_spend",
                                              "total_categorized_spending_post_winsor"))) %>% 
  arrange(spending_category)

df_cats_formatted_subset <- df_cats_formatted %>% 
  filter(denominator == "income")

df_total <- df_cats_formatted_subset %>% 
  filter(spending_category == "total_categorized_spending_post_winsor") %>% 
  select(design, apc, se) %>% 
  rename(apc_total = apc,
         se_total = se)

df_cats_merged <- df_cats_formatted_subset %>% 
  full_join(df_total, by = "design")

df_cats_sum <- df_cats_merged %>% 
  filter(spending_category %in% cats_vector_to_sum) %>% 
  group_by(design) %>% 
  summarise(apc = sum(apc)) %>% 
  left_join(df_total, by ="design") %>% 
  mutate(apc_over_total_categorized_spend =  apc/apc_total) %>% 
  mutate(spending_category = "sum of apcs") %>% 
  ungroup()

df_cats_formatted_subset_w_shares <- df_cats_merged %>% 
  group_by(design) %>% 
  mutate(apc_over_total_categorized_spend =  apc/apc_total) 

df_cats_formatted_w_shares <- df_cats_sum %>% 
  bind_rows(df_cats_formatted_subset_w_shares) %>% 
  select(-c(se_total)) %>% 
  arrange(desc(design), desc(apc))

df_cats_formatted <- df_cats_formatted_w_shares %>% 
  rename(`apc share of total apc` = apc_over_total_categorized_spend)

write_csv(df_cats_formatted, str_c(path_out, 
                                   name_df_cats_formatted_all_designs, ".csv"))

# Production table
designs_for_production <- c("UI onset (waiting)",
                            "FPUC expiration",
                            "$300 onset",
                            "$300 expiration Sept states")

designs_for_production_tex <- c("UI onset (waiting)",
                                "FPUC expiration",
                                "\\$300 onset",
                                "\\$300 expiration")

cats_main_table <- sort(c(durables, 
                          non_durables))

cats_supp_table <- c("auto_loan_pmts",
                     "student_loan_pmts",
                     "mortgage_pmts")

create_cats_table <- function(data, cats_for_table){
  data %>% 
    filter(design %in% designs_for_production) %>% 
    rename(category = spending_category) %>% 
    rename(apc_category = apc) %>% 
    group_by(design) %>% 
    mutate(apc_total = apc_category[which(category == "total_categorized_spending_post_winsor")],
           spend_total_unemp =  spend_category_unemp[which(category == "total_categorized_spending_post_winsor")],
           spend_total_emp =  spend_category_emp[which(category == "total_categorized_spending_post_winsor")]) %>% 
    mutate(`apc_category/apc_total` = apc_category/apc_total,
           `spend_category_unemp/spend_total_unemp` = spend_category_unemp/spend_total_unemp,
           `spend_category_emp/spend_total_emp` = spend_category_emp/spend_total_emp) %>% 
    ungroup() %>% 
    rename(apc_category_se = se) %>% 
    filter(category %in% c(cats_for_table)) 
}

df_main <- create_cats_table(data = df_cats_formatted,
                             cats_for_table = cats_main_table) %>% 
  select(design,
         category, 
         apc_category,
         `apc_category/apc_total`,
         `spend_category_emp/spend_total_emp`,
         `spend_category_unemp/spend_total_unemp`,
         n_obs) %>% 
  arrange(desc(design), category) %>% 
  mutate(category = case_when(category == "Organizations_&_Institutions" ~ "Orgs_&_Institutions",
                              category == "Professional_&_Personal_Services" ~ "Prof_&_Personal_Services",
                              category == "Transit_&_Ground_Transportation" ~ "Transport",
                              category == "Restaurant" ~ "Restaurants",
                              category == "schools" ~ "Schools",
                              category == "Hotels_&_Rental_cars" ~ "Hotels_&_Rental_Cars",
                              TRUE ~ category)) 

df_supp <- create_cats_table(data = df_cats_formatted,
                             cats_for_table = cats_supp_table) %>% 
  select(design,
         category, 
         apc_category,
         apc_category_se,
         `apc_category/apc_total`,
         `spend_category_emp/spend_total_emp`,
         `spend_category_unemp/spend_total_unemp`,
         n_obs) %>% 
  arrange(desc(design))  %>% 
  mutate(category = case_when(category == "auto_loan_pmts" ~ "Auto Loans",
                              category == "student_loan_pmts" ~ "Student Loans",
                              category == "mortgage_pmts" ~ "Mortgages",
                              TRUE ~ category)) 

# Write summary tables
create_table_mpcs_main <- function(df_table, 
                                   index_designs_for_table,
                                   table_name){
  
  df_table_cleaned <-df_table  %>% 
    select(-`spend_category_emp/spend_total_emp`,
           n_obs) %>% 
    pivot_wider(names_from = design, 
                values_from = c(apc_category, 
                                `apc_category/apc_total`,
                                `spend_category_unemp/spend_total_unemp`),
                names_sep = ": ")  %>% 
    select(category,
           contains(designs_for_production[[index_designs_for_table[[1]]]]),
           contains(designs_for_production[[index_designs_for_table[[2]]]])) %>% 
    mutate(category = gsub("_", " ", category)) %>% 
    arrange(category)
  # Add empty column
  df_table_cleaned_w_empty_col <- df_table_cleaned %>% 
    mutate(empty_col = "")
  #must use integer indexing here since relocate() or .after doesn't exist in this version of tidyverse
  df_table_cleaned_reordered <- cbind(df_table_cleaned_w_empty_col[,c(1:4)],
                                      df_table_cleaned_w_empty_col[,"empty_col"],
                                      df_table_cleaned_w_empty_col[,c(5:7)])
  xtable <- xtable(df_table_cleaned_reordered, align="llccccccc",
                   digits = c(0,0, rep(3,7)))
  
  # Note: the last command gives a warning but it's safe to ignore.
  table_num_cols <- 'MPC & MPC Share & Pre-Pandemic'
  table_den_cols <- ' &   & Spend Share'
  
  
  addtorow <- list()
  addtorow$pos <- list(0, 0, 0, 0, nrow(xtable))
  addtorow$command[[1]] <- paste0(paste0('\\toprule \\toprule & \\multicolumn{3}{c}{ ', designs_for_production_tex[[index_designs_for_table[[1]]]], '} &&',
                                         '\\multicolumn{3}{c}{ ', designs_for_production_tex[[index_designs_for_table[[2]]]],  '}',collapse=''), '\\\\')
  addtorow$command[[2]] <- paste0('\\cmidrule(lr){2-4} \\cmidrule(lr){6-8} &', table_num_cols, '&&', table_num_cols,  collapse='', '\\\\')
  addtorow$command[[3]] <- paste0(' &', table_den_cols, '&&', table_den_cols,  collapse='', '\\\\')
  addtorow$command[[4]] <- paste0('\\midrule    ',  collapse='')
  addtorow$command[[5]] <- paste0('\\bottomrule',  collapse='')
  
  print(xtable, type="latex", include.rownames=FALSE, 
        add.to.row=addtorow, include.colnames=FALSE,
        booktabs = TRUE, hline.after = NULL,
        file=paste0(path_out, table_name, ".tex"), 
        format.args = list(big.mark = ",", decimal.mark = "."))
  write.csv(df_table_cleaned_reordered, paste0(path_out, table_name, ".csv"))
  return(xtable)
}

table_main_1 <- create_table_mpcs_main(df_table = df_main,
                                       index_designs_for_table = list(1,2),
                                       table_name = name_spending_categories_main_1)

table_main_2 <- create_table_mpcs_main(df_table = df_main,
                                       index_designs_for_table = list(3, 4),
                                       table_name = name_spending_categories_main_2)

format_se <- function(vec) {
  #Format to exactly 3 decimal places and surround by parantheses
  vec <- paste0("(",
                format(round(vec, 3), nsmall = 3),
                ")")
}


format_coef <- function(vec) {
  #Format to exactly 3 decimal places
  vec <- format(round(vec, 3), nsmall = 3)
}

create_table_mpcs_supp <- function(df_table, 
                                   table_name){
  
  df_table_cleaned_est <-df_table  %>% 
    filter(design %in% designs_for_production) %>% 
    select(design,
           category,
           apc_category) %>% 
    pivot_wider(names_from = design, 
                values_from = c(apc_category),
                names_sep = ": ")  %>% 
    mutate(category = gsub("_", " ", category)) %>% 
    arrange(category) %>% 
    mutate(stat = "coef") %>% 
    mutate_if(is.numeric, ~ format_coef(.))
  
  
  df_table_cleaned_se <-df_table  %>% 
    filter(design %in% designs_for_production) %>% 
    select(design,
           category,
           apc_category_se) %>% 
    pivot_wider(names_from = design, 
                values_from = c(apc_category_se),
                names_sep = ": ")  %>% 
    mutate(category = gsub("_", " ", category)) %>% 
    arrange(category) %>% 
    mutate(stat = "se") %>% 
    mutate_if(is.numeric, ~ format_se(.)) 
  
  
  df_table_cleaned <- df_table_cleaned_est %>% 
    rbind(df_table_cleaned_se) %>% 
    arrange(category, stat) %>% 
    mutate(category = if_else(stat == "se", "", category)) %>% 
    select(-stat) 
  
  # Add empty column
  df_table_cleaned_w_empty_col <- df_table_cleaned %>% 
    mutate(empty_col = "")
  #must use integer indexing here since relocate() or .after doesn't exist in this version of tidyverse
  df_table_cleaned_reordered <- cbind(df_table_cleaned_w_empty_col[,c(1:3)],
                                      df_table_cleaned_w_empty_col[,"empty_col"],
                                      df_table_cleaned_w_empty_col[,c(4:5)])
  
  xtable <- xtable(df_table_cleaned_reordered, align="llccccc",
                   digits = c(0,0, rep(3,5)))
  
  table_num_cols <- paste0(designs_for_production_tex[[1]],
                           ' & ',
                           designs_for_production_tex[[2]],
                           ' && ',
                           designs_for_production_tex[[3]],
                           ' & ',
                           designs_for_production_tex[[4]])
  
  
  addtorow <- list()
  addtorow$pos <- list(0, 0, nrow(xtable))
  addtorow$command[[1]] <- paste0(paste0('\\toprule \\toprule & ', table_num_cols ,collapse=''), '\\\\')
  addtorow$command[[2]] <- paste0('\\midrule    ',  collapse='')
  addtorow$command[[3]] <- paste0('\\bottomrule',  collapse='')
  
  print(xtable, type="latex", include.rownames=FALSE, 
        add.to.row=addtorow, include.colnames=FALSE,
        booktabs = TRUE, hline.after = NULL,
        file=paste0(path_out, table_name, ".tex"), 
        format.args = list(big.mark = ",", decimal.mark = "."))
  write.csv(df_table_cleaned_reordered, paste0(path_out, table_name, ".csv"))
  return(xtable)
}

table_supp <- create_table_mpcs_supp(df_table = df_supp,
                                     table_name = name_spending_categories_supp)

# Stats for text
df_stats_for_text <- read_csv(str_c(path_out, 
                                    name_df_cats_formatted_all_designs, ".csv")) %>% 
  filter(design %in% designs_for_production,
         spending_category == "off_us_creditcard_pmts")

write_csv(df_stats_for_text, str_c(path_out, name_stats_for_text, ".csv"))

# Min agg standards ----
min_agg_cats <- df_main %>% 
  mutate(table = case_when(design %in% c(designs_for_production[[1]],
                                         designs_for_production[[2]]) ~ name_spending_categories_main_1,
                           design %in% c(designs_for_production[[3]],
                                         designs_for_production[[4]]) ~ name_spending_categories_main_2)) %>% 
  bind_rows(df_supp %>% mutate(table = name_spending_categories_supp)) %>% 
  group_by(table) %>% 
  dplyr::summarize(min_n = min(n_obs)) 

write_csv(min_agg_cats, str_c(path_out, name_min_agg_standards, ".csv"))

