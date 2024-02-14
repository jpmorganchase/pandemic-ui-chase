# SPDX-License-Identifier: MIT
# Data read in for producing 1% sub-sample
# September 2022
# This script reads in the 100% sample and produces a 1% sub-sample

#INS ####
latest_pid <- 202112
#########

# Data path #######
pids_2019_customers <- c(seq(201901, 201912)) 
weekly_flows_202021_latest <- "2023-01-12"
weekly_flows_2019_latest <- "2021-09-26"
rds_demog_latest <- "2023-01-11"
eips_latest <- "2023-01-12"
weekly_cp_latest <- "2023-01-12"
# prefixes
pids_2020_customers <- c(201912, seq(202001, 202012), seq(202101, latest_pid)) #2020 ui recips and non-ui customers have data from 201901 thru last pid of 2020 data
rds_weekly_prefix_2020 <- str_c(data_path, "weekly_flows/", weekly_flows_202021_latest, "weekly_flows_100pct_") #CHANGE THIS EACH MONTH
prefix_2020_ui_recip <- paste0(rds_weekly_prefix_2020, "202021_ui_recipient_")
prefix_2020_nonui_recip <- paste0(rds_weekly_prefix_2020, "2020_nonui_recipient_")
rds_weekly_prefix_2019 <- str_c(data_path, "weekly_flows/", weekly_flows_2019_latest, "weekly_flows_100pct_") ##prefix for 2019 UI recipients
prefix_2019_ui_recip <- paste0(rds_weekly_prefix_2019, "2019_ui_recipient_")
prefix_2019_nonui_recip <- paste0(rds_weekly_prefix_2019, "2019_nonui_recipient_")
rds_demog_list <- paste0(data_path, "demog/",
                         list.files(path = paste0(data_path, "demog/"),
                                    pattern = rds_demog_latest))
rds_eips <- paste0(data_path, "eips/", eips_latest, "eips_list_100pct.rds")

#####

# Data read in #####
data_1pct_path <- "/data/jpmci/teams/gnlab/ui_covid/1pct_data/"

subsample_cond <- function(df){
  df[endsWith(df$cust_number, 'a'),]
}

# rds_demog_list -- Done 1pct ####
read_in_df_demog <- rds_demog_list %>% map( ~ read_rds(.))

one_pct_df_demog <- map(seq_along(read_in_df_demog),
                        ~ subsample_cond(read_in_df_demog[[.x]]))

View(one_pct_df_demog)

names_one_pct_df_demog <- sub(".*demog/", "", rds_demog_list) %>%
  str_replace("100pct", "1pct") %>%
  str_replace(rds_demog_latest, as.character(Sys.Date()))

one_pct_df_demog_save <- map(seq_along(one_pct_df_demog),
                             ~saveRDS(one_pct_df_demog[[.x]], 
                                      file=paste0(data_1pct_path, "demog/", names_one_pct_df_demog[[.x]])))
#####

# rds_eips -- Done 1pct ####
read_in_df_eips <- read_rds(rds_eips)

one_pct_df_eips <- subsample_cond(read_in_df_eips)

names_one_pct_df_eips <- sub(".*eips/", "", rds_eips) %>%
  str_replace("100pct", "1pct") %>%
  str_replace(eips_latest, as.character(Sys.Date()))

test_that("1 pct",
          (nrow(one_pct_df_eips)/nrow(read_in_df_eips))*100 < 2)

saveRDS(one_pct_df_eips, 
        file=paste0(data_1pct_path, "eips/", names_one_pct_df_eips))
#####

# read_in_df_cp -- Done 1pct ####
path_df_cp <- list.files(str_c(data_path, "weekly_cp"), pattern = weekly_cp_latest, full.names = TRUE)
read_in_df_cp <- path_df_cp %>% map( ~ read_rds(.))

names_one_pct_df_cp <- sub(".*weekly_cp/", "", path_df_cp) %>%
  str_replace("100pct", "1pct") %>%
  str_replace(weekly_cp_latest, as.character(Sys.Date()))

one_pct_df_cp <- map(seq_along(read_in_df_cp),
                     ~ subsample_cond(read_in_df_cp[[.x]]))

one_pct_df_cp_save <- map(seq_along(one_pct_df_cp),
                          ~saveRDS(one_pct_df_cp[[.x]], 
                                   file=paste0(data_1pct_path, "weekly_cp/", names_one_pct_df_cp[[.x]])))

test_that("1 pct",
          (nrow(one_pct_df_cp[[1]])/nrow(read_in_df_cp[[1]]))*100 < 2)
#####

# read_in_weekly_ui -- Done 1pct ####
read_in_weekly_ui <- pids_2020_customers %>%
  map(~ read_rds(paste0(prefix_2020_ui_recip, ., '.rds')))

names_one_pct_weekly_ui <- sub(".*weekly_flows/", "", paste0(prefix_2020_ui_recip, 
                                                             pids_2020_customers, '.rds')) %>%
  str_replace("100pct", "1pct") %>%
  str_replace(weekly_flows_202021_latest, as.character(Sys.Date()))

one_pct_weekly_ui <- map(seq_along(read_in_weekly_ui),
                         ~ subsample_cond(read_in_weekly_ui[[.x]]))

one_pct_weekly_ui_save <- map(seq_along(one_pct_weekly_ui),
                              ~saveRDS(one_pct_weekly_ui[[.x]], 
                                       file=paste0(data_1pct_path, "weekly_flows/", 
                                                   names_one_pct_weekly_ui[[.x]])))

test_that("1 pct",
          (nrow(one_pct_weekly_ui[[1]])/nrow(read_in_weekly_ui[[1]]))*100 < 2)

#####

# read_in_weekly_nonui -- Done 1pct ####
read_in_weekly_nonui <- pids_2020_customers %>%
  map(~ read_rds(paste0(prefix_2020_nonui_recip, ., '.rds')))

names_one_pct_weekly_nonui <- sub(".*weekly_flows/", "", paste0(prefix_2020_nonui_recip, 
                                                                pids_2020_customers, '.rds')) %>%
  str_replace("100pct", "1pct")%>%
  str_replace(weekly_flows_202021_latest, as.character(Sys.Date()))

one_pct_weekly_nonui <- map(seq_along(read_in_weekly_nonui),
                            ~ subsample_cond(read_in_weekly_nonui[[.x]]))

one_pct_weekly_nonui_save <- map(seq_along(one_pct_weekly_nonui),
                                 ~saveRDS(one_pct_weekly_nonui[[.x]], 
                                          file=paste0(data_1pct_path, "weekly_flows/", names_one_pct_weekly_nonui[[.x]])))

test_that("1 pct",
          (nrow(one_pct_weekly_nonui[[10]])/nrow(read_in_weekly_nonui[[10]]))*100 < 2)

#####

# read_in_2019_ui_recip -- Done 1pct ####
read_in_2019_ui_recip <- read_rds(paste0(prefix_2019_ui_recip, "201912", '.rds'))

names_one_pct_2019_ui_recip <- sub(".*weekly_flows/", "",
                                   paste0(prefix_2019_ui_recip, "201912", '.rds')) %>% 
  str_replace("100pct", "1pct")%>%
  str_replace("2021-09-26", as.character(Sys.Date()))

one_pct_2019_ui_recip <- subsample_cond(read_in_2019_ui_recip)

saveRDS(one_pct_2019_ui_recip, file = paste0(data_1pct_path, "weekly_flows/", names_one_pct_2019_ui_recip))

test_that("1 pct",
          (nrow(one_pct_2019_ui_recip)/nrow(read_in_2019_ui_recip))*100 < 2)

#####

# read_in_2019_nonui_recip -- Done 1pct ####
read_in_2019_nonui_recip <- read_rds(paste0(prefix_2019_nonui_recip, "201912", '.rds'))

names_one_pct_2019_nonui_recip <- sub(".*weekly_flows/", "",
                                      paste0(prefix_2019_nonui_recip, "201912", '.rds')) %>% 
  str_replace("100pct", "1pct")%>%
  str_replace("2021-09-26", as.character(Sys.Date()))

one_pct_2019_nonui_recip <- subsample_cond(read_in_2019_nonui_recip)

saveRDS(one_pct_2019_nonui_recip, 
        file = paste0(data_1pct_path, "weekly_flows/", names_one_pct_2019_nonui_recip))

test_that("1 pct",
          (nrow(one_pct_2019_nonui_recip)/nrow(read_in_2019_nonui_recip))*100 < 2)

#####

# read_in_df_eip_rounds -- Done 1pct ####
rds_eip_rounds <-
  paste0(data_path, 'eips/', '2022-03-11eip_rounds_list_100pct.rds')
read_in_df_eip_rounds <- read_rds(rds_eip_rounds)

names_one_pct_df_eip_rounds <- sub(".*eips/", "",
                                   rds_eip_rounds) %>% 
  str_replace("100pct", "1pct")%>%
  str_replace("2022-03-11", as.character(Sys.Date()))

one_pct_df_eip_rounds <- subsample_cond(read_in_df_eip_rounds)

saveRDS(one_pct_df_eip_rounds, file = paste0(data_1pct_path, "eips/", names_one_pct_df_eip_rounds))

test_that("1 pct",
          (nrow(one_pct_df_eip_rounds)/nrow(read_in_df_eip_rounds))*100 < 2)
#####
