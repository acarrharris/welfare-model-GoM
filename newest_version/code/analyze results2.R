

#historical catch 
historical_catch <- read_dta("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/input_data/cod_hadd_catch_data_1_15.dta")

historical_catch<-historical_catch %>% 
  dplyr::filter(year>=2011 & year<=2020) %>% 
  dplyr::mutate(dtrip=1) %>% 
  dplyr::group_by(year) %>%
  summarise(across(c(tot_cat_cod, tot_cat_hadd, dtrip), \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% 
  dplyr::mutate(codcattrip=tot_cat_cod/dtrip, 
                haddcattrip=tot_cat_hadd/dtrip, 
                copula="baseline", 
                correlation="baseline") 



##
#calibration data 
# Create an empty list to store each data frame
calibration_list <- list()

# Loop through the numbers 1 to 100
for (i in 1:100) {
  # Construct the file name
  file_name <- paste0(output_data_cd, "calibration_data_2021draw", i, ".rds")
  
  # Read the .rds file and store it in the list
  calibration_list[[i]] <- readRDS(file_name)
}

# Combine all data frames into one (assuming they are all data frames or tibbles)
combined_calibration <- do.call(rbind, calibration_list)
combined_calibration<-combined_calibration %>% 
    dplyr::group_by(draw) %>%
     summarise(across(c(estimated_trips, n_choice_occasions, 
                        tot_cod_cat, tot_hadd_cat, 
                        tot_keep_cod, tot_keep_hadd, 
                        tot_rel_cod, tot_rel_hadd), \(x) sum(x, na.rm = TRUE)),.groups="drop") %>% 
  dplyr::mutate(codcattrip=tot_cod_cat/estimated_trips, 
                haddcattrip=tot_hadd_cat/estimated_trips) 


##data from 4-21-25

##Input data from Jorge
pkgs_to_use <- c("tidyverse",  "haven", "data.table")
install.packages(setdiff(pkgs_to_use, rownames(installed.packages())))  
lapply(pkgs_to_use, library, character.only = TRUE, quietly = TRUE)

library(tidyverse)
library(haven)
library(data.table)  # for fast fread()

# --- Step 1: Read in the Stata file ---
proj_data <- read_dta("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/input_data/projection_catch_base_wide.dta")

# Rename columns
proj_data <- proj_data %>%
  rename(month = month1, mode = mode1)

# Recode 'state' values
proj_data <- proj_data %>%
  mutate(state = case_when(
    state == "MA" ~ 25,
    state == "ME" ~ 23,
    state == "NH" ~ 33,
    TRUE ~ NA_real_
  ))

# Collapse (mean) of had* and cod* by month/mode/area/state
proj_data <- proj_data %>%
  dplyr::group_by(month, mode, area, state) %>%
  dplyr::summarise(across(starts_with("had"), \(x) mean(x, na.rm = TRUE)),
            across(starts_with("cod"),\(x) mean(x, na.rm = TRUE)),
            .groups = "drop")

# Reshape wide to long format for multiple 'decade' groups
proj_data_long <- proj_data %>%
  pivot_longer(
    cols = c(
      starts_with("cod_corr_clayton"), starts_with("cod_ind_clayton"),
      starts_with("had_corr_clayton"), starts_with("had_ind_clayton"), 
      starts_with("cod_corr_plackett"), starts_with("cod_ind_plackett"),
      starts_with("had_corr_plackett"), starts_with("had_ind_plackett"), 
      starts_with("cod_corr_gaussian"), starts_with("cod_ind_gaussian"),
      starts_with("had_corr_gaussian"), starts_with("had_ind_gaussian"), 
      starts_with("cod_corr_frank"), starts_with("cod_ind_frank"),
      starts_with("had_corr_frank"), starts_with("had_ind_frank"), 
      starts_with("cod_corr_gumbel"), starts_with("cod_ind_gumbel"),
      starts_with("had_corr_gumbel"), starts_with("had_ind_gumbel")
    ),
    names_to = c(".value", "decade"),
    names_pattern = "(cod_corr_clayton|had_corr_clayton|cod_ind_clayton|had_ind_clayton|cod_corr_plackett|had_corr_plackett|cod_ind_plackett|had_ind_plackett|cod_corr_gaussian|had_corr_gaussian|cod_ind_gaussian|had_ind_gaussian|cod_corr_frank|had_corr_frank|cod_ind_frank|had_ind_frank|cod_corr_gumbel|had_corr_gumbel|cod_ind_gumbel|had_ind_gumbel)(.+)"
  )

# --- Step 2: Read the directed trips CSV ---
directed_trips <- fread("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/input_data/directed trips and regulations 2010_2020_disaggregated.csv")

# Filter and rename
directed_trips <- directed_trips %>%
  filter(year == 2021) %>%
  rename(state = st)

# Collapse (sum) dtrip by month/mode/area/state
directed_trips_summarized <- directed_trips %>%
  group_by(month, mode, area, state) %>%
  summarise(dtrip = sum(dtrip, na.rm = TRUE), .groups = "drop")

# --- Step 3: Merge with reshaped projection data ---
merged_data <- directed_trips_summarized %>%
  left_join(proj_data_long, by = c("month", "mode", "area", "state")) %>%
  dplyr::filter(!is.na(decade)) %>%  # equivalent to dropping _merge==1
  select(-contains("ind"))  # drop *ind*

# --- Step 4: Collapse weighted means by decade ---
final_result <- merged_data %>%
  group_by(decade) %>%
  summarise(
    cod_corr_clayton = weighted.mean(cod_corr_clayton, dtrip, na.rm = TRUE),
    had_corr_clayton = weighted.mean(had_corr_clayton, dtrip, na.rm = TRUE),
    cod_corr_frank = weighted.mean(cod_corr_frank, dtrip, na.rm = TRUE),
    had_corr_frank = weighted.mean(had_corr_frank, dtrip, na.rm = TRUE),
    cod_corr_gumbel = weighted.mean(cod_corr_gumbel, dtrip, na.rm = TRUE),
    had_corr_gumbel = weighted.mean(had_corr_gumbel, dtrip, na.rm = TRUE),
    cod_corr_plackett = weighted.mean(cod_corr_plackett, dtrip, na.rm = TRUE),
    had_corr_plackett = weighted.mean(had_corr_plackett, dtrip, na.rm = TRUE),
    cod_corr_gaussian = weighted.mean(cod_corr_gaussian, dtrip, na.rm = TRUE),
    had_corr_gaussian = weighted.mean(had_corr_gaussian, dtrip, na.rm = TRUE),
    # If cod_ind and had_ind are still there
    #cod_ind = weighted.mean(cod_ind, dtrip, na.rm = TRUE),
    #had_ind = weighted.mean(had_ind, dtrip, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  select(-contains("ind"))  # drop *ind*
list(final_result)
head(final_result)
# If you want to write the result to a file:
# write_csv(final_result, "projection_summary.csv")


#2021 output 
base_output<-read_excel(paste0(output_data_cd,"model_output_4-21-25.xlsx")) %>% 
  dplyr::select(month, mode, area, state, draw, matches("base")) %>%
  rename_with(~ gsub("_(cat)", "\\1", .x), matches("_cat")) %>%
  rename_with(~ gsub("_(keep)", "\\1", .x), matches("_keep")) %>%
  rename_with(~ gsub("_(rel)", "\\1", .x), matches("_rel")) %>%
  rename_with(~ gsub("_(alt)", "\\1", .x), matches("_alt")) %>% 
  rename_with(~ gsub("base_", "", .x), contains("base_"))

base_output_long <- base_output %>%
  pivot_longer(
    cols =  starts_with("codkeep") | starts_with("codrel") | starts_with("haddkeep") |
      starts_with("haddrel") | starts_with("haddcat") | starts_with("codcat") | starts_with("ntrips"),  # Select columns to reshape
    names_to = c(".value", "correlation", "copula", "decade"), 
    names_pattern = "([a-z]+)_([a-z]+)_([a-z]+)([12345678])"  
  )

base_output_summarized <- base_output_long %>%
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, haddcat, haddkeep, haddrel, ntrips), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::mutate(haddrel=haddcat-haddkeep) %>% 
  dplyr::filter(copula=="clayton", correlation=="corr", decade==1) %>% 
  dplyr::mutate(copula="baseline", correlation="baseline", decade=0) %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips, 
                year=2021)  

mean(base_output_summarized$codcattrip)
mean(base_output_summarized$haddcattrip)

#Projection output by decade
output<-read_excel(paste0(output_data_cd,"model_output_4-21-25.xlsx")) %>% 
  dplyr::select(-matches("base"))  %>%
  rename_with(~ gsub("_(cat)", "\\1", .x), matches("_cat")) %>%
  rename_with(~ gsub("_(keep)", "\\1", .x), matches("_keep")) %>%
  rename_with(~ gsub("_(rel)", "\\1", .x), matches("_rel")) %>%
  rename_with(~ gsub("_(alt)", "\\1", .x), matches("_alt")) 

# Reshaping to long format
output_long <- output %>%
  pivot_longer(
    cols = starts_with("cv") | starts_with("codkeep") | starts_with("codrel") | starts_with("haddkeep") |
      starts_with("haddrel") | starts_with("haddcat") | starts_with("codcat") | starts_with("ntripsalt"),  # Select columns to reshape
    names_to = c(".value", "correlation", "copula", "decade"), 
    names_pattern = "([a-z]+)_([a-z]+)_([a-z]+)([12345678])"  
  )


# Summing selected variables by grouping variables
output_summarized <- output_long %>%
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cv, haddcat, haddkeep, haddrel, ntripsalt), \(x) sum(x, na.rm = TRUE)), .groups="drop")

output_summarized <- output_summarized %>%
  dplyr::mutate(decade=as.numeric(decade)) %>%  
  dplyr::mutate(codcattrip=codcat/ntripsalt, 
                codkeeptrip=codkeep/ntripsalt, 
                haddcattrip=haddcat/ntripsalt, 
                haddkeeptrip=haddkeep/ntripsalt, 
                cvtrip=cv/ntripsalt, 
                year=decade+2029) %>% 
  dplyr::rename(ntrips=ntripsalt)


## What to plot:
#cod keep trip/total
#hadd keep trip/total 
#cod catch trip/total
#hadd catch trip/total 
#cv trip/total

#Append the baseline output
output_summarized1 <- output_summarized %>%
  rbind.fill(base_output_summarized) 

#By decade and copula
output_summarized_corr<-output_summarized1 %>% 
  dplyr::filter(correlation=="corr" | correlation=="baseline") %>% 
  dplyr::select(-correlation)

# output_summarized_corr <- output_summarized_corr %>%
#   rename_with(~ paste0(.x, "_corr"), c(codcat, codkeep, codrel, cv, haddcat, haddkeep, haddrel, ntrips, 
#                                        codcattrip, codkeeptrip, haddcattrip, haddkeeptrip, cvtrip))

output_summarized_ind<-output_summarized1 %>% 
  dplyr::filter(correlation=="ind" | correlation=="baseline") %>% 
  dplyr::select(-correlation)

# output_summarized_ind <- output_summarized_ind %>%
#   rename_with(~ paste0(.x, "_ind"), c(codcat, codkeep, codrel, cv, haddcat, haddkeep, haddrel, ntrips, 
#                                       codcattrip, codkeeptrip, haddcattrip, haddkeeptrip, cvtrip))

# output_summarized <- output_summarized_corr %>% 
#   dplyr::left_join(output_summarized_ind, by=c("draw", "copula","decade" )) %>% 
#   dplyr::mutate(diff_codcat= codcat_corr-codcat_ind,
#                 diff_codkeep=  codkeep_corr- codkeep_ind,
#                 diff_codrel=  codrel_corr- codrel_ind,
#                 diff_cv=  cv_corr- cv_ind,
#                 diff_haddcat=  haddcat_corr- haddcat_ind,
#                 diff_haddkeep= haddkeep_corr- haddkeep_ind,
#                 diff_haddrel= haddrel_corr- haddrel_ind,
#                 diff_ntrips= ntrips_corr- ntrips_ind,
#                 diff_codcattrip=  codcattrip_corr- codcattrip_ind,
#                 diff_codkeeptrip= codkeeptrip_corr- codkeeptrip_ind,
#                 diff_haddcattrip= haddcattrip_corr- haddcattrip_ind,
#                 diff_haddkeeptrip= haddkeeptrip_corr- haddkeeptrip_ind,
#                 diff_cvtrip=cvtrip_corr- cvtrip_ind)
# 
# output_summarized<-rbind.fill(output_summarized, base_output_summarized)

ggplot(output_summarized1, aes(x=factor(decade), y=codcattrip, color = factor(copula)))+
  geom_boxplot() 

##data from 2-14-25

#2021 output 
base_output<-read_excel(paste0(output_data_cd,"model_output_2-14-25.xlsx")) %>% 
  dplyr::select(month, mode, area, state, draw, matches("base")) %>%
  rename_with(~ gsub("_(cat)", "\\1", .x), matches("_cat")) %>%
  rename_with(~ gsub("_(keep)", "\\1", .x), matches("_keep")) %>%
  rename_with(~ gsub("_(rel)", "\\1", .x), matches("_rel")) %>%
  rename_with(~ gsub("_(alt)", "\\1", .x), matches("_alt")) %>% 
  rename_with(~ gsub("base_", "", .x), contains("base_"))

base_output_long <- base_output %>%
  pivot_longer(
    cols =  starts_with("codkeep") | starts_with("codrel") | starts_with("haddkeep") |
      starts_with("haddrel") | starts_with("haddcat") | starts_with("codcat") | starts_with("ntrips"),  # Select columns to reshape
    names_to = c(".value", "correlation", "copula", "decade"), 
    names_pattern = "([a-z]+)_([a-z]+)_([a-z]+)([12345678])"  
  )

base_output_summarized <- base_output_long %>%
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, haddcat, haddkeep, haddrel, ntrips), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
  dplyr::mutate(haddrel=haddcat-haddkeep) %>% 
  dplyr::filter(copula=="clayton", correlation=="corr", decade==1) %>% 
  dplyr::mutate(copula="baseline", correlation="baseline", decade=0) %>% 
  dplyr::mutate(codcattrip=codcat/ntrips, 
                codkeeptrip=codkeep/ntrips, 
                haddcattrip=haddcat/ntrips, 
                haddkeeptrip=haddkeep/ntrips) 

mean(base_output_summarized$codcattrip)
mean(base_output_summarized$haddcattrip)

#Projection output by decade
output<-read_excel(paste0(output_data_cd,"model_output_4-21-25.xlsx")) %>% 
  dplyr::select(-matches("base"))  %>%
  rename_with(~ gsub("_(cat)", "\\1", .x), matches("_cat")) %>%
  rename_with(~ gsub("_(keep)", "\\1", .x), matches("_keep")) %>%
  rename_with(~ gsub("_(rel)", "\\1", .x), matches("_rel")) %>%
  rename_with(~ gsub("_(alt)", "\\1", .x), matches("_alt")) 

# Reshaping to long format
output_long <- output %>%
  pivot_longer(
    cols = starts_with("cv") | starts_with("codkeep") | starts_with("codrel") | starts_with("haddkeep") |
      starts_with("haddrel") | starts_with("haddcat") | starts_with("codcat") | starts_with("ntripsalt"),  # Select columns to reshape
    names_to = c(".value", "correlation", "copula", "decade"), 
    names_pattern = "([a-z]+)_([a-z]+)_([a-z]+)([12345678])"  
  )


# Summing selected variables by grouping variables
output_summarized <- output_long %>%
  group_by(draw, copula, correlation, decade) %>%
  summarise(across(c(codcat, codkeep, codrel, cv, haddcat, haddkeep, haddrel, ntripsalt), \(x) sum(x, na.rm = TRUE)), .groups="drop")

output_summarized <- output_summarized %>%
  dplyr::mutate(codcattrip=codcat/ntripsalt, 
                codkeeptrip=codkeep/ntripsalt, 
                haddcattrip=haddcat/ntripsalt, 
                haddkeeptrip=haddkeep/ntripsalt, 
                cvtrip=cv/ntripsalt) %>% 
  dplyr::rename(ntrips=ntripsalt)


## What to plot:
#cod keep trip/total
#hadd keep trip/total 
#cod catch trip/total
#hadd catch trip/total 
#cv trip/total

#Append the baseline output
output_summarized1 <- output_summarized %>%
  rbind.fill(base_output_summarized) %>% 
  dplyr::filter(correlation=="baseline" | correlation=="corr")

ggplot(output_summarized1, aes(x=factor(decade), y=haddcattrip, color = factor(copula)))+
  geom_boxplot() 



