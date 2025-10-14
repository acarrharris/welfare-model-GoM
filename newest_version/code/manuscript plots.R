


historical_and_projections<- read_excel(paste0(output_data_cd,"bottom_temp_reformat1.xlsx"))  #save the data
historical_and_projections_annual<- read_excel(paste0(output_data_cd,"bottom_temp_reformat2.xlsx")) 
output_list_y<-list()

k=1
yrz<- c(2019, 2020, 2021)
for (y in yrz){
projection_temps<-historical_and_projections %>% 
  dplyr::filter(is.na(year))


output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", y,"_outliers_9-21-25_reformat_coast.rds"))  %>%
  dplyr::mutate(codreltrip=codrel/ntrips,
                haddreltrip=haddrel/ntrips, 
                codrelchoice=codrel/n_choice_occasions,
                haddrelchoice=haddrel/n_choice_occasions, 
                cod_hadd_keep=codkeep+haddkeep, 
                cod_hadd_cat=codcat+haddcat)

output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
output_summarized1$copula[output_summarized1$correlation=="independent"] <- "independent"

output2<-output_summarized1 %>%
  dplyr::filter(!is.na(decade))  %>%
  dplyr::filter(decade!=0)  %>%
  dplyr::mutate(decade = dplyr::recode(decade,
                                       `1` = "2021-2030",
                                       `2` = "2031-2040",
                                       `3` = "2041-2050",
                                       `4` = "2051-2060",
                                       `5` = "2061-2070",
                                       `6` = "2071-2080",
                                       `7` = "2081-2090",
                                       `8` = "2091-3000"
  )) 

df_subset <- output2 %>%
  # keep only the independent/independent rows
  dplyr::filter(copula == "independent", correlation == "independent") %>%
  # stratified sample: 100 per decade
  dplyr::group_by(decade) %>%
  dplyr::slice_sample(n = 100) %>%
  dplyr::ungroup()

output2<-output2 %>% 
  dplyr::filter(copula!="independent") 

output2<-output2 %>% 
  plyr::rbind.fill(df_subset) %>% 
  dplyr::select(-year) %>% 
  dplyr::mutate(codkeep=codkeep/1000, haddkeep=haddkeep/1000, cv=cv/1000000,
                codcat=codcat/1000, haddcat=haddcat/1000,)



library(ggplot2)
library(patchwork)

# Reorder factor levels so "independent" is first
output2$copula <- factor(output2$copula,
                         levels = c("independent", setdiff(unique(output2$copula), "independent")))

# Shared color scale: no title, single row legend
copula_colors <- scale_color_discrete(
  name = NULL,
  guide = guide_legend(nrow = 1)
)

output_list_y[[k]]<-output2
k=k+1
w<-.5
sz=.3
osz=.5
# 1. Temperatures with decade strip labels
p1 <- ggplot(projection_temps, aes(month, mean_temp)) +
  geom_point(size = .9) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width=w, linewidth=sz) +
  facet_wrap(~decade, nrow = 1) +
  geom_hline(aes(yintercept = mean_decadal_tmp), color = "red", size = .2) +
  labs(x = NULL, y = "Sea bottom\ntemperature (C)") +
  copula_colors +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    strip.text    = element_text(size = 10, margin = margin(t = 5, b = 5)),
    legend.position = "none", 
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank()
  )


# 2. Cod harvest
p2 <- ggplot(output2, aes(x = factor(copula), y = codkeep, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  facet_wrap(~decade, nrow = 1) +
  labs(x = NULL, y = "Cod harvest\n('000s)") +
  copula_colors +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    strip.text    = element_blank(),
    legend.position = "none", 
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank()
  )

# 3. Haddock harvest
p3 <- ggplot(output2, aes(x = factor(copula),y = haddkeep, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  facet_wrap(~decade, nrow = 1) +
  labs(x = NULL, y = "Haddock harvest\n('000s)") +
  copula_colors +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    strip.text    = element_blank(),
    legend.position = "none", 
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank()
  )


# Check if 0 is between the minimum lower bound and the maximum upper bound
zero_line_layer <- if (min(output2$cv) < 0 && max(output2$cv) > 0) {
  # Add the line if 0 is within the range. Using a light grey and dashed line.
  geom_hline(yintercept = 0, color = "grey40", size = 0.2)
} else {
  # Otherwise, return NULL, which ggplot will ignore.
  NULL
}

# 4. Compensating variation
p4 <- ggplot(output2, aes(x = factor(copula), y = cv, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  facet_wrap(~decade, nrow = 1) +
  zero_line_layer +
  labs(x=NULL,  y = "Compensating\nvariation ($M)") +
  copula_colors +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    strip.text    = element_blank() , 
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank()

  )+
  scale_y_continuous(breaks = seq(
    floor(min(output2$cv, na.rm = TRUE)), 
    ceiling(max(output2$cv, na.rm = TRUE)), 
    by = 1   # tick every 1 unit
  )) 


# Combine plots with shared legend
final_plot <- (p1 / p2 / p3 / p4) +
  plot_layout(heights = c(1, 1, 1, 1), guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title.y = element_text(size = 9), 
    legend.key = element_blank()
  )

final_plot
ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/havest_cv_plot", y,".jpg"), plot = final_plot, width = 12, height = 8, units = "in")


#plot 2 
# 1. Cod catch
p1 <- ggplot(output2, aes(x = factor(copula), y = codcattrip, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  labs(x = NULL, y = " \nCod catch-per-trip\n ") +
  copula_colors +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    strip.text    = element_text(size = 10, margin = margin(t = 5, b = 5)),
    legend.position = "none", 
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank()
  )+
  facet_wrap(~decade, nrow = 1)
  

# 2. Haddock catch
p2 <- ggplot(output2, aes(x = factor(copula), y = haddcattrip, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  facet_wrap(~decade, nrow = 1) +
  labs(x = NULL, y = " \nHaddock catch-per-trip\n ") +
  copula_colors +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    strip.text    = element_blank(),
    legend.position = "none", 
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank()
  )

# 2. k-tau catch 
ktau_yr<-k_tau_data_combined_coast %>% 
  dplyr::filter(base_year==y)


y_min <- floor(min(ktau_yr$k_tau_catch_est, ktau_yr$k_tau_keep_est, na.rm = TRUE)*10)/ 10
y_max <- ceiling(max(ktau_yr$k_tau_catch_est, ktau_yr$k_tau_keep_est, na.rm = TRUE)*10)/ 10

# 2. Define the common breaks sequence
y_breaks <- seq(y_min, y_max, by = 0.1)

# Check if 0 is between the minimum lower bound and the maximum upper bound
zero_line_layer <- if (min(ktau_yr$k_tau_catch_est, ktau_yr$k_tau_keep_est) < 0 && max(ktau_yr$k_tau_catch_est, ktau_yr$k_tau_keep_est) > 0) {
  # Add the line if 0 is within the range. Using a light grey and dashed line.
  geom_hline(yintercept = 0, color = "grey40", size = 0.2)
} else {
  # Otherwise, return NULL, which ggplot will ignore.
  NULL
}

p3 <- ggplot(ktau_yr, aes(x = factor(copula), y = k_tau_catch_est, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  facet_wrap(~decade, nrow = 1) +
  labs(x = NULL, y = "Correlation in\ncatch-per-trip\n(Kendall's tau)") +
  copula_colors +
  # Add the horizontal line at y = 0
  zero_line_layer +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    strip.text    = element_blank(),
    legend.position = "none", 
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank()) +
  # Apply common limits and breaks
  scale_y_continuous(
    limits = c(y_min, y_max),
    breaks = y_breaks
  )

# 2. k-tau harvest 
p4 <- ggplot(ktau_yr, aes(x = factor(copula), y = k_tau_keep_est, color = copula)) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  facet_wrap(~decade, nrow = 1) +
  labs(x = NULL, y = "Correlation in\n harvest-per-trip\n(Kendall's tau)") +
  copula_colors +
  # Add the horizontal line at y = 0
  zero_line_layer +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    strip.text    = element_blank() , 
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank()) +
  # Apply common limits and breaks
  scale_y_continuous(
    limits = c(y_min, y_max),
    breaks = y_breaks
  )

# Combine plots with shared legend
final_plot <- (p1 / p2 / p3 / p4) +
  plot_layout(heights = c(1, 1, 1, 1), guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    axis.title.y = element_text(size = 9), 
    legend.key = element_blank()
  )

final_plot

ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/ktau_plot", y,".jpg"), plot = final_plot, width = 12, height = 8, units = "in")
}

output_allyears <- dplyr::bind_rows(output_list_y)



### Values for results section
# Pull in all the data and reformat for plotting 
k_tau_data_combined_coast_list<-list()
k_tau_data_combined_state_list<-list()
k_tau_data_combined_mode_list<-list()
output_summarized1_list<-list()
plot_data_list<-list()
cvtrip_diff_list<-list()
cv_pct_list<-list()
diff_list<-list()

years<-c(2019, 2020, 2021)
for(yr in years){
  
  ktau_data<-read_excel(paste0(output_data_cd,"ktau_output_y", yr, "_outliers_9-21-25.xlsx"))
  ktau_annual<-ktau_data %>% filter(month==0 & domain1=="all") %>% 
    dplyr::select(-k_tau_catch_p, -k_tau_keep_p, -domain1) %>% 
    dplyr::mutate(pct_both_above=(both_above/2000)*100, pct_both_below=(both_below/2000)*100)
  
  ktau_annual_long <- ktau_annual %>%
    mutate(parts = str_split(domain, "_", simplify = TRUE),
           correlation = parts[, 1],
           copula = str_extract(parts[, 2], "^[a-zA-Z]+"),
           decade = str_extract(parts[, 2], "[0-9]+")
    ) %>%
    select(draw, correlation, copula, decade, k_tau_keep_est, k_tau_catch_est, pct_both_above, pct_both_below)
  
  ktau_data_ind<-read_excel(paste0(output_data_cd,"ktau_output_y", yr,"_outliers_9-21-25_ind.xlsx"))
  ktau_annual_ind<-ktau_data_ind %>% filter(month==0 & domain1=="all") %>% 
    dplyr::select(-k_tau_catch_p, -k_tau_keep_p, -domain1)%>% 
    dplyr::mutate(pct_both_above=(both_above/2000)*100, pct_both_below=(both_below/2000)*100)
  
  ktau_annual_ind_long <- ktau_annual_ind %>%
    mutate(parts = str_split(domain, "_", simplify = TRUE),
           correlation = parts[, 1],
           copula = str_extract(parts[, 2], "^[a-zA-Z]+"),
           decade = str_extract(parts[, 2], "[0-9]+")
    ) %>%
    select(draw, correlation, copula, decade, k_tau_keep_est, k_tau_catch_est, pct_both_above, pct_both_below) %>% 
    dplyr::mutate(copula="independent")
  
  k_tau_data_combined_coast<-ktau_annual_ind_long %>%
    plyr::rbind.fill(ktau_annual_long) 
  
  k_tau_data_combined_coast<- k_tau_data_combined_coast %>% 
    dplyr::filter(decade!=0) %>% 
    dplyr::filter(!is.na(decade))
  
  k_tau_data_combined_coast$copula <- factor(k_tau_data_combined_coast$copula,
                                             levels = c("independent", setdiff(unique(k_tau_data_combined_coast$copula), "independent")))
  k_tau_data_combined_coast$base_year<-yr
  
  k_tau_data_combined_coast_list[[yr]]<-k_tau_data_combined_coast
  
  
  # coastwide by state ktau data 
  ktau_data<-read_excel(paste0(output_data_cd,"ktau_output_y", yr, "_outliers_9-21-25.xlsx"))
  ktau_annual<-ktau_data %>% filter(month==0 & domain1 %in% c("ME", "NH", "MA")) %>% 
    dplyr::select(-k_tau_catch_p, -k_tau_keep_p) %>% 
    dplyr::rename(state=domain1) %>% 
    dplyr::mutate(pct_both_above=(both_above/2000)*100, pct_both_below=(both_below/2000)*100)
  
  ktau_annual_long <- ktau_annual %>%
    mutate(parts = str_split(domain, "_", simplify = TRUE),
           correlation = parts[, 1],
           copula = str_extract(parts[, 2], "^[a-zA-Z]+"),
           decade = str_extract(parts[, 2], "[0-9]+")
    ) %>%
    select(draw, correlation, copula, decade, k_tau_keep_est, k_tau_catch_est, state, pct_both_above,pct_both_below)
  
  
  ktau_data_ind<-read_excel(paste0(output_data_cd,"ktau_output_y", yr,"_outliers_9-21-25_ind.xlsx"))
  ktau_annual_ind<-ktau_data_ind %>% filter(month==0 & domain1 %in% c("ME", "NH", "MA")) %>% 
    dplyr::select(-k_tau_catch_p, -k_tau_keep_p) %>% 
    dplyr::rename(state=domain1) %>% 
    dplyr::mutate(pct_both_above=(both_above/2000)*100, pct_both_below=(both_below/2000)*100)
  
  ktau_annual_ind_long <- ktau_annual_ind %>%
    mutate(parts = str_split(domain, "_", simplify = TRUE),
           correlation = parts[, 1],
           copula = str_extract(parts[, 2], "^[a-zA-Z]+"),
           decade = str_extract(parts[, 2], "[0-9]+")
    ) %>%
    select(draw, correlation, copula, decade, k_tau_keep_est, k_tau_catch_est, state, pct_both_above, pct_both_below) %>% 
    dplyr::mutate(copula="independent")
  
  k_tau_data_combined_state<-ktau_annual_ind_long %>% 
    plyr::rbind.fill(ktau_annual_long) 
  
  k_tau_data_combined_state<- k_tau_data_combined_state %>% 
    dplyr::filter(decade!=0) %>% 
    dplyr::filter(!is.na(decade))
  
  k_tau_data_combined_state$copula <- factor(k_tau_data_combined_state$copula,
                                             levels = c("independent", setdiff(unique(k_tau_data_combined_state$copula), "independent")))
  k_tau_data_combined_state$base_year<-yr
  
  k_tau_data_combined_state_list[[yr]]<-k_tau_data_combined_state
  
  # coastwide by mode ktau data 
  ktau_data<-read_excel(paste0(output_data_cd,"ktau_output_y", yr,"_outliers_9-21-25.xlsx"))
  ktau_annual<-ktau_data %>% filter(month==0 & domain1 %in% c("pr", "fh")) %>% 
    dplyr::select(-k_tau_catch_p, -k_tau_keep_p) %>% 
    dplyr::rename(mode=domain1) %>% 
    dplyr::mutate(pct_both_above=(both_above/2000)*100, pct_both_below=(both_below/2000)*100)
  
  ktau_annual_long <- ktau_annual %>%
    mutate(parts = str_split(domain, "_", simplify = TRUE),
           correlation = parts[, 1],
           copula = str_extract(parts[, 2], "^[a-zA-Z]+"),
           decade = str_extract(parts[, 2], "[0-9]+")
    ) %>%
    select(draw, correlation, copula, decade, k_tau_keep_est, k_tau_catch_est, mode, pct_both_above, pct_both_below)
  
  
  ktau_data_ind<-read_excel(paste0(output_data_cd,"ktau_output_y", yr, "_outliers_9-21-25_ind.xlsx"))
  ktau_annual_ind<-ktau_data_ind %>% filter(month==0 & domain1 %in% c("pr", "fh")) %>% 
    dplyr::select(-k_tau_catch_p, -k_tau_keep_p) %>% 
    dplyr::rename(mode=domain1) %>% 
    dplyr::mutate(pct_both_above=(both_above/2000)*100, pct_both_below=(both_below/2000)*100)
  
  ktau_annual_ind_long <- ktau_annual_ind %>%
    mutate(parts = str_split(domain, "_", simplify = TRUE),
           correlation = parts[, 1],
           copula = str_extract(parts[, 2], "^[a-zA-Z]+"),
           decade = str_extract(parts[, 2], "[0-9]+")
    ) %>%
    select(draw, correlation, copula, decade, k_tau_keep_est, k_tau_catch_est, mode, pct_both_above, pct_both_below) %>% 
    dplyr::mutate(copula="independent")
  
  k_tau_data_combined_mode<-ktau_annual_ind_long %>% 
    plyr::rbind.fill(ktau_annual_long) 
  
  k_tau_data_combined_mode<- k_tau_data_combined_mode %>% 
    dplyr::filter(decade!=0) %>% 
    dplyr::filter(!is.na(decade))
  
  k_tau_data_combined_mode$copula <- factor(k_tau_data_combined_mode$copula,
                                            levels = c("independent", setdiff(unique(k_tau_data_combined_mode$copula), "independent")))
  
  
  k_tau_data_combined_mode$base_year<-yr
  
  k_tau_data_combined_mode_list[[yr]]<-k_tau_data_combined_mode
  
  #plot data 
  output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", yr,"_outliers_9-21-25_reformat_coast.rds"))  %>%
    dplyr::mutate(codreltrip=codrel/ntrips,
                  haddreltrip=haddrel/ntrips, 
                  codrelchoice=codrel/n_choice_occasions,
                  haddrelchoice=haddrel/n_choice_occasions, 
                  cod_hadd_keep=codkeep+haddkeep, 
                  cod_hadd_cat=codcat+haddcat)
  
  output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
  output_summarized1$copula[output_summarized1$correlation=="independent"] <- "independent"
  output_summarized1$base_year<-yr
  output_summarized1_list[[yr]]<-output_summarized1
  
  plot_data<-output_summarized1 %>% 
    dplyr::filter(decade!=0)
  
  plot_data$base_year<-yr
  plot_data_list[[yr]]<-plot_data
  
  #differences data CV
  
  #pull in data for closure of both fisheries
  closure<-read_excel(paste0(output_data_cd,"model_output_y", yr, "_outliers_9-21-25_close cod and haddock.xlsx")) %>% 
    dplyr::rename(cv_closure=change_CS) %>% 
    dplyr::group_by(draw)  %>% 
    summarise(across(c(cv_closure), \(x) sum(x, na.rm = TRUE)), .groups="drop") %>% 
    dplyr::ungroup()  
  
  output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", yr,"_outliers_9-21-25_reformat_coast.rds"))  %>%
    dplyr::mutate(codreltrip=codrel/ntrips,
                  haddreltrip=haddrel/ntrips, 
                  codrelchoice=codrel/n_choice_occasions,
                  haddrelchoice=haddrel/n_choice_occasions, 
                  cod_hadd_keep=codkeep+haddkeep, 
                  cod_hadd_cat=codcat+haddcat)
  
  output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
  
  output_summarized2<-output_summarized1 %>% 
    dplyr::mutate(
      copula = ifelse(correlation == "independent", "independent", copula)
    )
  
  output_summarized_corr<-output_summarized1 %>% 
    dplyr::filter(correlation=="corr") %>% 
    dplyr::filter(decade!=0) 
  
  output_summarized_ind<-output_summarized1 %>% 
    dplyr::filter(correlation=="independent" ) %>% 
    dplyr::filter(decade!=0) 
  
  cvtrip_diff_corr <- output_summarized_corr %>%
    rename_with(~ paste0(.x, "_corr"), c(cvtrip, cv_choice, cv, ntrips)) %>%
    dplyr::select("draw", "copula","decade","cvtrip_corr", "cv_choice_corr", "cv_corr", "ntrips_corr") %>%     dplyr::filter(decade!=0 )
  
  cvtrip_diff_ind <- output_summarized_ind %>%
    rename_with(~ paste0(.x, "_ind"), c(cvtrip, cv_choice, cv, ntrips)) %>%
    dplyr::select("draw", "copula","decade","cvtrip_ind", "cv_choice_ind", "cv_ind", "ntrips_ind") %>%     dplyr::filter(decade!=0 )
  
  cvtrip_diff<-cvtrip_diff_corr %>% 
    left_join(cvtrip_diff_ind, by=c("draw", "copula","decade" )) %>%
    mutate(diff_ntrips=ntrips_ind-ntrips_corr, 
           diff_cvtrip=cvtrip_ind-cvtrip_corr , 
           diff_cv_choice=cv_choice_ind-cv_choice_corr , 
           diff_cv=cv_ind-cv_corr )
  
  
  cvtrip_diff$base_year<-yr
  
  cvtrip_diff_list[[yr]]<-cvtrip_diff
  
  cv_pct<-  output_summarized2 %>% 
    dplyr::filter(decade!=0) %>% 
    dplyr::select("draw", "copula","decade","cv") %>% 
    dplyr::left_join(closure, by="draw") %>% 
    dplyr::mutate(cv_pct_total_value=(cv/cv_closure)*100) %>% 
    dplyr::mutate(base_year=yr)
  
  cv_pct_list[[yr]]<-cv_pct
  
  
  
  #differences data catch
  diff_corr <- output_summarized_corr %>%
    rename_with(~ paste0(.x, "_corr"), c(codkeep, haddkeep, codcat, haddcat, cv,
                                         cod_hadd_keep, cod_hadd_cat, cod_tot_mort, hadd_tot_mort)) %>%
    dplyr::select("draw", "copula","decade","codkeep_corr", "haddkeep_corr", "cv_corr",
                  "codcat_corr", "haddcat_corr", "cod_hadd_keep_corr", "cod_hadd_cat_corr", 
                  "cod_tot_mort_corr", "hadd_tot_mort_corr") %>%   
    dplyr::filter(decade!=0 )
  
  diff_ind <- output_summarized_ind %>%
    rename_with(~ paste0(.x, "_ind"), c(codkeep, haddkeep, codcat, haddcat, cv,
                                        cod_hadd_keep, cod_hadd_cat, cod_tot_mort, hadd_tot_mort)) %>%
    dplyr::select("draw", "copula","decade","codkeep_ind", "haddkeep_ind", "cv_ind",
                  "codcat_ind", "haddcat_ind", "cod_hadd_keep_ind", "cod_hadd_cat_ind", 
                  "cod_tot_mort_ind", "hadd_tot_mort_ind") %>%   
    dplyr::filter(decade!=0 )
  
  diff<-diff_corr %>% 
    left_join(diff_ind, by=c("draw", "copula","decade" )) %>%
    mutate(
      # Absolute Difference: Difference = Ind - Corr
      diff_cv           = cv_ind - cv_corr,
      diff_codkeep      = codkeep_ind - codkeep_corr,
      diff_haddkeep     = haddkeep_ind - haddkeep_corr,
      diff_codcat       = codcat_ind - codcat_corr,
      diff_haddcat      = haddcat_ind - haddcat_corr,
      diff_cod_hadd_keep = cod_hadd_keep_ind - cod_hadd_keep_corr,
      diff_cod_hadd_cat = cod_hadd_cat_ind - cod_hadd_cat_corr,
      diff_cod_mort     = cod_tot_mort_ind - cod_tot_mort_corr,
      diff_hadd_mort    = hadd_tot_mort_ind - hadd_tot_mort_corr,
      
      # Percentage Difference: Pct Diff = ((Ind - Corr) / Corr) * 100
      pct_diff_codkeep   = ((codkeep_ind - codkeep_corr) / codkeep_corr) * 100,
      pct_diff_haddkeep  = ((haddkeep_ind - haddkeep_corr) / haddkeep_corr) * 100,
      pct_diff_codcat    = ((codcat_ind - codcat_corr) / codcat_corr) * 100,
      pct_diff_haddcat   = ((haddcat_ind - haddcat_corr) / haddcat_corr) * 100,
      pct_diff_cod_hadd_keep = ((cod_hadd_keep_ind - cod_hadd_keep_corr) / cod_hadd_keep_corr) * 100,
      pct_diff_cod_hadd_cat  = ((cod_hadd_cat_ind - cod_hadd_cat_corr) / cod_hadd_cat_corr) * 100,
      pct_diff_cod_mort  = ((cod_tot_mort_ind - cod_tot_mort_corr) / cod_tot_mort_corr) * 100,
      pct_diff_hadd_mort = ((hadd_tot_mort_ind - hadd_tot_mort_corr) / hadd_tot_mort_corr) * 100,
      
      # Other variable
      base_year = yr)
  
  diff_list[[yr]]<-diff
  
}

k_tau_data_combined_coast <- dplyr::bind_rows(k_tau_data_combined_coast_list)
k_tau_data_combined_state <- dplyr::bind_rows(k_tau_data_combined_state_list)
k_tau_data_combined_mode <- dplyr::bind_rows(k_tau_data_combined_mode_list)
diff_combined <- dplyr::bind_rows(diff_list)
plot_data_combined <- dplyr::bind_rows(plot_data_list)
cvtrip_diff_combined <- dplyr::bind_rows(cvtrip_diff_list)
cv_pct_combined <- dplyr::bind_rows(cv_pct_list)
output_summarized1_combined<-dplyr::bind_rows(output_summarized1_list)

cv_pct_combined_filtered<-cv_pct_combined %>% 
  dplyr::filter(copula!="independent") %>% 
  dplyr::select(copula, draw, decade, cv_closure, base_year)

cvtrip_diff_combined<-cvtrip_diff_combined %>% 
  dplyr::left_join(cv_pct_combined_filtered, by=c("copula", "draw", "decade", "base_year")) %>% 
  dplyr::mutate(diff_cv_total_cv=(diff_cv/cv_closure)*100, 
                cv_ind_total_cv=(cv_ind/cv_closure)*100, 
                cv_corr_total_cv=(cv_corr/cv_closure)*100) 

#ntrips base_years
ntrips<-output_summarized1_combined %>% 
  dplyr::filter(copula=="observed") %>% 
  dplyr::select(draw, base_year, ntrips) %>% 
  dplyr::rename(ntrips_base=ntrips)
#merge to trip differences

cvtrip_diff_combined<-cvtrip_diff_combined %>% 
  dplyr::left_join(ntrips, by=c("draw",  "base_year")) %>% 
  dplyr::mutate(diff_trips_base_trips=(diff_ntrips/ntrips_base)*100) %>% 
  dplyr::mutate(pct_diff_ntrips_corr=((diff_ntrips)/ntrips_corr)*100)




#######################
#######################
#PLOTS by mode

# 1. Define the Custom Labeller Function
# This function takes the original variable name (labels) and returns the new desired labels.
mode_labeller <- function(variable, value) {
  # 'value' contains the unique levels of the faceting variable (e.g., "fh", "pr")
  
  # Create a named vector mapping the old values to the new display names
  new_labels <- c(
    "fh" = "Party & Charter", 
    "pr" = "Private",
    # Add any other mode values you have and their labels here (e.g., "sh" = "Shore")
    "sh" = "Shore" 
  )
  
  # Look up the new labels based on the old values
  return(new_labels[value])
}

output2_list<-list()
k=1
yrz<- c(2019, 2020, 2021)
for (y in yrz){
  projection_temps<-historical_and_projections %>% 
    dplyr::filter(is.na(year))
  
  
  output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", y,"_outliers_9-21-25_reformat_mode.rds"))  %>%
    dplyr::mutate(codreltrip=codrel/ntrips,
                  haddreltrip=haddrel/ntrips, 
                  codrelchoice=codrel/n_choice_occasions,
                  haddrelchoice=haddrel/n_choice_occasions, 
                  cod_hadd_keep=codkeep+haddkeep, 
                  cod_hadd_cat=codcat+haddcat)
  
  output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
  output_summarized1$copula[output_summarized1$correlation=="independent"] <- "independent"
  
  output2<-output_summarized1 %>%
    dplyr::filter(!is.na(decade))  %>%
    dplyr::filter(decade!=0)  %>%
    dplyr::mutate(decade = dplyr::recode(decade,
                                         `1` = "2021-2030",
                                         `2` = "2031-2040",
                                         `3` = "2041-2050",
                                         `4` = "2051-2060",
                                         `5` = "2061-2070",
                                         `6` = "2071-2080",
                                         `7` = "2081-2090",
                                         `8` = "2091-3000"
    )) 
  
  df_subset <- output2 %>%
    # keep only the independent/independent rows
    dplyr::filter(copula == "independent", correlation == "independent") %>%
    # stratified sample: 100 per decade
    dplyr::group_by(decade) %>%
    dplyr::slice_sample(n = 100) %>%
    dplyr::ungroup()
  
  output2<-output2 %>% 
    dplyr::filter(copula!="independent") 
  
  output2<-output2 %>% 
    plyr::rbind.fill(df_subset) %>% 
    dplyr::select(-year) %>% 
    dplyr::mutate(codkeep=codkeep/1000, haddkeep=haddkeep/1000, cv=cv/1000000,
                  codcat=codcat/1000, haddcat=haddcat/1000,base_year=y)
  
  
  library(ggplot2)
  library(patchwork)
  
  # Reorder factor levels so "independent" is first
  output2$copula <- factor(output2$copula,
                           levels = c("independent", setdiff(unique(output2$copula), "independent")))
  
  output2_list[[k]]<-output2
  k=k+1
  
  # Shared color scale: no title, single row legend
  copula_colors <- scale_color_discrete(
    name = NULL,
    guide = guide_legend(nrow = 1)
  )
}
output2_allyrs <- dplyr::bind_rows(output2_list)

# Check if 0 is between the minimum lower bound and the maximum upper bound
zero_line_layer <- if (min(output2$cv) < 0 && max(output2$cv) > 0) {
  # Add the line if 0 is within the range. Using a light grey and dashed line.
  geom_hline(yintercept = 0, color = "grey40", size = 0.2)
} else {
  # Otherwise, return NULL, which ggplot will ignore.
  NULL
}

# 4. Compensating variation
output2_all2019<-output2_allyrs %>% dplyr::filter(base_year==2019) %>% 
  dplyr::mutate(base_year = "Baseline Year = 2019")


output2_all2020<-output2_allyrs %>% dplyr::filter(base_year==2020) %>% 
  dplyr::mutate(base_year = "Baseline Year = 2020")


output2_all2021<-output2_allyrs %>% dplyr::filter(base_year==2021) %>% 
  dplyr::mutate(base_year = "Baseline Year = 2021")

# Define the common theme settings for centering the plot title
theme_centered_title <- theme(
  plot.title = element_text(
    size = 11, 
    hjust = 0.5, # Centers the plot title
    margin = margin(b = 10) 
  )
)

global_min_y <- floor(min(output2_allyrs$cv, na.rm = TRUE))
global_max_y <- ceiling(max(output2_allyrs$cv, na.rm = TRUE))

p1<- ggplot(output2_all2019, aes(x = decade, y = cv, color = factor(copula))) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  zero_line_layer +
  copula_colors +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank()
    
  )+
  scale_y_continuous(
    breaks = seq(global_min_y, global_max_y, by = 2),
    limits = c(global_min_y, global_max_y) # ADDED: Explicitly set limits
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Baseline Year = 2019" 
  ) +
  theme_centered_title + # Apply centered title theme
  facet_wrap(~ mode,  labeller = mode_labeller, ncol = 2)


p2<- ggplot(output2_all2020, aes(x = decade, y = cv, color = factor(copula))) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  zero_line_layer +
  copula_colors +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank(), 
    # NEW: Adjust size and margin for the spanning Y-axis title
    axis.title.y = element_text(size = 12, margin = margin(r = 15)))+
  scale_y_continuous(
    breaks = seq(global_min_y, global_max_y, by = 2),
    limits = c(global_min_y, global_max_y) # ADDED: Explicitly set limits
  ) +
  labs(
    x = NULL, 
    y = "Compensating variation ($M)",
    title = "Baseline Year = 2020" 
  ) +
  theme_centered_title + # Apply centered title theme
  facet_wrap(~ mode,  labeller = mode_labeller, ncol = 2)


p3<- ggplot(output2_all2021, aes(x = decade, y = cv, color = factor(copula))) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  zero_line_layer +
  copula_colors +
  theme(
    axis.ticks.x  = element_blank(),
    axis.text.x=element_text( size=8),
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank()
    
  )+
  scale_y_continuous(
    breaks = seq(global_min_y, global_max_y, by = 2),
    limits = c(global_min_y, global_max_y) # ADDED: Explicitly set limits
  ) +
  labs(
    x = NULL, 
    y = NULL,
    title = "Baseline Year = 2021" 
  ) +
  theme_centered_title + # Apply centered title theme
  facet_wrap(~ mode,  labeller = mode_labeller, ncol = 2)


final_plot <- ( p1 / p2 / p3) +
  plot_layout(heights = c(1, 1, 1), guides = "collect") &
    theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key = element_blank()
  ) 


final_plot
ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/cv_by_mode_allyears.jpg"), plot = final_plot, width = 12, height = 8, units = "in")

#######################
#######################

#######################
#######################
#PLOTS by state

# 1. Define the Custom Labeller Function
# This function takes the original variable name (labels) and returns the new desired labels.

output2_list<-list()
k=1
yrz<- c(2019, 2020, 2021)
for (y in yrz){
  projection_temps<-historical_and_projections %>% 
    dplyr::filter(is.na(year))
  
  
  output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", y,"_outliers_9-21-25_reformat_state.rds"))  %>%
    dplyr::mutate(codreltrip=codrel/ntrips,
                  haddreltrip=haddrel/ntrips, 
                  codrelchoice=codrel/n_choice_occasions,
                  haddrelchoice=haddrel/n_choice_occasions, 
                  cod_hadd_keep=codkeep+haddkeep, 
                  cod_hadd_cat=codcat+haddcat)
  
  output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
  output_summarized1$copula[output_summarized1$correlation=="independent"] <- "independent"
  
  output2<-output_summarized1 %>%
    dplyr::filter(!is.na(decade))  %>%
    dplyr::filter(decade!=0)  %>%
    dplyr::mutate(decade = dplyr::recode(decade,
                                         `1` = "2021-2030",
                                         `2` = "2031-2040",
                                         `3` = "2041-2050",
                                         `4` = "2051-2060",
                                         `5` = "2061-2070",
                                         `6` = "2071-2080",
                                         `7` = "2081-2090",
                                         `8` = "2091-3000"
    )) %>%
    dplyr::mutate(state = dplyr::recode(state,
                                        `33` = "New Hampshire",
                                        `25` = "Massachusetts",
                                        `23` = "Maine")) 
  
  df_subset <- output2 %>%
    # keep only the independent/independent rows
    dplyr::filter(copula == "independent", correlation == "independent") %>%
    # stratified sample: 100 per decade
    dplyr::group_by(decade, state, correlation, copula) %>%
    dplyr::slice_sample(n = 100) %>%
    dplyr::ungroup()
  
  # df_subset %>%
  #   group_by(decade, state, correlation, copula) %>% # 1. Specify the variable(s) to group by
  #   summarize(
  #     Count = n()               # 2. Create a new column 'Count' using the n() function, which counts the number of rows in each group
  #   ) %>% print(n=24)
  
  output2<-output2 %>% 
    dplyr::filter(copula!="independent") 
  
  output2<-output2 %>% 
    plyr::rbind.fill(df_subset) %>% 
    dplyr::select(-year) %>% 
    dplyr::mutate(codkeep=codkeep/1000, haddkeep=haddkeep/1000, cv=cv/1000000,
                  codcat=codcat/1000, haddcat=haddcat/1000,base_year=y)
  
  
  library(ggplot2)
  library(patchwork)
  
  # Reorder factor levels so "independent" is first
  output2$copula <- factor(output2$copula,
                           levels = c("independent", setdiff(unique(output2$copula), "independent")))
  
  output2_list[[k]]<-output2
  k=k+1
  
  # Shared color scale: no title, single row legend
  copula_colors <- scale_color_discrete(
    name = NULL,
    guide = guide_legend(nrow = 1)
  )
}
output2_allyrs <- dplyr::bind_rows(output2_list)

# Check if 0 is between the minimum lower bound and the maximum upper bound
zero_line_layer <- if (min(output2$cv) < 0 && max(output2$cv) > 0) {
  # Add the line if 0 is within the range. Using a light grey and dashed line.
  geom_hline(yintercept = 0, color = "grey40", size = 0.2)
} else {
  # Otherwise, return NULL, which ggplot will ignore.
  NULL
}

# 4. Compensating variation
output2_all2019<-output2_allyrs %>% dplyr::filter(base_year==2019) %>% 
  dplyr::mutate(base_year = "Baseline Year = 2019")


output2_all2020<-output2_allyrs %>% dplyr::filter(base_year==2020) %>% 
  dplyr::mutate(base_year = "Baseline Year = 2020")


output2_all2021<-output2_allyrs %>% dplyr::filter(base_year==2021) %>% 
  dplyr::mutate(base_year = "Baseline Year = 2021")

# Define the common theme settings for centering the plot title
theme_centered_title <- theme(
  plot.title = element_text(
    size = 11, 
    hjust = 0.5, # Centers the plot title
    margin = margin(b = 10) 
  )
)

global_min_y <- floor(min(output2_allyrs$cv, na.rm = TRUE))
global_max_y <- ceiling(max(output2_allyrs$cv, na.rm = TRUE))

p1<- ggplot(output2_all2019, aes(x = decade, y = cv, color = factor(copula))) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  zero_line_layer +
  copula_colors +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank()
    
  )+
  scale_y_continuous(
    breaks = seq(global_min_y, global_max_y, by = 2),
    limits = c(global_min_y, global_max_y) # ADDED: Explicitly set limits
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Baseline Year = 2019" 
  ) +
  theme_centered_title + # Apply centered title theme
  facet_wrap(~ state,  ncol = 3)


p2<- ggplot(output2_all2020, aes(x = decade, y = cv, color = factor(copula))) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  zero_line_layer +
  copula_colors +
  theme(
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank(), 
    # NEW: Adjust size and margin for the spanning Y-axis title
    axis.title.y = element_text(size = 12, margin = margin(r = 15)))+
  scale_y_continuous(
    breaks = seq(global_min_y, global_max_y, by = 2),
    limits = c(global_min_y, global_max_y) # ADDED: Explicitly set limits
  ) +
  labs(
    x = NULL, 
    y = "Compensating variation ($M)",
    title = "Baseline Year = 2020" 
  ) +
  theme_centered_title + # Apply centered title theme
  facet_wrap(~ state,  ncol = 3)


p3<- ggplot(output2_all2021, aes(x = decade, y = cv, color = factor(copula))) +
  geom_boxplot(width=w, size = sz, outlier.size = osz) +
  zero_line_layer +
  copula_colors +
  theme(
    axis.ticks.x  = element_blank(),
    axis.text.x=element_text(angle=45,  size=7, hjust=1),
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
    # Remove minor vertical grid lines (x-axis)
    panel.grid.minor.x = element_blank()
    
  )+
  scale_y_continuous(
    breaks = seq(global_min_y, global_max_y, by = 2),
    limits = c(global_min_y, global_max_y) # ADDED: Explicitly set limits
  ) +
  labs(
    x = NULL, 
    y = NULL,
    title = "Baseline Year = 2021" 
  ) +
  theme_centered_title + # Apply centered title theme
  facet_wrap(~ state,  ncol = 3)


final_plot <- ( p1 / p2 / p3) +
  plot_layout(heights = c(1, 1, 1), guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key = element_blank()
  ) 


final_plot
ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/cv_by_state_allyears.jpg"), plot = final_plot, width = 12, height = 8, units = "in")

#######################
#######################


###Plots by state

# 1. Define the Custom Labeller Function
# This function takes the original variable name (labels) and returns the new desired labels.
mode_labeller <- function(variable, value) {
  # 'value' contains the unique levels of the faceting variable (e.g., "fh", "pr")
  
  # Create a named vector mapping the old values to the new display names
  new_labels <- c(
    "fh" = "Party & Charter", 
    "pr" = "Private",
    # Add any other mode values you have and their labels here (e.g., "sh" = "Shore")
    "sh" = "Shore" 
  )
  
  # Look up the new labels based on the old values
  return(new_labels[value])
}

yrz<- c(2019, 2020, 2021)
for (y in yrz){
  projection_temps<-historical_and_projections %>% 
    dplyr::filter(is.na(year))
  
  
  output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", y,"_outliers_9-21-25_reformat_state.rds"))  %>%
    dplyr::mutate(codreltrip=codrel/ntrips,
                  haddreltrip=haddrel/ntrips, 
                  codrelchoice=codrel/n_choice_occasions,
                  haddrelchoice=haddrel/n_choice_occasions, 
                  cod_hadd_keep=codkeep+haddkeep, 
                  cod_hadd_cat=codcat+haddcat)
  
  output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
  output_summarized1$copula[output_summarized1$correlation=="independent"] <- "independent"
  
  output2<-output_summarized1 %>%
    dplyr::filter(!is.na(decade))  %>%
    dplyr::filter(decade!=0)  %>%
    dplyr::mutate(decade = dplyr::recode(decade,
                                         `1` = "2021-2030",
                                         `2` = "2031-2040",
                                         `3` = "2041-2050",
                                         `4` = "2051-2060",
                                         `5` = "2061-2070",
                                         `6` = "2071-2080",
                                         `7` = "2081-2090",
                                         `8` = "2091-3000"
    )) %>%
    dplyr::mutate(state = dplyr::recode(state,
                                         `33` = "New Hampshire",
                                         `25` = "Massachusetts",
                                         `23` = "Maine")) 

  
  df_subset <- output2 %>%
    # keep only the independent/independent rows
    dplyr::filter(copula == "independent", correlation == "independent") %>%
    # stratified sample: 100 per decade
    dplyr::group_by(decade) %>%
    dplyr::slice_sample(n = 100) %>%
    dplyr::ungroup()
  
  output2<-output2 %>% 
    dplyr::filter(copula!="independent") 
  
  output2<-output2 %>% 
    plyr::rbind.fill(df_subset) %>% 
    dplyr::select(-year) %>% 
    dplyr::mutate(codkeep=codkeep/1000, haddkeep=haddkeep/1000, cv=cv/1000000,
                  codcat=codcat/1000, haddcat=haddcat/1000,)
  
  
  
  library(ggplot2)
  library(patchwork)
  
  # Reorder factor levels so "independent" is first
  output2$copula <- factor(output2$copula,
                           levels = c("independent", setdiff(unique(output2$copula), "independent")))
  
  # Shared color scale: no title, single row legend
  copula_colors <- scale_color_discrete(
    name = NULL,
    guide = guide_legend(nrow = 1)
  )
  
  
  # 2. Cod harvest
  p2 <-   ggplot(output2, aes(x=decade, y=codkeep, color = copula))+
    geom_boxplot() +
    labs(x = NULL, y = "Cod harvest\n('000s)") +
    copula_colors +
    theme(
      axis.text.x   = element_blank(),
      axis.ticks.x  = element_blank(),
      legend.position = "none",
      # Remove major vertical grid lines (x-axis)
      panel.grid.major.x = element_blank(),
      # Remove minor vertical grid lines (x-axis)
      panel.grid.minor.x = element_blank()
    )+
    facet_wrap(~ state,  ncol = 3)
  
  
  # 3. Haddock harvest
  p3 <-   ggplot(output2, aes(x=decade, y=haddkeep, color = copula))+
    geom_boxplot() +
    labs(x = NULL, y = "Haddock harvest\n('000s)") +
    copula_colors +
    theme(
      axis.text.x   = element_blank(),
      axis.ticks.x  = element_blank(),
      legend.position = "none",
      # Remove major vertical grid lines (x-axis)
      panel.grid.major.x = element_blank(),
      # Remove minor vertical grid lines (x-axis)
      panel.grid.minor.x = element_blank()
    )+
    facet_wrap(~ state,  ncol = 3)
  
  
  # 4. Compensating variation
  p4 <-  ggplot(output2, aes(x=decade, y=cv, color = copula))+
    geom_boxplot() +
    labs(x=NULL,  y = "Compensating\nvariation ($M)") +
    copula_colors +
    theme(
      #axis.text.x   = element_blank(),
      axis.text.x=element_text(angle = 45, hjust = 1),
      # Remove major vertical grid lines (x-axis)
      panel.grid.major.x = element_blank(),
      # Remove minor vertical grid lines (x-axis)
      panel.grid.minor.x = element_blank()
      
    )+
    scale_y_continuous(breaks = seq(
      floor(min(output2$cv, na.rm = TRUE)), 
      ceiling(max(output2$cv, na.rm = TRUE)), 
      by = 1   # tick every 1 unit
    )) +
    facet_wrap(~ state,  ncol = 3)
  
  # Combine plots with shared legend
  final_plot <- ( p2 / p3 / p4) +
    plot_layout(heights = c(1, 1, 1), guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      axis.title.y = element_text(size = 9), 
      legend.key = element_blank()
    )
  
  final_plot
  ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/havest_cv_plot_st", y,".jpg"), plot = final_plot,  width = 14, height = 8, units = "in")
  
###############
###############
  
  
  
  
  
  
#plot 2 
  # 1. Cod catch
  p1 <-   ggplot(output2, aes(x=decade, y=codcattrip, color = copula))+
    geom_boxplot() +
    labs(x = NULL, y = " \nCod catch-per-trip\n ") +
    copula_colors +
    theme(
      axis.text.x   = element_blank(),
      axis.ticks.x  = element_blank(),
      legend.position = "none",
      # Remove major vertical grid lines (x-axis)
      panel.grid.major.x = element_blank(),
      # Remove minor vertical grid lines (x-axis)
      panel.grid.minor.x = element_blank()
    )+
    facet_wrap(~ state,  ncol = 3)
  
  
  # Haddock catch
  p2 <-   ggplot(output2, aes(x=decade, y=haddcattrip, color = copula))+
    geom_boxplot() +
    labs(x = NULL, y = " \nHaddock catch-per-trip\n ") +
    copula_colors +
    theme(
      axis.text.x   = element_blank(),
      axis.ticks.x  = element_blank(),
      legend.position = "none",
      # Remove major vertical grid lines (x-axis)
      panel.grid.major.x = element_blank(),
      # Remove minor vertical grid lines (x-axis)
      panel.grid.minor.x = element_blank()
    )+
    facet_wrap(~ state,  ncol = 3)
  
  
  # 2. k-tau catch 
  ktau_yr<-k_tau_data_combined_state %>% 
    dplyr::filter(base_year==y)
  
  ktau_subset <- ktau_yr %>%
    # keep only the independent/independent rows
    dplyr::filter(copula == "independent", correlation == "ind") %>%
    # stratified sample: 100 per decade
    dplyr::group_by(state, decade) %>%
    dplyr::slice_sample(n = 100) %>%
    dplyr::ungroup()
  
  ktau_yr<-ktau_yr %>% 
    dplyr::filter(copula!="independent") 
  
  ktau_yr<-ktau_yr %>% 
    plyr::rbind.fill(ktau_subset) %>% 
    dplyr::mutate(decade = dplyr::recode(decade,
                                         `1` = "2021-2030",
                                         `2` = "2031-2040",
                                         `3` = "2041-2050",
                                         `4` = "2051-2060",
                                         `5` = "2061-2070",
                                         `6` = "2071-2080",
                                         `7` = "2081-2090",
                                         `8` = "2091-3000"
    )) %>%
    dplyr::mutate(state = dplyr::recode(state,
                                        `NH` = "New Hampshire",
                                        `MA` = "Massachusetts",
                                        `ME` = "Maine")) 
  # ktau_yr %>%
  #   group_by(correlation, copula, mode, decade) %>%
  #   summarize(Count = n()) %>% 
  #   print(n = 96)
  
  y_min <- floor(min(ktau_yr$k_tau_catch_est, ktau_yr$k_tau_keep_est, na.rm = TRUE)*10)/ 10
  y_max <- ceiling(max(ktau_yr$k_tau_catch_est, ktau_yr$k_tau_keep_est, na.rm = TRUE)*10)/ 10
  
  # 2. Define the common breaks sequence
  y_breaks <- seq(y_min, y_max, by = 0.05)
  
  
  p3 <- ggplot(ktau_yr, aes(x=decade, y = k_tau_catch_est, color = factor(copula))) +
    geom_boxplot() +
    labs(x = NULL, y = "Correlation in\ncatch-per-trip\n(Kendall's tau)") +
    copula_colors +
    # Add the horizontal line at y = 0
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
    theme(
      axis.text.x   = element_blank(),
      axis.ticks.x  = element_blank(),
      legend.position = "none", 
      # Remove major vertical grid lines (x-axis)
      panel.grid.major.x = element_blank(),
      # Remove minor vertical grid lines (x-axis)
      panel.grid.minor.x = element_blank()) +
    #Apply common limits and breaks
    scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = y_breaks
    )+
    facet_wrap(~ state,  ncol = 3)
  
  # 2. k-tau harvest 
  p4 <- ggplot(ktau_yr, aes(x=decade, y = k_tau_keep_est, color = copula)) +
    geom_boxplot() +
    facet_wrap(~decade, nrow = 1) +
    labs(x = NULL, y = "Correlation in\n harvest-per-trip\n(Kendall's tau)") +
    copula_colors +
    # Add the horizontal line at y = 0
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
    theme(
      #axis.text.x   = element_blank(),
      axis.text.x=element_text(angle = 45, hjust = 1),
      # Remove major vertical grid lines (x-axis)
      panel.grid.major.x = element_blank(),
      # Remove minor vertical grid lines (x-axis)
      panel.grid.minor.x = element_blank()) +
    # Apply common limits and breaks
    scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = y_breaks
    )+
    facet_wrap(~ state,  ncol = 3)
  
  # Combine plots with shared legend
  final_plot <- (p1 / p2 / p3 / p4) +
    plot_layout(heights = c(1, 1, 1, 1), guides = "collect") &
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      axis.title.y = element_text(size = 9), 
      legend.key = element_blank()
    )
  
  final_plot
  
  ggsave(paste0("C:/Users/andrew.carr-harris/Desktop/Git/welfare-model-GoM/newest_version/output_data/figures/ktau_plot_st", y,".jpg"), plot = final_plot, width = 14, height = 8, units = "in")
}


  
# Range of changes in pct welfare across years
  # min(cv_pct_combined$cv_pct_total_value)
  # max(cv_pct_combined$cv_pct_total_value)
  # 
  # min(cv_pct_combined$cv_pct_total_value[cv_pct_combined$base_year == 2019])
  # max(cv_pct_combined$cv_pct_total_value[cv_pct_combined$base_year == 2019])
  # 
  # min(cv_pct_combined$cv_pct_total_value[cv_pct_combined$base_year == 2020])
  # max(cv_pct_combined$cv_pct_total_value[cv_pct_combined$base_year == 2020])
  # 
  # min(cv_pct_combined$cv_pct_total_value[cv_pct_combined$base_year == 2021])
  # max(cv_pct_combined$cv_pct_total_value[cv_pct_combined$base_year == 2021])

# Average pct_change in welfare 
# min(cv_pct_combined$cv_pct_total_value)
cv_pct_combined_ind<-cv_pct_combined %>% 
  dplyr::filter(copula=="independent") %>%
  dplyr::group_by(copula, draw, decade, base_year) %>% 
  dplyr::slice_sample(n = 1) %>%
  dplyr::ungroup()

cv_pct_combined<-cv_pct_combined %>% 
  dplyr::filter(copula!="independent") %>%
  dplyr::bind_rows(cv_pct_combined_ind)
  
summary_stats <- cv_pct_combined %>%
  # 1. Specify the groups for which you want separate calculations
  #group_by(Group) %>%
  # 2. Compute the statistics for each group
  summarise(
    Median_Value = median(cv, na.rm = TRUE),
    IQR_Value    = IQR(cv, na.rm = TRUE),
    Q1           = quantile(cv, 0.25, na.rm = TRUE),
    Q3           = quantile(cv, 0.75, na.rm = TRUE),
    .groups = 'drop' # Recommended to ungroup after summarising
  )

print(summary_stats)

summary_stats <- cv_pct_combined %>%
  # 1. Specify the groups for which you want separate calculations
  #group_by(Group) %>%
  # 2. Compute the statistics for each group
  summarise(
    Median_Value = median(cv_closure, na.rm = TRUE),
    IQR_Value    = IQR(cv_closure),
    Q1           = quantile(cv_closure),
    Q3           = quantile(cv_closure, 0.75, na.rm = TRUE),
    .groups = 'drop' # Recommended to ungroup after summarising
  )

print(summary_stats)

summary_stats <- cv_pct_combined %>%
  # 1. Specify the groups for which you want separate calculations
  #group_by(Group) %>%
  # 2. Compute the statistics for each group
  summarise(
    Median_Value = median(cv_pct_total_value, na.rm = TRUE),
    IQR_Value    = IQR(cv_pct_total_value, na.rm = TRUE),
    Q1           = quantile(cv_pct_total_value, 0.25, na.rm = TRUE),
    Q3           = quantile(cv_pct_total_value, 0.75, na.rm = TRUE),
    .groups = 'drop' # Recommended to ungroup after summarising
  )

print(summary_stats)


# Degree of bias from ignoring unconditional catch
  # Difference in cv over total value 
  min(cvtrip_diff_combined$diff_cv_total_cv[cvtrip_diff_combined$base_year == 2019])
  max(cvtrip_diff_combined$diff_cv_total_cv[cvtrip_diff_combined$base_year == 2019])

  min(cvtrip_diff_combined$diff_cv_total_cv[cvtrip_diff_combined$base_year == 2020])
  max(cvtrip_diff_combined$diff_cv_total_cv[cvtrip_diff_combined$base_year == 2020])

  min(cvtrip_diff_combined$diff_cv_total_cv[cvtrip_diff_combined$base_year == 2021])
  max(cvtrip_diff_combined$diff_cv_total_cv[cvtrip_diff_combined$base_year == 2021])

  mean(cvtrip_diff_combined$diff_cv_total_cv[cvtrip_diff_combined$base_year == 2019])
  mean(cvtrip_diff_combined$diff_cv_total_cv[cvtrip_diff_combined$base_year == 2020])
  mean(cvtrip_diff_combined$diff_cv_total_cv[cvtrip_diff_combined$base_year == 2021])

  summary_stats <- cvtrip_diff_combined %>%
    # 1. Specify the groups for which you want separate calculations
    #group_by(Group) %>%
    # 2. Compute the statistics for each group
    summarise(
      Median_Value = median(diff_cv_total_cv, na.rm = TRUE),
      IQR_Value    = IQR(diff_cv_total_cv, na.rm = TRUE),
      Q1           = quantile(diff_cv_total_cv, 0.25, na.rm = TRUE),
      Q3           = quantile(diff_cv_total_cv, 0.75, na.rm = TRUE),
      .groups = 'drop' # Recommended to ungroup after summarising
    )
  
  print(summary_stats)
  
  
  # cv 8th decade baseline year 2021 ind vs corr 
  cvtrip_diff_combined<-cvtrip_diff_combined %>% 
    dplyr::filter(base_year==2021 & decade==8)
  
  summary_stats <- cvtrip_diff_combined %>%
    # 1. Specify the groups for which you want separate calculations
    #group_by(Group) %>%
    # 2. Compute the statistics for each group
    summarise(
      Median_Value = median(cv_ind, na.rm = TRUE),
      IQR_Value    = IQR(cv_ind, na.rm = TRUE),
      Q1           = quantile(cv_ind, 0.25, na.rm = TRUE),
      Q3           = quantile(cv_ind, 0.75, na.rm = TRUE),
      .groups = 'drop' # Recommended to ungroup after summarising
    )
  
  print(summary_stats)
  
  summary_stats <- cvtrip_diff_combined %>%
    # 1. Specify the groups for which you want separate calculations
    #group_by(Group) %>%
    # 2. Compute the statistics for each group
    summarise(
      Median_Value = median(cv_ind_total_cv, na.rm = TRUE),
      IQR_Value    = IQR(cv_ind_total_cv, na.rm = TRUE),
      Q1           = quantile(cv_ind_total_cv, 0.25, na.rm = TRUE),
      Q3           = quantile(cv_ind_total_cv, 0.75, na.rm = TRUE),
      .groups = 'drop' # Recommended to ungroup after summarising
    )
  
  print(summary_stats)
  
  summary_stats <- cvtrip_diff_combined %>%
    # 1. Specify the groups for which you want separate calculations
    #group_by(Group) %>%
    # 2. Compute the statistics for each group
    summarise(
      Median_Value = median(cv_corr, na.rm = TRUE),
      IQR_Value    = IQR(cv_corr, na.rm = TRUE),
      Q1           = quantile(cv_corr, 0.25, na.rm = TRUE),
      Q3           = quantile(cv_corr, 0.75, na.rm = TRUE),
      .groups = 'drop' # Recommended to ungroup after summarising
    )
  
  print(summary_stats)
  
  summary_stats <- cvtrip_diff_combined %>%
    # 1. Specify the groups for which you want separate calculations
    #group_by(Group) %>%
    # 2. Compute the statistics for each group
    summarise(
      Median_Value = median(cv_corr_total_cv, na.rm = TRUE),
      IQR_Value    = IQR(cv_corr_total_cv, na.rm = TRUE),
      Q1           = quantile(cv_corr_total_cv, 0.25, na.rm = TRUE),
      Q3           = quantile(cv_corr_total_cv, 0.75, na.rm = TRUE),
      .groups = 'drop' # Recommended to ungroup after summarising
    )
  
  print(summary_stats)
  
  #difference in ntrips compared to base year 
  mean(cvtrip_diff_combined$diff_trips_base_trips[cvtrip_diff_combined$base_year == 2019])
  mean(cvtrip_diff_combined$diff_trips_base_trips[cvtrip_diff_combined$base_year == 2020])
  mean(cvtrip_diff_combined$diff_trips_base_trips[cvtrip_diff_combined$base_year == 2021])
  
  summary_stats <- cvtrip_diff_combined %>%
    # 1. Specify the groups for which you want separate calculations
    #group_by(Group) %>%
    # 2. Compute the statistics for each group
    summarise(
      Median_Value = median(pct_diff_ntrips_corr, na.rm = TRUE),
      IQR_Value    = IQR(pct_diff_ntrips_corr, na.rm = TRUE),
      Q1           = quantile(pct_diff_ntrips_corr, 0.25, na.rm = TRUE),
      Q3           = quantile(pct_diff_ntrips_corr, 0.75, na.rm = TRUE),
      .groups = 'drop' # Recommended to ungroup after summarising
    )
  
  print(summary_stats)
  
  #difference in ntrips compared to ind
  mean(cvtrip_diff_combined$pct_diff_ntrips_ind[cvtrip_diff_combined$base_year == 2019])
  mean(cvtrip_diff_combined$pct_diff_ntrips_ind[cvtrip_diff_combined$base_year == 2020])
  mean(cvtrip_diff_combined$pct_diff_ntrips_ind[cvtrip_diff_combined$base_year == 2021])
  
  #difference in haddock harverst compared to ind
  mean(diff_combined$pct_diff_haddkeep[diff_combined$base_year == 2019])
  mean(diff_combined$pct_diff_haddkeep[diff_combined$base_year == 2020])
  mean(diff_combined$pct_diff_haddkeep[diff_combined$base_year == 2021])
  
  # welfare loss, and pct total value, 8th decade, indepependent
  mean(cvtrip_diff_combined$cv_ind[cvtrip_diff_combined$base_year == 2019 & cvtrip_diff_combined$decade == 8])
  mean(cvtrip_diff_combined$cv_ind_total_cv[cvtrip_diff_combined$base_year == 2019 & cvtrip_diff_combined$decade == 8])
  
  mean(cvtrip_diff_combined$cv_ind[cvtrip_diff_combined$base_year == 2020 & cvtrip_diff_combined$decade == 8])
  mean(cvtrip_diff_combined$cv_ind_total_cv[cvtrip_diff_combined$base_year == 2020 & cvtrip_diff_combined$decade == 8])
  
  mean(cvtrip_diff_combined$cv_ind[cvtrip_diff_combined$base_year == 2021 & cvtrip_diff_combined$decade == 8])
  mean(cvtrip_diff_combined$cv_ind_total_cv[cvtrip_diff_combined$base_year == 2021 & cvtrip_diff_combined$decade == 8])
  
  
  # welfare loss, and pct total value, 8th decade, correlated
  mean(cvtrip_diff_combined$cv_corr[cvtrip_diff_combined$base_year == 2019 & cvtrip_diff_combined$decade == 8])
  mean(cvtrip_diff_combined$cv_corr_total_cv[cvtrip_diff_combined$base_year == 2019 & cvtrip_diff_combined$decade == 8])
  
  mean(cvtrip_diff_combined$cv_corr[cvtrip_diff_combined$base_year == 2020 & cvtrip_diff_combined$decade == 8])
  mean(cvtrip_diff_combined$cv_corr_total_cv[cvtrip_diff_combined$base_year == 2020 & cvtrip_diff_combined$decade == 8])
  
  mean(cvtrip_diff_combined$cv_corr[cvtrip_diff_combined$base_year == 2021 & cvtrip_diff_combined$decade == 8])
  mean(cvtrip_diff_combined$cv_corr_total_cv[cvtrip_diff_combined$base_year == 2021 & cvtrip_diff_combined$decade == 8])
  
  # pct differences in harvest
  summary_stats <- diff_combined %>%
    # 1. Specify the groups for which you want separate calculations
    #group_by(Group) %>%
    # 2. Compute the statistics for each group
    summarise(
      Median_Value = median(pct_diff_haddkeep, na.rm = TRUE),
      IQR_Value    = IQR(pct_diff_haddkeep, na.rm = TRUE),
      Q1           = quantile(pct_diff_haddkeep, 0.25, na.rm = TRUE),
      Q3           = quantile(pct_diff_haddkeep, 0.75, na.rm = TRUE),
      .groups = 'drop' # Recommended to ungroup after summarising
    )
  
  print(summary_stats)
  
  
  
  
  
  ###plots by mode
  # Pull in all the data and reformat for plotting 
  output_summarized1_mode_list<-list()
  plot_data_mode_list<-list()
  cvtrip_diff_mode_list<-list()
  diff_mode_list<-list()
  
  years<-c(2019, 2020, 2021)
  for(yr in years){
    
    #plot data 
    output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y",yr,"_outliers_9-21-25_reformat_mode.rds"))  %>%
      dplyr::mutate(codreltrip=codrel/ntrips,
                    haddreltrip=haddrel/ntrips, 
                    codrelchoice=codrel/n_choice_occasions,
                    haddrelchoice=haddrel/n_choice_occasions, 
                    cod_hadd_keep=codkeep+haddkeep, 
                    cod_hadd_cat=codcat+haddcat)
    
    output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
    output_summarized1$copula[output_summarized1$correlation=="independent"] <- "independent"
    output_summarized1$base_year<-yr
    output_summarized1_mode_list[[yr]]<-output_summarized1
    
    plot_data<-output_summarized1 %>% 
      dplyr::filter(decade!=0)
    
    plot_data$base_year<-yr
    plot_data_mode_list[[yr]]<-plot_data
    
    #differences data CV
    output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", yr,"_outliers_9-21-25_reformat_mode.rds"))  %>%
      dplyr::mutate(codreltrip=codrel/ntrips,
                    haddreltrip=haddrel/ntrips, 
                    codrelchoice=codrel/n_choice_occasions,
                    haddrelchoice=haddrel/n_choice_occasions, 
                    cod_hadd_keep=codkeep+haddkeep, 
                    cod_hadd_cat=codcat+haddcat)
    
    output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
    
    output_summarized_corr<-output_summarized1 %>% 
      dplyr::filter(correlation=="corr") %>% 
      dplyr::filter(decade!=0) 
    
    output_summarized_ind<-output_summarized1 %>% 
      dplyr::filter(correlation=="independent" ) %>% 
      dplyr::filter(decade!=0) 
    
    cvtrip_diff_corr <- output_summarized_corr %>%
      rename_with(~ paste0(.x, "_corr"), c(cvtrip, cv_choice, cv, ntrips)) %>%
      dplyr::select("draw", "copula","decade","cvtrip_corr", "cv_choice_corr", "cv_corr", "ntrips_corr", "mode") %>%     dplyr::filter(decade!=0 )
    
    cvtrip_diff_ind <- output_summarized_ind %>%
      rename_with(~ paste0(.x, "_ind"), c(cvtrip, cv_choice, cv, ntrips)) %>%
      dplyr::select("draw", "copula","decade","cvtrip_ind", "cv_choice_ind", "cv_ind", "ntrips_ind", "mode") %>%     dplyr::filter(decade!=0 )
    
    cvtrip_diff<-cvtrip_diff_corr %>% 
      left_join(cvtrip_diff_ind, by=c("draw", "copula","decade", "mode" )) %>%
      mutate(diff_ntrips=ntrips_corr- ntrips_ind, 
             diff_cvtrip=cvtrip_corr- cvtrip_ind, 
             diff_cv_choice=cv_choice_corr- cv_choice_ind, 
             diff_cv=cv_corr- cv_ind)
    
    cvtrip_diff$base_year<-yr
    
    cvtrip_diff_mode_list[[yr]]<-cvtrip_diff
    
    #differences data catch
    diff_corr <- output_summarized_corr %>%
      rename_with(~ paste0(.x, "_corr"), c(codkeep, haddkeep, codcat, haddcat, cv,
                                           cod_hadd_keep, cod_hadd_cat, cod_tot_mort, hadd_tot_mort)) %>%
      dplyr::select("draw", "copula","decade","codkeep_corr", "haddkeep_corr", "cv_corr",
                    "codcat_corr", "haddcat_corr", "cod_hadd_keep_corr", "cod_hadd_cat_corr", 
                    "cod_tot_mort_corr", "hadd_tot_mort_corr", "mode") %>%   
      dplyr::filter(decade!=0 )
    
    diff_ind <- output_summarized_ind %>%
      rename_with(~ paste0(.x, "_ind"), c(codkeep, haddkeep, codcat, haddcat, cv,
                                          cod_hadd_keep, cod_hadd_cat, cod_tot_mort, hadd_tot_mort)) %>%
      dplyr::select("draw", "copula","decade","codkeep_ind", "haddkeep_ind", "cv_ind",
                    "codcat_ind", "haddcat_ind", "cod_hadd_keep_ind", "cod_hadd_cat_ind", 
                    "cod_tot_mort_ind", "hadd_tot_mort_ind", "mode") %>%   
      dplyr::filter(decade!=0 )
    
    diff<-diff_corr %>% 
      left_join(diff_ind, by=c("draw", "copula","decade", "mode"  )) %>%
      mutate(diff_cv=cv_corr- cv_ind,
             diff_codkeep=codkeep_corr- codkeep_ind, 
             diff_haddkeep=haddkeep_corr- haddkeep_ind, 
             diff_codcat=codcat_corr- codcat_ind, 
             diff_haddcat=haddcat_corr- haddcat_ind, 
             diff_cod_hadd_keep=cod_hadd_keep_corr- cod_hadd_keep_ind, 
             diff_cod_hadd_cat=cod_hadd_cat_corr- cod_hadd_cat_ind, 
             diff_cod_mort=cod_tot_mort_corr- cod_tot_mort_ind, 
             diff_hadd_mort=hadd_tot_mort_corr- hadd_tot_mort_ind, 
             pct_diff_codkeep=((codkeep_corr- codkeep_ind)/codkeep_ind)*100, 
             pct_diff_haddkeep=((haddkeep_corr- haddkeep_ind)/haddkeep_ind)*100, 
             pct_diff_codcat=((codcat_corr- codcat_ind)/codcat_ind)*100, 
             pct_diff_haddcat=((haddcat_corr- haddcat_ind)/haddcat_ind)*100, 
             pct_diff_cod_hadd_keep=((cod_hadd_keep_corr- cod_hadd_keep_ind)/cod_hadd_keep_ind)*100, 
             pct_diff_cod_hadd_cat=((cod_hadd_cat_corr- cod_hadd_cat_ind)/cod_hadd_cat_ind)*100, 
             pct_diff_cod_mort=((cod_tot_mort_corr- cod_tot_mort_ind)/cod_tot_mort_ind)*100, 
             pct_diff_hadd_mort=((hadd_tot_mort_corr- hadd_tot_mort_ind)/hadd_tot_mort_ind)*100, 
             base_year=yr)
    
    diff_mode_list[[yr]]<-diff
    
  }
  
  diff_mode_combined <- dplyr::bind_rows(diff_mode_list)
  plot_data_mode_combined <- dplyr::bind_rows(plot_data_mode_list)
  cvtrip_diff_mode_combined <- dplyr::bind_rows(cvtrip_diff_mode_list)
  output_summarized1_mode_combined<-dplyr::bind_rows(output_summarized1_mode_list)
  
  
  # Reorder factor levels so "independent" is first
  output2$copula <- factor(output2$copula,
                           levels = c("independent", setdiff(unique(output2$copula), "independent")))
  
  # Shared color scale: no title, single row legend
  copula_colors <- scale_color_discrete(
    name = NULL,
    guide = guide_legend(nrow = 1)
  )
  
  ggplot(plot_data_mode_combined, aes(x = factor(decade), y = cv, color = factor(copula))) +
    geom_boxplot() +
    copula_colors +
    labs(x = "Decade", y = "Compensating variation ($)")+
    theme(
    # Remove major vertical grid lines (x-axis)
    panel.grid.major.x = element_blank(),
  # Remove minor vertical grid lines (x-axis)
  panel.grid.minor.x = element_blank(), 
  legend.position = "bottom",
  legend.direction = "horizontal")+
    facet_wrap(~ interaction(mode, base_year, sep = " - "), ncol = 2) 
  
  
  gplot(output2, aes(y = haddkeep, color = copula)) +
    geom_boxplot() +
    facet_wrap(~decade, nrow = 1) +
    labs(x = NULL, y = "Haddock harvest\n('000s)") +
    copula_colors +
    theme(
      axis.text.x   = element_blank(),
      axis.ticks.x  = element_blank(),
      strip.text    = element_blank(),
      legend.position = "none", 
      # Remove major vertical grid lines (x-axis)
      panel.grid.major.x = element_blank(),
      # Remove minor vertical grid lines (x-axis)
      panel.grid.minor.x = element_blank()
    )
  
  
  
  
  output_summarized1<- read_rds(paste0(output_data_cd,"model_output_y", y,"_outliers_9-21-25_reformat_mode.rds"))  %>%
    dplyr::mutate(codreltrip=codrel/ntrips,
                  haddreltrip=haddrel/ntrips, 
                  codrelchoice=codrel/n_choice_occasions,
                  haddrelchoice=haddrel/n_choice_occasions, 
                  cod_hadd_keep=codkeep+haddkeep, 
                  cod_hadd_cat=codcat+haddcat)
  
  output_summarized1$ntrips[is.na(output_summarized1$ntrips)] <- output_summarized1$dtrip[is.na(output_summarized1$ntrips)]
  output_summarized1$copula[output_summarized1$correlation=="independent"] <- "independent"
  
  output2<-output_summarized1 %>%
    dplyr::filter(!is.na(decade))  %>%
    dplyr::filter(decade!=0)  %>%
    dplyr::mutate(decade = dplyr::recode(decade,
                                         `1` = "2021-2030",
                                         `2` = "2031-2040",
                                         `3` = "2041-2050",
                                         `4` = "2051-2060",
                                         `5` = "2061-2070",
                                         `6` = "2071-2080",
                                         `7` = "2081-2090",
                                         `8` = "2091-3000"
    )) 
  
  df_subset <- output2 %>%
    # keep only the independent/independent rows
    dplyr::filter(copula == "independent", correlation == "independent") %>%
    # stratified sample: 100 per decade
    dplyr::group_by(decade) %>%
    dplyr::slice_sample(n = 100) %>%
    dplyr::ungroup()
  
  output2<-output2 %>% 
    dplyr::filter(copula!="independent") 
  
  output2<-output2 %>% 
    plyr::rbind.fill(df_subset) %>% 
    dplyr::select(-year) %>% 
    dplyr::mutate(codkeep=codkeep/1000, haddkeep=haddkeep/1000, cv=cv/1000000,
                  codcat=codcat/1000, haddcat=haddcat/1000,)
  
  
  
  library(ggplot2)
  library(patchwork)
  
  # Reorder factor levels so "independent" is first
  output2$copula <- factor(output2$copula,
                           levels = c("independent", setdiff(unique(output2$copula), "independent")))
  
  # Shared color scale: no title, single row legend
  copula_colors <- scale_color_discrete(
    name = NULL,
    guide = guide_legend(nrow = 1)
  )
  
  ggplot(output2, aes(x = factor(decade), y = cv, color = factor(copula))) +
    geom_boxplot() +
    copula_colors +
    labs(x = "Decade", y = "Compensating variation ($)")+
    theme(
      # Remove major vertical grid lines (x-axis)
      panel.grid.major.x = element_blank(),
      # Remove minor vertical grid lines (x-axis)
      panel.grid.minor.x = element_blank(), 
      legend.position = "bottom",
      legend.direction = "horizontal")+
    facet_wrap(~mode) 