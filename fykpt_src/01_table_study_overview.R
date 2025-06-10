library(flextable)
library(magrittr)
library(tidyverse)

options(gtsummary.tbl_summary.percent_fun = function(x) sprintf(x * 100, fmt='%#.1f'))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# check <- stringi::stri_unescape_unicode("\\u2714")

tab_study_overview <- rio::import('table_study_overview.xlsx')
risk_factors_by_study_pre <- rio::import('table_risk_factors_by_study.xlsx', which = 1) %>% 
  rename(study_name = "Study") %>% 
  group_by(study_name) %>% 
  summarize(`No. of covariates` = n())

tab_study_overview <- 
  left_join(tab_study_overview,
          risk_factors_by_study_pre,
          by = 'study_name'
          )

########################################################################################*
# Long detailed study table for supplement (Table S1) ###################################
########################################################################################*

print_tab_study_overview_detailed <- 
  tab_study_overview %>% 
  select(study_name:PR, `No. of covariates`) %>% 
  rename("First author,\nyear" = study_name,
         "Focus" = focus,
         "Period" = Interval,
         "No.\npatients" = N_Patients,
         "No.\nhospitals" = N_Hospitals,
         "No.\nsurgeons" = N_Surgeons,
         "Database" = data_source,
         "Data\ntype" = `data type`,
         "Country" = country,
         "Outcome" = endpoint,
         "Diagnoses" = diseases
  ) %>% 
  flextable() %>%
  fontsize(size = 6, part = 'all') %>%   
  height(height = .1) %>% 
  autofit() %>%
  
  add_footer_lines('NS not stated') %>% 
  add_footer_lines('DP distal pancreatectomy, PD pancreaticoduodenectomy, TP total pancreatectomy, SOR Segmental/other resection') %>% 
  add_footer_lines('^a^ average number of observation across years/periods') %>% 
  add_footer_lines('^b^ weighted/extrapolated') %>% 
  add_footer_lines('^c^ differing case numbers')

print_tab_study_overview_detailed


########################################################################################*
# summarized overview of studies (Table 1) ##############################################
########################################################################################*

tab_study_overview_2 <- 
  tab_study_overview %>% 
  # region
  mutate(Region = fct_collapse(country,
                               'Europe' = c('Finnland', 'Germany', 'Italy', 'Sweden', 'France', 'Netherlands', 'Switzerland', 'UK', 'Spain'),
                               'North-America' = c('USA', 'Canada', 'USA/CAN/Italy/Scotland'),
                               other_level = 'Other'
                               )) %>% 
  # data type
  mutate(`Data type` = case_when(
    `data type` == 'a' ~ 'Administrative',
    `data type` == 'c' ~ 'Clinical'
  )) %>% 
  # Study focus
  mutate(Focus = str_to_title(focus)) %>% 
  # Endpoint
  mutate(Mortality = str_replace_all(endpoint, ' mortality', '')) %>% 
  mutate(Mortality = str_replace_all(Mortality, 'd', ' day')) %>% 
  # Study decades
  mutate(Interval = str_replace_all(Interval, '\\d\\d/','')) %>% 
  mutate(Interval_Start = str_extract(Interval, "^\\d{4}") %>% as.numeric) %>% 
  mutate(Interval_End = str_extract(Interval, "\\d{4}$") %>% as.numeric) %>% 
  mutate(decade_80 = case_when(
    Interval_Start < 1990 ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(decade_90 = case_when(
    Interval_Start >= 1990 & Interval_Start < 2000 ~ 1,
    Interval_End >= 1990 & Interval_End < 2000 ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(decade_00 = case_when(
    Interval_Start >= 2000 & Interval_Start < 2010 ~ 1,
    Interval_Start < 2000 & Interval_End > 2010 ~ 1, 
    Interval_End >= 2000 & Interval_End < 2010 ~ 1,
    TRUE ~ 0
  )) %>% 
  mutate(decade_10 = case_when(
    Interval_Start >= 2010 ~ 1,
    Interval_End > 2010 ~ 1,
    TRUE ~ 0
  )) %>% 
  labelled::set_variable_labels(decade_80 = 'before 1990', decade_90 = '1990-1999', decade_00 = '2000-2010', decade_10 = 'after 2010') %>% 
  # Procedures
  mutate(across(RP:PR, ~if_else(.x == 'X', '1', '0', missing = '0') %>% 
                  as.numeric %>% 
                  as.logical)) %>% 
  labelled::set_variable_labels(RP = 'Radical pancreaticoduodenectomy', TP = 'Total pancreatectomy', 
                                PrP = 'Proximal pancreatectomy', DP = 'Distal pancreatectomy',
                                RSP = 'Radical subtotal pancreatectomy', OPP = 'Other partial pancreatectomy',
                                PR = 'Pancreatic resection (no details provided)')


# "data by study"-colum
table_part_study <- 
  tab_study_overview_2 %>% 
  select(Region, `Data type`, Focus, Mortality, RP:PR, contains('decade'), `No. of covariates`) %>% 
  gtsummary::tbl_summary() 

# "data by patients"-colum
table_part_patients <-
  tab_study_overview_2 %>% 
  mutate(N_Patients = as.numeric(str_extract(N_Patients, '(\\d*)'))) %>% 
  uncount(N_Patients) %>% 
  labelled::set_variable_labels(decade_80 = 'before 1990', decade_90 = '1990-1999', decade_00 = '2000-2010', decade_10 = 'after 2010') %>% 
  labelled::set_variable_labels(RP = 'Radical pancreaticoduodenectomy', TP = 'Total pancreatectomy', 
                                PrP = 'Proximal pancreatectomy', DP = 'Distal pancreatectomy',
                                RSP = 'Radical subtotal pancreatectomy', OPP = 'Other partial pancreatectomy',
                                PR = 'Pancreatic resection (no details provided)') %>% 
  select(Region, `Data type`, Focus, Mortality, RP:PR, contains('decade'), `No. of covariates`) %>% 
  gtsummary::tbl_summary() 

print_tab_study_overview_summarized <- 
  gtsummary::tbl_merge(list(table_part_study, table_part_patients), tab_spanner = c('by studies', 'by patients')) %>% 
  gtsummary::as_flex_table() %>% 
  fontsize(size = 6, part = 'all') %>%   
  height(height = .1) %>% 
  autofit() 

print_tab_study_overview_summarized


################# calculations ################################################

# number of patients
tab_study_overview %>% 
  mutate(N_Patients = as.numeric(str_extract(N_Patients, '(\\d*)'))) %>% 
  summarize(gesamt = sum(N_Patients, na.rm=T))


tab_study_overview %>% #pull(country) %>% unique
  group_by(country) %>% 
  summarize(Anzahl = n())


# no of covariates by data type
tab_study_overview_2 %>% 
  group_by(`data type`) %>% 
  summarise(med = median(`No. of covariates`, na.rm = T),
            q1 = quantile(`No. of covariates`, probs = .25, na.rm = T),
            q3 = quantile(`No. of covariates`, probs = .75, na.rm = T)
            )

