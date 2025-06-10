library(treemapify)
# library(tidyverse)
# devtools::install_github("crsh/papaja")

# color codes
cols <- c(  "1" = "red",
            "2" = "black",
            "3" = "orangered",
            "4" = "black", 
            "9" = "blue",
           "11" = "blue")

risk_factors_by_study <- 
  rio::import('table_risk_factors_by_study.xlsx', which = 1) %>% 

 # order and renaming of covariates for presentation 
  mutate(`Adjusting factor` = as.factor(`Adjusting factor`)) %>% 
  mutate(`Adjusting factor` = fct_relevel(`Adjusting factor`, c(
    # General patient related factors: 14
    'Gender', 'Age', 'Race', 'Nationality', 
    "Education level", 'Martial status', 'Residence',
    'Socio-economic status', 'Insurance type', 
    'BMI', 'Smoking status', 
    "Activities of daily living", 
    "Education level neighborhood", 

    # Special patient related factors: 19
    'Diagnosis', 'Procedure', 'Admission type',
    'Comorbidity scores', 'Single comorbidities', 'Referral',
    'Functional status', 'ASA class',
    "Dialysis", "Fistula risk score", 
    "Severity of illness", "Risk of mortality", 
    "White blood cell count", "Platelet count", 
    "Prothrombin time", "Partial thromboplastin time", 
    "Serum creatinine levels", "Travel distance",
    
    # Disease factors: 6
    "T stage", "Tumour size", "Tumour location",
    "Tumour histology", "Tumour grading", 
    "N stage", 
    
    # Treatment factors: 14
    "Resection year", 
    "Vascular resection", "Multivisceral resections",
    'Necrosectomy', "Minimally invasive",
    "Radiotherapy (adj.)", "Chemotherapy (adj.)",
    'Neoadjuvant therapy',
    "Radiotherapy (neoadj.)", "Chemotherapy (neoadj.)", 
    "Wound class", "Steroid use", 
    "Admission-surgery delay", 
    "Weekday",
    
    # Outcome: 7
    'Length-of-stay', 'Readmission', "Systemic sepsis",
    "Ascites", "Ventilator dependent", "Complication", 
    "Respiratory distress", 

    # Hospital factors: 8
    'Hospital teaching status', 'Hospital bed size',
    'Hospital nurse/patient ratio', 'Hospital business model',
    "Hospital region", "Hospital experience", 
    "Facility type", 'Hospital volume',
    
    # Surgeon factors: 7
    "Surgeon gender", "Surgeon age", 
    'Surgeon years of practice', "Surgeon speciality",
    "Surgeon procedure frequency", 
    "Surgeon US trained", 'Surgeon volume'
    
  ))) %>% 
  
  # order of studies (alphabetically)
  arrange(Study) %>% 
  mutate(Study = fct_rev(fct_inorder(Study))) %>% 
  
  # for plotting
  mutate(Consideration = as.factor(Consideration)) %>%
  
  # what number to display
  mutate(Effect_Label = case_when(
    Effect == 0 ~ " ", # valu not reported, so nothing to display
    #Effect == 1 ~ papaja::printnum(Effect, digits = 2),
    TRUE ~ papaja::printnum(Effect, digits = 2)
  ))


########################################################################################*
# Risk map: study x covariate (Figure S2) ###################################
########################################################################################*

make_plot <- function(data = NULL) {
  # general printing
  # axis colors
  df2 <- risk_factors_by_study %>% 
    group_by(`Adjusting factor`) %>% 
    mutate(Sig = case_when(
      any(Consideration %in% c(1,3)) ~ 'red',
      all(Consideration %in% c(9,11)) ~ 'blue',
      TRUE ~ "black"
    )) %>% 
    ungroup() %>% 
    mutate(`Adjusting factor` = as.character(`Adjusting factor`)) %>% 
    distinct(`Adjusting factor`, .keep_all = T) 
  
  color_vector <- left_join(levels(risk_factors_by_study$`Adjusting factor`) %>% as_tibble_col('Adjusting factor'), 
                            df2, 
                            by = "Adjusting factor") %>% 
    pull(Sig)
  # axis colors - END
  
  
  output <- 
  data %>% 
  ggplot(aes(x=`Adjusting factor`, y = Study, color = Consideration, size = Effect)) +
  geom_point() +
  scale_x_discrete(position = "top") +
  scale_y_discrete(expand = expansion(add = c(1,3))) +
  scale_color_manual(values = cols) +
  
  # look & feel
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0, color = color_vector),
        axis.text.y = element_text(color = 'black', size = 8), 
        aspect.ratio = 0.62,
        plot.margin = unit(c(0,38,0,0), 'pt'),
        legend.position = 'NULL') +
        #legend.position = 'bottom',
        #legend.box = 'horizontal') +
  
  geom_text(aes(label=Effect_Label, size = 3), color = 'black', nudge_y = -.3) +
    
  geom_vline(xintercept = c(13.5, 31.5, 37.5, 51.5, 58.5, 66.5, 73.5)) +
  geom_hline(yintercept = 87.5) +
    
  annotate('text', x=7,    y=88.9, size = 4, label = 'patient factors (general)') +
  annotate('text', x=23,   y=88.9, size = 4, label = 'patient factors (medical)') +
  annotate('text', x=34.5, y=88.9, size = 4, label = 'disease factors') +
  annotate('text', x=44,   y=88.9, size = 4, label = 'treatment factors') +
  annotate('text', x=55,   y=88.9, size = 4, label = 'outcome factors') +
  annotate('text', x=62.5, y=88.9, size = 4, label = 'hospital factors') +
  annotate('text', x=70  , y=88.9, size = 4, label = 'surgeon factors') 
}


risk_factors_by_study %>% make_plot() + labs(title = NULL, x = 'Covariate')

#### FIGURE S2 ####
ggsave(path = '.', filename = 'Fig. S2.png', dpi = 600, scale = 2, width = 29, height = 18, units = 'cm')


###########################################################################################*
########## Figure 1 ########################################################################
###########################################################################################*


# see here: https://www.metafor-project.org/doku.php/tips:assembling_data_or
risk_factors_by_study <- 
  risk_factors_by_study %>%
  rowwise() %>% 
  mutate(vi = ((log(CIUL)-log(CILL))/( qnorm(.975) *2))^2) %>% 
  mutate(yi = log(Effect)) %>% 
  ungroup()  

### test a single comorbidity condition
# risk_factors_by_study %>% 
#   filter(!is.na(vi) & !is.infinite(yi)) %>% 
#   filter(`Adjusting factor` == 'Age') %>% 
#   metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')->bb

data_for_metaanalysis <- 
  risk_factors_by_study %>% 
  filter(!is.na(vi) & !is.infinite(yi)) %>% 
  group_by(`Adjusting factor`) %>% 
  mutate(Anzahl = n()) %>% 
  filter(Anzahl >= 5) %>% 
  ungroup() %>% 
  group_by(`Adjusting factor`) 

meta1_beta <- 
  data_for_metaanalysis %>% 
  group_modify(~metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')[['beta']] %>% as.numeric %>% exp %>% enframe, .keep = F)%>% 
  select(-name) %>% 
  rename('beta' = value)

meta1_cilb <- 
  data_for_metaanalysis %>% 
  group_modify(~metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')[['ci.lb']] %>% as.numeric %>% exp %>% enframe, .keep = F) %>% 
  select(-name)%>% 
  rename('CI.LB' = value)

meta1_ciub <- 
  data_for_metaanalysis %>% 
  group_modify(~metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')[['ci.ub']] %>% as.numeric %>% exp %>% enframe, .keep = F)%>% 
  select(-name) %>% 
  rename('CI.UB' = value)

meta1_i2 <- 
  data_for_metaanalysis %>% 
  group_modify(~metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')[['I2']] %>% 
                 as.numeric %>% papaja::printnum(., digits = 1) %>% str_c(., '%')  %>% enframe, 
               .keep = F) %>% 
  select(-name) %>% 
  rename('I2' = value)

meta1_h2 <- 
  data_for_metaanalysis %>% 
  group_modify(~metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')[['H2']] %>% 
                 as.numeric %>% papaja::printnum(., digits = 1) %>% str_c(., '%') %>% enframe, 
               .keep = F)%>% 
  select(-name) %>% 
  rename('H2' = value)

meta1_tau2 <- 
  data_for_metaanalysis %>% 
  group_modify(~metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')[['tau2']] %>%                 
                 as.numeric %>% papaja::printnum(., digits = 1) %>% str_c(., '%') %>% enframe, 
               .keep = F)%>% 
  select(-name) %>% 
  rename('tau2' = value)

risk_factors_by_study <- plyr::join_all(list(risk_factors_by_study, meta1_beta, meta1_cilb, meta1_ciub, meta1_i2, meta1_h2, meta1_tau2), by = 'Adjusting factor') %>% 
  mutate(Study = as.factor(Study))


# plotting
risk_factors_by_study %>% 
  
  # number of patients for weighting
  left_join(tab_study_overview %>% rename(Study = 'study_name'), by = 'Study') %>% 
  mutate(N_Patients = as.numeric(str_extract(N_Patients, '(\\d*)'))) %>% 

  # exclude missings
  filter(!is.na(vi) & !is.infinite(yi)) %>% 
  
  # exclude infrequently considered covariates
  group_by(`Adjusting factor`) %>% 
  mutate(Anzahl = n()) %>% 
  filter(Anzahl > 5| `Adjusting factor` == 'Hospital bed size') %>% 
  ungroup() %>% 
  #mutate(`Adjusting factor` = droplevels(`Adjusting factor`)) %>% 
  
  # reorder Comorbidities by efffect size (instead of alphabetically)
  mutate(`Adjusting factor` = fct_reorder(`Adjusting factor`, beta)) %>% 
  
  # point color to indicate significance
  mutate(point_color = if_else(CILL < 1 & CIUL > 1, 'black', 'red')) %>% 
  
  # plotting
  # ::::::::::::::::::::::::::::::::
  ggplot(aes(x=`Adjusting factor`, y=Effect)) +#fct_reorder(Comorbidity, median_density)
  # points and violins

  # meta-analytic overall effect
  geom_crossbar(aes(x=`Adjusting factor`, y=beta, ymin=CI.LB, ymax=CI.UB), 
                width=.7, color = 'grey50') +
  
  geom_point(aes(color=point_color, size=N_Patients), alpha = .6) +
  geom_hline(yintercept = 1, linetype = 'dashed', color = 'grey50') +
  
  # additional comments: OR
  #annotate('text', x= meta1_beta$`Adjusting factor`, y = 16, label = papaja::printnum(meta1_beta$beta, digits = 2),  size = 3.4) + 
  annotate('text', x= meta1_beta$`Adjusting factor`, y = 16, label = papaja::printnum(meta1_beta$beta, digits = 2),  size = 3.4) + 
  annotate('text', x= meta1_beta$`Adjusting factor`, y = 17, size = 3.4,
           label = str_c(
             '(',papaja::printnum(meta1_cilb$CI.LB, digits = 2),  
             '-',papaja::printnum(meta1_ciub$CI.UB, digits = 2), ')'
           )) + 
  annotate('text', x= meta1_beta$`Adjusting factor`, y = 18.2, label = papaja::printnum(meta1_i2$I2, digits = 2),  size = 3.4) + 
  
  # additional comments: heading
  annotate('text', x = 17.5, y = 16,   label = 'atop(bold("OR"))', size = 3.4, parse=T) +  
  annotate('text', x = 17.5, y = 17,   label = 'atop(bold("95%-CI"))', size = 3.4, parse=T) +
  annotate('text', x = 17.5, y = 18.2, label = 'atop(bold("I?"))', size = 3.4, parse=T) +
  
  ###annotate('text', x = 16.4, y = 16.4, label = 'atop(bold("Exposure category"))', size = 3.4, hjust = 0, parse=T) + 
  # styling
  coord_flip() +
  theme_classic() +
  # expand range to the right for additional comments
  scale_y_continuous(limits = c(0,19), breaks = seq(0,15,1), expand = expansion(add = c(0,0))) +
  # expand range on top for additional comments
  scale_x_discrete(expand = expansion(add = c(.5,1.5))) +
  # tweak colors and labels of points
  scale_color_manual(values = c('black', 'red'), labels = c('not significant', 'significant')) +
  scale_size_continuous(labels = scales::comma) +
  labs(x='Covariate', y='Effect size (OR)', color='Significance', size='Study size\n(no. of patients)') +
  # axis text in black
  theme(axis.text = element_text(color = 'black', size = 10)) +
  scale_alpha_continuous(guide = NULL)


# save
ggsave(path = '.', filename = 'Fig. 1.png', dpi = 600, scale = 1.2, width = 29, height = (17/29)*18, units = 'cm')


###########################################################################################*
########## Figure 2: covariate and effect size (x) by area ########################
###########################################################################################*

# treemap

meta1_beta %>% 
  mutate(group = case_when(
    `Adjusting factor` %in% c('Age', 'Gender', 'Socio-economic status', 'Race', 'Insurance type') ~ 
      'General patient factors',
    `Adjusting factor` %in% c('Resection year', 'Minimally invasive') ~ 
      'Treatment factors',
    `Adjusting factor` %in% c('Comorbidity scores', 'Admission type', 'Diagnosis', 'Procedure', 'Single comorbidities') ~ 
      'Medical patient factors',
    `Adjusting factor` %in% c('Hospital teaching status', 'Hospital bed size', 'Hospital region', 'Hospital volume') ~ 
      'Hospital factors',
    `Adjusting factor` %in% c('Surgeon volume') ~ 
      'Surgeon factors'
  )) %>% 
  rename(Covariate = `Adjusting factor`) %>% 
  mutate(Covariate = str_c(Covariate, '\n(', papaja::printnum(beta, digits = 2), ')')) %>% 
  ggplot(aes(area = beta, fill = group, label = Covariate, subgroup = group)) +
  geom_treemap(fill = 'black') +
  geom_treemap(aes(alpha = beta)) +
  geom_treemap_subgroup_border(color = 'white') +
  geom_treemap_subgroup_text(place = "top", grow = F, alpha = 0.8, padding.y = grid::unit(5, "pt"),
                             colour = "white", fontface = "italic", size = 18) +
  geom_treemap_text(colour = "white", place = "bottom", reflow = T, size = 12, grow = F, padding.y = grid::unit(7, "pt")) +
  scale_alpha_continuous(range = c(1,.4), labels = c("A", "4", "3", '2', 'B')) +
  scale_fill_brewer(palette = 'Set1') +
  labs(fill = 'Covariate category', alpha = 'Average effect') +
  guides(#alpha = guide_legend(reverse = T),
         fill = FALSE, alpha = FALSE)

ggsave(path = '.', filename = 'Fig. 2.png', dpi = 600, scale = 1, width = 29, height = 18, units = 'cm')



###########################################################################################*
########## Figure 4: Plot number of adjustors against effect (hospital) ########################
###########################################################################################*

d <- 
  left_join(risk_factors_by_study,
            tab_study_overview %>% 
              rename(Study = study_name) %>% 
              mutate(N_Patients = as.numeric(str_extract(N_Patients, '(\\d*)'))),
            by = 'Study') 

# show top hospital volume effects
d %>% 
  filter(`Adjusting factor` == 'Hospital volume') %>% 
  arrange(desc(Effect))
  
# plot number of covariates against Volume effect
d %>%   
  #filter(Consideration %in% c(1,3)) %>% 
  group_by(Study) %>% 
  mutate(`Number of adjusting factors` = n()) %>% 
  ungroup() %>% 
  filter(`Adjusting factor` == 'Hospital volume' & Effect != 0 & Effect < 14) %>% 
  
  ggplot(aes(x=`Number of adjusting factors`, y = Effect)) +   #, weight=N_Patients
  #geom_smooth(method = 'lm', formula = y ~ poly(x,2), se = T, color = 'grey50', fill = 'grey80') +
  #geom_smooth(method = 'lm', formula = y ~ x + I(x^2)+1, se = T, color = 'grey50', fill = 'grey80') +
  geom_smooth(method = 'lm', formula = y ~ x+1, se = T, color = 'grey50', fill = 'grey80') +
  geom_point(aes(size = N_Patients, color = `data type`), alpha = 0.4) +
  #geom_abline(intercept = 3.19, slope = -0.06, color = 'grey50') +
  geom_abline(intercept = 1, slope = 0, color = 'grey', linetype = '33') +
  
  labs(size = 'Sample size', y = 'Hospital volume effect (OR)', x = 'Number of considered covariates'
       #, caption = 'Two studies excluded: Ahola, 2019; Gordon, 1999\nPoints are jittered relative to x-axis to reduce overlap'
       ) +
  
  scale_y_continuous(breaks = c(-1:9), expand = expansion(add=0)) +
  scale_x_continuous(limits = c(1,23), breaks = seq(2,22,2)) +
  scale_size_continuous(labels = scales::comma) +
  
  # zoom in (se of geom_smooth goes far under 0)
  coord_cartesian(ylim = c(0,9)) +
  theme_classic() +
  
  scale_color_discrete(name = 'Database', labels = c('administrative', 'clinical')) +
  
  annotate('text', x = 14, y = 2.7, label = 'y = 4.26 - 0.14x\nP = 0.007', size = 3, hjust = 0)

ggsave(path = '.', filename = 'Fig. 4.png', dpi = 600, scale = 0.7, width = 29, height = 18, units = 'cm')


# linear regression 
lm_model <- 
  d %>% 
  #filter(Consideration %in% c(1,3) ) %>% 
  group_by(Study) %>% 
  mutate(`Number of adjusting factors` = n()) %>% 
  ungroup() %>% 
  filter(`Adjusting factor` == 'Hospital volume' & Effect != 0 & Effect < 10) %$% # 
  lm(Effect ~ `Number of adjusting factors`) # , weights = N_Patients


summary(lm_model)
confint(lm_model)


#############################################################################################*
# Table S2  ########################
#############################################################################################* 

# detailed numbers
risk_factors_by_study_table <- 
  left_join(risk_factors_by_study,
            tab_study_overview %>% 
              rename(Study = study_name) %>% 
              mutate(N_Patients = as.numeric(str_extract(N_Patients, '(\\d*)'))),
            by = 'Study') %>%  
  group_by(`Adjusting factor`) %>% 
  summarize(Considered = n(),
            Significant       = sum(Consideration %in% c(1,3),  na.rm=T),
            `Non-significant` = sum(Consideration %in% c(2,4),  na.rm=T),
            `Not reported`    = sum(Consideration %in% c(9,11), na.rm=T)) %>% 
  mutate(`Not considered` = length(unique(d$Study)) - Considered) %>% 
  mutate(Reported = Considered - `Not reported`) %>% 
  mutate(Prop_Sig_of_Total = Significant/Considered*100) %>% 
  mutate(Prop_Sig_of_Reported = Significant/Reported*100) %>% 
  mutate(Prop_Considered = Considered/length(unique(d$Study))*100) %>% 
  mutate_if(is.double, ~round(., 1)) %>% 
  arrange(desc(Prop_Considered)) %>% 
  ungroup()

# table for appendix
risk_factors_by_study_table %>% 
  flextable() %>% 
  autofit()



################################################### for APPENDIX #########################################
# most often significant covariates
risk_factors_by_study_table %>% 
  filter(Considered > 4) %>% 
  arrange(desc(Prop_Sig_of_Reported))

# most often significant covariates
risk_factors_by_study_table %>% 
  #filter(Considered <= 4) %>% 
  group_by(Considered) %>% 
  summarize(Number_not_reported = sum(Reported == 0, na.rm=T)) 

###### infrequently considered covariates #####*
# number of infrequent considerd covariates
risk_factors_by_study_table %>% 
  filter(Considered <= 4) %>% 
  arrange(desc(Prop_Sig_of_Reported)) %>% nrow

# overview of infrequently considered covariates
risk_factors_by_study_table %>% 
  filter(Considered <= 4) %>% 
  arrange(desc(Prop_Sig_of_Reported))

# significance of infrequent covariates
risk_factors_by_study_table %>% 
  filter(Considered <= 4) %>% 
  group_by(Significant) %>% 
  summarize(Number_not_reported = n()) 

# which are the infrequent significant covariates?
risk_factors_by_study_table %>% 
  filter(Considered <= 4) %>% 
  filter(Significant %in% c(1,3)) %>% 
  pull(`Adjusting factor`) %>% 
  str_flatten(collapse = ', ')

# nuber of covariates never reported
risk_factors_by_study_table %>% 
  filter(Reported == 0) %>% 
  nrow()


#############################################################################################*
########## Calculations on Volumes ########################
#############################################################################################* 

# overview: what does studies examine: hospital or surgeon volume or both
risk_factors_by_study %>% filter(`Adjusting factor` == 'Hospital volume') %>% pull(Study) -> a
tab_study_overview %>% filter(focus %in% c('hospital', 'both')) %>% pull(study_name) -> b
setdiff(a, b)

# study frequency by focus
table(tab_study_overview$focus)
prop.table(table(tab_study_overview$focus)) %>% multiply_by(100) %>% round(2)

# 1. hospital only
#--------------------.
tab_study_overview %>% filter(focus %in% c('hospital')) %>% pull(study_name) -> hospital_only_studies
risk_factors_by_study %>% filter(Study %in% hospital_only_studies) %>% filter(`Adjusting factor` == 'Hospital volume') %$% table (Consideration)

# 6 non-significant studies: 
risk_factors_by_study %>% filter(Study %in% hospital_only_studies) %>% filter(`Adjusting factor` == 'Hospital volume' & Consideration == 2)

# names of hospital-only studies with significant effects:
risk_factors_by_study %>% filter(Study %in% hospital_only_studies) %>% filter(`Adjusting factor` == 'Hospital volume' & Consideration %in% c(1,3)) %>% 
  pull(Study) %>% str_flatten(col = '; @') %>% str_replace_all(", ", "") %>% str_replace_all(" ", "") %>% str_replace_all("['-]", "") %>% str_to_lower() %>% str_c("@", .)


# 2. surgeon only
#--------------------.
tab_study_overview %>% filter(focus %in% c('surgeon')) %>% pull(study_name) -> surgeon_only_studies
risk_factors_by_study %>% filter(Study %in% surgeon_only_studies) %>% filter(`Adjusting factor` == 'Surgeon volume') %$% table (Consideration)

# 2 non-significant studies: 
risk_factors_by_study %>% filter(Study %in% surgeon_only_studies) %>% filter(`Adjusting factor` == 'Surgeon volume')


# 3. both
#--------------------.
tab_study_overview %>% filter(focus %in% c('both')) %>% pull(study_name) -> both_only_studies
risk_factors_by_study %>% filter(Study %in% both_only_studies) %>% filter(str_detect(`Adjusting factor`, 'volume')) 

# hospital (1 & 3)
#--------------------.
risk_factors_by_study %>% filter(Study %in% c(hospital_only_studies, both_only_studies)) %>% filter(`Adjusting factor` == 'Hospital volume') %$% table(Consideration)
risk_factors_by_study %>% filter(Study %in% c(hospital_only_studies, both_only_studies)) %>% filter(`Adjusting factor` == 'Hospital volume') %>% filter(Consideration == 2) %>% 
  left_join(., tab_study_overview %>% rename(Study = "study_name"), by = "Study")


# surgeon (2 & 3)
#--------------------.
risk_factors_by_study %>% filter(Study %in% c(surgeon_only_studies, both_only_studies)) %>% filter(`Adjusting factor` == 'Surgeon volume') %$% table(Consideration)
risk_factors_by_study %>% filter(Study %in% c(surgeon_only_studies, both_only_studies)) %>% filter(`Adjusting factor` == 'Surgeon volume') %>% filter(Consideration == 2) %>% 
  left_join(., tab_study_overview %>% rename(Study = "study_name"), by = "Study")


#############################################################################################*
# Figure S3 ########################
#############################################################################################* 

# proportion of significant-to-total considerations
z <- 
  left_join(risk_factors_by_study,
            tab_study_overview %>% 
              rename(Study = study_name) %>% 
              mutate(N_Patients = as.numeric(str_extract(N_Patients, '(\\d*)'))),
            by = 'Study') %>% 
  group_by(`Adjusting factor`) %>% 
  summarize(Total = n(),
            Significant = sum(Consideration %in% c(1,3), na.rm=T),
            `Non-significant` = sum(Consideration %in% c(2,4), na.rm=T),
            `Not reported` = sum(Consideration %in% c(9,11), na.rm=T)) %>% 
  mutate(`Not considered` = length(unique(d$Study)) - Total) %>% 
  
  # create axis labels with number of studies in parentheses
  mutate(`Adjusting factor label` = str_c(`Adjusting factor`, ' (', Total, ')')) %>% 
  #select(-Total) %>% 
  pivot_longer(cols = c(-`Adjusting factor`, -`Adjusting factor label`), names_to = 'Evaluation', values_to = 'Number') %>% 
  
  # for correct plotting order (gender up)
  mutate(`Adjusting factor` = fct_rev(`Adjusting factor`)) 


 #plot it!
z %>% 
  filter(!Evaluation %in% c('Not considered', 'Total')) %>% 
  ggplot(aes(x=`Adjusting factor`, y=Number, fill=Evaluation)) +
  geom_bar(position = 'fill', stat = 'identity', width = .8) +
  coord_flip() +
  scale_y_continuous(label = scales::percent, expand = expansion(add = c(0,0)), breaks = seq(0,1,.2)) +
  scale_x_discrete(label = element_blank()) + # label = rev(unique(z$`Adjusting factor label`))
  labs(y='Proportion', x= element_blank()) +
  theme_classic() +
  theme(plot.margin = unit(c(5,15,0,0), 'pt')) +
  scale_fill_manual(values = c('black', 'orange', 'red')) -> p1


# proportion of significant-to-total considerations
z %>% 
  filter(!Evaluation %in% c('Total')) %>% 
  ggplot(aes(x=`Adjusting factor`, y=Number, fill=Evaluation)) +
  geom_bar(position = 'fill', stat = 'identity') +
  coord_flip() +
  scale_y_continuous(label = scales::percent, expand = expansion(add = c(0,0)), breaks = seq(0,1,.2)) +
  scale_x_discrete(label = rev(unique(z$`Adjusting factor label`))) + # label = rev(unique(z$`Adjusting factor label`))
  labs(y='Proportion') +
  theme_classic() +
  theme(plot.margin = unit(c(5,15,0,0), 'pt')) +
  scale_fill_manual(values = c('black', 'grey70', 'orange', 'red'), guide = guide_legend(reverse = TRUE)) -> p2


# multi-plot panel with common legend

library(grid)
library(gridExtra)

grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right")) {
    
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
    
  }

grid_arrange_shared_legend(p2, p1) %>% 
  ggsave(path = '.', filename = 'Fig. S3.png', dpi = 600, scale = 1.3, width = 29, height = 18, units = 'cm')


