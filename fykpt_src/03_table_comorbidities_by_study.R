

df <- rio::import('table_comorbidities_by_study.xlsx') %>% as_tibble()
#glimpse(df)

### code checker
# icd::icd10_map_elix %>% rlist::list.search(grepl('C77.*$', .), unlist = T) %>% as.data.frame() %>% filter(. == TRUE)
# icd10_map_elix$Mets

# make long 
df_long <- 
  df %>% 
  pivot_longer(cols = -c(Nbr, Comorbidity), names_to = 'Study', values_to = 'Value')

# extract values, nummeric
df_long <- 
  df_long %>% 
  rowwise() %>% 
  mutate(OR = (str_match(Value, '^(\\d+,\\d+)')[2]) %>% str_replace(., ',', '.') %>% as.numeric()) %>% 
  mutate(CI_LL = (str_match(Value, '\\((\\d+,\\d+)-')[2]) %>% str_replace(., ',', '.') %>% as.numeric()) %>% 
  mutate(CI_UL = (str_match(Value, '-(\\d+,\\d+)\\)')[2]) %>% str_replace(., ',', '.') %>% as.numeric()) %>% 
  filter(Comorbidity != 'Definition type') %>% 
  arrange(desc(Comorbidity)) %>% 
  mutate(Comorbidity = fct_inorder(Comorbidity))

# calculate nudge values of points to avoid overplotting
# (1.6 ... nudge range, .8 ... positive offset)
# starting from nudge +.8 step down study by study to nugde = -.8
nudge_difference <-  0.6/(length(unique(df_long$Study))-1) * -1
df_nudge <- 
  df_long %$% 
  unique(Study) %>% 
  as_tibble() %>% 
  rename(Study = value) %>% 
  mutate(nudge_x_value = (row_number()-1)  * nudge_difference + 0.3) 
df_long <- left_join(df_long, df_nudge, by = 'Study')

# make study descriptor nice :)
df_long <- 
  df_long %>% 
  tidyr::extract(Study, c('Name', 'Year'), '(.+)(\\d{4})') %>% 
  tidyr::unite(Study, c(Name, Year), sep = ', ')


# get number of patients (from 'table_overview_of_studies.R')
df_long <- 
  left_join(df_long,
            tab_study_overview %>% 
              select(study_name, N_Patients) %>%
              rename(Study = study_name) %>% 
              mutate(N_Patients = as.numeric(str_extract(N_Patients, '(\\d*)'))),
            by = 'Study')

# infere standard errors from confidence intervals
# df_long <- df_long %>%
#   mutate(seLL = abs(log(CI_LL))) %>%
#   mutate(seUL = abs(log(CI_UL))) %>%
#   rowwise %>%
#   mutate(diff = max(seLL, seUL) - min(seLL, seUL)) %>%
#   mutate(se = diff / (qnorm(.975)*2)) %>%
#   mutate(lOR = log(OR)) %>% 
#   ungroup()  



# preparation for meta-analytic integration -------------------------------

# see here: https://www.metafor-project.org/doku.php/tips:assembling_data_or
df_long <- df_long %>%
  rowwise() %>% 
  mutate(vi = ((log(CI_UL)-log(CI_LL))/( qnorm(.975) *2))^2) %>% 
  mutate(yi = log(OR)) %>% 
  ungroup()  

### test a single comorbidity condition
# df_long %>% 
#   filter(Comorbidity == 'Renal failure' & !is.na(OR)) %>% 
#   metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')->bb

meta_beta <- 
  df_long %>% 
  filter(!is.na(Value)) %>% 
  group_by(Comorbidity) %>% 
  group_modify(~metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')[['beta']] %>% as.numeric %>% exp %>% enframe, .keep = F)%>% 
  select(-name)%>% 
  rename('beta' = value)

meta_cilb <- 
  df_long %>% 
  filter(!is.na(Value)) %>% 
  group_by(Comorbidity) %>% 
  group_modify(~metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')[['ci.lb']] %>% as.numeric %>% exp %>% enframe, .keep = F) %>% 
  select(-name)%>% 
  rename('CI.LB' = value)

meta_ciub <- 
  df_long %>% 
  filter(!is.na(Value)) %>% 
  group_by(Comorbidity) %>% 
  group_modify(~metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')[['ci.ub']] %>% as.numeric %>% exp %>% enframe, .keep = F)%>% 
  select(-name) %>% 
  rename('CI.UB' = value)

meta_i2 <- 
  df_long %>% 
  filter(!is.na(Value)) %>% 
  group_by(Comorbidity) %>% 
  group_modify(~metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')[['I2']] %>% 
                 as.numeric %>% papaja::printnum(., digits = 1) %>% str_c(., '%')  %>% enframe, 
               .keep = F) %>% 
  select(-name) %>% 
  rename('I2' = value)

meta_h2 <- 
  df_long %>% 
  filter(!is.na(Value)) %>% 
  group_by(Comorbidity) %>% 
  group_modify(~metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')[['H2']] %>% 
                 as.numeric %>% papaja::printnum(., digits = 1) %>% str_c(., '%') %>% enframe, 
               .keep = F)%>% 
  select(-name) %>% 
  rename('H2' = value)

meta_tau2 <- 
  df_long %>% 
  filter(!is.na(Value)) %>% 
  group_by(Comorbidity) %>% 
  group_modify(~metafor::rma(yi = yi, vi = vi, data = ., method = 'REML', measure = 'OR')[['tau2']] %>%                 
                 as.numeric %>% papaja::printnum(., digits = 1) %>% str_c(., '%') %>% enframe, 
               .keep = F)%>% 
  select(-name) %>% 
  rename('tau2' = value)

df_long <- plyr::join_all(list(df_long, meta_beta, meta_cilb, meta_ciub, meta_i2, meta_h2, meta_tau2), by = 'Comorbidity') %>% 
  mutate(Study = as.factor(Study))


###########################################################################*
# points & meta-analytic component ----------------------------------------
###########################################################################*
  
  df_long %>% 
    filter(!is.na(Value)) %>% 
    group_by(Comorbidity) %>% 
    summarize (Anzahl = n()) %>% 
    arrange(Anzahl)
  
  
  df_long %>% 

    # preparation for plotting
    # ::::::::::::::::::::::::::::::::
    # number of patients for weighting
    #left_join(tab_study_overview %>% rename(Study = 'study_name'), by = 'Study') %>% 
    mutate(N_Patients = as.numeric(str_extract(N_Patients, '(\\d*)'))) %>% 
    # reorder Comorbidities by efffect size (instead of alphabetically)
    mutate(Comorbidity = fct_reorder(Comorbidity, beta)) %>% 
    # exclude missings
    filter(!is.na(Value)) %>% 
    # point color to indicate significance
    mutate(point_color = if_else(CI_LL < 1 & CI_UL > 1, 'black', 'red')) %>% 

    # plotting
    # ::::::::::::::::::::::::::::::::
    ggplot(aes(x=Comorbidity, y=OR)) +#fct_reorder(Comorbidity, median_density)
    # points and violins
    #geom_violin(aes(weight = N_Patients), scale = 'width', trim = F, draw_quantiles = c(.5)) + 
    
    # meta-analytic overall effect
    geom_crossbar(aes(x=Comorbidity, y=beta, ymin=CI.LB, ymax=CI.UB), 
                  width=.7, color = 'grey50') +
    
    geom_point(aes(color=point_color, size=N_Patients), alpha = .6) +
    geom_hline(yintercept = 1, linetype = 'dashed', color = 'grey50') +
    
    # additional comments: OR
    annotate('text', x= meta_beta$Comorbidity, y = 16, label = papaja::printnum(meta_beta$beta, digits = 2),  size = 3.4) + 
    annotate('text', x= meta_beta$Comorbidity, y = 17.2, size = 3.4,
             label = str_c(
               '(',papaja::printnum(meta_cilb$CI.LB, digits = 2),  
               '-',papaja::printnum(meta_ciub$CI.UB, digits = 2), ')'
             )) +
    annotate('text', x= meta_i2$Comorbidity, y = 19, label = papaja::printnum(meta_i2$I2, digits = 2),  size = 3.4, hjust = 1) +
    
    # additional comments: heading
    annotate('text', x = 29.5, y = 16,   label = 'atop(bold("OR"))', size = 3.4, parse=T) + 
    annotate('text', x = 29.5, y = 17.2, label = 'atop(bold("95%-CI"))', size = 3.4, parse=T) +
    annotate('text', x = 29.5, y = 18.6, label = 'atop(bold("I?"))', size = 3.4, parse=T) +
    
    # styling
    coord_flip() +
    theme_classic() +
    # expand range to the right for additional comments
    scale_y_continuous(limits = c(0,19.5), breaks = seq(0,15,1), expand = expansion(add = c(0,0))) +
    # expand range on top for additional comments
    scale_x_discrete(expand = expansion(add = c(1,1.5))) +
    # tweak colors and labels of points
    scale_color_manual(values = c('black', 'red'), labels = c('not significant', 'significant')) +
    scale_size_continuous(labels = scales::comma) +
    labs(x='Comorbidity', y='Effect size (OR)', color='Significance', size='Study size\n(no. of patients)') +
    # axis text in black
    theme(axis.text = element_text(color = 'black', size = 10)) +
    scale_alpha_continuous(guide = NULL)
  
  ggsave(path = '.', filename = 'Fig. 3.png', dpi = 600, scale = 1.2, width = 29, height = 18, units = 'cm')
  
  