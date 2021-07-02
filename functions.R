

# Filter df by two vars before producing table ------------------------

filtered_appendix <- function(data, filter1, filter2) {
  data %>% 
    filter(pt_comparison == filter1 & outcome_type == filter2) %>% 
    select(-pt_comparison, -outcome_type)
}


# Function to clean data ---------------------------------

meta_clean_shiny <- function(converted_data){
  converted_data %>%  
    mutate(cohens_d = as.numeric(cohens_d),
           reverse = as.factor(reverse)) %>% 
    mutate(coded_cohens_d_var = (((n_pt + n_comparison)/(n_pt*n_comparison)) + ((cohens_d^2)/(2*(n_pt+n_comparison))))) %>% #calculated variance by hand for the d's we extracted from articles directly; the ones we converted also came with var calculations
    pivot_longer(c(cohens_d, d.x, d.y, yi, d_values)) %>% #  Wrangling all converted d's into one column
    mutate(d = value) %>% 
    filter(!is.na(d)) %>% 
    dplyr::select(-c(name, value)) %>% 
    pivot_longer(c(coded_cohens_d_var, var_d.x, var_d.y, vi, d_values_var)) %>% #wrangling all variances into one column
    mutate(var = value) %>% 
    filter(!is.na(var)) %>% 
    dplyr::select(-c(name, value)) %>% 
    mutate(df = (n_pt + n_comparison - 2), #getting degrees of freedom
           J = 1- (3/((4*df)-1)), #calculating hedges correction factor for d unbiased
           dunb = J*d, #d unbiased
           var_dunb = ((J^2)*var),  #variance for d unbiased
           lowCI_dunb = dunb-1.96*sqrt(var_dunb), #getting 95% CI's for d unbiased
           upCI_dunb  = dunb+1.96*sqrt(var_dunb)) %>% 
    mutate(dunb = ifelse(reverse == "yes", 
                         dunb*-1,
                         ifelse(reverse == "no",
                                dunb*1, NA))) %>% #reverse scored dunb that needed to be 
    dplyr::select(-c(DOI, Notes, outcome_description_from_coding, target_long_description,
                     weird_sample, F_score, t_score, effect_size_type,
                     effect_direction, p_value, mean_pt, sd_pt, se_pt, 
                     mean_comparison, sd_comparison,se_comparison, 
                     `Note about directionality of cohen's d`)) %>% 
    mutate(sample_number_total = as.factor(sample_number_total),
           pt_comparison = as.factor(pt_comparison),
           outcome_type = as.factor(outcome_type),
           target_ingroup_nonspecific = as.factor(target_ingroup_nonspecific),
           outcome_scale_group = as.factor(outcome_scale_group),
           target_out_minor = as.factor(target_out_minor),
           target_adversary = as.factor(target_adversary),
           target_emapthetic_need = as.factor(target_empathetic_need),
           between_within = as.factor(between_within),
           target_information = as.factor(target_information)) %>% 
    select(authors, Year, effect_size_num, study_num_per_article, sample_number_total, pt_comparison, outcome_type, dunb, var_dunb, n_pt, n_comparison) %>% 
    mutate(pt_comparison = dplyr::recode(pt_comparison,
                                         `1` = "Self PT vs Day control",
                                         `2` = "Self PT vs Other control",
                                         `3` = "Self PT vs Objective",
                                         `4` = "Self PT vs Suppression",
                                         `5` = "Other PT vs Day control",
                                         `6` = "Other PT vs Other control",
                                         `7` = "Other PT vs Objective",
                                         `9` = "Other PT vs Self PT",
                                         `10` = "PT US vs Day control",
                                         `11` = "PT US vs other control",
                                         `12` = "PT US vs Objective",
                                         `13` = "PT US vs Suppression"),
           outcome_type = dplyr::recode(outcome_type,
                                        `3` = "Interpersonal Feels",
                                        `2` = "Overlap",
                                        `1` = "Stereotyping"))
}