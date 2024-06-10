########################################################################################################
#                                                                                                      #
#                HIGH-FREQUENCY CHECKS AND PHONE TRACKING CODE -- DESCRIPTIVE STATISTICS               #
#                                                                                                      #
########################################################################################################

## PURPOSE      Create a descriptive statistics sheet in the HFC dashboard.

## LAST UPDATE  May 26, 2024


########################################################################################################


## 1. Import Data ----

main_folder <- 'C:/Users/wb615108/OneDrive - WBG/Documents/Peru/'

## Raw High Frequency Data (REMEMBER TO REENCRYPT)
hfc_constr <- read.csv(paste0(main_folder, "hfc_data_constr.csv"),
                       stringsAsFactors = FALSE)


########################################################################################################

# 2. Count number of surveys per day, per location}

locationcounts <- hfc_constr %>% select("SubmissionDate", "consent_yes", "survey_location", "contact")

locationcounts <- locationcounts %>%
  mutate(SubmissionDate = as.Date(SubmissionDate))


# Group by survey_location and SubmissionDate, then calculate counts based on conditions
submission_counts <- locationcounts %>%
  group_by(survey_location, SubmissionDate) %>%
  summarise(
    total = n(),
    proportion_consent_yes = round(sum(consent_yes == 1)/n(), 2),
    proportion_contact_yes = round(sum(contact == 1)/n(), 2),
    .groups = 'drop'
  )

# Create human-readable names for locations
submission_counts <- submission_counts %>%
  mutate(location_name = case_when(
    survey_location == 1 ~ "Desaguadero",
    survey_location == 2 ~ "Arequipa",
    survey_location == 3 ~ "Lima"
  ))

# Reshape the data
reshaped_data <- submission_counts %>%
  select(-survey_location) %>%
  pivot_wider(
    names_from = location_name,
    values_from = c(total, proportion_consent_yes, proportion_contact_yes),
    names_glue = "surveys_{location_name}_{.value}",
    values_fill = list(total = 0, proportion_consent_yes = 0, proportion_contact_yes = 0)
  )




# 3. Create Descriptive Stats Dataset ----

# Descriptive variables may differ if needed across countries

desc_stat_vars <- hfc_constr %>% 
  
  select(
    
    ## Household information module
    assistance_prev_numeric, hh_total_no, contact, facebook_url_1_missing,
    number_countries, last_is_peru, fcs_score, fies_score_reversed, rcsi_score,
    wb_anxiety, wb_worried, wb_depressed, wb_lack_interest,
    treatment_compliance, loan, non_fam_number, thermomether_test,
    assistance_amount, members_reported_mobile_phone, 
    members_reported_whatsapp
    
  )  %>%
  
  names()


#desc_stat_vars = hfc_data %>% select(c_00_1b) %>% names()  

for(i in 1:length(desc_stat_vars)) {
  
  desc_stat_var <- desc_stat_vars[i]
  
  temp_desc <- hfc_constr %>%
    
    select(!!desc_stat_var)
  
  
  temp_desc <- temp_desc %>%
    
    mutate(
      
      across(
        
        desc_stat_var,
        
        ~ case_when(
          
          (.x == -66 | .x == -99|.x == -77| .x == -88 ) ~ NA_real_,
          
          TRUE                    ~ as.numeric(.x)
          
        )
        
      )
      
    ) %>%
    
    summarize(
      
      variable = desc_stat_var,
      
      mean     = round(mean(!!sym(desc_stat_var), na.rm = TRUE), 2),
      
      sd       = round(sd(!!sym(desc_stat_var), na.rm = TRUE), 2),
      
      min      = round(min(!!sym(desc_stat_var), na.rm = TRUE), 2),
      
      max      = round(max(!!sym(desc_stat_var), na.rm = TRUE), 2),
      
      quant    = round(quantile(!!sym(desc_stat_var), c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = TRUE), 2),
      
      q        = c(5, 25, 50, 75, 95)
      
    ) %>%
    
    mutate(
      
      across(
        
        c(min, max),
        
        ~ case_when(
          
          is.infinite(.x) ~ NA_real_,
          
          TRUE ~ as.numeric(.x)
          
        )
        
      )
      
    ) %>%
    
    pivot_wider(
      
      names_from   = q,
      
      names_prefix = "quant_",
      
      values_from  = quant
      
    ) %>%
    
    select(variable:sd, min, quant_5:quant_95, max)
  
  if(i == 1) {
    
    desc_stats <- temp_desc
    
  } else {
    
    desc_stats <- desc_stats %>%
      
      bind_rows(temp_desc)
    
  }
  
}



########################################################################################################

# 4. Export Data ----

hfc_sheet %>%
  
  sheet_write(data = reshaped_data, sheet = "surveys_by_location")

## If google sheets asks to input 1 to continute
1

hfc_sheet %>%
  
  sheet_write(data = desc_stats, sheet = "desc_stats_data")

## If google sheets asks to input 1 to continute
1
########################################################################################################


