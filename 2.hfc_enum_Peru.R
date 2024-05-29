########################################################################################################
#                                                                                                      #
#                     HIGH-FREQUENCY CHECKS  -- ENUMERATOR HIGH-FREQUENCY CHECKS                       #
#                                                                                                      #
########################################################################################################

## PURPOSE      Create an enumerator high-frequency check sheet in the HFC dashboard.

## LAST UPDATE  May 29, 2024

main_folder <- 'C:/Users/wb615108/OneDrive - WBG/Documents/Peru/'

## Raw High Frequency Data (REMEMBER TO REENCRYPT)
hfc_constr <- read.csv(paste0(main_folder, "hfc_data_constr.csv"),
                     stringsAsFactors = FALSE)


########################################################################################################


# 2. Create Enumerator Datasets ----


sample_enum <- hfc_constr %>%
  
  filter(!is.na(enumerator_id) ) %>%
  
  group_by(enumerator_id) %>%
  
  summarize(total = n()) %>%
  
  ungroup() %>%
  
  select(enumerator_id, total) %>%
  
  distinct() %>%
  
  right_join(hfc_constr, by = c("enumerator_id")) 


### Enumerator Progress and Duration Dataset

enum <- sample_enum %>%
  
  filter(!is.na(enumerator_id) ) %>%
  
  group_by(hh_id) %>%
  
  mutate(
    
    duration_mins = sum(duration/60, na.rm = TRUE) # in minutes
    
  ) %>%
  
  ungroup() %>%
  
  group_by(enumerator_id) %>%
  
  mutate(n = n())



enum <- enum %>%
  
  summarise(
    
    survey_num     = first(n),#, na.rm = TRUE
    
    total_num      = first(total), #, na.rm = TRUE
    
    ave_duration_mins   = round((mean(duration/60, na.rm = TRUE)), 2),
    
    ave_duration_mins   = round((mean(duration/60, na.rm = TRUE)), 2),
    
    count_short_surveys     = sum(duration < 30, na.rm = TRUE), # Count of surveys with duration less than 30 minutes
    
    across(matches("_dur$"), ~round(mean(as.numeric(.x), na.rm = TRUE), 2))
    
  ) 

### Daily Enumerator Dataset

enum_daily <- sample_enum %>%
  
  filter(!is.na(enumerator_id) ) %>%
  
  group_by(hh_id) %>%
  
  mutate(
    
    duration_mins  = sum(duration/60, na.rm = TRUE),
    
  ) %>%
  
  ungroup() %>%
  
  select(enumerator_id, enddate) %>%
  
  group_by(enumerator_id, enddate) %>%
  
  count() %>%
  
  ungroup() %>%
  
  arrange(enddate) %>%
  
  pivot_wider(names_from = enddate, values_from = n)


# NOTE -- The below collects information on what happened during the latest day of data collection,

### Enumerator Last Day Dataset

last_day <- sample_enum %>%
  
  filter(!is.na(enumerator_id) ) %>%
  
  group_by(hh_id) %>%
  
  mutate(
    
    duration = sum(duration/60, na.rm = TRUE),
    
  ) %>%
  
  ungroup()   %>%
  
  filter(enddate == max((enddate), na.rm = TRUE)) %>%
  
  group_by(enumerator_id) %>%
  
  mutate(n = n()) %>%
  
  summarize(
    
    last_date          = unique(enddate),
    
    ld_survey_num      = first(n),#, na.rm = TRUE
    
    ld_ave_duration    = round((mean(duration, na.rm = TRUE)), 2),
    
  )


### Enumerator Consent Dataset


consent <- sample_enum %>%
  
  mutate(
    
    consent_asked = case_when(
      
      !is.na(consent) ~ 1,
      
      TRUE                     ~ 0
      
    )
    
  ) %>%
  
  group_by(enumerator_id) %>%
  
  summarize(
    
    consent_secured = sum(consent, na.rm = TRUE),
    
    consent_asked   = sum(consent_asked,    na.rm = TRUE),
    
    consent_duration_average = round((mean(consent_duration_module, na.rm = TRUE)), 2),

    
  )

# ID_03: a_enum_name
### Enumerator Don't Know / Refused to Respond Dataset

# NOTE -- The below calculates the percentage of 'Don't know' (-99) and 'refused to respond' (-66) answers
# for each enumerator.

dkrtr_rates <- sample_enum %>%
  
  select(enumerator_id, where(is.numeric)) %>%
  
  mutate(
    
    ncols = rowSums(across(everything(), ~ !is.na(.)), na.rm = TRUE),
    
    dk    = rowSums(across(where(is.numeric), ~ . == '-99'), na.rm = TRUE),
    
    rtr   = rowSums(across(where(is.numeric), ~ . == '-66'), na.rm = TRUE),
    
    dontknow_rate  = round(dk  / ncols, 4),
    
    refusedtorespond_rate = round(rtr / ncols, 4)
    
  ) %>%
  
  group_by(enumerator_id) %>%
  
  summarize(
    
    dontknow_rate  = round(mean(dontknow_rate,  na.rm = TRUE), 4),
    
    refusedtorespond_rate = round(mean(refusedtorespond_rate, na.rm = TRUE), 4)
    
  )


### 


vars_to_check <- hfc_constr %>%
  
  select(
    ## Household information module
    assistance_prev_numeric, hh_total_no, contact, facebook_url_1_missing,
    number_countries, last_is_peru, fcs_score, fies_score_reversed, rcsi_score,
    wb_anxiety, wb_worried, wb_depressed, wb_lack_interest,
    treatment_compliance, loan, members_reported_mobile_phone, 
    members_reported_whatsapp
    ) %>%
  
  names()


enum_vars <- hfc_constr %>% 
  
  filter(!is.na(enumerator_id) ) %>% 
  
  mutate(
    
    across(
      
      c(vars_to_check),
      
      ~ case_when(
        
        .x == -77 | .x == -99 | .x == -66 | .x == -88 ~ NA_real_,
        
        TRUE                              ~ as.numeric(.x)
        
      )
      
    )
    
  ) %>%
  
  group_by(enumerator_id) %>%
  
  summarize(
    
    across(
      
      c(vars_to_check),
      
      ~ round(mean(.x, na.rm = TRUE), 0)
      
    )
    
  ) 

enum_vars <- hfc_constr %>% 
  
  filter(!is.na(enumerator_id) ) %>% 
  
  mutate(
    
    across(
      
      c(vars_to_check),
      
      ~ case_when(
        
        .x == -77 | .x == -99 | .x == -66 | .x == -88 ~ NA_real_,
        
        TRUE                              ~ as.numeric(.x)
        
      )
      
    )
    
  ) %>%
  
  group_by(enumerator_id) %>%
  
  summarize(
    
    across(
      
      c(vars_to_check),
      
      ~ round(mean(.x, na.rm = TRUE), 0)
      
    )
    
  ) 
########################################################################################################

# 3. Create Final Enumerator Dataset ----

# NOTE -- The below brings everything together and creates proportions for the 'outcome' and consent
# variables.

enum <- enum %>%
  
  left_join(enumerators,by=c('enumerator_id'='dni_key')) %>% 
  
  left_join(last_day) %>%
  
  left_join(consent) %>%
  
  left_join(dkrtr_rates) %>%
  
  left_join(enum_vars) %>%
  
  left_join(enum_daily) 

enum <- enum %>% relocate(nombre, .after=enumerator_id)

########################################################################################################

# 4. Export Data ----

# NOTE -- The below is what exports the data to Google Sheets - for IE team and Survey Firm

hfc_sheet %>%
  
  sheet_write(data = enum, sheet = "enum_data")

# In case googlesheets4 asks me to enter '1' to continue

1


