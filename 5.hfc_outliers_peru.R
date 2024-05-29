########################################################################################################
#                                                                                                      #
#            HIGH-FREQUENCY CHECKS AND PHONE TRACKING CODE -- OUTLIER HIGH-FREQUENCY CHECKS            #
#                                                                                                      #
########################################################################################################

## PURPOSE      Create an outlier high-frequency check sheet in the HFC dashboard.

## LAST UPDATE  May 29, 2024

########################################################################################################

## 1. Import Data ----

main_folder <- 'C:/Users/wb615108/OneDrive - WBG/Documents/Peru/'

## Raw High Frequency Data (REMEMBER TO REENCRYPT)
hfc_constr <- read.csv(paste0(main_folder, "hfc_data_constr.csv"),
                       stringsAsFactors = FALSE)

########################################################################################################

# 2. Create Variable-Specific Outlier Dataset Function ----

outlier_function <- function(outlier_var) {
  
  temp_summary <- hfc_constr %>%
    
    select(!!outlier_var) %>%
    
    mutate(
      
      across(
        
        outlier_var,
        
        ~ case_when(
          
          (.x == -66 | .x == -99 | .x ==-77 | .x ==-88) ~ NA_real_,
          
          TRUE                    ~ as.numeric(.x)
          
        )
        
      )
      
    ) %>%
    
    summarize(
      
      across(
        
        outlier_var,
        
        ~ list(mean = mean(.x, na.rm = TRUE), sd = sd(.x, na.rm = TRUE), q1 = unname(quantile(.x, 0.25, na.rm = TRUE)),
               
               q3 = unname(quantile(.x, 0.75, na.rm = TRUE)))
        
      )
      
    ) %>%
    
    pluck(1)
  
  #### Bounds
  
  upper_bound            <- temp_summary$mean + 1.96 * temp_summary$sd 
  
  lower_bound            <- temp_summary$mean - 1.96 * temp_summary$sd   
  
  temp_check <- hfc_constr %>%
    
    mutate(
      
      across(
        
        outlier_var,
        
        ~ case_when(
          
          (.x == -66 | .x == -99 | .x == -77 | .x == -88) ~ NA_real_,
          
          TRUE                    ~ as.numeric(.x)
          
        )
        
      )
      
    )
  
  
  
  temp_check <- temp_check %>%
    
    filter(
      
      across(
        
        outlier_var,
        
        ~ .x < lower_bound  | .x > upper_bound
        
      )
      
    ) %>%
    
    mutate(
      
      issue_var              = outlier_var,
      
      mean                   = round(temp_summary$mean, 2),
      
      sd                     = round(temp_summary$sd, 2),
      
      lower_bound  = round(lower_bound , 2),
      
      upper_bound = round(upper_bound , 2)
      
    ) %>%
    
    rename(issue_value = !!outlier_var) %>%
    
    select(
      
      hh_id, enumerator_id, enumerator_name,SubmissionDate, issue_var, issue_value,
      
      mean, sd, lower_bound, upper_bound
      
    )
  
  return(temp_check)
  
}


########################################################################################################

# 3. Create Final Outlier Dataset ----

## Keeping relevant variables

outlier_vars <- hfc_constr %>%
  
  select(
    ## Household information module
    assistance_prev_numeric, hh_total_no, age_1, #The age_hh_1 for some reason is all NA, and we have age_1 instead
    non_fam_number, thermomether_test, fcs_score, fies_score_reversed, rcsi_score,
    assistance_amount, wb_anxiety, wb_worried, wb_depressed, wb_lack_interest, 
    loan_amount, members_reported_mobile_phone, members_reported_whatsapp
  ) %>%

  names()


## Looping over each selected variable to check for outliers

for(i in 1:length(outlier_vars)) {
  
  temp_outlier_check <- outlier_function(outlier_vars[i])
  
  if(i == 1) {
    
    outlier_check <- temp_outlier_check
    
  } else {
    
    outlier_check <- outlier_check %>%
      
      bind_rows(temp_outlier_check)
    
  }
  
  print(paste0("Variable ", outlier_vars[i], " done."))
  
}

########################################################################################################

# 4. Export Data ----

hfc_sheet %>%
  
  sheet_write(data = outlier_check, sheet = "outlier_data")

# In case googlesheets4 asks me to enter '1' to continue

1


# 
# export outlier list
write.csv(outlier_check,
          file.path(main_folder,
                    'outliers.csv'),
          row.names=F)   
