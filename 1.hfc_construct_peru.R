
########################################################################################################
#                                                                                                      #
#                                HIGH-FREQUENCY CHECKS  -- CONSTRUCT/CLEAN                             #
#                                                                                                      #
########################################################################################################


## PURPOSE      High-frequency checks

## LAST UPDATE  May 29, 2024


########################################################################################################

## libraries
library(tidyverse)
library(lubridate)
library(googlesheets4)
library(readxl)


# 1. Import Data ----
main_folder <- 'C:/Users/wb615108/OneDrive - WBG/Documents/Peru/'

## Raw High Frequency Data (REMEMBER TO REENCRYPT)
raw_data <- read.csv(paste0(main_folder, "baseline_data.csv"),
                 stringsAsFactors = FALSE)


# Variables submissiondate, starttime, endtime:
date_variables_dmy <- raw_data %>%
  
  select(
    
    SubmissionDate, starttime, endtime, consent_start, consent_end
    
  )%>%
  
  names()

raw_data <- raw_data %>%
  mutate(
    across(
      date_variables_dmy,
      
      ~ (mdy_hms(.,
                 
                 tz = Sys.timezone() ))))

# Filter out observations from pilot:
raw_data <- raw_data %>% filter(lubridate::date(SubmissionDate) > as.Date("2024-04-29") )

raw_data <- raw_data %>% filter(id_number != 999)

## 2. Overall Construction ----

# Check IDs:

raw_data$issue_id <- 1- as.numeric(raw_data$hh_id == raw_data$hh_id2)

hfc_constr <- raw_data %>%
  
  group_by(hh_id) %>%
  
  mutate(n_attempts = n()) %>%
  
  mutate(
    
    indiv_id = case_when( # To have something to match later down if NA ID
      
      is.na(hh_id) ~ paste0("9999", row_number()),
      
      n_attempts > 1 ~ paste0(hh_id, letters[row_number()]),
      
      TRUE           ~ as.character(hh_id)
      
    )
    
  ) %>%
  
  ungroup() %>%
  
  select(indiv_id, everything())


## Date Stuff - extracting dates from date-time variable and creating duration for consent module

hfc_constr <- hfc_constr %>%
  
  mutate(
    
    submissiondatetime     = SubmissionDate,
    
    submissiondate         = lubridate::date(submissiondatetime),
    
    startdate              = lubridate::date(starttime),
    
    enddate                = lubridate::date(endtime)  ) 


hfc_constr <- hfc_constr %>%
  
  mutate(
    
    consent_duration_module = as.numeric(difftime(consent_end, consent_start, units = "mins"))
    
  )

## Enumerator Names 

enumerators <- read.csv(paste0(main_folder, "Datos_Encuestadores.csv"),
         stringsAsFactors = FALSE)

# add enumerator name 
hfc_constr = hfc_constr %>% 
  left_join(enumerators, by=c('enumerator_id'='dni_key'))

# Recoding NAs:

hfc_constr <- hfc_constr %>% 
  mutate(assistance_prev_numeric = case_when((assistance_type_prev==1|assistance_type_prev ==2) ~ 1, 
                                               assistance_type_prev == 3 ~ 0, 
                                               TRUE ~ NA))

vars_to_change <- hfc_constr %>% select (mobile_phone_number_1, whatsapp_number_1, assistance_prev_numeric, fies_1, 
                                         fies_2, fies_3, fies_4, fies_5, fies_6, fies_7, fies_8, 
                                         cereals, legumes, dairy, meat, vegetables, fruits, oil, sugar, loan, 
                                         coping_food_1, coping_food_2, coping_food_3, coping_food_4, coping_food_5) %>%
  names()

hfc_constr <- hfc_constr %>% 
  mutate(
    across(
      all_of(vars_to_change),
      ~ case_when(
        .x == -77 | .x == -99 | .x == -66 | .x == -88 ~ NA_real_,
        TRUE ~ as.numeric(.x)
      )
    )
  )

hfc_constr <- hfc_constr %>% mutate(
  
  contact = as.numeric(!is.na(mobile_phone_number_1) | !is.na(whatsapp_number_1)), 
  
  facebook_url_1_missing = as.numeric(is.na(facebook_url_1)), 
  
  fies_score_reversed = 8 - rowSums(select(., matches("^fies_[1-8]$")), na.rm = TRUE)
  ,
  
  location_name = case_when(
      survey_location == 1 ~ "Desaguadero",
      survey_location == 2 ~ "Arequipa",
      survey_location == 3 ~ "Lima"
  ), 
  assistance_name = case_when(
    assistance_type == 1 ~ "Sodexo",
    assistance_type == 2 ~ "Zinli", 
    assistance_type == 3 ~ "None", 
    TRUE ~ NA_character_
  ), 
  
  rcsi_score = coping_food_1 + 2 * coping_food_2 + coping_food_3 + coping_food_4 + 3*coping_food_5
  )

hfc_constr <- hfc_constr %>%
  rowwise() %>%
  mutate(incidents_1_sum = sum(c_across(matches("incidents_([1-9]|1[0-4])_1|incidents__81_1")), na.rm = TRUE),
         members_reported_mobile_phone = sum(!is.na(c_across(matches("mobile_phone_number_([2-9])")))),
         members_reported_whatsapp = sum(!is.na(c_across(matches("whatsapp_number_([2-9])"))))
  ) %>%
  ungroup()


hfc_constr <- hfc_constr %>%
    mutate(
      fcs_score =
        2   * cereals  +
        3   * legumes  +
        4   * dairy    +
        4   * meat     +
        1   * vegetables +
        1   * fruits  +
        0.5 * oil +
        0.5 * sugar
)

# Checking if last country is Peru
hfc_constr <- hfc_constr %>% 
  mutate(across(starts_with("country_name1_"), ~ na_if(trimws(.), "")))

long_data <- hfc_constr %>%
  select(indiv_id, starts_with("country_name1_")) %>%
  pivot_longer(
    cols = starts_with("country_name1_"),
    names_to = "country_var",
    values_to = "country_name"
  )

result <- long_data %>%
  filter(!is.na(country_name)) %>%
  group_by(indiv_id) %>%
  summarise(last_country = last(country_name)) %>%
  mutate(last_is_peru = as.numeric(last_country == "Peru"))

hfc_constr <- hfc_constr %>%
  left_join(result, by = "indiv_id")

# 3. Adding Treatment

treatment <- read_excel("Peru/Treatment.xlsx")

treatment_long <- treatment %>% 
  pivot_longer(
    cols = c("Desaguadero", "Arequipa"),
    names_to = "location_name",
    values_to = "treatment"
  ) %>%
  rename(submissiondate = SubmissionDate)

hfc_constr <- hfc_constr %>%
  left_join(treatment_long, by = c("submissiondate", "location_name"))

hfc_constr <- hfc_constr %>% 
  mutate(
    treatment_compliance = as.numeric(assistance_name == treatment)
  )

## 4. Export Data duplicates files ----

hfc_sheet <- googledrive::drive_get(paste0("hfc_peru"))

1

duplicates = hfc_constr %>% filter(consent==1) %>% 
  group_by(hh_id) %>% mutate(n=n()) %>% 
  filter(n>1)

duplicates <- duplicates %>% select(SubmissionDate, enumerator_id, hh_id)

hfc_sheet %>%
  
  sheet_write(data = duplicates, sheet = "Duplicates")

1

# export duplicates
write.csv(duplicates,
          file.path(main_folder,
                    'duplicates.csv'),
          row.names=F)


# Export main data

write.csv(hfc_constr, paste0(main_folder, "hfc_data_constr.csv"), row.names = FALSE)

