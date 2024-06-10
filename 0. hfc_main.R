########################################################################################################
#                                                                                                      #
#                                      PERU CBT -- MAIN                                                #
#                                                                                                      #
########################################################################################################

## PURPOSE      Load, Check, Clean, Construct, and Conduct High Frequency Checks

## AUTHOR       Camila Ramírez

## LAST UPDATE  May 24, 2024


########################################################################################################

## 1. Packages and Settings ----

options(scipen    = 999)

options(max.print = 5000)

options(tibble.width = Inf)

if(!require("pacman")) install.packages("pacman")

github_install <- 0

if(github_install == 1) {
  
  devtools::install_github("tidyverse/googlesheets4")
  
  library(googlesheets4)
  
  devtools::install_github("tidyverse/googledrive")
  
  library(googledrive)
  
} else {
  
  pacman::p_load(googlesheets4, googledrive)
  
}

pacman::p_load(
  
  tidyverse, readstata13, gdata, purrr, lubridate, janitor,
  
  gt, RStata, openxlsx, data.table, devtools, estimatr
  
)

# The below gets rid of package function conflicts, is amazing and why did no one tell me about this

filter    <- dplyr::filter

select    <- dplyr::select

summarize <- dplyr::summarize


########################################################################################################

## 2. Folder Path Variables ----

### Set User

# Change User and add working directory code as needed.

# User 1 -- Camila Ramírez

### Set Round

# Change Round as needed.

# Pilot    -- pilot

# Baseline -- baseline

# Follow up  -- followup

round <- "baseline"

### Create File Path Variables


if(user_num == 1) {
  
  github_filepath          <- "C:/Users/wb615108/OneDrive - WBG/Documents/GitHub/PeruCBT/"
  
  main_onedrive_filepath    <- paste0("C:/Users/wb615108/One-Drive/Peru CBT")
  
  onedrive_filepath         <- paste0(main_onedrive_filepath, "data/")
  
}  

### Decryption

crypt_code_filepath        <- paste0(github_filepath,      "encrypted_code/crypt_code/")

### High Frequency Checks

hfc_code_filepath          <- paste0(github_filepath,      "encrypted_code/hfc_code/")

hfc_code_round_filepath    <- paste0(hfc_code_filepath,    round, "/")

hfc_data_filepath          <- paste0(onedrive_filepath,     "encrypted_data/hfc_data/")

hfc_data_round_filepath    <- paste0(hfc_data_filepath,    round, "/")

#######################################################################################################

### 4. Run R Scripts ----

# Action Dummy Variables

# NOTE - Set up to '1' if you want to run a specific action.

# NOTE -- These processes are separate. You should only run them one at a time.

run <- 1

if(run == 1) {
  
  source(paste0(hfc_code_filepath,   "1.hfc_construct.R"))
  
  print("Script 1 Done.")
  
  source(paste0(hfc_code_filepath,   "2.hfc_enum.R"))
  
  # In case googledrive and googlesheets4 packages ask for '1' to confirm account re-authentication
  
  1
  
  1
  
  print("Script 2 Done.")
  
  source(paste0(hfc_code_filepath,   "3.hfc_outliersn.R"))
  
  print("Script 3 Done.")
  
  source(paste0(hfc_code_filepath,   "4.hfc_stats.R"))
  
  print("Script 4 Done.")
  
  # NOTE -- RStata and veracrypt hate each other. Boo. So need to run this directly in Stata. Ugh.
  
  #stata(paste0(crypt_code_filepath,  "encrypt_data.do"))
  
  #  print("Encryption Done.")
  
}
