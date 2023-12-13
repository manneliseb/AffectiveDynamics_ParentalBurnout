#---------------------------------------------------------------#
# Data Wrangling                                                #
#---------------------------------------------------------------#

# This R script contains code importing and joining the multiple datafiles used
# for this project (i.e., ESM & PBA scores for both US & Belgium samples)

# All code and materials for this project can be found here: https://osf.io/5nfbu/

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R packages -----------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# First: check that the top-level directory of the project is correct 
here::i_am("scripts/01_wranglingdata.R") 

# For entire project
library(here) # for relative paths within project
library(tidyverse) 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Importing Data ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### 1.1 Previous dataset 1 (genpop Belgium) --------------------------------------------

# Importing data from previous study (ESM; unselected sample)
  # Full project info with anonymized data here: https://osf.io/e5qru 

Daily_genpop <- readr::read_csv(here::here("data", "raw", "ESM_PB_clean_FromUnselectedSample.csv"))

# Also baseline demographic scores per participant (not publicly available):
demo_genpop <- readr::read_csv(here::here("data", "raw", "ESM_PB_demographic_questionnaires_8w_FromUnselectedSample.csv")) %>%
  dplyr::select(ID, PBA_total)


### 1.2 Previous dataset 2 (PB Belgium) --------------------------------------------

# Importing data from previous study (daily assessments; parents with parental burnout)
  # Full project info: https://osf.io/9zgbw

Daily_PB <- readr::read_csv(here::here("data", "raw", "ESM2_PB_clean_anon.csv"))
demo_PB <- readr::read_csv(here::here("data", "raw", "ESM2_PB_Demographic_questionnaires.csv")) %>%
  dplyr::select(ID, PBA_total)

### 1.3 Dataset 3 (US; daily diary item scales: 0-10) --------------------------------------------
Daily_eng <- readr::read_csv(here::here("data", "raw", "baby emu eod for UCLouvain.csv"))
PBA_eng <- readr::read_csv(here::here("data", "raw", "baby emu pba for UCLouvain.csv"))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Formatting/Merging Data ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### 2.1 Formatting dataset 1 (genpop Belgium) --------------------------------------------

# Formatting names in similar way to PB dataset + reversing pertinent variables (already done for PB dataset...)
Daily_genpop <- Daily_genpop %>%
  dplyr::select (ID, n_obs, Exh = ESM_exhaustion, Dist = ESM_distance, FedUp = ESM_fedup) %>%
  #reversing Distance to match PB dataset
  dplyr::mutate(Dist = 100-Dist) %>%
  # Making observation # variable more relevant to dataset (-> # day)
  dplyr::rename(n_day = n_obs) 


# Adding baseline questionnaire scores (PBA; BDI; BR2; GAD)
Daily_genpop <- merge(Daily_genpop, demo_genpop, by ="ID")

### 2.2 Formatting dataset 2 (Parental Burnout sample Belgium) --------------------------------------------

# Formatting names in similar way to PB dataset + reversing pertinent variables (already done for PB dataset...)
Daily_PB <- Daily_PB %>%
  # Making observation # variable more relevant to dataset (-> # day)
  dplyr::rename( n_day = n_obs) %>%
  dplyr::select (ID, n_day, Exh = ESM_exhaustion, Dist = ESM_distance_Rev, FedUp = ESM_fedup) 

Daily_PB <- merge(Daily_PB, demo_PB, by ="ID")


# Adding baseline questionnaire scores (PBA; BDI; BR2; GAD)
data_joined_Belgium <-  bind_rows(Daily_PB, Daily_genpop) %>%
  mutate(Sample = "Belgium")



### 2.3 Formatting dataset 3 (US sample) --------------------------------------------

# Compute total score for PBA: 
PBA_eng <- PBA_eng %>%
  # US sample PBA scores was automatically saved by Qualtrics on a different
  # scale (1-7 vs 0-6) so subtracting one from all PBA items to match...
  dplyr::mutate(across(PBA_1:PBA_25, ~ .x -1)) %>%
  dplyr::mutate(PBA_total = rowSums(dplyr::select(., contains("PBA_"))))  %>%
  dplyr::rename(ID = PID_masked) %>%
  dplyr::select(ID, PBA_total)


# Make sure variable names are identical
Daily_eng <- Daily_eng %>% dplyr::rename(ID = PID_masked, 
                                     n_day = DAY) %>%
  #Making Closeness into "Emotional Distance" to match other dataset (reversing item)
  dplyr::mutate(Dist = 10 - Close, .keep = "unused")  
  
  
# Padding answers with NAs (i.e., having empty rows on days where participants
# didn't respond to daily survey)
Daily_eng <- Daily_eng %>% 
  dplyr::arrange(ID, n_day) %>%
  tidyr::complete(ID, n_day = 1:21) 

# Merging daily data with PBA data and adding sample identifier (=US)
data_joined_US <- merge(Daily_eng, PBA_eng, by = "ID") %>%
  mutate(Sample = "US")


### 2.3 Merging Belgium & US datasets --------------------------------------------
data_parents <- bind_rows(data_joined_Belgium, data_joined_US)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5) Exporting Data ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
readr::write_csv(data_parents, here::here("data", "processed", "data_parents.csv"))


