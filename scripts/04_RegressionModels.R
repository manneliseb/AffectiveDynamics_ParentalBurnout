#---------------------------------------------------------------#
# Regression Models                                             #
#---------------------------------------------------------------#
# All code and materials for this project can be found here: https://osf.io/5nfbu/

# This R script contains code to generate the regression models, examining how
# the affective indices predict parental burnout severity. 
# All models include assumption checks.

# Models included: 
# - The main regression model (presented in the manuscript, with relative SD as 
#   the operationalization of variation & multilevel AR(1)) as the
#   operationalization of inertia)
# - multiple sensitivity analyses (varying the operationalizations of affective 
#   indices, as well as if models are computed with both samples or separated 
#   by sample). 
# - When exporting models, sensitivity analyses also included performing analyses 
#   both with the regular regression models & via bootstrapping.

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R packages -----------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(performance) # for model assumption checks
library(see) # to visualize plots for above
library(sjPlot)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Importing Data -------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_indices <- readr::read_csv(here::here("data", "processed", "data_indices.csv"))

# Creating dataset for regression models

# First: ID participants with zero variance (NA for relative SD, so with
# listwise deletion will be excluded overall; take them out for all...)
pns_zerovar <- data_indices %>%
  dplyr::filter(mean_Exh == 0 | mean_Dist == 0 | mean_FedUp == 0) %>%
  dplyr::distinct(ID, .keep_all = T) 

# Create dataset
df_regression <- data_indices %>%
  dplyr::group_by(ID) %>%
  dplyr::select(-c(n_day:FedUp, Exh_scaled:FedUp_scaled)) %>% 
  dplyr::slice(1) %>%
  # remove participants with SD = 0 (will be automatically removed from models
  # with relative SD in any case, bc of listwise deletion)
  filter(!ID %in% pns_zerovar$ID)


# Create dummy variable for sample type (living in Belgium or not)
df_regression <- df_regression %>%
  mutate(Belgium = ifelse(Sample == "Belgium", 1, 0))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Main Analysis Model -----------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


### Model 1: Just mean-levels (joint sample) --------------------------------------------

model1_means <- lm(PBA_total ~ 
                     mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled,
                     data = df_regression)


  # Check assumptions of model
  performance::check_model(model1_means)
  performance::check_heteroscedasticity(model1_means) 
    # Heteroscedasticity (non-constant error variance) detected (p < .001).
  performance::check_normality(model1_means) 
    # Non-normality of residuals detected (p < .001)

  # Because of heteroscedasticity & non-normality, will recompute parameter
  # estimates with bootstrapping (more robust against these violations)
  # Will do this through sjPlot package when exporting tables 
  
  

### Model 1: Dummy variable with just mean-levels (joint sample) --------------------------------------------
model1_means_dummy <- lm(PBA_total ~ Belgium +
                           mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled,
                         data = df_regression)


# Check assumptions of model
performance::check_model(model1_means_dummy)
performance::check_heteroscedasticity(model1_means_dummy) 
# Heteroscedasticity (non-constant error variance) detected (p < .001).
performance::check_normality(model1_means_dummy) 
# Non-normality of residuals detected (p = 0.015)



### Model 2: Affect Dynamic predictors with dummy variable for Sample type --------------------------------------------

# Included indices: 
# - variability: relative SD for Exh, Dist, Fed Up; 
# - inertia: multilevel AR(1) for Exh, Dist, Fed Up;
# - covariation: ICC;
# - Also dummy variable for Sample + its interactions with all affective indices.

model2_arml_rsd <- lm(PBA_total ~ Belgium +  # < intercept & dummy variables
                         mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                         # affective dynamic indices:
                         rsd_Exh + rsd_Dist + rsd_FedUp + 
                         AR.1ml_Exh + AR.1ml_Dist + AR.1ml_FedUp +
                         ICC + 
                         # interactions with dummy variables: 
                         rsd_Exh*Belgium + rsd_Dist*Belgium + rsd_FedUp*Belgium + 
                         AR.1ml_Exh*Belgium + AR.1ml_Dist*Belgium + AR.1ml_FedUp*Belgium +
                         ICC*Belgium, 
                       data = df_regression)


# Check assumptions of model
performance::check_model(model2_arml_rsd)
performance::check_outliers(model2_arml_rsd) # none
performance::check_heteroscedasticity(model2_arml_rsd) # significant
performance::check_normality(model2_arml_rsd) # okay
performance::check_collinearity(model2_arml_rsd) # a lot of interaction terms + scaled Exh & FedUp means 
  # Note: Model has interaction terms. VIFs might be inflated. You may check
  # multicollinearity among predictors of a model without interaction terms.

    # Checking model without interaction terms: 
    model2.5_aff_nointeractions <- lm(PBA_total ~ Belgium +  # < intercept & dummy variables
                                        mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                                        # affective dynamic indices:
                                        rsd_Exh + rsd_Dist + rsd_FedUp + 
                                        AR.1ml_Exh + AR.1ml_Dist + AR.1ml_FedUp +
                                        ICC, 
                                      data = df_regression)
    
    performance::check_collinearity(model2.5_aff_nointeractions) # collinearity okay

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Sensitivity Analyses ----------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.1) Main Analyses by Sample  ----------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Model 1 (US) --------------------------------------------

model1_means_US <- lm(PBA_total ~
                        mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled, 
                      data = df_regression[df_regression$Sample=="US",])
# Check assumptions of model
performance::check_model(model1_means_US)
performance::check_heteroscedasticity(model1_means_US) # significant
performance::check_normality(model1_means_US) # significant
performance::check_collinearity(model1_means_US) #okay

### Model 2 (US): main model indices --------------------------------------------

model2_arml_rsd_US <- lm(PBA_total ~ 
                      mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                      rsd_Exh + rsd_Dist + rsd_FedUp + 
                      AR.1ml_Exh + AR.1ml_Dist + AR.1ml_FedUp +
                      ICC, 
                    data = df_regression[df_regression$Sample=="US",])

performance::check_model(model2_arml_rsd_US)
performance::check_heteroscedasticity(model2_arml_rsd_US) # sig
performance::check_normality(model2_arml_rsd_US) # sig
 

### Model 1 (Belgium) --------------------------------------------

model1_means_BE <- lm(PBA_total ~
                        mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled, 
                      data = df_regression[df_regression$Sample=="Belgium",])

# Check assumptions of model
performance::check_model(model1_means_BE)
performance::check_heteroscedasticity(model1_means_BE) # okay
performance::check_normality(model1_means_BE) # okay
performance::check_collinearity(model1_means_BE) # okay

### Model 2 (Belgium): main model indices --------------------------------------------

model2_arml_rsd_BE <- lm(PBA_total ~ 
                      mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                      rsd_Exh + rsd_Dist + rsd_FedUp + 
                      AR.1ml_Exh + AR.1ml_Dist + AR.1ml_FedUp +
                      ICC, 
                    data = df_regression[df_regression$Sample=="Belgium",])

performance::check_model(model2_arml_rsd_BE)
performance::check_heteroscedasticity(model2_arml_rsd_BE) # Non-constant error variance detected
performance::check_normality(model2_arml_rsd_BE) # okay
performance::check_collinearity(model2_arml_rsd_BE) # okay


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.2) mlVAR index (AR Sensitivity) ------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### mlVAR: Joint sample with dummy variable --------------------------------------------

model2_mlVAR_rsd <- lm(PBA_total ~ Belgium + 
                         mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                         rsd_Exh + rsd_Dist + rsd_FedUp + 
                         AR.mlVAR_Exh + AR.mlVAR_Dist + AR.mlVAR_FedUp +
                         ICC + 
                         # interactions with dummy variables: 
                         rsd_Exh*Belgium + rsd_Dist*Belgium + rsd_FedUp*Belgium + 
                         AR.mlVAR_Exh*Belgium + AR.mlVAR_Dist*Belgium + AR.mlVAR_FedUp*Belgium +
                         ICC*Belgium, 
                       data = df_regression)


# Check assumptions of model
performance::check_model(model2_mlVAR_rsd)  
performance::check_outliers(model2_mlVAR_rsd) # none
performance::check_heteroscedasticity(model2_mlVAR_rsd) # significant
performance::check_normality(model2_mlVAR_rsd) # okay
performance::check_collinearity(model2_mlVAR_rsd) # a lot of interaction terms + scaled Exh & FedUp means 
# Note: Model has interaction terms. VIFs might be inflated. You may check
# multicollinearity among predictors of a model without interaction terms.

    # Checking model without interaction terms (for collinearity purposes)
    model2.5_aff_nointeractions_mlVAR <- lm(PBA_total ~ Belgium +  
                                        mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                                        rsd_Exh + rsd_Dist + rsd_FedUp + 
                                        AR.mlVAR_Exh + AR.mlVAR_Dist + AR.mlVAR_FedUp +
                                        ICC, 
                                      data = df_regression)
    
    performance::check_collinearity(model2.5_aff_nointeractions_mlVAR) # collinearity okay


### mlVAR: By sample --------------------------------------------

  # US Sample
  model2_mlVAR_rsd_US <- lm(PBA_total ~ 
                            mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                            rsd_Exh + rsd_Dist + rsd_FedUp + 
                            AR.mlVAR_Exh + AR.mlVAR_Dist + AR.mlVAR_FedUp +
                            ICC, 
                          data = df_regression[df_regression$Sample=="US",])

  performance::check_model(model2_mlVAR_rsd_US) # heteroscedastic & non-normal



  # Belgium Sample
    model2_mlVAR_rsd_BE <- lm(PBA_total ~ 
                            mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                            rsd_Exh + rsd_Dist + rsd_FedUp + 
                            AR.mlVAR_Exh + AR.mlVAR_Dist + AR.mlVAR_FedUp +
                            ICC, 
                          data = df_regression[df_regression$Sample=="Belgium",])

performance::check_model(model2_mlVAR_rsd_BE, panel = FALSE) # heteroscedastic & collinearity
  performance::check_collinearity(model2_mlVAR_rsd_BE) 
  # okay for collinearity
  


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.3) AR(1) index (AR Sensitivity) --------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### AR(1): Joint sample with dummy variable --------------------------------------------

model2_AR1_rsd <- lm(PBA_total ~ Belgium +  
                               mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                               rsd_Exh + rsd_Dist + rsd_FedUp + 
                               AR.1_Exh + AR.1_Dist + AR.1_FedUp +
                               ICC + 
                               # interactions with dummy variables: 
                               rsd_Exh*Belgium + rsd_Dist*Belgium + rsd_FedUp*Belgium + 
                               AR.1_Exh*Belgium + AR.1_Dist*Belgium + AR.1_FedUp*Belgium +
                               ICC*Belgium, 
                             data = df_regression)

# Check assumptions of model
performance::check_model(model2_AR1_rsd)  
performance::check_outliers(model2_AR1_rsd) # none
performance::check_heteroscedasticity(model2_AR1_rsd) # significant
performance::check_normality(model2_AR1_rsd) # okay
performance::check_collinearity(model2_AR1_rsd) # a lot of interaction terms + scaled Exh & FedUp means 
# Note: Model has interaction terms. VIFs might be inflated. You may check
# multicollinearity among predictors of a model without interaction terms.

    # Checking model without interaction terms (for collinearity purposes)
    
    model2.5_aff_nointeractions_AR1 <- lm(PBA_total ~ Belgium +  
                                              mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                                              rsd_Exh + rsd_Dist + rsd_FedUp + 
                                              AR.1_Exh + AR.1_Dist + AR.1_FedUp +
                                              ICC, 
                                            data = df_regression)
    
    performance::check_collinearity(model2.5_aff_nointeractions_AR1) # collinearity okay

### AR(1): By sample --------------------------------------------

model2_AR1_rsd_US <- lm(PBA_total ~ 
                            mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                            rsd_Exh + rsd_Dist + rsd_FedUp + 
                            AR.1_Exh + AR.1_Dist + AR.1_FedUp +
                            ICC, 
                          data = df_regression[df_regression$Sample=="US",])
performance::check_model(model2_AR1_rsd_US)  # non-normal & heteroscedastic


model2_AR1_rsd_BE <- lm(PBA_total ~ 
                          mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                          rsd_Exh + rsd_Dist + rsd_FedUp + 
                          AR.1_Exh + AR.1_Dist + AR.1_FedUp +
                          ICC, 
                        data = df_regression[df_regression$Sample=="Belgium",])

performance::check_model(model2_AR1_rsd_BE) # heteroscedastic


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.3) SD (Sensitivity of operationalizing variation, vs relative SD) --------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### SD: Joint sample (with dummy variable) ---------------------------------------

model2_arml_sd <- lm(PBA_total ~ Belgium +  
                         mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                         sd_Exh_scaled + sd_Dist_scaled + sd_FedUp_scaled + 
                         AR.1ml_Exh + AR.1ml_Dist + AR.1ml_FedUp +
                         ICC + 
                         # interactions with dummy variables: 
                         sd_Exh_scaled*Belgium + sd_Dist_scaled*Belgium + sd_FedUp_scaled*Belgium + 
                         AR.1ml_Exh*Belgium + AR.1ml_Dist*Belgium + AR.1ml_FedUp*Belgium +
                         ICC*Belgium, 
                       data = df_regression)


# Check assumptions of model
performance::check_model(model2_arml_sd)
performance::check_outliers(model2_arml_sd) # none
performance::check_heteroscedasticity(model2_arml_sd) # significant
performance::check_normality(model2_arml_sd) # significant
performance::check_collinearity(model2_arml_sd) # a lot of interaction terms + scaled Exh & FedUp means 
# Note: Model has interaction terms. VIFs might be inflated. You may check
# multicollinearity among predictors of a model without interaction terms.

    # Checking model without interaction terms for collinearity: 
    
    model2.5_aff_nointeractions_sd <- lm(PBA_total ~ Belgium + 
                                        mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                                        sd_Exh_scaled + sd_Dist_scaled + sd_FedUp_scaled + 
                                        AR.1ml_Exh + AR.1ml_Dist + AR.1ml_FedUp +
                                        ICC, 
                                      data = df_regression)
    
    performance::check_collinearity(model2.5_aff_nointeractions_sd) # collinearity okay


### SD: By sample -------------------------------------------------

# US Sample
model2_arml_sd_US <- lm(PBA_total ~ 
                            mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                            sd_Exh_scaled + sd_Dist_scaled + sd_FedUp_scaled + 
                            AR.1ml_Exh + AR.1ml_Dist + AR.1ml_FedUp +
                            ICC, 
                          data = df_regression[df_regression$Sample=="US",])

performance::check_model(model2_arml_sd_US) # heteroscedastic & non-normal


# Belgium Sample
model2_arml_sd_BE <- lm(PBA_total ~ 
                         mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                         sd_Exh_scaled + sd_Dist_scaled + sd_FedUp_scaled + 
                         AR.1ml_Exh + AR.1ml_Dist + AR.1ml_FedUp +
                         ICC, 
                       data = df_regression[df_regression$Sample=="Belgium",])

performance::check_model(model2_arml_sd_BE) # heteroscedastic 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2.4) All other combinations ------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### mlVAR & SD ----------------------------------------------------------------------------------------

  # joint samples +++++++++++++++++++++++++++++++++++++++++++++++++++
  
  model2_mlVAR_sd <- lm(PBA_total ~ Belgium +  
                              mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                              sd_Exh_scaled + sd_Dist_scaled + sd_FedUp_scaled + 
                              AR.mlVAR_Exh + AR.mlVAR_Dist + AR.mlVAR_FedUp +
                              ICC + 
                              # interactions with dummy variables: 
                              sd_Exh_scaled*Belgium + sd_Dist_scaled*Belgium + sd_FedUp_scaled*Belgium + 
                              AR.mlVAR_Exh*Belgium + AR.mlVAR_Dist*Belgium + AR.mlVAR_FedUp*Belgium +
                              ICC*Belgium, 
                            data = df_regression)
  
  # Check assumptions of model
  performance::check_model(model2_mlVAR_sd) # Heteroscedasticity & non-normality

  # High collinearity so checking model without interaction terms for collinearity: 
      model2.5_mlVAR_SD_nointeractions <- lm(PBA_total ~ Belgium + 
                                           mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                                           sd_Exh_scaled + sd_Dist_scaled + sd_FedUp_scaled + 
                                           AR.mlVAR_Exh + AR.mlVAR_Dist + AR.mlVAR_FedUp +
                                           ICC, 
                                         data = df_regression)
    
    performance::check_collinearity(model2.5_mlVAR_SD_nointeractions) # collinearity okay
    
  
### US sample only (SD & mlVAR) +++++++++++++++++++++++++++++++++++++++++++++
  
  model2_mlVAR_sd_US <- lm(PBA_total ~ 
                           mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                           sd_Exh_scaled + sd_Dist_scaled + sd_FedUp_scaled + 
                           AR.mlVAR_Exh + AR.mlVAR_Dist + AR.mlVAR_FedUp +
                           ICC, 
                         data = df_regression[df_regression$Sample=="US",])
  
  performance::check_model(model2_mlVAR_sd_US) # heteroscedastic + non-normal
    # Also note: high correlation for mean Exh & mean FedUp 

    
### Belgium sample only (SD & mlVAR) +++++++++++++++++++++++++++++++++++++++++++++
  
  model2_mlVAR_sd_BE <- lm(PBA_total ~ 
                             mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                             sd_Exh_scaled + sd_Dist_scaled + sd_FedUp_scaled + 
                             AR.mlVAR_Exh + AR.mlVAR_Dist + AR.mlVAR_FedUp +
                             ICC, 
                           data = df_regression[df_regression$Sample=="Belgium",])
  
  performance::check_model(model2_mlVAR_sd_BE) # Collinearity: Mean Exh, mean FedUp & AR Exh all highly overlapping

  
  ### FYI: Not looking at mlVAR & SD with all 194 participants, since
  ### participants having no variation on one item will impact the
  ### autoregressive estimations for the other 2 variables (since this model
  ### includes cross-lags)
  
  
  #### AR(1) & SD ----------------------------------------------------------------------------------------
  
  # joint samples +++++++++++++++++++++++++++++++++++++++++++++++++++
  
  model2_AR1_sd <- lm(PBA_total ~ Belgium +  
                          mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                          sd_Exh_scaled + sd_Dist_scaled + sd_FedUp_scaled + 
                          AR.1_Exh + AR.1_Dist + AR.1_FedUp +
                          ICC + 
                          # interactions with dummy variables: 
                          sd_Exh_scaled*Belgium + sd_Dist_scaled*Belgium + sd_FedUp_scaled*Belgium + 
                          AR.1_Exh*Belgium + AR.1_Dist*Belgium + AR.1_FedUp*Belgium +
                          ICC*Belgium, 
                        data = df_regression)
  
  # Check assumptions of model
  performance::check_model(model2_AR1_sd) # Heteroscedasticity & non-normality

  # Also collinearity so checking model without interaction terms for collinearity: 
  model2.5_AR1_SD_nointeractions <- lm(PBA_total ~ Belgium + 
                                           mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                                           sd_Exh_scaled + sd_Dist_scaled + sd_FedUp_scaled + 
                                           AR.1_Exh + AR.1_Dist + AR.1_FedUp +
                                           ICC, 
                                         data = df_regression)
  
  performance::check_collinearity(model2.5_AR1_SD_nointeractions) # all okay
  
  
  ### US sample only (SD & mlVAR) +++++++++++++++++++++++++++++++++++++++++++++
  
  model2_AR1_sd_US <- lm(PBA_total ~ 
                             mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                             sd_Exh_scaled + sd_Dist_scaled + sd_FedUp_scaled + 
                             AR.1_Exh + AR.1_Dist + AR.1_FedUp +
                             ICC, 
                           data = df_regression[df_regression$Sample=="US",])
  
  performance::check_model(model2_AR1_sd_US) # heteroscedastic + non-normal

  ### Belgium sample only (SD & mlVAR) +++++++++++++++++++++++++++++++++++++++++++++
  
  model2_AR1_sd_BE <- lm(PBA_total ~ 
                             mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                             sd_Exh_scaled + sd_Dist_scaled + sd_FedUp_scaled + 
                             AR.1_Exh + AR.1_Dist + AR.1_FedUp +
                             ICC, 
                           data = df_regression[df_regression$Sample=="Belgium",])
  
  performance::check_model(model2_AR1_sd_BE) # Heteroscedastic

  
  
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3) Exploratory model: Moderation by mean-level ------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

model_exp_BE <- lm(PBA_total ~   
                         mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                         # affective dynamic indices:
                         rsd_Exh + rsd_Dist + rsd_FedUp + 
                         AR.1ml_Exh + AR.1ml_Dist + AR.1ml_FedUp +
                         # interactions with mean levels
                         rsd_Exh*mean_Exh_scaled + rsd_Dist*mean_Dist_scaled + rsd_FedUp*mean_FedUp_scaled + 
                         AR.1ml_Exh*mean_Exh_scaled + AR.1ml_Dist*mean_Dist_scaled + AR.1ml_FedUp*mean_FedUp_scaled,
                       data = df_regression[df_regression$Sample=="Belgium",])


performance::check_model(model_exp_BE)
performance::check_heteroscedasticity(model_exp_BE) # okay
performance::check_normality(model_exp_BE) # okay
performance::check_collinearity(model_exp_BE) 
  # lots of collinear variables but interactions; model without interactions is okay...
 


  
model_exp_US <- lm(PBA_total ~   
                     mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                     # affective dynamic indices:
                     rsd_Exh + rsd_Dist + rsd_FedUp + 
                     AR.1ml_Exh + AR.1ml_Dist + AR.1ml_FedUp +
                     # interactions with mean levels
                     rsd_Exh*mean_Exh_scaled + rsd_Dist*mean_Dist_scaled + rsd_FedUp*mean_FedUp_scaled + 
                     AR.1ml_Exh*mean_Exh_scaled + AR.1ml_Dist*mean_Dist_scaled + AR.1ml_FedUp*mean_FedUp_scaled,
                   data = df_regression[df_regression$Sample=="US",])

performance::check_model(model_exp_US)
performance::check_heteroscedasticity(model_exp_US) # sig
performance::check_normality(model_exp_US) # sig
performance::check_collinearity(model_exp_US) 
# lots of collinear variables but interactions; model without interactions is okay...


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4) Exporting tables ( + bootstrapping + adjusting p-values) -----------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4.1) Main table ------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

sjPlot::tab_model(model1_means_dummy, model2_arml_rsd, 
                  show.intercept = FALSE, show.fstat = TRUE, show.obs = TRUE, emph.p = T, 
                  p.adjust = (method = "BH") , 
                  title = "Bootstrapped regression models (with main affective indices)",
                  bootstrap = TRUE, iterations = 5000, seed = 404,
                  file = here::here("output", "tables", "Main_models_bootstrap.xls"))
                  #file = )


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4.2) Main Sensitivity Tables (Joint Sample, Bootstrapped) -----------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### Renaming variables so all in a row in the table --------------------------------------------
names(model2_arml_sd$coefficients) %<>%
  sub("sd_Exh_scaled", "sd_Exh", .) %>%
  sub("sd_Dist_scaled", "sd_Dist", .) %>%
  sub("sd_FedUp_scaled", "sd_FedUp", .) %>% 
  sub("AR.1ml_Exh", "AR_Exh", .) %>%
  sub("AR.1ml_Dist", "AR_Dist", .) %>%
  sub("AR.1ml_FedUp", "AR_FedUp", .)

names(model2_AR1_rsd$coefficients) %<>%
  sub("rsd_Exh", "sd_Exh", .) %>%
  sub("rsd_Dist", "sd_Dist", .) %>%
  sub("rsd_FedUp", "sd_FedUp", .) %>%
  sub("AR.1_Exh", "AR_Exh", .) %>%
  sub("AR.1_Dist", "AR_Dist", .) %>%
  sub("AR.1_FedUp", "AR_FedUp", .)

names(model2_AR1_sd$coefficients) %<>%
  sub("sd_Exh_scaled", "sd_Exh", .) %>%
  sub("sd_Dist_scaled", "sd_Dist", .) %>%
  sub("sd_FedUp_scaled", "sd_FedUp", .) %>%
  sub("AR.1_Exh", "AR_Exh", .) %>%
  sub("AR.1_Dist", "AR_Dist", .) %>%
  sub("AR.1_FedUp", "AR_FedUp", .)

names(model2_mlVAR_rsd$coefficients) %<>%
  sub("rsd_Exh", "sd_Exh", .) %>%
  sub("rsd_Dist", "sd_Dist", .) %>%
  sub("rsd_FedUp", "sd_FedUp", .) %>%
  sub("AR.mlVAR_Exh", "AR_Exh", .) %>%
  sub("AR.mlVAR_Dist", "AR_Dist", .) %>%
  sub("AR.mlVAR_FedUp", "AR_FedUp", .)

names(model2_mlVAR_sd$coefficients) %<>%
  sub("sd_Exh_scaled", "sd_Exh", .) %>%
  sub("sd_Dist_scaled", "sd_Dist", .) %>%
  sub("sd_FedUp_scaled", "sd_FedUp", .) %>%
  sub("AR.mlVAR_Exh", "AR_Exh", .) %>%
  sub("AR.mlVAR_Dist", "AR_Dist", .) %>%
  sub("AR.mlVAR_FedUp", "AR_FedUp", .)


#### Table: SD & rSD ----------------------------------------------------------------

# All sensitivity analyses (bootstrapped, joint sample)
sjPlot::tab_model(model2_arml_sd, model2_AR1_rsd, model2_AR1_sd, model2_mlVAR_rsd, model2_mlVAR_sd,
                  show.intercept = FALSE, show.obs = TRUE, emph.p = T, 
                  p.adjust = (method = "BH") , 
                  bootstrap = TRUE, iterations = 5000, seed = 404,
                  title = "Bootstrapped Regression Models: Sensitivity Analyses (Joint Samples)",
                  dv.labels = c("Ml.AR(1) & SD", "AR(1) & rSD", "AR(1) & SD", "mlVAR & rSD", "mlVAR & SD"),
                  string.est = "B", digits = 1, digits.p = 2,
                  file = here::here("output", "tables", "Sensitivity models","Main_Sensitivity_Bootstrapped.xls"))
                  #file = )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4.3) Raw data - Main sensitivity analyses tables ---------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### Table: Main Analysis (but with raw data) ----------------------------------------------------------------
sjPlot::tab_model(model1_means_dummy, model2_arml_rsd, 
                  show.intercept = FALSE, show.fstat = TRUE, show.obs = TRUE, emph.p = T, 
                  p.adjust = (method = "BH") , 
                  title = "Regression models (with main affective indices): Joint data with raw data",
                  #file = )
                  file = here::here("output", "tables", "Sensitivity models","Main_models_raw.xls"))

#### Table: SD & rSD ----------------------------------------------------------------

sjPlot::tab_model(model2_arml_sd, model2_AR1_rsd, model2_AR1_sd, model2_mlVAR_rsd, model2_mlVAR_sd,
                  show.intercept = FALSE, show.obs = TRUE, emph.p = T, 
                  p.adjust = (method = "BH") , 
                  title = "Regression Models with Raw Data: Sensitivity Analyses (Joint Samples)",
                  dv.labels = c("Ml.AR(1) & SD", "AR(1) & rSD", "AR(1) & SD", "mlVAR & rSD", "mlVAR & SD"),
                  string.est = "B", digits = 1, digits.p = 2,
                  file = here::here("output", "tables", "Sensitivity models","Main_Sensitivity_RawData.xls"))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4.4) Belgian Sample Sensitivity Tables (Bootstrapped) ----------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Renaming variables so all in a row in the table --------------------------------------------
# relative SD
names(model2_arml_rsd_BE$coefficients) %<>%
  sub("AR.1ml_Exh", "AR_Exh", .) %>%
  sub("AR.1ml_Dist", "AR_Dist", .) %>%
  sub("AR.1ml_FedUp", "AR_FedUp", .)

names(model2_AR1_rsd_BE$coefficients) %<>%
  sub("AR.1_Exh", "AR_Exh", .) %>%
  sub("AR.1_Dist", "AR_Dist", .) %>%
  sub("AR.1_FedUp", "AR_FedUp", .)

names(model2_mlVAR_rsd_BE$coefficients) %<>%
  sub("AR.mlVAR_Exh", "AR_Exh", .) %>%
  sub("AR.mlVAR_Dist", "AR_Dist", .) %>%
  sub("AR.mlVAR_FedUp", "AR_FedUp", .)

# SD models
names(model2_arml_sd_BE$coefficients) %<>%
  sub("AR.1ml_Exh", "AR_Exh", .) %>%
  sub("AR.1ml_Dist", "AR_Dist", .) %>%
  sub("AR.1ml_FedUp", "AR_FedUp", .)

names(model2_AR1_sd_BE$coefficients) %<>%
  sub("AR.1_Exh", "AR_Exh", .) %>%
  sub("AR.1_Dist", "AR_Dist", .) %>%
  sub("AR.1_FedUp", "AR_FedUp", .)

names(model2_mlVAR_sd_BE$coefficients) %<>%
  sub("AR.mlVAR_Exh", "AR_Exh", .) %>%
  sub("AR.mlVAR_Dist", "AR_Dist", .) %>%
  sub("AR.mlVAR_FedUp", "AR_FedUp", .)


### Table: Relative SD models ---------------------------------------------------------
sjPlot::tab_model(model2_arml_rsd_BE, model2_AR1_rsd_BE, model2_mlVAR_rsd_BE, 
                  show.intercept = FALSE, show.obs = TRUE, emph.p = T, 
                  p.adjust = (method = "BH") , 
                  bootstrap = TRUE, iterations = 5000, seed = 404,
                  title = "Bootstrapped Regression Models: Sensitivity Analyses with relative SD (Belgium Sample)",
                  dv.labels = c("Ml.AR(1) & rSD", "AR(1) & rSD", "mlVAR & rSD"),
                  string.est = "B", digits = 1, digits.p = 2,
                  file = here::here("output", "tables", "Sensitivity models", "Sensitivity_Belgium_relativeSD_Bootstrapped.xls"))


### Table: SD models ---------------------------------------------------------

sjPlot::tab_model(model2_arml_sd_BE, model2_AR1_sd_BE, model2_mlVAR_sd_BE,
                  show.intercept = FALSE, show.obs = TRUE, emph.p = T, 
                  p.adjust = (method = "BH") , 
                  bootstrap = TRUE, iterations = 5000, seed = 404,
                  title = "Bootstrapped Regression Models: Sensitivity Analyses with SD - all participants (Belgium Sample)",
                  dv.labels = c("Ml.AR(1) & SD",  "AR(1) & SD", "mlVAR & SD"),
                  string.est = "B", digits = 1, digits.p = 2,
                  file = here::here("output", "tables", "Sensitivity models","Sensitivity_Belgium_SD_Bootstrapped.xls"))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4.5) US Sample Sensitivity Tables (Bootstrapped) ---------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#### Renaming variables so all in a row in the table -------------------------
# relative SD
names(model2_arml_rsd_US$coefficients) %<>%
  sub("AR.1ml_Exh", "AR_Exh", .) %>%
  sub("AR.1ml_Dist", "AR_Dist", .) %>%
  sub("AR.1ml_FedUp", "AR_FedUp", .)

names(model2_AR1_rsd_US$coefficients) %<>%
  sub("AR.1_Exh", "AR_Exh", .) %>%
  sub("AR.1_Dist", "AR_Dist", .) %>%
  sub("AR.1_FedUp", "AR_FedUp", .)

names(model2_mlVAR_rsd_US$coefficients) %<>%
  sub("AR.mlVAR_Exh", "AR_Exh", .) %>%
  sub("AR.mlVAR_Dist", "AR_Dist", .) %>%
  sub("AR.mlVAR_FedUp", "AR_FedUp", .)

# SD models
names(model2_arml_sd_US$coefficients) %<>%
  sub("AR.1ml_Exh", "AR_Exh", .) %>%
  sub("AR.1ml_Dist", "AR_Dist", .) %>%
  sub("AR.1ml_FedUp", "AR_FedUp", .)

names(model2_AR1_sd_US$coefficients) %<>%
  sub("AR.1_Exh", "AR_Exh", .) %>%
  sub("AR.1_Dist", "AR_Dist", .) %>%
  sub("AR.1_FedUp", "AR_FedUp", .)

names(model2_mlVAR_sd_US$coefficients) %<>%
  sub("AR.mlVAR_Exh", "AR_Exh", .) %>%
  sub("AR.mlVAR_Dist", "AR_Dist", .) %>%
  sub("AR.mlVAR_FedUp", "AR_FedUp", .)


#### Table: Relative SD models ---------------------------------------------------------
sjPlot::tab_model(model2_arml_rsd_US, model2_AR1_rsd_US, model2_mlVAR_rsd_US, 
                  show.intercept = FALSE, show.obs = TRUE, emph.p = T, 
                  p.adjust = (method = "BH") , 
                  bootstrap = TRUE, iterations = 5000, seed = 404,
                  title = "Bootstrapped Regression Models: Sensitivity Analyses with relative SD (US Sample)",
                  dv.labels = c("Ml.AR(1) & rSD", "AR(1) & rSD", "mlVAR & rSD"),
                  string.est = "B", digits = 1, digits.p = 2,
                  file = here::here("output", "tables","Sensitivity models", "Sensitivity_US_relativeSD_Bootstrapped.xls"))


#### Table: SD models ---------------------------------------------------------

sjPlot::tab_model(model2_arml_sd_US, model2_AR1_sd_US, model2_mlVAR_sd_US,
                  show.intercept = FALSE, show.obs = TRUE, emph.p = T, 
                  p.adjust = (method = "BH") , 
                  bootstrap = TRUE, iterations = 5000, seed = 404,
                  title = "Bootstrapped Regression Models: Sensitivity Analyses with SD - all participants (Belgium Sample)",
                  dv.labels = c("Ml.AR(1) & SD", "AR(1) & SD", "mlVAR & SD"),
                  string.est = "B", digits = 1, digits.p = 2,
                  file = here::here("output", "tables", "Sensitivity models", "Sensitivity_US_SD_Bootstrapped.xls"))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4.6) Exploratory mean-moderation models ------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Raw data
sjPlot::tab_model(model_exp_BE, model_exp_US, 
                  show.intercept = FALSE, show.fstat = TRUE, show.obs = TRUE, emph.p = T, 
                  p.adjust = (method = "BH") , 
                  dv.labels = c("Belgium Sample", "US Sample"),
                  title = "Regression models with moderation by mean-level (Exploratory): With raw data",
                  file = here::here("output", "tables", "exploratory_meanmoderation_raw.xls"))
                  #file = )


# Bootstrapped (bc US sample model is heteroscedastic & non-normal)
sjPlot::tab_model(model_exp_BE, model_exp_US, 
                  show.intercept = FALSE, show.fstat = TRUE, show.obs = TRUE, emph.p = T, 
                  p.adjust = (method = "BH") , 
                  dv.labels = c("Belgium Sample", "US Sample"),
                  bootstrap = TRUE, iterations = 2000, seed = 404,
                  title = "Bootstrapped regression models with moderation by mean-level (Exploratory)",
                  file = here::here("output", "tables", "exploratory_meanmoderation_bootstrap.xls"))
                  #file = )


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4.6) Controlling for age of kid   ------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### First: Adding in age of kid into exact models from main analyses --------------------
# Model 1: Dummy variable with just mean-levels (joint sample) 
model1_means_dummy_age <- lm(PBA_total ~ Belgium +
                           mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled + 
                             agekid_onlyOrYoungest,
                         data = df_regression)


# Check assumptions of model
performance::check_collinearity(model1_means_dummy_age) # okay
performance::check_heteroscedasticity(model1_means_dummy_age) 
# Heteroscedasticity (non-constant error variance) detected (p < .001).
performance::check_normality(model1_means_dummy_age) 
# Non-normality of residuals detected (p = 0.009)



# Model 2: Affect Dynamic predictors with dummy variable for Sample type 

model2_arml_rsd_age <- lm(PBA_total ~ Belgium +  # < intercept & dummy variables
                        mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                        # affective dynamic indices:
                        rsd_Exh + rsd_Dist + rsd_FedUp + 
                        AR.1ml_Exh + AR.1ml_Dist + AR.1ml_FedUp +
                        ICC + 
                        # interactions with dummy variables: 
                        rsd_Exh*Belgium + rsd_Dist*Belgium + rsd_FedUp*Belgium + 
                        AR.1ml_Exh*Belgium + AR.1ml_Dist*Belgium + AR.1ml_FedUp*Belgium +
                        ICC*Belgium +
                          agekid_onlyOrYoungest,
                      data = df_regression)

# Check assumptions of model
performance::check_collinearity(model2_arml_rsd_age) # high collinearity but not with age variale
performance::check_heteroscedasticity(model2_arml_rsd_age) 
# Heteroscedasticity (non-constant error variance) detected (p < .001).
performance::check_normality(model2_arml_rsd_age) 
# Okay (p = 0.060)
summary(model2_arml_rsd_age)


### Second: Adding in instead of dummy variable --------------------
model1_means_age <- lm(PBA_total ~ mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled + 
                               agekid_onlyOrYoungest,
                             data = df_regression)

summary(model1_means_age) # now age sig

performance::check_collinearity(model1_means_age) # okay
performance::check_heteroscedasticity(model1_means_age) 
# Heteroscedasticity (non-constant error variance) detected (p < .001).
performance::check_normality(model1_means_age) 
# Okay (p = 0.001)



model2_age <- lm(PBA_total ~ mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                            # affective dynamic indices:
                            rsd_Exh + rsd_Dist + rsd_FedUp + 
                            AR.1ml_Exh + AR.1ml_Dist + AR.1ml_FedUp +
                            ICC + 
                            agekid_onlyOrYoungest,
                          data = df_regression)

summary(model2_age) # now sig

performance::check_collinearity(model2_age) # okay
performance::check_heteroscedasticity(model2_age) 
# Heteroscedasticity (non-constant error variance) detected (p < .001).
performance::check_normality(model2_age) 
# Okay (p = 0.039)

### Saving bootstrapped table ---------------------------------
sjPlot::tab_model(model1_means_age, model1_means_dummy_age, model2_arml_rsd_age, model2_age, 
                  show.intercept = FALSE, show.fstat = TRUE, show.obs = TRUE, emph.p = T, 
                  p.adjust = (method = "BH") , 
                  dv.labels = c("Means", "Means & Sample", "Main model in Article", "Model without Sample"),
                  bootstrap = TRUE, iterations = 2000, seed = 404,
                  title = "Bootstrapped regression models controlling for age of kids",
                  file = here::here("output", "tables", "modelbootstrap_controllingAgeKids.xls"))
#file = )
