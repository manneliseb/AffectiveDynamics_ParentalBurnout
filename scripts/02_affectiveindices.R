#---------------------------------------------------------------#
# Computing Affective Indices                                   #
#---------------------------------------------------------------#

# This R script computes all affective indices, specifically: 
# - intensity (mean)
# - variation (SD & relative SD)
# - covariation (ICC)
# - inertia (autoregressive parameters from AR(1), multilevel AR(1) and mlVAR models)

# All code and materials for this project can be found here: https://osf.io/5nfbu/

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R packages -----------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(here) # for relative paths within project
library(tidyverse) 
library(mlVAR) # autoregression
library(psychometric) #for ICCs
library(lme4) # for multilevel AR(1) models
library(DataCombine) # to create lagged predictors (for multilevel AR(1) models)

# the relativeVariability package is downloaded & installed from here:
# https://ppw.kuleuven.be/okp/software/relative_variability/ (follow
# instructions from ReadMe file)
library(relativeVariability)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Importing Data -------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Import dataset (From Script #1 on Wrangling Data)
 data_parents <- readr::read_csv(here::here("data", "processed", "data_parents.csv"))
 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Relative SD & within-person means ---------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Getting individual means & SDs 
data_indices <- data_parents %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(mean_Exh = mean(Exh, na.rm=TRUE),  sd_Exh = sd(Exh, na.rm=TRUE), 
                mean_Dist = mean(Dist, na.rm=TRUE),  sd_Dist = sd(Dist, na.rm=TRUE), 
                mean_FedUp = mean(FedUp, na.rm=TRUE),  sd_FedUp = sd(FedUp, na.rm=TRUE)) %>%
  ungroup()



# Getting individual relativeSD
  data_indices <- data_indices %>%
    dplyr::group_by(ID) %>%
         # Belgium sample has 0-100 scale; US scale has 0-10
         dplyr::mutate(rsd_Exh = ifelse(Sample == "Belgium",
                                        relativeSD(Exh, 0, 100), relativeSD(Exh, 0, 10)),
                       rsd_Dist = ifelse(Sample == "Belgium",
                                         relativeSD(Dist, 0, 100), relativeSD(Dist, 0, 10)),
                       rsd_FedUp = ifelse(Sample == "Belgium", relativeSD(FedUp, 0, 100), relativeSD(FedUp, 0, 10)))
  
# Note - warning message, some participants have a mean = minimum of scale, so outputs 'NAN' for their measures

  # To identify these participants: 
  pns_zerovar <- data_indices %>%
    dplyr::filter(mean_Exh == 0 | mean_Dist == 0 | mean_FedUp == 0) %>%
    dplyr::distinct(ID, .keep_all = T) 
  # There are 14 participants that have at least 1 mean of daily responses = to 0
  # (1 from Belgium dataset; 13 from US dataset)
  

# Getting means on same scale (by dividing the ones from a 0-100 scale by 10)

# Belgium sample: divide means by 10 so on same scale as US sample 
data_indices <- data_indices %>%
    dplyr::mutate(mean_Exh_scaled = ifelse(Sample == "Belgium", mean_Exh/10, mean_Exh),
                mean_Dist_scaled = ifelse(Sample == "Belgium", mean_Dist/10, mean_Dist),
                mean_FedUp_scaled = ifelse(Sample == "Belgium", mean_FedUp/10, mean_FedUp))


### Sensitivity: Regular SD (not relative) ------------------------------------------------

# The main analyses use the relative SD proposed by Mestdagh et al. (2018) that
# takes into account overlap with the mean; this sensitivity analysis examines
# if using the regular SD impacts results.

# Getting individual SD
data_indices <- data_indices %>%
  dplyr::group_by(ID) %>%
  # Belgium sample has 0-100 scale; US scale has 0-10
  dplyr::mutate(Exh_scaled = ifelse(Sample == "Belgium",
                                    Exh/10, Exh),
                Dist_scaled = ifelse(Sample == "Belgium",
                                     Dist/10, Dist),
                FedUp_scaled = ifelse(Sample == "Belgium", 
                                      FedUp/10, FedUp)) %>%
  dplyr::mutate(sd_Exh_scaled = sd(Exh_scaled, na.rm=TRUE),
                sd_Dist_scaled = sd(Dist_scaled, na.rm=TRUE),
                sd_FedUp_scaled = sd(FedUp_scaled, na.rm=TRUE))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Getting initial dataset descriptives ------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Compliance rate for Belgian Sample
compliance_BE = data_parents %>%
  dplyr::filter(Sample == "Belgium") %>%
  dplyr::filter(!is.na(Exh)) %>%
  # Removing pns that have SD = 0
  filter(!(ID %in% pns_zerovar$ID)) %>% 
  dplyr::group_by(ID) %>%
  dplyr::count() %>%
  dplyr::mutate(compliance = n / 56, .keep = "all") 

print (paste0("mean compliance for Belgian sample: ", round(mean(compliance_BE$compliance, na.rm = TRUE), 2) ))
# "mean compliance for Belgian sample: 0.93"

# Compliance rate for US Sample
compliance_US = data_parents %>%
  dplyr::filter(Sample == "US") %>%
  dplyr::filter(!is.na(Exh)) %>%
  filter(!(ID %in% pns_zerovar$ID)) %>% 
  dplyr::group_by(ID) %>%
  dplyr::count() %>%
  dplyr::mutate(compliance = n / 21, .keep = "all")

print (paste0("mean compliance for US sample: ", round(mean(compliance_US$compliance, na.rm = TRUE), 2) ))
# "mean compliance for US sample: 0.82"

# Putting the compliance for both samples together
compliance_alldata <- bind_rows(compliance_BE, compliance_US) %>%
  dplyr::rename(n_total = n)

# Adding total # of timepoints and compliance rates to full dataset
data_parents <- merge(data_parents, compliance_alldata, by="ID")

# Make sure all response rates are over 50
min(data_parents$compliance) # Minimum value: .52


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3) ICC ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Following Erbas et al. (2014) & Ottenstein & Lischetzke (2019), we're using
# the ICC 3 (Consistency ICC) from Shrout & Fleiss (1979). This is because this
# measure of ICC is not influenced by the means of responses. In
# addition, ICC 3 assumes that the same items are rated at each
# measurement occasion, and that these items are not randomly chosen to be part
# of the study but chosen for theoretical reasons

# Adapted from code from Ottenstein & Lischetzke (2019)
# https://osf.io/9udkt?view_only=6d20f2d6cd8e4f6aaba6f274635d10d8

# Create empty dataframe to host IDs & ICC values
ICC_values <- as.data.frame(x=rep(NA,length(unique(data_parents$ID))))
ICC_values$ID <- NA
names(ICC_values) <- c("ID", "ICC")

# Now get ICC for each participant:   
for(i in 1:length(unique(data_parents$ID))){
  iccdata <- as.matrix(data_parents[data_parents$ID == unique(data_parents$ID)[i], c("Exh", "Dist", "FedUp")])
  icc.ind <- psych::ICC(iccdata, missing =TRUE, lmer=TRUE)
  
  icc3 <- icc.ind$results$ICC[3] # This chooses the consistency ICC measure
  print(i)
  print(icc3)
  
  # Save ICC values to dataframe
  ICC_values$ID[i] <- unique(data_parents$ID)[i] # Adding the participant ID
  ICC_values$ICC[i] <- icc3

}

# Check if any ICC values are negative
ICC_values %>% filter(ICC < 0)
# Check if any ICC values are NA 
ICC_values %>% filter(is.na(ICC)) #-> no (since participants with SD=0 taken out)

data_indices <- merge(data_indices, ICC_values, by ="ID")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4) Inertia (autoregressive) values -----------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Multilevel VAR models --------------------------------------------

# Get inertia values by getting autoregressive coefficients (when taking into
# account all relationships between all variables, from t-1 to t, i.e. a
# multilevel VAR model)

# Estimate mlVAR model
esm_mlVAR_BE <- mlVAR::mlVAR(data_parents[data_parents$Sample=="Belgium",], vars = c("Exh", "Dist", "FedUp"), idvar = "ID", lags = 1, beepvar = "n_day", estimator = "lmer", 
                              contemporaneous = "correlated", temporal = "correlated")
  # Keep (& re-reread) R objects so don't have to estimate again: 
  #readr::write_rds(esm_mlVAR_BE, here::here("output", "R_Objects", "esm_mlVAR_BE.rds"))
  #esm_mlVAR_BE <- readr::read_rds(here::here("output", "R_Objects", "esm_mlVAR_BE.rds"))

esm_mlVAR_US <- mlVAR::mlVAR(data_parents[data_parents$Sample=="US",], vars = c("Exh", "Dist", "FedUp"), idvar = "ID", lags = 1, beepvar = "n_day", estimator = "lmer", 
                             contemporaneous = "correlated", temporal = "correlated")
  
  # Note: warning message
  # 80 subjects detected with < 20 measurements. This is not recommended, as
  # within-person centering with too few observations per subject will lead to
  # biased estimates (most notably: negative self-loops).

  #readr::write_rds(esm_mlVAR_US, here::here("output", "R_Objects", "esm_mlVAR_US.rds"))
  #esm_mlVAR_US <- readr::read_rds(here::here("output", "R_Objects", "esm_mlVAR_US.rds"))

# Code (to pull out autoregressive values) is adapted from Yorgo Hoebeke

# For participants in Belgium: 
# Create dataframe for variables: 
AR_value_mlvar_BE <- data.frame(ID = character(),
                                   AR.mlVAR_Exh = double(),
                                   AR.mlVAR_Dist= double(),
                                   AR.mlVAR_FedUp= double())


  
  for(i in seq(1:length(unique(esm_mlVAR_BE$ID)))){ # for each participant in ESM data frame
    AR_value_mlvar_BE <- AR_value_mlvar_BE %>%
      add_row(ID = as.character(esm_mlVAR_BE$IDs[[i]]), # make sure matches correct ID from mlVAR model
              AR.mlVAR_Exh = as.numeric(as.data.frame(esm_mlVAR_BE$results$Beta$subject[[i]][,,1])["Exh", "Exh"]),
              AR.mlVAR_Dist = as.numeric(as.data.frame(esm_mlVAR_BE$results$Beta$subject[[i]][,,1])["Dist", "Dist"]),
              AR.mlVAR_FedUp = as.numeric(as.data.frame(esm_mlVAR_BE$results$Beta$subject[[i]][,,1])["FedUp", "FedUp"]))
  }


# For participants in US: 

# Create dataframe for variables: 
AR_value_mlvar_US <- data.frame(ID = character(),
                                   AR.mlVAR_Exh = double(),
                                   AR.mlVAR_Dist = double(),
                                   AR.mlVAR_FedUp = double())



for(i in seq(1:length(unique(esm_mlVAR_US$ID)))){ # for each participant in ESM data frame
  AR_value_mlvar_US <- AR_value_mlvar_US %>%
    add_row(ID = as.character(esm_mlVAR_US$IDs[[i]]), # make sure matches correct ID from mlVAR model
            AR.mlVAR_Exh = as.numeric(as.data.frame(esm_mlVAR_US$results$Beta$subject[[i]][,,1])["Exh", "Exh"]),
            AR.mlVAR_Dist = as.numeric(as.data.frame(esm_mlVAR_US$results$Beta$subject[[i]][,,1])["Dist", "Dist"]),
            AR.mlVAR_FedUp = as.numeric(as.data.frame(esm_mlVAR_US$results$Beta$subject[[i]][,,1])["FedUp", "FedUp"]))
}


# Put all indices together: 
AR_value_mlvar_all <- bind_rows(AR_value_mlvar_BE, AR_value_mlvar_US)

# Merge autocorrelation values with full indices dataframe
data_indices <- merge(data_indices, AR_value_mlvar_all, by ="ID")


# Note - checking how many negative autocorrelation values (because of warning): 
pns_negative_ar <- data_indices %>%
  dplyr::filter(AR.mlVAR_Exh < 0 | AR.mlVAR_Dist < 0 | AR.mlVAR_FedUp < 0) %>%
  dplyr::distinct(ID, .keep_all = T) 
# There are 75 participants with negative autocorrelation values
  # 3 from Belgium sample; 72 US sample



### Sensitivity: Multilevel AR(1) models --------------------------------------------

data_parents <- data_parents %>%
  dplyr::mutate(Exh_scaled = ifelse(Sample == "Belgium", Exh/10, Exh),
                Dist_scaled = ifelse(Sample == "Belgium", Dist/10, Dist),
                FedUp_scaled = ifelse(Sample == "Belgium", FedUp/10, FedUp))

# Creating lagged variables
data_parents <- DataCombine::slide(data_parents, Var = "Exh_scaled", GroupVar = "ID", NewVar = "Exh_scaled_lag", slideBy = -1)
data_parents <- DataCombine::slide(data_parents, Var = "Dist_scaled", GroupVar = "ID", NewVar = "Dist_scaled_lag", slideBy = -1)
data_parents <- DataCombine::slide(data_parents, Var = "FedUp_scaled", GroupVar = "ID", NewVar = "FedUp_scaled_lag", slideBy = -1)

# Multilevel AR(1) models for each variable (Exh, Dist, FedUp) and both sample (US and Belgium)

# Exhaustion

mlAR1_Exh_US <- lme4::lmer(Exh_scaled ~ 1 + Exh_scaled_lag + (1 + Exh_scaled_lag | ID), data = data_parents[data_parents$Sample=="US",], REML = FALSE)
mlAR1_Exh_BE <- lme4::lmer(Exh_scaled ~ 1 + Exh_scaled_lag + (1 + Exh_scaled_lag | ID), data = data_parents[data_parents$Sample=="Belgium",], REML = FALSE)
  # Note: This model leads to a warning messages about non-convergence; however,
  # when looking at the conversation here
  # (https://github.com/lme4/lme4/issues/120) & testing whether following is < .001 :
    relgrad <- with(mlAR1_Exh_BE@optinfo$derivs,solve(Hessian,gradient))
    max(abs(relgrad)) 
    # Then looking at different optimizers:
    
    # When testing 2 other optimizers, there are no longer convergence issues: 
    update(mlAR1_Exh_BE, control=lmerControl(optimizer="Nelder_Mead"))
    update(mlAR1_Exh_BE, control=lmerControl(optimizer="bobyqa"))
    
    # Therefore keeping one of these models as the final one:
mlAR1_Exh_BE <- update(mlAR1_Exh_BE, control=lmerControl(optimizer="bobyqa"))
    

# Distance
mlAR1_Dist_US <- lme4::lmer(Dist_scaled ~ 1 + Dist_scaled_lag + (1 + Dist_scaled_lag | ID), data = data_parents[data_parents$Sample=="US",], REML = FALSE)
mlAR1_Dist_BE <- lme4::lmer(Dist_scaled ~ 1 + Dist_scaled_lag + (1 + Dist_scaled_lag | ID), data = data_parents[data_parents$Sample=="Belgium",], REML = FALSE)

# Fed Up
mlAR1_FedUp_US <- lme4::lmer(FedUp_scaled ~ 1 + FedUp_scaled_lag + (1 + FedUp_scaled_lag | ID), data = data_parents[data_parents$Sample=="US",], REML = FALSE)
mlAR1_FedUp_BE <- lme4::lmer(FedUp_scaled ~ 1 + FedUp_scaled_lag + (1 + FedUp_scaled_lag | ID), data = data_parents[data_parents$Sample=="Belgium",], REML = FALSE)
  update(mlAR1_FedUp_BE, control=lmerControl(optimizer="bobyqa"))

# Getting values into joint dataframes
AR_Exh <- dplyr::bind_rows(coef(mlAR1_Exh_US)$ID["Exh_scaled_lag"], coef(mlAR1_Exh_BE)$ID["Exh_scaled_lag"]) %>%
  tibble::rownames_to_column("ID") %>% dplyr::rename(AR.1ml_Exh = Exh_scaled_lag )

AR_Dist <- dplyr::bind_rows(coef(mlAR1_Dist_US)$ID["Dist_scaled_lag"], coef(mlAR1_Dist_BE)$ID["Dist_scaled_lag"]) %>%
  tibble::rownames_to_column("ID") %>% dplyr::rename(AR.1ml_Dist = Dist_scaled_lag )

AR_FedUp <- dplyr::bind_rows(coef(mlAR1_FedUp_US)$ID["FedUp_scaled_lag"], coef(mlAR1_FedUp_BE)$ID["FedUp_scaled_lag"]) %>%
  tibble::rownames_to_column("ID") %>% dplyr::rename(AR.1ml_FedUp = FedUp_scaled_lag )


# Adding to index dataframe
data_indices <- merge(data_indices, AR_Exh, by ="ID")
data_indices <- merge(data_indices, AR_Dist, by ="ID")
data_indices <- merge(data_indices, AR_FedUp, by ="ID")



### Sensitivity: AR(1) -----------------------------------------------------------

# Computing inertia/autoregression in isolation (instead of from a multilevel
# model), by using an AR(1) model for each variable per participant

# Create dataframe for variables: 
AR1_values <- data.frame(ID = character(),
                         AR.1_Exh = double(),
                         AR.1_Dist = double(),
                         AR.1_FedUp= double())

# Computing inertia :
for(i in seq(1:length(unique(data_parents$ID)))){
  
  #AR(1) models for each variable for each participant
  Exh_AR1 <- stats::arima(data_parents$Exh[data_parents$ID == unique(data_parents$ID)[i]], order=c(1,0,0))
  Dist_AR1 <- stats::arima(data_parents$Dist[data_parents$ID == unique(data_parents$ID)[i]], order=c(1,0,0))
  FedUp_AR1 <- stats::arima(data_parents$FedUp[data_parents$ID == unique(data_parents$ID)[i]], order=c(1,0,0))
  
  AR1_values <- AR1_values %>%
    add_row(ID = unique(data_parents$ID)[i],
            AR.1_Exh = Exh_AR1$coef[[1]],
            AR.1_Dist = Dist_AR1$coef[[1]],
            AR.1_FedUp = FedUp_AR1$coef[[1]])
  
}




# Merge with index dataset: 
data_indices <- merge(data_indices, AR1_values, by ="ID", all.x = TRUE)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5) Exporting Data ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
readr::write_csv(data_indices, here::here("data", "processed", "data_indices.csv"))


