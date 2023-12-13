# This R code is a preregistered version for a reanalysis project, examining the
# impact of several affective dynamic indices on parental burnout severity.

# M. Annelise Blanchard
# 2023/07/28

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R packages -----------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(here) # for relative paths within project
library(tidyverse) # also loads lubridate (02)
library(performance) # for model checks
library(mlVAR) # autoregression
library(psychometric) #for ICCs

# the relativeVariability package is downloaded & installed from here:
# https://ppw.kuleuven.be/okp/software/relative_variability/ (follow
# instructions from ReadMe file)
library(relativeVariability)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Importing Data ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Data format: Will include ID, daily diary measures (3 PB features), Sample (US or Belgium) & PBA total score

  # Will join different datasets in a data wrangling script when get all data

# Import dataset (already formatted etc)
 data_parents <- readr::read_csv(here::here("data", "processed", "data_parents.csv"))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Relative SD & within-person means ---------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Getting individual relativeSD
  data_indices <- data_parents %>%
    dplyr::group_by(ID) %>%
         # Belgium sample has 0-100 scale; US scale has 0-10
         dplyr::mutate(rsd_Exh = ifelse(Sample == "Belgium",
                                        relativeSD(Exh, 0, 100), relativeSD(Exh, 0, 10)),
                       rsd_Dist = ifelse(Sample == "Belgium",
                                         relativeSD(Dist, 0, 100), relativeSD(Dist, 0, 10)),
                       rsd_FedUp = ifelse(Sample == "Belgium", relativeSD(FedUp, 0, 100), relativeSD(FedUp, 0, 10)))
  

# Getting individual means & SDs 
data_indices <- data_indices %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(mean_Exh = mean(Exh, na.rm=TRUE),  sd_Exh = sd(Exh, na.rm=TRUE), 
                mean_Dist = mean(Dist, na.rm=TRUE),  sd_Dist = sd(Dist, na.rm=TRUE), 
                mean_FedUp = mean(FedUp, na.rm=TRUE),  sd_FedUp = sd(FedUp, na.rm=TRUE)) %>%
  ungroup()
  # FYI Later will use for descriptive table per Sample/dataset


# Getting means on same scale (by dividing the ones from a 0-100 scale by 10)

# Belgium sample: divide means by 10 so on same scale as US sample (MAB check)
data_indices <- data_indices %>%
    dplyr::mutate(mean_Exh_scaled = ifelse(Sample == "Belgium", mean_Exh/10, mean_Exh),
                mean_Dist_scaled = ifelse(Sample == "Belgium", mean_Dist/10, mean_Exh),
                mean_FedUp_scaled = ifelse(Sample == "Belgium", mean_FedUp/10, mean_Exh))



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

data_indices <- merge(data_indices, ICC_values, by ="ID")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4) Inertia (autoregressive) values -----------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Get inertia values by getting autoregressive coefficients (when taking into
# account all relationships between all variables, from t-1 to t, i.e. a
# multilevel VAR model)

# Estimate mlVAR model
esm_mlVAR_BE <- mlVAR::mlVAR(data_parents[data_parents$Sample=="Belgium",], vars = c("Exh", "Dist", "FedUp"), idvar = "ID", lags = 1, beepvar = "n_obs", estimator = "lmer", 
                              contemporaneous = "correlated", temporal = "correlated")
  # Keep (& reread) R objects so don't have to estimate again: 
  #readr::write_rds(esm_mlVAR_BE, here::here("output", "R_Objects", "esm_mlVAR_BE.rds"))
  #esm_mlVAR_BE <- readr::read_rds(here::here("output", "R_Objects", "esm_mlVAR_BE.rds"))

esm_mlVAR_US <- mlVAR::mlVAR(data_parents[data_parents$Sample=="US",], vars = c("Exh", "Dist", "FedUp"), idvar = "ID", lags = 1, beepvar = "n_obs", estimator = "lmer", 
                             contemporaneous = "correlated", temporal = "correlated")
  #readr::write_rds(esm_mlVAR_US, here::here("output", "R_Objects", "esm_mlVAR_US.rds"))
  #esm_mlVAR_US <- readr::read_rds(here::here("output", "R_Objects", "esm_mlVAR_US.rds"))

# Code (to pull out autoregressive values) is adapted from Yorgo Hoebeke

# For participants in Belgium: 
# Create dataframe for variables: 
autocorr_value_mlvar <- data.frame(ID = character(),
                                   autocorr_Exh = double(),
                                   autocorr_Dist= double(),
                                   autocorr_FedUp= double())


  
  for(i in seq(1:length(unique(esm_mlVAR_BE$ID)))){ # for each participant in ESM data frame
    autocorr_value_mlvar <- autocorr_value_mlvar %>%
      add_row(ID = as.character(esm_mlVAR_BE$IDs[[i]]), # make sure matches correct ID from mlVAR model
              autocorr_Exh = as.numeric(as.data.frame(esm_mlVAR_BE$results$Beta$subject[[i]][,,1])["Exh", "Exh"]),
              autocorr_Dist = as.numeric(as.data.frame(esm_mlVAR_BE$results$Beta$subject[[i]][,,1])["Dist", "Dist"]),
              autocorr_FedUp = as.numeric(as.data.frame(esm_mlVAR_BE$results$Beta$subject[[i]][,,1])["FedUp", "FedUp"]))
  }


# Merge autocorrelation values with full indices dataframe
data_indices <- merge(data_indices, autocorr_value_mlvar, by ="ID")


# For participants in US: 

# Create dataframe for variables: 
autocorr_value_mlvar <- data.frame(ID = character(),
                                   autocorr_Exh = double(),
                                   autocorr_Dist = double(),
                                   autocorr_FedUp = double())



for(i in seq(1:length(unique(esm_mlVAR_US$ID)))){ # for each participant in ESM data frame
  autocorr_value_mlvar <- autocorr_value_mlvar %>%
    add_row(ID = as.character(esm_mlVAR_US$IDs[[i]]), # make sure matches correct ID from mlVAR model
            autocorr_Exh = as.numeric(as.data.frame(esm_mlVAR_US$results$Beta$subject[[i]][,,1])["Exh", "Exh"]),
            autocorr_Dist = as.numeric(as.data.frame(esm_mlVAR_US$results$Beta$subject[[i]][,,1])["Dist", "Dist"]),
            autocorr_FedUp = as.numeric(as.data.frame(esm_mlVAR_US$results$Beta$subject[[i]][,,1])["FedUp", "FedUp"]))
}


# Merge autocorrelation values with full indices dataframe
data_indices <- merge(data_indices, autocorr_value_mlvar, by ="ID")



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5) Models ----------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Dataset for 2 main regression models: 

df_regression <- data_indices %>%
  dplyr::group_by(ID) %>%
  select(-n_obs) %>%
  slice(1)

### Model 1: Just mean-levels --------------------------------------------
# Just to control for mean-levels

model1_means <- lm(PBA_total ~
                     mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled, 
                   data = df_regression)

# Check assumptions of model
performance::check_model(model1_means)


### Model 2: Affect Dynamic predictors with dummy variable for Sample type --------------------------------------------

# Here, we include all affective dynamic indices (i.e., 3 relative SD variables,
# 3 autoregression variables, and ICC), as well as a dummy variable create  a
# dummy variable coding Sample type, and include interactions between the dummy
# variable and all affective indices

# Create dummy variable for sample type (living in Belgium or not)
df_regression <- df_regression %>%
  mutate(Belgium = ifelse(Sample == "Belgium", 1, 0))

# Model
model2_aff_dummy <- lm(PBA_total ~ Belgium +  # < intercept & dummy variables
                     mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                       # affective dynamic indices:
                     rsd_Exh + rsd_Dist + rsd_FedUp + 
                     autocorr_Exh + autocorr_Dist + autocorr_FedUp +
                     PB_ICC + 
                       # interactions with dummy variables: 
                     rsd_Exh*Belgium + rsd_Dist*Belgium + rsd_FedUp*Belgium + 
                     autocorr_Exh*Belgium + autocorr_Dist*Belgium + autocorr_FedUp*Belgium +
                     PB_ICC*Belgium, 
                   data = df_regression)


# Check assumptions of model
performance::check_model(model2_aff_dummy)

# Compare adjusted r squared of two models
summary(model1_means)$adj.r.squared 
summary(model2_aff_dummy)$adj.r.squared

# General model summary: 
summary(model2_aff_dummy)

# Add in corrections for multiple comparisons: 

# First: Get p-values
p <- summary(model2_aff_dummy)$coefficients[,4]
# Second: Correct (method - Benjamini & Hochberg, controlling the false discovery rate)
p.adjust(p, method = "BH", n = length(p))




### Additional exploratory model: Moderation of mean? --------------------------------------------

# This is to examine whether mean levels moderate the relationship between
# affective indices & PB severity


# If the dummy variable & its interactions are not significant, will do the below
# model with all data; if they are significant, we will do 2 separate regression
# models, one per Sample type (Belgium vs US)

model_exp_moderation <- lm(PBA_total ~ 
                         mean_Exh_scaled + mean_Dist_scaled + mean_FedUp_scaled +
                         rsd_Exh + rsd_Dist + rsd_FedUp + 
                         autocorr_Exh + autocorr_Dist + autocorr_FedUp +
                         PB_ICC + 
                         # interactions with mean of relevant variable 
                         rsd_Exh*mean_Exh_scaled + rsd_Dist*mean_Dist_scaled + rsd_FedUp*mean_FedUp_scaled + 
                         autocorr_Exh*mean_Exh_scaled + autocorr_Dist*mean_Dist_scaled + autocorr_FedUp*mean_FedUp_scaled,
                       data = df_regression)


# Check assumptions of model
performance::check_model(model_exp_moderation)

#Model summary: 
summary(model_exp_moderation)



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6) Sensitivity Analyses ----------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# For the sensitivity analyses, will use the new dataset/index described each
# time below to replace the parallel one from the main analysis, and look at the
# model output to see if similar or not

### Inertia: AR(1) -----------------------------------------------------------

# Computing inertia/autoregression in isolation (instead of from a multilevel
# model), by using an AR(1) model for each variable per participant

# Create dataframe for variables: 
AR1_values <- data.frame(ID = character(),
                         Exh_AR1 = double(),
                         Dist_AR1 = double(),
                         FedUp_AR1= double())

# Computing inertia :
for(i in seq(1:length(unique(data_parents$ID)))){
  
  #AR(1) models for each variable for each participant
  AR1_Exh <- stats::arima(data_parents$Exh[data_parents$ID == unique(data_parents$ID)[i]], order=c(1,0,0))
  AR1_Dist <- stats::arima(data_parents$Dist[data_parents$ID == unique(data_parents$ID)[i]], order=c(1,0,0))
  AR1_FedUp <- stats::arima(data_parents$FedUp[data_parents$ID == unique(data_parents$ID)[i]], order=c(1,0,0))
  
  Exh_AR1 <- stats::arima(data_parents$Exh[data_parents$ID == unique(data_parents$ID)[i]], order=c(1,0,0))
  Dist_AR1 <- stats::arima(data_parents$Dist[data_parents$ID == unique(data_parents$ID)[i]], order=c(1,0,0))
  FedUp_AR1 <- stats::arima(data_parents$FedUp[data_parents$ID == unique(data_parents$ID)[i]], order=c(1,0,0))
  
  AR1_values <- AR1_values %>%
    add_row(ID = unique(data_parents$ID)[i],
            AR1_Exh = Exh_AR1$coef[[1]],
            AR1_Dist = Dist_AR1$coef[[1]],
            AR1_FedUp = FedUp_AR1$coef[[1]])
  
}

  # MAB note - I think will have to remove/have error handling for when SD=0 for a variable


### Regular SD (not relative) ------------------------------------------------

# The main analyses use the relative SD proposed by Mestdagh et al. (2018) that
# takes into account overlap with the mean; this sensitivity analysis examines
# if using the regular SD impacts results.

# Getting individual relativeSD
data_indices2 <- data_indices %>%
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

### Sample: Belgium vs US ---------------------------------------------------

# Based on whether the dummy variable in Model 2 representing sample (i.e.,
# either Belgium sample with 0-100 scale or US sample with 0-10 scale) shows
# that there is an impact of sample on how affective dynamic indices predict
# parental burnout severity or not, we will perform one of the following
# sensitivity analyses:

# 1 - If the dummy variable/its interactions with indices are significant: We
# will also examine two separate regression models, one per sample 

# 2 - If the dummy variable/its interactions are NOT significant/are trivial: We
# will also examine a regression model with the joined dataset but without any
# dummy variables

### Possible additional sensitivity analyses ---------------------------------


# 1 - If the performance::check_model function in the main analysis identifies
# outliers, we will run a sensitivity analysis where we exclude these outliers

# 2 - If we ID participants with an intraindividual SD = 0 for any daily diary
# variable (i.e., they answer the exact same for all days), we will exclude that
# participant from the entire analysis