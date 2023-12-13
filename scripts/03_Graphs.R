#---------------------------------------------------------------#
# Graphs & Figures                                              #
#---------------------------------------------------------------#

# This R script contains code for creating distribution & correlation graphs for
# all affective indices and for PBA scores

# All code and materials for this project can be found here: https://osf.io/5nfbu/

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# R packages -----------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(tidyverse)
library(ggplot2) # for graphs

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Importing Data -------------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

data_indices <- readr::read_csv(here::here("data", "processed", "data_indices.csv"))

# First ID'ing participants with SD = 0
pns_zerovar <- data_indices %>%
  dplyr::filter(sd_Exh == 0 | sd_Dist == 0 | sd_FedUp == 0) %>%
  dplyr::distinct(ID, .keep_all = T) 

# Creating dataset adapted for ggplot (all indices in one column...)
data_indices_long <- data_indices %>%
  dplyr::group_by(ID) %>%
  dplyr::select(-c(n_day:FedUp, mean_Exh:sd_FedUp, Exh_scaled:FedUp_scaled)) %>%
  dplyr::slice(1) %>%
  dplyr::rename_with(., ~str_replace_all(., '_scaled', '')) %>%
  tidyr::pivot_longer(cols = rsd_Exh:AR.1_FedUp, names_to = c("Index", "Variable"),
                       names_sep = '_',values_to = "Value")

# Remove participants with SD = 0 (no fluctuations, can't compute many indices...)
data_indices_long <- data_indices_long %>%
  filter(!ID %in% pns_zerovar$ID)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1) Graph parameters --------------------------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# # Setting shapes & colors for Sample
shape_sample <- c("Belgium" = 15, "US" = 16)
color_sample <- c("Belgium" = "#7fcdbb", "US" = "#2c7fb8")

# Labels for plots
label_vars <- c(Exh = "Exhaustion", Dist = "Distance", FedUp = "Fed Up")
label_AR <- c(AR.1 = "AR(1)", AR.mlVAR = "Multilevel VAR", AR.1ml = "Multilevel AR(1)")
label_ind <- c(mean = "Mean", sd = "SD", rsd = "Relative SD")

# Grouping variables for graphs (which indices are plotted together)
AR_plot_vars <- list("AR.1", "AR.1ml", "AR.mlVAR")
desc_plot_vars <- c("mean", "sd")
otherindices_plot_vars <- list("rsd", "ICC")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2) Violin plots (separated by SAMPLE) --------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Fig 1: Mean & SD Distribution (Violin plot) --------------------------------------------

#postscript(file = here::here("output", "figures", "./Fig1_violin_mean_sd.eps"), width = 4, height = 4)
#jpeg(file = here::here("output", "figures", "./Fig1_violin_mean_sd.jpg"), width = 4, height = 4, units = "in", res = 1000)

ggplot(subset(data_indices_long, Index %in% desc_plot_vars),  aes(x = Sample, y = Value, fill = Sample)) +
  geom_violin(alpha = 0.6, draw_quantiles = .5) +
  geom_point(position = position_jitter(seed = 1, width = 0.2), alpha = 0.1) + 
  scale_fill_manual(values = color_sample) +
  facet_grid(Variable ~ Index, 
             labeller = labeller(Index = c(mean = "Mean", sd = "SD"), 
                                 Variable = label_vars)) + 
  ylab("Affective Index Value") +
  theme_bw() +
  theme(legend.position = "bottom") 

#dev.off()


### Fig 2: PBA Distribution (violin plots) --------------------------------------------

# PBA Cut-off parameters
cut_offs <- data.frame(yintercept = c(52.7, 86.3), Threshold = c("Moderate", "Severe"))
threshold_color <- c("Moderate" = "deeppink4", "Severe" = "red")

#postscript(file = here::here("output", "figures", "./Fig2_violin_PBA.eps"), width = 3, height = 3)
#jpeg(file = here::here("output", "figures", "./Fig2_violin_mean_PBA.jpg"), width = 3, height = 3, units = "in", res = 1000)

ggplot(data = data_indices_long %>% group_by(ID) %>%slice(1),  aes(x = Sample, y = PBA_total, fill = Sample)) +
  geom_violin(alpha = 0.6, draw_quantiles = c(.25, .5, .75)) +
  geom_point(position = position_jitter(seed = 1, width = 0.2), alpha = 0.3) + 
  scale_fill_manual(values = color_sample) + 
  ylab("PBA Score") +
  geom_hline(aes(yintercept = yintercept, linetype = Threshold, color = Threshold), cut_offs) +
  scale_color_manual(values = threshold_color) +
  coord_cartesian(ylim = c(0,138)) + # PBA possible range
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  guides(fill = "none",
         linetype = guide_legend(title = "Cutoffs:"),
         color = guide_legend(title = "Cutoffs:")) + # Taking out sample from legend
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.background = element_rect(color = "lightgray"))

#dev.off()


### Fig 4: Inertia (AR) distribution --------------------------------------------

#postscript(file = here::here("output", "figures", "./Fig4_violin_AR.eps"), width = 8, height = 6)
#jpeg(file = here::here("output", "figures", "./Fig4_violin_AR.jpg"), width = 5, height = 5, units = "in", res = 1000)

# First get all AR/inertia indices - on the max scale
ggplot(subset(data_indices_long, Index %in% AR_plot_vars), aes(x = Sample, y = Value, fill = Sample)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark gray") +
  geom_violin(alpha = 0.6,  draw_quantiles = .5) +
  geom_point(position = position_jitter(seed = 1, width = 0.2), alpha = 0.1) + 
  scale_fill_manual(values = color_sample) +
  facet_grid(Variable ~ Index,
             labeller = labeller(Index = label_AR, Variable = label_vars)) + 
  ylab("Affective Index Value") +
  theme_bw() +
  theme(legend.position = "bottom") 

#dev.off()


### Fig S1: ICC Distribution (Violin plot) --------------------------------------------

#postscript(file = here::here("output", "figures", "./FigS1_violin_icc.eps"), width = 3, height = 3)
#jpeg(file = here::here("output", "figures", "./FigS1_violin_icc.jpg"), width = 3, height = 3, units = "in", res = 1000)

ggplot(subset(data_indices_long, Index %in% "ICC"), aes(x = Sample, y = Value, fill = Sample)) +
  geom_violin(alpha = 0.6, draw_quantiles = .5) +
  geom_point(position = position_jitter(seed = 1, width = 0.2), alpha = 0.3) + 
  scale_fill_manual(values = color_sample) +
  facet_wrap(vars(Index)) + 
  ylab("Affective Index Value") +
  theme_bw() 

#dev.off()


### Fig S2: relative SD distributions (violin plots) --------------------------------------------

#postscript(file = here::here("output", "figures", "./FigS2_violin_rsd.eps"), width = 8, height = 3)
#jpeg(file = here::here("output", "figures", "./FigS2_violin_rsd.jpg"), width = 8, height = 3, units = "in", res = 1000)

ggplot(subset(data_indices_long, Index %in% "rsd"), aes(x = Sample, y = Value, fill = Sample))+
  geom_violin(alpha = 0.6, draw_quantiles = .5) +
  geom_point(position = position_jitter(seed = 1, width = 0.2), alpha = 0.3) + 
  scale_fill_manual(values = color_sample) +
  facet_grid(Index ~ Variable,
             labeller = labeller(Index = c(rsd = "Relative SD"), Variable = label_vars)) + 
  ylab("Affective Index Value") +
  theme_bw()

#dev.off()



# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3) Correlation plots (Index x PBA) -----------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Fig S3: Mean & SD & rSD Correlation Plot --------------------------------------------

#postscript(file = here::here("output", "figures", "./FigS3_corr_mean_sd.eps"), width = 8, height = 6)
#jpeg(file = here::here("output", "figures", "./FigS3_corr_mean_sd.jpg"), width = 8, height = 6, units = "in", res = 1000)

ggplot(subset(data_indices_long, Index %in% c("mean", "sd", "rsd")), aes(x=Value, y=PBA_total)) +
  geom_point(aes(color = Sample, shape = Sample)) +
  ggpubr::stat_cor(method = "pearson", show.legend = FALSE, r.accuracy = .01, p.accuracy = .001) +
  facet_grid(Variable ~ Index, 
             scales='free',
             labeller = labeller(Variable = label_vars, Index = label_ind)) +
  scale_color_manual(values = color_sample)  +
  scale_shape_manual(values = shape_sample) +
  ylab("Parental Burnout Assessment Score") + xlab("Affective Index Value") +
  theme_bw() 

#dev.off()


### Fig S4: Inertia Correlation plots --------------------------------------------

#postscript(file = here::here("output", "figures", "./FigS4_corr_AR.eps"), width = 8, height = 6)
#jpeg(file = here::here("output", "figures", "./FigS4_corr_AR.jpg"), width = 8, height = 6, units = "in", res = 1000)

ggplot(subset(data_indices_long, Index %in% AR_plot_vars), aes(x=Value, y=PBA_total)) +
  geom_vline(xintercept = 0, color = "dark gray", linetype = "dashed") +
  geom_point(aes(color = Sample, shape = Sample)) +
  ggpubr::stat_cor(method = "pearson", show.legend = FALSE) +
  facet_grid(Variable ~ Index, scales='free', 
             labeller = labeller(Index = label_AR, Variable = label_vars)) +
  scale_color_manual(values = color_sample) +
  scale_shape_manual(values = shape_sample) +
  ylab("Parental Burnout Assessment Score") + xlab("Affective Index Value") +
  theme_bw() 

#dev.off()


### Fig S5: ICC Correlation Plot --------------------------------------------

#postscript(file = here::here("output", "figures", "./FigS5_corr_icc.eps"), width = 4, height = 3)
#jpeg(file = here::here("output", "figures", "./FigS5_corr_icc.jpg"), width = 4, height = 3, units = "in", res = 1000)

ggplot(subset(data_indices_long, Index %in% "ICC"), aes(x=Value, y=PBA_total)) +
  geom_point(aes(color = Sample, shape = Sample)) +
  ggpubr::stat_cor(method = "pearson", show.legend = FALSE) +
  facet_grid(~Index) +
  scale_color_manual(values = color_sample) +  
  scale_shape_manual(values = shape_sample) +
  ylab("Parental Burnout Assessment") +
  theme_bw() 

#dev.off()



