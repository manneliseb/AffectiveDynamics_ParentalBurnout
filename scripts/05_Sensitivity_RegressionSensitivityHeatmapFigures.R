#---------------------------------------------------------------#
# Heatmap with Sensitivity Anaysis Results                      #
#---------------------------------------------------------------#
# All code and materials for this project can be found here: https://osf.io/5nfbu/

# This R script contains code to generate a heatmap visualizing the results from
# the different sensitivity analyses

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Creating significance heatmap tables ---------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(ggpattern) #for heatmap figures

# Setting colors for graphs
sig_palette <- c("0" = "black", "1" = "#5ab4ac")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Importing binary significance tables ---------------------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Note - these tables were manually coded from the tables in script 04 RegressionModels,
# since the output of those was directly as .xls

sensitivity_boot <- readr::read_csv2(here::here("data", "processed_sharing", "SensitivityTable_Bootstrap.csv"))
sensitivity_boot_dummy <- readr::read_csv2(here::here("data", "processed_sharing", "SensitivityTable_Bootstrap_Dummy.csv"))

sensitivity_boot_BE <- readr::read_csv2(here::here("data", "processed_sharing", "SensitivityTable_Bootstrap_Belgium.csv"))
sensitivity_boot_US <- readr::read_csv2(here::here("data", "processed_sharing", "SensitivityTable_Bootstrap_US.csv"))

sensitivity_raw <- readr::read_csv2(here::here("data", "processed_sharing", "SensitivityTable_RawData.csv"))
sensitivity_raw_dummy <- readr::read_csv2(here::here("data", "processed_sharing", "SensitivityTable_RawData_Dummy.csv"))


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Heatmap: Bootstrapped data (Main model; Fig 3) -----------------------------
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### Formatting data for graphing ---------------------------------------------

# Make dataframes long format for plotting
long_boot <- sensitivity_boot %>%
  tidyr::pivot_longer(!Predictor, names_to = "Model", values_to = "Significant")

long_boot_dummy <- sensitivity_boot_dummy %>%
  tidyr::pivot_longer(!Predictor, names_to = "Model", values_to = "DummySignificant") %>%
  # Change '*' to 'x' (more centered when plotting...)
  dplyr::mutate(DummySignificant = str_replace(DummySignificant, "\\*", "Yes")) %>%
  dplyr::mutate(DummySignificant = tidyr::replace_na(DummySignificant, "No"))


# Merge dataframes
long_boot <- merge(long_boot, long_boot_dummy, by = c("Predictor", "Model"))

# Keep order of Models & Variables 
long_boot <- long_boot %>% 
  mutate(Predictor = factor(Predictor), Model = factor(Model)) %>%
  mutate(Model = fct_relevel(Model, c("Just means & dummy")),
         Predictor = fct_relevel(Predictor, c("Belgium (Dummy)", "Mean Dist", "Mean Exh", "Mean Fed Up"))) %>%
  mutate(Predictor = fct_relevel(Predictor, "Covariation", after = Inf)) %>%
  mutate(Predictor = fct_rev(Predictor), Significant = factor(Significant))

# Check factor levels in correct order for plotting etc. 
levels(long_boot$Predictor)
levels(long_boot$Model)


### Create heatmap (colors) ---------------------------------------------

heatmap_sig_main <- ggplot(data = long_boot, aes(x = Model, y = Predictor,  pattern = DummySignificant, fill = Significant)) +
  ggpattern::geom_tile_pattern(color = 'white', lwd = .5,
                               pattern_fill = "white",
                               pattern_color = NA,
                               pattern_density = .1,
                               pattern_spacing = .02,
                               pattern_angle = 45.,
                               pattern_key_scale_factor = 1) + 
  # Making cells crosshatched if that variable had a significant interaction
  # with the dummy variable (= Sample)
  scale_pattern_manual(values = c(Yes = "crosshatch", No = "none"), 
                       guide = guide_legend(override.aes = list(fill = "gray"))) +
  scale_fill_manual(values = sig_palette,
                    na.value = "white", 
                    guide = guide_legend(override.aes = list(pattern = "none"))) +
  labs(x = "Sensitivity Models",
       y = "Predictors",
       fill = "Significant",
       pattern = "Dummy Interaction") +
  coord_equal() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(legend.position = "bottom")  +
  scale_x_discrete(expand = expansion(0,0)) + scale_y_discrete(expand = expansion(0,0))



heatmap_sig_main

# Save plot
ggplot2::ggsave(filename = here::here("output", "figures", "./Fig3_SigHeatmeap_Bootstrap.eps"),
                plot = heatmap_sig_main,
                device = "eps", dpi = 1200, width = 6, height = 6, units = "in")

ggplot2::ggsave(filename = here::here("output", "figures", "./Fig3_SigHeatmeap_Bootstrap.jpg"),
                plot = heatmap_sig_main,
                device = "jpg", dpi = 1200, width = 6, height = 6, units = "in")


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Heatmap: All other sensitivity analyses (Figure S6 in supp. materials) -----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# i.e.: Raw data (but joint sample), and then by sample (Belgian & US, both bootstrapped)

### Formatting data for graphing ---------------------------------------------

# Make dataframes long format for plotting
long_boot_BE <- sensitivity_boot_BE %>%
  tidyr::pivot_longer(!Predictor, names_to = "Model", values_to = "Significant") %>%
  dplyr::mutate(DummySignificant = rep("No", n())) %>%
  dplyr::mutate(Source = "Belgium Sample (Bootstrapped)")

long_boot_US <- sensitivity_boot_US %>%
  tidyr::pivot_longer(!Predictor, names_to = "Model", values_to = "Significant") %>%
  dplyr::mutate(DummySignificant = rep("No", n())) %>%
  dplyr::mutate(Source = "US Sample (Bootstrapped)")

# Format & join together raw data (with dummy interactions) as well

long_raw <- sensitivity_raw %>%
  tidyr::pivot_longer(!Predictor, names_to = "Model", values_to = "Significant")

long_raw_dummy <- sensitivity_raw_dummy %>%
  tidyr::pivot_longer(!Predictor, names_to = "Model", values_to = "DummySignificant") %>%
  dplyr::mutate(DummySignificant = str_replace(DummySignificant, "\\*", "Yes")) %>%
  dplyr::mutate(DummySignificant = tidyr::replace_na(DummySignificant, "No")) 

# Merge raw dataframes
long_raw <- merge(long_raw, long_raw_dummy, by = c("Predictor", "Model")) %>%
  mutate(Source = "Raw Data (Joint Sample)")


# Merge all dataframes
long_boot_extra <- bind_rows(long_boot_BE, long_boot_US, long_raw)


# Set order of Models & Variables 
long_boot_extra <- long_boot_extra %>% 
  mutate(Predictor = factor(Predictor), Model = factor(Model)) %>%
  mutate(Model = fct_relevel(Model, c("Means")),
         Predictor = fct_relevel(Predictor, c("Belgium (Dummy)", "Mean Dist", "Mean Exh", "Mean Fed Up"))) %>%
  mutate(Predictor = fct_relevel(Predictor, "Covariation", after = Inf)) %>%
  mutate(Predictor = fct_rev(Predictor), Significant = factor(Significant)) %>%
  mutate(Source = fct_relevel(Source, "Raw Data (Joint Sample)"))


### Create heatmap (colors) ---------------------------------------------

heatmap_sig_extra <- ggplot(data = long_boot_extra, aes(x = Model, y = Predictor,  pattern = DummySignificant, fill = Significant)) +
  ggpattern::geom_tile_pattern(color = 'white', lwd = .5,
                               pattern_fill = "white",
                               pattern_color = NA,
                               pattern_density = .1,
                               pattern_spacing = .02,
                               pattern_angle = 45.,
                               pattern_key_scale_factor = 1) + 
  scale_pattern_manual(values = c(Yes = "crosshatch", No = "none"), 
                       guide = guide_legend(override.aes = list(fill = "gray"))) +
  scale_fill_manual(values = sig_palette,
                    na.value = "white", 
                    guide = guide_legend(override.aes = list(pattern = "none"))) +
  labs(x = "Sensitivity Models",
       y = "Predictors",
       fill = "Significant",
       pattern = "Dummy Interaction") +
  coord_equal() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        legend.position = "bottom") +
  scale_x_discrete(expand = expansion(0,0)) + scale_y_discrete(expand = expansion(0,0)) +
  facet_wrap(~Source)

heatmap_sig_extra

# Save plot
ggplot2::ggsave(filename = here::here("output", "figures", "./FigS6_SigHeatmeap_3Others.eps"),
                plot = heatmap_sig_extra,
                device = "eps", dpi = 1200, width = 8, height = 5, units = "in")

ggplot2::ggsave(filename = here::here("output", "figures", "./FigS6_SigHeatmeap_3Others.jpg"),
                plot = heatmap_sig_extra,
                device = "jpg", dpi = 1200, width = 8, height = 5, units = "in")


