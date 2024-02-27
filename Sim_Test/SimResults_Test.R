# Summarize the results

sim_results <- Test_02222024 %>%
  gather("var", "val", raw_bias.rapi_yint_est:convergence_rate.tspa_yint_est) %>%
  dplyr::select(-c(SIM_TIME:WARNINGS)) %>%
  separate(col = var, into = c("stats", "parmet"), sep = "\\.") %>%
  separate(col = parmet, into = c("method", "par", "result"),  sep = "_") %>%
  dplyr::select(-result) %>%
  spread(stats, val) %>%
  relocate(REPLICATIONS, .after = last_col()) %>%
  mutate(N_lab = as_factor(paste0("italic(N) == ", N)),
         beta1_lab = as_factor(paste0("\\beta_{1} == ", beta1)),
         beta2_lab = as_factor(paste0("\\beta_{1} == ", beta2)),
         beta3_lab = as_factor(paste0("\\beta_{1} == ", beta3)),
         cor_xm_lab = as_factor(paste0("Correlation_XM == ", cor_xm)),
         rel_lab = as_factor(paste0("Reliability == ", rel)))

write_csv(sim_results, "Sim_Test/Test_02222024")

# Plot results
sim_plots <- read.csv("Sim_Test/Test_02222024")
# # Bias
# sim_plots %>%
#   ggplot(aes(x = factor(N), y = bias, color = method)) +
#   geom_boxplot() +
#   facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
#   labs(x = "Sample Size (N)", y = "Bias")

# Raw Bias
sim_plots %>%
  ggplot(aes(x = factor(N), y = raw_bias, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  labs(x = "Sample Size (N)", y = "Raw Bias")

# Standard Bias
sim_plots %>%
  ggplot(aes(x = factor(N), y = std_bias, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  labs(x = "Sample Size (N)", y = "Standardized Bias")

# Trimmed Bias
sim_plots %>%
  ggplot(aes(x = factor(N), y = trim_bias, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  labs(x = "Sample Size (N)", y = "Trimmed Bias")

# Median-MAD Bias
sim_plots %>%
  ggplot(aes(x = factor(N), y = stdMed_bias, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  labs(x = "Sample Size (N)", y = "Median-MAD Bias")

# Raw Relative SE Bias
sim_plots %>%
  ggplot(aes(x = factor(N), y = raw_rse_bias, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  coord_cartesian(ylim = c(-0.5, 0.75)) +
  labs(x = "Sample Size (N)", y = "Raw Relative SE Bias")

# Median-Mad Relative SE Bias
sim_plots %>%
  ggplot(aes(x = factor(N), y = stdMed_rse_bias, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  coord_cartesian(ylim = c(-0.5, 0.75)) +
  labs(x = "Sample Size (N)", y = "Median-Mad Relative SE Bias")

# Trimmed Relative SE Bias
sim_plots %>%
  ggplot(aes(x = factor(N), y = trim_rse_bias, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  coord_cartesian(ylim = c(-0.5, 0.75)) +
  labs(x = "Sample Size (N)", y = "Trimmed Relative SE Bias")

# Outlier_Percentage
sim_plots %>%
  ggplot(aes(x = factor(N), y = outlier_se, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  coord_cartesian(ylim = c(-0.5, 0.75)) +
  labs(x = "Sample Size (N)", y = "Percentage of SE Outliers")

# Coverage rate
sim_plots %>%
  ggplot(aes(x = factor(N), y = coverage, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  labs(x = "Sample Size (N)", y = "Coverage Rate (95%)")

# RMSE
sim_plots %>%
  ggplot(aes(x = factor(N), y = rmse, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  labs(x = "Sample Size (N)", y = "RMSE")

# Convergence Rate
sim_plots %>%
  ggplot(aes(x = factor(N), y = convergence_rate, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  labs(x = "Sample Size (N)", y = "Convergence Rate")
