# Summarize the results

sim_results <- All_BoundBeta_02062024 %>%
  gather("var", "val", std_bias.rapi_yint_est:rse_bias.tspa_yint_se) %>%
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

write_csv(sim_results, "Sim_Data/All_BoundBeta_02062024.csv")

# Plot results
sim_plots <- read.csv("Sim_Data/All_BoundBeta_02062024.csv")
# # Bias
# sim_plots %>%
#   ggplot(aes(x = factor(N), y = bias, color = method)) +
#   geom_boxplot() +
#   facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
#   labs(x = "Sample Size (N)", y = "Bias")

# Standard Bias
sim_plots %>%
  ggplot(aes(x = factor(N), y = std_bias, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  labs(x = "Sample Size (N)", y = "Standardized Bias")

# Relative SE Bias
sim_plots %>%
  ggplot(aes(x = factor(N), y = rse_bias, color = method)) +
  geom_boxplot() +
  facet_grid(cor_xm_lab ~ rel_lab, labeller = label_parsed) +
  coord_cartesian(ylim = c(-0.5, 0.75)) +
  labs(x = "Sample Size (N)", y = "Relative SE Bias")

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
