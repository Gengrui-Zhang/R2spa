# Load packages
library(haven)
library(dplyr)
library(psych)
library(semTools)
library(lavaan)
library(emmeans)
library(ggplot2)
library(gridExtra)
library(R2spa)

# Import Dataset
# Information of demonstration data:
# Transition into Adulthood Supplement to the Panel Study of Income Dynamics,
# public use dataset [restricted use data, if appropriate].
# Produced and distributed by the Survey Research Center, Institute for Social Research,
# University of Michigan, Ann Arbor, MI (2019).
TA2019 <- zap_formats(zap_labels(read_sav("Qual 2 Paper Draft/TA2019.sav")))

# Hypothesis: Depression is predicted by sef-esteem, Perceived EveryDay Discrimination (PED),
# and their interaction effect.

# Data Preprocessing

# Scale 1: ROSENBURG SELF-ESTEEM SCALE
# Recoded items: TA190106, TA190108, TA190111, TA190112, TA190113
# Strongly agree - 4; Agree - 3; Disagree - 2; Strongly Disagree - 1; Missing - 9.
self_esteem <- TA2019 %>%
  select(TA190104:TA190113) %>%
  mutate(TA190106 = recode(TA190106, "1" = "4", "2" = "3", "3" = "2", "4" = "1", "9" = "9"),
         TA190108 = recode(TA190108, "1" = "4", "2" = "3", "3" = "2", "4" = "1", "9" = "9"),
         TA190111 = recode(TA190111, "1" = "4", "2" = "3", "3" = "2", "4" = "1", "9" = "9"),
         TA190112 = recode(TA190112, "1" = "4", "2" = "3", "3" = "2", "4" = "1", "9" = "9"),
         TA190113 = recode(TA190113, "1" = "4", "2" = "3", "3" = "2", "4" = "1", "9" = "9")) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_all(na_if, 9)

# Scale 2: Everyday Discrimination
# No recoded items
# Never - 1; Less than once a year - 2; A few times a year - 3; A few times a month - 4;
# At least once a week - 5; Almost every day - 6; Missing - 9.
discrimination <- TA2019 %>%
  select(TA192066:TA192072) %>%
  mutate_all(na_if, 9) %>%
  mutate_all(na_if, 8)

# Scale 3: PHQ-9 Depression Scale
# No recoded items
# Not at all - 1; Several days - 2; More than half the days - 3; Nearly every day - 4; Missing - 9.
phq9 <- TA2019 %>%
  select(TA190114:TA190122) %>%
  mutate_all(na_if, 9) %>%
  mutate_all(na_if, 8)

# Examine Reliability using Internal Consistency
# Self-esteem
a_selfEst <- alpha(self_esteem) # alpha = 0.88
# Order of items based on r.cor
apply(a_selfEst$item.stats["r.cor"], 2, sort, decreasing = T)
# First seven items with the highest r.cor": TA190113, TA190112, TA190109, TA190110,
#                                            TA190105, TA190106, TA190108
# Discrimination
a_disc <- alpha(discrimination) # alpha = 0.90
# Order of items based on r.cor
apply(a_disc$item.stats["r.cor"], 2, sort, decreasing = T)
# Order seven items with the highest r.cor": TA192072, TA192068, TA192071, TA192066,
#                                            TA192070, TA192067, TA192069
# Depression
alpha(phq9) # alpha = 0.87

# Dataset for model fitting
int_df <- cbind(discrimination,
                self_esteem,
                phq9)

# The Common Model for all the methods
model <- "# Measurement model
            Depression =~ TA190114 + TA190115 + TA190116 + TA190117 + TA190118 + TA190119 + TA190120 + TA190121 + TA190122
            Esteem =~ TA190105 + TA190106 + TA190108 + TA190109 + TA190110 + TA190112 + TA190113
            Discrimination =~ TA192072 + TA192068 + TA192071 + TA192066 + TA192070 + TA192067 + TA192069
          # Structural model
            Depression ~ b1*Discrimination + b2*Esteem + b3*Esteem:Discrimination
          # Define Standardized Coefficients
            Esteem ~~ v1*Esteem
            Discrimination ~~ v2*Discrimination
          # Standardized Coefficients
            beta1 := b1*sqrt(v1)
            beta2 := b2*sqrt(v2)
            beta3 := b3*sqrt(v1)*sqrt(v2)"

# Using matched-pair UPI to fit the latent interaction model
# Notes: According to the suggestions of using product indicator strategies for matched-pair UPI in Marsh et al.(2014),
#        One possibility would be to matched the best items from the first factor with the equal number of item from the second factor.

upi_match_fit <- upi(data = int_df,
                    model = model,
                    mode = "match")
summary(upi_match_fit,
        fit.measure = TRUE,
        standardized = TRUE)

# Using all-pair UPI to fit the latent interaction model

upi_all_fit <- upi(data = int_df,
                    model = model,
                    mode = "all")

summary(upi_all_fit,
        fit.measure = TRUE)

# Use RAPI to fit the latent interaction model

rapi_mod_fit <- rapi(model = model,
                     data = int_df)

summary(rapi_mod_fit,
        fit.measure = TRUE)

# Use 2S-PA-Int to fit the latent interaction model
# Stage 1: Use Bartlett scores as factor scores
fs_mod <- "Depression =~ TA190114 + TA190115 + TA190116 + TA190117 + TA190118 + TA190119 + TA190120 + TA190121 + TA190122
           Esteem =~ TA190104 + TA190105 + TA190106 + TA190107 + TA190108 + TA190109 + TA190110 + TA190111 + TA190112 + TA190113
           Discrimination =~ TA192066 + TA192067 + TA192068 + TA192069 + TA192070 + TA192071 + TA192072"

fs_int_df <- get_fs(int_df,
                    model = fs_mod,
                    method = "Bartlett",
                    std.lv = T)

# Stage 2: Fit 2S-PA-Int Model
tspa_mod_fit <- tspa(model = "Depression ~ b1*Discrimination + b2*Esteem + b3*Esteem:Discrimination
                              beta1 := b1 * sqrt(v1)
                              beta2 := b2 * sqrt(v2)
                              beta3 := b3 * sqrt(v1) * sqrt(v2)",
                      data = fs_int_df,
                      se = list(Discrimination = fs_int_df$fs_Discrimination_se[1],
                                Depression = fs_int_df$fs_Depression_se[1],
                                Esteem = fs_int_df$fs_Esteem_se[1]))

summary(tspa_mod_fit,
        fit.measures = TRUE)

# Plot the interaction effects
int_plot <- function(mod_fit,
                     dv = "Depression",
                     iv = "Discrimination",
                     moderator = "Esteem",
                     sd_scale = 1) {

  data <- as.data.frame(lavPredict(mod_fit))
  mean_minus_sd <- round(mean(data[,moderator], na.rm = T) - sd_scale*sd(data[,moderator], na.rm = T), 3)
  mean_sd <- round(mean(data[,moderator], na.rm = T), 3)
  mean_plus_sd <- round(mean(data[,moderator], na.rm = T) + sd_scale*sd(data[,moderator], na.rm = T), 3)

  mod_list <- list(dv = seq(-2, 4, by = 0.4), iv = c(mean_minus_sd, mean_sd, mean_plus_sd))
  names(mod_list) <- c(iv, moderator)
  le_mod <- lm(paste(dv, "~", iv, "*", moderator), data)

  # Use emmip to estimate marginal means for plotting
  mod_data <- emmip(le_mod,
                    as.formula(paste(moderator, "~", iv)),
                    at = mod_list,
                    CIs = TRUE,
                    plotit = FALSE)

  # Adjust the Esteem variable for plotting
  mod_data$fmod <- factor(as.character(mod_data[,moderator]))
  levels(mod_data$fmod) <- c("low", "med", "high")

  # Generate the plot
  mod_plot <- ggplot(mod_data, aes(x = .data[[iv]], y = yvar, color = fmod)) +
    geom_line() +
    ylab("Depression") +
    xlab("Discrimination") +  # Changed from "PED" to match the variable name
    ylim(-1, 3) +
    labs(colour = "Self-esteem") +
    theme_classic()

  return(mod_plot)
}

grid.arrange(int_plot(upi_all_fit), int_plot(upi_match_fit),
             int_plot(rapi_mod_fit), int_plot(tspa_mod_fit), ncol = 2)
