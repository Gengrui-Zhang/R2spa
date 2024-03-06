# Load packages
library(haven)
library(dplyr)
library(tidyverse)
library(modelsummary)
library(psych)
library(sjPlot)
library(ufs)
library(semPlot)
library(lavaan)
library(semTools)
library(emmeans)
library(ggpubr)

# Import Dataset
TA2019 <- zap_formats(zap_labels(read_sav("Qual 2 Paper Draft/TA2019.sav")))

# Step 1: Extract Scales

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

# Check score distributions

apply(self_esteem, 2, table)
barplot(apply(apply(self_esteem, 2, table), 1, sum))

# Scale 2: General Anxiety Disorder (GAD) Scale
# No recoded items
# Not at all - 1; Several days - 2; More than half the days - 3; Nearly every day - 4; Missing - 9.

gad <- TA2019 %>%
  select(TA190124:TA190130) %>%
  mutate_all(na_if, 9) %>%
  mutate_all(na_if, 8)

# Check score distributions
apply(gad, 2, table)
barplot(apply(apply(gad, 2, table), 1, sum))

# Scale 3: Social Anxiety
# No recoded items
# Never - 1; Rarely - 2; Sometimes - 3; Often - 4; Almost always or always - 5; Missing - 9.

social_anxiety <- TA2019 %>%
  select(TA190100:TA190103) %>%
  mutate_all(na_if, 9)

# Check score distributions
apply(social_anxiety, 2, table)
barplot(apply(apply(social_anxiety, 2, table), 1, sum))

# Scale 4: Well-being
# No recoded items
# Never - 1; Once or twice - 2; About once a week - 3; Two or three times a week - 4;
# Almost every day - 5; Every day - 6; Missing - 9.

emotional_wb <- TA2019 %>%
  select(TA190070:TA190072) %>%
  mutate_all(na_if, 9) %>%
  mutate_all(na_if, 8)

social_wb <- TA2019 %>%
  select(TA190073:TA190077) %>%
  mutate_all(na_if, 9) %>%
  mutate_all(na_if, 8)

psychological_wb <- TA2019 %>%
  select(TA190078:TA190083) %>%
  mutate_all(na_if, 9) %>%
  mutate_all(na_if, 8)

# Check score distributions
apply(emotional_wb, 2, table)
apply(social_wb, 2, table)
apply(psychological_wb, 2, table)

# Scale 5: PHQ-9 Depression Scale
# No recoded items
# Not at all - 1; Several days - 2; More than half the days - 3; Nearly every day - 4; Missing - 9.

phq9 <- TA2019 %>%
  select(TA190114:TA190122) %>%
  mutate_all(na_if, 9) %>%
  mutate_all(na_if, 8)

# Check score distributions
apply(phq9, 2, table)
barplot(apply(apply(phq9, 2, table), 1, sum))

# Scale 6: Psychological Distress
# All items are recoded for easier interpretation
# None of the time - 1; A little of the time - 2; Some of the time - 3; Most of the time - 4; All of the time - 5; Missing - 9.

psy_distress <- TA2019 %>%
  select(TA191961:TA191966) %>%
  mutate(TA191961 = recode(TA191961, "1" = "5", "2" = "4", "3" = "3", "4" = "2", "5" = "1", "8" = "8", "9" = "9"),
         TA191962 = recode(TA191962, "1" = "5", "2" = "4", "3" = "3", "4" = "2", "5" = "1", "8" = "8", "9" = "9"),
         TA191963 = recode(TA191963, "1" = "5", "2" = "4", "3" = "3", "4" = "2", "5" = "1", "8" = "8", "9" = "9"),
         TA191964 = recode(TA191964, "1" = "5", "2" = "4", "3" = "3", "4" = "2", "5" = "1", "8" = "8", "9" = "9"),
         TA191965 = recode(TA191965, "1" = "5", "2" = "4", "3" = "3", "4" = "2", "5" = "1", "8" = "8", "9" = "9"),
         TA191966 = recode(TA191966, "1" = "5", "2" = "4", "3" = "3", "4" = "2", "5" = "1", "8" = "8", "9" = "9")) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_all(na_if, 9) %>%
  mutate_all(na_if, 8)

# Check score distributions
apply(psy_distress, 2, table)
barplot(apply(apply(psy_distress, 2, table), 1, sum))

# Scale 7: Everyday Discrimination
# No recoded items
# Never - 1; Less than once a year - 2; A few times a year - 3; A few times a month - 4;
# At least once a week - 5; Almost every day - 6; Missing - 9.

discrimination <- TA2019 %>%
  select(TA192066:TA192072) %>%
  mutate_all(na_if, 9) %>%
  mutate_all(na_if, 8)

# Check score distributions
apply(discrimination, 2, table)
barplot(apply(apply(discrimination, 2, table), 1, sum))

# Step 2: Exploratory Data Analysis

datasummary_skim(self_esteem, fmt = "%.2f")
# Comment: It seems that item 190111 and 190112 are not as skewed as others,
# indicating that a fairly amount of participants rate low scores on these two self-esteem items.
  tab_itemscale(self_esteem)
  # It seems that Item 190107 is more problematic for low item discrimination
datasummary_skim(gad, fmt = "%.2f")
datasummary_skim(social_anxiety, fmt = "%.2f")
datasummary_skim(emotional_wb, fmt = "%.2f")
datasummary_skim(social_wb, fmt = "%.2f")
datasummary_skim(psychological_wb, fmt = "%.2f")
datasummary_skim(phq9, fmt = "%.2f")
datasummary_skim(psy_distress, fmt = "%.2f")
datasummary_skim(discrimination, fmt = "%.2f")

# Step 3: Examine internal consistency

alpha(self_esteem)
alpha(gad)
alpha(social_anxiety)
alpha(emotional_wb)

alpha(social_wb)
# Comment: scale reliability of social well-being scale is quite low. It's probably due to
# small number of items in this scale

alpha(psychological_wb)
alpha(phq9)
alpha(psy_distress)
alpha(discrimination)

# Step 4: Variables Selection
# Group variables:
# Race, gender, states, regions
#
# group_var <- TA2019 %>%
#   select(TA190159, TA192131, TA192196, TA192197) %>%
#   mutate(TA190159 = ifelse(TA190159 %in% c(7, 9), NA, TA190159),
#          TA192131 = ifelse(TA192131 %in% c(6, 7, 8, 98, 99), NA, TA192131),
#          TA192196 = ifelse(TA192196 == 9, NA, TA192196),
#          TA192197 = ifelse(TA192197 %in% c(5, 6, 8, 9), NA, TA192197))
#
# colnames(group_var) <- c("Gender", "Race", "States", "Region")

# Step 5: Confirmatory Factor Analysis

# Depression

model_phq9_fa1 <- "Depression =~ TA190114 + TA190115 + TA190116 +
                    TA190117 + TA190118 + TA190119 +
                    TA190120 + TA190121 + TA190122"
phq9_fa1_fit <- cfa(model_phq9_fa1, phq9)
summary(phq9_fa1_fit, standardized = T, fit.measures = T)

# Categorical CFA
model_phq9_fa1 <- "Depression =~ TA190114 + TA190115 + TA190116 +
                    TA190117 + TA190118 + TA190119 +
                    TA190120 + TA190121 + TA190122"
phq9_fa1_fit <- cfa(model_phq9_fa1,
                    ordered = colnames(phq9))
summary(phq9_fa1_fit, standardized = T, fit.measures = T)

model_phq9_fa2 <- "Somatic =~ TA190114 + TA190115 + TA190116 +
                    TA190117 + TA190118;
                   Cognitive =~ TA190119 +
                    TA190120 + TA190121 + TA190122"
phq9_fa2_fit <- cfa(model_phq9_fa2, phq9)
summary(phq9_fa2_fit, standardized = T, fit.measures = T)

# Categorical CFA
model_phq9_fa2 <- "Somatic =~ TA190114 + TA190115 + TA190116 +
                    TA190117 + TA190118;
                   Cognitive =~ TA190119 +
                    TA190120 + TA190121 + TA190122"
phq9_fa2_fit <- cfa(model_phq9_fa2,
                    phq9,
                    ordered = colnames(phq9))
summary(phq9_fa2_fit, standardized = T, fit.measures = T)

# Comment:it seems that a two-factor structure of phq-9 supports a higher CFI and lower RMSEA and SRMR.

# Discrimination

model_discrimination_fa <- "Discrimination =~ TA192066 + TA192067 + TA192068 +
                                              TA192069 + TA192070 + TA192071 +
                                              TA192072"
discrimination_fa_fit <- cfa(model_discrimination_fa, discrimination)
summary(discrimination_fa_fit, standardized = T, fit.measures = T)

# Categorical CFA
model_discrimination_fa <- "Discrimination =~ TA192066 + TA192067 + TA192068 +
                                              TA192069 + TA192070 + TA192071 +
                                              TA192072"
discrimination_fa_fit <- cfa(model_discrimination_fa,
                             discrimination,
                             ordered = colnames(discrimination))
summary(discrimination_fa_fit, standardized = T, fit.measures = T)

# Comment:the one-factor structure for everyday discrimination scale is generally good with high CFI and low SRMR
#         However, the RMSEA is higher than .08 threhold.

# Self-esteem

model_self_esteem_fa <- "Esteem =~ TA190104 + TA190105 + TA190106 + TA190107 +
                                   TA190108 + TA190109 + TA190110 + TA190111 +
                                   TA190112 + TA190113"
self_esteem_fa_fit <- cfa(model_self_esteem_fa, self_esteem)
summary(self_esteem_fa_fit, standardized = T, fit.measures = T)

# Categorical CFA
model_self_esteem_fa <- "Esteem =~ TA190104 + TA190105 + TA190106 + TA190107 +
                                   TA190108 + TA190109 + TA190110 + TA190111 +
                                   TA190112 + TA190113"
self_esteem_fa_fit <- cfa(model_self_esteem_fa,
                          self_esteem,
                          ordered = colnames(self_esteem))
summary(self_esteem_fa_fit, standardized = T, fit.measures = T)

# Comment:the one-factor structure for everyday discrimination scale is generally good with high CFI and low SRMR
#         However, the RMSEA is higher than .08 threhold.

# General Anxiety
model_gad_fa <- "GAD =~ TA190124 + TA190125 + TA190126 + TA190127 +
                         TA190128 + TA190129 + TA190130"
gad_fa_fit <- cfa(model_gad_fa, gad)
summary(gad_fa_fit, standardized = T, fit.measures = T)

# Categorical CFA
model_gad_fa <- "GAD =~ TA190124 + TA190125 + TA190126 + TA190127 +
                         TA190128 + TA190129 + TA190130"
gad_fa_fit <- cfa(model_gad_fa,
                  gad,
                  ordered = colnames(gad))
summary(gad_fa_fit, standardized = T, fit.measures = T)

# Social Anxiety

model_soc_anx_fa <- "Soc_Anx =~ TA190100 + TA190101 + TA190102 + TA190103"
soc_anx_fa_fit <- cfa(model_soc_anx_fa, social_anxiety)
summary(soc_anx_fa_fit, standardized = T, fit.measures = T)

# Categorical CFA
model_soc_anx_fa <- "Soc_Anx =~ TA190100 + TA190101 + TA190102 + TA190103"
soc_anx_fa_fit <- cfa(model_soc_anx_fa,
                      social_anxiety,
                      ordered = colnames(social_anxiety))
summary(soc_anx_fa_fit, standardized = T, fit.measures = T)

# Psychological Distress

model_psy_dis_fa <- "Distress =~ TA191961 + TA191962 + TA191963 +
                          TA191964 + TA191965 + TA191966"
psy_dis_fa_fit <- cfa(model_psy_dis_fa, psy_distress)
summary(psy_dis_fa_fit, standardized = T, fit.measures = T)

# Categorical CFA
model_psy_dis_fa <- "Distress =~ TA191961 + TA191962 + TA191963 +
                          TA191964 + TA191965 + TA191966"
psy_dis_fa_fit <- cfa(model_psy_dis_fa,
                      psy_distress,
                      ordered = colnames(psy_distress))
summary(psy_dis_fa_fit, standardized = T, fit.measures = T)

# Step 6: Building up latent interaction models

# Get factor scores
int_df <- cbind(discrimination,
                 self_esteem,
                 psy_distress,
                 phq9,
                 gad,
                 social_anxiety,
                 emotional_wb,
                 social_wb,
                 psychological_wb)

int_df_model <- "Depression =~ TA190114 + TA190115 + TA190116 +
                            TA190117 + TA190118 + TA190119 +
                            TA190120 + TA190121 + TA190122

                  Esteem =~ TA190104 + TA190105 + TA190106 + TA190107 +
                            TA190108 + TA190109 + TA190110 + TA190111 +
                            TA190112 + TA190113

                  Discrimination =~ TA192066 + TA192067 + TA192068 + TA192069 +
                               TA192070 + TA192071 + TA192072

                  Distress =~ TA191961 + TA191962 + TA191963 +
                          TA191964 + TA191965 + TA191966

                  GAD =~ TA190124 + TA190125 + TA190126 + TA190127 +
                         TA190128 + TA190129 + TA190130

                  Soc_Anx =~ TA190100 + TA190101 + TA190102 + TA190103

                  Emo =~ TA190070 + TA190071 + TA190072

                  Social =~ TA190073 + TA190074 + TA190075 + TA190076 + TA190077

                  Psychological =~ TA190078 + TA190079 + TA190080 + TA190081 + TA190082 + TA190083
                  "
fs_int_df <- get_fs(int_df, model = int_df_model, method = "Bartlett", std.lv = T)

# Model 1: Depression is predicted by Self-esteem, discrimination, and their latent interaction

tspa_mod1_fit <- tspa(model = "Depression ~ Discrimination + Esteem + Esteem:Discrimination",
                    data = fs_int_df,
                    se = list(Discrimination = fs_int_df$fs_Discrimination_se[1],
                              Depression = fs_int_df$fs_Depression_se[1],
                              Esteem = fs_int_df$fs_Esteem_se[1]))
summary(tspa_mod1_fit, standardized = T, fit.measures = T)
parameterEstimates(tspa_mod1_fit)
fitmeasures(tspa_mod1_fit)

# Plot interaction
tspa_mod1_data <- as.data.frame(lavPredict(tspa_mod1_fit))
esteem_low <- round(mean(tspa_mod1_data$Esteem) - sd(tspa_mod1_data$Esteem), 3)
esteem <- round(mean(tspa_mod1_data$Esteem), 3)
esteem_high <- round(mean(tspa_mod1_data$Esteem) + sd(tspa_mod1_data$Esteem), 3)

tspa_mod1_list <- list(Discrimination = seq(-2, 4, by = 0.4), Esteem = c(esteem_low, esteem, esteem_high))
tspa_mod1 <- lm(Depression ~ Discrimination*Esteem, tspa_mod1_data)
tspamod1dat <- emmip(tspa_mod1, Esteem ~ Discrimination, at = tspa_mod1_list, CIs = TRUE, plotit = FALSE)
tspamod1dat$fEsteem <- factor(as.character(tspamod1dat$Esteem))
levels(tspamod1dat$fEsteem) <- c("low","med","high")

tspa_mod1_plot <- ggplot(tspamod1dat, aes(x = Discrimination, y = yvar, color = fEsteem)) +
                    geom_line() +
                    ylab("Depression") +
                    xlab("PED") +
                    ylim(-1, 3) +
                    labs(colour = "Self-esteem") +
                    theme_classic()

# Model 1: Using rapi

rapi_mod1 <- " Depression =~ TA190114 + TA190115 + TA190116 +
                               TA190117 + TA190118 + TA190119 +
                               TA190120 + TA190121 + TA190122

                 Esteem =~ TA190104 + TA190105 + TA190106 + TA190107 +
                           TA190108 + TA190109 + TA190110 + TA190111 +
                           TA190112 + TA190113

                 Discrimination =~ TA192066 + TA192067 + TA192068 + TA192069 +
                                   TA192070 + TA192071 + TA192072

                 Depression ~ Discrimination + Esteem + Esteem:Discrimination"
rapi_mod1_fit <- rapi(data = int_df,
                      model = rapi_mod1)
summary(rapi_mod1_fit,
        fit.measure = TRUE,
        standardized = TRUE)
parameterEstimates(rapi_mod1_fit)

lavInspect(rapi_mod1_fit, what = "data")
lavPredict(rapi_mod1_fit)

# Plot interaction
rapi_mod1_data <- as.data.frame(lavPredict(rapi_mod1_fit))
esteem_low <- round(mean(rapi_mod1_data$Esteem) - sd(rapi_mod1_data$Esteem), 3)
esteem <- round(mean(rapi_mod1_data$Esteem), 3)
esteem_high <- round(mean(rapi_mod1_data$Esteem) + sd(rapi_mod1_data$Esteem), 3)

rapi_mod1_list <- list(Discrimination = seq(-2, 4, by = 0.4), Esteem = c(esteem_low, esteem, esteem_high))
rapi_mod1 <- lm(Depression ~ Discrimination*Esteem, rapi_mod1_data)
rapimod1dat <- emmip(rapi_mod1, Esteem ~ Discrimination, at = rapi_mod1_list, CIs = TRUE, plotit = FALSE)
rapimod1dat$fEsteem <- factor(as.character(rapimod1dat$Esteem))
levels(rapimod1dat$fEsteem) <- c("low","med","high")

rapi_mod1_plot <- ggplot(rapimod1dat, aes(x = Discrimination, y = yvar, color = fEsteem)) +
                    geom_line() +
                    ylab("Depression") +
                    xlab("PED") +
                    labs(colour = "Self-esteem") +
                    theme_classic()

# Model 1: Using upi

upi_mod1 <- " Depression =~ TA190114 + TA190115 + TA190116 +
                               TA190117 + TA190118 + TA190119 +
                               TA190120 + TA190121 + TA190122

                 Esteem =~ TA190104 + TA190105 + TA190106 + TA190107 +
                           TA190108 + TA190109 + TA190110 + TA190111 +
                           TA190112 + TA190113

                 Discrimination =~ TA192066 + TA192067 + TA192068 + TA192069 +
                                   TA192070 + TA192071 + TA192072

                 Depression ~ Discrimination + Esteem + Esteem:Discrimination "
upi_mod1_fit <- upi(data = int_df,
                      model = upi_mod1)
summary(upi_mod1_fit,
        fit.measure = TRUE,
        standardized = TRUE)
parameterEstimates(upi_mod1_fit)

lavPredict(upi_mod1_fit)

# Plot interaction
upi_mod1_data <- as.data.frame(lavPredict(upi_mod1_fit))
esteem_low <- round(mean(upi_mod1_data$Esteem) - sd(rapi_mod1_data$Esteem), 3)
esteem <- round(mean(upi_mod1_data$Esteem), 3)
esteem_high <- round(mean(upi_mod1_data$Esteem) + sd(upi_mod1_data$Esteem), 3)

upi_mod1_list <- list(Discrimination = seq(-2, 4, by = 0.4), Esteem = c(esteem_low, esteem, esteem_high))
upi_mod1 <- lm(Depression ~ Discrimination*Esteem, upi_mod1_data)
upimod1dat <- emmip(upi_mod1, Esteem ~ Discrimination, at = upi_mod1_list, CIs = TRUE, plotit = FALSE)
upimod1dat$fEsteem <- factor(as.character(upimod1dat$Esteem))
levels(upimod1dat$fEsteem) <- c("low","med","high")

upi_mod1_plot <- ggplot(upimod1dat, aes(x = Discrimination, y = yvar, color = fEsteem)) +
                    geom_line() +
                    ylab("Depression") +
                    xlab("PED") +
                    labs(colour = "Self-esteem") +
                    theme_classic()

# Put three plots together

par(mfrow=c(1,3))
tspa_mod1_plot
rapi_mod1_plot
upi_mod1_plot

figure_depression <- ggarrange(tspa_mod1_plot, upi_mod1_plot, rapi_mod1_plot,
                    labels = c("2S-PA", "UPI", "RAPI"),
                    common.legend = T,
                    legend = "bottom",
                    font.label = list(size = 10),
                    vjust = 0.2,
                    hjust = -0.4) + coord_fixed()

# Model 2: General Anxiety Disorder is predicted by Self-esteem, discrimination, and their latent interaction

tspa_mod2_fit <- tspa(model = "GAD ~ Discrimination + Esteem + Esteem:Discrimination",
                    data = fs_int_df,
                    se = list(Discrimination = fs_int_df$fs_Discrimination_se[1],
                              GAD = fs_int_df$fs_GAD_se[1],
                              Esteem = fs_int_df$fs_Esteem_se[1]))
summary(tspa_mod2_fit, standardized = T, fit.measures = T)
parameterEstimates(tspa_mod2_fit)

# Plot interaction
tspa_mod2_data <- as.data.frame(lavPredict(tspa_mod2_fit))
esteem_low <- round(mean(tspa_mod2_data$Esteem) - sd(tspa_mod2_data$Esteem), 3)
esteem <- round(mean(tspa_mod2_data$Esteem), 3)
esteem_high <- round(mean(tspa_mod2_data$Esteem) + sd(tspa_mod2_data$Esteem), 3)

tspa_mod2_list <- list(Discrimination = seq(-2, 3, by = 0.4), Esteem = c(esteem_low, esteem, esteem_high))
tspa_mod2 <- lm(GAD ~ Discrimination*Esteem, tspa_mod2_data)
tspamod2dat <- emmip(tspa_mod2, Esteem ~ Discrimination, at = tspa_mod2_list, CIs = TRUE, plotit = FALSE)
tspamod2dat$fEsteem <- factor(as.character(tspamod2dat$Esteem))
levels(tspamod2dat$fEsteem) <- c("low","med","high")

tspa_mod2_plot <- ggplot(tspamod2dat, aes(x = Discrimination, y = yvar, color = fEsteem)) +
  geom_line() +
  ylab("GAD") +
  xlab("PED") +
  labs(colour = "Self-esteem") +
  theme_classic()

figure_gad <- ggarrange(tspa_mod2_plot,
                               labels = c("2S-PA"),
                               common.legend = T,
                               legend = "bottom",
                               font.label = list(size = 10),
                               vjust = 0.2,
                               hjust = -0.4) + coord_fixed()

# Model 2: Using rapi

rapi_mod2 <- " GAD =~ TA190124 + TA190125 + TA190126 + TA190127 +
                         TA190128 + TA190129 + TA190130

                 Esteem =~ TA190104 + TA190105 + TA190106 + TA190107 +
                           TA190108 + TA190109 + TA190110 + TA190111 +
                           TA190112 + TA190113

                 Discrimination =~ TA192066 + TA192067 + TA192068 + TA192069 +
                                   TA192070 + TA192071 + TA192072

                 GAD ~ Discrimination + Esteem + Esteem:Discrimination "
rapi_mod2_fit <- rapi(data = int_df,
                      model = rapi_mod2)
summary(rapi_mod2_fit,
        fit.measure = TRUE,
        standardized = TRUE)
parameterEstimates(rapi_mod2_fit)


# Model 2: Using upi

upi_mod2 <- " GAD =~ TA190124 + TA190125 + TA190126 + TA190127 +
                         TA190128 + TA190129 + TA190130

                 Esteem =~ TA190104 + TA190105 + TA190106 + TA190107 +
                           TA190108 + TA190109 + TA190110 + TA190111 +
                           TA190112 + TA190113

                 Discrimination =~ TA192066 + TA192067 + TA192068 + TA192069 +
                                   TA192070 + TA192071 + TA192072

                 GAD ~ Discrimination + Esteem + Esteem:Discrimination "
upi_mod2_fit <- upi(data = int_df,
                    model = upi_mod2)
summary(upi_mod2_fit,
        fit.measure = TRUE,
        standardized = TRUE)
parameterEstimates(upi_mod2_fit)

# Model 3: Social Anxiety is predicted by Self-esteem, discrimination, and their latent interaction

tspa_mod3_fit <- tspa(model = "Soc_Anx ~ Discrimination + Esteem + Esteem:Discrimination",
                      data = fs_int_df,
                      se = list(Discrimination = fs_int_df$fs_Discrimination_se[1],
                                Soc_Anx = fs_int_df$fs_Soc_Anx_se[1],
                                Esteem = fs_int_df$fs_Esteem_se[1]))
summary(tspa_mod3_fit, standardized = T, fit.measures = T)
parameterEstimates(tspa_mod3_fit)

# Model 3: Using rapi

rapi_mod3 <- " Soc_Anx =~ TA190100 + TA190101 + TA190102 + TA190103

                 Esteem =~ TA190104 + TA190105 + TA190106 + TA190107 +
                           TA190108 + TA190109 + TA190110 + TA190111 +
                           TA190112 + TA190113

                 Discrimination =~ TA192066 + TA192067 + TA192068 + TA192069 +
                                   TA192070 + TA192071 + TA192072

                 Soc_Anx ~ Discrimination + Esteem + Esteem:Discrimination "
rapi_mod3_fit <- rapi(data = int_df,
                      model = rapi_mod3)
summary(rapi_mod3_fit,
        fit.measure = TRUE,
        standardized = TRUE)
parameterEstimates(rapi_mod3_fit)


# Model 3: Using upi

upi_mod3 <- " Soc_Anx =~ TA190100 + TA190101 + TA190102 + TA190103

                 Esteem =~ TA190104 + TA190105 + TA190106 + TA190107 +
                           TA190108 + TA190109 + TA190110 + TA190111 +
                           TA190112 + TA190113

                 Discrimination =~ TA192066 + TA192067 + TA192068 + TA192069 +
                                   TA192070 + TA192071 + TA192072

                 Soc_Anx ~ Discrimination + Esteem + Esteem:Discrimination "
upi_mod3_fit <- upi(data = int_df,
                    model = upi_mod3)
summary(upi_mod3_fit,
        fit.measure = TRUE,
        standardized = TRUE)
parameterEstimates(upi_mod3_fit)

# Model 4: Psychological distress is predicted by Self-esteem, discrimination, and their latent interaction

tspa_mod4_fit <- tspa(model = "Distress ~ Discrimination + Esteem + Esteem:Discrimination",
                      data = fs_int_df,
                      se = list(Discrimination = fs_int_df$fs_Discrimination_se[1],
                                Distress = fs_int_df$fs_Distress_se[1],
                                Esteem = fs_int_df$fs_Esteem_se[1]))
summary(tspa_mod4_fit, standardized = T, fit.measures = T)
parameterEstimates(tspa_mod4_fit)

# Plot interaction
tspa_mod4_data <- as.data.frame(lavPredict(tspa_mod4_fit))
esteem_low <- round(mean(tspa_mod4_data$Esteem) - sd(tspa_mod4_data$Esteem), 3)
esteem <- round(mean(tspa_mod4_data$Esteem), 3)
esteem_high <- round(mean(tspa_mod4_data$Esteem) + sd(tspa_mod4_data$Esteem), 3)

tspa_mod4_list <- list(Discrimination = seq(-2, 3, by = 0.4), Esteem = c(esteem_low, esteem, esteem_high))
tspa_mod4 <- lm(Distress ~ Discrimination*Esteem, tspa_mod4_data)
tspamod4dat <- emmip(tspa_mod4, Esteem ~ Discrimination, at = tspa_mod4_list, CIs = TRUE, plotit = FALSE)
tspamod4dat$fEsteem <- factor(as.character(tspamod4dat$Esteem))
levels(tspamod4dat$fEsteem) <- c("low","med","high")

tspa_mod4_plot <- ggplot(tspamod4dat, aes(x = Discrimination, y = yvar, color = fEsteem)) +
  geom_line() +
  ylab("Distress") +
  xlab("PED") +
  ylim(-1, 1.5) +
  labs(colour = "Self-esteem") +
  theme_classic()

# Model 4: Using rapi

rapi_mod4 <- " Distress =~ TA191961 + TA191962 + TA191963 +
                          TA191964 + TA191965 + TA191966

                 Esteem =~ TA190104 + TA190105 + TA190106 + TA190107 +
                           TA190108 + TA190109 + TA190110 + TA190111 +
                           TA190112 + TA190113

                 Discrimination =~ TA192066 + TA192067 + TA192068 + TA192069 +
                                   TA192070 + TA192071 + TA192072

                 Distress ~ Discrimination + Esteem + Esteem:Discrimination "
rapi_mod4_fit <- rapi(data = int_df,
                      model = rapi_mod4)
summary(rapi_mod4_fit,
        fit.measure = TRUE,
        standardized = TRUE)
parameterEstimates(rapi_mod4_fit)

# Plot interaction
rapi_mod4_data <- as.data.frame(lavPredict(rapi_mod4_fit))
esteem_low <- round(mean(rapi_mod4_data$Esteem) - sd(rapi_mod4_data$Esteem), 3)
esteem <- round(mean(rapi_mod4_data$Esteem), 3)
esteem_high <- round(mean(rapi_mod4_data$Esteem) + sd(rapi_mod4_data$Esteem), 3)

rapi_mod4_list <- list(Discrimination = seq(-2, 3, by = 0.4), Esteem = c(esteem_low, esteem, esteem_high))
rapi_mod4 <- lm(Distress ~ Discrimination*Esteem, rapi_mod4_data)
rapimod4dat <- emmip(rapi_mod4, Esteem ~ Discrimination, at = rapi_mod4_list, CIs = TRUE, plotit = FALSE)
rapimod4dat$fEsteem <- factor(as.character(rapimod4dat$Esteem))
levels(rapimod4dat$fEsteem) <- c("low","med","high")

rapi_mod4_plot <- ggplot(rapimod4dat, aes(x = Discrimination, y = yvar, color = fEsteem)) +
  geom_line() +
  ylab("Distress") +
  xlab("PED") +
  ylim(-1, 1.5) +
  labs(colour = "Self-esteem") +
  theme_classic()


figure_distress <- ggarrange(tspa_mod4_plot, rapi_mod4_plot,
                               labels = c("2S-PA", "RAPI"),
                               common.legend = T,
                               legend = "bottom",
                               font.label = list(size = 10),
                               vjust = 0.2,
                               hjust = -0.4) +
                   coord_fixed(ratio = 0.5)


# Model 4: Using upi

upi_mod4 <- " Distress =~ TA191961 + TA191962 + TA191963 +
                          TA191964 + TA191965 + TA191966

                 Esteem =~ TA190104 + TA190105 + TA190106 + TA190107 +
                           TA190108 + TA190109 + TA190110 + TA190111 +
                           TA190112 + TA190113

                 Discrimination =~ TA192066 + TA192067 + TA192068 + TA192069 +
                                   TA192070 + TA192071 + TA192072

                 Distress ~ Discrimination + Esteem + Esteem:Discrimination "
upi_mod4_fit <- upi(data = int_df,
                    model = upi_mod4)
summary(upi_mod4_fit,
        fit.measure = TRUE,
        standardized = TRUE)
parameterEstimates(upi_mod4_fit)

# Model 5: Depression as a mediator, affecting emotional well-being

tspa_med1_fit <- tspa(model = "Depression ~ Discrimination + Esteem + Esteem:Discrimination
                             Emo ~ Depression",
                    data = fs_int_df,
                    se = list(Esteem = fs_int_df$fs_Esteem_se[1],
                              Depression = fs_int_df$fs_Depression_se[1],
                              Discrimination = fs_int_df$fs_Discrimination_se[1],
                              Emo = fs_int_df$fs_Emo_se[1]))
summary(tspa_med1_fit, standardized = T, fit.measures = T)

# Model 6: Depression as a mediator, affecting social well-being

tspa_med2_fit <- tspa(model = "Depression ~ Discrimination + Esteem + Esteem:Discrimination
                             Social ~ Depression",
                      data = fs_int_df,
                      se = list(Esteem = fs_int_df$fs_Esteem_se[1],
                                Depression = fs_int_df$fs_Depression_se[1],
                                Discrimination = fs_int_df$fs_Discrimination_se[1],
                                Social = fs_int_df$fs_Social_se[1]))
summary(tspa_med2_fit, standardized = T, fit.measures = T)

# Model 7: Depression as a mediator, affecting psychological well-being

tspa_med3_fit <- tspa(model = "Depression ~ Discrimination + Esteem + Esteem:Discrimination
                              Psychological ~ Depression",
                      data = fs_int_df,
                      se = list(Esteem = fs_int_df$fs_Esteem_se[1],
                                Depression = fs_int_df$fs_Depression_se[1],
                                Discrimination = fs_int_df$fs_Discrimination_se[1],
                                Psychological = fs_int_df$fs_Psychological_se[1]))
summary(tspa_med3_fit, standardized = T, fit.measures = T)

