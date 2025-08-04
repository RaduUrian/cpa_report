#title: MA report report

# packages ----------------------------------------------------------------

# load required packages
pacman::p_load(tidyverse, pacman, dplyr, ggplot2, ggthemes, 
               ggvis, httr, lavaan, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr, psych,lme4, effsize, ggeffects, effects, reshape2, scales,
               rmarkdown, unheadr, viridis, ggThemeAssist, sjPlot, car, ggstatsplot, visreg, paletteer,
               corrplot, performance, here, ggpubr, ggsci)

setwd("/Users/selestebeaulieu/Documents/Academic/CSCD/projects/ora")

# files and wrangling -----------------------------------------------------

# read in the the data
ora_child_raw <- read_csv("ORA_CSCD_Template_T1_raw.csv",
                          na = c("NA", "na", " ", "", "."))

ora_child_agg <- read_csv("ORA_CSCD_Template_T1_agg.csv", 
                          na = c("NA", "na", " ", "", "."))

ora_parent_raw <- read_csv("ORA_CSCD_Template_T1_pq.csv", 
                           na = c("NA", "na", "", " ", "."))

#selecting variables I want
ora_child_raw <- ora_child_raw %>% 
  select(c(ID_child, Gender, age_years, age_months, Pain_PA, Helping_Cue_PSB_only, Sharing_Cue_PSB_only,
           Comforting_Cue_PSB_only, drop_DG, drop_AHT, drop_PainSim, drop_SimpPSB_he)) 

ora_child_agg <- ora_child_agg %>% 
  select(c(ID_child, DG_cos_pr, AHT_noEscape))

ora_parent_raw <- ora_parent_raw %>% 
  select(c(ID_child, SES_ethnic_ch, SES_whoanswers, SES_multilangual, SES_siblings1, SES_net_income_formatted))

## joining datasets & renaming
ma_data <- full_join(ora_child_raw, ora_child_agg, by = "ID_child")
ma_data <-full_join(ma_data, ora_parent_raw, by = "ID_child")

ma_data <- ma_data %>%
  rename(
    pain_sim = Pain_PA,
    mdg = DG_cos_pr, 
    aht = AHT_noEscape,
    blocked_door = Helping_Cue_PSB_only, 
    sticker_sharing = Sharing_Cue_PSB_only, 
    broken_teddy = Comforting_Cue_PSB_only
  )

# mutating data --------------------------------------------------------
ma_data <- ma_data %>%
  mutate(Gender = case_when(Gender == "girl" ~ 2, Gender == "boy" ~ 1))%>%
         mutate(Gender = as.numeric(Gender))

ma_data <- ma_data %>%
  mutate(aht = ifelse(drop_AHT == "1" | drop_AHT == "2", NA, aht))

ma_data <- ma_data %>%
  mutate(mdg = ifelse(drop_DG == "1" | drop_DG == "2", NA, mdg))

ma_data <- ma_data %>%
  mutate(pain_sim = ifelse(drop_PainSim == "1" | drop_PainSim == "2", NA, pain_sim))

ma_data <- ma_data %>%
  mutate(blocked_door = ifelse(drop_SimpPSB_he == "1" | drop_SimpPSB_he == "2", NA, blocked_door))

ma_data <- ma_data %>%
  mutate(SES_ethnic_ch = case_when(
    SES_ethnic_ch == "1" ~ "White", 
    SES_ethnic_ch == "2" ~ "Black", 
    SES_ethnic_ch == "3" ~ "East Asian",
    SES_ethnic_ch == "4" ~ "South Asian",
    SES_ethnic_ch == "5" ~ "Southeast Asian", 
    SES_ethnic_ch == "6" ~ "West Asian",
    SES_ethnic_ch == "7" ~ "Latin American",
    SES_ethnic_ch == "8" ~ "Australian",
    SES_ethnic_ch == "9" ~ "Indigenous", 
    SES_ethnic_ch == "10"~ "Prefer not to respond",
    SES_ethnic_ch == "11" ~ "Not listed",
    SES_ethnic_ch > "11" ~ "Multi-ethnic"))

# prosocial behavior (4pt coding scheme)
ma_data <- ma_data %>%
 mutate(blocked_door = case_when(
   blocked_door == "0" ~ "0", 
   blocked_door == "1" | blocked_door == "2" ~ "1", 
   blocked_door == "3" | blocked_door == "4" | blocked_door == "5" ~ "2", 
   blocked_door == "6" | blocked_door == "7" ~ "3"))%>% 
  mutate(blocked_door = as.numeric(blocked_door))

#ma_data$blocked_door <- factor(ma_data$blocked_door,
 #                              levels = c("3", "2", "1", "0"),
  #                             labels = c("Spontaneous", "Cued", "Compliant", "No help"))

ma_data <- ma_data %>%
  mutate(sticker_sharing = case_when(
    sticker_sharing == "0" ~ "0", 
    sticker_sharing == "1" | sticker_sharing == "2" ~ "1", 
    sticker_sharing == "3" | sticker_sharing == "4" | sticker_sharing == "5" ~ "2", 
    sticker_sharing == "6" | sticker_sharing == "7" ~ "3"))%>% 
  mutate(sticker_sharing = as.numeric(sticker_sharing))

#ma_data$sticker_sharing <- factor(ma_data$sticker_sharing,
#                             levels = c("3", "2", "1", "0"),
 #                            labels = c("Spontaneous", "Cued", "Compliant", "No sharing"))

ma_data <- ma_data %>%
  mutate(broken_teddy = case_when(
    broken_teddy == "0" ~ "0", 
    broken_teddy == "1" | broken_teddy == "2" ~ "1", 
    broken_teddy == "3" | broken_teddy == "4" | broken_teddy == "5" ~ "2", 
    broken_teddy == "6" | broken_teddy == "7" ~ "3"))%>% 
  mutate(broken_teddy = as.numeric(broken_teddy))

#ma_data$broken_teddy <- factor(ma_data$broken_teddy,
 #                                  levels = c("3", "2", "1", "0"),
  #                                 labels = c("Spontaneous", "Cued", "Compliant", "No comforting"))

#descriptives ------------------------------------------------------------

#descriptives for overall sample
describe(ma_data)
summary(ma_data)
hist(ma_data$pain_sim, col = "lightblue", main = "pain sim", xlab = "score")

#prosocial task descriptives by age
ma_data_ages4 <- ma_data %>%
  mutate(age_months = case_when(
    age_months < 54 ~ "4", 
    age_months >= 54 & age_months < 66 ~ "5", 
    age_months >= 66 & age_months < 78 ~ "6", 
    age_months >= 78 & age_months < 90 ~ "7")) %>%
  mutate(age_months = as.numeric(age_months))

ma_data_four <- ma_data_ages4 %>% 
  filter(age_months == "4")
ma_data_five <- ma_data_ages4 %>% 
  filter(age_months == "5")
ma_data_six <- ma_data_ages4 %>% 
  filter(age_months == "6")
ma_data_seven <- ma_data_ages4 %>% 
  filter(age_months == "7")

describe(ma_data_four)
describe(ma_data_five)
describe(ma_data_six)
describe(ma_data_seven)

# correlations -----------------------------------------------------------

#demographics correlations & task correlations in entire sample
correlations_ma_data <- ma_data %>% 
  select(age_months, Gender, blocked_door, sticker_sharing, broken_teddy, aht, mdg, pain_sim, SES_siblings1, SES_net_income_formatted) %>%
  na.omit() 

cor_p <- cor.mtest(correlations_ma_data, method = "spearman")
cor <- cor(correlations_ma_data,
           method = "spearman")

correlations <- corrplot(cor, p.mat = cor_p$p, insig = "blank", addCoef.col = "black", diag = FALSE, type = "lower")

#task correlations in young sample
ma_data_ages2 <- ma_data %>%
  mutate(age_months = case_when(
    age_months < 42 & age_months < 66 ~ "1", 
    age_months >= 66 & age_months < 90 ~ "2")) %>%
  mutate(age_months = as.numeric(age_months))

ma_data_young <- ma_data_ages2 %>% 
  filter(age_months == "1")
ma_data_old <- ma_data_ages2 %>% 
  filter(age_months == "2")

correlations_ma_data_young <- ma_data_young %>% 
  select(blocked_door, sticker_sharing, broken_teddy, aht, mdg, pain_sim) %>%
  na.omit() 

cor_p <- cor.mtest(correlations_ma_data_young, method = "spearman")
cor <- cor(correlations_ma_data_young,
           method = "spearman")

correlations_ma_data_young <- corrplot(cor, insig = "blank", addCoef.col = "black", diag = FALSE, type = "lower")

#task correlations in older sample
correlations_ma_data_old <- ma_data_old %>% 
  select(blocked_door, sticker_sharing, broken_teddy, aht, mdg, pain_sim) %>%
  na.omit() 

cor_p <- cor.mtest(correlations_ma_data_old, method = "spearman")
cor <- cor(correlations_ma_data_old,
           method = "spearman")

correlations_ma_data_old <- corrplot(cor, insig = "blank", addCoef.col = "black", diag = FALSE, type = "lower")


# SEM --------------------------------------------------------------------

#FIML for missing data (how does R handle missing data vs. excluded data?)
#missing = "ML"

#define the SEM model -----

#model with 1 latent factor
#latent variable: PSB
#observed variables: blocked_door, sticker_sharing, broken_teddy, aht, mdg, pain_sim

#model with 3 latent factors
#latent variables: helping, sharing, and comforting
#observed variables: blocked_door & aht, sticker_sharing & mdg, broken_teddy & pain_sim

#model with 2 latent factors
#latent variables: costly, instru-affective
#observed variables: aht & mdg & sticker_sharing, blocked_door & broken_teddy & pain_sim

#sem_model <- '
  # Define latent variables
    C =~ aht + mdg + sticker_sharing
    IA =~ blocked_door + broken_teddy + pain_sim
    
  # Define regression paths
    C ~ IA

  # Covariances (if needed)
    aht ~ mdg ~ sticker_sharing
    blocked_door ~ broken_teddy ~ pain_sim

#estimate the model -----

#model with 2 latent factors
  #fit <- sem(sem_model, data = ma_data, missing = "ML")
    
#assess model fit -----

#model with 2 latent factors
  #summary(fit, fit.measures = TRUE, standardized = TRUE)

#interpret model results -----
    
#model with 2 latent factors
  #summary(fit, standardized = TRUE, fit.measures = TRUE)
    
#visualize the SEM ----
    
  #install.packages("semPlot")
  #library(semPlot)
    
  #semPaths(fit, what="std", layout="tree", edge.label.cex=1.2, curvePivot=TRUE)
    
#to check modification indices
    #modindices(fit)