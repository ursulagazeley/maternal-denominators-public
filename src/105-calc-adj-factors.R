################################################################################
#' @description Calculate adjustment factors from reproductive calendars and fph
#' @return Spreadsheet with combined adjustment factors
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
source("./src/000-functions.R")
library(tidyverse)
## Pregnancy outcome proportions
pregout_repcal <- read.csv("./gen/pregout-repcal-byage.csv")
pregout_fph <- read.csv("./gen/pregout-fph-byage.csv")
################################################################################

# Reshape pregnancy outcome files wide

repcalWide <- pregout_repcal %>%
  select(SurveyId, agegrp, var, perwt, n) %>%
  filter(!(agegrp %in% c("<15", ">50"))) %>%
  pivot_wider(
    id_cols = c(SurveyId, agegrp),
    names_from = var,
    values_from =  c(perwt, n), 
    values_fill = 0
  ) %>%
  arrange(SurveyId)

fphWide <- pregout_fph %>%
  mutate(var = paste0(var, "_fph", sep = "")) %>%
  select(SurveyId, agegrp, var, perwt, n) %>%
  filter(!(agegrp %in% c("<15", ">50"))) %>%
  pivot_wider(
    id_cols = c(SurveyId, agegrp),
    names_from = var,
    values_from = c(perwt, n), 
    values_fill = 0
  ) %>%
  arrange(SurveyId) %>%
  mutate(TRM_fph = perwt_AB_fph + perwt_MSC_fph,
         n_TRM_fph = n_AB_fph + n_MSC_fph)

# Combine pregnancy outcome proportions from reproductive calendars and fph
pregout <- merge(repcalWide, fphWide, by = c("SurveyId","agegrp"), suffixes = c("","_fph"), all = TRUE)
pregout <- pregout[order(pregout$SurveyId, pregout$agegrp),]
# Remove perwt suffix
names(pregout) <- sub("perwt_", "", names(pregout))

# Calculate adjustment factors

pregout$noAdj <- 1

# Adjustment from reproductive calendar
pregout$adjSB <- (pregout$LB + pregout$SB)/pregout$LB
pregout$adjTRM <- (pregout$LB + pregout$TRM)/pregout$LB
pregout$adjAll <- (pregout$LB + pregout$SB + pregout$TRM)/pregout$LB

# Adjustment from fph
pregout$adjSB_fph <- (pregout$LB_fph + pregout$SB_fph)/pregout$LB_fph
pregout$adjTRM_fph <- (pregout$LB_fph + pregout$TRM_fph)/pregout$LB_fph
pregout$adjAll_fph <- (pregout$LB_fph + pregout$SB_fph + pregout$AB_fph + pregout$MSC_fph)/pregout$LB_fph
pregout$adjAB_fph <- (pregout$LB_fph + pregout$AB_fph)/pregout$LB_fph
pregout$adjMSC_fph <- (pregout$LB_fph + pregout$MSC_fph)/pregout$LB_fph
pregout$adjMSCSB_fph <- (pregout$LB_fph + pregout$SB_fph + pregout$MSC_fph)/pregout$LB_fph

# Order columns
pregout <- pregout %>%
  select(SurveyId, agegrp, LB, SB, TRM, 
         n_LB, n_SB, n_TRM, 
         LB_fph, SB_fph, MSC_fph, AB_fph, TRM_fph,
         n_LB_fph, n_SB_fph, n_MSC_fph, n_AB_fph, n_TRM_fph,
         noAdj,
         everything()) 

# Save output(s) ----------------------------------------------------------

write.csv(pregout, "./gen/adjustment-factors.csv", row.names = FALSE)
