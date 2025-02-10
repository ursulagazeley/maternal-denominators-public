################################################################################
#' @description Calculate pregnancy outcome proportions in reproductive calendar
#' @return Data frame with pregnancy outcome proportions for all reproductive calendars
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
source("./src/000-functions.R")
library(haven)
library(tidyverse)
#' Inputs
## IR survey modules
l_ir <- readRDS("./data/ir.rds")
## Analytical sample
sample <- read.csv("./gen/surveys-sample.csv")
################################################################################

# Limit to surveys that meet inclusion criteria for MMR and PRMR
v_samp <- subset(sample, Include == TRUE)$SurveyId
l_sample <- l_ir[names(l_ir) %in% v_samp]

# Prepare data
data_prep <- lapply(l_sample, prep_dhs, module = "ir")

# Create categorical variable for pregnancy outcomes
data_var <- lapply(data_prep, function(x){ gen_pregout_cal(x, tips = c(0,4)) })

# Weighted pregnancy outcome proportions
l_prop <- lapply(data_var, calc_wtd_prop)
df_prop <- plyr::ldply(l_prop, .id = "SurveyId")

# Weighted pregnancy outcome proportions by age
l_propage <- lapply(data_var, calc_wtd_prop, byage = TRUE)
df_propage <- plyr::ldply(l_propage, .id = "SurveyId")
df_propage[is.na(df_propage)] <- 0

# Save output(s) ----------------------------------------------------------

write.csv(df_propage, "./gen/pregout-repcal-byage.csv", row.names = FALSE)

