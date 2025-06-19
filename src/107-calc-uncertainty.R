################################################################################
#' @description Calculate uncertainty for unadjusted MMR and PRMR
#' @return List of data frames with results
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
source("./src/000-functions.R")
library(haven)
library(tidyverse)
#' Inputs
l_ir <- readRDS("./data/ir.rds")
## Analytical sample
sample <- read.csv("./gen/surveys-sample.csv")
################################################################################

# Maternal Mortality Ratio (MMR) ------------------------------------------

v_samp <- subset(sample, IncludeMMR == TRUE)$SurveyId

# Limit to surveys that meet sample inclusion criteria
l_sample <- l_ir[names(l_ir) %in% v_samp]
l_sample <- l_sample[sort(names(l_sample))]
df_evermw <- subset(sample, SurveyId %in% v_samp)
df_evermw <- df_evermw[order(df_evermw$SurveyId),]
l_evermw <- as.list(df_evermw$EverMW)
l_evermw <- lapply(l_evermw, function(x) ifelse(x == TRUE, "Yes", x))
l_evermw <- lapply(l_evermw, function(x){if (x=="Yes") return(x) else return(NULL)})
names(l_evermw) <- df_evermw$SurveyId

# Combine data and evermw info
l_dat <- list(l_sample, l_evermw)

# Lists for storing final results
l_res <- list()
l_MMR <- list()
l_AdjInfoMMR <- list()

for(i in 1:length(l_sample)){
  
  print(i)
  # Set survey
  dat <- l_dat[[1]][[i]]
  # Set ever married indicator
  v_evermw <- l_dat[[2]][[i]]
  
  # Calculate mmr
  l_res[[i]] <- my_admort(dat, Indicator="mmr", EverMW = v_evermw, CL = 95, JK = "Yes")
  # Store results in two lists
  l_AdjInfoMMR[[i]] <- lapply(l_res[[i]], function(x) x[[1]])
  l_MMR[[i]] <- lapply(l_res[[i]], function(x) x[[2]])
  
}
names(l_AdjInfoMMR) <- names(l_sample)
names(l_MMR) <- names(l_sample)

df_MMR <- plyr::ldply(l_MMR, .id = "SurveyId")
df_MMR$SurveyId <- as.character(df_MMR$SurveyId)
df_MMR <- df_MMR[order(df_MMR$SurveyId),]

df_AdjInfoMMR <- plyr::ldply(l_AdjInfoMMR, .id = "SurveyId")
df_AdjInfoMMR$SurveyId <- as.character(df_AdjInfoMMR$SurveyId)
df_AdjInfoMMR <- df_AdjInfoMMR[order(df_AdjInfoMMR$SurveyId),]

# Pregnancy-Related Mortality Ratio (PRMR) ------------------------------------------

# Limit to only the most recent survey for each country
v_samp <- sample %>%
  filter(IncludePRMR == TRUE) %>%
  group_by(CountryId) %>%
  filter(SurveyYear == max(SurveyYear)) %>%
  pull(SurveyId)

# Limit to surveys that meet sample inclusion criteria
l_sample <- l_ir[names(l_ir) %in% v_samp]
l_sample <- l_sample[sort(names(l_sample))]
df_evermw <- subset(sample, SurveyId %in% v_samp)
df_evermw <- df_evermw[order(df_evermw$SurveyId),]
l_evermw <- as.list(df_evermw$EverMW)
l_evermw <- lapply(l_evermw, function(x) ifelse(x == TRUE, "Yes", x))
l_evermw <- lapply(l_evermw, function(x){if (x=="Yes") return(x) else return(NULL)})
names(l_evermw) <- df_evermw$SurveyId

# Combine data and evermw info
l_dat <- list(l_sample, l_evermw)

# Lists for storing final results
l_res <- list()
l_PRMR <- list()
l_AdjInfoPRMR <- list()

for(i in 1:length(l_sample)){
  
  print(i)
  # Set survey
  dat <- l_dat[[1]][[i]]
  # Set ever married indicator
  v_evermw <- l_dat[[2]][[i]]
  
  # Calculate prmr
  l_res[[i]] <- my_admort(dat, Indicator="prmr", EverMW = v_evermw, CL = 95, JK = "Yes")
  # Store results in two lists
  l_AdjInfoPRMR[[i]] <- l_res[[i]][[1]]
  l_PRMR[[i]] <- l_res[[i]][[2]]
  
}

names(l_AdjInfoPRMR) <- names(l_sample)
names(l_PRMR) <- names(l_sample)

df_PRMR <- plyr::ldply(l_PRMR, .id = "SurveyId")
df_PRMR$SurveyId <- as.character(df_PRMR$SurveyId)
df_PRMR <- df_PRMR[order(df_PRMR$SurveyId),]

df_AdjInfoPRMR <- plyr::ldply(l_AdjInfoPRMR, .id = "SurveyId")
df_AdjInfoPRMR$SurveyId <- as.character(df_AdjInfoPRMR$SurveyId)
df_AdjInfoPRMR <- df_AdjInfoPRMR[order(df_AdjInfoPRMR$SurveyId),]

# Save output(s) ----------------------------------------------------------

l_res <- list(df_MMR, df_AdjInfoMMR, df_PRMR, df_AdjInfoPRMR)
names(l_res) <- c("MMR", "AdjInfoMMR", "PRMR", "AdjInfoPRMR")
saveRDS(l_res, "./gen/mort-rates-uncertainty.rds")
