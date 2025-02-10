################################################################################
#' @description Calculate MMR and PRMR with adjusted denominators
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
## Pregnancy outcome proportions
adj_fac <- read.csv("./gen/adjustment-factors.csv")
################################################################################

# Adjustment factors ------------------------------------------------------

# From reproductive calendar
v_adj_repcal <- c("noAdj", "adjSB", "adjTRM","adjAll")

# From reproductive calendar and FPH
v_adj_all <- c("noAdj", "adjSB", "adjTRM","adjAll", 
               "adjSB_fph", "adjTRM_fph", "adjAll_fph", "adjAB_fph", "adjMSC_fph", "adjMSCSB_fph")

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

# Convert to list
df_adj <- subset(adj_fac, SurveyId %in% v_samp)
l_adj <- split(df_adj, df_adj$SurveyId)
l_adj <- l_adj[sort(names(l_adj))]

# Check that data and adjustment factors are same length
length(l_sample) == length(l_adj)
# Check that surveys are same
all(names(l_sample) == names(l_adj))

# Combine data and adjustment factors
l_dat <- list(l_sample, l_adj, l_evermw)

# Lists for storing final results
l_MMR <- list()
l_AdjInfoMMR <- list()

for(i in 1:length(l_sample)){  
  
  print(i)
  # Set survey
  dat <- l_dat[[1]][[i]]
  # Set adjustment factors
  df_adjfac <- l_dat[[2]][[i]]
  # Set ever married indicator
  v_evermw <- l_dat[[3]][[i]]
  
  # Set whether to do just reproductive calendar adjustments or fph adjustments as well
  if(all(is.na(df_adjfac[,"adjSB_fph"]))){
    v_adj <- v_adj_repcal
  }else{
    v_adj <- v_adj_all
  }
  
  l_res <- list()
  for(j in 1:length(v_adj)){ 
    
    # Vector with age-specific adjustment factors
    v_adjfac <- as.vector(unlist(df_adjfac[,v_adj[j]]), mode='numeric')
    # Calculate mmr with vector j
    newres <- my_admort(dat, Indicator="mmr", EverMW = v_evermw, asfrAdj = v_adjfac ) # CL = 0.95, JK = TRUE
    newres[[1]]$Adj <- v_adj[j]
    newres[[2]]$Adj <- v_adj[j]
    # Save result
    l_res[[j]] <- newres
  }
  # Rbind results for all adjustments
  l_AdjInfoMMR[[i]] <- do.call(rbind, lapply(l_res, function(x) x[[1]]))
  l_MMR[[i]] <- do.call(rbind, lapply(l_res, function(x) x[[2]]))
  
}

names(l_AdjInfoMMR) <- names(l_sample)
names(l_MMR) <- names(l_sample)

df_MMR <- plyr::ldply(l_MMR, .id = "SurveyId")
df_MMR$SurveyId <- as.character(df_MMR$SurveyId)
df_MMR <- df_MMR[order(df_MMR$SurveyId),]

df_AdjInfoMMR <- plyr::ldply(l_AdjInfoMMR, .id = "SurveyId")
df_AdjInfoMMR$SurveyId <- as.character(df_AdjInfoMMR$SurveyId)
df_AdjInfoMMR <- df_AdjInfoMMR[order(df_AdjInfoMMR$SurveyId),]


# Pregnancy-related mortality ratio (PRMR) --------------------------------

v_samp <- subset(sample, IncludePRMR == TRUE)$SurveyId

# Limit to surveys that meet sample inclusion criteria
l_sample <- l_ir[names(l_ir) %in% v_samp]
l_sample <- l_sample[sort(names(l_sample))]
df_evermw <- subset(sample, SurveyId %in% v_samp)
df_evermw <- df_evermw[order(df_evermw$SurveyId),]
l_evermw <- as.list(df_evermw$EverMW)
l_evermw <- lapply(l_evermw, function(x) ifelse(x == TRUE, "Yes", x))
l_evermw <- lapply(l_evermw, function(x){if (x=="Yes") return(x) else return(NULL)})
names(l_evermw) <- df_evermw$SurveyId

# Convert to list
df_adj <- subset(adj_fac, SurveyId %in% v_samp)
l_adj <- split(df_adj, df_adj$SurveyId)
l_adj <- l_adj[sort(names(l_adj))]

# Check that data and adjustment factors are same length
length(l_sample) == length(l_adj)
# Check that surveys are same
all(names(l_sample) == names(l_adj))

# Combine data and adjustment factors
l_dat <- list(l_sample, l_adj, l_evermw)

# Lists for storing final results
l_PRMR <- list()
l_AdjInfoPRMR <- list()

for(i in 1:length(l_sample)){  
  
  print(i)
  # Set survey
  dat <- l_dat[[1]][[i]]
  # Set adjustment factors
  df_adjfac <- l_dat[[2]][[i]]
  # Set ever married indicator
  v_evermw <- l_dat[[3]][[i]]
  
  # Set whether to do just reproductive calendar adjustments or fph adjustments as well
  if(all(is.na(df_adjfac[,"adjSB_fph"]))){
    v_adj <- v_adj_repcal
  }else{
    v_adj <- v_adj_all
  }

  l_res <- list()
  for(j in 1:length(v_adj)){ 
    
    # Vector with age-specific adjustment factors
    v_adjfac <- as.vector(unlist(df_adjfac[,v_adj[j]]), mode='numeric')
    # Calculate mmr with vector j
    newres <- my_admort(dat, Indicator="prmr", EverMW = v_evermw, asfrAdj = v_adjfac ) # CL = 0.95, JK = TRUE
    # Add identifying column for type of adjustment
    newres[[1]]$Adj <- v_adj[j] # Adjustment Info
    newres[[2]]$Adj <- v_adj[j] # MMR
    # Save result
    l_res[[j]] <- newres
  }
  # Rbind results for all adjustments
  l_AdjInfoPRMR[[i]] <- do.call(rbind, lapply(l_res, function(x) x[[1]]))
  l_PRMR[[i]] <- do.call(rbind, lapply(l_res, function(x) x[[2]]))
  
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

saveRDS(l_res, "./gen/mort-rates-adj.rds")
