################################################################################
#' @description Check reporting of variables in individual, birth, and pregnancy recode modules for each survey
#' @return Data frame with survey metadata regarding whether variables required for mortality calculations are present
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(data.table)
library(haven)
library(tidyverse)
#' Inputs
## BR modules
l_br <- readRDS("./data/br.rds")
## IR modules
l_ir <- readRDS("./data/ir.rds")
## GR modules
l_gr <- readRDS("./data/gr.rds")
################################################################################

# IR ----------------------------------------------------------------------

# Get DHS version
l_phase <- lapply(l_ir, function(x) substr(as.character(x$v000[1]),3,3))
df_phase <- plyr::ldply(l_phase, .id = "SurveyId")
df_phase$V1[df_phase$V1==""] <- 1
df_phase$V1[is.na(df_phase$V1)] <- 1
df_phase$V1[df_phase$SurveyId == "VN1997DHS"] <- 3
df_phase$V1[df_phase$SurveyId == "VN2002DHS"] <- 4
df_phase$phase <- df_phase$V1
df_phase$V1 <- NULL

# Check if dataset has vcal_1 (reproductive calendar)
l_vcal_1 <- lapply(l_ir, function(x) "vcal_1" %in% names(x))
df_vcal_1 <- plyr::ldply(l_vcal_1, .id = "SurveyId")
df_vcal_1$vcal_1 <- df_vcal_1$V1
df_vcal_1$V1 <- NULL
# Check that vcal_1 isn't blank
l_vcal_1b <- lapply(l_ir[subset(df_vcal_1, vcal_1 == TRUE)$SurveyId], function(x){ x$vcal_1[grepl("^\\s*$", x$vcal_1)] <- NA ; return(x) })
l_vcal_1b <- lapply(l_vcal_1b, function(x) nrow(subset(x, !is.na(vcal_1))) != 0)
df_vcal_1b <- plyr::ldply(l_vcal_1b, .id = "SurveyId")
df_vcal_1b$vcal_1b <- df_vcal_1b$V1
df_vcal_1b$V1 <- NULL

# v020 ever-married sample indicator
# for all woman samples it is code 0, for ever married samples it is code 1
l_v020 <- lapply(l_ir, function(x) as.character(x$v020)[1])
df_v020 <- plyr::ldply(l_v020, .id = "SurveyId")
df_v020$EverMW <- df_v020$V1
df_v020$V1 <- NULL
df_v020$EverMW[df_v020$EverMW == 1] <- TRUE
df_v020$EverMW[df_v020$EverMW == 0] <- FALSE
df_v020$EverMW[is.na(df_v020$EverMW) & df_v020$SurveyId == "EG1988DHS"] <- TRUE
df_v020$EverMW[is.na(df_v020$EverMW) & df_v020$SurveyId == "ID1987DHS"] <- TRUE
df_v020$EverMW[is.na(df_v020$EverMW) & df_v020$SurveyId == "LK1987DHS"] <- TRUE
df_v020$EverMW[is.na(df_v020$EverMW) & df_v020$SurveyId == "SD1990DHS"] <- TRUE
df_v020$EverMW[is.na(df_v020$EverMW) & df_v020$SurveyId == "TN1988DHS"] <- TRUE
df_v020$EverMW[is.na(df_v020$EverMW)] <- FALSE

# mm2 - whether sibling is dead or alive
# TRUE if mm2 included
l_mm2 <- lapply(l_ir, function(x) "mm2_01" %in% names(x))
df_mm2 <- plyr::ldply(l_mm2, .id = "SurveyId")
df_mm2$mm2 <- df_mm2$V1
df_mm2$V1 <- NULL

# mm8 - sibling date of death in CMC
# TRUE if mm8 is not all NA values
l_mm8 <- lapply(l_ir, function(x) any(!is.na(x$mm8_01)))
df_mm8 <- plyr::ldply(l_mm8, .id = "SurveyId")
df_mm8$mm8 <- df_mm8$V1
df_mm8$V1 <- NULL

# mm9 - Indicates if the respondent's sister was pregnant when she died, if she died during
# childbirth, within six weeks after the delivery, within 2 months after the delivery
# In some countries the question is only asked for ever married siblings (MM5 = 1)
# TRUE if mm9 is not all NA values
l_mm9 <- lapply(l_ir, function(x) any(!is.na(x$mm9_01)))
df_mm9 <- plyr::ldply(l_mm9, .id = "SurveyId")
df_mm9$mm9 <- df_mm9$V1
df_mm9$V1 <- NULL

# mm16 - death due to violence or an accident
# TRUE if mm16 included
l_mm16 <- lapply(l_ir, function(x) "mm16_01" %in% names(x))
df_mm16 <- plyr::ldply(l_mm16, .id = "SurveyId")
df_mm16$mm16 <- df_mm16$V1
df_mm16$V1 <- NULL
df_mm16$mm16[df_mm16$SurveyId == "PE2010DHS"] <- FALSE # mm16 is place of death in these surveys
df_mm16$mm16[df_mm16$SurveyId == "PE2011DHS"] <- FALSE # Place of death

df_ir_var <- merge(df_phase, df_vcal_1, by = c("SurveyId"), all =TRUE)
df_ir_var <- merge(df_ir_var, df_vcal_1b, by = c("SurveyId"), all =TRUE)
df_ir_var <- merge(df_ir_var, df_v020, by = c("SurveyId"), all =TRUE)
df_ir_var <- merge(df_ir_var, df_mm2, by = c("SurveyId"), all =TRUE)
df_ir_var <- merge(df_ir_var, df_mm8, by = c("SurveyId"), all =TRUE)
df_ir_var <- merge(df_ir_var, df_mm9, by = c("SurveyId"), all =TRUE)
df_ir_var <- merge(df_ir_var, df_mm16, by = c("SurveyId"), all =TRUE)
df_ir_var$SurveyId <- as.character(df_ir_var$SurveyId)

# BR ----------------------------------------------------------------------

# Check if dataset has v229 (FBH+ questions related to pregnancy terminations)
l_v229 <- lapply(l_br, function(x) "v229" %in% names(x))
df_v229 <- plyr::ldply(l_v229, .id = "SurveyId")
df_v229 <- subset(df_v229, V1 == TRUE)
df_v229$v229 <- df_v229$V1
df_v229$V1 <- NULL

df_br_var <- df_v229
df_br_var$SurveyId <- as.character(df_br_var$SurveyId)

# GR ----------------------------------------------------------------------

# Check if dataset has p32 (FPH pregnancy outcome question)
l_p32 <- lapply(l_gr, function(x) "p32" %in% names(x))
df_p32 <- plyr::ldply(l_p32, .id = "SurveyId")
df_p32 <- subset(df_p32, V1 == TRUE)
df_p32$p32 <- df_p32$V1
df_p32$V1 <- NULL

df_gr_var <- df_p32
df_gr_var$SurveyId <- as.character(df_gr_var$SurveyId)

# Combine IR, BR, GR ------------------------------------------------------

df_data <- merge(df_ir_var, df_br_var, by = c("SurveyId"), all =TRUE)
df_data <- merge(df_data, df_gr_var, by = c("SurveyId"), all =TRUE)
df_data$phase[df_data$SurveyId == "TR2018DHS"] <- 7 # This one is not in IR
df_data[is.na(df_data)] <- FALSE

df_data$CountryId <- substr(df_data$SurveyId, 1, 2)
df_data$SurveyYear <- as.numeric(as.character(substr(df_data$SurveyId, 3, 6)))
df_data <- df_data %>% relocate(any_of(c("CountryId","SurveyYear")), .after = SurveyId)
df_data <- df_data[order(as.character(df_data$SurveyId)),]

# Inclusion criteria ------------------------------------------------------

# Has sibling survival indicator and cmc date of death
df_data$SibSurvAvail <- ifelse(df_data$mm2 == TRUE & df_data$mm8 == TRUE, TRUE, FALSE)

# Indicator for inclusion for prmr
# Has reproductive calendar, reproductive calendar is not blank, has sibling survival, 
# Has indicator for if the respondent's sister was pregnant when she died, if she died during childbirth, within six weeks after the delivery, within 2 months after the delivery (mm9)
df_data$IncludePRMR <- ifelse(df_data$vcal_1 == TRUE & df_data$vcal_1b == TRUE & df_data$SibSurvAvail == TRUE &
                                df_data$mm9 == TRUE, TRUE, FALSE)

# Indicator for inclusion for mmr
# Has reproductive calendar, reproductive calendar is not blank, has sibling survival, 
# Has indicator for if the respondent's sister was pregnant when she died, if she died during childbirth, within six weeks after the delivery, within 2 months after the delivery (mm9)
# Has indicator for whether death was accidental *mm16)
df_data$IncludeMMR <- ifelse(df_data$vcal_1 == TRUE & df_data$vcal_1b == TRUE & df_data$SibSurvAvail == TRUE &
                               df_data$mm9 == TRUE & df_data$mm16 == TRUE, TRUE, FALSE)


# Include for either MMR or PRMR
df_data$Include <- ifelse(df_data$IncludeMMR == TRUE | df_data$IncludePRMR == TRUE, TRUE, FALSE)

# Sample
df_sample <- subset(df_data, Include == TRUE)

# Save output(s) ----------------------------------------------------------

write.csv(df_data, "./gen/surveys-all.csv", row.names = FALSE)
write.csv(df_sample, "./gen/surveys-sample.csv", row.names = FALSE)


