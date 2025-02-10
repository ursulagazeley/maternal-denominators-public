################################################################################
#' @description Download dhs datasets using rdhs
#' @return Separate rds files for individual, birth, and pregnancy recode modules
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(rdhs)
#' Inputs
## Set credentials
set_rdhs_config(email = "NAME@EMAIL",
                password_prompt =  TRUE,
                project = "PROJECTNAME", 
                config_path = "rdhs.json",
                global=FALSE)
## Permissions
Sys.setenv("rdhs_RENVIRON_PERMISSION"=1)
## return API requests as data.table rather than data.frame.
Sys.setenv(rdhs_DATA_TABLE = "TRUE")
################################################################################

# Individual recode -------------------------------------------------------

# Information on available surveys
surveys <- dhs_surveys(surveyYearStart=1980)

# Individual recode metadata
ir_meta <- dhs_datasets(fileType = "IR", fileFormat = "flat")

# Exclude subnational surveys from India because don't have permission to download them via rdhs
ir_meta$subnational <- ifelse(substr(ir_meta$SurveyId,1,2) != substr(ir_meta$FileName,1,2), TRUE, FALSE)
ir_meta <- subset(ir_meta, subnational == FALSE)

# Exclude subnational surveys that conflict with national names
ir_meta <- subset(ir_meta, !(FileName %in% c("KEIR03FL.ZIP", "SNIR02FL.ZIP")))

# Identify modules of interest corresponding to surveys of interest
ird <- ir_meta[which(ir_meta$SurveyId %in% surveys$SurveyId),]

# Limit to surveys which have IR module
surveys_ir <- dhs_surveys(surveyYearStart=1980)
surveys_ir <- subset(surveys_ir, SurveyId %in% ird$SurveyId)

# Load all of the datasets into R as a list
ird$path <- unlist(get_datasets(ird$FileName))
ir <- list()
for(survid in ird$SurveyId){
  print(survid)
  dat <- readRDS(ird[which(ird$SurveyId == survid),]$path)
  dat <- dat[grep("caseid|^v0|^v1|awfactt|^vcal_1|^b|^mm", names(dat))]
  ir[[survid]] <- dat
}
ir <- Map(data.frame,
          SurveyId = surveys_ir$SurveyId,
          CountryName = surveys_ir$CountryName,
          SurveyYear = surveys_ir$SurveyYear,
          ir)
saveRDS(ir, file = "./data/ir.rds")
rm(ir)

# Birth recode ------------------------------------------------------------

# Information on available surveys
surveys <- dhs_surveys(surveyYearStart=1980)

# Individual recode metadata
br_meta <- dhs_datasets(fileType = "BR", fileFormat = "flat")

# Exclude subnational surveys from India because don't have permission to download them via rdhs
br_meta$subnational <- ifelse(substr(br_meta$SurveyId,1,2) != substr(br_meta$FileName,1,2), TRUE, FALSE)
br_meta <- subset(br_meta, subnational == FALSE)

# Exclude subnational surveys that conflict with national names
br_meta <- subset(br_meta, !(FileName %in% c("KEBR42FL.ZIP")))

# Identify modules of interest corresponding to surveys of interest
brd <- br_meta[which(br_meta$SurveyId %in% surveys$SurveyId),]

# Limit to surveys which have BR module
surveys_br <- dhs_surveys(surveyYearStart=1980)
surveys_br <- subset(surveys_br, SurveyId %in% brd$SurveyId)

# Load all of the datasets into R as a list
brd$path <- unlist(get_datasets(brd$FileName))
br <- list()
for(survid in brd$SurveyId){
  print(survid)
  dat <- readRDS(brd[which(brd$SurveyId == survid),]$path)
  dat <- dat[grep("caseid|^v0|^v1|^v2|^b", names(dat))]
  br[[survid]] <- dat
}
br <- Map(data.frame,
          SurveyId = surveys_br$SurveyId,
          CountryName = surveys_br$CountryName,
          SurveyYear = surveys_br$SurveyYear,
          br)
saveRDS(br, file = "./data/br.rds")
rm(br)

# Pregnancy Recode --------------------------------------------------------

# Information on available surveys
surveys <- dhs_surveys(surveyYearStart=1980)

# Pregnancy recode metadata
gr_meta <- dhs_datasets(fileType = "GR", fileFormat = "flat")

# Identify modules of interest corresponding to surveys of interest
grd <- gr_meta[which(gr_meta$SurveyId %in% surveys$SurveyId),]

# Limit to surveys which have BR module
surveys_gr <- dhs_surveys(surveyYearStart=1980)
surveys_gr <- subset(surveys_gr, SurveyId %in% grd$SurveyId)

# Load all of the datasets into R as a list
grd$path <- unlist(get_datasets(grd$FileName))
gr <- list()
for(survid in grd$SurveyId){
  print(survid)
  dat <- readRDS(grd[which(grd$SurveyId == survid),]$path)
  dat <- dat[grep("caseid|^v0|^v1|^p", names(dat))] # Set variable prefixes of interest (saving on memory)
  gr[[survid]] <- dat
}

# Add survey-level variables
gr <- Map(data.frame,
          SurveyId = surveys_gr$SurveyId,
          CountryName = surveys_gr$CountryName,
          SurveyYear = surveys_gr$SurveyYear,
          gr)
saveRDS(gr, file = "./data/gr.rds")

