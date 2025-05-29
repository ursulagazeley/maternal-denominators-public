########################################
#---------Denominators --------------# 
#-----for maternal outcomes ------------# 
########################################

## --- FILE 100-main.R: INSTALL AND LOAD PACKAGES, RUN ALL OTHER FILES ---

## packages to be installed from cran
from.cran <- c("rdhs", "haven", "tidyverse", "here", "DHS.rates", "countrycode", "ggplot2", 
               "rnaturalearth", "rnaturalearthdata", "gt", "gtsummary", "patchwork", "webshot2")

for(i in c(from.cran)){
  
  ## check if installed, else install  
  if(system.file(package = i) == ""){install.packages(i)}
  
  ## load packages    
  library(i, character.only = TRUE)
  
}

## set path
here::i_am("src/100-main.R")

## create output folder
if(!dir.exists(here::here("gen"))){dir.create(here::here("gen"))}

## run scripts
source(here::here("scripts", "000-functions.R")) 
source(here::here("scripts", "101-download-dhs.R")) 
source(here::here("scripts", "102-set-analytical-sample.R")) 
source(here::here("scripts", "103-prep-FPH.R")) 
source(here::here("scripts", "104-prep-rep-cal.R")) 
source(here::here("scripts", "105-calc-adj-factors.R"))
source(here::here("scripts", "106-adj-denom-mort-rates.R"))
source(here::here("scripts", "107-calc-uncertainty.R"))
source(here::here("scripts", "108-gen-viz.R"))


