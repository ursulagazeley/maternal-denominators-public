################################################################################
#' @description Loads all functions required for analysis
#' @return Functions loaded below
################################################################################

# DHS data helper functions -----------------------------------------------

prep_dhs <- function(x, module = NULL){
  
  x$wt <- x$v005/1000000
  if("v024" %in% names(x)){
    x$region_name <- haven::as_factor(x$v024)
  }else{
    x$region_name <- haven::as_factor(x$v101)
  }
  
  if(module == "ir"){
    # Prepare my vcal_1
    x$myvcal1 <- as.character(x$vcal_1)
    # Replace encoding errors
    x$myvcal1 <- str_replace_all( x$myvcal1, '[\xe1]', 'x')
    # Trim white space
    x$myvcal1 <- gsub(" ", "", x$myvcal1, fixed = TRUE)
    # Recode strings with non-alphanumeric characters as NA
    #x$myvcal1 <- ifelse(grepl("[^a-zA-Z0-9[:space:]]", x$myvcal1), NA, x$myvcal1)
  }
  
  return(x)
}

gen_pregout_gr <- function(x, tips){
  
  # tips should be c(0,4) for 5 years prior to survey
  # tips should be c(0,6) for 7 years prior to survey
  
  periodduration <- (tips[2]-tips[1]+1)*12-1
  
  x$outposition <- x$v008 - x$p3 + x$v018
  x$beg <- x$v018
  x$end <- x$v018 + periodduration
  # Age at pregnancy outcome
  x$age <- (x$p3 - x$v011)/12
  x$agegrp <- cut(x$age, breaks=c(-Inf,15, 20, 25, 30, 35, 40, 45, 50, Inf), 
                        labels=c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49",">50"))
  
  # Subset to births taking place during period of interest
  x <- subset(x, outposition >= beg & outposition <= end)
  
  x$pregout <- NA
  x$pregout[x$p32 == 1] <- "LB"
  x$pregout[x$p32 == 2] <- "SB"
  x$pregout[x$p32 == 3] <- "MSC"
  x$pregout[x$p32 == 4] <- "AB"
  
  x$var <- x$pregout
  x <- subset(x, !is.na(var))
  
  return(x)
}

gen_pregout_cal <- function(x, tips){
  
  # tips should be c(0,4) for 5 years prior to survey
  # v008 Date of interview (CMC)
  # v011 Date of birth of respondent (CMC)
  # v017 Century month code (CMC) for the first month of the calendar. 
  # v018 Row (position) in calendar of month of interview
  # v019 Records the length of the calendar to use for this case. v019 is equal to 80-v018+1. Typically the values are in the range of 60-80.
  
  periodstart <- tips[1]*12
  periodduration <- (tips[2]-tips[1]+1)*12-1
  
  # Create sequential id for each record
  x$indid <- 1:nrow(x)
  
  x <- x[,c("indid","wt","region_name","v008","v011","v012","v013","v017","v018", "v019", "myvcal1")]
  
  x$pbeg <- x$v018 + periodstart # Earlier position in the myvcal1 string
  x$pend <- x$v018 + periodstart + periodduration # Later position in the myvcal1 string
  
  # Check length of myvcal1
  x$myvcal1len <- data.frame(names=x$myvcal1,chr=apply(x,2,nchar))$chr.myvcal1
  # Proportion of observations dropped if pbeg is used as period start
  prop_shortstr <- nrow(subset(x, x$myvcal1len < pbeg))/nrow(x)
  thresh <- 0
  
  # Length of calendar
  x$callen <- x$v019
  # If calendar is not long enough to cover entire period, recode period end
  x$pend <- ifelse(x$callen < x$pend, x$callen, x$pend)
  
  # Count stillbirths
  x$n_stb <- str_count(x$myvcal1, "TPPPPPP")
  # Count live births
  x$n_lb <- str_count(x$myvcal1, "B")
  # Count terminations
  x$n_trm <- str_count(x$myvcal1, "T")
  
  # Only keep individuals who have had a live birth
  df_lb <- subset(x, n_lb > 0)
  # Identify location in string of all live births
  l_lb <- lapply(df_lb$myvcal1, function(y) str_locate_all(y, "B")[[1]])
  names(l_lb) <- df_lb$indid
  # One row per live birth
  df_lb_bylb <- plyr::ldply(l_lb, .id = "indid")
  df_lb_bylb <- merge(df_lb, df_lb_bylb, by = "indid")
  # If myvcal1 is full length (as it is in most recent surveys), use pbeg/pend period
  if(prop_shortstr <= thresh){
    # Limit to those in period of interest
    df_lb_bylb <- subset(df_lb_bylb, start >= pbeg & start <= pend)
  }
  # For stillbirths, need to have 6 months of data prior to termination for it to count
  # Do not count any live births in the first 6 months
  df_lb_bylb$censor <- df_lb_bylb$v019 - 6
  df_lb_bylb <- subset(df_lb_bylb, start < censor)
  # Date of interview minus (month number of preg outcome - month number of interview)
  df_lb_bylb$po_date <- df_lb_bylb$v008 - (df_lb_bylb$start - df_lb_bylb$v018) 
  # Age at pregnancy outcome
  df_lb_bylb$age <- (df_lb_bylb$po_date - df_lb_bylb$v011)/12
  # Age group
  df_lb_bylb$agegrp <- cut(df_lb_bylb$age, 
                           breaks=c(-Inf,15, 20, 25, 30, 35, 40, 45, 50, Inf), 
                           labels=c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49",">50"))
  
  # Only keep individuals who have had a stillbirth
  df_stb <- subset(x, n_stb > 0)
  # Identify location in string of all stillbirths
  l_stb <- lapply(df_stb$myvcal1, function(y) str_locate_all(y, "TPPPPPP")[[1]])
  names(l_stb) <- df_stb$indid
  # One row per stillbirth
  df_stb_bystb <- plyr::ldply(l_stb, .id = "indid")
  df_stb_bystb <- merge(df_stb, df_stb_bystb, by = "indid")
  # If myvcal1 is full length (as it is in most recent surveys), use pbeg/pend period
  if(prop_shortstr <= thresh){
    # Limit to those in period of interest
    df_stb_bystb <- subset(df_stb_bystb, start >= pbeg & start <= pend)
  }
  # Date of preg outcome (CMC)
  # Date of interview minus (month number of preg outcome - month number of interview)
  df_stb_bystb$po_date <- df_stb_bystb$v008 - (df_stb_bystb$start - df_stb_bystb$v018) 
  # Age at pregnancy outcome
  df_stb_bystb$age <- (df_stb_bystb$po_date - df_stb_bystb$v011)/12
  # Age group
  df_stb_bystb$agegrp <- cut(df_stb_bystb$age, 
                             breaks=c(-Inf,15, 20, 25, 30, 35, 40, 45, 50, Inf), 
                             labels=c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49",">50"))
  
  
  # Only keep individuals who have had a termination
  df_trm <- subset(x, n_trm > 0)
  # Identify location in string of all terminations
  l_trm <- lapply(df_trm$myvcal1, function(y) str_locate_all(y, "T")[[1]])
  names(l_trm) <- df_trm$indid
  # One row per termination
  df_trm_bytrm <- plyr::ldply(l_trm, .id = "indid")
  df_trm_bytrm <- merge(df_trm, df_trm_bytrm, by = "indid")
  # Exclude terminations that are stillbirths
  df_trm_bytrm$tstring <- substr(df_trm_bytrm$myvcal1, df_trm_bytrm$start, df_trm_bytrm$start + 6)
  df_trm_bytrm$stb <- ifelse(df_trm_bytrm$tstring == "TPPPPPP", 1, 0)
  df_trm_bytrm <- subset(df_trm_bytrm, stb == 0)
  # Exclude terminations that happened right after period start and therefore we can't know if they were actually a stb
  # Exclude if T is the earliest reported month
  # Exclude if there are <= 5 P before T
  df_trm_bytrm$ntstring <- nchar(df_trm_bytrm$tstring)
  df_trm_bytrm$followingt <- as.character(substr(df_trm_bytrm$tstring, 2, df_trm_bytrm$ntstring))
  df_trm_bytrm$checkp <- lapply(df_trm_bytrm$ntstring, function(x) paste0(rep("P", x-1),  collapse = ""))
  df_trm_bytrm <- subset(df_trm_bytrm, !(ntstring == 1))
  df_trm_bytrm <- subset(df_trm_bytrm, !(followingt == checkp))
  # If myvcal1 is full length (as it is in most recent surveys), use pbeg/pend period
  if(prop_shortstr <= thresh){
    # Limit to those in period of interest
    df_trm_bytrm <- subset(df_trm_bytrm, start >= pbeg & start <= pend)
  }
  # Date of preg outcome (CMC)
  # Date of interview minus (month number of preg outcome - month number of interview)
  df_trm_bytrm$po_date <- df_trm_bytrm$v008 - (df_trm_bytrm$start - df_trm_bytrm$v018) 
  # Age at pregnancy outcome
  df_trm_bytrm$age <- (df_trm_bytrm$po_date - df_trm_bytrm$v011)/12
  # Age group
  df_trm_bytrm$agegrp <- cut(df_trm_bytrm$age, 
                             breaks=c(-Inf,15, 20, 25, 30, 35, 40, 45, 50, Inf), 
                             labels=c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49",">50"))
  
  df1 <- df_lb_bylb %>%
    select(wt, agegrp, region_name) %>%
    mutate(var = "LB")
  
  df2 <- df_stb_bystb %>%
    select(wt, agegrp, region_name) %>%
    mutate(var = "SB")
  
  df3 <- df_trm_bytrm %>%
    select(wt, agegrp, region_name) %>%
    mutate(var = "TRM")
  
  res <- rbind(df1, df2, df3)
  
  return(res)
}

calc_wtd_prop <- function(x, byage = FALSE){
  
  if(byage){
    # Calculate proportion
    suppressMessages(
      result <- x %>%
        group_by(agegrp, var) %>%
        dplyr::summarise(n = n()) %>%
        mutate(per = n /sum(n)) %>%
        filter(!is.na(per)) 
    )
    # Calculate weighted proportion
    suppressMessages(
      result_wtd <- x %>%
        group_by(agegrp, var) %>%
        dplyr::summarise(n = sum(wt)) %>%
        mutate(perwt = n /sum(n)) %>%
        filter(!is.na(perwt)) %>%
        select(-c(n))
    )
  }else{
    suppressMessages(
      result <- x %>%
        group_by(var) %>%
        dplyr::summarise(n = n()) %>%
        mutate(per = n /sum(n)) %>%
        filter(!is.na(per)) 
    )
    suppressMessages(
      result_wtd <- x %>%
        group_by(var) %>%
        dplyr::summarise(n = sum(wt)) %>%
        mutate(perwt = n /sum(n)) %>%
        filter(!is.na(perwt)) %>%
        select(-c(n))
    )
  }
  
  suppressMessages(
    result <- full_join(result, result_wtd)
  )
  return(result)
}

# admort ------------------------------------------------------------------

my_admort <- function(Data.Name, Indicator, JK = NULL, CL = NULL, Strata = NULL, Cluster = NULL, Weight = NULL,
                      Date_of_interview = NULL, PeriodEnd = NULL, Period = NULL, asfrAdj = 1, 
                      EverMW = NULL){
  
  if (!Indicator %in% c("asmr", "aamr", "asmmr", "aammr", "asprmr", "aaprmr", "prmr", "mmr", "aagfr","40q15"))
    stop("Please specify a valid adult mortality indicator, such as asmr, aamr, asmmr, aammr, asprmr, mmr, prmr or aagfr")
  
  if (!is.null(Strata)){
    Data.Name$strata = Data.Name[[Strata]]
    Data.Name$v022 = NULL
    names(Data.Name)[names(Data.Name) == c("strata")] <- c("v022")
  }
  
  if (!is.null(Cluster)){
    Data.Name$cluster = Data.Name[[Cluster]]
    Data.Name$v021 = NULL
    names(Data.Name)[names(Data.Name) == c("cluster")] <- c("v021")
  }
  
  if (!is.null(Weight)){
    Data.Name$weight = Data.Name[[Weight]]
    Data.Name$v005 = NULL
    names(Data.Name)[names(Data.Name) == c("weight")] <- c("v005")
  }
  
  if (!is.null(Date_of_interview)){
    Data.Name$DOI = Data.Name[[Date_of_interview]]
    Data.Name$v008 = NULL
    names(Data.Name)[names(Data.Name) == c("DOI")] <- c("v008")
  }
  
  if (!("v021" %in% names(Data.Name))) stop({message("Error: v021/Primary-sampling-unit is missing")})
  if (!("v005" %in% names(Data.Name))) stop({message("Error: v005/Sample-weight is missing")})
  if (!("v008" %in% names(Data.Name))) stop({message("Error: v008/Date-of-Interview is missing")})
  if (!("v022" %in% names(Data.Name))) stop({message("Error: v022/Sample-strata is missing")})
  
  else{
    
    if (("TRUE" %in% (!(paste("mm1_0", 1:9, sep = "") %in% names(Data.Name)))) |
        ("TRUE" %in% (!(paste("mm1_", 10:20, sep = "") %in% names(Data.Name)))))
    {warning("Siblings variables mm1_01:mm1_20 are not complete; the missing variables were created")}
    
    if (("TRUE" %in% (!(paste("mm2_0", 1:9, sep = "") %in% names(Data.Name)))) |
        ("TRUE" %in% (!(paste("mm2_", 10:20, sep = "") %in% names(Data.Name)))))
    {warning("Siblings variables mm2_01:mm2_20 are not complete; the missing variables were created")}
    
    if (("TRUE" %in% (!(paste("mm4_0", 1:9, sep = "") %in% names(Data.Name)))) |
        ("TRUE" %in% (!(paste("mm4_", 10:20, sep = "") %in% names(Data.Name)))))
    {warning("Siblings variables mm4_01:mm4_20 are not complete; the missing variables were created")}
    
    if (("TRUE" %in% (!(paste("mm8_0", 1:9, sep = "") %in% names(Data.Name)))) |
        ("TRUE" %in% (!(paste("mm8_", 10:20, sep = "") %in% names(Data.Name)))))
    {warning("Siblings variables mm8_01:mm8_20 are not complete; the missing variables were created")}
    
    if (("TRUE" %in% (!(paste("mm9_0", 1:9, sep = "") %in% names(Data.Name)))) |
        ("TRUE" %in% (!(paste("mm9_", 10:20, sep = "") %in% names(Data.Name)))))
    {warning("Siblings variables mm9_01:mm9_20 are not complete; the missing variables were created")}
    
    if (("TRUE" %in% (!(paste("mm12_0", 1:9, sep = "") %in% names(Data.Name)))) |
        ("TRUE" %in% (!(paste("mm12_", 10:20, sep = "") %in% names(Data.Name)))))
    {warning("Siblings variables mm12_01:mm12_20 are not complete; the missing variables were created")}
    
  }
  
  for (i in 1:9){

    if ("TRUE" %in% (!(paste("mmidx_0", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mmidx_", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm1_0", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm1_0", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm2_0", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm2_0", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm4_0", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm4_0", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm8_0", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm8_0", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm9_0", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm9_0", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm12_0", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm12_0", i, sep = "")]] <- NA
    
    # Flag data with no data for MMR
    if ("TRUE" %in% (!(paste("mm16_0", i, sep = "") %in% names(Data.Name))))
      Data.Name$PRMMRT <- NA
    if ("TRUE" %in% (!(paste("mm16_0", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm16_0", i, sep = "")]] <- NA
  }
  
  for (i in 10:20){

    if ("TRUE" %in% (!(paste("mmidx_", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mmidx_", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm1_", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm1_", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm2_", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm2_", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm4_", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm4_", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm8_", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm8_", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm9_", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm9_", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm12_", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm12_", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("mm16_", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("mm16_", i, sep = "")]] <- NA
    
    if ("TRUE" %in% (!(paste("b3_", i, sep = "") %in% names(Data.Name))))
      Data.Name[[paste("b3_", i, sep = "")]] <- NA

  }
  
  Data.Name <- as.data.frame(Data.Name)
  
  if (Indicator == "asmr"){
    my_ASMR(Data.Name, CL, PeriodEnd, Period)[[1]]
    
  }
  else if (Indicator == "aamr"){
    AAMR(Data.Name, JK, CL, PeriodEnd, Period)[[1]]
    
  }
  else if (Indicator == "asmmr"){
    ASMMR(Data.Name, CL, PeriodEnd, Period)[[1]]
    
  }
  else if (Indicator == "aammr"){
    AAMMR(Data.Name, JK, CL, PeriodEnd, Period)[[1]]
    
  }
  else if (Indicator == "asprmr"){
    ASPRMR(Data.Name, CL, PeriodEnd, Period)[[1]]
    
  }
  else if (Indicator == "aaprmr"){
    AAPRMR(Data.Name, JK, CL, PeriodEnd, Period)[[1]]
    
  }
  else if (Indicator == "prmr"){
    my_PRMR(Data.Name, JK, CL, PeriodEnd, Period, asfrAdj, EverMW)
    
  }
  else if (Indicator == "mmr"){
    my_MMR(Data.Name, JK, CL, PeriodEnd, Period, asfrAdj, EverMW)
    
  }
  else if (Indicator == "aagfr"){
    AAGFR(Data.Name, PeriodEnd, Period)[[1]]
    
  }
}

# MMR ---------------------------------------------------------------------

my_MMR <- function (Data.Name, JK = NULL, CL = NULL,
                    PeriodEnd = NULL, Period = NULL, asfrAdj = NULL, EverMW = NULL){
  
  v013 <- rweight <- mm1 <- age5 <- birth <- exposure <- sex <- agegrp <- mm_death <- NULL
  
  Data.Name <- Data.Name[!Data.Name$v005 == 0, ]
  Data.Name$ID <- seq.int(nrow(Data.Name))
  
  # The CI confidence level
  if (is.null(CL)) {
    Z <- stats::qnorm(.025,lower.tail=FALSE)
  } else {
    Z <- stats::qnorm((100-CL)/200,lower.tail=FALSE)
  }
  
  ## Title for the results #########################
  if (is.null(Period)){Periodmsg = 84} else {Periodmsg = Period}
  
  if (is.null(PeriodEnd)){
    PeriodEndy_ <- as.integer((mean(Data.Name$v008) - 1)/12)+1900
    PeriodEndm_ <- round(mean(Data.Name$v008) - ((PeriodEndy_ - 1900) * 12),0)
    
    PeriodEndm_m <- round(min(Data.Name$v008) - ((PeriodEndy_ - 1900) * 12),0)
    PeriodEndm_x <- round(max(Data.Name$v008) - ((PeriodEndy_ - 1900) * 12),0)
  } else {
    dates <- paste(PeriodEnd, "01", sep = "-")
    PeriodEndm_ <- as.numeric(format(as.Date(dates), "%m"))
    PeriodEndy_ <- as.numeric(format(as.Date(dates), "%Y"))
    
    if (PeriodEndm_ >=  round(mean(Data.Name$v008) - (((as.integer((mean(Data.Name$v008) - 1)/12)+1900) - 1900) * 12),0) &
        PeriodEndy_ >= as.integer((mean(Data.Name$v008) - 1)/12)+1900)
      
      message(crayon::bold("Note:", "\n",
                           "You specified a reference period that ends after the survey fieldwork dates....."), "\n",
              "1. Make sure the dates in the survey are coded according to the Gregorian calendar.", "\n",
              "2. If the dates are coded according to the Gregorian calendar, use a proper PeriodEnd that came before the time of the survey.", "\n",
              "3. If the dates are not coded according to the Gregorian calendar, use a PeriodEnd according to the used calendar.")
    
  }
  
  if (is.null(PeriodEnd)){
    cat("\n", crayon::white$bgBlue$bold("The current function calculated MMR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended at the time of the interview, in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_m]), "-", crayon::red$bold$underline(month.abb[PeriodEndm_x]), crayon::red$bold$underline(PeriodEndy_), "\n",
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")
    
  } else {
    cat("\n", crayon::white$bgBlue$bold("The current function calculated MMR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_]), crayon::red$bold$underline(PeriodEndy_), "\n",
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")
  }
  
  
  Data.Name$id <- c(as.factor(Data.Name$v021))
  Data.Name$rweight = Data.Name$v005 / 1000000
  
  DeathEx <- my_DataPrepareM(Data.Name, PeriodEnd, Period)
  
  if (is.null(EverMW)){Data.Name$allwoment = 1} else {Data.Name$allwoment = Data.Name$awfactt/100}
  BirthEx <- my_DataPrepareM_GFR(Data.Name, PeriodEnd, Period)
  BirthEx$exposure = BirthEx$allwoment * BirthEx$exposure
  
  if (is.null(JK)){PSU <- 0} else {PSU <- max(as.numeric(DeathEx$id))}
  
  DeathEx$mm9[is.na(DeathEx$mm9)] <- 0
  DeathEx$prm_death = ifelse(DeathEx$mm1 ==2 &  DeathEx$mm9 >= 2 & DeathEx$mm9 <= 6, DeathEx$death, 0)
  
  PRMMRT = 0
  PRMMRT <- ifelse("TRUE" %in% ("PRMMRT" %in% names(Data.Name)), 0, 1)
  
  if (PRMMRT==0) {
    
    stop({message("Error: mm16 is missing; calculating Maternal Mortality Ratio is not possible. Try Indicator = PRMR for Pregnancy Related Mortality Ratio.")})
    
  } else {
    DeathEx$mm16char <- as.character(DeathEx$mm16)
    DeathEx$mm16char[is.na(DeathEx$mm16char)] <- 0
    
    DeathEx$mm_death = 0
    
    DeathEx$mm_death = ifelse(((DeathEx$mm12 >= 100 &  DeathEx$mm12 <= 141) |
                                 (DeathEx$mm12 == 198 | DeathEx$mm12 == 199 | is.na(DeathEx$mm12))) &
                                (!DeathEx$mm16char == 1 & !DeathEx$mm16char == 2),
                              DeathEx$prm_death, 0)
    
    DeathEx$mm_death = ifelse(is.na(DeathEx$mm_death),
                              0, DeathEx$mm_death)
  }
  
  options(dplyr.summarise.inform = FALSE)
  AGEDIST <- (dplyr::group_by(Data.Name, v013) %>% 
              filter(v013 %in% 1:7) %>% 
              summarise(x = sum(rweight)))$x/sum(Data.Name$rweight)
  
  options(survey.lonely.psu = "adjust")
  dstrat <- tryCatch({
    survey::svydesign(id = ~ v021, strata = ~ v022, weights = ~ rweight, data = DeathEx)
  }, 
  error = function(e){
    survey::svydesign(id = ~ v021, strata = ~ v022, weights = ~ rweight, data = DeathEx, nest = TRUE)
  }
  )
  
  dsub <- subset(dstrat, mm1==2)
  asmmr <- (survey::svyby(~ mm_death, by = ~ agegrp, denominator = ~ exposure,
                          design = dsub, survey::svyratio))[, 2]
  asmmr <- asmmr[1:7]
  
  ASFR <- (dplyr::group_by(BirthEx, age5) %>% summarise(x = sum(birth*rweight)))$x/
    (dplyr::group_by(BirthEx, age5) %>% summarise(x = sum(exposure*rweight)))$x
  ASFR <- ASFR[1:7]
  
  # Adjustment factor
  ASFR_adj <- ASFR * asfrAdj
  
  mmr <- (sum(asmmr * AGEDIST) * 100000) / ceiling(sum(ASFR_adj * AGEDIST))
  
  N     =  (dplyr::group_by(DeathEx, sex) %>% summarise(x = sum(exposure)))$x[1]
  WN    = (survey::svyby(~ exposure, by = ~ sex, design = dsub, survey::svytotal))$exposure[1]
  MMR_DEFT = sqrt(survey::svyby(~ mm_death, by = ~ sex, denominator = ~ exposure,
                                design = dsub, deff = "replace", survey::svyratio)$DEff)[1]
  
  JKres <- matrix(0, nrow = PSU, ncol = 1)
  dimnames(JKres) <- list(NULL, c("MMRj_f"))
  
  AGEGRP <- 1:7
  ADJ <- cbind.data.frame(AGEGRP, AGEDIST, asmmr, ASFR, ASFR_adj, asfrAdj)
  names(ADJ) <- c("AgeGrp","AgeDist", "ASMMR", "ASFR", "ASFRadj", "AdjFac")
  l_res <- list()
  l_res[[1]] <- ADJ
  
  if (is.null(JK)){
    
    RESULTS <- cbind.data.frame(round(WN, 0),round(mmr, 0), round(N, 0), row.names = NULL)
    names(RESULTS) <- c("Exposure_years","MMR", "N")
    l_res[[2]] <- RESULTS
    return(l_res)
    
  } else {
    
    for (i in unique(as.numeric(DeathEx$id)))
    {
      Data.NameJ <- Data.Name[which(!Data.Name$id == i), ]
      DeathExJ <- DeathEx[which(!DeathEx$id == i), ]
      BirthExJ <- BirthEx[which(!BirthEx$id == i), ]
      
      AGEDISTj <- (dplyr::group_by(Data.NameJ, v013) %>% 
                   filter(v013 %in% 1:7) %>% 
                   summarise(x = sum(rweight)))$x/sum(Data.NameJ$rweight)
      
      ASMMRj <- (dplyr::group_by(DeathExJ, sex, agegrp) %>% summarise(x = sum(mm_death*rweight)))$x/
        (dplyr::group_by(DeathExJ, sex, agegrp) %>% summarise(x = sum(exposure*rweight)))$x
      ASMMRj <- ASMMRj[1:7]
      
      ASFRj <- (dplyr::group_by(BirthExJ, age5) %>% summarise(x = sum(birth*rweight)))$x/
        (dplyr::group_by(BirthExJ, age5) %>% summarise(x = sum(exposure*rweight)))$x
      ASFRj <- ASFRj[1:7]
      ASFRj_adj <- ASFRj * asfrAdj
      
      JKres[i,1] <- (sum(ASMMRj * AGEDISTj) * 100000) / (sum(ASFRj_adj * AGEDISTj))
    }
    MMRjf = JKres[1:PSU, 1]
    JKSEf = ((PSU * mmr[1] - (PSU-1) * MMRjf)-mmr[1])^2
    SE = sqrt(sum(JKSEf) / (PSU * (PSU-1)))
    RSE = SE / mmr
    LCI = mmr - (Z * SE)
    LCI[LCI <= 0] = 0
    UCI = mmr + (Z * SE)
    PSUs = PSU
    
    RESULTS <- cbind.data.frame(round(WN, 0), round(mmr,0), round(SE,3), round(N, 0), round(MMR_DEFT,3), round(RSE,3), round(LCI,3), round(UCI,3), PSUs, row.names = NULL)
    names(RESULTS) <- c("Exposure_years", "MMR", "SE", "N", "DEFT", "RSE", "LCI", "UCI", "iterations")
    l_res[[2]] <- RESULTS
    return(l_res)
  }
}

# PRMR --------------------------------------------------------------------

# https://github.com/cran/DHS.rates/blob/master/R/PRMR.R
my_PRMR <- function (Data.Name, JK = NULL, CL = NULL,
                     PeriodEnd = NULL, Period = NULL, asfrAdj = NULL, EverMW = NULL){
  
  v013 <- rweight <- mm1 <- age5 <- birth <- exposure <- sex <- agegrp <- mm_death <- NULL
  
  Data.Name <- Data.Name[!Data.Name$v005 == 0, ]
  Data.Name$ID <- seq.int(nrow(Data.Name))
  
  # The CI confidence level
  if (is.null(CL)) {
    Z <- stats::qnorm(.025,lower.tail=FALSE)
  } else {
    Z <- stats::qnorm((100-CL)/200,lower.tail=FALSE)
  }
  
  ## Title for the results #########################
  if (is.null(Period)){Periodmsg = 84} else {Periodmsg = Period}
  
  if (is.null(PeriodEnd)){
    PeriodEndy_ <- as.integer((mean(Data.Name$v008) - 1)/12)+1900
    PeriodEndm_ <- round(mean(Data.Name$v008) - ((PeriodEndy_ - 1900) * 12),0)
    
    PeriodEndm_m <- round(min(Data.Name$v008) - ((PeriodEndy_ - 1900) * 12),0)
    PeriodEndm_x <- round(max(Data.Name$v008) - ((PeriodEndy_ - 1900) * 12),0)
  } else {
    dates <- paste(PeriodEnd, "01", sep = "-")
    PeriodEndm_ <- as.numeric(format(as.Date(dates), "%m"))
    PeriodEndy_ <- as.numeric(format(as.Date(dates), "%Y"))
    
    if (PeriodEndm_ >=  round(mean(Data.Name$v008) - (((as.integer((mean(Data.Name$v008) - 1)/12)+1900) - 1900) * 12),0) &
        PeriodEndy_ >= as.integer((mean(Data.Name$v008) - 1)/12)+1900)
      
      message(crayon::bold("Note:", "\n",
                           "You specified a reference period that ends after the survey fieldwork dates....."), "\n",
              "1. Make sure the dates in the survey are coded according to the Gregorian calendar.", "\n",
              "2. If the dates are coded according to the Gregorian calendar, use a proper PeriodEnd that came before the time of the survey.", "\n",
              "3. If the dates are not coded according to the Gregorian calendar, use a PeriodEnd according to the used calendar.")
    
  }
  
  if (is.null(PeriodEnd)){
    cat("\n", crayon::white$bgBlue$bold("The current function calculated PRMR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended at the time of the interview, in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_m]), "-", crayon::red$bold$underline(month.abb[PeriodEndm_x]), crayon::red$bold$underline(PeriodEndy_), "\n",
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")
    
  } else {
    cat("\n", crayon::white$bgBlue$bold("The current function calculated PRMR based on a reference period of"),
        crayon::red$bold$underline(Periodmsg), crayon::white$bold$bgBlue("months"), "\n", crayon::white$bold$bgBlue("The reference period ended in"), crayon::red$bold$underline(PeriodEndy_ + round(PeriodEndm_/12,digits=2)), "OR", crayon::red$bold$underline(month.abb[PeriodEndm_]), crayon::red$bold$underline(PeriodEndy_), "\n",
        crayon::white$bold$bgBlue("The average reference period is"), crayon::red$bold$underline(round((PeriodEndy_ + PeriodEndm_/12)-(Periodmsg/24), digits =2)), "\n")
  }
  
  
  Data.Name$id <- c(as.factor(Data.Name$v021))
  Data.Name$rweight = Data.Name$v005 / 1000000
  
  DeathEx <- my_DataPrepareM(Data.Name, PeriodEnd, Period)
  
  if (is.null(EverMW)){Data.Name$allwoment = 1} else {Data.Name$allwoment = Data.Name$awfactt/100} 
  BirthEx <- my_DataPrepareM_GFR(Data.Name, PeriodEnd, Period)
  BirthEx$exposure = BirthEx$allwoment * BirthEx$exposure 
  
  if (is.null(JK)){PSU <- 0} else {PSU <- max(as.numeric(DeathEx$id))}
  
  DeathEx$mm9[is.na(DeathEx$mm9)] <- 0
  DeathEx$prm_death = ifelse(DeathEx$mm1 ==2 &  DeathEx$mm9 >= 2 & DeathEx$mm9 <= 6, DeathEx$death, 0)
  
  
  DeathEx$mm_death = DeathEx$prm_death
  
  options(dplyr.summarise.inform = FALSE)
  AGEDIST <- (dplyr::group_by(Data.Name, v013) %>% 
              filter(v013 %in% 1:7) %>% 
              summarise(x = sum(rweight)))$x/sum(Data.Name$rweight)
  
  options(survey.lonely.psu = "adjust")
  dstrat <- tryCatch({
    survey::svydesign(id = ~ v021, strata = ~ v022, weights = ~ rweight, data = DeathEx)
  }, 
  error = function(e){
    survey::svydesign(id = ~ v021, strata = ~ v022, weights = ~ rweight, data = DeathEx, nest = TRUE)
  }
  )
  
  dsub <- subset(dstrat, mm1==2)
  asprmr <- (survey::svyby(~ mm_death, by = ~ agegrp, denominator = ~ exposure,
                           design = dsub, survey::svyratio))[, 2]
  asprmr <- asprmr[1:7]
  
  ASFR <- (dplyr::group_by(BirthEx, age5) %>% summarise(x = sum(birth*rweight)))$x/
    (dplyr::group_by(BirthEx, age5) %>% summarise(x = sum(exposure*rweight)))$x
  ASFR <- ASFR[1:7]
  
  # Adjustment factor
  ASFR_adj <- ASFR * asfrAdj
  
  prmr <- (sum(asprmr * AGEDIST) * 100000) / ceiling(sum(ASFR_adj * AGEDIST))
  
  N     =  (dplyr::group_by(DeathEx, sex) %>% summarise(x = sum(exposure)))$x[1]
  WN    = (survey::svyby(~ exposure, by = ~ sex, design = dsub, survey::svytotal))$exposure[1]
  PRMR_DEFT = sqrt(survey::svyby(~ mm_death, by = ~ sex, denominator = ~ exposure,
                                 design = dsub, deff = "replace", survey::svyratio)$DEff)[1]
  
  JKres <- matrix(0, nrow = PSU, ncol = 1)
  dimnames(JKres) <- list(NULL, c("PRMRj_f"))
  
  AGEGRP <- 1:7
  ADJ <- cbind.data.frame(AGEGRP, AGEDIST, asprmr, ASFR, ASFR_adj, asfrAdj)
  names(ADJ) <- c("AgeGrp","AgeDist", "ASPRMR", "ASFR", "ASFRadj", "AdjFac")
  l_res <- list()
  l_res[[1]] <- ADJ
  
  if (is.null(JK)){
    
    RESULTS <- cbind.data.frame(round(WN, 0),round(prmr, 0), round(N, 0),  row.names = NULL)
    names(RESULTS) <- c("Exposure_years","PRMR", "N")
    l_res[[2]] <- RESULTS
    return(l_res)
    
  } else {
    
    for (i in unique(as.numeric(DeathEx$id)))
    {
      Data.NameJ <- Data.Name[which(!Data.Name$id == i), ]
      DeathExJ <- DeathEx[which(!DeathEx$id == i), ]
      BirthExJ <- BirthEx[which(!BirthEx$id == i), ]
      
      AGEDISTj <- (dplyr::group_by(Data.NameJ, v013) %>% 
                   filter(v013 %in% 1:7) %>% 
                   summarise(x = sum(rweight)))$x/sum(Data.NameJ$rweight)
      
      ASPRMRj <- (dplyr::group_by(DeathExJ, sex, agegrp) %>% summarise(x = sum(mm_death*rweight)))$x/
        (dplyr::group_by(DeathExJ, sex, agegrp) %>% summarise(x = sum(exposure*rweight)))$x
      ASPRMRj <- ASPRMRj[1:7]
      
      ASFRj <- (dplyr::group_by(BirthExJ, age5) %>% summarise(x = sum(birth*rweight)))$x/
        (dplyr::group_by(BirthExJ, age5) %>% summarise(x = sum(exposure*rweight)))$x
      ASFRj <- ASFRj[1:7]
      ASFRj_adj <- ASFRj * asfrAdj
      
      JKres[i,1] <- (sum(ASPRMRj * AGEDISTj) * 100000) / (sum(ASFRj_adj * AGEDISTj))
    }
    PRMRjf = JKres[1:PSU, 1]
    JKSEf = ((PSU * prmr[1] - (PSU-1) * PRMRjf)-prmr[1])^2
    SE = sqrt(sum(JKSEf) / (PSU * (PSU-1)))
    RSE = SE / prmr
    LCI = prmr - (Z * SE)
    LCI[LCI <= 0] = 0
    UCI = prmr + (Z * SE)
    PSUs = PSU
    
    RESULTS <- cbind.data.frame(round(WN, 0), round(prmr,0), round(SE,3), round(N, 0), round(PRMR_DEFT,3), round(RSE,3), round(LCI,3), round(UCI,3), PSUs, row.names = NULL)
    names(RESULTS) <- c("Exposure_years","PRMR", "SE", "N", "DEFT", "RSE", "LCI", "UCI", "iterations")
    l_res[[2]] <- RESULTS
    return(l_res)
  }
}

# AAGFR -------------------------------------------------------------------

# https://github.com/cran/DHS.rates/blob/master/R/AAGFR.R
AAGFR <- function (Data.Name,
                   PeriodEnd = NULL, Period = NULL) {
  
  v013 <- rweight <- age5 <- birth <- exposure <- NULL
  
  Data.Name <- Data.Name[!Data.Name$v005 == 0, ]
  Data.Name$ID <- seq.int(nrow(Data.Name))
  
  Data.Name$id <- c(as.factor(Data.Name$v021))
  Data.Name$rweight = Data.Name$v005 / 1000000
  
  BirthEx <- DataPrepareM_GFR(Data.Name, PeriodEnd, Period)
  
  options(dplyr.summarise.inform = FALSE)
  AGEDIST <- (dplyr::group_by(Data.Name, v013) %>% summarise(x = sum(rweight)))$x/sum(Data.Name$rweight)
  
  ASFR <- (dplyr::group_by(BirthEx, age5) %>% summarise(x = sum(birth*rweight)))$x/
    (dplyr::group_by(BirthEx, age5) %>% summarise(x = sum(exposure*rweight)))$x
  
  gfr <- ceiling(sum(ASFR[1:7] * AGEDIST))
  
  list(gfr)[[1]]
}

# DataPrepareM ------------------------------------------------------------

# https://github.com/cran/DHS.rates/blob/master/R/DataPrepareM.R
my_DataPrepareM <- function(Dat, PeriodEnd = NULL, Period = NULL){
  
  Dat$rweight = Dat$v005 / 1000000
  
  if(any(is.na(Dat$v022))){
    Dat$v022 <- as.numeric(as.character(Dat$v023))
    if(any(is.na(Dat$v022))){
      Dat$v022 <- 1
    }
  }
  
  if (!is.null(PeriodEnd)) {
    dates <- paste(PeriodEnd, "01", sep = "-")
    PeriodEndm <- as.numeric(format(as.Date(dates), "%m"))
    PeriodEndy <- as.numeric(format(as.Date(dates), "%Y"))
    PeriodEndcmc <- ((PeriodEndy - 1900) * 12) + PeriodEndm
  }
  
  # 1. Construct a siblings data ##########################################################
  myvarsid <- c("ID", "v021", "v005", "v008", "v022", "rweight")
  
  myvars <- c(myvarsid,
              paste("mmidx_0", 1:9, sep = ""), paste("mmidx_", 10:20, sep = ""),
              paste("mm1_0", 1:9, sep = ""), paste("mm1_", 10:20, sep = ""),
              paste("mm2_0", 1:9, sep = ""), paste("mm2_", 10:20, sep = ""),
              paste("mm4_0", 1:9, sep = ""), paste("mm4_", 10:20, sep = ""),
              paste("mm8_0", 1:9, sep = ""), paste("mm8_", 10:20, sep = ""),
              paste("mm9_0", 1:9, sep = ""), paste("mm9_", 10:20, sep = ""),
              paste("mm12_0", 1:9, sep = ""), paste("mm12_", 10:20, sep = ""),
              paste("mm16_0", 1:9, sep = ""), paste("mm16_", 10:20, sep = ""))
  
  Dat <- as.data.frame(Dat[myvars])
  
  def <- stats::reshape(Dat,
                        direction = "long",
                        varying = list(mmidx = 7:26, mm1 = 27:46, mm2 = 47:66, mm4 = 67:86, mm8 = 87:106,
                                       mm9 = 107:126, mm12 = 127:146, mm16 = 147:166),
                        v.names = c("mmidx", "mm1", "mm2", "mm4", "mm8", "mm9", "mm12", "mm16"),
                        timevar = "alt")
  
  #################################################################################
  
  def <- def[stats::complete.cases(def$mmidx), ]
  def <- def[stats::complete.cases(def$mm1), ]
  def <- def[stats::complete.cases(def$mm2), ]
  def <- def[!def$mm1 == 8 & !def$mm1 == 9 & !def$mm2 == 8 & !def$mm2 == 9 , ] 
  # Dropping if sibling sex or survival status are don't know or missing 
  
  #################################################################################
  # 2. Deaths and exposure for each age group  #################################
  if (is.null(PeriodEnd)){def$periodend = def$v008} else {def$periodend = PeriodEndcmc}
  
  if (is.null(Period)){def$period = 84} else {def$period = Period}
  
  def$upplim <- ifelse(def$mm2 == 0, def$mm8, def$v008-1)
  def$lowlim = def$v008-def$period
  def$exposure <- ifelse(def$upplim-def$lowlim+1 < 0, 0, def$upplim - def$lowlim + 1)
  
  def$agegrp1 = as.integer((def$upplim-def$mm4)/60)
  def$expo1 = ifelse(def$exposure < def$upplim - (def$mm4 + def$agegrp1*60) + 1, def$exposure, def$upplim - (def$mm4 + def$agegrp1*60) + 1)
  def$death1 = ifelse(def$mm2==0 & def$expo1 > 0, 1, 0)
  def$exposure = def$exposure - def$expo1
  
  def$agegrp2 = def$agegrp1 - 1
  def$expo2 = ifelse(def$exposure < 60, def$exposure, 60)
  def$death2 = 0
  def$exposure = def$exposure - def$expo2
  
  def$agegrp3 = def$agegrp2 - 1
  def$expo3 = def$exposure
  def$death3 = 0
  
  myvarsid <- c("ID", "v021", "v005", "v008", "v022", "rweight",
                "mmidx","mm1","mm2","mm4","mm8","mm9","mm12", "mm16")
  
  myvars <- c(myvarsid,
              paste("agegrp", 1:3, sep = ""),
              paste("expo", 1:3, sep = ""),
              paste("death", 1:3, sep = ""))
  
  def <- as.data.frame(def[myvars])
  
  rdef <- stats::reshape(def,
                         direction = "long",
                         varying = list(agegrp = 15:17, expo = 18:20, death = 21:23),
                         v.names = c("agegrp", "expo", "death"),
                         timevar = "alt")
  
  DeathEx <- rdef[rdef$agegrp >= 3 & rdef$agegrp <= 9, ]
  
  DeathEx <- subset(DeathEx, !is.na(v021))
  
  DeathEx$sex = 2 - DeathEx$mm1
  DeathEx$sex[DeathEx$sex == -1] <- 1 # If male transgender (e.g., CO2015DHS, mm1 = 3), assign to male
  DeathEx$sex[DeathEx$sex == -2] <- 0 # If female transgender (e.g., CO2015DHS, mm1 = 4), assign to female
  
  DeathEx$exposure = DeathEx$expo/12
  DeathEx$death = DeathEx$death*1000
  
  DeathEx$id <- c(as.factor(DeathEx$v021))
  
  return(DeathEx)
}

# DataPrepM_GFR -----------------------------------------------------------

# https://github.com/cran/DHS.rates/blob/master/R/DataPrepareM_GFR.R
my_DataPrepareM_GFR <- function(Dat, PeriodEnd = NULL, Period = NULL){
  
  Dat$rweight = Dat$v005 / 1000000
  
  if(any(is.na(Dat$v022))){
    Dat$v022 <- as.numeric(as.character(Dat$v023))
    if(any(is.na(Dat$v022))){
      Dat$v022 <- 1
    }
  }
  
  if (!is.null(PeriodEnd)) {
    dates <- paste(PeriodEnd, "01", sep = "-")
    PeriodEndm <- as.numeric(format(as.Date(dates), "%m"))
    PeriodEndy <- as.numeric(format(as.Date(dates), "%Y"))
    PeriodEndcmc <- ((PeriodEndy - 1900) * 12) + PeriodEndm
  }
  
  # 1. Construct a children data ##########################################################
  myvars <- c(paste("ID"), paste("v021"), paste("v005"), paste("v008"), paste("v011"),
              paste("v022"), paste("allwoment"),paste("rweight"),
              paste("b3_0", 1:9, sep = ""), paste("b3_", 10:20, sep = ""))
  
  def <- reshape::melt(as.data.frame(Dat[myvars]), id = c("ID", "v021", "v005", "v008", "v011",
                                                          "v022", "rweight", "allwoment")) ## hallie patch: added allwoment
  
  names(def)[names(def) == c("value")] <- c("B3")
  def$variable <- NULL
  
  # 2. Births to women 15-49 during the reference period  #################################
  if (is.null(PeriodEnd)){def$periodend = def$v008} else {def$periodend = PeriodEndcmc}
  
  if (is.null(Period)){def$period = 84} else {def$period = Period}
  def$age5 = as.integer((def$B3 - def$v011) / 60) - 3
  def$birth <- 0
  def$birth[def$periodend - def$B3 >= 0 &
              def$periodend - def$B3 <= def$period & def$age5 >= 0] <- 1
  
  def$B3 <- NULL
  def$exposure = 0
  def$exposureg = 0
  def <- def[stats::complete.cases(def$age5), ]
  
  def$birth <- ifelse(def$age5 < 0, 0, def$birth)
  
  # 3. Exposure of women 15-49  ###########################################################
  newdata <- c("ID", "v021", "v005", "v008", "v011", "v022", "rweight", "allwoment") ## hallie patch: added allwoment
  def2 <- Dat[newdata]
  
  if (is.null(PeriodEnd)){def2$periodend = def2$v008} else {def2$periodend = PeriodEndcmc}
  
  if (is.null(Period)){def2$period = 84} else {def2$period = Period}
  def2$agem   = def2$periodend - def2$v011 - 1 #age at the end of the period
  def2$age5   = as.integer(def2$agem / 60) #age group at the end of the period
  def2$higexp = def2$agem - (def2$age5 * 60) + 1  #Exposure (number of months) in current age group
  def2$higexp <- ifelse(def2$higexp >= def2$period, def2$period, def2$higexp)
  def2$age5   = def2$age5 - 3
  def2 <- def2[def2$age5 >= 0, ]
  
  ## Exposure in previous age group #####
  def2$lowexp <- ifelse(def2$higexp < def2$period & def2$age5 >= 1 , def2$period - def2$higexp, 0)
  def2$birth = 0
  def2$agem <- NULL
  def2l <- def2
  def2$lowexp <- NULL
  def2l$higexp <- NULL
  names(def2)[names(def2) == c("higexp")] <- c("exposure")
  names(def2l)[names(def2l) == c("lowexp")] <- c("exposure")
  def2l$age5 = def2l$age5 -1
  def3 <- rbind(def2, def2l)
  def3$exposure = def3$exposure / 12
  def3$exposureg <- ifelse(def3$age5 == 6, 0, def3$exposure)
  
  def4 <- rbind(def, def3)
  def4$birth = def4$birth * 1000
  
  BirthEx <- merge(stats::aggregate(list(def4$birth, def4$exposure, def4$exposureg),
                                    list(def4$ID, def4$v021, def4$v022, def4$age5, def4$allwoment), sum), ## hallie patch: added allwoment
                   stats::aggregate(def4$rweight, list(def4$ID), mean), by = "Group.1")
  
  names(BirthEx) <- c("ID", "v021", "v022", "age5", "allwoment","birth", "exposure", ## hallie patch: added allwoment
                      "exposureg", "rweight")
  BirthEx <- BirthEx[BirthEx$birth != 0 | BirthEx$exposure != 0, ]
  BirthEx$id <- c(as.factor(BirthEx$v021))
  
  return(BirthEx)
}

# DataPrepare -------------------------------------------------------------

# https://github.com/cran/DHS.rates/blob/master/R/DataPrepare.R
my_DataPrepare <- function(Dat, PeriodEnd = NULL, Period = NULL)
{
  Dat$rweight = Dat$v005 / 1000000
  
  # Hallie patch ###########
  if(any(is.na(Dat$v022))){
    Dat$v022 <- as.numeric(as.character(Dat$v023))
    # If still NA...
    if(any(is.na(Dat$v022))){
      Dat$v022 <- 1
    }
  }
  #################################################
  
  if (!is.null(PeriodEnd)) {
    dates <- paste(PeriodEnd, "01", sep = "-")
    PeriodEndm <- as.numeric(format(as.Date(dates), "%m"))
    PeriodEndy <- as.numeric(format(as.Date(dates), "%Y"))
    PeriodEndcmc <- ((PeriodEndy - 1900) * 12) + PeriodEndm
  }
  
  # 1. Construct a children data ##########################################################
  myvars <- c(paste("ID"), paste("v021"), paste("v005"), paste("v008"), paste("v011"),
              paste("v022"), paste("allwoment"), paste("rweight"),
              paste("b3_0", 1:9, sep = ""), paste("b3_", 10:20, sep = ""))
  
  def <- reshape::melt(as.data.frame(Dat[myvars]), id = c("ID", "v021", "v005", "v008", "v011",
                                                          "v022", "rweight", "allwoment"))
  
  names(def)[names(def) == c("value")] <- c("B3")
  def$variable <- NULL
  
  # 2. Births to women 15-49 during the reference period  #################################
  if (is.null(PeriodEnd)){def$periodend = def$v008} else {def$periodend = PeriodEndcmc}
  
  if (is.null(Period)){def$period = 36} else {def$period = Period}
  def$age5 = as.integer((def$B3 - def$v011) / 60) - 3
  def$birth <- 0
  def$birth[def$periodend - def$B3 > 0 &
              def$periodend - def$B3 <= def$period & def$age5 >= 0] <- 1
  def$B3 <- NULL
  def$exposure = 0
  def$exposureg = 0
  def <- def[stats::complete.cases(def$age5), ]
  
  # 3. Exposure of women 15-49  ###########################################################
  newdata <- c("ID", "v021", "v005", "v008", "v011", "v022", "rweight", "allwoment")
  def2 <- Dat[newdata]
  
  if (is.null(PeriodEnd)){def2$periodend = def2$v008} else {def2$periodend = PeriodEndcmc}
  
  if (is.null(Period)){def2$period = 36} else {def2$period = Period}
  def2$agem   = def2$periodend - def2$v011 - 1 #age at the end of the period
  def2$age5   = as.integer(def2$agem / 60) #age group at the end of the period
  def2$higexp = def2$agem - (def2$age5 * 60) + 1  #Exposure (number of months) in current age group
  def2$higexp <- ifelse(def2$higexp >= def2$period, def2$period, def2$higexp)
  def2$age5   = def2$age5 - 3
  def2 <- def2[def2$age5 >= 0, ]
  
  ## Exposure in previous age group #####
  def2$lowexp <- ifelse(def2$higexp < def2$period & def2$age5 >= 1 , def2$period - def2$higexp, 0)
  def2$birth = 0
  def2$agem <- NULL
  def2l <- def2
  def2$lowexp <- NULL
  def2l$higexp <- NULL
  names(def2)[names(def2) == c("higexp")] <- c("exposure")
  names(def2l)[names(def2l) == c("lowexp")] <- c("exposure")
  def2l$age5 = def2l$age5 -1
  def3 <- rbind(def2, def2l)
  def3$exposure = def3$exposure / 12
  def3$exposureg <- ifelse(def3$age5 == 6, 0, def3$exposure)
  
  def4 <- rbind(def, def3)
  def4$birth = def4$birth * 1000
  
  BirthEx <- merge(stats::aggregate(list(def4$birth, def4$exposure, def4$exposureg),
                                    list(def4$ID, def4$v021, def4$v022, def4$age5, def4$allwoment), sum),
                   stats::aggregate(def4$rweight, list(def4$ID), mean), by = "Group.1")
  
  names(BirthEx) <- c("ID", "v021", "v022", "age5", "allwoment", "birth", "exposure",
                      "exposureg", "rweight")
  BirthEx <- BirthEx[BirthEx$birth != 0 | BirthEx$exposure != 0, ]
  BirthEx$id <- c(as.factor(BirthEx$v021))
  
  return(BirthEx)
}

# Helper functions for visualizations -------------------------------------

# Function to process and filter recent datasets
process_and_filter_recent <- function(data, dhs_cc) {
  # Extract country code and survey year
  data$country_code <- substr(data$SurveyId, 1, 2)
  data$survey_year <- as.numeric(substr(data$SurveyId, 3, 6)) # Convert to numeric
  data <- merge(data, dhs_cc, by = "country_code", all = TRUE)
  data$country_name <- countrycode(data$iso3, origin = "iso3c", destination = "country.name")

  # Create the 'country_year' column
  data$country_year <- paste(data$country_name, " ", data$survey_year, sep = "")
  
  # Keep only the most recent survey for each country
  data <- data %>%
    group_by(country_name) %>%
    filter(survey_year == max(survey_year)) %>%
    ungroup() # Ungroup after filtering
  
  return(data)
}

# Calculating absolute and relative change in MMR and PRMR 
process_mmr_prmr <- function(data, value_column) {
  data %>%
    pivot_wider(names_from = Adj, values_from = !!sym(value_column))%>% 
    mutate(
      abs_change_SB = (adjSB - noAdj),
      abs_change_TRM = (adjTRM - noAdj),
      abs_change_ALL = (adjAll - noAdj),
      perc_change_SB = ((adjSB - noAdj) / noAdj) * 100,
      log_change_SB = (adjSB / noAdj),
      perc_change_TRM = ((adjTRM - noAdj) / noAdj) * 100,
      log_change_TRM = (adjTRM / noAdj),
      perc_change_ALL = ((adjAll - noAdj) / noAdj) * 100,
      log_change_ALL = (adjAll / noAdj)
    ) %>%
    # Keep just variables we need
    select(
      SurveyId,
      country_year, country_name, country_code, 
      abs_change_SB, abs_change_ALL, perc_change_SB, perc_change_ALL)%>%
    pivot_longer(
      cols = c(abs_change_SB, abs_change_ALL, perc_change_SB, perc_change_ALL),
      names_to = "Adj",
      values_to = value_column)
}

# Identify largest and smallest changes in MMR and PRMR 
find_min_max <- function(data, value_column) {
  data %>%
    filter(Adj %in% c("perc_change_SB", "perc_change_ALL")) %>% 
    group_by(Adj) %>%
    summarize(
      min_country = country_year[which.min(!!sym(value_column))],
      min_value = min(!!sym(value_column), na.rm = TRUE),
      max_country = country_year[which.max(!!sym(value_column))],
      max_value = max(!!sym(value_column), na.rm = TRUE),
      mean_value = mean(!!sym(value_column), na.rm = TRUE), 
      .groups = "drop"
    )
}
