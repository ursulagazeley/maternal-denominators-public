################################################################################
#' @description Generate figures and tables for displaying results
#' @return Tables and plots
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
source("./src/000-functions.R")
library(tidyverse)
library(ggplot2)
library(gt)
library(countrycode)
library(here)
library(patchwork)
library(webshot2)
#' Inputs
# Mortality ratios
l_res <- readRDS("./gen/mort-rates-adj.rds")
mmr <- l_res$MMR
prmr <- l_res$PRMR
# Uncertainty calculations for mmr and prmr
l_uncert <- readRDS("./gen/mort-rates-uncertainty.rds")
mmr_uncert <- l_uncert$MMR
prmr_uncert <- l_uncert$PRMR
# Pregnancy outcome proportions from both reproductive calendar and fphs with adjustment factors
adjfac <- read.csv("./gen/adjustment-factors.csv")
# Analytical sample
sample <- read.csv("./gen/surveys-sample.csv")
# DHS country codes and ISO3
dhs_cc <- read.csv("./data/dhs-country-codes.csv")
################################################################################

# List of datasets to process
datasets <- list(mmr, prmr, adjfac, sample, mmr_uncert, prmr_uncert)

# Apply the processing and filtering function to each dataset
datasets <- lapply(datasets, function(x){ process_and_filter_recent(x, dhs_cc)})

mmr <- datasets[[1]]
prmr <- datasets[[2]]
adjfac <- datasets[[3]]
sample <- datasets[[4]]
mmr_uncert <- datasets[[5]]
prmr_uncert <- datasets[[6]]

##### SUMMARY STATISTICS #####

# Identifying highest and lowest proportions for pregnancy outcomes 
adjfac_long <- adjfac %>%
  pivot_longer(
    cols = c(LB, SB, TRM),    
    names_to = "var",         
    values_to = "perwt"       
  )

adjfac_country <- adjfac_long %>%
  group_by(country_year, var) %>% 
  summarise(total_perwt = sum(perwt), .groups = "drop") %>% 
  group_by(country_year) %>% 
  mutate(proportion = total_perwt / sum(total_perwt)) %>% 
  ungroup()

proportions <- adjfac_country %>%
  group_by(var) %>%                   
  summarize(
    highest_country = country_year[which.max(proportion)], 
    highest_value = max(proportion),                     
    lowest_country = country_year[which.min(proportion)], 
    lowest_value = min(proportion),
    mean_proportion = mean(proportion, na.rm = TRUE) 
  )                        
print(proportions)

# Apply the function to both mmr and prmr datasets
mmr_change <- process_mmr_prmr(mmr, value_column = "MMR")
prmr_change <- process_mmr_prmr(prmr, value_column = "PRMR")

# Apply to both mmr_change and prmr_change
mmr_min_max <- find_min_max(mmr_change, value_column = "MMR")
prmr_min_max <- find_min_max(prmr_change, value_column = "PRMR")

# Print results
print(mmr_min_max)
print(prmr_min_max)


###### OUTPUT FOR MAIN TEXT ######

# Figure 1 ----------------------------------------------------------------

# plot proportion of pregnancy outcomes by country

# Step 1: Ensure the global order of `var` (LB -> TRM -> SB)
adjfac_country$var <- factor(adjfac_country$var, levels = c("LB", "TRM", "SB"))

# Step 2: Country order 
country_order <- adjfac_country %>%
  group_by(country_year) %>%
  summarize(
    lb_score = sum(proportion[var == "LB"]),
    trm_sb_score = sum(proportion[var %in% c("TRM", "SB")])
  ) %>%
  arrange(desc(lb_score), desc(trm_sb_score)) %>%
  pull(country_year)

summary_country <- adjfac_country
summary_country$country_year <- factor(
  summary_country$country_year,
  levels = country_order
)

# Step 3: Create the plot
fig1 <- ggplot(summary_country, aes(x = proportion, y = country_year, fill = var)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +  # Convert x-axis to percentage
  scale_fill_manual(values = c("LB" = "#1f77b4", "SB" = "#2ca02c", "TRM" = "#ff7f0e"),
                    labels = c("Stillbirths", "Miscarriages/Induced abortions", "Livebirths"), 
                    breaks = c("SB", "TRM", "LB")) +  
  labs(
    title = NULL,
    x = "Proportion",
    y = "Country and Survey Year",
    fill = "Pregnancy outcome"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = "bottom"
  )

fig1
ggsave(here("gen", "Figure1_preg-prop-country.png"), fig1, bg = "white", width = 10, height = 15)



# Figure 2 ----------------------------------------------------------------

# absolute and relative changes in mmr with total births adjustment

# Step 1: Order countries by the size of relative MMR change using total births 
mmr_change_tb <- mmr_change %>%
  group_by(country_year) %>%
  mutate(
    MMR_change = MMR[Adj == "perc_change_SB"],
    country_order = mean(MMR_change, na.rm = TRUE)
  ) %>%
  filter(Adj == "abs_change_SB"| Adj == "perc_change_SB")%>%
  ungroup()

# Step 2

# Absolute change (total births adjustment)
fig2a <- ggplot(subset(mmr_change_tb, Adj == "abs_change_SB"), 
                aes(x = MMR, y = reorder(country_year, -country_order))) +
  geom_segment(aes(x = 0, xend = MMR, y = reorder(country_year, -country_order), yend = reorder(country_year, -country_order)), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "#1f77b4") +  
  labs(
    x = "Absolute change in Maternal Mortality Ratio (MMR)",
    y = "Country and DHS Survey"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

fig2a

# Relative change (total births adjustment)
fig2b <- ggplot(subset(mmr_change_tb, Adj == "perc_change_SB"), 
                aes(x = MMR, y = reorder(country_year, -country_order))) +
  geom_segment(aes(x = 0, xend = MMR, y = reorder(country_year, -country_order), yend = reorder(country_year, -country_order)), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "#1f77b4") +  
  labs(
    x = "Relative change in Maternal Mortality Ratio (MMR) (%)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 16), 
    axis.title.x = element_text(size = 14)
  )

fig2b

# Combine plots, sharing the y-axis
combined_fig2 <- fig2a + fig2b +
  plot_layout(ncol = 2)

combined_fig2

ggsave(here("gen", "Figure2-MMR-SB-only.png"), combined_fig2, width = 14, height = 10, bg = "white" )


# Figure 3 ----------------------------------------------------------------

# absolute and relative changes in prmr with total births adjustment

# Step 1: Order countries by the relative PRMR change from using total births 
prmr_change_tb <- prmr_change %>%
  group_by(country_year) %>%
  mutate(
    PRMR_change = PRMR[Adj == "perc_change_SB"],
    country_order = mean(PRMR_change, na.rm = TRUE)
  ) %>%
  filter(Adj == "abs_change_SB"| Adj == "perc_change_SB")%>%
  ungroup()

# Step 2

# Absolute change (total births adjustment)
fig3a <- ggplot(subset(prmr_change_tb, Adj == "abs_change_SB"), 
                aes(x = PRMR, y = reorder(country_year, -country_order))) +
  geom_segment(aes(x = 0, xend = PRMR, y = reorder(country_year, -country_order), yend = reorder(country_year, -country_order)), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "#1f77b4") +  
  labs(
    x = "Absolute change in Pregnancy-related Mortality Ratio (PRMR)",
    y = "Country and DHS Survey"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )

fig3a


# Relative change (total births adjustment)
fig3b <- ggplot(subset(prmr_change_tb, Adj == "perc_change_SB"), 
                aes(x = PRMR, y = reorder(country_year, -country_order))) +
  geom_segment(aes(x = 0, xend = PRMR, y = reorder(country_year, -country_order), yend = reorder(country_year, -country_order)), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "#1f77b4") +  
  labs(
    x = "Relative change in Pregnancy-related Mortality Ratio (PRMR) (%)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 16), 
    axis.title.x = element_text(size = 14)
  )

fig3b

# Combine plots, sharing the y-axis
combined_fig3 <- fig3a + fig3b +
  plot_layout(ncol = 2)

combined_fig3

ggsave(here("gen", "Figure3-PRMR_SB-only.png"), combined_fig3, width = 14, height = 10, bg = "white" )


###### OUTPUT FOR APPENDIX #######

# Table S1 ----------------------------------------------------------------

# DHS sample 

gt_tbl1 <- 
  sample %>%
  arrange(country_name) %>% 
  select(country_name, survey_year, phase, EverMW, 
         vcal_1, p32, IncludeMMR, IncludePRMR) %>% 
  gt(groupname_col = "country_name") %>%
  tab_header(title = "DHS data included") %>% 
  cols_label(
    country_name = md("Country"), 
    survey_year = md("Year"),
    phase = md("DHS Phase"),
    EverMW = md("Ever-married sample"),
    vcal_1 = md("Repro. calendar"),
    p32 = md("FPH"),
    IncludeMMR = md("Calculate MMR"),
    IncludePRMR = md("Calculate PRMR")
  ) %>% 
  fmt(
    columns = c(EverMW, vcal_1, p32, IncludeMMR, IncludePRMR),
    rows = everything(),
    fns = function(x) ifelse(x, "Yes", "No")  # Replace TRUE with ✔ and FALSE with ✘
  ) %>%
  tab_options(
    row_group.as_column = TRUE, 
    table.font.size = 14
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"), 
    locations = cells_column_labels()
  )

gt_tbl1
gtsave(gt_tbl1, here::here("gen", "table-1_sample.rtf"))



# Figure S1 ---------------------------------------------------------------

# pregnancy outcome proportions by FPH

adjfac_fph <- adjfac %>%
  pivot_longer(
    cols = c(AB_fph, LB_fph, SB_fph, MSC_fph),    
    names_to = "var",         
    values_to = "perwt"       
  )

adjfac_fph <- adjfac_fph %>%
  group_by(country_year, var) %>% 
  summarise(total_perwt = sum(perwt), .groups = "drop") %>% 
  group_by(country_year) %>% 
  mutate(proportion = total_perwt / sum(total_perwt)) %>% 
  filter(!is.na(proportion))

# Step 1: Ensure the global order of `var` (LB -> TRM -> SB)
adjfac_fph <- adjfac_fph %>% 
  mutate(var_clean = case_when(var == "LB_fph" ~ "Livebirth", 
                               var == "AB_fph" ~ "Abortion",
                               var == "MSC_fph" ~ "Miscarriage",
                               TRUE ~ "Stillbirth"))

adjfac_fph$var_clean <- factor(adjfac_fph$var_clean, levels = c("Livebirth",
                                                                "Miscarriage",
                                                                "Abortion",
                                                                "Stillbirth"))

# Step 2: Country order 
country_order <- adjfac_fph %>%
  group_by(country_year) %>%
  summarize(
    lb_score = sum(proportion[var == "LB_fph"]),
    msc_score = sum(proportion[var == "MSC_fph"])
  ) %>%
  arrange(desc(lb_score), desc(msc_score)) %>%
  pull(country_year)

adjfac_fph$country_year <- factor(
  adjfac_fph$country_year,
  levels = country_order
)

# Step 3: Create the plot
figS1 <- ggplot(adjfac_fph, aes(x = proportion, y = country_year, fill = var_clean)) +
  geom_bar(stat = "identity", position = "stack") +  
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +  # Convert x-axis to percentage
  labs(
    title = NULL,
    x = "Proportion",
    y = "Country and Survey Year",
    fill = "Outcome"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.position = "bottom"
  )
figS1

ggsave(here("gen", "FigureS1_preg-prop-fph.png"), figS1, bg = "white", width = 8, height = 6)

# Figure S2 ---------------------------------------------------------------

# absolute and relative changes in mmr with total pregnancies adjustment

# Step 1: Order countries by the size of relative MMR change using total pregnancies
mmr_change_tp <- mmr_change %>%
  group_by(country_year) %>%
  mutate(
    MMR_change = MMR[Adj == "perc_change_ALL"],
    country_order = mean(MMR_change, na.rm = TRUE) 
  ) %>%
  filter(Adj == "abs_change_ALL"| Adj == "perc_change_ALL")%>%
  ungroup()

# Step 2

# Absolute change (total pregnanciess adjustment)
figS2a <- ggplot(subset(mmr_change_tp, Adj == "abs_change_ALL"), 
                aes(x = MMR, y = reorder(country_year, -country_order))) +
  geom_segment(aes(x = 0, xend = MMR, y = reorder(country_year, -country_order), yend = reorder(country_year, -country_order)), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "#1f77b4") +  
  labs(
    x = "Absolute change in Maternal Mortality Ratio (MMR)",
    y = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

figS2a

# Relative change (total pregnancies adjustment)
figS2b <- ggplot(subset(mmr_change_tp, Adj == "perc_change_ALL"), 
                aes(x = MMR, y = reorder(country_year, -country_order))) +
  geom_segment(aes(x = 0, xend = MMR, y = reorder(country_year, -country_order), yend = reorder(country_year, -country_order)), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "#1f77b4") +  
  labs(
    x = "Relative change in Maternal Mortality Ratio (MMR) (%)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 14), 
    axis.title.x = element_text(size = 14)
  )

figS2b

# Combine plots, sharing the y-axis
combined_figS2 <- figS2a + figS2b +
  plot_layout(ncol = 2)

combined_figS2

ggsave(here("gen", "FigureS2-MMR-ALL.png"), combined_figS2, width = 14, height = 10, bg = "white" )


# Figure S3 ---------------------------------------------------------------

# absolute and relative changes in prmr with total pregnancies adjustment

# Step 1: Order countries by the size of relative MMR change using total pregnancies
prmr_change_tp <- prmr_change %>%
  group_by(country_year) %>%
  mutate(
    PRMR_change = PRMR[Adj == "perc_change_ALL"],
    country_order = mean(PRMR_change, na.rm = TRUE)
  ) %>%
  filter(Adj == "abs_change_ALL"| Adj == "perc_change_ALL")%>%
  ungroup()

# Step 2

# Absolute change (total pregnanciess adjustment)
figS3a <- ggplot(subset(prmr_change_tp, Adj == "abs_change_ALL"), 
                 aes(x = PRMR, y = reorder(country_year, -country_order))) +
  geom_segment(aes(x = 0, xend = PRMR, y = reorder(country_year, -country_order), yend = reorder(country_year, -country_order)), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "#1f77b4") +  
  labs(
    x = "Absolute change in Pregnancy-related Mortality Ratio (PRMR)",
    y = "Country"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

figS3a

# Relative change (total pregnancies adjustment)
figS3b <- ggplot(subset(prmr_change_tp, Adj == "perc_change_ALL"), 
                 aes(x = PRMR, y = reorder(country_year, -country_order))) +
  geom_segment(aes(x = 0, xend = PRMR, y = reorder(country_year, -country_order), yend = reorder(country_year, -country_order)), 
               arrow = arrow(length = unit(0.2, "cm")), 
               color = "#1f77b4") +  
  labs(
    x = "Relative change in Pregnancy-related Mortality Ratio (PRMR) (%)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 14), 
    axis.title.x = element_text(size = 14)
  )

figS3b

# Combine plots, sharing the y-axis
combined_figS3 <- figS3a + figS3b +
  plot_layout(ncol = 2)

combined_figS3

ggsave(here("gen", "FigureS3-PRMR-ALL.png"), combined_figS3, width = 14, height = 10, bg = "white" )

# ---------- Table S2 MMR ---------------------

# Original (unadjusted) values
mmr_livebirth <- subset(mmr, Adj == "noAdj")[, c("country_year", "MMR")] %>%
  rename(mmr_livebirth = MMR)
prmr_livebirth <- subset(prmr, Adj == "noAdj")[, c("country_year", "PRMR")] %>%
  rename(prmr_livebirth = PRMR)

# Adjusted (total births)
mmr_totalbirths <- subset(mmr, Adj == "adjSB")[, c("country_year", "MMR")] %>%
  rename(mmr_totalbirths = MMR)
mmr_diff_totalbirths <- subset(mmr_change_tb, Adj == "abs_change_SB")[, c("country_year", "MMR", "MMR_change")] %>%
  rename(mmr_abs_change_tb = MMR, mmr_rel_change_tb = MMR_change)

# Adjusted (total pregnancies)
mmr_totalpregnancies <- subset(mmr, Adj == "adjAll")[, c("country_year", "MMR")] %>%
  rename(mmr_totalpregnancies = MMR)
mmr_diff_totalpregnancies <- subset(mmr_change_tp, Adj == "abs_change_ALL")[, c("country_year", "MMR", "MMR_change")] %>%
  rename(mmr_abs_change_tp = MMR, mmr_rel_change_tp = MMR_change)

# Merge all total births data
mmr_tb <- reduce(list(mmr_livebirth, mmr_totalbirths, mmr_diff_totalbirths), left_join, by = "country_year")
mmr_tp <- reduce(list(mmr_totalpregnancies, mmr_diff_totalpregnancies), left_join, by = "country_year")
results_mmr <- left_join(mmr_tb, mmr_tp, by = "country_year")

# Format table
gt_tbl_mmr <- 
  results_mmr %>%
  arrange(country_year) %>%
  gt() %>%
  tab_header(title = "MMR using alternative denominators") %>% 
  cols_label(
    country_year = md("Country year"),
    mmr_livebirth = md("MMR (live births)"),
    mmr_totalbirths = md("MMR (total births)"),
    mmr_abs_change_tb = md("Absolute change in MMR with total birth denominator"),
    mmr_rel_change_tb = md("Relative change in MMR with total birth denominator (%)"),
    mmr_totalpregnancies = md("MMR (total pregnancies)"),
    mmr_abs_change_tp = md("Absolute change in MMR with total pregnancy denominator"),
    mmr_rel_change_tp = md("Relative change in MMR with total pregnancy denominator (%)")
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  fmt_number(
    columns = c(mmr_rel_change_tb, mmr_rel_change_tp),
    decimals = 1
  ) %>%
  tab_options(
    row_group.as_column = TRUE,
    table.font.size = 14
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

gt_tbl_mmr
gtsave(gt_tbl_mmr, here::here("gen", "table-s2_mmr.rtf"))

# ---------- Table S3 PRMR ---------------------

# Adjusted (total births)
prmr_totalbirths <- subset(prmr, Adj == "adjSB")[, c("country_year", "PRMR")] %>%
  rename(prmr_totalbirths = PRMR)
prmr_diff_totalbirths <- subset(prmr_change_tb, Adj == "abs_change_SB")[, c("country_year", "PRMR", "PRMR_change")] %>%
  rename(prmr_abs_change_tb = PRMR, prmr_rel_change_tb = PRMR_change)

# Adjusted (total pregnancies)
prmr_totalpregnancies <- subset(prmr, Adj == "adjAll")[, c("country_year", "PRMR")] %>%
  rename(prmr_totalpregnancies = PRMR)
prmr_diff_totalpregnancies <- subset(prmr_change_tp, Adj == "abs_change_ALL")[, c("country_year", "PRMR", "PRMR_change")] %>%
  rename(prmr_abs_change_tp = PRMR, prmr_rel_change_tp = PRMR_change)

# Merge all total births data
prmr_tb <- reduce(list(prmr_livebirth, prmr_totalbirths, prmr_diff_totalbirths), left_join, by = "country_year")
prmr_tp <- reduce(list(prmr_totalpregnancies, prmr_diff_totalpregnancies), left_join, by = "country_year")
results_prmr <- left_join(prmr_tb, prmr_tp, by = "country_year")

# Format table
gt_tbl_prmr <- 
  results_prmr %>%
  arrange(country_year) %>%
  gt() %>%
  tab_header(title = "PRMR using alternative denominators") %>% 
  cols_label(
    country_year = md("Country year"),
    prmr_livebirth = md("PRMR (live births)"),
    prmr_totalbirths = md("PRMR (total births)"),
    prmr_abs_change_tb = md("Absolute change in PRMR with total birth denominator"),
    prmr_rel_change_tb = md("Relative change in PRMR with total birth denominator (%)"),
    prmr_totalpregnancies = md("PRMR (total pregnancies)"),
    prmr_abs_change_tp = md("Absolute change in PRMR with total pregnancy denominator"),
    prmr_rel_change_tp = md("Relative change in PRMR with total pregnancy denominator (%)")
  ) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "-"
  ) %>%
  fmt_number(
    columns = c(prmr_rel_change_tb, prmr_rel_change_tp),
    decimals = 1
  ) %>%
  tab_options(
    row_group.as_column = TRUE,
    table.font.size = 14
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

gt_tbl_prmr
gtsave(gt_tbl_prmr, here::here("gen", "table-s3_prmr.rtf"))

# Figure S4 ---------------------------------------------------------------

# mmr and adjusted mmr with uncertainty intervals

# Plot impact of different denominator adjustments on MMR, with uncertainty
mmr_points <- subset(mmr, Adj == "adjSB"| Adj =="adjAll")[,c("country_year","MMR","Adj")]
mmr_points$LCI <- NA 
mmr_points$UCI <- NA
mmr_points_uncert <- mmr_uncert[,c("country_year","MMR","LCI","UCI")]
mmr_points_uncert$Adj <- "noAdj"
mmr_points <- rbind(mmr_points, mmr_points_uncert)

mmr_jittered <- mmr_points %>%
  mutate(jitter_y = case_when(
    Adj == 'adjSB' ~ -0.2,
    Adj == 'adjAll' ~ 0.2,
    TRUE ~ 0
  ))

figS4 <- ggplot(mmr_jittered, aes(x = MMR, y = country_year)) +
  geom_errorbar(
    data = mmr_jittered  %>% filter(Adj == 'noAdj'),
    aes(xmin = LCI, xmax = UCI, color = Adj),
    position = position_nudge(y = 0) # no nudge for noAdj
  ) +
  geom_point(
    data = mmr_jittered  %>% filter(Adj != 'noAdj'),
    aes(color = Adj),
    position = position_nudge(y = mmr_jittered$jitter_y[mmr_jittered$Adj != 'noAdj']),
    size = 4
  ) +
  geom_point(
    data = mmr_jittered %>% filter(Adj == 'noAdj'),
    aes(color = Adj),
    size = 4
  ) +
  scale_y_discrete(limits = rev(levels(factor(mmr_jittered$country_year)))) + 
  scale_colour_manual(breaks = c("noAdj", "adjSB", "adjAll"),
                      values = c("#1f77b4", "#2ca02c", "#ff7f0e"),
                      labels = c("Live births", "Total births (LB + SB)", "Total preg (LB + SB + MSC/AB)")) +  
  labs(
    title = NULL,
    x = "MMR per 100,000",
    y = NULL,
    colour = "Denominator"
  ) +
  theme_minimal() + 
  theme(
    axis.text.y = element_text(size = 14), 
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 14), 
    axis.title.x = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = "bottom"
  )

figS4

ggsave(here("gen", "FigureS4-MMR-Uncert.png"), figS4, width = 14, height = 10, bg = "white" )


# Figure S5 ---------------------------------------------------------------

# prmr and adjusted prmr with uncertainty intervals

# Plot impact of different denominator adjustments on PRMR, with uncertainty
prmr_points <- subset(prmr, Adj == "adjSB"| Adj =="adjAll")[,c("country_year","PRMR","Adj")]
prmr_points$LCI <- NA 
prmr_points$UCI <- NA
prmr_points_uncert <- prmr_uncert[,c("country_year","PRMR","LCI","UCI")]
prmr_points_uncert$Adj <- "noAdj"
prmr_points <- rbind(prmr_points, prmr_points_uncert)

prmr_jittered <- prmr_points %>%
  mutate(jitter_y = case_when(
    Adj == 'adjSB' ~ -0.2,
    Adj == 'adjAll' ~ 0.2,
    TRUE ~ 0
  ))

figS5 <- ggplot(prmr_jittered, aes(x = PRMR, y = country_year)) +
  geom_errorbar(
    data = prmr_jittered %>% filter(Adj == 'noAdj'),
    aes(xmin = LCI, xmax = UCI, color = Adj),
    position = position_nudge(y = 0)
  ) +
  geom_point(
    data = prmr_jittered %>% filter(Adj != 'noAdj'),
    aes(color = Adj), 
    position = position_nudge(y = prmr_jittered$jitter_y[prmr_jittered$Adj != 'noAdj']),
    size = 4
  ) +
  geom_point(
    data = prmr_jittered %>% filter(Adj == 'noAdj'),
    aes(color = Adj),
    size = 4
  ) +
  scale_y_discrete(limits = rev(levels(factor(prmr_jittered$country_year)))) + 
  scale_colour_manual(breaks = c("noAdj", "adjSB", "adjAll"),
                      values = c("#1f77b4", "#2ca02c", "#ff7f0e"),
                      labels = c("Live births", "Total births (LB + SB)", "Total preg (LB + SB + MSC/AB)")) +  
  labs(
    title = NULL,
    x = "PRMR per 100,000",
    y = NULL,
    colour = "Denominator"
  ) +
  theme_minimal() + 
  theme(
    axis.text.y = element_text(size = 14), 
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 14), 
    axis.title.x = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.position = "bottom"
  )

figS5

ggsave(here("gen", "FigureS5-PRMR-Uncert.png"), figS5, width = 14, height = 10, bg = "white" )







