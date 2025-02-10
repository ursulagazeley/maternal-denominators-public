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
################################################################################

# List of datasets to process
datasets <- list(mmr, prmr, adjfac, sample, mmr_uncert, prmr_uncert)

# Apply the processing and filtering function to each dataset
datasets <- lapply(datasets, process_and_filter_recent)

mmr <- datasets[[1]]
prmr <- datasets[[2]]
adjfac <- datasets[[3]]
sample <- datasets[[4]]
mmr_uncert <- datasets[[5]]
prmr_uncert <- datasets[[6]]

# Summary stats for main text ---------------------------------------------

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
gtsave(gt_tbl1, here::here("gen", "table-1_sample.pdf"))


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


# Figure S1 ---------------------------------------------------------------

# pregnancy outcome proportions by age group

adjfac_age <- adjfac_long %>%
  group_by(agegrp, var) %>% 
  summarise(total_perwt = sum(perwt), .groups = "drop") %>% 
  group_by(agegrp) %>% 
  mutate(proportion = total_perwt / sum(total_perwt)) %>% 
  ungroup()

agegrp_order <- c("<15", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

summary_age <- adjfac_age %>%
  mutate(agegrp = factor(agegrp, levels = agegrp_order),
         var = case_when(var == "LB"~ "Livebirth", 
                         var == "SB" ~ "Stillbirth",
                         TRUE ~ "Termination"))

figS1 <-  ggplot(summary_age, aes(x = proportion, y = agegrp, fill = var)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) + 
  labs(
    title = NULL,
    x = "Proportion",
    y = "Age Group",
    fill = "Outcome"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )
figS1
ggsave(here("gen", "FigureS1_preg-prop-age.png"), figS1, height = 6, width = 6, bg = "white")

# Figure S2 ---------------------------------------------------------------

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
figS2 <- ggplot(adjfac_fph, aes(x = proportion, y = country_year, fill = var_clean)) +
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
figS2

ggsave(here("gen", "FigureS2_preg-prop-fph.png"), figS2, bg = "white", width = 8, height = 6)

# Figure 2 ----------------------------------------------------------------

# absolute and relative changes in mmr with total births adjustment

# Step 1: Order countries by the size of relative MMR change using total births 
mmr_change <- mmr_change %>%
  group_by(country_year) %>%
  mutate(
    MMR_change = MMR[Adj == "perc_change_SB"],
    country_order = mean(MMR_change, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2

# Absolute change (total births adjustment)
fig2a <- ggplot(subset(mmr_change, Adj == "abs_change_SB"), 
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
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

fig2a

# Relative change (total births adjustment)
fig2b <- ggplot(subset(mmr_change, Adj == "perc_change_SB"), 
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

fig2b

# Combine plots, sharing the y-axis
combined_fig2 <- fig2a + fig2b +
  plot_layout(ncol = 2)

combined_fig2

ggsave(here("gen", "Figure2-MMR-SB-only.png"), combined_fig2, width = 14, height = 10, bg = "white" )


# Figure 3 ----------------------------------------------------------------

# absolute and relative changes in prmr with total births adjustment

# Step 1: Order countries by the relative PRMR change from using total births 
prmr_change <- prmr_change %>%
  group_by(country_year) %>%
  mutate(
    prmr_change = PRMR[Adj == "perc_change_SB"],
    country_order = mean(prmr_change, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2

# Absolute change (total births adjustment)
fig3a <- ggplot(subset(prmr_change, Adj == "abs_change_SB"), 
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
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

fig3a


# Relative change (total births adjustment)
fig3b <- ggplot(subset(prmr_change, Adj == "perc_change_SB"), 
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

fig3b

# Combine plots, sharing the y-axis
combined_fig3 <- fig3a + fig3b +
  plot_layout(ncol = 2)

combined_fig3

ggsave(here("gen", "Figure3-PRMR_adj.png"), combined_fig3, width = 14, height = 10, bg = "white" )


# Figure S3 ---------------------------------------------------------------

# absolute and relative changes in mmr with total pregnancies adjustment

# Step 1: Order countries by the size of relative MMR change using total pregnancies
mmr_change <- mmr_change %>%
  group_by(country_year) %>%
  mutate(
    MMR_change = MMR[Adj == "perc_change_ALL"],
    country_order = mean(MMR_change, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2

# Absolute change (total pregnanciess adjustment)
figS3a <- ggplot(subset(mmr_change, Adj == "abs_change_ALL"), 
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

figS3a

# Relative change (total pregnancies adjustment)
figS3b <- ggplot(subset(mmr_change, Adj == "perc_change_ALL"), 
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

figS3b

# Combine plots, sharing the y-axis
combined_figS3 <- figS3a + figS3b +
  plot_layout(ncol = 2)

combined_figS3

ggsave(here("gen", "FigureS3-MMR-ALL.png"), combined_figS3, width = 14, height = 10, bg = "white" )


# Figure S4 ---------------------------------------------------------------

# absolute and relative changes in prmr with total pregnancies adjustment

# Step 1: Order countries by the size of relative MMR change using total pregnancies
prmr_change <- prmr_change %>%
  group_by(country_year) %>%
  mutate(
    PRMR_change = PRMR[Adj == "perc_change_ALL"],
    country_order = mean(PRMR_change, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 2

# Absolute change (total pregnanciess adjustment)
figS4a <- ggplot(subset(prmr_change, Adj == "abs_change_ALL"), 
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

figS4a

# Relative change (total pregnancies adjustment)
figS4b <- ggplot(subset(prmr_change, Adj == "perc_change_ALL"), 
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

figS4b

# Combine plots, sharing the y-axis
combined_figS4 <- figS4a + figS4b +
  plot_layout(ncol = 2)

combined_figS4

ggsave(here("gen", "FigureS4-PRMR-ALL.png"), combined_figS4, width = 14, height = 10, bg = "white" )


# Figure S5 ---------------------------------------------------------------

# mmr and adjusted mmr with uncertainty intervals

# Plot impact of different denominator adjustments on MMR, with uncertainty
mmr_points <- subset(mmr, Adj == "adjSB"| Adj =="adjAll")[,c("country_year","MMR","Adj")]
mmr_points$LCI <- NA 
mmr_points$UCI <- NA
mmr_points_uncert <- mmr_uncert[,c("country_year","MMR","LCI","UCI")]
mmr_points_uncert$Adj <- "noAdj"
mmr_points <- rbind(mmr_points, mmr_points_uncert)

figS5 <- mmr_points %>%
  ggplot(aes(x = MMR, y = country_year, colour = Adj)) +
  geom_point(size = 4) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI)) +
  scale_y_discrete(limits = rev(levels(factor(mmr$country_year)))) + 
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

figS5

ggsave(here("gen", "FigureS5-MMR-Uncert.png"), figS5, width = 14, height = 10, bg = "white" )


# Figure S6 ---------------------------------------------------------------

# prmr and adjusted prmr with uncertainty intervals

# Plot impact of different denominator adjustments on PRMR, with uncertainty
prmr_points <- subset(prmr, Adj == "adjSB"| Adj =="adjAll")[,c("country_year","PRMR","Adj")]
prmr_points$LCI <- NA 
prmr_points$UCI <- NA
prmr_points_uncert <- prmr_uncert[,c("country_year","PRMR","LCI","UCI")]
prmr_points_uncert$Adj <- "noAdj"
prmr_points <- rbind(prmr_points, prmr_points_uncert)

figS6 <- prmr_points %>%
  ggplot(aes(x = PRMR, y = country_year, colour = Adj)) +
  geom_point(size = 4) +
  geom_errorbar(aes(xmin = LCI, xmax = UCI)) +
  scale_y_discrete(limits = rev(levels(factor(prmr$country_year)))) + 
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

figS6

ggsave(here("gen", "FigureS6-PRMR-Uncert.png"), figS6, width = 14, height = 10, bg = "white" )







