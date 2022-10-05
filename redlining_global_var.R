# Global Redlining Information (methods, etc.)
# By Hannah De los Santos
# Originated on: 6/24/21

# load libraries ----

library(rgdal)
library(broom)
library(ggplot2)
library(raster)
library(rgeos)
library(tidycensus)
library(sf)
library(shiny)
library(reshape2)
library(grid)
library(gridExtra)
library(colorspace)
library(tigris)
library(scales)

# load global data ----

data_folder <- data_folder <- file.path(
  gsub("\\\\","/", gsub("OneDrive - ","", Sys.getenv("OneDrive"))), 
  "Health Equity MIP - Redlining Data")

# load HOLC data -- much faster to load it from RData
# holc_dat <- readOGR(
#   file.path(data_folder, "Redlining", "redlining_fullshpfile", "HOLC_Cities.gdb"),
#   "holc_ad_data")
load(file.path(data_folder, "Redlining", "HOLC_shpfile.RData")) 
# will result in holc_dat

# load us cities data (for mapping to counties for easy census download)
us_cities <- read.csv(file.path(data_folder, "US_cities.csv"))
# also load specific city mappings
specific_us_cities <- read.csv(file.path(data_folder, "Specific_US_cities.csv"))


# load centroids
centr_pop <- read.csv(
  file.path(
    data_folder,"CenPop2010_Means", "CenPop2010_Mean_US.csv"
  ),
  colClasses = c(rep("character",3), rep("numeric",3))
)
centr_pop$GEOID <- paste0(
  centr_pop$STATEFP,
  centr_pop$COUNTYFP,
  centr_pop$TRACTCE
)

# load PLACES data (for health outcome data)
places_df <- read.csv(
  file.path(data_folder, "PLACES", "CT",
            "PLACES__Census_Tract_Data__GIS_Friendly_Format___2020_release.csv"),
  colClasses = c("TractFIPS" = "character")
)
rownames(places_df) <- places_df$TractFIPS

# load life expectancy data
le_df <- read.csv(
  file.path(data_folder, "Life_Expectancy",
            "USALEEP_Life_Expectancy.CSV"),
  colClasses = c("Tract.ID" = "character")
)
rownames(le_df) <- le_df$Tract.ID

# global variables, general ----

holc_colors <- c(
  "A" = "#759e64", # green
  "B" = "#75a3b6", # blue
  "C" = "#c5c473", # yellow
  "D" = "#c27e8d" # red
)

holc_points <- c(
  "A" = 1, # green
  "B" = 2, # blue
  "C" = 3, # yellow
  "D" = 4 # red
)

# HOLC colors for lynch run from (.5 to 4)
holc_colors_lynch <- c(
  "U" = "#9b6ad4", # purple??
  "A" = "#759e64", # green
  "B" = "#75a3b6", # blue
  "C" = "#c5c473", # yellow
  "D" = "#c27e8d" # red
)

methods_avail <- c(
  "Nardone, et al.: Population weighted centroid" = "w_centroid",
  "Crossney and Bartelt: Proportion of area, filter by people and housing units" = "crossney",
  "NCRC: Proportion of area, 20% threshold" = "ncrc",
  "Krieger, et al.+: Majority of area, 50% threshold" = "krieger",
  "Wilson: Unweighted centroid" = "unw_centroid",
  "Li and Yuan: Highest graded area, 10% threshold" = "li",
  "Lynch, et al.: Total proportion of area, 50% threshold" = "lynch",
  "Lane, et al.: Plurality of area, 0% threshold" = "lane",
  "Lee, et al.: Plurality of area, 50% threshold" = "lee",
  "Mujahid, et al.: Rounded proportion of area, 0% threshold" = "mujahid",
  "Proportion of total area" = "prop_area",
  "Proportion of total population" = "prop_pop",
  # "Proportion of total area, including unclassified" = "weighted_score_w_not_gr",
  "Plurality of area rules" = "plurality_area",
  # "Proportion of total population, including unclassified" = "weighted_score_w_not_gr_pop",
  "Plurality of population rules" = "plurality_pop",
  "Rounded proportion of area" = "round_area",
  "Rounded proportion of population" = "round_pop"
)

methods_abbrev <- c(
  "unw_centroid" = "Wilson: Unweighted Centr.",
  "w_centroid"= "Nardone, et al.: Weighted Centr.",
  "crossney" = "Crossney, et al.: Prop. Area, filter by pop.",
  "ncrc" = "NCRC: Prop. Area, 20% threshold",
  "krieger" = "Krieger, et al.+: Majority Area, 50% threhsold",
  "li" = "Li and Yuan: Highest Graded Area, 10% threshold",
  "lynch" = "Lynch, et al.: Tot. Prop. Area, 50% threshold",
  "lane" = "Lane, et al.: Plurality Area, 0% threshold",
  "lee" = "Lee, et al.: Plurality Area, 50% threshold",
  "mujahid" = "Mujahid, et al.: Round Prop. Area, 0% threshold",
  "prop_area" = "Prop. Area",
  # "weighted_score_w_not_gr" = "Prop. Area, All",
  "plurality_area" = "Plurality Area",
  "prop_pop" = "Prop. Pop.",
  # "weighted_score_w_not_gr_pop" = "Prop. Pop, All",
  "plurality_pop" = "Plurality Pop.",
  "round_area" = "Round Prop. Area",
  "round_pop" = "Round Prop. Pop."
)

# global variables, analysis ----

methods_avail_analysis <- c(
  # paper methods ---
  "Nardone, et al.: Population weighted centroid" = "w_centroid",
  "Crossney and Bartelt: Proportion of area, filter by people and housing units" = "crossney",
  "NCRC: Proportion of area, 20% threshold" = "ncrc",
  "Krieger, et al.+: Majority of area, 50% threshold" = "krieger",
  "Wilson: Unweighted centroid" = "unw_centroid",
  "Li and Yuan: Highest graded area, 10% threshold" = "li",
  "Lynch, et al.: Total proportion of area, 50% threshold" = "lynch",
  "Lane, et al.: Plurality of area, 0% threshold" = "lane",
  "Lee, et al.: Plurality of area, 50% threshold" = "lee",
  "Mujahid, et al.: Rounded proportion of area, 0% threshold" = "mujahid",
  
  # extensions (no duplicates of methods) ---
 "Proportion of area, 0% threshold" = "prop_area_0thr",
 "Proportion of population, 0% threshold" = "prop_pop_0thr",
 "Proportion of area, 10% threshold" = "prop_area_10thr",
 "Proportion of population, 10% threshold" = "prop_pop_10thr",
 "Proportion of population, 20% threshold" = "prop_pop_20thr",
 "Proportion of area, 30% threshold" = "prop_area_30thr",
 "Proportion of population, 30% threshold" = "prop_pop_30thr",
 "Proportion of area, 40% threshold" = "prop_area_40thr",
 "Proportion of population, 40% threshold" = "prop_pop_40thr",
 "Proportion of area, 50% threshold" = "prop_area_50thr",
 "Proportion of population, 50% threshold" = "prop_pop_50thr",
 "Proportion of area, weighted" = "prop_area_wt",
 "Proportion of population, weighted" = "prop_pop_wt",
 
 "Plurality of population rules, 0% threshold" = "plurality_pop_0thr",
 "Plurality of area rules, 10% threshold" = "plurality_area_10thr",
 "Plurality of population rules, 10% threshold" = "plurality_pop_10thr",
 "Plurality of area rules, 20% threshold" = "plurality_area_20thr",
 "Plurality of population rules, 20% threshold" = "plurality_pop_20thr",
 "Plurality of area rules, 30% threshold" = "plurality_area_30thr",
 "Plurality of population rules, 30% threshold" = "plurality_pop_30thr",
 "Plurality of area rules, 40% threshold" = "plurality_area_40thr",
 "Plurality of population rules, 40% threshold" = "plurality_pop_40thr",
 "Plurality of population rules, 50% threshold" = "plurality_pop_50thr",
 "Plurality of area rules, weighted" = "plurality_area_wt",
 "Plurality of population rules, weighted" = "plurality_pop_wt",
 
 "Rounded proportion of population, 0% threshold" = "round_pop_0thr",
 "Rounded proportion of area, 10% threshold" = "round_area_10thr",
 "Rounded proportion of population, 10% threshold" = "round_pop_10thr",
 "Rounded proportion of area, 20% threshold" = "round_area_20thr",
 "Rounded proportion of population, 20% threshold" = "round_pop_20thr",
 "Rounded proportion of area, 30% threshold" = "round_area_30thr",
 "Rounded proportion of population, 30% threshold" = "round_pop_30thr",
 "Rounded proportion of area, 40% threshold" = "round_area_40thr",
 "Rounded proportion of population, 40% threshold" = "round_pop_40thr",
 "Rounded proportion of area, 50% threshold" = "round_area_50thr",
 "Rounded proportion of population, 50% threshold" = "round_pop_50thr",
 "Rounded proportion of area, weighted" = "round_area_wt",
 "Rounded proportion of population, weighted" = "round_pop_wt"
)

methods_abbrev_analysis <- c(
  # paper methods ---
  "Nardone, et al.: Weighted centr." = "w_centroid",
  "Crossney, et al.: Prop. area, pop. thr." = "crossney",
  "NCRC: Prop. area, 20% thr." = "ncrc",
  "Krieger, et al.+: Majority area, 50% thr." = "krieger",
  "Wilson: Unweighted centr." = "unw_centroid",
  "Li, et al.: Plurality area, 10% thr." = "li",
  "Lynch, et al.: Tot. prop. area, 50% thr." = "lynch",
  "Lane, et al.: Plurality area, 0% thr." = "lane",
  "Lee, et al.: Plurality area, 50% thr." = "lee",
  "Mujahid, et al.: Round prop. area, 0% thr." = "mujahid",
  
  # extensions (no duplicates of methods) ---
  "Prop. area, 0% thr." = "prop_area_0thr",
  "Prop. pop., 0% thr." = "prop_pop_0thr",
  "Prop. area, 10% thr." = "prop_area_10thr",
  "Prop. pop., 10% thr." = "prop_pop_10thr",
  "Prop. pop., 20% thr." = "prop_pop_20thr",
  "Prop. area, 30% thr." = "prop_area_30thr",
  "Prop. pop., 30% thr." = "prop_pop_30thr",
  "Prop. area, 40% thr." = "prop_area_40thr",
  "Prop. pop., 40% thr." = "prop_pop_40thr",
  "Prop. area, 50% thr." = "prop_area_50thr",
  "Prop. pop., 50% thr." = "prop_pop_50thr",
  "Prop. area, weighted" = "prop_area_wt",
  "Prop. pop., weighted" = "prop_pop_wt",
  
  "Plurality area, 0% thr." = "plurality_area_0thr",
  "Plurality pop., 0% thr." = "plurality_pop_0thr",
  "Plurality area, 10% thr." = "plurality_area_10thr",
  "Plurality pop., 10% thr." = "plurality_pop_10thr",
  "Plurality area, 20% thr." = "plurality_area_20thr",
  "Plurality pop., 20% thr." = "plurality_pop_20thr",
  "Plurality area, 30% thr." = "plurality_area_30thr",
  "Plurality pop., 30% thr." = "plurality_pop_30thr",
  "Plurality area, 40% thr." = "plurality_area_40thr",
  "Plurality pop., 40% thr." = "plurality_pop_40thr",
  "Plurality pop., 50% thr." = "plurality_pop_50thr",
  "Plurality area, weighted" = "plurality_area_wt",
  "Plurality pop., weighted" = "plurality_pop_wt",
  
  "Round prop. pop., 0% thr." = "round_pop_0thr",
  "Round prop. area, 10% thr." = "round_area_10thr",
  "Round prop. pop., 10% thr." = "round_pop_10thr",
  "Round prop. area, 20% thr." = "round_area_20thr",
  "Round prop. pop., 20% thr." = "round_pop_20thr",
  "Round prop. area, 30% thr." = "round_area_30thr",
  "Round prop. pop., 30% thr." = "round_pop_30thr",
  "Round prop. area, 40% thr." = "round_area_40thr",
  "Round prop. pop., 40% thr." = "round_pop_40thr",
  "Round prop. area, 50% thr." = "round_area_50thr",
  "Round prop. pop., 50% thr." = "round_pop_50thr",
  "Round prop. area, weighted" = "round_area_wt",
  "Round prop. pop., weighted" = "round_pop_wt"
)
methods_abbrev_analysis <- 
  setNames(names(methods_abbrev_analysis), methods_abbrev_analysis)

# continuous v discrete
methods_type <- setNames(
  rep("Discrete", length(methods_avail_analysis)),
  methods_avail_analysis
)
methods_type[grepl("proportion", tolower(names(methods_avail_analysis)))] <-
  "Continuous"
methods_type[grepl("round", tolower(names(methods_avail_analysis)))] <-
  "Discrete"

# threshold v weight v none
methods_cutoff <- setNames(
  rep("Threshold", length(methods_avail_analysis)),
  methods_avail_analysis
)
methods_cutoff[grepl("centroid", tolower(names(methods_avail_analysis)))] <-
  "Centroid"
methods_cutoff[grepl(", weighted", names(methods_avail_analysis))] <-
  "Weighted"

# area v population
methods_contribution <- setNames(
  rep("Area", length(methods_avail_analysis)),
  methods_avail_analysis
)
methods_contribution[grepl("population", tolower(names(methods_avail_analysis)))] <-
  "Population"

# mixed classifications:
# [0, 1] = NA (Not Graded)
# (1, 25] - Very Mixed
# (25, 50] - Mixed
# (50, 75] - Moderately Mixed
# (75, 99] - Not Very Mixed
# (99, 100] - Not Mixed
mixed_class <- c(
  "End" = 0,
  "Not Graded" = 1,
  "Very Mixed" = 25,
  "Mixed" = 50,
  "Moderately Mixed" = 75,
  "Not Very Mixed" = 99,
  "Not Mixed" = 100
)

# global variables, app ----

paper_avail <- c(
  "Nardone, et al.: Population weighted centroid" = "w_centroid",
  "Crossney and Bartelt: Proportion of area, filter by people and housing units" = "crossney",
  "NCRC: Proportion of area, 20% threshold" = "ncrc",
  "Krieger, et al.+: Majority of area, 50% threshold" = "krieger",
  "Wilson: Unweighted centroid" = "unw_centroid",
  "Li and Yuan: Highest graded area, 10% threshold" = "li",
  "Lynch, et al.: Total proportion of area, 50% threshold" = "lynch",
  "Lane, et al.: Plurality of area, 0% threshold" = "lane",
  "Lee, et al.: Plurality of area, 50% threshold" = "lee",
  "Mujahid, et al.: Rounded proportion of area, 0% threshold" = "mujahid"
)

type_avail <- c(
  "Proportion of" = "prop", 
  "Plurality of" = "plurality", 
  "Rounded Proportion of" = "round",
  "Centroid" = "centroid"
)

contribution_avail <- c(
  "Area" = "area",
  "Population" = "pop"
)

cutoff_avail <- c(
  "Threshold" = "thr",
  "Weighting" = "wt",
  "Centroid" = ""
)

cutoff_num_avail <-
  setNames(
    c("", seq(0,50, by = 10)),
    c("N/A",
      paste0(seq(0,50, by = 10), "%")
    )
  )
  
specific_method_map <- c(
  "prop_area_20thr" = "ncrc",
  "plurality_area_0thr" = "lane",
  "plurality_area_50thr" = "lee",
  "round_area_0thr" = "lane"
)

outcome_map <- c(
  "Life Expectancy" = "le",
  "Physical Health" = "pe",
  "Mental Health" = "mh"
)
