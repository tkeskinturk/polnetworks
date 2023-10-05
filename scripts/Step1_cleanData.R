
# - The Organization of Political Belief Networks: A Cross-Country Analysis ----------------------------------- #
# - Turgut Keskinturk - Bogazici University - 2022 ------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------- #

### section 1: load and clean data ###

# - PART 01 --------------------------------------------------------------------------------------------------- #

# load the necessary packages

rm(list = ls())
pacman::p_load(readstata13, tidyverse, boot, parallel, reshape2, lme4, pls, # use the same R session for all scripts
               ggrepel, gridExtra, qgraph, igraph, bootnet, BMA, psych, sjPlot, hrbrthemes, RColorBrewer)

# - PART 02 --------------------------------------------------------------------------------------------------- #

# load the main data file

## data cleaning
df <- read.dta13("./data/WVS_EVS_Joint2017.dta", convert.factors = FALSE) %>%
  subset(select = c(
    cntry, A124_02, A124_06, B008, C001, C002, D059, D060, D061, D078, 
    E033, E035, E036, E037, E039, E114, E115, E117, E224, E225, E226, E227, E229, 
    E233, E233A, E233B, E235, F120, F144_02, G052, G007_35_B, G007_36_B, 
    E069_01, E069_02, E069_04, E069_05, E069_06, E069_07, E069_08, E069_18A, 
    E069_18, E069_11, E069_12, E069_13, E069_14, E069_17, E069_20
  )) %>% # N = 135.000
  mutate(
    B008 = ifelse(B008 == 3, NA_real_, B008),
    C001 = recode(C001, "1" = 3, "2" = 1, "3" = 2), 
    C002 = recode(C002, "1" = 3, "2" = 1, "3" = 2),
    E224 = ifelse(E224 == 0, NA_real_, E224), 
    E225 = ifelse(E225 == 0, NA_real_, E225),
    E226 = ifelse(E226 == 0, NA_real_, E226), 
    E227 = ifelse(E227 == 0, NA_real_, E227),
    E229 = ifelse(E229 == 0, NA_real_, E229), 
    E233 = ifelse(E233 == 0, NA_real_, E233), 
    E233A = ifelse(E233A == 0, NA_real_, E233A), 
    E233B = ifelse(E233B == 0, NA_real_, E233B))

## scale construction
items.conf <- c("E069_01", "E069_02", "E069_04", "E069_05", "E069_06", "E069_07", "E069_08", "E069_18A", 
                "E069_18", "E069_11", "E069_12", "E069_13", "E069_14", "E069_17", "E069_20")

df <- df %>% mutate(
  conf = rowMeans(df[, items.conf], na.rm = TRUE)
  ) %>% 
  subset(select = -c(
    E069_01, E069_02, E069_04, E069_05, E069_06, E069_07, E069_08, E069_18A, 
    E069_18, E069_11, E069_12, E069_13, E069_14, E069_17, E069_20
  )) # 32 items, one of which is a composite scale (treated as interval variable later)

# - PART 03 --------------------------------------------------------------------------------------------------- #

# macro-files

macro_issue <- read.csv("./data/addsheets_issuemacro.csv", na.strings = ".") # issue labels
issues <- unique(macro_issue$var) # vector of unique issues
macro_country <- read.csv("./data/addsheets_countrymacro.csv", na.strings = ".") %>% # country labels
  mutate(d_mode = factor(d_mode, levels = c("PAPI", "CAPI", "Others")),
         d_lang = factor(d_lang, levels = c("Single", "Multiple")))
countries <- unique(df$cntry) # vector of unique countries

rm(items.conf) # clean-up for later scripts

# ------------------------------------------------------------------------------------------------------------- #
