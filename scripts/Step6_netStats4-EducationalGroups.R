
# - The Organization of Political Belief Networks: A Cross-Country Analysis ----------------------------------- #
# - Turgut Keskinturk - Bogazici University - 2022 ------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------- #

### section 6: adjudicating different educational groups ###

# - PART 01 --------------------------------------------------------------------------------------------------- #

# let's group individuals according to their educational status

df_educ <- read.dta13("./data/WVS_EVS_Joint2017.dta", convert.factors = FALSE) %>%
  subset(select = c(cntry, X025R, X025A_01)) %>%
  mutate(X025R = ifelse(cntry == 840 & X025A_01 == 3, 1, X025R))
df_educ <- df_educ %>% mutate(
  X025R = recode(X025R, "-5" = NA_real_, "-2" = NA_real_, "-1" = NA_real_,
                 "1" = 1, "2" = 2, "3" = 3)) %>%
  subset(select = -c(cntry, X025A_01))
df$educ <- df_educ$X025R # add the education variable to the main dataset

df_H <- df %>% filter(educ == 3) %>% subset(select = -c(educ)) # educational group = high
df_L <- df %>% filter(educ == 1) %>% subset(select = -c(educ)) # educational group = low

# - PART 02 --------------------------------------------------------------------------------------------------- #

# calculate the correlation matrices for each group

## group H
df.mat.H <- list()
loopfr <- 1 # our loop friend

for (c in countries) {
  print(c)
  df.cnty <- df_H %>% 
    filter(cntry == c) %>% subset(select = -c(cntry)) # subset for each country
  df.cnty <- df.cnty[, colSums(is.na(df.cnty)) < nrow(df.cnty)] # drop fully missing variables
  adj.mat <- suppressWarnings(cor_auto(df.cnty,
                                       npn.SKEPTIC = T,
                                       ordinalLevelMax = 7,
                                       forcePD = TRUE,
                                       missing = "pairwise",
                                       verbose = FALSE))  
  df.mat.H[[loopfr]] <- adj.mat^2 # soft-thresholding to reduce the effects of non-significant correlations
  names(df.mat.H)[loopfr] <- c # give names to list elements with country codes
  rm(df.cnty, adj.mat); loopfr <- loopfr+1
}
rm(c, loopfr) # clean-up

## group L
df.mat.L <- list()
loopfr <- 1 # our loop friend

for (c in countries) {
  print(c)
  df.cnty <- df_L %>% 
    filter(cntry == c) %>% subset(select = -c(cntry)) # subset for each country
  df.cnty <- df.cnty[, colSums(is.na(df.cnty)) < nrow(df.cnty)] # drop fully missing variables
  adj.mat <- suppressWarnings(cor_auto(df.cnty,
                                       npn.SKEPTIC = T,
                                       ordinalLevelMax = 7,
                                       forcePD = TRUE,
                                       missing = "pairwise",
                                       verbose = FALSE))  
  df.mat.L[[loopfr]] <- adj.mat^2 # soft-thresholding to reduce the effects of non-significant correlations
  names(df.mat.L)[loopfr] <- c # give names to list elements with country codes
  rm(df.cnty, adj.mat); loopfr <- loopfr+1
}
rm(c, loopfr) # clean-up

# - PART 03 --------------------------------------------------------------------------------------------------- #

# prepare the country-level constraint data frames

## group H
nt_structures.H <- as.data.frame(matrix(NA, nrow = length(countries), ncol = 2))
nt_structures.H[, 1] <- countries

for (i in sort(countries)) {
  print(i)
  
  # networks
  g_adjmat <- df.mat.H[as.character(i)][[1]] # adjacency matrices
  g_qgraph <- qgraph(g_adjmat, DoNotPlot = TRUE, diag = FALSE) # qgraph object
  g_igraph <- as.igraph(g_qgraph, attributes = TRUE) # igraph object
  
  nt_structures.H[nt_structures.H$V1 == i, 2] = 
    2*sum((E(g_igraph)$weight)) / ((vcount(g_igraph)*(vcount(g_igraph)-1)))
}
rm(i, g_adjmat, g_qgraph, g_igraph) # clean-up

## group L
nt_structures.L <- as.data.frame(matrix(NA, nrow = length(countries), ncol = 2))
nt_structures.L[, 1] <- countries

for (i in sort(countries)) {
  print(i)
  
  # networks
  g_adjmat <- df.mat.L[as.character(i)][[1]] # adjacency matrices
  g_qgraph <- qgraph(g_adjmat, DoNotPlot = TRUE, diag = FALSE) # qgraph object
  g_igraph <- as.igraph(g_qgraph, attributes = TRUE) # igraph object
  
  nt_structures.L[nt_structures.L$V1 == i, 2] = 
    2*sum((E(g_igraph)$weight)) / ((vcount(g_igraph)*(vcount(g_igraph)-1)))
}
rm(i, g_adjmat, g_qgraph, g_igraph) # clean-up

# - PART 04 --------------------------------------------------------------------------------------------------- #

# compare the within-country differences across the groups

## t-test of differences
nt_structures_educ <- inner_join(nt_structures.L, nt_structures.H, by = "V1")
t.test(nt_structures_educ$V2.x, nt_structures_educ$V2.y, paired = T, var.equal = T) # t = -3.72 & p < 0.001

## prepare the main data frame
nt_structures_educ <- nt_structures_educ %>% dplyr::rename(cnty = V1, denL = V2.x, denH = V2.y) %>% 
  left_join(macro_country, by = "cnty") %>% na.omit()
nt_structures_educ$partyinst <- rowMeans(nt_structures_educ[, partyinst_var])
nt_structures_educ <- nt_structures_educ %>% mutate_at(c(
  "partyinst", "educ", "polact", "civic", "nparty", "parorg", "parbra", "parlink", "parplat", "parcoh", 
  "polar", "media", "ethnic", "relig"), 
  scale.items) %>% # scale the items such that each is standardized & centralized
  mutate(d_polid = 1 - d_polid) # countries in which the question of political ideology is asked

# - PART 05 --------------------------------------------------------------------------------------------------- #

# regression analyses

## separate groups
fit.edH <- lm(scale.items(denH) ~ partyinst + polact + d_polid + d_mode + d_size + d_lang + educ,
              data = nt_structures_educ)
fit.edL <- lm(scale.items(denL) ~ partyinst + polact + d_polid + d_mode + d_size + d_lang + educ,
              data = nt_structures_educ)

tab_model(fit.edH, fit.edL,
          collapse.se = TRUE, digits = 3, p.style = "stars", show.ci = F,
          robust = TRUE, vcov.type = "HC3") # robust standard errors

## pooled groups
nt_structures_educ_M <- nt_structures_educ %>%
  subset(select = c(denH, denL, cnty)) %>% gather(key = "group", value, -cnty) %>%
  mutate(cnty = as.integer(cnty)) %>%
  left_join(nt_structures_educ, by = "cnty")

fit.edT <- lm(scale.items(value) ~ 
                partyinst + polact + d_polid + d_mode + d_size + d_lang + educ +
                factor(group),
              data = nt_structures_educ_M)

tab_model(fit.edT,
          collapse.se = TRUE, digits = 3, p.style = "stars", show.ci = F,
          vcov.fun = "vcovCL", vcov.type = "HC1", 
          vcov.args = list(cluster = nt_structures_educ_M$cnty)) # robust standard errors

# ------------------------------------------------------------------------------------------------------------- #
