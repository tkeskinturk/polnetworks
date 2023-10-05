
# - The Organization of Political Belief Networks: A Cross-Country Analysis ----------------------------------- #
# - Turgut Keskinturk - Bogazici University - 2022 ------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------- #

### section 2: estimate networks and construct data files ###

# - PART 01 --------------------------------------------------------------------------------------------------- #

# construct adjacency matrices

df.mat <- list()
loopfr <- 1 # our loop friend

for (c in countries) {
    print(c)
    df.cnty <- df %>% 
      filter(cntry == c) %>% subset(select = -c(cntry)) # subset for each country
    df.cnty <- df.cnty[, colSums(is.na(df.cnty)) < nrow(df.cnty)] # drop fully missing variables
    adj.mat <- suppressWarnings(cor_auto(df.cnty,
                                         npn.SKEPTIC = T,
                                         ordinalLevelMax = 7,
                                         forcePD = TRUE,
                                         missing = "pairwise",
                                         verbose = FALSE))
    df.mat[[loopfr]] <- adj.mat^2 # soft-thresholding to reduce the effects of non-significant correlations
    names(df.mat)[loopfr] <- c # give names to list elements with country codes
    rm(df.cnty, adj.mat); loopfr <- loopfr+1
}
rm(c, loopfr) # clean-up

# - PART 02 --------------------------------------------------------------------------------------------------- #

# construct edge-list data frame

df.cor <- list()
loopfr <- 1 # our loop friend

for (c in countries) {
  print(c)
  adj.mat <- df.mat[[as.character(c)]]
  adj.cor <- data.frame(x=rownames(adj.mat)[row(adj.mat)[upper.tri(adj.mat)]], 
                        y=colnames(adj.mat)[col(adj.mat)[upper.tri(adj.mat)]], 
                        c=adj.mat[upper.tri(adj.mat)])
  df.cor[[loopfr]] <- adj.cor
  names(df.cor)[loopfr] <- c # give names to list elements with country codes
  rm(adj.mat, adj.cor); loopfr <- loopfr+1
}
rm(c, loopfr) # clean-up

# - PART 03 --------------------------------------------------------------------------------------------------- #

# construct bootstrapped adjacency matrices

df.bts <- list()
loopfr <- 1 # our loop friend

for (c in countries) {
  print(c)
  df.cnty <- df %>% 
    filter(cntry == c) %>% subset(select = -c(cntry)) # subset for each country
  df.cnty <- df.cnty[, colSums(is.na(df.cnty)) < nrow(df.cnty)] # drop fully missing variables
  df.boot <- bootnet(df.cnty, nBoots = 1000, nCores = 6,
                     default = "cor", type = "nonparametric", model = "GGM",
                     corMethod = "cor_auto", corArgs = list(npn.SKEPTIC = TRUE, forcePD = TRUE),
                     statistics = "edge", computeCentrality = F)
  df.bts[[loopfr]] <- df.boot # store 1000 bootstrapped gaussian network data for each country
  names(df.bts)[loopfr] <- c # give names to list elements with country codes
  rm(df.cnty, adj.mat); loopfr <- loopfr+1
}
rm(c, loopfr, df.boot) # clean-up

save(df.bts, file = "./data/bootstrapped_graphs.saved") # personal save for later

# ------------------------------------------------------------------------------------------------------------- #
