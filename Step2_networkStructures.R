
# ----------------- The Organization of Political Belief Networks: A Cross-Country Analysis ------------------- #
# ------------------------------ Turgut Keskintürk - Bogaziçi University - 2021 ------------------------------- #

# ------------------------------------------------------------------------------------------------------------- #

### section 2: computes belief network properties and visualizes the distributions ###

# load the necessary packages

library(qgraph); library(igraph); library(bootnet); library(WGCNA); library(psych)

# ------------------------------------------------------ PART 01 ---------------------------------------------- #

# collect the correlations for each country, regularize them, and store adjacency matrices in a list

df.adj <- list()
loopfr <- 1 # our loop friend

for (c in countries) {
  print(c)
  df.cnty <- df %>% filter(cntry == c) %>% subset(select = -c(cntry, uniqid))
  df.cnty <- df.cnty[,colSums(is.na(df.cnty))<nrow(df.cnty)]
  
  adj.mat <- cor(df.cnty, use = "pairwise.complete.obs")
  adj.net <- EBICglasso(adj.mat, n = nrow(df.cnty), verbose = FALSE, lambda.min.ratio = 0.001)
  df.adj[[loopfr]] <- abs(adj.net) # put each adjacency matrix to the list
  names(df.adj)[loopfr] <- c # give names to list elements with country codes
  
  rm(df.cnty, adj.mat, adj.net); loopfr <- loopfr+1
}
rm(c, loopfr) # clean-up

# ------------------------------------------------------ PART 02 ---------------------------------------------- #

# let's compute network properties for each belief network

nt_structures <- as.data.frame(matrix(NA, nrow = length(countries), ncol = 6))
nt_structures[, 1] <- countries

for (i in sort(countries)) {
  print(i)
  
  # networks
  g_adjmat <- df.adj[as.character(i)][[1]] # adjacency matrices
  g_qgraph <- qgraph(g_adjmat, DoNotPlot = TRUE, diag = FALSE) # qgraph object
  g_igraph <- as.igraph(g_qgraph, attributes = TRUE) # igraph object
  
  # communities
  g_commun <- cluster_fast_greedy(g_igraph,
                                  weights = E(g_igraph)$weight) # clustering algorithm
  clusters <- g_commun$membership # community memberships
  V(g_igraph)$com = clusters # community memberships for igraph
  
  con <- intramodularConnectivity.fromExpr(g_adjmat, 
                                           colors = clusters, # connectivity measures 
                                           power = 1,
                                           getWholeNetworkConnectivity = TRUE,
                                           networkType = "unsigned")
  
  nt_structures[nt_structures$V1 == i, 2] = mean(con[, 1]) # mean total connectivity
  nt_structures[nt_structures$V1 == i, 3] = mean(con[, 2]) # mean intra connectivity
  nt_structures[nt_structures$V1 == i, 4] = mean(con[, 3]) # mean outer connectivity
  nt_structures[nt_structures$V1 == i, 5] = modularity(g_igraph, V(g_igraph)$com, weights = E(g_igraph)$weight)
  nt_structures[nt_structures$V1 == i, 6] = length(unique(g_commun$membership))
  
}
rm(i, g_adjmat, g_qgraph, g_igraph, g_commun, clusters, con) # clean-up

nt_structures <- nt_structures %>%
  dplyr::rename(cnty = V1, c.total = V2, c.intra = V3, c.extra = V4, modularity = V5, n.clust = V6) %>% 
  left_join(macro_country, by = "cnty")

# ------------------------------------------------------ PART 03 ---------------------------------------------- #

library(ggthemr) # set new ggplot theme
ggthemr("fresh")

# visualize the network properties

png("FIG01.png", w = 10, h = 10, units = "in", res = 1000)
nt_structures %>%
  subset(select = c(c.total, c.intra, c.extra, modularity)) %>%
  rename("Total Connectivity" = "c.total",
         "Intra-Modular Connectivity" = "c.intra",
         "Extra-Modular Connectivity" = "c.extra",
         "Modularity" = "modularity") %>%
  psych::pairs.panels(method = "pearson", hist.col = "#00AFBB", density = TRUE,
                      scale = FALSE, smooth = FALSE, ellipses = FALSE)
dev.off()

# ------------------------------------------------------ PART 04 ---------------------------------------------- #

# finding network structures through issue-pairs

nd_structures <- as.data.frame(expand.grid(countries, issues)) %>%
  rename(V1 = Var1, V2 = Var2) %>%
  filter(V2 != "E033")
nd_structures$V3 <- NA # empty columns for connectivity results
nd_structures$V4 <- NA
nd_structures$V5 <- NA

for (i in sort(countries)) {
  print(i)
  
  # networks
  g_adjmat <- ((df.adj[as.character(i)])[[1]]) # adjacency matrices
  g_qgraph <- qgraph(g_adjmat, DoNotPlot = TRUE) # qgraph object
  g_igraph <- as.igraph(g_qgraph, attributes = TRUE) # igraph object
  
  # communities
  g_commun <- cluster_fast_greedy(g_igraph,
                                  weights = E(g_igraph)$weight) # clustering algorithm
  clusters <- g_commun$membership # community memberships
  V(g_igraph)$com <- clusters # community memberships for igraph
  
  con <- intramodularConnectivity.fromExpr(g_adjmat, 
                                           colors = clusters, # connectivity measures 
                                           power = 1,
                                           getWholeNetworkConnectivity = TRUE)
  
  if (nrow(con) == 30) {
    rownames(con) <- issues
    con <- tibble::rownames_to_column(con, "name") %>%
      filter(name != "E033") 
  } else {
    issues_reduced <- issues[-6]
    rownames(con) <- issues_reduced
    con <- tibble::rownames_to_column(con, "name")
  }
  
  for (k in issues) {
    nd_structures[nd_structures$V1 == i & nd_structures$V2 == k, 3] = con[con$name == k, 2]
    nd_structures[nd_structures$V1 == i & nd_structures$V2 == k, 4] = con[con$name == k, 3]
    nd_structures[nd_structures$V1 == i & nd_structures$V2 == k, 5] = con[con$name == k, 4]
  }
  
}
rm(i, g_adjmat, g_qgraph, g_igraph, g_commun, clusters, con, k, issues_reduced) # clean-up

nd_structures <- nd_structures %>%
  dplyr::rename(cnty = V1, issues = V2, c.total = V3, c.intra = V4, c.extra = V5) %>% 
  left_join(macro_country, by = "cnty")

# ------------------------------------------------------------------------------------------------------------- #
