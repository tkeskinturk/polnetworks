
# ----------------- The Organization of Political Belief Networks: A Cross-Country Analysis ------------------- #
# ------------------------------ Turgut Keskintürk - Bogaziçi University - 2021 ------------------------------- #

# ------------------------------------------------------------------------------------------------------------- #

### section 4: non-parametric bootstraps and sampling variability ###

# load the necessary packages

library(boot); library(parallel) # for parallel computing in Windows

# ------------------------------------------------------ PART 01 ---------------------------------------------- #

# non-parametric bootstrap

## function for bootstrapping the samples and saving the results

bootstrap_cors = function(dinput) {
  print(dinput)
  sub = subset(df, df$cntry==dinput) %>% subset(select = -c(cntry, uniqid))
  b = bootstrap(sub, 1000); b_corrs = list()
  
  for(j in 1:1000) {
    bs <- as.data.frame(b$strap[[j]])
    bs <- bs[,colSums(is.na(bs))<nrow(bs)]
    adj.mat <- cor(bs, use = "pairwise.complete.obs", method = "pearson")
    b_corrs[[j]] <- abs(
      EBICglasso(adj.mat, n = nrow(bs), verbose = FALSE, lambda.min.ratio = 0.001)
    )
  }
  save(b_corrs, file=paste("bootstrapped_corrs_", dinput, ".saved", sep=""))
  return(b_corrs)
}

## parallel computing for bootstrapping

copies_of_r_cluster <- makeCluster(6) # number of clusters allocated to the apply functions

clusterExport(
  copies_of_r_cluster, c("df", "countries", "bootstrap_cors"))
clusterEvalQ(
  copies_of_r_cluster, c("df", "countries", "bootstrap_cors"))
clusterEvalQ(
  copies_of_r_cluster, {
    library(qgraph); library(igraph); library(bootnet); library(WGCNA); library(psych); library(tidyverse); 
    library(boot); library(plyr); library(janitor); library(stringr); library(ggpubr); library(sjstats)
    }
  )

bootstrapped_correlations <- parLapply(
  copies_of_r_cluster, 
  countries, 
  fun = bootstrap_cors) # bootstrap 1,000 files for each country

stopCluster(copies_of_r_cluster)
save(bootstrapped_correlations, file = "bootstrapped_correlations.saved")
rm(copies_of_r_cluster) # clean-up

# ------------------------------------------------------ PART 02 ---------------------------------------------- #

# bootstrap files

bt_structures <- list() # the empty list for network properties

for(c in 1:length(countries)) {
  print(c)
  
  bt_structures[[c]] <- as.data.frame(matrix(NA, nrow = 1000, ncol = 5))
  
  for(i in 1:1000) {
    
    # networks
    g_adjmat <- bootstrapped_correlations[[c]][[i]] # adjacency matrices
    g_qgraph <- qgraph(g_adjmat, DoNotPlot = TRUE, diag = FALSE) # qgraph object
    g_igraph <- as.igraph(g_qgraph, attributes = TRUE) # igraph object
    
    # communities
    g_commun <- cluster_fast_greedy(g_igraph,
                                    weights = E(g_igraph)$weight) # clustering algorithm
    clusters <- g_commun$membership # community memberships
    V(g_igraph)$com = clusters # community memberships for igraph
    
    invisible(capture.output(con <- intramodularConnectivity.fromExpr(g_adjmat, 
                                                                      colors = clusters, # connectivity measures 
                                                                      power = 1,
                                                                      getWholeNetworkConnectivity = TRUE)))
    
    bt_structures[[c]][i, 1] = mean(con[, 1]) # mean total connectivity
    bt_structures[[c]][i, 2] = mean(con[, 2]) # mean intra connectivity
    bt_structures[[c]][i, 3] = mean(con[, 3]) # mean outer connectivity
    bt_structures[[c]][i, 4] = modularity(g_igraph, V(g_igraph)$com, weights = E(g_igraph)$weight)
    bt_structures[[c]][i, 5] = length(unique(g_commun$membership))
    }
}
rm(c, i, g_adjmat, g_qgraph, g_igraph, g_commun, clusters, con) # clean-up
rm(bootstrapped_correlations) # too big

# ------------------------------------------------------------------------------------------------------------- #
