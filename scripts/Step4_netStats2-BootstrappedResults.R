
# - The Organization of Political Belief Networks: A Cross-Country Analysis ----------------------------------- #
# - Turgut Keskinturk - Bogazici University - 2022 ------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------- #

### section 4: the bootstraps ###

# - PART 01 --------------------------------------------------------------------------------------------------- #

# prepare row bootstraps

nt_bootstraps <- matrix(NA, nrow = length(countries), ncol = 1001) # empty matrix
nt_bootstraps[,1] = sort(countries)
nt_bootstraps <- as.data.frame(nt_bootstraps)

matrix_loopfr <- 1 # our loop friend
for (c in countries) {
  print(c)
  cor.dta <- df.bts[[as.character(c)]]$boots # bootstrapped adjacency matrices
  
  for (i in 1:1000) {
    g1 = abs(cor.dta[[i]]$graph)^2 # soft-thresholding
    g2 = graph_from_adjacency_matrix(g1, mode = "undirected", weighted = TRUE) # igraph object
    nt_bootstraps[nt_bootstraps$V1 == c, i+1] = 
      2*sum((E(g2)$weight)) / ((vcount(g2)*(vcount(g2)-1))) # belief constraint estimates
    rm(g1, g2)
  } 
  matrix_loopfr = matrix_loopfr + 1
}
rm(c, i, cor.dta, matrix_loopfr) # clean-up

for (a in 1:length(countries)) {
  nt_bootstraps$stdv[a] <- sd( # standard deviation of bootstrapped networks
    nt_bootstraps[a, 2:1001], na.rm = TRUE)
}
nt_bootstraps$mean <- rowMeans(nt_bootstraps[, 2:1001], na.rm = TRUE) # mean of bootstrapped networks
rm(a) # clean-up

## prepare covariates
nt_bootstraps <- nt_bootstraps %>% left_join(macro_country, by = c("V1" = "cnty"))
nt_bootstraps$partyinst <- rowMeans(nt_bootstraps[, partyinst_var])
nt_bootstraps <- nt_bootstraps %>% mutate_at(c("partyinst", "polact"), scale.items)
nt_bootstraps <- nt_bootstraps %>% na.omit()

# - PART 02 --------------------------------------------------------------------------------------------------- #

# bootstrapped fits & visualizations

## bootstrapped fits
fit.bts <- as.data.frame(matrix(NA, nrow = 1000, ncol = 2))

for (i in 1:1000) {
  print(i) # 1000 OLS regression models for each bootstrapped network data
  reg.fit <- lm(scale.items(nt_bootstraps[, i+1]) ~ 
                  partyinst + polact + d_polid + d_mode + d_size + d_lang,
                data = nt_bootstraps)
  fit.bts[i, 1] <- t(as.data.frame(summary(reg.fit)$coefficients)[2, 1]) # coefficients
  fit.bts[i, 2] <- abs(t(as.data.frame(summary(reg.fit)$coefficients)[2, 3])) # t-value
}
rm(reg.fit, i) # clean-up

## visualize bootstrapped linear fits
viz.fits <- ggplot(fit.bts, aes(x = V2)) + 
  geom_histogram(aes(y = ..density..), bins = 30) + geom_density(size = 0.5) +
  theme_ipsum() + ggtitle(element_blank()) +
  xlab("The Distribution of t-values") + ylab("Density") +
  scale_x_continuous(limits = c(3.5, 5), breaks = c(3.5, 4, 4.5, 5))

## visualize bootstrapped mean values
viz.boot <- ggplot(nt_bootstraps, 
                   aes(x = scale.items(partyinst), 
                       y = mean)) +
  geom_smooth(se = F, col = "black", size = 1, method = "lm", formula = y ~ splines::bs(x, 3)) +
  geom_smooth(aes(x = scale.items(partyinst), y = mean + 1.96*stdv),
              se = F, col = "gray50", size = 0.5, method = "lm",
              formula = y ~ splines::bs(x, 3), linetype = "dashed") +
  geom_smooth(aes(x = scale.items(partyinst), y = mean - 1.96*stdv),
              se = F, col = "gray50", size = 0.5, method = "lm",
              formula = y ~ splines::bs(x, 3), linetype = "dashed") +
  theme_ipsum() + ggtitle(element_blank()) +
  xlab("Ideological Institutionalization") + ylab("Constraint")

## print out
png("./figures/FIG02.png", width = 15, height = 5, res = 1000, units = "in")
grid.arrange(viz.fits, viz.boot, nrow = 1)
dev.off() # left panel for the distribution of t-values, and the right panel for the bootstrapped means

# - PART 03 --------------------------------------------------------------------------------------------------- #

# prepare column bootstraps

## belief pairs for each country
nt_itemselect <- bind_rows(df.cor, .id = "cnty") # create dataset of pairwise correlations
nt_itemselect$cnty <- as.integer(nt_itemselect$cnty)
nt_itemselect$pair <- paste(nt_itemselect$x, nt_itemselect$y, sep = ", ")

## user-written function for estimating constraint with different items
items.selection <- function(dinput, times) {
  newsample <- sample(issues, times) # sample the belief items X times
  newdatafr <- dinput %>%
    mutate(fi = x %in% newsample & y %in% newsample) %>% subset(fi == TRUE)
  newconstr <- newdatafr %>% group_by(cnty) %>%
    summarise(mean = mean(abs(c))) %>% # mean constraint for each country
    left_join(macro_country, by = "cnty") # new data frame
  newconstr$partyinst <- rowMeans(newconstr[, partyinst_var])
  fitted_model <- lm(scale.items(mean) ~ 
                       scale.items(partyinst) + scale.items(polact) + d_polid + d_mode + d_size + d_lang, 
                     data = newconstr) # OLS regression models
  coef = summary(fitted_model)$coefficients[2,1]
  tval = summary(fitted_model)$coefficients[2,3]
  fitted_coefficients <- data.frame(list(coef, tval))
  return(fitted_coefficients)
}

## find the bootstrapped regression results
set.seed(1212) # replicability for item heterogeneity
fit.it1 <- (replicate(1000, items.selection(nt_itemselect, 24)))
fit.it2 <- (replicate(1000, items.selection(nt_itemselect, 16)))

# - PART 04 --------------------------------------------------------------------------------------------------- #

# bootstrapped fits & visualizations

fit.it1 <- as.data.frame(t(matrix(unlist(fit.it1), nrow = 2)))
fit.it2 <- as.data.frame(t(matrix(unlist(fit.it2), nrow = 2)))

viz.item1 <- ggplot(fit.it1, aes(x = V2)) + 
  geom_histogram(aes(y = ..density..), bins = 30) + geom_density(size = 0.5) +
  theme_ipsum() + ggtitle("N = 24") +
  xlab("The Distribution of t-values") + ylab("Density") +
  scale_x_continuous(limits = c(3, 5), breaks = c(3, 3.5, 4, 4.5, 5))

viz.item2 <- ggplot(fit.it2, aes(x = V2)) + 
  geom_histogram(aes(y = ..density..), bins = 30) + geom_density(size = 0.5) +
  theme_ipsum() + ggtitle("N = 16") +
  geom_vline(xintercept = 1.96, linetype = "dashed") + # t = 1.96 cut
  xlab("The Distribution of t-values") + ylab("Density") +
  scale_x_continuous(limits = c(1.5, 5.5), breaks = c(1.5, 2.5, 3.5, 4.5, 5.5))

png("./figures/FIG03.png", width = 15, height = 5, res = 1000, units = "in")
grid.arrange(viz.item1, viz.item2, nrow = 1)
dev.off() # left panel for the distribution of t-values, and the right panel for the bootstrapped means
# 99.5% of the boots have t-values higher than 1.96

# ------------------------------------------------------------------------------------------------------------- #
