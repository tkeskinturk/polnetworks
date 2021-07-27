
# ----------------- The Organization of Political Belief Networks: A Cross-Country Analysis ------------------- #
# ------------------------------ Turgut Keskintürk - Bogaziçi University - 2021 ------------------------------- #

# ------------------------------------------------------------------------------------------------------------- #

### section 3: graph-level network properties ###
# part 1 and part 2 analyze observed belief networks
# part 3 and part 4 use row bootstraps to confirm the belief constraint trends
# part 5 and part 6 use column bootstraps for item selection tests

# ------------------------------------------------------ PART 01 ---------------------------------------------- #

# let's compute network density for each belief network

nt_structures <- as.data.frame(matrix(NA, nrow = length(countries), ncol = 2))
nt_structures[, 1] <- countries

for (i in sort(countries)) {
  print(i)
  
  # networks
  g_adjmat <- df.mat[as.character(i)][[1]] # adjacency matrices
  g_qgraph <- qgraph(g_adjmat, DoNotPlot = TRUE, diag = FALSE) # qgraph object
  g_igraph <- as.igraph(g_qgraph, attributes = TRUE) # igraph object
  
  nt_structures[nt_structures$V1 == i, 2] = 
    2*sum((E(g_igraph)$weight)) / ((vcount(g_igraph)*(vcount(g_igraph)-1)))
}
rm(i, g_adjmat, g_qgraph, g_igraph) # clean-up

nt_structures <- nt_structures %>% dplyr::rename(cnty = V1, den = V2) %>% 
  left_join(macro_country, by = "cnty") %>% na.omit()

# ------------------------------------------------------ PART 02 ---------------------------------------------- #

# OLS regressions and visualizations

## user-function for scaling

scale.items <- function(x) {
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE) # this is for normalizing and centering variables
} # "scale" function changes the type of variables, which create certain complications

## visualize constraint

png("FIG01.png", width = 12.5, height = 7.5, res = 1000, units = "in")
ggplot(nt_structures, aes(scale.items(party.inst), den)) +
  geom_point(alpha = 0.75, color = "black") +
  geom_smooth(se = T, fill = "gray90", method = "lm", formula = y ~ splines::bs(x, 3), col = "black") +
  xlab("Ideological Institutionalization") + ylab("Constraint") +
  geom_text_repel(aes(label = label)) +
  scale_y_continuous(limits = c(0.008, 0.060), 
                     breaks = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06)) +
  theme_ipsum()
dev.off()

## scale the predictors & select models

nt_structures <- nt_structures %>% mutate_at(c(
  "den", "party.inst", "nparty", "media.diff", "polact", "leader", "ideo", "civic", "educ", "delib", "lgdp", "lpop"), 
  scale.items) %>% # scale the items such that each is standardized & centralized
  mutate(polid = 1 - polid)

fit.bic <- bicreg(nt_structures[, 4:15], nt_structures$den)
summary(fit.bic) # the best model includes party institutionalization / political activism / pol.id control
# the posterior probability of the first model is 39.1%, while the second model is 9%.

## linear fits and control for regression assumptions

fit.lm1 <- lm(den ~ party.inst + polact + nparty + 
                delib + media.diff + ideo + leader + civic + educ + polid + lgdp + lpop,
              data = nt_structures) 
fit.lm2 <- lm(den ~ party.inst + polact + polid, # reduced model according to BMA
              data = nt_structures)

plot_model(fit.lm2, type = "diag")[[1]] # variance inflation factors are below 5
plot_model(fit.lm2, type = "diag")[[2]] # there is no non-normality of residuals and outliers
plot_model(fit.lm2, type = "diag")[[3]] # residuals are normally distributed
plot_model(fit.lm2, type = "diag")[[4]] # there is heteroskedasticity, which calls for heteroskedasticity-correction
shapiro.test(resid(fit.lm2)) # shapiro-wilk shows that residuals are normally distributed (W = 0.97, p = 0.25)

tab_model(fit.lm1, fit.lm2,
          collapse.se = TRUE, digits = 3, p.style = "stars", show.ci = F,
          robust = TRUE, vcov.type = "HC3") # robust standard errors

# ------------------------------------------------------ PART 03 ---------------------------------------------- #

# prepare row bootstraps

bt_structures <- matrix(NA, nrow = length(countries), ncol = 1001) # empty matrix
bt_structures[,1] = sort(countries)
bt_structures <- as.data.frame(bt_structures)

matrix_loopfr <- 1 # our loop friend
for (c in countries) {
  print(c)
  cor.dta <- df.bts[[as.character(c)]]$boots # bootstrapped adjacency matrices
  
  for (i in 1:1000) {
    g1 = abs(cor.dta[[i]]$graph)^2 # soft-thresholding
    g2 = graph_from_adjacency_matrix(g1, mode = "undirected", weighted = TRUE) # igraph object
    bt_structures[bt_structures$V1 == c, i+1] = 
      2*sum((E(g2)$weight)) / ((vcount(g2)*(vcount(g2)-1))) # belief constraint estimates
    rm(g1, g2)
  } 
  matrix_loopfr = matrix_loopfr + 1
}
rm(c, i, cor.dta, matrix_loopfr) # clean-up

for (a in 1:length(countries)) {
  bt_structures$stdv[a] <- sd( # standard deviation of bootstrapped networks
    bt_structures[a, 2:1001], na.rm = TRUE)
}
bt_structures$mean <- rowMeans(bt_structures[, 2:1001], na.rm = TRUE) # mean of bootstrapped networks
rm(a) # clean-up

bt_structures <- bt_structures %>% left_join(macro_country, by = c("V1" = "cnty"))
bt_structures <- bt_structures %>% na.omit()

# ------------------------------------------------------ PART 04 ---------------------------------------------- #

# bootstrapped fits & visualizations

## bootstrapped fits

lm.boot <- as.data.frame(matrix(NA, nrow = 1000, ncol = 2))

for (i in 1:1000) {
  print(i) # 1000 OLS regression models for each bootstrapped network data
  reg.fit <- lm(scale.items(bt_structures[, i+1]) ~ 
                  scale.items(party.inst) + scale.items(polact) + polid,
                data = bt_structures)
  lm.boot[i, 1] <- t(as.data.frame(summary(reg.fit)$coefficients)[2, 1]) # coefficients
  lm.boot[i, 2] <- abs(t(as.data.frame(summary(reg.fit)$coefficients)[2, 3])) # t-value
}
rm(reg.fit, i) # clean-up

## visualize bootstrapped linear fits

viz.fits <- ggplot(lm.boot, aes(x = V2)) + 
  geom_histogram(aes(y = ..density..), bins = 30) + geom_density(size = 0.5) +
  theme_ipsum() + ggtitle(element_blank()) +
  xlab("The Distribution of t-values") + ylab("Density")

## visualize bootstrapped mean values

viz.boot <- ggplot(bt_structures, 
                   aes(x = scale.items(party.inst), 
                       y = mean)) +
  geom_smooth(se = F, col = "black", size = 1, method = "lm", formula = y ~ splines::bs(x, 3)) +
  geom_smooth(aes(x = scale.items(party.inst), y = mean + 1.96*stdv),
              se = F, col = "gray50", size = 0.5, method = "lm",
              formula = y ~ splines::bs(x, 3), linetype = "dashed") +
  geom_smooth(aes(x = scale.items(party.inst), y = mean - 1.96*stdv),
              se = F, col = "gray50", size = 0.5, method = "lm",
              formula = y ~ splines::bs(x, 3), linetype = "dashed") +
  theme_ipsum() + ggtitle(element_blank()) +
  xlab("Ideological Institutionalization") + ylab("Constraint")

## print out

png("FIG02.png", width = 15, height = 5, res = 1000, units = "in")
grid.arrange(viz.fits, viz.boot, nrow = 1)
dev.off() # left panel for the distribution of t-values, and the right panel for the bootstrapped means

# ------------------------------------------------------ PART 05 ---------------------------------------------- #

# prepare column bootstraps

## belief pairs for each country

it_structures <- bind_rows(df.cor, .id = "cnty") # create dataset of pairwise correlations
it_structures$cnty <- as.integer(it_structures$cnty)
it_structures$pair <- paste(it_structures$x, it_structures$y, sep = ", ")

## user-written function for estimating constraint with different items

items.selection <- function(dinput, times) {
  newsample <- sample(issues, times) # sample the belief items X times
  newdatafr <- dinput %>%
    mutate(fi = x %in% newsample & y %in% newsample) %>% subset(fi == TRUE)
  newconstr <- newdatafr %>% group_by(cnty) %>%
    summarise(mean = mean(abs(c))) %>% # mean constraint for each country
    left_join(macro_country, by = "cnty") # new data frame
  fitted_model <- lm(scale.items(mean) ~ 
                       scale.items(party.inst) + scale.items(polact) + polid, 
                     data = newconstr) # OLS regression models
  coef = summary(fitted_model)$coefficients[2,1]
  tval = summary(fitted_model)$coefficients[2,3]
  fitted_coefficients <- data.frame(list(coef, tval))
  return(fitted_coefficients)
}

## find the bootstrapped regression results

set.seed(211) # replicability for item heterogeneity
lm.item1 <- (replicate(1000, items.selection(it_structures, 24)))
lm.item2 <- (replicate(1000, items.selection(it_structures, 16)))

# ------------------------------------------------------ PART 06 ---------------------------------------------- #

# bootstrapped fits & visualizations

lm.item1 <- as.data.frame(t(matrix(unlist(lm.item1), nrow = 2)))
lm.item2 <- as.data.frame(t(matrix(unlist(lm.item2), nrow = 2)))

viz.item1 <- ggplot(lm.item1, aes(x = V2)) + 
  geom_histogram(aes(y = ..density..), bins = 30) + geom_density(size = 0.5) +
  theme_ipsum() + ggtitle("N = 24") +
  xlab("The Distribution of t-values") + ylab("Density")

viz.item2 <- ggplot(lm.item2, aes(x = V2)) + 
  geom_histogram(aes(y = ..density..), bins = 30) + geom_density(size = 0.5) +
  theme_ipsum() + ggtitle("N = 16") +
  geom_vline(xintercept = 1.96, linetype = "dashed") + # t = 1.96 cut
  xlab("The Distribution of t-values") + ylab("Density")

png("FIG03.png", width = 15, height = 5, res = 1000, units = "in")
grid.arrange(viz.item1, viz.item2, nrow = 1)
dev.off() # left panel for the distribution of t-values, and the right panel for the bootstrapped means
# 9 out of 1,000 boots have t-values lower than 1.96

rm(bt_structures, it_structures, nt_structures, fit.bic, fit.lm1, fit.lm2, items.selection, lm.boot, 
   lm.item1, lm.item2, viz.boot, viz.fits, viz.item1, viz.item2) # clean-up for later scripts

# ------------------------------------------------------------------------------------------------------------- #
