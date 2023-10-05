
# - The Organization of Political Belief Networks: A Cross-Country Analysis ----------------------------------- #
# - Turgut Keskinturk - Bogazici University - 2022 ------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------- #

### section 3: the analysis of constraint ###

# - PART 01 --------------------------------------------------------------------------------------------------- #

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

# - PART 02 --------------------------------------------------------------------------------------------------- #

# party institutionalization index

## variable list
partyinst_var <- c("parorg", "parbra", "parlink", "parplat", "parcoh")

## scale reliability & principal component analyses
partyinst_pca <- pca(nt_structures[, partyinst_var]) 
# all items load into one factor / SS = 3.36 / % Var = 0.67 / "partycoh" seems low in its contribution
nt_structures$partyinst_f <- predict.psych(partyinst_pca, nt_structures[, partyinst_var])
psych::alpha(nt_structures[, partyinst_var]) # 0.86
nt_structures$partyinst <- rowMeans(nt_structures[, partyinst_var])
cor(nt_structures[, 13:17]) # correlation matrix

## solution comparison (cor = 99.5%)
cor(nt_structures$partyinst, nt_structures$partyinst_f, use = "pairwise.complete.obs")

## partial least squares regressions
partyinst_mvr <- mvr(den ~ parorg + parbra + parlink + parplat + parcoh,
                     data = nt_structures)
partyinst_val <- crossval(partyinst_mvr, segments = 2, segment.type = "random")
plot(t(partyinst_val$validation$PRESS)) # one-component model is the best

# - PART 03 --------------------------------------------------------------------------------------------------- #

# visualize the bivariate correlation

cor(nt_structures$partyinst, nt_structures$den) # a correlation of 0.65

## visualize constraint
png("./figures/FIG01.png", width = 12, height = 7.5, res = 1000, units = "in")
ggplot(nt_structures, aes(scale(partyinst), den)) +
  geom_point(alpha = 0.75, color = "black") +
  geom_smooth(se = T, fill = "gray90", method = "lm", formula = y ~ splines::bs(x, 3), col = "black") +
  xlab("Ideological Institutionalization") + ylab("Constraint") +
  geom_text_repel(aes(label = label)) +
  scale_y_continuous(limits = c(0.008, 0.060), 
                     breaks = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06)) +
  theme_ipsum()
dev.off()

# - PART 04 --------------------------------------------------------------------------------------------------- #

# clean and prepare covariates

## user-function for scaling
scale.items <- function(x) {
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE) # this is for normalizing and centering variables
} # "scale" function changes the type of variables, which creates certain complications

## scale the predictors
nt_structures <- nt_structures %>% mutate_at(c(
  "den", "partyinst", "educ", "polact", "civic", "nparty", "parorg", "parbra", "parlink", "parplat", "parcoh", 
  "polar", "media", "ethnic", "relig"), 
  scale.items) %>% # scale the items such that each is standardized & centralized
  mutate(d_polid = 1 - d_polid) # countries in which the question of political ideology is asked

# - PART 05 --------------------------------------------------------------------------------------------------- #

# OLS models 

## model selection
fit.bic <- bicreg(nt_structures[, c(5:12, 18:21, 23)], nt_structures$den)
summary(fit.bic) # the best model includes party institutionalization & political activism & survey controls
# the posterior probability of the first model is 26%, while the second model is 11%.

## linear fits and control for regression assumptions
fit.lm1 <- lm(den ~ partyinst + d_polid + d_mode + d_size + d_lang +
                educ + media + polact + civic + nparty + polar + ethnic + relig,
              data = nt_structures)
fit.lm2 <- lm(den ~ partyinst + d_polid + d_mode + d_size + d_lang + 
                polact, # reduced model
              data = nt_structures)

tab_model(fit.lm1, fit.lm2,
          collapse.se = TRUE, digits = 3, p.style = "stars", show.ci = F,
          robust = TRUE, vcov.type = "HC3") # robust standard errors

## potential explanations for the education
fit.med <- mediate(
  den ~ educ + (partyinst) + (polact), data = nt_structures, n.iter = 10000)
## partyinst & polact strongly mediate the effects of education

# ------------------------------------------------------------------------------------------------------------- #
