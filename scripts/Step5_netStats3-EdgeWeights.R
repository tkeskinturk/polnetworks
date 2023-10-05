
# - The Organization of Political Belief Networks: A Cross-Country Analysis ----------------------------------- #
# - Turgut Keskinturk - Bogazici University - 2022 ------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------------------------- #

### section 5: edge-level network properties ###

# - PART 01 --------------------------------------------------------------------------------------------------- #

# edge-list network properties

nt_edgeweight <- bind_rows(df.cor, .id = "cnty") # create dataset of pairwise correlations
nt_edgeweight$pair <- paste(nt_edgeweight$x, nt_edgeweight$y, sep = ", ")
nt_edgeweight$cnty <- as.integer(nt_edgeweight$cnty)
nt_edgeweight <- nt_edgeweight %>% 
  left_join(macro_country, by = "cnty") %>%
  left_join(macro_issue, by = c("x" = "var")) %>%
  left_join(macro_issue, by = c("y" = "var")) %>%
  mutate(issues = paste(type.x, type.y, sep = "-")) %>%
  mutate(issues = recode(issues,
                         "Symbolic-Symbolic" = "Symbolic",
                         "Operational-Operational" = "Operational",
                         "Symbolic-Operational" = "Cross-Levels",
                         "Operational-Symbolic" = "Cross-Levels")) %>% # categories of connection
  mutate(issues = 
           factor(issues, 
                  levels = c("Symbolic", "Operational", "Cross-Levels"))) %>%
  na.omit() # pair-level network statistics for each country-level network
nt_edgeweight$partyinst <- rowMeans(nt_edgeweight[, partyinst_var])
nt_edgeweight <- nt_edgeweight %>% mutate_at(c("partyinst", "polact"), scale.items)

# - PART 02 --------------------------------------------------------------------------------------------------- #

# reduced multilevel models replicating the OLS models

## fit multilevel models
fit.ml1 <- lmer(scale.items(c) ~ 
                  (1 + 1 | pair) +
                  partyinst + polact + d_polid + d_mode + d_size + d_lang,
                data = nt_edgeweight,
                control = lmerControl(optimizer="bobyqa"))

fit.ml2 <- lmer(scale.items(c) ~ 
                  (1 + partyinst | pair) +
                  partyinst + polact + d_polid + d_mode + d_size + d_lang,
                data = nt_edgeweight,
                control = lmerControl(optimizer="bobyqa"))

fit.ml3 <- lmer(scale.items(c) ~ 
                  (1 + partyinst + polact | pair) +
                  partyinst + polact + d_polid + d_mode + d_size + d_lang,
                data = nt_edgeweight,
                control = lmerControl(optimizer="bobyqa"))

## compare the model fits & extract coefficients
BIC(fit.ml1); BIC(fit.ml2); BIC(fit.ml3) # the best model is the model #3.
partyinst_mle <- coef(fit.ml3)$pair$partyinst %>%
  as.data.frame() %>% rename("coef" = ".") # varying slopes

## plot the distribution of the coefficients of party institutionalization
png("./figures/FIG04.png", width = 7.5, height = 5, res = 1000, units = "in")
ggplot(partyinst_mle, aes(x = coef)) +
  geom_histogram(aes(y = ..density..), bins = 30) + geom_density(size = 0.5) +
  theme_ipsum() + geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("The Distribution of Varying Slopes") + ylab("Density")
dev.off()

# - PART 03 --------------------------------------------------------------------------------------------------- #

# the variations among issue levels

item.constraints <- nt_edgeweight %>% # country-level item constraint for different levels
  group_by(cnty, issues) %>% summarise(cor = mean(c, na.rm = TRUE))

png("./figures/FIG05.png", width = 10, height = 5, res = 1000, units = "in")
ggplot(item.constraints, aes(x = issues, y = cor)) + 
  geom_boxplot() + geom_jitter(width = 0.1, alpha = 0.25) + theme_ipsum() +
  xlab("Item Levels") + ylab("Mean Constraint")
dev.off()

# - PART 04 --------------------------------------------------------------------------------------------------- #

# predictors of network variation

## within-group standardization of edge weights & scaling

nt_edgeweight <- nt_edgeweight %>%
  group_by(issues) %>% mutate(c_std = scale.items(c)) # issue-level standardization
nt_edgeweight <- nt_edgeweight %>% mutate_at(c("partyinst", "polact"), scale.items)

## visualize group-level trends

png("./figures/FIG06.png", w = 10, h = 7.5, units = "in", res = 1000)
nt_edgeweight %>%
  ggplot(aes(partyinst, c_std, color = issues)) +
  geom_smooth(se = T, fill = "gray90", method = "lm", formula = y ~ splines::bs(x, 3)) +
  xlab("Ideological Institutionalization") + ylab("Mean Constraint of Within-Standardized Edge Weights") + 
  theme_ipsum() + theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Item Levels") + guides(color = guide_legend(nrow = 1, byrow = TRUE))
dev.off()

## a more formal test: multi-level fits

fit.ml4 <- lmer(scale.items(c) ~ 
                  (1 + partyinst + polact | pair) +
                  partyinst * issues + polact + d_polid + d_mode + d_size + d_lang,
                data = nt_edgeweight,
                control = lmerControl(optimizer="bobyqa"))
tab_model(fit.ml4, collapse.se = TRUE, digits = 3, p.style = "stars", show.ci = F)
## no statistically significant differences between different item groups

# ------------------------------------------------------------------------------------------------------------- #
