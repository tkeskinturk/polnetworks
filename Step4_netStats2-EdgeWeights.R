
# ----------------- The Organization of Political Belief Networks: A Cross-Country Analysis ------------------- #
# ------------------------------ Turgut Keskintürk - Bogaziçi University - 2021 ------------------------------- #

# ------------------------------------------------------------------------------------------------------------- #

### section 4: edge-level network properties ###

# ------------------------------------------------------ PART 01 ---------------------------------------------- #

# edge-list network properties

ed_structures <- bind_rows(df.cor, .id = "cnty") # create dataset of pairwise correlations
ed_structures$pair <- paste(ed_structures$x, ed_structures$y, sep = ", ")
ed_structures$cnty <- as.integer(ed_structures$cnty)
ed_structures <- ed_structures %>% 
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

# ------------------------------------------------------ PART 02 ---------------------------------------------- #

# the variations among issue levels

item.constraints <- ed_structures %>% # country-level item constraint for different levels
  group_by(cnty, issues) %>% summarise(cor = mean(c, na.rm = TRUE))

png("FIG04.png", width = 10, height = 5, res = 1000, units = "in")
ggplot(item.constraints, aes(x = issues, y = cor)) + 
  geom_boxplot() + geom_jitter(width = 0.1, alpha = 0.25) + theme_ipsum() +
  xlab("Item Levels") + ylab("Mean Constraint")
dev.off()

# ------------------------------------------------------ PART 03 ---------------------------------------------- #

# predictors of network variation

## within-group standardization of edge weights & scaling

ed_structures <- ed_structures %>%
  group_by(issues) %>% mutate(c_std = scale.items(c)) # issue-level standardization
ed_structures <- ed_structures %>% mutate_at(c("party.inst", "polact"), scale.items)

## visualize group-level trends

png("FIG05.png", w = 10, h = 7.5, units = "in", res = 1000)
ed_structures %>%
  ggplot(aes(party.inst, c_std, color = issues)) +
  geom_smooth(se = T, fill = "gray90", method = "lm", formula = y ~ splines::bs(x, 3)) +
  xlab("Ideological Institutionalization") + ylab("Standardized Edge Weights") + 
  theme_ipsum() + theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Item Levels") + guides(color = guide_legend(nrow = 1, byrow = TRUE))
dev.off()

## a more formal test: multi-level fits

lme.fit <- lmer(c_std ~ party.inst * issues + polid + polact + (1 + 1 | pair), 
                data = ed_structures)
tab_model(lme.fit, collapse.se = TRUE, digits = 3, p.style = "stars", show.ci = F)

rm(item.constraints, lme.fit, ed_structures) # clean-up for later scripts

# ------------------------------------------------------------------------------------------------------------- #
