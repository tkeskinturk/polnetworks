
# ----------------- The Organization of Political Belief Networks: A Cross-Country Analysis ------------------- #
# ------------------------------ Turgut Keskintürk - Bogaziçi University - 2021 ------------------------------- #

# ------------------------------------------------------------------------------------------------------------- #

### section 4: visualize bootstrap variations & refit regression models in all samples ###

library(reshape2); library(gridExtra)
ggthemr("fresh")

# ------------------------------------------------------ PART 01 ---------------------------------------------- #

# bootstrapped means and standard deviations of network properties

bt_structures_sum <- purrr::map_df(bt_structures, 
                                   function(df){summarize_all(df, c(mean, sd))}) 
bt_structures_sum$cnty <- countries # add countries to the data frame
bt_structures_sum <- bt_structures_sum %>% left_join(macro_country, by = "cnty")

bootplot.1 <- ggplot(bt_structures_sum, aes(x = party.inst, y = V1_fn1)) + 
  geom_smooth(fill = "gray80", colour = "black",  size = 1.25, se = T, 
              method = "lm", formula = y ~ x + I(x^2)) + 
  xlab("Party Institutionalization") + ylab("") + labs(title="A - Total Connectivity") +
  scale_y_continuous(limits = c(3, 4.25), breaks = c(3.0, 3.25, 3.50, 3.75, 4.0, 4.25))

bootplot.2 <- ggplot(bt_structures_sum, aes(x = party.inst, y = V2_fn1)) + 
  geom_smooth(fill = "gray80", colour = "black",  size = 1.25, se = T, 
              method = "lm", formula = y ~ x + I(x^2)) +
  xlab("Party Institutionalization") + ylab("") + labs(title="B - Intra-Modular Connectivity") +
  scale_y_continuous(limits = c(0.9, 1.8))

bootplot.3 <- ggplot(bt_structures_sum, aes(x = party.inst, y = V3_fn1)) + 
  geom_smooth(fill = "gray80", colour = "black",  size = 1.25, se = T, 
              method = "lm", formula = y ~ x + I(x^2)) +
  xlab("Party Institutionalization") + ylab("") + labs(title="C - Extra-Modular Connectivity") +
  scale_y_continuous(limits = c(1.5, 3))

bootplot.4 <- ggplot(bt_structures_sum, aes(x = party.inst, y = V4_fn1)) + 
  geom_smooth(fill = "gray80", colour = "black",  size = 1.25, se = T, 
              method = "lm", formula = y ~ x + I(x^2)) +
  xlab("Party Institutionalization") + ylab("") + labs(title="D - Modularity") +
  scale_y_continuous(limits = c(0.35, 0.75))

png("FIG02.png", width = 10, height = 10, units = "in", res = 500)
grid.arrange(bootplot.1, bootplot.2, bootplot.3, bootplot.4, nrow = 2)
dev.off()
rm(bootplot.1, bootplot.2, bootplot.3, bootplot.4) # clean-up

# ------------------------------------------------------ PART 02 ---------------------------------------------- #

# replicate regression models in all bootstraps

fit.bts <- list()

for (k in 1:4) {
  print(k)
  bt_structures_big <- bind_rows(bt_structures, .id = "column_label")
  bt_structures_big <- bt_structures_big[, c(1, k+1)]
  bt_structures_big$boots <- rep(1:1000, times = 78, each = 1)
  bt_structures_big <- reshape(bt_structures_big, idvar = "column_label",
                               timevar = "boots", direction = "wide")
  bt_structures_big$cnty <- countries
  bt_structures_big <- bt_structures_big %>% left_join(
    macro_country, by = "cnty"
  )
  
  fit.bts[[k]] <- as.data.frame(matrix(NA, nrow = 1000, ncol = 2))
  
  for (i in 1:1000) {
    reg.fit <- lm(bt_structures_big[, i+1] ~ 
                    party.inst + pol.ideo + n.party + I(n.party^2) + media.diff, 
                  data = bt_structures_big)
    fit.bts[[k]][i, 1] <- t(as.data.frame(summary(reg.fit)$coefficients)[2, 1]) # party institutionalization coefficients
    fit.bts[[k]][i, 2] <- abs(t(as.data.frame(summary(reg.fit)$coefficients)[2, 3])) # party institutionalization t-values
  }
}
rm(k, i, bt_structures_big, reg.fit) # clean-up

# calculate the summary statistics for regression t-values

fit.bts_sum <- list() # list of summary t-values

for (k in 1:2) {
  fit.bts.sum <- as.data.frame(matrix(NA, nrow = 4, ncol = 3))
  
  fit.bts.sum[1, 3] = "Connectivity (T)"
  fit.bts.sum[1, 1] = mean(fit.bts[[1]][[k]]) 
  fit.bts.sum[1, 2] = sd(fit.bts[[1]][[k]])
  fit.bts.sum[2, 3] = "Connectivity (M)"
  fit.bts.sum[2, 1] = mean(fit.bts[[2]][[k]]); 
  fit.bts.sum[2, 2] = sd(fit.bts[[2]][[k]])
  fit.bts.sum[3, 3] = "Connectivity (E)"
  fit.bts.sum[3, 1] = mean(fit.bts[[3]][[k]]); 
  fit.bts.sum[3, 2] = sd(fit.bts[[3]][[k]])
  fit.bts.sum[4, 1] = mean(fit.bts[[4]][[k]]); 
  fit.bts.sum[4, 3] = "Modularity"
  fit.bts.sum[4, 2] = sd(fit.bts[[4]][[k]])
  fit.bts_sum[[k]] <- fit.bts.sum
}
rm(k, fit.bts.sum) # clean-up

# ------------------------------------------------------ PART 03 ---------------------------------------------- #

# visualize the regression coefficients

regplot.1 <- fit.bts_sum[[1]] %>%
  mutate(V3 = factor(V3, levels = c(
    "Modularity", "Connectivity (E)", "Connectivity (M)", "Connectivity (T)"),
    ordered = TRUE)) %>%
  ggplot(aes(x = V3, y = V1)) +
  geom_pointrange(aes(ymin = V1 - 1.96 * V2, 
                      ymax = V1 + 1.96 * V2)) +
  coord_flip() + geom_hline(yintercept = 0, col = "gray", linetype = "dashed") +
  ggtitle("") + 
  ylab("Coefficients") + xlab(element_blank()) +
  scale_y_continuous(limits = c(-0.6, 1.6))

regplot.2 <- fit.bts_sum[[2]] %>%
  mutate(V3 = factor(V3, levels = c(
    "Modularity", "Connectivity (E)", "Connectivity (M)", "Connectivity (T)"),
    ordered = TRUE)) %>%
  ggplot(aes(x = V3, y = V1)) +
  geom_pointrange(aes(ymin = V1 - 1.96 * V2, 
                      ymax = V1 + 1.96 * V2)) +
  coord_flip() + 
  geom_hline(yintercept = 1.96, col = "gray20", linetype = "dashed") +
  ggtitle("") + 
  ylab("T-Values") + xlab(element_blank()) +
  scale_y_continuous(limits = c(-0.1, 7.5))
  
png("FIG03.png", width = 10, height = 3.5, units = "in", res = 500)
grid.arrange(regplot.1, regplot.2, nrow = 1)
dev.off()
rm(regplot.1, regplot.2) # clean-up

# ------------------------------------------------------------------------------------------------------------- #
