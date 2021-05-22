
# ----------------- The Organization of Political Belief Networks: A Cross-Country Analysis ------------------- #
# ------------------------------ Turgut Keskintürk - Bogaziçi University - 2021 ------------------------------- #

# ------------------------------------------------------------------------------------------------------------- #

### section 3: the contextual predictors of political belief organization ###

# load the necessary packages

library(sjPlot); library(lme4); library(lm.beta); library(lmtest); library(sandwich)

# ------------------------------------------------------ PART 01 ---------------------------------------------- #

# regression analyses for the pooled samples

## regression results

fit.reg <- list()
for (k in 1:4) {
  print(k)
  reg.fit <- lm(nt_structures[, k+1] ~ 
                  party.inst + pol.ideo + n.party + I(n.party^2) + media.diff, data = nt_structures)
  fit.reg[[k]] <- reg.fit
  names(fit.reg)[k] <- names(nt_structures)[k+1]
}
rm(reg.fit, k) # clean-up

## extract regression results

tab_model(fit.reg$c.total, fit.reg$c.intra, fit.reg$c.extra, fit.reg$modularity,
          p.style = "stars", show.se = TRUE, show.ci = FALSE, collapse.se = TRUE, digits = 3,
          robust = TRUE, vcov.fun = "vcovCL", vcov.type = "HC3",
          file = "regressionsresults1.html")

# ------------------------------------------------------ PART 02 ---------------------------------------------- #

# multilevel models

fit.lme <- list()
for (k in 1:3) {
  print(k)
  lme.fit <- lmer(nd_structures[, k+2] ~ 
                    party.inst + pol.ideo + n.party + I(n.party^2) + media.diff +
                    (1 + party.inst | issues), data = nd_structures,
                  control = lmerControl(optimizer = "Nelder_Mead"))
  fit.lme[[k]] <- lme.fit
  names(fit.lme)[k] <- names(nd_structures)[k+2]
}
rm(lme.fit, k) # clean-up

# extract regression results

tab_model(fit.lme$c.total, fit.lme$c.intra, fit.lme$c.extra, 
          p.style = "stars", show.se = TRUE, show.ci = FALSE, collapse.se = TRUE, digits = 3,
          file = "regressionsresults2.html")

# ------------------------------------------------------------------------------------------------------------- #
