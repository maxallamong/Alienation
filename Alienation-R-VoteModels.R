# Maxwell B. Allamong
# Political Alienation and the Trump Vote in the 2016-2020 U.S. Presidential Elections
# File: Vote Models 
# Updated: Jun. 11th, 2023

# README ----
#  To replicate this analysis, first open the R Project file (Alienation.RProj).
#  Next, simply run this entire script: it will automatically set the working directory to the
#  folder that contains the R Project file, and then it will source the data from the source
#  file (Alienation-Source.R).

# Packages ----
  #install.packages(c("MASS", "nnet", "srvyr", "car", "svyVGAM",
                      #"coefplot", "survey", "stargazer", "tidyverse",
                      #"texreg", "conflicted"))

# Libraries ----
  library(MASS)
  library(nnet)
  library(here)
  library(srvyr)
  library(car)
  library(svyVGAM)
  library(coefplot)
  library(survey)
  library(stargazer)
  library(tidyverse)
  library(texreg)
  library(conflicted)
    conflict_prefer("select","dplyr")
    conflict_prefer("filter","dplyr")
    
# Load Data ----
  mydata.20 <- read.csv("Data/mydata-20-2.csv")
  mydata.16 <- read.csv("Data/mydata-16-2.csv")
  mydata.cdf <- read.csv("Data/mydata-cdf-2.csv")
  #source("Alienation-ANES-Source.R")
  
  weights.2016 <- read_dta("Data/ANES/Survey Data/anes_timeseries_2016.dta") 
  weights.2016 <- weights.2016 %>%
    select(V160001, V160001_orig) %>%
    rename(caseid.alt = V160001, caseid = V160001_orig) %>%
    mutate(year = 2016)
  weights.cdf <- read_dta("Data/ANES/Survey Data/anes_timeseries_cdf_stata_20211118.dta") # read in CDF
  weights.cdf <- weights.cdf %>%
    select(VCF0009z, VCF0006, VCF0004) %>%
    rename(weight = VCF0009z, caseid = VCF0006, year = VCF0004)
  
# Working Directory ----
  #setwd(here())
# Analyses ----
  ## Turnout ----
    ### 2016-2020 ----
      #### Models ----

      ##### Base model
      base.mod.16 <- mydata.16 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(base.mod.16)
      base.mod.16$aic <- AIC(base.mod.16)[2]
      
      ###### Demographics model
      demos.mod.16 <- mydata.16 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(demos.mod.16)
      demos.mod.16$aic <- AIC(demos.mod.16)[2]
      
      ###### Groups plus demographics model
      groups.mod.16 <- mydata.16 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 therm.demgroups,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(groups.mod.16)
      groups.mod.16$aic <- AIC(groups.mod.16)[2]

      ###### Sexism plus demographics model
      msi.mod.16 <- mydata.16 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 msi,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(msi.mod.16)
      msi.mod.16$aic <- AIC(msi.mod.16)[2]          
      
      ###### Authoritarian plus demographics model
      auth.mod.16 <- mydata.16 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 child.trait,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(auth.mod.16)
      auth.mod.16$aic <- AIC(auth.mod.16)[2]          
      
      ###### Economics plus demographics model
      econ.mod.16 <- mydata.16 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 oppose.trade + econ.retro,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(econ.mod.16)
      econ.mod.16$aic <- AIC(econ.mod.16)[2]

      ###### Full model
      full.mod.16.df <- mydata.16 %>% # 2016
        select(weight, strata, cluster,
               voted.general, cyn, eff,
               party.strength, pol.int, educ, evangel, income.q, female, black, white, age,
               therm.demgroups, msi, child.trait, oppose.trade, econ.retro) %>%
        drop_na() 
      
      full.mod.16 <- mydata.16 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 therm.demgroups + msi + child.trait + oppose.trade + econ.retro,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(full.mod.16)
      full.mod.16$aic <- AIC(full.mod.16)[2]
      
      
      texreg(list(base.mod.16, demos.mod.16, groups.mod.16, msi.mod.16, 
                  auth.mod.16, econ.mod.16, full.mod.16),
             single.row = F,
             stars = numeric(0),
             digits = 3,
             override.se = list(summary(base.mod.16)$coefficients[,4], summary(demos.mod.16)$coefficients[,4],
                                summary(groups.mod.16)$coefficients[,4], summary(msi.mod.16)$coefficients[,4],
                                summary(auth.mod.16)$coefficients[,4], summary(econ.mod.16)$coefficients[,4],
                                summary(full.mod.16)$coefficients[,4]),
             #override.se = list(base.p.16, demos.p.16, groups.p.16, msi.p.16, auth.p.16, econ.p.16, full.p.16),
             custom.coef.names = c("Constant","Inefficacy","Cynicism","Partisan Strength",
                                   "Political Interest","Education","Evangelical","Income","Female", "Black",
                                   "White","Age","Democratic-Aligned Group Therms.","Modern Sexism",
                                   "Child-Rearing Authoritarianism","Oppose Trade","Retrospective Econ. Assessments"),
             reorder.coef = c(seq(2,17,1), 1),
             custom.gof.rows = list(Observations = c(nobs(base.mod.16), nobs(demos.mod.16), nobs(groups.mod.16),
                                                     nobs(msi.mod.16), nobs(auth.mod.16), nobs(econ.mod.16),
                                                     nobs(full.mod.16)),
                                    AIC = c(AIC(base.mod.16)[2], AIC(demos.mod.16)[2], AIC(groups.mod.16)[2],
                                            AIC(msi.mod.16)[2], AIC(auth.mod.16)[2], AIC(econ.mod.16)[2],
                                            AIC(full.mod.16)[2])))
      
      
      
      ##### Base model
      base.mod.20 <- mydata.20 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(base.mod.20)
      base.mod.20$aic <- AIC(base.mod.20)[2]
      
      ###### Demographics model
      demos.mod.20 <- mydata.20 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(demos.mod.20)
      demos.mod.20$aic <- AIC(demos.mod.20)[2]
      
      ###### Groups plus demographics model
      groups.mod.20 <- mydata.20 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 therm.demgroups,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(groups.mod.20)
      groups.mod.20$aic <- AIC(groups.mod.20)[2]
      
      ###### Sexism plus demographics model
      msi.mod.20 <- mydata.20 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 msi,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(msi.mod.20)
      msi.mod.20$aic <- AIC(msi.mod.20)[2]          
      
      ###### Authoritarian plus demographics model
      auth.mod.20 <- mydata.20 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 child.trait,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(auth.mod.20)
      auth.mod.20$aic <- AIC(auth.mod.20)[2]          
      
      ###### Economics plus demographics model
      econ.mod.20 <- mydata.20 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 oppose.trade + econ.retro,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(econ.mod.20)
      econ.mod.20$aic <- AIC(econ.mod.20)[2]
      
      ###### Full model
      full.mod.20.df <- mydata.20 %>% # 2016
        select(weight, strata, cluster,
               voted.general, cyn, eff,
               party.strength, pol.int, educ, evangel, income.q, female, black, white, age,
               therm.demgroups, msi, child.trait, oppose.trade, econ.retro) %>%
        drop_na() 
      
      full.mod.20 <- mydata.20 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 therm.demgroups + msi + child.trait + oppose.trade + econ.retro,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(full.mod.20)
      full.mod.20$aic <- AIC(full.mod.20)[2]
      
      
      
      texreg(list(base.mod.20, demos.mod.20, groups.mod.20, msi.mod.20, 
                  auth.mod.20, econ.mod.20, full.mod.20),
             single.row = F,
             stars = numeric(0),
             digits = 3,
             override.se = list(summary(base.mod.20)$coefficients[,4], summary(demos.mod.20)$coefficients[,4],
                                summary(groups.mod.20)$coefficients[,4], summary(msi.mod.20)$coefficients[,4],
                                summary(auth.mod.20)$coefficients[,4], summary(econ.mod.20)$coefficients[,4],
                                summary(full.mod.20)$coefficients[,4]),
             #override.se = list(base.p.20, demos.p.20, groups.p.20, msi.p.20, auth.p.20, econ.p.20, full.p.20),
             custom.coef.names = c("Constant","Inefficacy","Cynicism","Partisan Strength",
                                   "Political Interest","Education","Evangelical","Income","Female", "Black",
                                   "White","Age","Democratic-Aligned Group Therms.","Modern Sexism",
                                   "Child-Rearing Authoritarianism","Oppose Trade","Retrospective Econ. Assessments"),
             reorder.coef = c(seq(2,17,1), 1),
             custom.gof.rows = list(Observations = c(nobs(base.mod.20), nobs(demos.mod.20), nobs(groups.mod.20),
                                                     nobs(msi.mod.20), nobs(auth.mod.20), nobs(econ.mod.20),
                                                     nobs(full.mod.20)),
                                    AIC = c(AIC(base.mod.20)[2], AIC(demos.mod.20)[2], AIC(groups.mod.20)[2],
                                            AIC(msi.mod.20)[2], AIC(auth.mod.20)[2], AIC(econ.mod.20)[2],
                                            AIC(full.mod.20)[2])))

  
  

      #### Wald tests -----
      linearHypothesis(full.mod.16, "cyn=msi") # p = 0.04982*
      linearHypothesis(full.mod.16, "cyn=oppose.trade") # p = 0.1284
      linearHypothesis(full.mod.16, "cyn=child.trait") # p = 0.0002037***
      linearHypothesis(full.mod.16, "cyn=econ.retro") # p = 0.2613
      linearHypothesis(full.mod.16, "cyn=therm.demgroups") # p = 0.5319
      linearHypothesis(full.mod.16, "cyn=party.strength") # p = 0.1295
      linearHypothesis(full.mod.16, "cyn=evangel") # p = 0.5576
      
      
      linearHypothesis(full.mod.20, "cyn=msi") # p = 0.07652*
      linearHypothesis(full.mod.20, "cyn=oppose.trade") # p = 0.1057
      linearHypothesis(full.mod.20, "cyn=child.trait") # p = 0.002348***
      linearHypothesis(full.mod.20, "cyn=econ.retro") # p = 0.5158
      linearHypothesis(full.mod.20, "cyn=therm.demgroups") # p = 0.5957
      linearHypothesis(full.mod.20, "cyn=party.strength") # p = 0.2666
      linearHypothesis(full.mod.20, "cyn=evangel") # p = 0.1446
      

      #### Coefficient Plot ----
      plot <- multiplot(full.mod.16, full.mod.20,
                        dodgeHeight = 0.4,
                        numberAngle = 0, 
                        innerCI = 1.645)
      plot <- plot$data
      plot <- plot %>%
        filter(Coefficient %in% c("eff","cyn","msi","econ.retro","oppose.trade","child.trait","therm.demgroups","party.strength","evangel"))
      plot$Coefficient <- factor(plot$Coefficient, levels = rev(c("eff","cyn","msi","econ.retro","oppose.trade","child.trait","therm.demgroups","party.strength","evangel")))
      plot$Model <- factor(plot$Model, levels = rev(c("full.mod.16", "full.mod.20")))
      
      ggplot(plot, aes(x = Value, y = Coefficient, color = Model)) +
        geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
        geom_errorbar(aes(xmin = LowInner, xmax = HighInner), width = 0, size = 1.35, position = position_dodge(width = 0.5)) +
        geom_errorbar(aes(xmin = LowOuter, xmax = HighOuter), width = 0, position = position_dodge(width = 0.5)) +
        geom_vline(xintercept = 0, lty = "dashed", color = "grey") +
        geom_hline(yintercept = 7.5) +
        labs(y = NULL, x = "Coefficient Estimate", title = NULL) +
        scale_x_continuous(limits = c(-2.2,2.2)) +
        scale_y_discrete(labels = rev(c("Inefficacy","Cynicism","Modern Sexism",
                                        "Retrospective Econ.\nAssessments","Oppose Free Trade",
                                        "Child-Rearing\nAuthoritarianism","Democratic-Aligned\nGroups Therm.",
                                        "Partisan Strength","Evangelical"))) +
        scale_color_manual("Year",
                           limits = c("full.mod.16","full.mod.20"),
                           labels = c("2016","2020"),
                           values = c("black","grey70")) +
        theme_set(theme_classic()) +
        theme(axis.text = element_text(size = 17),
              #axis.text.x = element_text(angle = 0),
              axis.title = element_text(size = 17),
              legend.text = element_text(size = 17),
              legend.title = element_text(size = 19),
              legend.position = "right",
              panel.border = element_rect(color = "black", size = 1, fill = NA),
              panel.background = element_rect(fill = "white", colour = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              plot.title = element_text(size = 17),
              plot.margin = margin(0.5,2,0.5,0.25,"cm"))
      ggsave(height = 8, width = 8, "Figures/Final Figures/Allamong 22-0313.R1 Figure 2.eps") # Plot
      
      #### Robustness ----
      # 2016
      full.mod.cyn.16 <- mydata.16 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 therm.demgroups + msi + child.trait + oppose.trade + econ.retro,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(full.mod.cyn.16)
      full.mod.cyn.16$aic <- AIC(full.mod.cyn.16)[2]

      full.mod.eff.16 <- mydata.16 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 therm.demgroups + msi + child.trait + oppose.trade + econ.retro,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(full.mod.eff.16)
      full.mod.eff.16$aic <- AIC(full.mod.eff.16)[2]

      
      
      # 2020
      full.mod.cyn.20 <- mydata.20 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ cyn + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 therm.demgroups + msi + child.trait + oppose.trade + econ.retro,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(full.mod.cyn.20)
      full.mod.cyn.20$aic <- AIC(full.mod.cyn.20)[2]

      full.mod.eff.20 <- mydata.20 %>% # 2016
        drop_na(weight, strata, cluster) %>%
        as_survey_design(weights = weight,
                         strata = strata,
                         ids = cluster,
                         nest = T) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + 
                 party.strength + pol.int + educ + evangel + income.q + female + black + white + age +
                 therm.demgroups + msi + child.trait + oppose.trade + econ.retro,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(full.mod.eff.20)
      full.mod.eff.20$aic <- AIC(full.mod.eff.20)[2]
      
      texreg(list(full.mod.eff.16, full.mod.cyn.16, full.mod.eff.20, full.mod.cyn.20),
             single.row = F,
             stars = numeric(0),
             digits = 3,
             override.se = list(summary(full.mod.eff.16)$coefficients[,4], summary(full.mod.cyn.16)$coefficients[,4],
                                summary(full.mod.eff.20)$coefficients[,4], summary(full.mod.cyn.20)$coefficients[,4]),
             #override.se = list(base.p.20, demos.p.20, groups.p.20, msi.p.20, auth.p.20, econ.p.20, full.p.20),
             custom.coef.names = c("Constant","Inefficacy","Cynicism","Partisan Strength",
                                   "Political Interest","Education","Evangelical","Income","Female", "Black",
                                   "White","Age","Democratic-Aligned Group Therms.","Modern Sexism",
                                   "Child-Rearing Authoritarianism","Oppose Trade","Retrospective Econ. Assessments"),
             reorder.coef = c(seq(2,17,1), 1),
             custom.gof.rows = list(Observations = c(nobs(full.mod.eff.16), nobs(full.mod.cyn.16), 
                                                     nobs(full.mod.eff.20), nobs(full.mod.cyn.20)),
                                    AIC = c(AIC(full.mod.eff.16)[2], AIC(full.mod.cyn.16)[2], 
                                            AIC(full.mod.eff.20)[2], AIC(full.mod.cyn.20)[2])))
      

      #### Predicted Probabilities ----
      # Inefficacy 
      # 2016
      mycoeff <- coef(full.mod.16) # save coefficients
      mycovar <- vcov(full.mod.16) # save variance-covariance matrix
      set.seed(219)
      simCoef <- mvrnorm(n = 5000, mycoeff, mycovar) # simulate 1000 coefficients
      x <- seq(0,1,0.01)
      temp <- matrix(NA, nrow = 5000, ncol = 1)
      sim.results <- matrix(NA, nrow = 101, ncol = 4)
      probdists.turnout.eff <- matrix(NA, nrow = 5000, ncol = 2)
    
      for(i in 1:length(x)){
        for(j in 1:nrow(simCoef)){
          Ag <- simCoef[j,1] + 
                  simCoef[j,2]*x[i] +
                  simCoef[j,3]*full.mod.16.df$cyn +
                  simCoef[j,4]*full.mod.16.df$party.strength +
                  simCoef[j,5]*full.mod.16.df$pol.int +
                  simCoef[j,6]*full.mod.16.df$educ +
                  simCoef[j,7]*full.mod.16.df$evangel +
                  simCoef[j,8]*full.mod.16.df$income.q +
                  simCoef[j,9]*full.mod.16.df$female +
                  simCoef[j,10]*full.mod.16.df$black +
                  simCoef[j,11]*full.mod.16.df$white + 
                  simCoef[j,12]*full.mod.16.df$age +
                  simCoef[j,13]*full.mod.16.df$therm.demgroups +
                  simCoef[j,14]*full.mod.16.df$msi +
                  simCoef[j,15]*full.mod.16.df$child.trait +
                  simCoef[j,16]*full.mod.16.df$oppose.trade +
                  simCoef[j,17]*full.mod.16.df$econ.retro
          
          pred <-exp(Ag)/(1+exp(Ag)) # convert log odds to probs
          temp[j,1] <- mean(pred) # # for i-th set of simulated coeficients, capture mean predicted probability across all observed valuesstore in j-th row, 1st column
          
        }
        
        sim.results[i,1] <- x[i]
        sim.results[i,2] <- median(temp, 0.5)
        sim.results[i,3] <- quantile(temp, 0.025)
        sim.results[i,4] <- quantile(temp, 0.975)
        
        if (x[i] == 0) { # min
          probdists.turnout.eff[,1] <- temp[,1]
        }
        
        if (x[i] == 1) { # max
          probdists.turnout.eff[,2] <- temp[,1]
        } 
        
      }
      
      sim.results <- as.data.frame(sim.results) # put results in data frame
      colnames(sim.results) <- c("x", "fit", "lwr", "upr")
      
      probdists <- as.data.frame(probdists.turnout.eff)
      colnames(probdists) <- c("lo.eff","hi.eff")
      round(quantile(probdists$hi.eff - probdists$lo.eff, c(0.05, 0.5, 0.95)),3) # -4.4%, [-9.2%, 0.6%]
    
      
      # 2020
      mycoeff <- coef(full.mod.20) # save coefficients
      mycovar <- vcov(full.mod.20) # save variance-covariance matrix
      set.seed(219)
      simCoef <- mvrnorm(n = 5000, mycoeff, mycovar) # simulate 1000 coefficients
      x <- seq(0,1,0.01)
      temp <- matrix(NA, nrow = 5000, ncol = 1)
      sim.results <- matrix(NA, nrow = 101, ncol = 4)
      probdists.turnout.eff <- matrix(NA, nrow = 5000, ncol = 2)
      
      for(i in 1:length(x)){
        for(j in 1:nrow(simCoef)){
          Ag <- simCoef[j,1] +
            simCoef[j,2]*x[i] +
            simCoef[j,3]*full.mod.20.df$cyn +
            simCoef[j,4]*full.mod.20.df$party.strength +
            simCoef[j,5]*full.mod.20.df$pol.int +
            simCoef[j,6]*full.mod.20.df$educ +
            simCoef[j,7]*full.mod.20.df$evangel +
            simCoef[j,8]*full.mod.20.df$income.q +
            simCoef[j,9]*full.mod.20.df$female +
            simCoef[j,10]*full.mod.20.df$black +
            simCoef[j,11]*full.mod.20.df$white + 
            simCoef[j,12]*full.mod.20.df$age +
            simCoef[j,13]*full.mod.20.df$therm.demgroups +
            simCoef[j,14]*full.mod.20.df$msi +
            simCoef[j,15]*full.mod.20.df$child.trait +
            simCoef[j,16]*full.mod.20.df$oppose.trade +
            simCoef[j,17]*full.mod.20.df$econ.retro
          
          pred <-exp(Ag)/(1+exp(Ag)) # convert log odds to probs
          temp[j,1] <- mean(pred) # # for i-th set of simulated coeficients, capture mean predicted probability across all observed valuesstore in j-th row, 1st column
          
        }
        
        sim.results[i,1] <- x[i]
        sim.results[i,2] <- median(temp, 0.5)
        sim.results[i,3] <- quantile(temp, 0.025)
        sim.results[i,4] <- quantile(temp, 0.975)
        
        if (x[i] == 0) { # min
          probdists.turnout.eff[,1] <- temp[,1]
        }
        
        if (x[i] == 1) { # max
          probdists.turnout.eff[,2] <- temp[,1]
        } 
        
      }
      
      sim.results <- as.data.frame(sim.results) # put results in data frame
      colnames(sim.results) <- c("x", "fit", "lwr", "upr")
      
      probdists <- as.data.frame(probdists.turnout.eff)
      colnames(probdists) <- c("lo.eff","hi.eff")
      round(quantile(probdists$hi.eff - probdists$lo.eff, c(0.05, 0.5, 0.95)),3) # -8.4%, [-12.8%, -3.7%]
      
      
      # Cynicism 
      # 2016
      mycoeff <- coef(full.mod.16) # save coefficients
      mycovar <- vcov(full.mod.16) # save variance-covariance matrix
      set.seed(219)
      simCoef <- mvrnorm(n = 5000, mycoeff, mycovar) # simulate 1000 coefficients
      x <- seq(0,1,0.01)
      temp <- matrix(NA, nrow = 5000, ncol = 1)
      sim.results <- matrix(NA, nrow = 101, ncol = 4)
      probdists.turnout.cyn <- matrix(NA, nrow = 5000, ncol = 2)
      
      for(i in 1:length(x)){
        for(j in 1:nrow(simCoef)){
          Ag <- simCoef[j,1] + 
            simCoef[j,2]*full.mod.16.df$eff +
            simCoef[j,3]*x[i] +
            simCoef[j,4]*full.mod.16.df$party.strength +
            simCoef[j,5]*full.mod.16.df$pol.int +
            simCoef[j,6]*full.mod.16.df$educ +
            simCoef[j,7]*full.mod.16.df$evangel +
            simCoef[j,8]*full.mod.16.df$income.q +
            simCoef[j,9]*full.mod.16.df$female +
            simCoef[j,10]*full.mod.16.df$black +
            simCoef[j,11]*full.mod.16.df$white + 
            simCoef[j,12]*full.mod.16.df$age +
            simCoef[j,13]*full.mod.16.df$therm.demgroups +
            simCoef[j,14]*full.mod.16.df$msi +
            simCoef[j,15]*full.mod.16.df$child.trait +
            simCoef[j,16]*full.mod.16.df$oppose.trade +
            simCoef[j,17]*full.mod.16.df$econ.retro
          
          pred <-exp(Ag)/(1+exp(Ag)) # convert log odds to probs
          temp[j,1] <- mean(pred) # # for i-th set of simulated coeficients, capture mean predicted probability across all observed valuesstore in j-th row, 1st column
          
        }
        
        sim.results[i,1] <- x[i]
        sim.results[i,2] <- median(temp, 0.5)
        sim.results[i,3] <- quantile(temp, 0.025)
        sim.results[i,4] <- quantile(temp, 0.975)
        
        if (x[i] == 0) { # min
          probdists.turnout.cyn[,1] <- temp[,1]
        }
        
        if (x[i] == 1) { # max
          probdists.turnout.cyn[,2] <- temp[,1]
        } 
        
      }
      
      sim.results <- as.data.frame(sim.results) # put results in data frame
      colnames(sim.results) <- c("x", "fit", "lwr", "upr")
      
      probdists <- as.data.frame(probdists.turnout.cyn)
      colnames(probdists) <- c("lo.cyn","hi.cyn")
      round(quantile(probdists$hi.cyn - probdists$lo.cyn, c(0.05, 0.5, 0.95)),3) # 13.9%, [5.6%, 22.4%]
      
      # 2020
      mycoeff <- coef(full.mod.20) # save coefficients
      mycovar <- vcov(full.mod.20) # save variance-covariance matrix
      set.seed(219)
      simCoef <- mvrnorm(n = 5000, mycoeff, mycovar) # simulate 1000 coefficients
      x <- seq(0,1,0.01)
      temp <- matrix(NA, nrow = 5000, ncol = 1)
      sim.results <- matrix(NA, nrow = 101, ncol = 4)
      probdists.turnout.eff <- matrix(NA, nrow = 5000, ncol = 2)
      
      for(i in 1:length(x)){
        for(j in 1:nrow(simCoef)){
          Ag <- simCoef[j,1] +
            simCoef[j,2]*full.mod.20.df$cyn +
            simCoef[j,3]*x[i] +
            simCoef[j,4]*full.mod.20.df$party.strength +
            simCoef[j,5]*full.mod.20.df$pol.int +
            simCoef[j,6]*full.mod.20.df$educ +
            simCoef[j,7]*full.mod.20.df$evangel +
            simCoef[j,8]*full.mod.20.df$income.q +
            simCoef[j,9]*full.mod.20.df$female +
            simCoef[j,10]*full.mod.20.df$black +
            simCoef[j,11]*full.mod.20.df$white + 
            simCoef[j,12]*full.mod.20.df$age +
            simCoef[j,13]*full.mod.20.df$therm.demgroups +
            simCoef[j,14]*full.mod.20.df$msi +
            simCoef[j,15]*full.mod.20.df$child.trait +
            simCoef[j,16]*full.mod.20.df$oppose.trade +
            simCoef[j,17]*full.mod.20.df$econ.retro
          
          pred <-exp(Ag)/(1+exp(Ag)) # convert log odds to probs
          temp[j,1] <- mean(pred) # # for i-th set of simulated coeficients, capture mean predicted probability across all observed valuesstore in j-th row, 1st column
          
        }
        
        sim.results[i,1] <- x[i]
        sim.results[i,2] <- median(temp, 0.5)
        sim.results[i,3] <- quantile(temp, 0.025)
        sim.results[i,4] <- quantile(temp, 0.975)
        
        if (x[i] == 0) { # min
          probdists.turnout.cyn[,1] <- temp[,1]
        }
        
        if (x[i] == 1) { # max
          probdists.turnout.cyn[,2] <- temp[,1]
        } 
        
      }
      
      sim.results <- as.data.frame(sim.results) # put results in data frame
      colnames(sim.results) <- c("x", "fit", "lwr", "upr")
      
      probdists <- as.data.frame(probdists.turnout.cyn)
      colnames(probdists) <- c("lo.cyn","hi.cyn")
      round(quantile(probdists$hi.cyn - probdists$lo.cyn, c(0.05, 0.5, 0.95)),3) # 7.0% [-0.3%,15.0%]
      
    ### 1988-2020 ----
      #### Models ----
      turnout.time.20 <- mydata.20 %>% # 2020
        left_join(weights.cdf, by = c("year","caseid")) %>%
        drop_na(weight.y) %>%
        as_survey_design(weights = weight.y) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + party.strength + educ + age + pol.int + income.q + female + white + black,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(turnout.time.20)
      turnout.time.20$aic <- AIC(turnout.time.20)[2]
      
      
      turnout.time.16 <- mydata.16 %>% # 2016
        left_join(weights.2016, by = c("caseid","year")) %>%
        select(-caseid) %>%
        rename(caseid = caseid.alt) %>%
        left_join(weights.cdf, by = c("caseid","year")) %>%
        drop_na(weight.y) %>% 
        as_survey_design(weights = weight.y) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + party.strength + educ + age + pol.int + income.q + female + white + black,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(turnout.time.16)
      turnout.time.16$aic <- AIC(turnout.time.16)[2]

      turnout.time.12 <- mydata.cdf %>% # 2012
        filter(year == 2012) %>%
        drop_na(weight) %>% 
        as_survey_design(weights = weight) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + party.strength + educ + age + pol.int + income.q + female + white + black,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(turnout.time.12)
      turnout.time.12$aic <- AIC(turnout.time.12)[2]
      
      turnout.time.08 <- mydata.cdf %>% # 2008
        filter(year == 2008) %>%
        drop_na(weight) %>% 
        as_survey_design(weights = weight) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + party.strength + educ + age + pol.int + income.q + female + white + black,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(turnout.time.08)
      turnout.time.08$aic <- AIC(turnout.time.08)[2]
      
    
      turnout.time.04 <- mydata.cdf %>% # 2004
        filter(year == 2004) %>%
        drop_na(weight) %>% 
        as_survey_design(weights = weight) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + party.strength + educ + age + pol.int + income.q + female + white + black,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(turnout.time.04)
      turnout.time.04$aic <- AIC(turnout.time.04)[2]
      
      
      turnout.time.00 <- mydata.cdf %>% # 2000
        filter(year == 2000) %>%
        drop_na(weight) %>% 
        as_survey_design(weights = weight) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + party.strength + educ + age + pol.int + income.q + female + white + black,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(turnout.time.00)
      turnout.time.00$aic <- AIC(turnout.time.00)[2]

      
      turnout.time.96 <- mydata.cdf %>% # 1996
        filter(year == 1996) %>%
        drop_na(weight) %>% 
        as_survey_design(weights = weight) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + party.strength + educ + age + pol.int + income.q + female + white + black,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(turnout.time.96)
      turnout.time.96$aic <- AIC(turnout.time.96)[2]

      
      turnout.time.92 <- mydata.cdf %>% # 1992
        filter(year == 1992) %>%
        drop_na(weight) %>% 
        as_survey_design(weights = weight) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + party.strength + educ + age + pol.int + income.q + female + white + black,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(turnout.time.92)
      turnout.time.92$aic <- AIC(turnout.time.92)[2]

      
      turnout.time.88 <- mydata.cdf %>% # 1988
        filter(year == 1988) %>%
        drop_na(weight) %>% 
        as_survey_design(weights = weight) %>%
        svyglm(design = .,
               formula = voted.general ~ eff + cyn + party.strength + educ + age + pol.int + income.q + female + white + black,
               family = stats::quasibinomial(link = "logit"),
               na.action = na.omit)
      summary(turnout.time.88)
      turnout.time.88$aic <- AIC(turnout.time.88)[2]      
  
      texreg(list(turnout.time.88, turnout.time.92, turnout.time.96, turnout.time.00, turnout.time.04, 
                  turnout.time.08, turnout.time.12, turnout.time.16, turnout.time.20),
             single.row = F,
             stars = numeric(0),
             custom.coef.names = c("Constant", "Inefficacy", "Cynicism", "Partisan Strength", 
                                   "Education", "Age", "Political Interest", "Income",
                                   "Female", "White", "Black"),
             reorder.coef = c(seq(2,11), 1),
             override.se = list(summary(turnout.time.88)$coefficients[,4], summary(turnout.time.92)$coefficients[,4],
                                summary(turnout.time.96)$coefficients[,4], summary(turnout.time.00)$coefficients[,4],
                                summary(turnout.time.04)$coefficients[,4], summary(turnout.time.08)$coefficients[,4],
                                summary(turnout.time.12)$coefficients[,4], summary(turnout.time.16)$coefficients[,4],
                                summary(turnout.time.20)$coefficients[,4]),
             custom.gof.rows = list(AIC = c(turnout.time.88$aic, turnout.time.92$aic, turnout.time.96$aic,
                                            turnout.time.00$aic, turnout.time.04$aic, turnout.time.08$aic,
                                            turnout.time.12$aic, turnout.time.16$aic, turnout.time.20$aic)))
      
      
      
      
      turnout.time.est <- data.frame(est = c(coef(turnout.time.88)[2], coef(turnout.time.92)[2], coef(turnout.time.96)[2], 
                                             coef(turnout.time.00)[2], coef(turnout.time.04)[2], coef(turnout.time.08)[2],
                                             coef(turnout.time.12)[2], coef(turnout.time.16)[2], coef(turnout.time.20)[2],
                                             coef(turnout.time.88)[3], coef(turnout.time.92)[3], coef(turnout.time.96)[3], 
                                             coef(turnout.time.00)[3], coef(turnout.time.04)[3], coef(turnout.time.08)[3],
                                             coef(turnout.time.12)[3], coef(turnout.time.16)[3], coef(turnout.time.20)[3]),
                                std.error = c(sqrt(diag(vcov(turnout.time.88)))[2], sqrt(diag(vcov(turnout.time.92)))[2], sqrt(diag(vcov(turnout.time.96)))[2], 
                                              sqrt(diag(vcov(turnout.time.00)))[2], sqrt(diag(vcov(turnout.time.04)))[2], sqrt(diag(vcov(turnout.time.08)))[2],
                                              sqrt(diag(vcov(turnout.time.12)))[2], sqrt(diag(vcov(turnout.time.16)))[2], sqrt(diag(vcov(turnout.time.20)))[2],
                                              sqrt(diag(vcov(turnout.time.88)))[3], sqrt(diag(vcov(turnout.time.92)))[3], sqrt(diag(vcov(turnout.time.96)))[3], 
                                              sqrt(diag(vcov(turnout.time.00)))[3], sqrt(diag(vcov(turnout.time.04)))[3], sqrt(diag(vcov(turnout.time.08)))[3],
                                              sqrt(diag(vcov(turnout.time.12)))[3], sqrt(diag(vcov(turnout.time.16)))[3], sqrt(diag(vcov(turnout.time.20)))[3]),
                                year = rep(seq(1988,2020,4), 2),
                                measure = c(rep("Inefficacy",9), rep("Cynicism",9)))
      turnout.time.est$lwr <- turnout.time.est$est - 1.645*turnout.time.est$std.error
      turnout.time.est$upr <- turnout.time.est$est + 1.645*turnout.time.est$std.error
      
      pdf(height = 7, width = 13, "Figures/2D-Turnout.pdf")
      ggplot(turnout.time.est, aes(x = year, y = est, color = measure)) +
        geom_point(position=position_dodge(width = 1.8), size = 6) +
        geom_errorbar(aes(ymin = lwr, ymax = upr), position = position_dodge(width = 1.8), width = 0, size = 2) +
        geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
        labs(x = "Year", y = "Logit Coefficient\n(DV: Turnout)") +
        scale_x_continuous(breaks = seq(1988,2020,4),
                           labels = c("1988","1992","1996","2000","2004","2008","2012","2016","2020")) +
        scale_colour_manual("Dimension", values = c("black","grey60")) +
        theme(axis.line = element_line(colour = "black"),
              legend.text = element_text(size = 22),
              legend.title = element_text(size= 24),
              plot.subtitle = element_text(vjust = 1), 
              plot.caption = element_text(vjust = 1), 
              plot.margin = margin(0.5,0,0.5,0.25,"cm"),
              panel.background = element_rect(fill = "white", colour = NA),
              panel.grid.major = element_line(linetype = "blank"), 
              panel.grid.minor = element_line(linetype = "blank"),
              axis.title = element_text(size = 28),
              axis.text = element_text(size = 26))
      dev.off()
    
    # Year-to-Year Differences ----
    turnout.df.20 <- mydata.20 %>%
      left_join(weights.cdf, by = c("year","caseid")) %>%
      select(-weight.x) %>%
      rename(weight = weight.y) %>%
      select(voted.general, cyn, eff, party.strength, educ, age, pol.int, female, white, black, income.q, weight) %>%
      mutate(year = 2020)
    
    turnout.df.16 <- mydata.16 %>%
      left_join(weights.2016, by = c("caseid","year")) %>%
      select(-caseid) %>%
      rename(caseid = caseid.alt) %>%
      left_join(weights.cdf, by = c("caseid","year")) %>%
      rename(weight = weight.y) %>%
      select(voted.general, cyn, eff, party.strength, educ, age, pol.int, female, white, black, income.q, weight) %>%
      mutate(year = 2016)
    
    turnout.df.cdf <- mydata.cdf %>%
      select(voted.general, cyn, eff, party.strength, educ, age, pol.int, female, white, black, income.q, year, weight)
    
    turnout.df <- rbind(turnout.df.16, turnout.df.cdf)
    turnout.ests <- turnout.df %>%
      drop_na(weight) %>%
      as_survey_design(weights = weight) %>%
      svyglm(design = .,
             formula = voted.general ~ eff + cyn*relevel(factor(year), ref = "2016") + + party.strength + educ + age + pol.int + income.q + female + white + black,
             family = stats::quasibinomial(link = "logit"),
             na.action = na.omit)
    summary(turnout.ests)
    turnout.ests$aic <- AIC(turnout.ests)[2]      
    
    texreg(list(turnout.ests),
           single.row = F,
           stars = numeric(0),
           custom.coef.names = c("Constant", "Inefficacy", "Cynicism", "1988", "1992",
                                 "1996", "2000", "2004", "2008", "2012", 
                                 "Partisan Strength", "Education", "Age", "Political Interest",
                                 "Female", "White", "Black", "Income", 
                                 "Cyn.$\\times$1988", "Cyn.$\\times$1992",
                                 "Cyn.$\\times$1996", "Cyn.$\\times$2000", "Cyn.$\\times$2004", "Cyn.$\\times$2008",
                                 "Cyn.$\\times$2012"),
           reorder.coef = c(seq(2,25), 1),
           override.se = list(summary(turnout.ests)$coefficients[,4]),
           custom.gof.rows = list(AIC = c(turnout.ests$aic)))

    
  ## Vote Choice ----
    ### 2016-2020 ----
      #### Models ----
    votechoice.16 <- mydata.16 %>% # 2016
      drop_na(weight, strata, cluster) %>%
      as_survey_design(weights = weight,
                       strata = strata,
                       ids = cluster,
                       nest = T) %>%
      svy_vglm(design = .,
               formula = vote.choice.general ~ eff + cyn + rep + ind + pol.int +
                 ideo7 + therm.demgroups + child.trait + msi +
                 oppose.trade + econ.retro + income.q +  educ + evangel + black + 
                 white + female + age,
               family = multinomial(refLevel = "Did not vote"))
    votechoice.16.coef <- as.data.frame(coef(votechoice.16))
    votechoice.16.vcov <- as.data.frame(vcov(votechoice.16))
    votechoice.16.aic <- AICvlm(votechoice.16$fit)
    votechoice.16.n <- nobs(votechoice.16$fit)
    votechoice.16.df <- df.residual_vlm(votechoice.16$fit)
    votechoice.16 <- as.data.frame(coef(summaryvglm(votechoice.16$fit)))
    votechoice.16$one.sided.p <- ifelse(votechoice.16[,3] < 0, pt(votechoice.16[,3], votechoice.16.df,lower=T), 
                                        pt(votechoice.16[,3], votechoice.16.df,lower=F)) # one-tailed p-values
    votechoice.16.clinton <- votechoice.16 %>% slice(seq(1, 100,3))
    votechoice.16.other <- votechoice.16 %>% slice(seq(2, 100,3))
    votechoice.16.trump <- votechoice.16 %>% slice(seq(3, 100,3))
    
    votechoice.16.clinton <- createTexreg(coef = votechoice.16.clinton[,1], 
                                        coef.names = gsub('.{2}$',"", rownames(votechoice.16.clinton)),
                                        se = round(votechoice.16.clinton[,4],3),
                                        pvalues = round(votechoice.16.clinton[,4],3))
    votechoice.16.other <- createTexreg(coef = votechoice.16.other[,1], 
                                        coef.names = gsub('.{2}$',"", rownames(votechoice.16.other)),
                                        se = round(votechoice.16.other[,4],3),
                                        pvalues = round(votechoice.16.other[,4],3))
    votechoice.16.trump <- createTexreg(coef = votechoice.16.trump[,1], 
                                        coef.names = gsub('.{2}$',"", rownames(votechoice.16.trump)),
                                        se = round(votechoice.16.trump[,4],3),
                                        pvalues = round(votechoice.16.trump[,4],3))
    
    
    votechoice.20 <- mydata.20 %>% # 2020
      drop_na(weight, strata, cluster) %>%
      as_survey_design(weights = weight,
                       strata = strata,
                       ids = cluster,
                       nest = T) %>%
      svy_vglm(design = .,
               formula = vote.choice.general ~ eff + cyn + rep + ind + pol.int +
                 ideo7 + therm.demgroups + child.trait + msi +
                 oppose.trade + econ.retro + income.q +  educ + evangel + black + 
                 white + female + age,
               family = multinomial(refLevel = "Did not vote"))
    votechoice.20.coef <- as.data.frame(coef(votechoice.20))
    votechoice.20.vcov <- as.data.frame(vcov(votechoice.20))
    votechoice.20.aic <- AICvlm(votechoice.20$fit)
    votechoice.20.n <- nobs(votechoice.20$fit)
    votechoice.20.df <- df.residual_vlm(votechoice.20$fit)
    votechoice.20 <- as.data.frame(coef(summaryvglm(votechoice.20$fit)))
    votechoice.20$one.sided.p <- ifelse(votechoice.20[,3] < 0, pt(votechoice.20[,3], votechoice.20.df,lower=T), 
                                                               pt(votechoice.20[,3], votechoice.20.df,lower=F)) # one-tailed p-values
    votechoice.20.biden <- votechoice.20 %>% slice(seq(1, 100,3))
    votechoice.20.other <- votechoice.20 %>% slice(seq(2, 100,3))
    votechoice.20.trump <- votechoice.20 %>% slice(seq(3, 100,3))
    
    votechoice.20.biden <- createTexreg(coef = votechoice.20.biden[,1], 
                                        coef.names = gsub('.{2}$',"", rownames(votechoice.20.biden)),
                                        se = round(votechoice.20.biden[,4],3),
                                        pvalues = round(votechoice.20.biden[,4],3))
    votechoice.20.other <- createTexreg(coef = votechoice.20.other[,1], 
                                        coef.names = gsub('.{2}$',"", rownames(votechoice.20.other)),
                                        se = round(votechoice.20.other[,4],3),
                                        pvalues = round(votechoice.20.other[,4],3))
    votechoice.20.trump <- createTexreg(coef = votechoice.20.trump[,1], 
                                        coef.names = gsub('.{2}$',"", rownames(votechoice.20.trump)),
                                        se = round(votechoice.20.trump[,4],3),
                                        pvalues = round(votechoice.20.trump[,4],3))
    
  
    texreg(list(votechoice.16.clinton, votechoice.16.other, votechoice.16.trump,
                votechoice.20.biden, votechoice.20.other, votechoice.20.trump),
           single.row = F,
           stars = numeric(0),
           custom.coef.names = c("Constant", "Inefficacy","Cynicism", "Republican",
                                 "Independent", "Political Interest", "Ideology", "Democratic-Aligned Group Therm.",
                                 "Child-Rearing Authoritarianism", "Modern Sexism", "Oppose Trade", 
                                 "Retrospective Econ. Assessments", "Income", "Education", "Evangelical", 
                                 "Black", "White", "Female", "Age"),
           reorder.coef = c(seq(2,19,1), 1),
           custom.gof.rows = list(Observations = c(rep(votechoice.16.n, 3), rep(votechoice.20.n, 3)),
                                  AIC = c(rep(votechoice.16.aic, 3), rep(votechoice.20.aic, 3))))
  
      
      #### Wald tests ----
      v <- vcov(votechoice.16)
      c <- coef(votechoice.16)
      
      
      diff <- c[3,2] - c[3,4] # cynicism to Republican
      se <- sqrt(v[40,40] + v[42,42] - 2*v[40,42])
      t <- diff/se
      pt(t,general.mod.16$edf)
      
      diff <- c[3,2] - c[3,10] # cynicism to sexism
      se <- sqrt(v[40,40] + v[46,46] - 2*v[40,46])
      t <- diff/se
      pt(t,general.mod.16$edf)
      
      diff <- c[3,2] - c[3,11] # cynicism to trade
      se <- sqrt(v[40,40] + v[47,47] - 2*v[40,47])
      t <- diff/se
      pt(t,general.mod.16$edf)
       
      #### Coefficient Plot ----
      
      # 2016
      plot <- data.frame(Value = c(coef(general.mod.16)[1,], 
                                   coef(general.mod.16)[2,],
                                   coef(general.mod.16)[3,]),
                         Outcome = c(rep("Clinton", 19),
                                     rep("Other", 19),
                                     rep("Trump", 19)),
                         SE = c(sqrt(diag(vcov(general.mod.16)[1:19, 1:19])),
                                sqrt(diag(vcov(general.mod.16)[20:38, 20:38])),
                                sqrt(diag(vcov(general.mod.16)[39:57, 39:57]))),
                         Coefficient = rep(c("Constant","Inefficacy","Cynicism","Republican",
                                             "Independent","Political Interest","Ideology",
                                             "Democratic-Aligned Group Therms.","Child-Rearing Authoritarianism",
                                             "Modern Sexism","Oppose Free-Trade","Retrospective Econ. Assessments",
                                             "Income","Education","Evangelical","Black","White","Female","Age"), 3))
      plot <- plot %>%
        mutate(LowOuter = Value - 1.96*SE,
               LowInner = Value - 1.645*SE,
               HighInner = Value + 1.645*SE,
               HighOuter = Value + 1.96*SE) %>%
        filter(Coefficient %in% c("Inefficacy","Cynicism","Modern Sexism",
                                  "Retrospective Econ. Assessments", "Oppose Free-Trade",
                                  "Child-Rearing Authoritarianism", "Democratic-Aligned Group Therms.",
                                  "Evangelical"))
      
      pdf(height = 8, width = 8, "Figures/VoteChoice-2016-CoefPlot.pdf") # Plot
      ggplot(plot, aes(x = Value, y = Coefficient, color = Outcome)) +
        geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
        geom_errorbar(aes(xmin = LowInner, xmax = HighInner), width = 0, size = 1.35, position = position_dodge(width = 0.5)) +
        geom_errorbar(aes(xmin = LowOuter, xmax = HighOuter), width = 0, position = position_dodge(width = 0.5)) +
        geom_vline(xintercept = 0, lty = "dashed", color = "grey") +
        geom_hline(yintercept = 6.5) +
        labs(y = NULL, x = "Coefficient Estimate", title = NULL) +
        scale_x_continuous(limits = c(-4,4)) +
        scale_y_discrete(limits = rev(c("Inefficacy","Cynicism","Modern Sexism",
                                        "Retrospective Econ. Assessments", "Oppose Free-Trade",
                                        "Child-Rearing Authoritarianism", "Democratic-Aligned Group Therms.",
                                        "Evangelical")),
                         labels = rev(c("Inefficacy","Cynicism","Modern Sexism",
                                        "Retrospective Econ.\nAssessments", "Oppose Free-Trade",
                                        "Child-Rearing\nAuthoritarianism", "Democratic-Aligned\nGroup Therms.",
                                        "Evangelical"))) +
        scale_color_manual("Vote Choice",
                           values = c("#519cd9","grey70","#dc3535")) +
        theme_set(theme_classic()) +
        theme(axis.text = element_text(size = 17),
              #axis.text.x = element_text(angle = 0),
              axis.title = element_text(size = 17),
              legend.text = element_text(size = 17),
              legend.title = element_text(size = 19),
              legend.position = "right",
              panel.border = element_rect(color = "black", size = 1, fill = NA),
              panel.background = element_rect(fill = "white", colour = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              plot.title = element_text(size = 17),
              plot.margin = margin(0.5,1.75,0.5,0.25,"cm"))
      dev.off()
      
      
      
      # 2020
      plot <- data.frame(Value = c(coef(general.mod.20)[1,], 
                                   coef(general.mod.20)[2,],
                                   coef(general.mod.20)[3,]),
                         Outcome = c(rep("Biden", 19),
                                     rep("Other", 19),
                                     rep("Trump", 19)),
                         SE = c(sqrt(diag(vcov(general.mod.20)[1:19, 1:19])),
                                sqrt(diag(vcov(general.mod.20)[20:38, 20:38])),
                                sqrt(diag(vcov(general.mod.20)[39:57, 39:57]))),
                         Coefficient = rep(c("Constant","Inefficacy","Cynicism","Republican",
                                             "Independent","Political Interest","Ideology",
                                             "Democratic-Aligned Group Therms.","Child-Rearing Authoritarianism",
                                             "Modern Sexism","Oppose Free-Trade","Retrospective Econ. Assessments",
                                             "Income","Education","Evangelical","Black","White","Female","Age"), 3))
      plot <- plot %>%
        mutate(LowOuter = Value - 1.96*SE,
               LowInner = Value - 1.645*SE,
               HighInner = Value + 1.645*SE,
               HighOuter = Value + 1.96*SE) %>%
        filter(Coefficient %in% c("Inefficacy","Cynicism","Modern Sexism",
                                  "Retrospective Econ. Assessments", "Oppose Free-Trade",
                                  "Child-Rearing Authoritarianism", "Democratic-Aligned Group Therms.",
                                  "Evangelical"))
      
      pdf(height = 8, width = 8, "Figures/VoteChoice-2020-CoefPlot.pdf") # Plot
      ggplot(plot, aes(x = Value, y = Coefficient, color = Outcome)) +
        geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
        geom_errorbar(aes(xmin = LowInner, xmax = HighInner), width = 0, size = 1.35, position = position_dodge(width = 0.5)) +
        geom_errorbar(aes(xmin = LowOuter, xmax = HighOuter), width = 0, position = position_dodge(width = 0.5)) +
        geom_vline(xintercept = 0, lty = "dashed", color = "grey") +
        geom_hline(yintercept = 6.5) +
        labs(y = NULL, x = "Coefficient Estimate", title = NULL) +
        scale_x_continuous(limits = c(-4,4)) +
        scale_y_discrete(limits = rev(c("Inefficacy","Cynicism","Modern Sexism",
                                        "Retrospective Econ. Assessments", "Oppose Free-Trade",
                                        "Child-Rearing Authoritarianism", "Democratic-Aligned Group Therms.",
                                        "Evangelical")),
                         labels = rev(c("Inefficacy","Cynicism","Modern Sexism",
                                        "Retrospective Econ.\nAssessments", "Oppose Free-Trade",
                                        "Child-Rearing\nAuthoritarianism", "Democratic-Aligned\nGroup Therms.",
                                        "Evangelical"))) +
        scale_color_manual("Vote Choice",
                           values = c("#519cd9","grey70","#dc3535")) +
        theme_set(theme_classic()) +
        theme(axis.text = element_text(size = 17),
              #axis.text.x = element_text(angle = 0),
              axis.title = element_text(size = 17),
              legend.text = element_text(size = 17),
              legend.title = element_text(size = 19),
              legend.position = "right",
              panel.border = element_rect(color = "black", size = 1, fill = NA),
              panel.background = element_rect(fill = "white", colour = "white"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              plot.title = element_text(size = 17),
              plot.margin = margin(0.5,1.75,0.5,0.25,"cm"))
      dev.off()
      
      #### Robustness ----
      # Cynicism only 
      set.seed(219)
      mydata.16$vote.choice.general <- relevel(factor(mydata.16$vote.choice.general), ref = "Did not vote")
      general.mod.16.cyn <- multinom(vote.choice.general ~ cyn + rep + ind + pol.int +
                                   ideo7 + therm.demgroups + child.trait + msi +
                                   oppose.trade + econ.retro + income.q +  educ + evangel + black +
                                   white + female + age, data = mydata.16, Hess = T)
      summary(general.mod.16.cyn)
      general.t.16.cyn <- summary(general.mod.16.cyn)$coefficients/summary(general.mod.16.cyn)$standard.errors # t values
      general.p.16.cyn <- ifelse(general.t.16.cyn < 0, pt(general.t.16.cyn,general.mod.16.cyn$edf,lower=T), pt(general.t.16.cyn,general.mod.16.cyn$edf,lower=F)) # one-tailed p-values
      
      set.seed(219)
      mydata.20$vote.choice.general <- relevel(as.factor(mydata.20$vote.choice.general), ref = "Did not vote")
      general.mod.20.cyn <- multinom(vote.choice.general ~ cyn + rep + ind + pol.int +
                                   ideo7 + therm.demgroups + child.trait + msi +
                                   oppose.trade + econ.retro + income.q +  educ + evangel + black + 
                                   white + female + age, data = mydata.20, Hess = T)
      summary(general.mod.20.cyn)
      general.t.20.cyn <- summary(general.mod.20.cyn)$coefficients/summary(general.mod.20.cyn)$standard.errors # t values
      general.p.20.cyn <- ifelse(general.t.20.cyn < 0, pt(general.t.20.cyn,general.mod.20.cyn$edf,lower=T), pt(general.t.20.cyn,general.mod.20.cyn$edf,lower=F)) # one-tailed p-values
      
      stargazer(general.mod.16.cyn, general.mod.20.cyn, digits = 3,
                dep.var.labels.include = T,
                p = list(general.p.16.cyn, general.p.20.cyn),
                covariate.labels = c("Cynicism", "Republican",
                                     "Independent", "Political Interest", "Ideology", "Democratic-Aligned Group Therm.",
                                     "Child-Rearing Authoritarianism", "Modern Sexism", "Oppose Trade", 
                                     "Retrospective Econ. Assessments", "Income", "Education", "Evangelical", 
                                     "Black", "White", "Female", "Age", "Constant"),
                keep.stat = c("aic","n"))
      
      dim(general.mod.16$fitted.values)
      dim(general.mod.20$fitted.values)
      
      # Inefficacy only
      # Cynicism only 
      set.seed(219)
      mydata.16$vote.choice.general <- relevel(factor(mydata.16$vote.choice.general), ref = "Did not vote")
      general.mod.16.eff <- multinom(vote.choice.general ~ eff + rep + ind + pol.int +
                                       ideo7 + therm.demgroups + child.trait + msi +
                                       oppose.trade + econ.retro + income.q +  educ + evangel + black +
                                       white + female + age, data = mydata.16, Hess = T)
      summary(general.mod.16.eff)
      general.t.16.eff <- summary(general.mod.16.eff)$coefficients/summary(general.mod.16.eff)$standard.errors # t values
      general.p.16.eff <- ifelse(general.t.16.eff < 0, pt(general.t.16.eff,general.mod.16.eff$edf,lower=T), pt(general.t.16.eff,general.mod.16.eff$edf,lower=F)) # one-tailed p-values
      
      set.seed(219)
      mydata.20$vote.choice.general <- relevel(as.factor(mydata.20$vote.choice.general), ref = "Did not vote")
      general.mod.20.eff <- multinom(vote.choice.general ~ eff + rep + ind + pol.int +
                                       ideo7 + therm.demgroups + child.trait + msi +
                                       oppose.trade + econ.retro + income.q +  educ + evangel + black + 
                                       white + female + age, data = mydata.20, Hess = T)
      summary(general.mod.20.eff)
      general.t.20.eff <- summary(general.mod.20.eff)$coefficients/summary(general.mod.20.eff)$standard.errors # t values
      general.p.20.eff <- ifelse(general.t.20.eff < 0, pt(general.t.20.eff,general.mod.20.eff$edf,lower=T), pt(general.t.20.eff,general.mod.20.eff$edf,lower=F)) # one-tailed p-values
      
      stargazer(general.mod.16.eff, general.mod.20.eff, digits = 3,
                dep.var.labels.include = T,
                p = list(general.p.16.eff, general.p.20.eff),
                covariate.labels = c("Inefficacy", "Republican",
                                     "Independent", "Political Interest", "Ideology", "Democratic-Aligned Group Therm.",
                                     "Child-Rearing Authoritarianism", "Modern Sexism", "Oppose Trade", 
                                     "Retrospective Econ. Assessments", "Income", "Education", "Evangelical", 
                                     "Black", "White", "Female", "Age", "Constant"),
                keep.stat = c("aic","n"))
      
      dim(general.mod.16$fitted.values)
      dim(general.mod.20$fitted.values)
      
      #### Predicted Probabilities ----
      # 2016 ----
     votechoice.16.moddf <- mydata.16 %>%
        select(weight, strata, cluster, 
               vote.choice.general, cyn, eff, rep, ind, 
               pol.int, ideo7, therm.demgroups, child.trait, 
               msi, oppose.trade, econ.retro, income.q, educ, 
               evangel, black, white, female, age) %>%
        drop_na()
      
      x <- seq(0,1,0.01)
      votechoice.16.coef.clinton <- votechoice.16.coef %>% slice(seq(1,55,3)) 
      votechoice.16.coef.other <- votechoice.16.coef %>% slice(seq(2,56,3)) 
      votechoice.16.coef.trump <- votechoice.16.coef %>% slice(seq(3,57,3)) 
      
      votechoice.16.vcov.clinton <- votechoice.16.vcov %>% slice(seq(1,55,3)) %>% select(c(seq(1,55,3)))
      votechoice.16.vcov.other <- votechoice.16.vcov %>% slice(seq(2,56,3)) %>% select(c(seq(2,56,3)))
      votechoice.16.vcov.trump <- votechoice.16.vcov %>% slice(seq(3,57,3)) %>% select(c(seq(3,57,3)))
      
      set.seed(219)
      clintonCoef <- mvrnorm(n = 1000, votechoice.16.coef.clinton[,1], votechoice.16.vcov.clinton) # simulate 1000 coefficients
      otherCoef <- mvrnorm(n = 1000, votechoice.16.coef.other[,1], votechoice.16.vcov.other) # simulate 1000 coefficients
      trumpCoef <- mvrnorm(n = 1000, votechoice.16.coef.trump[,1], votechoice.16.vcov.trump) # simulate 1000 coefficients
      
        # Cynicism ----
        temp <- matrix(NA, nrow = 1000, ncol = 4)
        sim.results <- matrix(NA, nrow = 404, ncol = 4)
        general.probdists.cynicism <- matrix(NA, nrow = 1000, ncol = 12)
          
          for(i in 1:length(x)){
            for(j in 1:nrow(clintonCoef)){
              clintonAg <- exp(clintonCoef[j,1] + # clinton aggregator function
                               clintonCoef[j,2]*votechoice.16.moddf$eff +
                               clintonCoef[j,3]*x[i] +
                               clintonCoef[j,4]*votechoice.16.moddf$rep +
                               clintonCoef[j,5]*votechoice.16.moddf$ind +
                               clintonCoef[j,6]*votechoice.16.moddf$pol.int +
                               clintonCoef[j,7]*votechoice.16.moddf$ideo7 +
                               clintonCoef[j,8]*votechoice.16.moddf$therm.demgroups +
                               clintonCoef[j,9]*votechoice.16.moddf$child.trait + 
                               clintonCoef[j,10]*votechoice.16.moddf$msi +
                               clintonCoef[j,11]*votechoice.16.moddf$oppose.trade +
                               clintonCoef[j,12]*votechoice.16.moddf$econ.retro +
                               clintonCoef[j,13]*votechoice.16.moddf$income.q + 
                               clintonCoef[j,14]*votechoice.16.moddf$educ +
                               clintonCoef[j,15]*votechoice.16.moddf$evangel +
                               clintonCoef[j,16]*votechoice.16.moddf$black +
                               clintonCoef[j,17]*votechoice.16.moddf$white +
                               clintonCoef[j,18]*votechoice.16.moddf$female +
                               clintonCoef[j,19]*votechoice.16.moddf$age)
              
              otherAg <- exp(otherCoef[j,1] + # other aggregator function
                               otherCoef[j,2]*votechoice.16.moddf$eff +
                               otherCoef[j,3]*x[i] +
                               otherCoef[j,4]*votechoice.16.moddf$rep +
                               otherCoef[j,5]*votechoice.16.moddf$ind +
                               otherCoef[j,6]*votechoice.16.moddf$pol.int +
                               otherCoef[j,7]*votechoice.16.moddf$ideo7 +
                               otherCoef[j,8]*votechoice.16.moddf$therm.demgroups +
                               otherCoef[j,9]*votechoice.16.moddf$child.trait + 
                               otherCoef[j,10]*votechoice.16.moddf$msi +
                               otherCoef[j,11]*votechoice.16.moddf$oppose.trade +
                               otherCoef[j,12]*votechoice.16.moddf$econ.retro +
                               otherCoef[j,13]*votechoice.16.moddf$income.q + 
                               otherCoef[j,14]*votechoice.16.moddf$educ +
                               otherCoef[j,15]*votechoice.16.moddf$evangel +
                               otherCoef[j,16]*votechoice.16.moddf$black +
                               otherCoef[j,17]*votechoice.16.moddf$white +
                               otherCoef[j,18]*votechoice.16.moddf$female +
                               otherCoef[j,19]*votechoice.16.moddf$age)
              
              trumpAg <- exp(trumpCoef[j,1] + # trump aggregator function
                               trumpCoef[j,2]*votechoice.16.moddf$eff +
                               trumpCoef[j,3]*x[i] +
                               trumpCoef[j,4]*votechoice.16.moddf$rep +
                               trumpCoef[j,5]*votechoice.16.moddf$ind +
                               trumpCoef[j,6]*votechoice.16.moddf$pol.int +
                               trumpCoef[j,7]*votechoice.16.moddf$ideo7 +
                               trumpCoef[j,8]*votechoice.16.moddf$therm.demgroups +
                               trumpCoef[j,9]*votechoice.16.moddf$child.trait + 
                               trumpCoef[j,10]*votechoice.16.moddf$msi +
                               trumpCoef[j,11]*votechoice.16.moddf$oppose.trade +
                               trumpCoef[j,12]*votechoice.16.moddf$econ.retro +
                               trumpCoef[j,13]*votechoice.16.moddf$income.q + 
                               trumpCoef[j,14]*votechoice.16.moddf$educ +
                               trumpCoef[j,15]*votechoice.16.moddf$evangel +
                               trumpCoef[j,16]*votechoice.16.moddf$black +
                               trumpCoef[j,17]*votechoice.16.moddf$white +
                               trumpCoef[j,18]*votechoice.16.moddf$female +
                               trumpCoef[j,19]*votechoice.16.moddf$age)
              
              clinton <- clintonAg / (1 + clintonAg + otherAg + trumpAg) # get probabilities
              other <- otherAg / (1 + clintonAg + otherAg + trumpAg)
              trump <- trumpAg / (1 + clintonAg + otherAg + trumpAg)
              novote <- 1 - clinton - other - trump
              
              temp[j,1] <- mean(clinton) # take mean predicted probability of clinton vote across all observable values, store in j-th row, 1st column
              temp[j,2] <- mean(other)
              temp[j,3] <- mean(trump)
              temp[j,4] <- mean(novote)
              
            }
            
            sim.results[((i*4)-3),1] <- x[i] # capture 'x' value
            #sim.results[((i*4)-3),2] <- quantile(temp[,1], 0.5) 
            sim.results[((i*4)-3),2] <- mean(temp[,1]) # mean of distribution of 1,000 observed-value predicted probabilities (clinton)
            sim.results[((i*4)-3),3] <- quantile(temp[,1], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (clinton)
            sim.results[((i*4)-3),4] <- quantile(temp[,1], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (clinton)
            
            sim.results[((i*4)-2),1] <- x[i] # capture 'x' value
            #sim.results[((i*4)-2),2] <- quantile(temp[,2], 0.5) 
            sim.results[((i*4)-2),2] <- mean(temp[,2]) # mean of distribution of 1,000 observed-value predicted probabilities (Other)
            sim.results[((i*4)-2),3] <- quantile(temp[,2], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Other)
            sim.results[((i*4)-2),4] <- quantile(temp[,2], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Other)
            
            sim.results[((i*4)-1),1] <- x[i] # capture 'x' value
            #sim.results[((i*4)-1),2] <- quantile(temp[,3], 0.5) 
            sim.results[((i*4)-1),2] <- mean(temp[,3]) # mean of distribution of 1,000 observed-value predicted probabilities (Trump)
            sim.results[((i*4)-1),3] <- quantile(temp[,3], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
            sim.results[((i*4)-1),4] <- quantile(temp[,3], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
            
            sim.results[((i*4)),1] <- x[i] # capture 'x' value
            #sim.results[((i*4)),2] <- quantile(temp[,4], 0.5) 
            sim.results[((i*4)),2] <- mean(temp[,4]) # mean of distribution of 1,000 observed-value predicted probabilities (Did not vote)
            sim.results[((i*4)),3] <- quantile(temp[,4], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
            sim.results[((i*4)),4] <- quantile(temp[,4], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
            
            if (x[i] == 0) { # min
              general.probdists.cynicism[,1] <- temp[,1]
              general.probdists.cynicism[,2] <- temp[,2]
              general.probdists.cynicism[,3] <- temp[,3]
              general.probdists.cynicism[,4] <- temp[,4]
            }
            
            if (x[i] == 0.5) { # med
              general.probdists.cynicism[,5] <- temp[,1]
              general.probdists.cynicism[,6] <- temp[,2]
              general.probdists.cynicism[,7] <- temp[,3]
              general.probdists.cynicism[,8] <- temp[,4]
            } 
            
            if (x[i] == 1) { # max
              general.probdists.cynicism[,9] <- temp[,1]
              general.probdists.cynicism[,10] <- temp[,2]
              general.probdists.cynicism[,11] <- temp[,3]
              general.probdists.cynicism[,12] <- temp[,4]
            } 
            
          }
        
        general.effect.cynicism <- as.data.frame(sim.results) # put results in data frame
        colnames(general.effect.cynicism)[1] <- "x"
        colnames(general.effect.cynicism)[2] <- "fit"
        colnames(general.effect.cynicism)[3] <- "lower"
        colnames(general.effect.cynicism)[4] <- "upper"
        general.effect.cynicism$votechoice16 <- rep(c("Clinton","Other","Trump","Did not vote"),101)
        general.effect.cynicism$category <- "Cynicism"
        
        # Inefficacy ----
        temp <- matrix(NA, nrow = 1000, ncol = 4)
        sim.results <- matrix(NA, nrow = 404, ncol = 4)
        general.probdists.inefficacy <- matrix(NA, nrow = 1000, ncol = 12)
        
        for(i in 1:length(x)){
          
          #clintonCoef <- mvrnorm(n = 1000, general.coef[1,], general.covar[1:12,1:12]) # simulate 1000 coefficients
          #trumpCoef <- mvrnorm(n = 1000, general.coef[2,], general.covar[13:24,13:24]) # simulate 1000 coefficients
          #otherCoef <- mvrnorm(n = 1000, general.coef[3,], general.covar[25:36,25:36]) # simulate 1000 coefficients
          
          for(j in 1:nrow(clintonCoef)){
            clintonAg <- exp(clintonCoef[j,1] + # clinton aggregator function
                               clintonCoef[j,2]*x[i] +
                               clintonCoef[j,3]*votechoice.16.moddf$cyn +
                               clintonCoef[j,4]*votechoice.16.moddf$rep +
                               clintonCoef[j,5]*votechoice.16.moddf$ind +
                               clintonCoef[j,6]*votechoice.16.moddf$pol.int +
                               clintonCoef[j,7]*votechoice.16.moddf$ideo7 +
                               clintonCoef[j,8]*votechoice.16.moddf$therm.demgroups +
                               clintonCoef[j,9]*votechoice.16.moddf$child.trait + 
                               clintonCoef[j,10]*votechoice.16.moddf$msi +
                               clintonCoef[j,11]*votechoice.16.moddf$oppose.trade +
                               clintonCoef[j,12]*votechoice.16.moddf$econ.retro +
                               clintonCoef[j,13]*votechoice.16.moddf$income.q + 
                               clintonCoef[j,14]*votechoice.16.moddf$educ +
                               clintonCoef[j,15]*votechoice.16.moddf$evangel +
                               clintonCoef[j,16]*votechoice.16.moddf$black +
                               clintonCoef[j,17]*votechoice.16.moddf$white +
                               clintonCoef[j,18]*votechoice.16.moddf$female +
                               clintonCoef[j,19]*votechoice.16.moddf$age)
            
            otherAg <- exp(otherCoef[j,1] + # other aggregator function
                             otherCoef[j,2]*x[i] +
                             otherCoef[j,3]*votechoice.16.moddf$cyn +
                             otherCoef[j,4]*votechoice.16.moddf$rep +
                             otherCoef[j,5]*votechoice.16.moddf$ind +
                             otherCoef[j,6]*votechoice.16.moddf$pol.int +
                             otherCoef[j,7]*votechoice.16.moddf$ideo7 +
                             otherCoef[j,8]*votechoice.16.moddf$therm.demgroups +
                             otherCoef[j,9]*votechoice.16.moddf$child.trait + 
                             otherCoef[j,10]*votechoice.16.moddf$msi +
                             otherCoef[j,11]*votechoice.16.moddf$oppose.trade +
                             otherCoef[j,12]*votechoice.16.moddf$econ.retro +
                             otherCoef[j,13]*votechoice.16.moddf$income.q + 
                             otherCoef[j,14]*votechoice.16.moddf$educ +
                             otherCoef[j,15]*votechoice.16.moddf$evangel +
                             otherCoef[j,16]*votechoice.16.moddf$black +
                             otherCoef[j,17]*votechoice.16.moddf$white +
                             otherCoef[j,18]*votechoice.16.moddf$female +
                             otherCoef[j,19]*votechoice.16.moddf$age)
            
            trumpAg <- exp(trumpCoef[j,1] + # trump aggregator function
                             trumpCoef[j,2]*x[i] +
                             trumpCoef[j,3]*votechoice.16.moddf$cyn +
                             trumpCoef[j,4]*votechoice.16.moddf$rep +
                             trumpCoef[j,5]*votechoice.16.moddf$ind +
                             trumpCoef[j,6]*votechoice.16.moddf$pol.int +
                             trumpCoef[j,7]*votechoice.16.moddf$ideo7 +
                             trumpCoef[j,8]*votechoice.16.moddf$therm.demgroups +
                             trumpCoef[j,9]*votechoice.16.moddf$child.trait + 
                             trumpCoef[j,10]*votechoice.16.moddf$msi +
                             trumpCoef[j,11]*votechoice.16.moddf$oppose.trade +
                             trumpCoef[j,12]*votechoice.16.moddf$econ.retro +
                             trumpCoef[j,13]*votechoice.16.moddf$income.q + 
                             trumpCoef[j,14]*votechoice.16.moddf$educ +
                             trumpCoef[j,15]*votechoice.16.moddf$evangel +
                             trumpCoef[j,16]*votechoice.16.moddf$black +
                             trumpCoef[j,17]*votechoice.16.moddf$white +
                             trumpCoef[j,18]*votechoice.16.moddf$female +
                             trumpCoef[j,19]*votechoice.16.moddf$age)
            
            clinton <- clintonAg / (1 + clintonAg + otherAg + trumpAg) # get probabilities
            other <- otherAg / (1 + clintonAg + otherAg + trumpAg)
            trump <- trumpAg / (1 + clintonAg + otherAg + trumpAg)
            novote <- 1 - clinton - other - trump
            
            temp[j,1] <- mean(clinton) # take mean predicted probability of clinton vote across all observable values, store in j-th row, 1st column
            temp[j,2] <- mean(other)
            temp[j,3] <- mean(trump)
            temp[j,4] <- mean(novote)
            
          }
          
          sim.results[((i*4)-3),1] <- x[i] # capture 'x' value
          #sim.results[((i*4)-3),2] <- quantile(temp[,1], 0.5) 
          sim.results[((i*4)-3),2] <- mean(temp[,1]) # mean of distribution of 1,000 observed-value predicted probabilities (Clinton)
          sim.results[((i*4)-3),3] <- quantile(temp[,1], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Clinton)
          sim.results[((i*4)-3),4] <- quantile(temp[,1], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Clinton)
          
          sim.results[((i*4)-2),1] <- x[i] # capture 'x' value
          #sim.results[((i*4)-2),2] <- quantile(temp[,2], 0.5) 
          sim.results[((i*4)-2),2] <- mean(temp[,2]) # mean of distribution of 1,000 observed-value predicted probabilities (Other)
          sim.results[((i*4)-2),3] <- quantile(temp[,2], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Other)
          sim.results[((i*4)-2),4] <- quantile(temp[,2], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Other)
          
          sim.results[((i*4)-1),1] <- x[i] # capture 'x' value
          #sim.results[((i*4)-1),2] <- quantile(temp[,3], 0.5) 
          sim.results[((i*4)-1),2] <- mean(temp[,3]) # mean of distribution of 1,000 observed-value predicted probabilities (Trump)
          sim.results[((i*4)-1),3] <- quantile(temp[,3], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
          sim.results[((i*4)-1),4] <- quantile(temp[,3], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
          
          sim.results[((i*4)),1] <- x[i] # capture 'x' value
          #sim.results[((i*4)),2] <- quantile(temp[,4], 0.5) 
          sim.results[((i*4)),2] <- mean(temp[,4]) # mean of distribution of 1,000 observed-value predicted probabilities (Did not vote)
          sim.results[((i*4)),3] <- quantile(temp[,4], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
          sim.results[((i*4)),4] <- quantile(temp[,4], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
          
          if (x[i] == 0) { # min
            general.probdists.inefficacy[,1] <- temp[,1]
            general.probdists.inefficacy[,2] <- temp[,2]
            general.probdists.inefficacy[,3] <- temp[,3]
            general.probdists.inefficacy[,4] <- temp[,4]
          }
          
          if (x[i] == 0.5) { # med
            general.probdists.inefficacy[,5] <- temp[,1]
            general.probdists.inefficacy[,6] <- temp[,2]
            general.probdists.inefficacy[,7] <- temp[,3]
            general.probdists.inefficacy[,8] <- temp[,4]
          } 
          
          if (x[i] == 1) { # max
            general.probdists.inefficacy[,9] <- temp[,1]
            general.probdists.inefficacy[,10] <- temp[,2]
            general.probdists.inefficacy[,11] <- temp[,3]
            general.probdists.inefficacy[,12] <- temp[,4]
          } 
          
        }
        
        general.effect.inefficacy <- as.data.frame(sim.results) # put results in data frame
        colnames(general.effect.inefficacy)[1] <- "x"
        colnames(general.effect.inefficacy)[2] <- "fit"
        colnames(general.effect.inefficacy)[3] <- "lower"
        colnames(general.effect.inefficacy)[4] <- "upper"
        general.effect.inefficacy$votechoice16 <- rep(c("Clinton","Other","Trump","Did not vote"),101)
        general.effect.inefficacy$category <- "Inefficacy"
        
        
        
        # Combine results and plot ----
        general.effects <- rbind(general.effect.cynicism, general.effect.inefficacy)
        general.effects$votechoice16 <- factor(general.effects$votechoice16, levels = c("Clinton","Other","Trump", "Did not vote"))
        
        pdf(height = 12, width = 12, "Figures/Probs-General.pdf")
        ggplot(general.effects, aes(x = x, y = fit, colour = votechoice16)) +
          geom_line(size = 2) +
          geom_line(aes(y = lower), lty = "dashed") +
          geom_line(aes(y = upper), lty = "dashed") +
          labs(x = "", y = "Probability of Vote Choice") +
          facet_grid(rows = vars(votechoice16), cols = vars(category), scales = "free_y") +
          scale_colour_manual("", values = c("#377EB8","#E41A1C","#4DAF4A","#984EA3")) +
          scale_x_continuous(breaks = c(0,1,2)) +
          theme(axis.line = element_line(colour = "black"),
                plot.subtitle = element_text(vjust = 1), 
                plot.caption = element_text(vjust = 1), 
                plot.margin = margin(0.5,3,0.5,0.5,"cm"),
                legend.text = element_text(size = 18),
                legend.position = "bottom",
                panel.background = element_rect(fill = "white", colour = "black", size = 1),
                panel.grid.major = element_line(linetype = "blank"), 
                panel.grid.minor = element_line(linetype = "blank"),
                axis.title = element_text(size = 22),
                axis.text = element_text(size = 18),
                strip.text = element_text(size = 18))
        dev.off()
        
        # Differences in Predicted Probabilities ----
        general.probdists.cynicism <- as.data.frame(general.probdists.cynicism)
        general.probdists.inefficacy <- as.data.frame(general.probdists.inefficacy)
        colnames(general.probdists.cynicism) <- c("clinton.lo","other.lo","trump.lo","dnv.lo",
                                                  "clinton.mid","other.mid","trump.mid","dnv.mid",
                                                  "clinton.hi","other.hi","trump.hi","dnv.hi")
        colnames(general.probdists.inefficacy) <- c("clinton.lo","other.lo","trump.lo","dnv.lo",
                                                    "clinton.mid","other.mid","trump.mid","dnv.mid",
                                                    "clinton.hi","other.hi","trump.hi","dnv.hi")
        
        # Cynicism
        round(quantile(general.probdists.cynicism$clinton.hi - general.probdists.cynicism$clinton.lo, c(0.05, 0.5, 0.95)),3) # -14.5, [-26.1, -4.3]*
        round(quantile(general.probdists.cynicism$trump.hi - general.probdists.cynicism$trump.lo, c(0.05, 0.5, 0.95)),3)  # 11.5, [0.6, 21.3]*
        round(quantile(general.probdists.cynicism$other.hi - general.probdists.cynicism$other.lo, c(0.05, 0.5, 0.95)),3)  # 6.5, [-0.7, 12.6]
        round(quantile(general.probdists.cynicism$dnv.hi - general.probdists.cynicism$dnv.lo, c(0.05, 0.5, 0.95)),3) # -2.8, [-10.4, 4.1]
        
        # Inefficacy
        round(quantile(general.probdists.inefficacy$clinton.hi - general.probdists.inefficacy$clinton.lo, c(0.05, 0.5, 0.95)),3) # -3.7, [-10.8, 4.0]
        round(quantile(general.probdists.inefficacy$trump.hi - general.probdists.inefficacy$trump.lo, c(0.05, 0.5, 0.95)),3) # -2.4, [-9.1, 3.9]
        round(quantile(general.probdists.inefficacy$other.hi - general.probdists.inefficacy$other.lo, c(0.05, 0.5, 0.95)),3) # -0.5, [-5.4, 4.2]
        round(quantile(general.probdists.inefficacy$dnv.hi - general.probdists.inefficacy$dnv.lo, c(0.05, 0.5, 0.95)),3) # 6.7, [2.4, 10.6]
        
        
        
        
      # 2020 ----
        votechoice.20.moddf <- mydata.20 %>%
          select(weight, strata, cluster, 
                 vote.choice.general, cyn, eff, rep, ind, 
                 pol.int, ideo7, therm.demgroups, child.trait, 
                 msi, oppose.trade, econ.retro, income.q, educ, 
                 evangel, black, white, female, age) %>%
          drop_na()
        
        x <- seq(0,1,0.01)
        votechoice.20.coef.biden <- votechoice.20.coef %>% slice(seq(1,55,3)) 
        votechoice.20.coef.other <- votechoice.20.coef %>% slice(seq(2,56,3)) 
        votechoice.20.coef.trump <- votechoice.20.coef %>% slice(seq(3,57,3)) 
        
        votechoice.20.vcov.biden <- votechoice.20.vcov %>% slice(seq(1,55,3)) %>% select(c(seq(1,55,3)))
        votechoice.20.vcov.other <- votechoice.20.vcov %>% slice(seq(2,56,3)) %>% select(c(seq(2,56,3)))
        votechoice.20.vcov.trump <- votechoice.20.vcov %>% slice(seq(3,57,3)) %>% select(c(seq(3,57,3)))
        
        set.seed(219)
        bidenCoef <- mvrnorm(n = 1000, votechoice.20.coef.biden[,1], votechoice.20.vcov.biden) # simulate 1000 coefficients
        otherCoef <- mvrnorm(n = 1000, votechoice.20.coef.other[,1], votechoice.20.vcov.other) # simulate 1000 coefficients
        trumpCoef <- mvrnorm(n = 1000, votechoice.20.coef.trump[,1], votechoice.20.vcov.trump) # simulate 1000 coefficients
      
        # Cynicism ----
        temp <- matrix(NA, nrow = 1000, ncol = 4)
        sim.results <- matrix(NA, nrow = 404, ncol = 4)
        general.probdists.cynicism <- matrix(NA, nrow = 1000, ncol = 12)
        
        for(i in 1:length(x)){
          for(j in 1:nrow(bidenCoef)){
            bidenAg <- exp(bidenCoef[j,1] + # biden aggregator function
                             bidenCoef[j,2]*votechoice.20.moddf$eff +
                             bidenCoef[j,3]*x[i] +
                             bidenCoef[j,4]*votechoice.20.moddf$rep +
                             bidenCoef[j,5]*votechoice.20.moddf$ind +
                             bidenCoef[j,6]*votechoice.20.moddf$pol.int +
                             bidenCoef[j,7]*votechoice.20.moddf$ideo7 +
                             bidenCoef[j,8]*votechoice.20.moddf$therm.demgroups +
                             bidenCoef[j,9]*votechoice.20.moddf$child.trait + 
                             bidenCoef[j,10]*votechoice.20.moddf$msi +
                             bidenCoef[j,11]*votechoice.20.moddf$oppose.trade +
                             bidenCoef[j,12]*votechoice.20.moddf$econ.retro +
                             bidenCoef[j,13]*votechoice.20.moddf$income.q + 
                             bidenCoef[j,14]*votechoice.20.moddf$educ +
                             bidenCoef[j,15]*votechoice.20.moddf$evangel +
                             bidenCoef[j,16]*votechoice.20.moddf$black +
                             bidenCoef[j,17]*votechoice.20.moddf$white +
                             bidenCoef[j,18]*votechoice.20.moddf$female +
                             bidenCoef[j,19]*votechoice.20.moddf$age)
            
            otherAg <- exp(otherCoef[j,1] + # other aggregator function
                             otherCoef[j,2]*votechoice.20.moddf$eff +
                             otherCoef[j,3]*x[i] +
                             otherCoef[j,4]*votechoice.20.moddf$rep +
                             otherCoef[j,5]*votechoice.20.moddf$ind +
                             otherCoef[j,6]*votechoice.20.moddf$pol.int +
                             otherCoef[j,7]*votechoice.20.moddf$ideo7 +
                             otherCoef[j,8]*votechoice.20.moddf$therm.demgroups +
                             otherCoef[j,9]*votechoice.20.moddf$child.trait + 
                             otherCoef[j,10]*votechoice.20.moddf$msi +
                             otherCoef[j,11]*votechoice.20.moddf$oppose.trade +
                             otherCoef[j,12]*votechoice.20.moddf$econ.retro +
                             otherCoef[j,13]*votechoice.20.moddf$income.q + 
                             otherCoef[j,14]*votechoice.20.moddf$educ +
                             otherCoef[j,15]*votechoice.20.moddf$evangel +
                             otherCoef[j,16]*votechoice.20.moddf$black +
                             otherCoef[j,17]*votechoice.20.moddf$white +
                             otherCoef[j,18]*votechoice.20.moddf$female +
                             otherCoef[j,19]*votechoice.20.moddf$age)
            
            trumpAg <- exp(trumpCoef[j,1] + # trump aggregator function
                             trumpCoef[j,2]*votechoice.20.moddf$eff +
                             trumpCoef[j,3]*x[i] +
                             trumpCoef[j,4]*votechoice.20.moddf$rep +
                             trumpCoef[j,5]*votechoice.20.moddf$ind +
                             trumpCoef[j,6]*votechoice.20.moddf$pol.int +
                             trumpCoef[j,7]*votechoice.20.moddf$ideo7 +
                             trumpCoef[j,8]*votechoice.20.moddf$therm.demgroups +
                             trumpCoef[j,9]*votechoice.20.moddf$child.trait + 
                             trumpCoef[j,10]*votechoice.20.moddf$msi +
                             trumpCoef[j,11]*votechoice.20.moddf$oppose.trade +
                             trumpCoef[j,12]*votechoice.20.moddf$econ.retro +
                             trumpCoef[j,13]*votechoice.20.moddf$income.q + 
                             trumpCoef[j,14]*votechoice.20.moddf$educ +
                             trumpCoef[j,15]*votechoice.20.moddf$evangel +
                             trumpCoef[j,16]*votechoice.20.moddf$black +
                             trumpCoef[j,17]*votechoice.20.moddf$white +
                             trumpCoef[j,18]*votechoice.20.moddf$female +
                             trumpCoef[j,19]*votechoice.20.moddf$age)
            
            biden <- bidenAg / (1 + bidenAg + otherAg + trumpAg) # get probabilities
            other <- otherAg / (1 + bidenAg + otherAg + trumpAg)
            trump <- trumpAg / (1 + bidenAg + otherAg + trumpAg)
            novote <- 1 - biden - other - trump
            
            temp[j,1] <- mean(biden) # take mean predicted probability of biden vote across all observable values, store in j-th row, 1st column
            temp[j,2] <- mean(other)
            temp[j,3] <- mean(trump)
            temp[j,4] <- mean(novote)
            
          }
          
          sim.results[((i*4)-3),1] <- x[i] # capture 'x' value
          #sim.results[((i*4)-3),2] <- quantile(temp[,1], 0.5) 
          sim.results[((i*4)-3),2] <- mean(temp[,1]) # mean of distribution of 1,000 observed-value predicted probabilities (Biden)
          sim.results[((i*4)-3),3] <- quantile(temp[,1], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Biden)
          sim.results[((i*4)-3),4] <- quantile(temp[,1], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Biden)
          
          sim.results[((i*4)-2),1] <- x[i] # capture 'x' value
          #sim.results[((i*4)-2),2] <- quantile(temp[,2], 0.5) 
          sim.results[((i*4)-2),2] <- mean(temp[,2]) # mean of distribution of 1,000 observed-value predicted probabilities (Other)
          sim.results[((i*4)-2),3] <- quantile(temp[,2], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Other)
          sim.results[((i*4)-2),4] <- quantile(temp[,2], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Other)
          
          sim.results[((i*4)-1),1] <- x[i] # capture 'x' value
          #sim.results[((i*4)-1),2] <- quantile(temp[,3], 0.5) 
          sim.results[((i*4)-1),2] <- mean(temp[,3]) # mean of distribution of 1,000 observed-value predicted probabilities (Trump)
          sim.results[((i*4)-1),3] <- quantile(temp[,3], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
          sim.results[((i*4)-1),4] <- quantile(temp[,3], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
          
          sim.results[((i*4)),1] <- x[i] # capture 'x' value
          #sim.results[((i*4)),2] <- quantile(temp[,4], 0.5) 
          sim.results[((i*4)),2] <- mean(temp[,4]) # mean of distribution of 1,000 observed-value predicted probabilities (Did not vote)
          sim.results[((i*4)),3] <- quantile(temp[,4], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
          sim.results[((i*4)),4] <- quantile(temp[,4], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
          
          if (x[i] == 0) { # min
            general.probdists.cynicism[,1] <- temp[,1]
            general.probdists.cynicism[,2] <- temp[,2]
            general.probdists.cynicism[,3] <- temp[,3]
            general.probdists.cynicism[,4] <- temp[,4]
          }
          
          if (x[i] == 0.5) { # med
            general.probdists.cynicism[,5] <- temp[,1]
            general.probdists.cynicism[,6] <- temp[,2]
            general.probdists.cynicism[,7] <- temp[,3]
            general.probdists.cynicism[,8] <- temp[,4]
          } 
          
          if (x[i] == 1) { # max
            general.probdists.cynicism[,9] <- temp[,1]
            general.probdists.cynicism[,10] <- temp[,2]
            general.probdists.cynicism[,11] <- temp[,3]
            general.probdists.cynicism[,12] <- temp[,4]
          } 
          
        }
        
        general.effect.cynicism <- as.data.frame(sim.results) # put results in data frame
        colnames(general.effect.cynicism)[1] <- "x"
        colnames(general.effect.cynicism)[2] <- "fit"
        colnames(general.effect.cynicism)[3] <- "lower"
        colnames(general.effect.cynicism)[4] <- "upper"
        general.effect.cynicism$votechoice16 <- rep(c("Biden","Other","Trump","Did not vote"),101)
        general.effect.cynicism$category <- "Cynicism"
        
        # Inefficacy ----
        temp <- matrix(NA, nrow = 1000, ncol = 4)
        sim.results <- matrix(NA, nrow = 404, ncol = 4)
        general.probdists.inefficacy <- matrix(NA, nrow = 1000, ncol = 12)
        
        for(i in 1:length(x)){
          for(j in 1:nrow(bidenCoef)){
            bidenAg <- exp(bidenCoef[j,1] + # biden aggregator function
                             bidenCoef[j,2]*x[i] +
                             bidenCoef[j,3]*votechoice.20.moddf$cyn +
                             bidenCoef[j,4]*votechoice.20.moddf$rep +
                             bidenCoef[j,5]*votechoice.20.moddf$ind +
                             bidenCoef[j,6]*votechoice.20.moddf$pol.int +
                             bidenCoef[j,7]*votechoice.20.moddf$ideo7 +
                             bidenCoef[j,8]*votechoice.20.moddf$therm.demgroups +
                             bidenCoef[j,9]*votechoice.20.moddf$child.trait + 
                             bidenCoef[j,10]*votechoice.20.moddf$msi +
                             bidenCoef[j,11]*votechoice.20.moddf$oppose.trade +
                             bidenCoef[j,12]*votechoice.20.moddf$econ.retro +
                             bidenCoef[j,13]*votechoice.20.moddf$income.q + 
                             bidenCoef[j,14]*votechoice.20.moddf$educ +
                             bidenCoef[j,15]*votechoice.20.moddf$evangel +
                             bidenCoef[j,16]*votechoice.20.moddf$black +
                             bidenCoef[j,17]*votechoice.20.moddf$white +
                             bidenCoef[j,18]*votechoice.20.moddf$female +
                             bidenCoef[j,19]*votechoice.20.moddf$age)
            
            otherAg <- exp(otherCoef[j,1] + # other aggregator function
                             otherCoef[j,2]*x[i] +
                             otherCoef[j,3]*votechoice.20.moddf$cyn +
                             otherCoef[j,4]*votechoice.20.moddf$rep +
                             otherCoef[j,5]*votechoice.20.moddf$ind +
                             otherCoef[j,6]*votechoice.20.moddf$pol.int +
                             otherCoef[j,7]*votechoice.20.moddf$ideo7 +
                             otherCoef[j,8]*votechoice.20.moddf$therm.demgroups +
                             otherCoef[j,9]*votechoice.20.moddf$child.trait + 
                             otherCoef[j,10]*votechoice.20.moddf$msi +
                             otherCoef[j,11]*votechoice.20.moddf$oppose.trade +
                             otherCoef[j,12]*votechoice.20.moddf$econ.retro +
                             otherCoef[j,13]*votechoice.20.moddf$income.q + 
                             otherCoef[j,14]*votechoice.20.moddf$educ +
                             otherCoef[j,15]*votechoice.20.moddf$evangel +
                             otherCoef[j,16]*votechoice.20.moddf$black +
                             otherCoef[j,17]*votechoice.20.moddf$white +
                             otherCoef[j,18]*votechoice.20.moddf$female +
                             otherCoef[j,19]*votechoice.20.moddf$age)
            
            trumpAg <- exp(trumpCoef[j,1] + # trump aggregator function
                             trumpCoef[j,2]*x[i] +
                             trumpCoef[j,3]*votechoice.20.moddf$cyn +
                             trumpCoef[j,4]*votechoice.20.moddf$rep +
                             trumpCoef[j,5]*votechoice.20.moddf$ind +
                             trumpCoef[j,6]*votechoice.20.moddf$pol.int +
                             trumpCoef[j,7]*votechoice.20.moddf$ideo7 +
                             trumpCoef[j,8]*votechoice.20.moddf$therm.demgroups +
                             trumpCoef[j,9]*votechoice.20.moddf$child.trait + 
                             trumpCoef[j,10]*votechoice.20.moddf$msi +
                             trumpCoef[j,11]*votechoice.20.moddf$oppose.trade +
                             trumpCoef[j,12]*votechoice.20.moddf$econ.retro +
                             trumpCoef[j,13]*votechoice.20.moddf$income.q + 
                             trumpCoef[j,14]*votechoice.20.moddf$educ +
                             trumpCoef[j,15]*votechoice.20.moddf$evangel +
                             trumpCoef[j,16]*votechoice.20.moddf$black +
                             trumpCoef[j,17]*votechoice.20.moddf$white +
                             trumpCoef[j,18]*votechoice.20.moddf$female +
                             trumpCoef[j,19]*votechoice.20.moddf$age)
            
            biden <- bidenAg / (1 + bidenAg + otherAg + trumpAg) # get probabilities
            other <- otherAg / (1 + bidenAg + otherAg + trumpAg)
            trump <- trumpAg / (1 + bidenAg + otherAg + trumpAg)
            novote <- 1 - biden - other - trump
            
            temp[j,1] <- mean(biden) # take mean predicted probability of biden vote across all observable values, store in j-th row, 1st column
            temp[j,2] <- mean(other)
            temp[j,3] <- mean(trump)
            temp[j,4] <- mean(novote)
            
          }
          
          sim.results[((i*4)-3),1] <- x[i] # capture 'x' value
          #sim.results[((i*4)-3),2] <- quantile(temp[,1], 0.5) 
          sim.results[((i*4)-3),2] <- mean(temp[,1]) # mean of distribution of 1,000 observed-value predicted probabilities (Biden)
          sim.results[((i*4)-3),3] <- quantile(temp[,1], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Biden)
          sim.results[((i*4)-3),4] <- quantile(temp[,1], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Biden)
          
          sim.results[((i*4)-2),1] <- x[i] # capture 'x' value
          #sim.results[((i*4)-2),2] <- quantile(temp[,2], 0.5) 
          sim.results[((i*4)-2),2] <- mean(temp[,2]) # mean of distribution of 1,000 observed-value predicted probabilities (Other)
          sim.results[((i*4)-2),3] <- quantile(temp[,2], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Other)
          sim.results[((i*4)-2),4] <- quantile(temp[,2], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Other)
          
          sim.results[((i*4)-1),1] <- x[i] # capture 'x' value
          #sim.results[((i*4)-1),2] <- quantile(temp[,3], 0.5) 
          sim.results[((i*4)-1),2] <- mean(temp[,3]) # mean of distribution of 1,000 observed-value predicted probabilities (Trump)
          sim.results[((i*4)-1),3] <- quantile(temp[,3], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
          sim.results[((i*4)-1),4] <- quantile(temp[,3], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
          
          sim.results[((i*4)),1] <- x[i] # capture 'x' value
          #sim.results[((i*4)),2] <- quantile(temp[,4], 0.5) 
          sim.results[((i*4)),2] <- mean(temp[,4]) # mean of distribution of 1,000 observed-value predicted probabilities (Did not vote)
          sim.results[((i*4)),3] <- quantile(temp[,4], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
          sim.results[((i*4)),4] <- quantile(temp[,4], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
          
          if (x[i] == 0) { # min
            general.probdists.inefficacy[,1] <- temp[,1]
            general.probdists.inefficacy[,2] <- temp[,2]
            general.probdists.inefficacy[,3] <- temp[,3]
            general.probdists.inefficacy[,4] <- temp[,4]
          }
          
          if (x[i] == 0.5) { # med
            general.probdists.inefficacy[,5] <- temp[,1]
            general.probdists.inefficacy[,6] <- temp[,2]
            general.probdists.inefficacy[,7] <- temp[,3]
            general.probdists.inefficacy[,8] <- temp[,4]
          } 
          
          if (x[i] == 1) { # max
            general.probdists.inefficacy[,9] <- temp[,1]
            general.probdists.inefficacy[,10] <- temp[,2]
            general.probdists.inefficacy[,11] <- temp[,3]
            general.probdists.inefficacy[,12] <- temp[,4]
          } 
          
        }
        
        general.effect.inefficacy <- as.data.frame(sim.results) # put results in data frame
        colnames(general.effect.inefficacy)[1] <- "x"
        colnames(general.effect.inefficacy)[2] <- "fit"
        colnames(general.effect.inefficacy)[3] <- "lower"
        colnames(general.effect.inefficacy)[4] <- "upper"
        general.effect.inefficacy$votechoice16 <- rep(c("Biden","Other","Trump","Did not vote"),101)
        general.effect.inefficacy$category <- "Inefficacy"
        
        
        
        # Combine results and plot ----
        general.effects <- rbind(general.effect.cynicism, general.effect.inefficacy)
        general.effects$votechoice16 <- factor(general.effects$votechoice16, levels = c("Biden","Other","Trump", "Did not vote"))
        
        pdf(height = 12, width = 12, "Figures/Probs-General.pdf")
        ggplot(general.effects, aes(x = x, y = fit, colour = votechoice16)) +
          geom_line(size = 2) +
          geom_line(aes(y = lower), lty = "dashed") +
          geom_line(aes(y = upper), lty = "dashed") +
          labs(x = "", y = "Probability of Vote Choice") +
          facet_grid(rows = vars(votechoice16), cols = vars(category), scales = "free_y") +
          scale_colour_manual("", values = c("#377EB8","#E41A1C","#4DAF4A","#984EA3")) +
          scale_x_continuous(breaks = c(0,1,2)) +
          theme(axis.line = element_line(colour = "black"),
                plot.subtitle = element_text(vjust = 1), 
                plot.caption = element_text(vjust = 1), 
                plot.margin = margin(0.5,3,0.5,0.5,"cm"),
                legend.text = element_text(size = 18),
                legend.position = "bottom",
                panel.background = element_rect(fill = "white", colour = "black", size = 1),
                panel.grid.major = element_line(linetype = "blank"), 
                panel.grid.minor = element_line(linetype = "blank"),
                axis.title = element_text(size = 22),
                axis.text = element_text(size = 18),
                strip.text = element_text(size = 18))
        dev.off()
        
        # Differences in Predicted Probabilities ----
        general.probdists.cynicism <- as.data.frame(general.probdists.cynicism)
        general.probdists.inefficacy <- as.data.frame(general.probdists.inefficacy)
        colnames(general.probdists.cynicism) <- c("biden.lo","other.lo","trump.lo","dnv.lo",
                                                  "biden.mid","other.mid","trump.mid","dnv.mid",
                                                  "biden.hi","other.hi","trump.hi","dnv.hi")
        colnames(general.probdists.inefficacy) <- c("biden.lo","other.lo","trump.lo","dnv.lo",
                                                    "biden.mid","other.mid","trump.mid","dnv.mid",
                                                    "biden.hi","other.hi","trump.hi","dnv.hi")
        
        # Cynicism
        round(quantile(general.probdists.cynicism$biden.hi - general.probdists.cynicism$biden.lo, c(0.05, 0.5, 0.95)),3) # -4.2, [-12.0, 3.7]
        round(quantile(general.probdists.cynicism$trump.hi - general.probdists.cynicism$trump.lo, c(0.05, 0.5, 0.95)),3)  # 2.0, [-4.9, 9.0]
        round(quantile(general.probdists.cynicism$other.hi - general.probdists.cynicism$other.lo, c(0.05, 0.5, 0.95)),3)  # 4.6, [2.4, 8.3]*
        round(quantile(general.probdists.cynicism$dnv.hi - general.probdists.cynicism$dnv.lo, c(0.05, 0.5, 0.95)),3) # -2.7, [-8.1, 2.3]
        
        # Inefficacy
        round(quantile(general.probdists.inefficacy$biden.hi - general.probdists.inefficacy$biden.lo, c(0.05, 0.5, 0.95)),3) # -11.7, [-18.6, -5.6]
        round(quantile(general.probdists.inefficacy$trump.hi - general.probdists.inefficacy$trump.lo, c(0.05, 0.5, 0.95)),3) # 1.8, [-3.9, 7.3]
        round(quantile(general.probdists.inefficacy$other.hi - general.probdists.inefficacy$other.lo, c(0.05, 0.5, 0.95)),3) # 0.8, [-2.1, 3.3]
        round(quantile(general.probdists.inefficacy$dnv.hi - general.probdists.inefficacy$dnv.lo, c(0.05, 0.5, 0.95)),3) # 9.4, [6.0, 12.8]
        
        
        
        

    
    

