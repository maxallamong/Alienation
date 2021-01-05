# Maxwell B. Allamong
# Alie(n)ation: Political Outsiders in the 2016 Election - Vote Models
# Created: Mar. 12th, 2020
# Updated: Jan. 3rd, 2020

# READ ME ----
#  To replicate this analysis, first open the R Project file (Alienation.RProj).
#  Next, simply run this entire script: it will automatically set the working directory to the
#  folder that contains the R Project file, and then it will source the data from the source
#  file (Alienation-Source.R).

# Source Data ----
  source("Alienation-Source.R")

# Models ----
  # Turnout ----
  
    # 2016 (LPM) ----
    turnout.16 <- lm(vote16 ~ alien.cynicism * elect.attn + party.strength + educ  + ind +
                       income.q + white + age,
                     data = mydata.16)
    summary(turnout.16)

    plot_model(turnout.16, type = "int") + # plot
      labs(x = "Cynicism", y = "Probability of Turnout in 2016 General Election", title = "") +
      scale_colour_discrete(name = "Elections Make Gov. Pay Attention?", labels = c("Responsive", "Unresponsive")) +
      theme(axis.text = element_text(size = 22),
            axis.title = element_text(size = 26),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20),
            plot.margin = margin(1,1,1,1,"cm"),
            panel.border = element_rect(color="black", fill=NA),
            #panel.grid.major = element_blank(),
            #panel.grid.minor = element_blank(),
            plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
            strip.background = element_rect(fill="white", color="black"))

    stargazer(turnout.16, digits = 3, # table
              intercept.bottom = T, dep.var.labels.include = F,
              covariate.labels = c("Normlessness", "Gov. Responsiveness", "Partisan Strength",
                                   "Education", "Independent", "Income", "White", "Age",
                                   "Normlessness \\times Gov. Responsiveness", "Constant"),
              column.labels = c("1988", "1992", "1996","2000","2004","2008","2012", "2016"),
              keep.stat = c("n","aic"))


    # Maringal Effects Plot
    beta.hat <- coef(turnout.16)  # marginal effects
    cov <- vcov(turnout.16)
    z0 <- seq(min(mydata.16$elect.attn, na.rm = T), max(mydata.16$elect.attn, na.rm = T), length.out = 1000)
    dy.dx <- beta.hat["alien.cynicism"] + beta.hat["alien.cynicism:elect.attn"]*z0
    se.dy.dx <- sqrt(cov["alien.cynicism", "alien.cynicism"] +
                       z0^2*cov["alien.cynicism:elect.attn", "alien.cynicism:elect.attn"] +
                       2*z0*cov["alien.cynicism", "alien.cynicism:elect.attn"])
    upr <- dy.dx + 1.96*se.dy.dx
    lwr <- dy.dx - 1.96*se.dy.dx

    par(family="serif",bty="l",mar=c(5,5.5,2,2))
    plot(x=z0, y=dy.dx,type="n",xlim=c(min(z0),max(z0)),
         ylim=c(min(lwr),max(upr)),
         xlab = "Unresponsiveness",
         ylab = expression(frac(partialdiff*paste("Turnout"),
                                partialdiff*paste("Cynicism"))),
         main="Marginal Effect of Cynicism on Turnout as Unresponsiveness Varies")
    lines(z0, dy.dx, lwd = 3)
    lines(z0, lwr)
    lines(z0, upr)
    abline(h=0,lty=2)
    
    # 1988-2016 (Logit) ----
    turnout.16 <- glm(vote16 ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 2016
                        income.q + white + age, 
                      data = mydata.16, family = "binomial")
    summary(turnout.16)
    turnout.16.df <- data.frame(t = coef(turnout.16) / sqrt(diag(vcov(turnout.16))),
                                df = turnout.16$df.null)
    turnout.16.df <- turnout.16.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
    
    turnout.12 <- glm(vote12 ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 2012
                        income.q + white + age, 
                      data = mydata.12, family = "binomial")
    summary(turnout.12)
    turnout.12.df <- data.frame(t = coef(turnout.12) / sqrt(diag(vcov(turnout.12))),
                                df = turnout.12$df.null)
    turnout.12.df <- turnout.12.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
  
    turnout.08 <- glm(voted ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 2008
                        income.q + white + age, data = mydata.cdf, 
                      subset = (year == 2008), family = "binomial")   
    summary(turnout.08)
    turnout.08.df <- data.frame(t = coef(turnout.08) / sqrt(diag(vcov(turnout.08))),
                                df = turnout.08$df.null)
    turnout.08.df <- turnout.08.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
  
    turnout.04 <- glm(voted ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 2004
                        income.q + white + age, data = mydata.cdf, 
                      subset = (year == 2004), family = "binomial")   
    summary(turnout.04)
    turnout.04.df <- data.frame(t = coef(turnout.04) / sqrt(diag(vcov(turnout.04))),
                                df = turnout.04$df.null)
    turnout.04.df <- turnout.04.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
  
    turnout.00 <- glm(voted ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 2000
                        income.q + white + age, data = mydata.cdf, 
                      subset = (year == 2000), family = "binomial")    
    summary(turnout.00)
    turnout.00.df <- data.frame(t = coef(turnout.00) / sqrt(diag(vcov(turnout.00))),
                                df = turnout.00$df.null)
    turnout.00.df <- turnout.00.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
  
    turnout.96 <- glm(voted ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 1996
                        income.q + white + age, data = mydata.cdf, 
                      subset = (year == 1996), family = "binomial") 
    summary(turnout.96)
    turnout.96.df <- data.frame(t = coef(turnout.96) / sqrt(diag(vcov(turnout.96))),
                                df = turnout.96$df.null)
    turnout.96.df <- turnout.96.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
    
    turnout.92 <- glm(voted ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 1992
                        income.q + white + age, data = mydata.cdf, 
                      subset = (year == 1992), family = "binomial") 
    summary(turnout.92)
    turnout.92.df <- data.frame(t = coef(turnout.92) / sqrt(diag(vcov(turnout.92))),
                                df = turnout.92$df.null)
    turnout.92.df <- turnout.92.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
    
    turnout.88 <- glm(voted ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 1988
                        income.q + white + age, data = mydata.cdf, 
                      subset = (year == 1988), family = "binomial")
    summary(turnout.88)
    turnout.88.df <- data.frame(t = coef(turnout.88) / sqrt(diag(vcov(turnout.88))),
                                df = turnout.88$df.null)
    turnout.88.df <- turnout.88.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
    stargazer(turnout.88, turnout.92, turnout.96, turnout.00, turnout.04, 
              turnout.08, turnout.12, turnout.16, digits = 3,
              dep.var.labels.include = F,
              p = list(turnout.88.df$p, turnout.92.df$p, turnout.96.df$p, turnout.00.df$p,
                       turnout.04.df$p, turnout.08.df$p, turnout.12.df$p, turnout.16.df$p),
              covariate.labels = c("Cynicism", "Gov. Unresponsive", "Partisan Strength", 
                                   "Education", "Independent", "Income", "White", "Age", "Constant"),
              column.labels = c("1988", "1992", "1996","2000","2004","2008","2012", "2016"),
              keep.stat = c("n","aic"))
    
    # Coefficient Plot
    turnout.est <- data.frame(cyn.est = c(coef(turnout.88)[2], coef(turnout.92)[2], coef(turnout.96)[2], 
                                          coef(turnout.00)[2], coef(turnout.04)[2], coef(turnout.08)[2],
                                          coef(turnout.12)[2], coef(turnout.16)[2]),
                              cyn.std.error = c(sqrt(diag(vcov(turnout.88)))[2], sqrt(diag(vcov(turnout.92)))[2],
                                                sqrt(diag(vcov(turnout.96)))[2], sqrt(diag(vcov(turnout.00)))[2],
                                                sqrt(diag(vcov(turnout.04)))[2], sqrt(diag(vcov(turnout.08)))[2],
                                                sqrt(diag(vcov(turnout.12)))[2], sqrt(diag(vcov(turnout.16)))[2]),
                              unresp.est = c(coef(turnout.88)[3], coef(turnout.92)[3], coef(turnout.96)[3], 
                                             coef(turnout.00)[3], coef(turnout.04)[3], coef(turnout.08)[3],
                                             coef(turnout.12)[3], coef(turnout.16)[3]),
                              unresp.std.error = c(sqrt(diag(vcov(turnout.88)))[3], sqrt(diag(vcov(turnout.92)))[3],
                                                   sqrt(diag(vcov(turnout.96)))[3], sqrt(diag(vcov(turnout.00)))[3],
                                                   sqrt(diag(vcov(turnout.04)))[3], sqrt(diag(vcov(turnout.08)))[3],
                                                   sqrt(diag(vcov(turnout.12)))[3], sqrt(diag(vcov(turnout.16)))[3]))
    turnout.est$cyn.lwr <- turnout.est$cyn.est - 2*turnout.est$cyn.std.error
    turnout.est$cyn.upr <- turnout.est$cyn.est + 2*turnout.est$cyn.std.error
    turnout.est$unresp.lwr <- turnout.est$unresp.est - 2*turnout.est$unresp.std.error
    turnout.est$unresp.upr <- turnout.est$unresp.est + 2*turnout.est$unresp.std.error
    turnout.est$year <- seq(1988,2016,4)
    
    pdf(height= 6, width = 12, "Figures/Coef-Turnout-Cynicism.pdf")
    ggplot(data = turnout.est, aes(x = year,y = cyn.est)) +  
      geom_point(position=position_dodge(width = .5), size = 4) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_errorbar(aes(ymin = cyn.lwr, ymax = cyn.upr), width = 0 , size = 1) +
      labs(x = "", y = "Coef. Estimate - Cynicism") +
      #ylim(-0.05,0.10) +
      scale_x_continuous(breaks = seq(1988,2016,4),
                         labels = c("1988","1992","1996","2000","2004","2008","2012","2016")) +
      theme(axis.line = element_line(colour = "black"),
            plot.subtitle = element_text(vjust = 1), 
            plot.caption = element_text(vjust = 1), 
            plot.margin = margin(0.5,3,0.5,0.5,"cm"),
            panel.background = element_rect(fill = "white", colour = NA),
            panel.grid.major = element_line(linetype = "blank"), 
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title = element_text(size = 22),
            axis.text = element_text(size = 18))
    dev.off()
    
    
    pdf(height = 6, width = 12, "Figures/Coef-Turnout-Unresponsive.pdf")
    ggplot(data = turnout.est, aes(x = year,y = unresp.est)) +  
      geom_point(position=position_dodge(width = .5), size = 4) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_errorbar(aes(ymin = unresp.lwr, ymax = unresp.upr), width = 0 , size = 1) +
      labs(x = "", y = "Coef. Estimate - Unresponsive") +
      #ylim(-0.05,0.10) +
      scale_x_continuous(breaks = seq(1988,2016,4),
                         labels = c("1988","1992","1996","2000","2004","2008","2012","2016")) +
      theme(axis.line = element_line(colour = "black"),
            plot.subtitle = element_text(vjust = 1), 
            plot.caption = element_text(vjust = 1), 
            plot.margin = margin(0.5,3,0.5,0.5,"cm"),
            panel.background = element_rect(fill = "white", colour = NA),
            panel.grid.major = element_line(linetype = "blank"), 
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title = element_text(size = 22),
            axis.text = element_text(size = 18))
      dev.off()  
      
  # Vote Choice ----
    # Primary Election Vote Model (Multinomial Logit) ----
    mydata.16$voteprimarychoice16 <- factor(mydata.16$voteprimarychoice16, levels = c("Any Other Candidate", "Sanders", "Trump", "Did not vote"))
    set.seed(219)
    primary.mod <- multinom(voteprimarychoice16 ~ alien.cynicism + elect.attn  + pid3 + ideo7 + income.q + party.strength +
                              educ + white + female + age, data = mydata.16, subset = (sup.tues == 1))
    primary.t <- summary(primary.mod)$coefficients/summary(primary.mod)$standard.errors # t values
    primary.p <- ifelse(primary.t < 0, pt(primary.t,primary.mod$edf,lower=T), pt(primary.t,primary.mod$edf,lower=F)) # one-tailed p-values
    stargazer(primary.mod, digits = 3,
              dep.var.labels.include = T,
              covariate.labels = c("Cynicism", "Unresponsive to Elec.", "Independent",
                                   "Republican", "Ideology", "Income", "Partisan Strength",
                                   "Education", "White", "Female", "Age", "Constant"),
              keep.stat = c("aic"))
    dim(primary.mod$fitted.values) # N = 602, stargazer doesn't produce N from multinom,
    
    # Plot
    effect.prim.cynicism <- effect("alien.cynicism", primary.mod, xlevels = list(alien.cynicism = seq(0,2,0.1)), typical = median,
                                   fixed.predictors = list(given.values = c(pid3Rep = 1)))
    effect.prim.unresp <- effect("elect.attn", primary.mod, xlevels = list(elect.attn = seq(0,2,0.1)), typical = median,
                                 fixed.predictors = list(given.values = c(pid3Rep = 1)))
    effect.prim.df <- data.frame(fit = c(effect.prim.cynicism$prob[,1], effect.prim.cynicism$prob[,2],
                                         effect.prim.cynicism$prob[,3], effect.prim.cynicism$prob[,4],
                                         effect.prim.unresp$prob[,1], effect.prim.unresp$prob[,2],
                                         effect.prim.unresp$prob[,3], effect.prim.unresp$prob[,4]),
                                 lwr = c(effect.prim.cynicism$lower.prob[,1], effect.prim.cynicism$lower.prob[,2],
                                         effect.prim.cynicism$lower.prob[,3], effect.prim.cynicism$lower.prob[,4],
                                         effect.prim.unresp$lower.prob[,1], effect.prim.unresp$lower.prob[,2],
                                         effect.prim.unresp$lower.prob[,3], effect.prim.unresp$lower.prob[,4]),
                                 upr = c(effect.prim.cynicism$upper.prob[,1], effect.prim.cynicism$upper.prob[,2],
                                         effect.prim.cynicism$upper.prob[,3], effect.prim.cynicism$upper.prob[,4],
                                         effect.prim.unresp$upper.prob[,1], effect.prim.unresp$upper.prob[,2],
                                         effect.prim.unresp$upper.prob[,3], effect.prim.unresp$upper.prob[,4]),
                                 choice = c(rep("Any Other Candidate", 21), rep("Sanders", 21),
                                            rep("Trump", 21), rep("Did not vote", 21),
                                            rep("Any Other Candidate", 21), rep("Sanders", 21),
                                            rep("Trump", 21), rep("Did not vote", 21)),
                                 x = rep(seq(0,2,0.1),8),
                                 dimension = c(rep("Cynicism", 84), rep("Gov. Unresponsiveness", 84)))
    effect.prim.df$choice <- factor(effect.prim.df$choice, levels = c("Sanders","Trump","Any Other Candidate","Did not vote"))
    
    pdf(height = 12, width = 12, "Figures/Probs-Primary.pdf")
    ggplot(effect.prim.df, aes(x = x, y = fit, colour = choice)) +
      geom_line(size = 2) +
      geom_line(aes(y = lwr), lty = "dashed") +
      geom_line(aes(y = upr), lty = "dashed") +
      labs(x = "", y = "Probability of Vote Choice") +
      facet_grid(rows = vars(choice), cols = vars(dimension), scales = "free_y") +
      scale_colour_manual("", values = c("#377EB8","#E41A1C","#4DAF4A","#984EA3")) +
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
    
    # Sanders
    se.sanders.l <- effect.prim.cynicism$se.prob[1,2]
    se.sanders.h <- effect.prim.cynicism$se.prob[21,2]
    est.sanders.l <- effect.prim.cynicism$prob[1,2]
    est.sanders.h <- effect.prim.cynicism$prob[21,2]
    t.sanders <- (est.sanders.h - est.sanders.l)/(sqrt(se.sanders.h^2 + se.sanders.l^2))
    p.sanders <- 2*pnorm(-abs(t.sanders))
    
    # Trump
    se.trump.l <- effect.prim.cynicism$se.prob[1,3]
    se.trump.h <- effect.prim.cynicism$se.prob[21,3]
    est.trump.l <- effect.prim.cynicism$prob[1,3]
    est.trump.h <- effect.prim.cynicism$prob[21,3]
    t.trump <- (est.trump.h - est.trump.l)/(sqrt(se.trump.h^2 + se.trump.l^2))
    p.trump <- 2*pnorm(-abs(t.trump))
    
    
    
    
    
    # General Election Vote Model (Multinomial Logit) ----
    mydata.16$votechoice16 <- factor(mydata.16$votechoice16, levels = c("Clinton","Trump","Other","Did not vote"))
    set.seed(219)
    general.mod <- multinom(votechoice16 ~ alien.cynicism + elect.attn  + pid3 + ideo7 + income.q + party.strength +
                                educ + white + female + age, data = mydata.16)
    general.t <- summary(general.mod)$coefficients/summary(general.mod)$standard.errors # t values
    general.p <- ifelse(general.t < 0, pt(general.t,general.mod$edf,lower=T), pt(general.t,general.mod$edf,lower=F)) # one-tailed p-values
    stargazer(general.mod, digits = 3,
              dep.var.labels.include = T,
              p = list(general.p),
              covariate.labels = c("Cynicism", "Unresponsive to Elec.", "Independent",
                                   "Republican", "Ideology", "Income", "Partisan Strength",
                                   "Education", "White", "Female", "Age", "Constant"),
              keep.stat = c("aic"))
    dim(general.mod$fitted.values) # N = 2,996, N not produced by multinom
    
    # Plot
    general.effect.cynicism <- effect("alien.cynicism", general.mod, xlevels = list(alien.cynicism = seq(0,2,0.1)), typical = median)
    general.effect.unresp <- effect("elect.attn", general.mod, xlevels = list(elect.attn = seq(0,2,0.1)), typical = median)
    general.effect.df <- data.frame(fit = c(general.effect.cynicism$prob[,1], general.effect.cynicism$prob[,2],
                                             general.effect.cynicism$prob[,3], general.effect.cynicism$prob[,4],
                                             general.effect.unresp$prob[,1], general.effect.unresp$prob[,2],
                                             general.effect.unresp$prob[,3], general.effect.unresp$prob[,4]),
                                     lwr = c(general.effect.cynicism$lower.prob[,1], general.effect.cynicism$lower.prob[,2],
                                             general.effect.cynicism$lower.prob[,3], general.effect.cynicism$lower.prob[,4],
                                             general.effect.unresp$lower.prob[,1], general.effect.unresp$lower.prob[,2],
                                             general.effect.unresp$lower.prob[,3], general.effect.unresp$lower.prob[,4]),
                                     upr = c(general.effect.cynicism$upper.prob[,1], general.effect.cynicism$upper.prob[,2],
                                             general.effect.cynicism$upper.prob[,3], general.effect.cynicism$upper.prob[,4],
                                             general.effect.unresp$upper.prob[,1], general.effect.unresp$upper.prob[,2],
                                             general.effect.unresp$upper.prob[,3], general.effect.unresp$upper.prob[,4]),
                                     choice = c(rep("Clinton", 21), rep("Trump", 21),
                                                rep("Other", 21), rep("Did not vote", 21),
                                                rep("Clinton", 21), rep("Trump", 21),
                                                rep("Other", 21), rep("Did not vote", 21)),
                                     x = rep(seq(0,2,0.1),8),
                                     dimension = c(rep("Cynicism", 84), rep("Gov. Unresponsiveness", 84)))
    general.effect.df$choice <- factor(general.effect.df$choice, levels = c("Clinton","Trump","Other","Did not vote"))
    
    # Clinton
    se.clinton.l <- general.effect.cynicism$se.prob[1,1]
    se.clinton.h <- general.effect.cynicism$se.prob[21,1]
    est.clinton.l <- general.effect.cynicism$prob[1,1]
    est.clinton.h <- general.effect.cynicism$prob[21,1]
    t.clinton.gen <- (est.clinton.h - est.clinton.l)/(sqrt(se.clinton.h^2 + se.clinton.l^2))
    p.clinton.gen <- 2*pnorm(-abs(t.clinton.gen))
    
    # Trump
    se.trump.l <- general.effect.cynicism$se.prob[1,2]
    se.trump.h <- general.effect.cynicism$se.prob[21,2]
    est.trump.l <- general.effect.cynicism$prob[1,2]
    est.trump.h <- general.effect.cynicism$prob[21,2]
    t.trump.gen <- (est.trump.h - est.trump.l)/(sqrt(se.trump.h^2 + se.trump.l^2))
    p.trump.gen <- 2*pnorm(-abs(t.trump.gen))
    
    # Trump
    se.other.l <- general.effect.cynicism$se.prob[1,3]
    se.other.h <- general.effect.cynicism$se.prob[21,3]
    est.other.l <- general.effect.cynicism$prob[1,3]
    est.other.h <- general.effect.cynicism$prob[21,3]
    t.other.gen <- (est.other.h - est.other.l)/(sqrt(se.other.h^2 + se.other.l^2))
    p.other.gen <- 2*pnorm(-abs(t.other.gen))
    
    # Did not vote
    se.dnv.l <- general.effect.cynicism$se.prob[1,4]
    se.dnv.h <- general.effect.cynicism$se.prob[21,4]
    est.dnv.l <- general.effect.cynicism$prob[1,4]
    est.dnv.h <- general.effect.cynicism$prob[21,4]
    t.dnv.gen <- (est.dnv.h - est.dnv.l)/(sqrt(se.dnv.h^2 + se.dnv.l^2))
    p.dnv.gen <- 2*pnorm(-abs(t.dnv.gen))




    
    pdf(height = 12, width = 12, "Figures/Probs-General.pdf")
    ggplot(general.effect.df, aes(x = x, y = fit, colour = choice)) +
      geom_line(size = 2) +
      geom_line(aes(y = lwr), lty = "dashed") +
      geom_line(aes(y = upr), lty = "dashed") +
      labs(x = "", y = "Probability of Vote Choice") +
      facet_grid(rows = vars(choice), cols = vars(dimension), scales = "free_y") +
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

  # Vote Preference ----
    # Preference in General Election (Multinomial Logit) ----
    mydata.16$votepref16 <- factor(mydata.16$votepref16, levels = c("Clinton", "Trump", "Other/Third-Party")) # change order of outcomes
    set.seed(219) # set seed
    pref.mod <- multinom(votepref16 ~ alien.cynicism + elect.attn  + pid3 + ideo7 + income.q + party.strength + # estimate multinomial reg.
                           educ + white + female + age, data = mydata.16, subset = (vote16 != 1))
    dim(pref.mod$fitted.values) # N = 293, N not produced by multinom
    prefmod.t <- summary(pref.mod)$coefficients/summary(pref.mod)$standard.errors # t values
    prefmod.p <- ifelse(prefmod.t < 0, pt(prefmod.t,pref.mod$edf,lower=T), pt(prefmod.t,pref.mod$edf,lower=F)) # one-tailed p-values
    stargazer(pref.mod, digits = 3,
              dep.var.labels.include = T,
              p = list(prefmod.p),
              covariate.labels = c("Cynicism", "Unresponsive to Elec.", "Independent",
                                   "Republican", "Ideology", "Income", "Partisan Strength",
                                   "Education", "White", "Female", "Age", "Constant"),
              keep.stat = c("aic"))
    
    