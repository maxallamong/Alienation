# Maxwell B. Allamong
# Alie(n)ation: Political Outsiders in the 2016 Election - Vote Models
# Created: Mar. 12th, 2020
# Updated: Feb. 1st, 2021

# READ ME ----
#  To replicate this analysis, first open the R Project file (Alienation.RProj).
#  Next, simply run this entire script: it will automatically set the working directory to the
#  folder that contains the R Project file, and then it will source the data from the source
#  file (Alienation-Source.R).

# Source Data ----
  source("Alienation-Source.R")

# Models ----
  # Turnout ----
  
    # Primary Election, 1988-2016 (Logit) ----
    primturnout.16 <- glm(voteprimary16 ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 2016
                        income.q + white + age, 
                      data = mydata.16, family = "binomial")
    summary(primturnout.16)
    primturnout.16.df <- data.frame(t = coef(primturnout.16) / sqrt(diag(vcov(primturnout.16))),
                                df = primturnout.16$df.null)
    primturnout.16.df <- primturnout.16.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
    
    primturnout.12 <- glm(voteprimary12 ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 2012
                        income.q + white + age, 
                      data = mydata.12, family = "binomial")
    summary(primturnout.12)
    primturnout.12.df <- data.frame(t = coef(primturnout.12) / sqrt(diag(vcov(primturnout.12))),
                                df = primturnout.12$df.null)
    primturnout.12.df <- primturnout.12.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
    
    primturnout.08 <- glm(voteprimary ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 2008
                        income.q + white + age, data = mydata.cdf, 
                      subset = (year == 2008), family = "binomial")   
    summary(primturnout.08)
    primturnout.08.df <- data.frame(t = coef(primturnout.08) / sqrt(diag(vcov(primturnout.08))),
                                df = primturnout.08$df.null)
    primturnout.08.df <- primturnout.08.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
    
    primturnout.04 <- glm(voteprimary ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 2004
                        income.q + white + age, data = mydata.cdf, 
                      subset = (year == 2004), family = "binomial")   
    summary(primturnout.04)
    primturnout.04.df <- data.frame(t = coef(primturnout.04) / sqrt(diag(vcov(primturnout.04))),
                                df = primturnout.04$df.null)
    primturnout.04.df <- primturnout.04.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
    
    primturnout.00 <- glm(voteprimary ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 2000
                        income.q + white + age, data = mydata.cdf, 
                      subset = (year == 2000), family = "binomial")    
    summary(primturnout.00)
    primturnout.00.df <- data.frame(t = coef(primturnout.00) / sqrt(diag(vcov(primturnout.00))),
                                df = primturnout.00$df.null)
    primturnout.00.df <- primturnout.00.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
    
    primturnout.96 <- glm(voteprimary ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 1996
                        income.q + white + age, data = mydata.cdf, 
                      subset = (year == 1996), family = "binomial") 
    summary(primturnout.96)
    primturnout.96.df <- data.frame(t = coef(primturnout.96) / sqrt(diag(vcov(primturnout.96))),
                                df = primturnout.96$df.null)
    primturnout.96.df <- primturnout.96.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
    
    primturnout.92 <- glm(voteprimary ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 1992
                        income.q + white + age, data = mydata.cdf, 
                      subset = (year == 1992), family = "binomial") 
    summary(primturnout.92)
    primturnout.92.df <- data.frame(t = coef(primturnout.92) / sqrt(diag(vcov(primturnout.92))),
                                df = primturnout.92$df.null)
    primturnout.92.df <- primturnout.92.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
    
    primturnout.88 <- glm(voteprimary ~ alien.cynicism + elect.attn + party.strength + educ + ind + # 1988
                        income.q + white + age, data = mydata.cdf, 
                      subset = (year == 1988), family = "binomial")
    summary(primturnout.88)
    primturnout.88.df <- data.frame(t = coef(primturnout.88) / sqrt(diag(vcov(primturnout.88))),
                                df = primturnout.88$df.null)
    primturnout.88.df <- primturnout.88.df %>% 
      mutate(p = ifelse(t < 0, pt(t, df, lower = TRUE), pt(t, df, lower = FALSE)))
    
    stargazer(primturnout.88, primturnout.92, primturnout.96, primturnout.00, primturnout.04, 
              primturnout.08, primturnout.12, primturnout.16, digits = 3,
              dep.var.labels.include = F,
              p = list(primturnout.88.df$p, primturnout.92.df$p, primturnout.96.df$p, primturnout.00.df$p,
                       primturnout.04.df$p, primturnout.08.df$p, primturnout.12.df$p, primturnout.16.df$p),
              covariate.labels = c("Cynicism", "Gov. Unresponsive", "Partisan Strength", 
                                   "Education", "Independent", "Income", "White", "Age", "Constant"),
              column.labels = c("1988", "1992", "1996","2000","2004","2008","2012", "2016"),
              keep.stat = c("n","aic"))
    
    # General Election, 1988-2016 (Logit) ----
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
    turnout.est$cyn.lwr <- turnout.est$cyn.est - 1.645*turnout.est$cyn.std.error
    turnout.est$cyn.upr <- turnout.est$cyn.est + 1.645*turnout.est$cyn.std.error
    turnout.est$unresp.lwr <- turnout.est$unresp.est - 1.645*turnout.est$unresp.std.error
    turnout.est$unresp.upr <- turnout.est$unresp.est + 1.645*turnout.est$unresp.std.error
    turnout.est$year <- seq(1988,2016,4)
    
    pdf(height= 6, width = 12, "Figures/Coef-Turnout-Cynicism.pdf")
    ggplot(data = turnout.est, aes(x = year,y = cyn.est)) +  
      geom_point(position=position_dodge(width = .5), size = 4) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_errorbar(aes(ymin = cyn.lwr, ymax = cyn.upr), width = 0 , size = 1) +
      labs(x = "", y = "Coefficient - Cynicism\n") +
      #ylim(-0.05,0.10) +
      scale_x_continuous(breaks = seq(1988,2016,4),
                         labels = c("1988","1992","1996","2000","2004","2008","2012","2016")) +
      theme(axis.line = element_line(colour = "black"),
            plot.subtitle = element_text(vjust = 1), 
            plot.caption = element_text(vjust = 1), 
            plot.margin = margin(0,0,0,0,"cm"),
            panel.background = element_rect(fill = "white", colour = NA),
            panel.grid.major = element_line(linetype = "blank"), 
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title = element_text(size = 28),
            axis.text = element_text(size = 26))
    dev.off()
    
    
    pdf(height = 6, width = 12, "Figures/Coef-Turnout-Unresponsive.pdf")
    ggplot(data = turnout.est, aes(x = year,y = unresp.est)) +  
      geom_point(position=position_dodge(width = .5), size = 4) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_errorbar(aes(ymin = unresp.lwr, ymax = unresp.upr), width = 0 , size = 1) +
      labs(x = "", y = "Coefficient - Election Unresp.\n") +
      #ylim(-0.05,0.10) +
      scale_x_continuous(breaks = seq(1988,2016,4),
                         labels = c("1988","1992","1996","2000","2004","2008","2012","2016")) +
      theme(axis.line = element_line(colour = "black"),
            plot.subtitle = element_text(vjust = 1), 
            plot.caption = element_text(vjust = 1), 
            plot.margin = margin(0,0,0,0,"cm"),
            panel.background = element_rect(fill = "white", colour = NA),
            panel.grid.major = element_line(linetype = "blank"), 
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title = element_text(size = 28),
            axis.text = element_text(size = 26))
    dev.off()  
      
  # Vote Choice ----
    # Primary Election Vote Model (Multinomial Logit) ----
    mydata.16$voteprimarychoice16 <- factor(mydata.16$voteprimarychoice16, levels = c("Sanders", "Trump", "Other", "Did not vote"))
    mydata.primary <- mydata.16 %>% # create data frame for primary models
      select(alien.cynicism, elect.attn, rep, ind, dem, ideo7, income.q,
             party.strength, educ, white, female, age, voteprimarychoice16, sup.tues, pid3) %>%
      filter(sup.tues == 1) %>%
      drop_na()
    set.seed(219) # seed
    mydata.primary$voteprimarychoice16 <- relevel(mydata.primary$voteprimarychoice16, ref = "Did not vote")
    primary.mod <- multinom(voteprimarychoice16 ~ alien.cynicism + elect.attn  + rep + ind + ideo7 + income.q + party.strength + # multinomial logit
                              educ + white + female + age, data = mydata.primary, Hess = T)
    primary.t <- summary(primary.mod)$coefficients/summary(primary.mod)$standard.errors # t values
    primary.p <- ifelse(primary.t < 0, pt(primary.t,primary.mod$edf,lower=T), pt(primary.t,primary.mod$edf,lower=F)) # one-tailed p-values
    stargazer(primary.mod, digits = 3, # create LaTeX table
              dep.var.labels.include = T,
              covariate.labels = c("Cynicism", "Unresponsive to Elec.", "Republican",
                                   "Independent", "Ideology", "Income", "Partisan Strength",
                                   "Education", "White", "Female", "Age", "Constant"),
              keep.stat = c("aic"))
    dim(primary.mod$fitted.values) # N = 602, stargazer doesn't produce N from multinom,
    
      # Simulating Predicted Probabilities ----
      x <- seq(0,2,0.01)
      primary.coef <- coef(primary.mod)
      primary.covar <- vcov(primary.mod)
      set.seed(219)
      sandersCoef <- mvrnorm(n = 1000, primary.coef[1,], primary.covar[1:12,1:12]) # simulate 1000 coefficients
      trumpCoef <- mvrnorm(n = 1000, primary.coef[2,], primary.covar[13:24,13:24]) # simulate 1000 coefficients
      otherCoef <- mvrnorm(n = 1000, primary.coef[3,], primary.covar[25:36,25:36]) # simulate 1000 coefficients
      
      # Cynicism ----
      temp <- matrix(NA, nrow = 1000, ncol = 4)
      sim.results <- matrix(NA, nrow = 804, ncol = 4)
      primary.probdists.cynicism <- matrix(NA, nrow = 1000, ncol = 12)

      for(i in 1:length(x)){
        
        #sandersCoef <- mvrnorm(n = 1000, primary.coef[1,], primary.covar[1:12,1:12]) # simulate 1000 coefficients
        #trumpCoef <- mvrnorm(n = 1000, primary.coef[2,], primary.covar[13:24,13:24]) # simulate 1000 coefficients
        #otherCoef <- mvrnorm(n = 1000, primary.coef[3,], primary.covar[25:36,25:36]) # simulate 1000 coefficients
        
        for(j in 1:nrow(sandersCoef)){
          sandersAg <- exp(sandersCoef[j,1] + # sanders aggregator function
                             sandersCoef[j,2]*x[i] +
                             sandersCoef[j,3]*mydata.primary$elect.attn +
                             sandersCoef[j,4]*mydata.primary$rep +
                             sandersCoef[j,5]*mydata.primary$ind +
                             sandersCoef[j,6]*mydata.primary$ideo7 +
                             sandersCoef[j,7]*mydata.primary$income.q +
                             sandersCoef[j,8]*mydata.primary$party.strength +
                             sandersCoef[j,9]*mydata.primary$educ + 
                             sandersCoef[j,10]*mydata.primary$white +
                             sandersCoef[j,11]*mydata.primary$female +
                             sandersCoef[j,12]*mydata.primary$age)
          
          trumpAg <- exp(trumpCoef[j,1] + # trump aggregator function
                           trumpCoef[j,2]*x[i] +
                           trumpCoef[j,3]*mydata.primary$elect.attn +
                           trumpCoef[j,4]*mydata.primary$rep +
                           trumpCoef[j,5]*mydata.primary$ind +
                           trumpCoef[j,6]*mydata.primary$ideo7 +
                           trumpCoef[j,7]*mydata.primary$income.q +
                           trumpCoef[j,8]*mydata.primary$party.strength +
                           trumpCoef[j,9]*mydata.primary$educ + 
                           trumpCoef[j,10]*mydata.primary$white +
                           trumpCoef[j,11]*mydata.primary$female +
                           trumpCoef[j,12]*mydata.primary$age)
          
          otherAg <- exp(otherCoef[j,1] + # no vote aggregator function
                            otherCoef[j,2]*x[i] +
                            otherCoef[j,3]*mydata.primary$elect.attn +
                            otherCoef[j,4]*mydata.primary$rep +
                            otherCoef[j,5]*mydata.primary$ind +
                            otherCoef[j,6]*mydata.primary$ideo7 +
                            otherCoef[j,7]*mydata.primary$income.q +
                            otherCoef[j,8]*mydata.primary$party.strength +
                            otherCoef[j,9]*mydata.primary$educ + 
                            otherCoef[j,10]*mydata.primary$white +
                            otherCoef[j,11]*mydata.primary$female +
                            otherCoef[j,12]*mydata.primary$age)
          
          sanders <- sandersAg / (1 + sandersAg + trumpAg + otherAg) # get probabilities
          trump <- trumpAg / (1 + sandersAg + trumpAg + otherAg)
          other <- otherAg / (1 + sandersAg + trumpAg + otherAg)
          novote <- 1 - sanders - trump - other
          
          temp[j,1] <- mean(sanders) # take mean predicted probability of sanders vote across all observable values, store in j-th row, 1st column
          temp[j,2] <- mean(trump)
          temp[j,3] <- mean(other)
          temp[j,4] <- mean(novote)
        }
        
        sim.results[((i*4)-3),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)-3),2] <- quantile(temp[,1], 0.5) 
        sim.results[((i*4)-3),2] <- mean(temp[,1]) # mean of distribution of 1,000 observed-value predicted probabilities (Sanders)
        sim.results[((i*4)-3),3] <- quantile(temp[,1], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Sanders)
        sim.results[((i*4)-3),4] <- quantile(temp[,1], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Sanders)
        
        sim.results[((i*4)-2),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)-2),2] <- quantile(temp[,2], 0.5) 
        sim.results[((i*4)-2),2] <- mean(temp[,2]) # mean of distribution of 1,000 observed-value predicted probabilities (Trump)
        sim.results[((i*4)-2),3] <- quantile(temp[,2], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
        sim.results[((i*4)-2),4] <- quantile(temp[,2], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
        
        sim.results[((i*4)-1),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)-1),2] <- quantile(temp[,3], 0.5) 
        sim.results[((i*4)-1),2] <- mean(temp[,3]) # mean of distribution of 1,000 observed-value predicted probabilities (Other)
        sim.results[((i*4)-1),3] <- quantile(temp[,3], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Other)
        sim.results[((i*4)-1),4] <- quantile(temp[,3], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Other)
        
        sim.results[((i*4)),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)),2] <- quantile(temp[,4], 0.5) 
        sim.results[((i*4)),2] <- mean(temp[,4]) # mean of distribution of 1,000 observed-value predicted probabilities (Did not vote)
        sim.results[((i*4)),3] <- quantile(temp[,4], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
        sim.results[((i*4)),4] <- quantile(temp[,4], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
        
        if (x[i] == 0) { # min
          primary.probdists.cynicism[,1] <- temp[,1]
          primary.probdists.cynicism[,2] <- temp[,2]
          primary.probdists.cynicism[,3] <- temp[,3]
          primary.probdists.cynicism[,4] <- temp[,4]
        }
        
        if (x[i] == 1) { # med
          primary.probdists.cynicism[,5] <- temp[,1]
          primary.probdists.cynicism[,6] <- temp[,2]
          primary.probdists.cynicism[,7] <- temp[,3]
          primary.probdists.cynicism[,8] <- temp[,4]
        }
        
        if (x[i] == 2) { # max
          primary.probdists.cynicism[,9] <- temp[,1]
          primary.probdists.cynicism[,10] <- temp[,2]
          primary.probdists.cynicism[,11] <- temp[,3]
          primary.probdists.cynicism[,12] <- temp[,4]
        } 
        
      }
      
      primary.effect.cynicism <- as.data.frame(sim.results) # put results in data frame
      colnames(primary.effect.cynicism)[1] <- "x"
      colnames(primary.effect.cynicism)[2] <- "fit"
      colnames(primary.effect.cynicism)[3] <- "lower"
      colnames(primary.effect.cynicism)[4] <- "upper"
      primary.effect.cynicism$voteprimarychoice16 <- rep(c("Sanders","Trump","Other", "Did not vote"),201)
      primary.effect.cynicism$category <- "Cynicism"
      
      # Election Unresponsiveness ----
      temp <- matrix(NA, nrow = 1000, ncol = 4) # create empty matrices for results
      sim.results <- matrix(NA, nrow = 804, ncol = 4)
      primary.probdists.unresp <- matrix(NA, nrow = 1000, ncol = 12)
      
      for(i in 1:length(x)){
        
        #sandersCoef <- mvrnorm(n = 1000, primary.coef[1,], primary.covar[1:12,1:12]) # simulate 1000 coefficients
        #trumpCoef <- mvrnorm(n = 1000, primary.coef[2,], primary.covar[13:24,13:24]) # simulate 1000 coefficients
        #otherCoef <- mvrnorm(n = 1000, primary.coef[3,], primary.covar[25:36,25:36]) # simulate 1000 coefficients
        
        for(j in 1:nrow(sandersCoef)){
          sandersAg <- exp(sandersCoef[j,1] + # sanders aggregator function
                             sandersCoef[j,2]*mydata.primary$alien.cynicism +
                             sandersCoef[j,3]*x[i] +
                             sandersCoef[j,4]*mydata.primary$rep +
                             sandersCoef[j,5]*mydata.primary$ind +
                             sandersCoef[j,6]*mydata.primary$ideo7 +
                             sandersCoef[j,7]*mydata.primary$income.q +
                             sandersCoef[j,8]*mydata.primary$party.strength +
                             sandersCoef[j,9]*mydata.primary$educ + 
                             sandersCoef[j,10]*mydata.primary$white +
                             sandersCoef[j,11]*mydata.primary$female +
                             sandersCoef[j,12]*mydata.primary$age)
          
          trumpAg <- exp(trumpCoef[j,1] + # trump aggregator function
                           trumpCoef[j,2]*mydata.primary$alien.cynicism +
                           trumpCoef[j,3]*x[i] +
                           trumpCoef[j,4]*mydata.primary$rep +
                           trumpCoef[j,5]*mydata.primary$ind +
                           trumpCoef[j,6]*mydata.primary$ideo7 +
                           trumpCoef[j,7]*mydata.primary$income.q +
                           trumpCoef[j,8]*mydata.primary$party.strength +
                           trumpCoef[j,9]*mydata.primary$educ + 
                           trumpCoef[j,10]*mydata.primary$white +
                           trumpCoef[j,11]*mydata.primary$female +
                           trumpCoef[j,12]*mydata.primary$age)
          
          otherAg <- exp(otherCoef[j,1] + # no vote aggregator function
                            otherCoef[j,2]*mydata.primary$alien.cynicism +
                            otherCoef[j,3]*x[i] +
                            otherCoef[j,4]*mydata.primary$rep +
                            otherCoef[j,5]*mydata.primary$ind +
                            otherCoef[j,6]*mydata.primary$ideo7 +
                            otherCoef[j,7]*mydata.primary$income.q +
                            otherCoef[j,8]*mydata.primary$party.strength +
                            otherCoef[j,9]*mydata.primary$educ + 
                            otherCoef[j,10]*mydata.primary$white +
                            otherCoef[j,11]*mydata.primary$female +
                            otherCoef[j,12]*mydata.primary$age)
          
          sanders <- sandersAg / (1 + sandersAg + trumpAg + otherAg) # get probabilities
          trump <- trumpAg / (1 + sandersAg + trumpAg + otherAg)
          other <- otherAg / (1 + sandersAg + trumpAg + otherAg)
          novote <- 1 - sanders - trump - other
          
          temp[j,1] <- mean(sanders) # take mean predicted probability of sanders vote across all observable values, store in j-th row, 1st column
          temp[j,2] <- mean(trump)
          temp[j,3] <- mean(other)
          temp[j,4] <- mean(novote)
        }
        
        sim.results[((i*4)-3),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)-3),2] <- quantile(temp[,1], 0.5) 
        sim.results[((i*4)-3),2] <- mean(temp[,1]) # mean of distribution of 1,000 observed-value predicted probabilities (Sanders)
        sim.results[((i*4)-3),3] <- quantile(temp[,1], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Sanders)
        sim.results[((i*4)-3),4] <- quantile(temp[,1], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Sanders)
        
        sim.results[((i*4)-2),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)-2),2] <- quantile(temp[,2], 0.5) 
        sim.results[((i*4)-2),2] <- mean(temp[,2]) # mean of distribution of 1,000 observed-value predicted probabilities (Trump)
        sim.results[((i*4)-2),3] <- quantile(temp[,2], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
        sim.results[((i*4)-2),4] <- quantile(temp[,2], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
        
        sim.results[((i*4)-1),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)-1),2] <- quantile(temp[,3], 0.5) 
        sim.results[((i*4)-1),2] <- mean(temp[,3]) # mean of distribution of 1,000 observed-value predicted probabilities (Other)
        sim.results[((i*4)-1),3] <- quantile(temp[,3], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Other)
        sim.results[((i*4)-1),4] <- quantile(temp[,3], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Other)
        
        sim.results[((i*4)),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)),2] <- quantile(temp[,4], 0.5) 
        sim.results[((i*4)),2] <- mean(temp[,4]) # mean of distribution of 1,000 observed-value predicted probabilities (Did not vote)
        sim.results[((i*4)),3] <- quantile(temp[,4], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
        sim.results[((i*4)),4] <- quantile(temp[,4], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
        
        if (x[i] == 0) { # min
          primary.probdists.unresp[,1] <- temp[,1]
          primary.probdists.unresp[,2] <- temp[,2]
          primary.probdists.unresp[,3] <- temp[,3]
          primary.probdists.unresp[,4] <- temp[,4]
        }
        
        if (x[i] == 1) { # med
          primary.probdists.unresp[,5] <- temp[,1]
          primary.probdists.unresp[,6] <- temp[,2]
          primary.probdists.unresp[,7] <- temp[,3]
          primary.probdists.unresp[,8] <- temp[,4]
        }
        
        if (x[i] == 2) { # max
          primary.probdists.unresp[,9] <- temp[,1]
          primary.probdists.unresp[,10] <- temp[,2]
          primary.probdists.unresp[,11] <- temp[,3]
          primary.probdists.unresp[,12] <- temp[,4]
        } 
        
      }
      
      primary.effect.unresp <- as.data.frame(sim.results) # put results in data frame
      colnames(primary.effect.unresp)[1] <- "x"
      colnames(primary.effect.unresp)[2] <- "fit"
      colnames(primary.effect.unresp)[3] <- "lower"
      colnames(primary.effect.unresp)[4] <- "upper"
      primary.effect.unresp$voteprimarychoice16 <- rep(c("Sanders","Trump","Other", "Did not vote"),201)
      primary.effect.unresp$category <- "Election Unresp."
    
      # Combine results and plot ----
      primary.effects <- rbind(primary.effect.cynicism, primary.effect.unresp)
      primary.effects$voteprimarychoice16 <- factor(primary.effects$voteprimarychoice16, levels = c("Sanders","Trump","Other", "Did not vote"))
      
      pdf(height = 12, width = 12, "Figures/Probs-Primary.pdf")
      ggplot(primary.effects, aes(x = x, y = fit, colour = voteprimarychoice16)) +
        geom_line(size = 2) +
        geom_line(aes(y = lower), lty = "dashed") +
        geom_line(aes(y = upper), lty = "dashed") +
        labs(x = "", y = "Probability of Vote Choice") +
        facet_grid(rows = vars(voteprimarychoice16), cols = vars(category), scales = "free_y") +
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
      primary.probdists.cynicism <- as.data.frame(primary.probdists.cynicism)
      primary.probdists.unresp <- as.data.frame(primary.probdists.unresp)
      colnames(primary.probdists.cynicism) <- c("sanders.lo","trump.lo","other.lo","dnv.lo",
                                                "sanders.mid","trump.mid","other.mid","dnv.mid",
                                                "sanders.hi","trump.hi","other.hi","dnv.hi")
      colnames(primary.probdists.unresp) <- c("sanders.lo","trump.lo","other.lo","dnv.lo",
                                              "sanders.mid","trump.mid","other.mid","dnv.mid",
                                              "sanders.hi","trump.hi","other.hi","dnv.hi")

      # Cynicism
      round(quantile(primary.probdists.cynicism$sanders.hi - primary.probdists.cynicism$sanders.lo, c(0.05, 0.5, 0.95)),3) # 0.078, [0.038, 0.115]*
      round(quantile(primary.probdists.cynicism$trump.hi - primary.probdists.cynicism$trump.lo, c(0.05, 0.5, 0.95)),3)  # 0.085, [0.013, 0.131]*
      round(quantile(primary.probdists.cynicism$other.hi - primary.probdists.cynicism$other.lo, c(0.05, 0.5, 0.95)),3)  # -0.049, [-0.149, 0.049]
      round(quantile(primary.probdists.cynicism$dnv.hi - primary.probdists.cynicism$dnv.lo, c(0.05, 0.5, 0.95)),3) # -0.107, [-0.190, -0.022]*
      
      # Election Unresponsiveness
      round(quantile(primary.probdists.unresp$sanders.hi - primary.probdists.unresp$sanders.lo, c(0.05, 0.5, 0.95)),3) # -0.027, [-0.075, 0.014]
      round(quantile(primary.probdists.unresp$trump.hi - primary.probdists.unresp$trump.lo, c(0.05, 0.5, 0.95)),3) # 0.007, [-0.058, 0.071]
      round(quantile(primary.probdists.unresp$other.hi - primary.probdists.unresp$other.lo, c(0.05, 0.5, 0.95)),3) # -0.004, [-0.089, 0.089]
      round(quantile(primary.probdists.unresp$dnv.hi - primary.probdists.unresp$dnv.lo, c(0.05, 0.5, 0.95)),3) # 0.023, [-0.049, 0.099]
      
    # General Election Vote Model (Multinomial Logit) ----
    mydata.16$votechoice16 <- factor(mydata.16$votechoice16, levels = c("Clinton","Trump","Other","Did not vote"))
    mydata.general <- mydata.16 %>%
      select(alien.cynicism, elect.attn, rep, dem, ind, ideo7, income.q,
             party.strength, educ, white, female, age, votechoice16) %>%
      drop_na()
    set.seed(219)
    mydata.general$votechoice16 <- relevel(mydata.general$votechoice16, ref = "Did not vote")
    general.mod <- multinom(votechoice16 ~ alien.cynicism + elect.attn  + rep + ind + ideo7 + income.q + party.strength +
                                educ + white + female + age, data = mydata.general, Hess = T)
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
    
      # Simulating Predicted Probabilities ----
      x <- seq(0,2,0.01)
      general.coef <- coef(general.mod)
      general.covar <- vcov(general.mod)
      set.seed(219)
      clintonCoef <- mvrnorm(n = 1000, general.coef[1,], general.covar[1:12,1:12]) # simulate 1000 coefficients
      trumpCoef <- mvrnorm(n = 1000, general.coef[2,], general.covar[13:24,13:24]) # simulate 1000 coefficients
      otherCoef <- mvrnorm(n = 1000, general.coef[3,], general.covar[25:36,25:36]) # simulate 1000 coefficients
      
      # Cynicism ----
      temp <- matrix(NA, nrow = 1000, ncol = 4)
      sim.results <- matrix(NA, nrow = 804, ncol = 4)
      general.probdists.cynicism <- matrix(NA, nrow = 1000, ncol = 12)
      
      for(i in 1:length(x)){
        
        #clintonCoef <- mvrnorm(n = 1000, general.coef[1,], general.covar[1:12,1:12]) # simulate 1000 coefficients
        #trumpCoef <- mvrnorm(n = 1000, general.coef[2,], general.covar[13:24,13:24]) # simulate 1000 coefficients
        #otherCoef <- mvrnorm(n = 1000, general.coef[3,], general.covar[25:36,25:36]) # simulate 1000 coefficients
        
        for(j in 1:nrow(clintonCoef)){
          clintonAg <- exp(clintonCoef[j,1] + # clinton aggregator function
                             clintonCoef[j,2]*x[i] +
                             clintonCoef[j,3]*mydata.general$elect.attn +
                             clintonCoef[j,4]*mydata.general$rep +
                             clintonCoef[j,5]*mydata.general$ind +
                             clintonCoef[j,6]*mydata.general$ideo7 +
                             clintonCoef[j,7]*mydata.general$income.q +
                             clintonCoef[j,8]*mydata.general$party.strength +
                             clintonCoef[j,9]*mydata.general$educ + 
                             clintonCoef[j,10]*mydata.general$white +
                             clintonCoef[j,11]*mydata.general$female +
                             clintonCoef[j,12]*mydata.general$age)
          
          trumpAg <- exp(trumpCoef[j,1] + # trump aggregator function
                           trumpCoef[j,2]*x[i] +
                           trumpCoef[j,3]*mydata.general$elect.attn +
                           trumpCoef[j,4]*mydata.general$rep +
                           trumpCoef[j,5]*mydata.general$ind +
                           trumpCoef[j,6]*mydata.general$ideo7 +
                           trumpCoef[j,7]*mydata.general$income.q +
                           trumpCoef[j,8]*mydata.general$party.strength +
                           trumpCoef[j,9]*mydata.general$educ + 
                           trumpCoef[j,10]*mydata.general$white +
                           trumpCoef[j,11]*mydata.general$female +
                           trumpCoef[j,12]*mydata.general$age)
          
          otherAg <- exp(otherCoef[j,1] + # other aggregator function
                            otherCoef[j,2]*x[i] +
                            otherCoef[j,3]*mydata.general$elect.attn +
                            otherCoef[j,4]*mydata.general$rep +
                            otherCoef[j,5]*mydata.general$ind +
                            otherCoef[j,6]*mydata.general$ideo7 +
                            otherCoef[j,7]*mydata.general$income.q +
                            otherCoef[j,8]*mydata.general$party.strength +
                            otherCoef[j,9]*mydata.general$educ + 
                            otherCoef[j,10]*mydata.general$white +
                            otherCoef[j,11]*mydata.general$female +
                            otherCoef[j,12]*mydata.general$age)
          
          clinton <- clintonAg / (1 + clintonAg + trumpAg + otherAg) # get probabilities
          trump <- trumpAg / (1 + clintonAg + trumpAg + otherAg)
          other <- otherAg / (1 + clintonAg + trumpAg + otherAg)
          novote <- 1 - clinton - trump - other
          
          temp[j,1] <- mean(clinton) # take mean predicted probability of clinton vote across all observable values, store in j-th row, 1st column
          temp[j,2] <- mean(trump)
          temp[j,3] <- mean(other)
          temp[j,4] <- mean(novote)
          
      }
      
        sim.results[((i*4)-3),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)-3),2] <- quantile(temp[,1], 0.5) 
        sim.results[((i*4)-3),2] <- mean(temp[,1]) # mean of distribution of 1,000 observed-value predicted probabilities (Clinton)
        sim.results[((i*4)-3),3] <- quantile(temp[,1], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Clinton)
        sim.results[((i*4)-3),4] <- quantile(temp[,1], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Clinton)
        
        sim.results[((i*4)-2),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)-2),2] <- quantile(temp[,2], 0.5) 
        sim.results[((i*4)-2),2] <- mean(temp[,2]) # mean of distribution of 1,000 observed-value predicted probabilities (Trump)
        sim.results[((i*4)-2),3] <- quantile(temp[,2], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
        sim.results[((i*4)-2),4] <- quantile(temp[,2], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
        
        sim.results[((i*4)-1),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)-1),2] <- quantile(temp[,3], 0.5) 
        sim.results[((i*4)-1),2] <- mean(temp[,3]) # mean of distribution of 1,000 observed-value predicted probabilities (Other)
        sim.results[((i*4)-1),3] <- quantile(temp[,3], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Other)
        sim.results[((i*4)-1),4] <- quantile(temp[,3], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Other)
        
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
        
        if (x[i] == 1) { # med
          general.probdists.cynicism[,5] <- temp[,1]
          general.probdists.cynicism[,6] <- temp[,2]
          general.probdists.cynicism[,7] <- temp[,3]
          general.probdists.cynicism[,8] <- temp[,4]
        } 
        
        if (x[i] == 2) { # max
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
      general.effect.cynicism$votechoice16 <- rep(c("Clinton","Trump","Other","Did not vote"),201)
      general.effect.cynicism$category <- "Cynicism"
      
      # Election Unresponsiveness ----
      temp <- matrix(NA, nrow = 1000, ncol = 4) # create empty matrices for results
      sim.results <- matrix(NA, nrow = 804, ncol = 4)
      general.probdists.unresp <- matrix(NA, nrow = 1000, ncol = 12)
      
      for(i in 1:length(x)){
        
        #clintonCoef <- mvrnorm(n = 1000, general.coef[1,], general.covar[1:12,1:12]) # simulate 1000 coefficients
        #trumpCoef <- mvrnorm(n = 1000, general.coef[2,], general.covar[13:24,13:24]) # simulate 1000 coefficients
        #otherCoef <- mvrnorm(n = 1000, general.coef[3,], general.covar[25:36,25:36]) # simulate 1000 coefficients
        
        for(j in 1:nrow(clintonCoef)){
          clintonAg <- exp(clintonCoef[j,1] + # clinton aggregator function
                             clintonCoef[j,2]*mydata.general$alien.cynicism +
                             clintonCoef[j,3]*x[i] +
                             clintonCoef[j,4]*mydata.general$rep +
                             clintonCoef[j,5]*mydata.general$ind +
                             clintonCoef[j,6]*mydata.general$ideo7 +
                             clintonCoef[j,7]*mydata.general$income.q +
                             clintonCoef[j,8]*mydata.general$party.strength +
                             clintonCoef[j,9]*mydata.general$educ + 
                             clintonCoef[j,10]*mydata.general$white +
                             clintonCoef[j,11]*mydata.general$female +
                             clintonCoef[j,12]*mydata.general$age)
          
          trumpAg <- exp(trumpCoef[j,1] + # trump aggregator function
                           trumpCoef[j,2]*mydata.general$alien.cynicism +
                           trumpCoef[j,3]*x[i] +
                           trumpCoef[j,4]*mydata.general$rep +
                           trumpCoef[j,5]*mydata.general$ind +
                           trumpCoef[j,6]*mydata.general$ideo7 +
                           trumpCoef[j,7]*mydata.general$income.q +
                           trumpCoef[j,8]*mydata.general$party.strength +
                           trumpCoef[j,9]*mydata.general$educ + 
                           trumpCoef[j,10]*mydata.general$white +
                           trumpCoef[j,11]*mydata.general$female +
                           trumpCoef[j,12]*mydata.general$age)
          
          otherAg <- exp(otherCoef[j,1] + # other aggregator function
                            otherCoef[j,2]*mydata.general$alien.cynicism +
                            otherCoef[j,3]*x[i] +
                            otherCoef[j,4]*mydata.general$rep +
                            otherCoef[j,5]*mydata.general$ind +
                            otherCoef[j,6]*mydata.general$ideo7 +
                            otherCoef[j,7]*mydata.general$income.q +
                            otherCoef[j,8]*mydata.general$party.strength +
                            otherCoef[j,9]*mydata.general$educ + 
                            otherCoef[j,10]*mydata.general$white +
                            otherCoef[j,11]*mydata.general$female +
                            otherCoef[j,12]*mydata.general$age)
          
          clinton <- clintonAg / (1 + clintonAg + trumpAg + otherAg) # get probabilities
          trump <- trumpAg / (1 + clintonAg + trumpAg + otherAg)
          other <- otherAg / (1 + clintonAg + trumpAg + otherAg)
          novote <- 1 - clinton - trump - other
          
          temp[j,1] <- mean(clinton) # take mean predicted probability of clinton vote across all observable values, store in j-th row, 1st column
          temp[j,2] <- mean(trump)
          temp[j,3] <- mean(other)
          temp[j,4] <- mean(novote)
        }
        
        sim.results[((i*4)-3),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)-3),2] <- quantile(temp[,1], 0.5) 
        sim.results[((i*4)-3),2] <- mean(temp[,1]) # mean of distribution of 1,000 observed-value predicted probabilities (Clinton)
        sim.results[((i*4)-3),3] <- quantile(temp[,1], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Clinton)
        sim.results[((i*4)-3),4] <- quantile(temp[,1], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Clinton)
        
        sim.results[((i*4)-2),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)-2),2] <- quantile(temp[,2], 0.5) 
        sim.results[((i*4)-2),2] <- mean(temp[,2]) # mean of distribution of 1,000 observed-value predicted probabilities (Trump)
        sim.results[((i*4)-2),3] <- quantile(temp[,2], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
        sim.results[((i*4)-2),4] <- quantile(temp[,2], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Trump)
        
        sim.results[((i*4)-1),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)-1),2] <- quantile(temp[,3], 0.5) 
        sim.results[((i*4)-1),2] <- mean(temp[,3]) # mean of distribution of 1,000 observed-value predicted probabilities (Other)
        sim.results[((i*4)-1),3] <- quantile(temp[,3], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Other)
        sim.results[((i*4)-1),4] <- quantile(temp[,3], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Other)
        
        sim.results[((i*4)),1] <- x[i] # capture 'x' value
        #sim.results[((i*4)),2] <- quantile(temp[,4], 0.5) 
        sim.results[((i*4)),2] <- mean(temp[,4]) # mean of distribution of 1,000 observed-value predicted probabilities (Did not vote)
        sim.results[((i*4)),3] <- quantile(temp[,4], 0.05) # lower-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
        sim.results[((i*4)),4] <- quantile(temp[,4], 0.95) # right-tail of distribution of 1,000 observed-value predicted probabilities (Did not vote)
        
        if (x[i] == 0) { # min
          general.probdists.unresp[,1] <- temp[,1]
          general.probdists.unresp[,2] <- temp[,2]
          general.probdists.unresp[,3] <- temp[,3]
          general.probdists.unresp[,4] <- temp[,4]
        }
        
        if (x[i] == 1) { # med
          general.probdists.unresp[,5] <- temp[,1]
          general.probdists.unresp[,6] <- temp[,2]
          general.probdists.unresp[,7] <- temp[,3]
          general.probdists.unresp[,8] <- temp[,4]
        } 
        
        if (x[i] == 2) { # max
          general.probdists.unresp[,9] <- temp[,1]
          general.probdists.unresp[,10] <- temp[,2]
          general.probdists.unresp[,11] <- temp[,3]
          general.probdists.unresp[,12] <- temp[,4]
        } 
        
      }
      
      general.effect.unresp <- as.data.frame(sim.results) # put results in data frame
      colnames(general.effect.unresp)[1] <- "x"
      colnames(general.effect.unresp)[2] <- "fit"
      colnames(general.effect.unresp)[3] <- "lower"
      colnames(general.effect.unresp)[4] <- "upper"
      general.effect.unresp$votechoice16 <- rep(c("Clinton","Trump","Other", "Did not vote"),201)
      general.effect.unresp$category <- "Election Unresp."
      
      # Combine results and plot ----
      general.effects <- rbind(general.effect.cynicism, general.effect.unresp)
      general.effects$votechoice16 <- factor(general.effects$votechoice16, levels = c("Clinton","Trump","Other", "Did not vote"))
      
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
      general.probdists.unresp <- as.data.frame(general.probdists.unresp)
      colnames(general.probdists.cynicism) <- c("clinton.lo","trump.lo","other.lo","dnv.lo",
                                                "clinton.mid","trump.mid","other.mid","dnv.mid",
                                                "clinton.hi","trump.hi","other.hi","dnv.hi")
      colnames(general.probdists.unresp) <- c("clinton.lo","trump.lo","other.lo","dnv.lo",
                                              "clinton.mid","trump.mid","other.mid","dnv.mid",
                                              "clinton.hi","trump.hi","other.hi","dnv.hi")
      
      # Cynicism
      round(quantile(general.probdists.cynicism$clinton.hi - general.probdists.cynicism$clinton.lo, c(0.05, 0.5, 0.95)),3) # -0.033, [-0.071, 0.006]
      round(quantile(general.probdists.cynicism$trump.hi - general.probdists.cynicism$trump.lo, c(0.05, 0.5, 0.95)),3)  # 0.075, [0.033, 0.115]*
      round(quantile(general.probdists.cynicism$other.hi - general.probdists.cynicism$other.lo, c(0.05, 0.5, 0.95)),3)  # 0.016, [-0.009, 0.038]
      round(quantile(general.probdists.cynicism$dnv.hi - general.probdists.cynicism$dnv.lo, c(0.05, 0.5, 0.95)),3) # -0.057, [-0.093, -0.018]*
      
      # Election Unresponsiveness
      round(quantile(general.probdists.unresp$clinton.hi - general.probdists.unresp$clinton.lo, c(0.05, 0.5, 0.95)),3) # -0.017, [-0.050, 0.018]
      round(quantile(general.probdists.unresp$trump.hi - general.probdists.unresp$trump.lo, c(0.05, 0.5, 0.95)),3) # 0.000, [-0.032, 0.033]
      round(quantile(general.probdists.unresp$other.hi - general.probdists.unresp$other.lo, c(0.05, 0.5, 0.95)),3) # 0.002, [-0.019, 0.023]
      round(quantile(general.probdists.unresp$dnv.hi - general.probdists.unresp$dnv.lo, c(0.05, 0.5, 0.95)),3) # 0.015, [-0.017, 0.044]
      
    
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
    
    