# Maxwell B. Allamong
# Alie(n)ation: Political Outsiders in the 2016 Election - STM
# Created: Jan. 25th, 2020
# Updated: Apr. 28th, 2020

# READ ME ----
  #  To replicate this analysis, set your working director to the folder that                   
  #  contains the data files ("anes_timeseries_2016_redacted_openends.xlsx", "ANES-2016.dta", 
  # "ANES-2012.dta", "ANES-CDF.dta"), and the source script ("Alienation-Source.R")                                              

# Working Directory ----
  #setwd(here())
    
# Source Data ----
  source("Alienation-Source.R")

# Options ----
  options(scipen = 999)
  
# STM ----
  # Preparation ----
    
    # Subset to necessary variables
    mydata.stm <- mydata.16 %>%
      select(alien.cynicism, trust, big.interests, corrupt, elect.attn,
             female, age, pid3, documents, income.q, pol.int,
             ideo7, educ, white) %>%
      drop_na()
    
    # Remove certain symbols
    mydata.stm$documents <- gsub("(", " ", mydata.stm$documents, fixed = TRUE) # replace left paranthesis with space
    mydata.stm$documents <- gsub(")", " ", mydata.stm$documents, fixed = TRUE) # replace right paranthesis with space
    mydata.stm$documents <- gsub("/", " ", mydata.stm$documents, fixed = TRUE) # replace backslash with space
    mydata.stm$documents <- gsub("\\", " ", mydata.stm$documents, fixed = TRUE) # replace forwardslash with space
    mydata.stm$documents <- gsub("-", " ", mydata.stm$documents, fixed = TRUE) # replace hyphen with space
    mydata.stm$documents <- gsub("'", " ", mydata.stm$documents, fixed = TRUE) # replace apostrophe with space
  
    # Pre-Process
    processed <- textProcessor(mydata.stm$documents, metadata = mydata.stm)
    shortdocs <- mydata.stm$documents # create shortdocs for 'exemplary' texts
    shortdocs <- shortdocs[-c(processed$docs.removed)]
    
    # Plot feature removal
    plotRemoved(processed$documents, lower.thresh = seq(0, 20, by = 2))
    
    # Remove unnecessary features
    out <- prepDocuments(processed$documents, processed$vocab,
                         processed$meta, lower.thresh = 1)
    shortdocs <- shortdocs[-c(out$docs.removed)] # update 'shortdocs'
    docs <- out$documents
    vocab <- out$vocab
    metadata <-out$meta
    
  # Model Selection and Exploration ----
    
    # Search for number of topics ----
    selectingK.wide <- searchK(out$documents, out$vocab, K = c(20,25,30,35,40),
                       prevalence =~ alien.cynicism + elect.attn, data = metadata)
    plot(selectingK.wide)
    selectingK.narrow <- searchK(out$documents, out$vocab, K = c(24,25,26,27),
                               prevalence =~ alien.cynicism + elect.attn, data = metadata)
    pdf(height = 6,width = 7, "Figures/selectingK.pdf")
    plot(selectingK.narrow)
    dev.off()

    # Generate topic model with X topics and different initializations ----
    # Additive
    fit.add <- selectModel(docs, vocab, K = 26,
                           prevalence =~ alien.cynicism + elect.attn,
                           max.em.its = 75, runs = 30, data = metadata,
                           seed = 219)
    pdf(height = 4,width = 6, "Figures/selectModel.pdf")
    par(cex = 1, lwd = 2, mar = c(4,4,2,2))
    plotModels(fit.add, pch = c(1,2,3,4,5,6))
    dev.off()
    fit.add <- fit.add$runout[[4]] # select the best model
    
    # Interaction
    fit.int <- selectModel(docs, vocab, K = 26,
                       prevalence =~ alien.cynicism * elect.attn,
                       max.em.its = 75, runs = 30, data = metadata,
                       seed = 219)
    plotModels(fit.int, pch = c(1,2,3,4,5,6)) # results are exact same as fit.add
    fit.int <- fit.int$runout[[4]] # select the best model
    
    # Topic Proportions -----
    # All topics
    pdf(height = 8,width = 12, "Figures/alltopics.pdf")
    par(cex = 1, lwd = 2)
    plot(fit.add, type = "summary", labeltype = "frex", n = 6)
    dev.off()
    
    # Top 8 topics
    pdf(height = 8, width = 8, "Figures/top8topics.pdf")
    par(cex = 1.35, lwd = 2, mar = c(4,1,1,1))
    plot(fit.add, type = "summary", labeltype = "frex", 
         topics = c(12,5,23,2,21,26,1,4), n = 5, main = "",
         topic.names = c("","","","","","","",""),
         #custom.labels = c("","","","","","","",""),
         #topic.names = c("(Alternative to Sec. Clinton)", "(Business Experience)", 
                           #"(Political Outsider)", "(Supreme Court)", 
                           #"(Immigration)", "(Lesser of Two Evils)", 
                           #"(Economy)","(Make America Great Again)"), xlim = c(0,0.08),
    xlim = c(0,0.14),
    ylim = c(0.5,8.5))
    text(0,8.25,"(Alternative to Sec. Clinton)", pos = 4, col = "red")
    text(0,7.25,"(Business Experience)", pos = 4, col = "red")
    text(0,6.25,"(Political Outsider)", pos = 4, col = "red")
    text(0,5.25,"(Supreme Court)", pos = 4, col = "red")
    text(0,4.25,"(Immigration)", pos = 4, col = "red")
    text(0,3.25,"(Lesser of Two Evils)", pos = 4, col = "red")
    text(0,2.25,"(Economy)", pos = 4, col = "red")
    text(0,1.25,"(MAGA)", pos = 4, col = "red")
    dev.off()
    
    # Exemplary Documents -----
    (outsider.thoughts <- findThoughts(fit.add, texts = shortdocs, 
                               topics = c(23), n = 175)$docs[[1]])
    outsider.thoughts <- c(outsider.thoughts[27], outsider.thoughts[30], outsider.thoughts[87], 
                      outsider.thoughts[110], outsider.thoughts[169], outsider.thoughts[175]) 
    pdf(height = 8, width = 8, "Figures/exemplar-texts.pdf")
    par(cex = 1.35, lwd = 2, mar = c(4,1,1,1))
    plotQuote(outsider.thoughts, text.cex = 1, width = 50)
    dev.off()
    
  # Estimate ----
    
    # Additive ----
    set.seed(46360)
    alien.add <- estimateEffect(c(12,5,23,2) ~  alien.cynicism + elect.attn + pid3 + pol.int,
                               fit.add, meta = out$meta, uncertainty = "Global")
    set.seed(46360)
    summary(alien.add, topics=c(12,5,23,2))
    
    set.seed(46360)
    alien.robust <- estimateEffect(c(23) ~  alien.cynicism + elect.attn + pid3 + pol.int +
                                     ideo7 + educ + white + female + age,
                                fit.add, meta = out$meta, uncertainty = "Global")
    set.seed(46360)
    summary(alien.robust, topics=c(23))
    
    
    
    
    # Interactive ----
    set.seed(46360)
    alien.int <- estimateEffect(c(23) ~ alien.cynicism * elect.attn + pid3 + pol.int,
                                fit.int, meta = out$meta, uncertainty = "Global")
    summary(alien.int, topics=c(23))
      
  # Visualize ----
    
    # Additive Model Coefficient Plots ----
    set.seed(46360)
    topic12.estimate <- as.data.frame(summary(alien.add)$tables[[1]])
    colnames(topic12.estimate) <-  c("estimate","std.error","t.value","p.value")
    topic12.estimate$var <- rownames(topic12.estimate)
    topic12.estimate$lwr <- topic12.estimate$estimate - (2*topic12.estimate$std.error)
    topic12.estimate$upr <- topic12.estimate$estimate + (2*topic12.estimate$std.error)
    
    set.seed(46360)
    topic5.estimate <- as.data.frame(summary(alien.add)$tables[[2]])
    colnames(topic5.estimate) <-  c("estimate","std.error","t.value","p.value")
    topic5.estimate$var <- rownames(topic5.estimate)
    topic5.estimate$lwr <- topic5.estimate$estimate - (2*topic5.estimate$std.error)
    topic5.estimate$upr <- topic5.estimate$estimate + (2*topic5.estimate$std.error)
    
    set.seed(46360)
    topic23.estimate <- as.data.frame(summary(alien.add)$tables[[3]])
    colnames(topic23.estimate) <-  c("estimate","std.error","t.value","p.value")
    topic23.estimate$var <- rownames(topic23.estimate)
    topic23.estimate$lwr <- topic23.estimate$estimate - (2*topic23.estimate$std.error)
    topic23.estimate$upr <- topic23.estimate$estimate + (2*topic23.estimate$std.error)
    
    set.seed(46360)
    topic2.estimate <- as.data.frame(summary(alien.add)$tables[[4]])
    colnames(topic2.estimate) <-  c("estimate","std.error","t.value","p.value")
    topic2.estimate$var <- rownames(topic2.estimate)
    topic2.estimate$lwr <- topic2.estimate$estimate - (2*topic2.estimate$std.error)
    topic2.estimate$upr <- topic2.estimate$estimate + (2*topic2.estimate$std.error)
    
    pdf(height = 8, width = 8, "Figures/STM-Additive-Topic12.pdf")
    ggplot(data = topic12.estimate, aes(x = var,y = estimate)) +  
      geom_point(position=position_dodge(width = .5), size = 4) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0 , size = 1) +
      labs(x = "", y = "\nCoefficient Estimate") +
      ylim(-0.05,0.10) +
      scale_x_discrete(limits = c("pol.int", "pid3Ind", "pid3Rep",
                                  "elect.attn","alien.cynicism","(Intercept)"),
                       labels = c("Political Interest","Independent","Republican",
                                  "Unresponsiveness","Cynicism","(Intercept)")) +
      coord_flip() + 
      theme(axis.line = element_line(colour = "black"),
            plot.subtitle = element_text(vjust = 1), 
            plot.caption = element_text(vjust = 1), 
            plot.margin = margin(0.5,3,0.5,0.5,"cm"),
            panel.background = element_rect(fill = "white", colour = NA),
            panel.grid.major = element_line(linetype = "blank"), 
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title = element_text(size = 24),
            axis.text = element_text(size = 20))
    dev.off()
    
    pdf(height = 8, width = 8, "Figures/STM-Additive-Topic5.pdf")
    ggplot(data = topic5.estimate, aes(x = var,y = estimate)) +  
      geom_point(position=position_dodge(width = .5), size = 4) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0 , size = 1) +
      labs(x = "", y = "\nCoefficient Estimate") +
      ylim(-0.05,0.10) +
      coord_flip() + 
      scale_x_discrete(limits = c("pol.int", "pid3Ind", "pid3Rep",
                                  "elect.attn","alien.cynicism","(Intercept)"),
                       labels = c("Political Interest","Independent","Republican",
                                  "Unresponsiveness","Cynicism","(Intercept)")) +
      theme(axis.line = element_line(colour = "black"),
            plot.subtitle = element_text(vjust = 1), 
            plot.caption = element_text(vjust = 1), 
            plot.margin = margin(0.5,3,0.5,0.5,"cm"),
            panel.background = element_rect(fill = "white", colour = NA),
            panel.grid.major = element_line(linetype = "blank"), 
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title = element_text(size = 24),
            axis.text = element_text(size = 20))
    dev.off()
    
    pdf(height = 8, width = 8, "Figures/STM-Additive-Topic23.pdf")
    ggplot(data = topic23.estimate, aes(x = var,y = estimate)) +  
      geom_point(position=position_dodge(width = .5), size = 4) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0 , size = 1) +
      labs(x = "", y = "\nCoefficient Estimate") +
      ylim(-0.05,0.10) +
      coord_flip() + 
      scale_x_discrete(limits = c("pol.int", "pid3Ind", "pid3Rep",
                                  "elect.attn","alien.cynicism","(Intercept)"),
                       labels = c("Political Interest","Independent","Republican",
                                  "Unresponsiveness","Cynicism","(Intercept)")) +
      theme(axis.line = element_line(colour = "black"),
            plot.subtitle = element_text(vjust = 1), 
            plot.caption = element_text(vjust = 1), 
            plot.margin = margin(0.5,3,0.5,0.5,"cm"),
            panel.background = element_rect(fill = "white", colour = NA),
            panel.grid.major = element_line(linetype = "blank"), 
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title = element_text(size = 24),
            axis.text = element_text(size = 20))
    dev.off()
    
    pdf(height = 8, width = 8, "Figures/STM-Additive-Topic2.pdf")
    ggplot(data = topic2.estimate, aes(x = var,y = estimate)) +  
      geom_point(position=position_dodge(width = .5), size = 4) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0 , size = 1) +
      labs(x = "", y = "\nCoefficient Estimate") +
      ylim(-0.05,0.10) +
      coord_flip() + 
      scale_x_discrete(limits = c("pol.int", "pid3Ind", "pid3Rep",
                                  "elect.attn","alien.cynicism","(Intercept)"),
                       labels = c("Political Interest","Independent","Republican",
                                  "Unresponsiveness","Cynicism","(Intercept)")) +
      theme(axis.line = element_line(colour = "black"),
            plot.subtitle = element_text(vjust = 1), 
            plot.caption = element_text(vjust = 1), 
            plot.margin = margin(0.5,3,0.5,0.5,"cm"),
            panel.background = element_rect(fill = "white", colour = NA),
            panel.grid.major = element_line(linetype = "blank"), 
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title = element_text(size = 24),
            axis.text = element_text(size = 20))
    dev.off()
        

  


    # Interaction of Cynicism and Election Unresponsiveness ----
    pdf(width = 10, height = 8, "Figures/Probs-Interaction-OutsiderTopic.pdf")
    par(cex = 1.7, lwd = 1.7)
    y <- plot(alien.int, covariate = "alien.cynicism", method = "continuous", topics = c(23), ci = 0.9,
              xlab = "Cynicism", ylim = c(-0.05,.15), linecol = "blue",
              moderator = "elect.attn", moderator.value = 0, printlegend = F)
    z <- plot(alien.int, covariate = "alien.cynicism", method = "continuous", topics = c(23), ci = 0.9,
              moderator = "elect.attn", moderator.value = 2, printlegend = F, add = T)
    legend("topleft", c("Unresponsive", "Responsive"),
           lwd = 2, col = c("red","blue"))
    dev.off()
    y$means[[1]][100]-y$means[[1]][1] 
    z$means[[1]][100]-z$means[[1]][1]
        
        
    
    
    
    
    
    
    
    
    
  # Save Document Topic Proportions w/ Meta Data ----
    dt <- make.dt(fit.add, meta=metadata)    
    dt <- dt %>%
      mutate(outsider.topic = do.call(pmax, c(select(., c(2:27)), na.rm = T))) %>%
      mutate(outsider.topic.b = ifelse(outsider.topic == Topic23, 1, 0))
    
    y <- lm(outsider.topic.b ~ alien.cynicism + elect.attn + pid3 + pol.int, data = dt)
    summary(y)
    
    y <- lm(responded ~ alien.cynicism + elect.attn + pid3 + pol.int, data = mydata.16)
    summary(y)
    
    y <- lm(like.trump ~ alien.cynicism + elect.attn + pid3 + pol.int, data = mydata.16)
    summary(y)
    
    
# Plots ----
        # Top 4 Topics
        # pdf(width = 5, height = 10, "Figures/alienation-outsider.pdf")
        # par(cex = 1.5, lwd = 3, col = "black", mar = c(5,5,2,2), mfrow = c(2,1))
        # y <- plot(alien.add, covariate = "alien.cynicism", method = "difference",
        #           cov.value1 = 1, cov.value2 = 0, ci = 0.95,
        #           custom.labels = c("","","",""), labeltype = "custom",
        #           xlim = c(-0.025, 0.025), xlab = "Effect of One-Unit Increase in Cynicism on Topic Use")
        # text(y$means[[1]], 4.2, "Alternative to Secretary Clinton")
        # text(y$means[[2]], 3.2, "Business Experience")
        # text(y$means[[3]], 2.2, "Political Outsider")
        # text(y$means[[4]], 1.2, "Supreme Court")
        # 
        # z <- plot(alien.add, covariate = "elect.attn", method = "difference",
        #           cov.value1 = 1, cov.value2 = 0, ci = 0.95,
        #           custom.labels = c("","","",""), labeltype = "custom",
        #           xlim = c(-0.025, 0.025), xlab = "Effect of One-Unit Increase in Election Unresponsiveness on Topic Use")
        # text(z$means[[1]], 4.2, "Alternative to Secretary Clinton")
        # text(z$means[[2]], 3.2, "Business Experience")
        # text(z$means[[3]], 2.2, "Political Outsider")
        # text(z$means[[4]], 1.2, "Supreme Court")
        # dev.off()
      
          
          
          
          
          
# STILL TO FIX ----    
          pdf(width = 9, height = 6.5, "Figures/int-alienation-outsider.pdf")
          par(cex = 1.7, lwd = 2, mar = c(5,5,2,2))
          y <- plot(alien.add, covariate = "alien.normless", method = "continuous", topics = c(12), ci = 0.9,
               xlab = "Normlessness", ylim = c(-0.05,.15), linecol = "blue",
               moderator = "elect.attn", moderator.value = 3,  printlegend = F)
          z <- plot(alien.add, covariate = "alien.normless", method = "continuous", topics = c(12), ci = 0.9,
               moderator = "elect.attn", moderator.value = 1, printlegend = F, add = T)
          legend("topleft", c("More Responsive","Less Responsive"),
                 lwd = 2, col = c("red","blue"))
          dev.off()
          y$means[[1]][100]-y$means[[1]][1] # Dems: 0.6% to 7.6% = 7.0%
          z$means[[1]][100]-z$means[[1]][1] # Reps: 3.7% to 5.3%  = 1.6%
          
          plot(mar = c(2,2,2,2))
          plot(alien.int, covariate = "alien.cynicism", cov.value1 = 0, cov.value2 = 2, method = "difference", 
               topics = c(23), ci = 0.9, moderator = "elect.attn", moderator.value = 2,
               xlim = c(-0.2,0.2))
          plot(alien.int, covariate = "alien.cynicism", cov.value1 = 0, cov.value2 = 2, method = "difference", 
               topics = c(23), ci = 0.9, moderator = "elect.attn", moderator.value = 0,
               xlim = c(-0.2,0.2))
          
          # Independents
          pdf(width = 9, height = 6.5, "Figures/int-alienation-outsider-ind.pdf")
          par(cex = 1.7, lwd = 2, mar = c(5,5,2,2))
          y <- plot(alien.int, covariate = "alienation", method = "continuous", 
               topics = c(3), ci = 0.9, linecol = "purple", moderator ="pid3", 
               xlab = "Political Alienation Scale", custom.labels = c("Not a Politician"),
               moderator.value = "Ind", ylim = c(-0.05,.15), printlegend = F)
          dev.off()
          y$means[[1]][100]-y$means[[1]][1] # Inds:: 1.4% to 7.0%  = 5.6%
          
        # Topic 9 (Anti-Establishment)
          # Partisans
          pdf(width = 9, height = 6.5, "Figures/int-alienation-antiestablishment.pdf")
          par(cex = 1.7, lwd = 2, mar = c(5,5,2,2))
          y <- plot(alien.int, covariate = "alien.cynicism", method = "continuous", topics = c(23), ci = 0.9,
               xlab = "Political Alienation Scale", ylim = c(-0.05,.15), linecol = "blue",
               moderator = "elect.attn", moderator.value = 0, printlegend = F)
          z <- plot(alien.int, covariate = "alien.cynicism", method = "continuous", topics = c(23), ci = 0.9,
               moderator = "elect.attn", moderator.value = 2, printlegend = F, add = T)
          legend("topleft", c("Republican","Democrat"),
                 lwd = 2, col = c("red","blue"))
          dev.off()
          y$means[[1]][100]-y$means[[1]][1] # Dems: 3.0% to 11.1%  = 8.1%
          z$means[[1]][100]-z$means[[1]][1] # Reps: 3.9% to 4.9% = 1.0%
          
          # Independents
          pdf(width = 9, height = 6.5, "Figures/int-alienation-antiestablishment-ind.pdf")
          par(cex = 1.7, lwd = 2, mar = c(5,5,2,2))
          y <- plot(alien.int, covariate = "alienation", method = "continuous", 
               topics = c(3), ci = 0.9, linecol = "purple", moderator ="pid3", 
               xlab = "Political Alienation Scale", custom.labels = c("Not a Politician"),
               moderator.value = "Ind", ylim = c(-0.05,.15), printlegend = F)
          dev.off()
          y$means[[1]][100]-y$means[[1]][1] # Inds: 1.4% to 7.1% = 5.7%
          

  dt$test <- as.numeric(quantcut(dt$Topic9, 10))
  dt <- dt %>%
    mutate(top9 = ifelse(Topic9 >= 0.2, 1, 0))

  dt <- mydata %>%
    left_join(dt, by = "id") %>%
    mutate(top3 = replace_na(top3, 0))
  
  dt <- mydata %>%
    left_join(dt, by = "id") %>%
    filter(vote16.x == 1)
  
  
  test <- dt %>%
    gather(topic, top.prop, Topic1:Topic25) %>%
    group_by(id) %>%
    slice(which.max(top.prop)) %>%
    right_join(mydata, by = "id") %>%
    filter(vote16 == 1) %>%
    mutate(outsider.topic = ifelse(topic == "Topic9", 1, 0))
  