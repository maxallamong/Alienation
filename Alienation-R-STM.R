# Maxwell B. Allamong
# Political Alienation and the Trump Vote in the 2016-2020 U.S. Presidential Elections
# File: STM 
# Updated: Jun. 11th, 2023

# README ----
  #  To replicate this analysis, set your working director to the folder that                   
  #  contains the data files ("anes_timeseries_2016_redacted_openends.xlsx", "ANES-2016.dta", 
  # "ANES-2012.dta", "ANES-CDF.dta"), and the source script ("Alienation-Source.R")                                              

# Packages ----
# Libraries ----
  library(tidyverse)
  library(stm)
  library(tm)
  library(hunspell)
  library(tidytext)

# Working Directory ----
#setwd(here())
# Commands ----
`%!in%` <- Negate(`%in%`)

# Load Data ----
  mydata.20 <- read.csv("Data/mydata-20-3.csv")
  mydata.20 <- mydata.20[,-1] # remove unnecessary first column
  design.vars.20 <- mydata.20 %>%
    select(weight, strata, cluster, caseid, year) %>%
    mutate(mode = "Web")
  mydata.20 <- mydata.20 %>%
    select(-weight, -strata, -cluster)
  
  mydata.16 <- read.csv("Data/mydata-16-3.csv")
  mydata.16 <- mydata.16[,-1] # remove unnecessary first column
  design.vars.16 <- mydata.16 %>%
    select(weight, strata, cluster, caseid, year, mode)
  mydata.16 <- mydata.16 %>%
    select(-weight, -strata, -cluster, -mode)
  
  
  design.vars <- rbind(design.vars.16, design.vars.20)
  

  
# Options ----
  options(scipen = 999)
  options(survey.lonely.psu="adjust")
  
# Spelling and Translation ----
  ## Translation ----
    ### 2016 ----
    mydata.16 <- mydata.16 %>%# detect non-english (i.e., spanish) responses
      mutate(likes.trump.text.lang1 = cld2::detect_language(likes.trump.text)) %>% 
      mutate(likes.trump.text.lang2 = cld3::detect_language(likes.trump.text)) %>%
      mutate(likes.trump.nonenglish = ifelse(likes.trump.text.lang1 != "en" & likes.trump.text.lang2 != "en", 1, 0)) 
    
    mydata.16.covars <- mydata.16 %>% # save covariates in separate frame
      select(-likes.trump.text)
    
    mydata.16.nonenglish <- mydata.16 %>% # save non-english responses in separate frame
      filter(likes.trump.nonenglish == 1) %>%
      select(likes.trump.text, caseid)
    
    mydata.16.english <- mydata.16 %>% # save english responses in a separate frame
      filter(caseid %!in% mydata.16.nonenglish$caseid) %>%
      select(likes.trump.text, caseid)
    
    write.csv(mydata.16.nonenglish, "Data/ANES/Open Ends/Translation/mydata_16_nonenglish.csv") # export non-english responses
    
    mydata.16.nonenglish <- read.csv("Data/ANES/Open Ends/Translation/mydata_16_nonenglish_translated.csv") # read-in translated responses
    
    mydata.temp <- rbind(mydata.16.nonenglish, # combine translated responses with english responses
                         mydata.16.english)
    
    mydata.16 <- mydata.16.covars %>% # reattach covariates to all responses
      left_join(mydata.temp, by = c("caseid"))
    
    ### 2020 ----
    mydata.20 <- mydata.20 %>%# detect non-english (i.e., spanish) responses
      mutate(likes.trump.text.lang1 = cld2::detect_language(likes.trump.text)) %>% 
      mutate(likes.trump.text.lang2 = cld3::detect_language(likes.trump.text)) %>%
      mutate(likes.trump.nonenglish = ifelse(likes.trump.text.lang1 != "en" & likes.trump.text.lang2 != "en", 1, 0)) 
    
    mydata.20.covars <- mydata.20 %>% # save covariates in separate frame
      select(-likes.trump.text)
    
    mydata.20.nonenglish <- mydata.20 %>% # save non-english responses in separate frame
      filter(likes.trump.nonenglish == 1) %>%
      select(likes.trump.text, caseid)
    
    mydata.20.english <- mydata.20 %>% # save english responses in a separate frame
      filter(caseid %!in% mydata.20.nonenglish$caseid) %>%
      select(likes.trump.text, caseid)
    
    write.csv(mydata.20.nonenglish, "Data/ANES/Open Ends/Translation/mydata_20_nonenglish.csv") # export non-english responses
    
    mydata.20.nonenglish <- read.csv("Data/ANES/Open Ends/Translation/mydata_20_nonenglish_translated.csv") # read-in translated responses
    
    mydata.temp <- rbind(mydata.20.nonenglish, # combine translated responses with english responses
                         mydata.20.english)
    
    mydata.20 <- mydata.20.covars %>% # reattach covariates to all responses
      left_join(mydata.temp, by = c("caseid"))
    
  ## Spelling ----
    ### 2016 ----
    mydata.temp <- mydata.16 %>% # find misspelled words
      select(likes.trump.text) %>%
      unnest_tokens(term, likes.trump.text, drop = F) %>%
      mutate(spellcheck = hunspell_check(term)) %>%
      filter(spellcheck == FALSE) %>%
      select(likes.trump.text, term, spellcheck)
    write.csv(mydata.temp, "Data/ANES/Open Ends/Spelling/misspellings-trump-16.csv") # export misspelled words, take to excel to create dictionary to fix misspellings
    
    spellcheck.trump.16 <- read.csv("Data/ANES/Open Ends/Spelling/spellcheck-trump-16.csv") # read in spell check dictionary
    froms <- spellcheck.trump.16$term # misspelled words
    tos <- spellcheck.trump.16$correction # spelling corrections
    cols <- colnames(mydata.16)[1:86]
    
    mydata.16 <- mydata.16 %>%
      unnest_tokens(term, likes.trump.text) %>%
      mutate(spellcheck = hunspell_check(term)) %>%
      mutate(term = ifelse(spellcheck == F, plyr::mapvalues(term, from = froms, to = tos), term)) %>%
      group_by(across(all_of(cols))) %>%
      summarize(likes.trump.text = str_c(term, collapse = " ")) %>%
      ungroup()
    
    
    ### 2020 ----
    mydata.temp <- mydata.20 %>% # find misspelled words
      select(likes.trump.text) %>%
      unnest_tokens(term, likes.trump.text, drop = F) %>%
      mutate(spellcheck = hunspell_check(term)) %>%
      filter(spellcheck == FALSE) %>%
      select(likes.trump.text, term, spellcheck)
    write.csv(mydata.temp, "Data/ANES/Open Ends/Spelling/misspellings-trump-20.csv") # export misspelled words, take to excel to create dictionary to fix misspellings
    
    spellcheck.trump.20 <- read.csv("Data/ANES/Open Ends/Spelling/spellcheck-trump-20.csv") # read in spell check dictionary
    froms <- spellcheck.trump.20$term # misspelled words
    tos <- spellcheck.trump.20$correction # spelling corrections
    cols <- colnames(mydata.20)[1:70]
    
    mydata.20 <- mydata.20 %>%
      unnest_tokens(term, likes.trump.text) %>%
      mutate(spellcheck = hunspell_check(term)) %>%
      mutate(term = ifelse(spellcheck == F, plyr::mapvalues(term, from = froms, to = tos), term)) %>%
      group_by(across(all_of(cols))) %>%
      summarize(likes.trump.text = str_c(term, collapse = " ")) %>%
      ungroup()
    
  ## Remove unnecessary data frames
    rm(mydata.16.covars, mydata.20.covars,
       mydata.16.english, mydata.20.english,
       mydata.16.nonenglish, mydata.20.nonenglish, 
       spellcheck.trump.16, spellcheck.trump.20,
       mydata.temp)
    
# Text Cleaning ----
  # Dataframe for STM estimation ----
  stm.20 <- mydata.20 %>%
    select(year, cyn, eff, pid7, likes.trump.text, likes.trump, caseid)
  stm.16 <- mydata.16 %>%
    select(year, cyn, eff, pid7, likes.trump.text, likes.trump, caseid)
  stm <- rbind(stm.20, stm.16)
  stm$id <- c(1:length(stm$year))
  
  stm.ids <- stm %>%
    select(year, caseid, id)
  
  stm <- stm %>%
    mutate(remove = ifelse(likes.trump.text == "", 1, 0)) %>%
    filter(likes.trump == 1 & remove == 0) %>%
    select(id, year, cyn, eff, pid7, likes.trump.text) %>%
    drop_na()
  
  
  # Create shortdocs.trump
  shortdocs <- data.frame(shortdocs = stm$likes.trump.text, id = stm$id)
  #shortdocs <- stm$likes.trump.text # create shortdocs.trump for 'exemplary' texts
  
  # Remove certain symbols
  stm$likes.trump.text <- gsub("(", " ", stm$likes.trump.text, fixed = TRUE) # replace left paranthesis with space
  stm$likes.trump.text <- gsub(")", " ", stm$likes.trump.text, fixed = TRUE) # replace right paranthesis with space
  stm$likes.trump.text <- gsub("/", " ", stm$likes.trump.text, fixed = TRUE) # replace backslash with space
  stm$likes.trump.text <- gsub("\\", " ", stm$likes.trump.text, fixed = TRUE) # replace forwardslash with space
  stm$likes.trump.text <- gsub("-", " ", stm$likes.trump.text, fixed = TRUE) # replace hyphen with space\
  stm$likes.trump.text <- gsub("’", "", stm$likes.trump.text, fixed = TRUE) # replace apostrophe
  stm$likes.trump.text <- gsub("2nd", "second", stm$likes.trump.text, fixed = TRUE) # replace hyphen with space
  corpus.stmdocs <- VCorpus(VectorSource(stm$likes.trump.text)) # turn texts into corpus object
  corpus.stmdocs <- tm_map(corpus.stmdocs, content_transformer(tolower)) # lower case
  corpus.stmdocs <- tm_map(corpus.stmdocs, removeWords, c(stopwords("english"))) # interruptors and stop words
  corpus.stmdocs <- tm_map(corpus.stmdocs, removePunctuation) # punctuatoin
  corpus.stmdocs <- tm_map(corpus.stmdocs, removeNumbers) # numbers
  corpus.stmdocs <- tm_map(corpus.stmdocs, stripWhitespace) # white space
  corpus.stmdocs <- tm_map(corpus.stmdocs, stemDocument) # stemming
  corpus.stmdocs[[2]]$content # inspect pre processed code
  stm$likes.trump.text <- sapply(corpus.stmdocs, as.character)
  
  stm <- stm %>%
    mutate(remove = ifelse(likes.trump.text == "", 1, 0)) %>%
    filter(remove == 0) %>%
    select(id, year, cyn, eff, pid7, likes.trump.text) %>%
    drop_na()
  
  shortdocs <- shortdocs %>%
    filter(id %in% stm$id)

  shortdocs <- shortdocs$shortdocs
  
# STM ----
  
    ## STM Pre-Processing ----
    processed <- textProcessor(stm$likes.trump.text, metadata = stm) # process texts
    shortdocs <- shortdocs[-c(processed$docs.removed)] # remove empty docs from shortdocs
    
    plotRemoved(processed$documents, lower.thresh = seq(0, 20, by = 2)) # plot feature removal
    
    out <- prepDocuments(processed$documents, processed$vocab,
                         processed$meta, lower.thresh = 4)
    shortdocs <- shortdocs[-c(out$docs.removed)] # update 'shortdocs'
    docs <- out$documents
    vocab <- out$vocab
    metadata <-out$meta
    
  # Model Selection and Exploration ----
    
    # Search for number of topics ----
    selectingK <- searchK(docs, vocab, K = c(30,32,34,36,38,40),
                       prevalence =~ cyn + eff + year, data = metadata)
    
    setEPS()
    postscript(height = 6,width = 7, "Figures/STM-selectingK.eps")
    plot(selectingK)
    dev.off()

    # Generate topic model with X topics and different initializations ----
    # Additive
    
    selectingMod <- selectModel(docs, vocab, K = 36,
                                prevalence =~ cyn + eff + year,
                                max.em.its = 75, runs = 25, data = metadata,
                                seed = 212121)
    setEPS()
    postscript(height = 4,width = 6, "Figures/Final Figures/Supplementary/STM-selectModel.eps")
    par(cex = 1, lwd = 2, mar = c(4,4,2,2))
    plotModels(selectingMod, pch = c(1,2,3,4,5,6))
    dev.off()
    selectedMod <- selectingMod$runout[[5]] # select the best model
    
    # Topic Proportions -----
    # All topics
    setEPS()
    postscript("Figures/Final Figures/Supplementary/STM-ExpectedTopicProportion.eps", height = 12, width = 22)
    par(cex = 2, lwd = 2, mar = c(4,0.5,0.5,0.5))
    plot(selectedMod, type = "summary", labeltype = "frex", n = 4, main = "")
    dev.off()

    
    # Exemplary Documents -----
    topicprops.texts <- make.dt(selectedMod, metadata)
    topicprops.texts <- topicprops.texts %>%
      mutate(shortdocs = shortdocs) %>%
      select(docnum, year, shortdocs, Topic1)
    
    outsider <- c(topicprops.texts$shortdocs[3468], topicprops.texts$shortdocs[3951], topicprops.texts$shortdocs[3908])
    
    setEPS()
    postscript("Figures/Final Figures/Supplementary/STM-Exemplar-Responses.eps", height = 9, width = 8)
    par(cex = 2, lwd = 2, mar = c(1,1,1,1))
    plotQuote(outsider, text.cex = 1, width = 30)
    dev.off()
    
    
    
  # Estimate and Visualize ----
    # estimateEffect from STM package ----
    set.seed(218)
    esteff <- estimateEffect(c(1) ~  factor(year),
                             selectedMod, meta = metadata, uncertainty = "Global", nsims= 50)
    (esteff.tab <- summary(esteff, topics=c(1))) # see results
    esteff.tab <- as.data.frame(esteff.tab$tables[[1]][,1:4]) # save results in dataframe
    
    esteff.tab <- esteff.tab %>%  # rename variables and calculate one-sided p-values
      mutate(var = rownames(esteff.tab)) %>%
      remove_rownames() %>%
      rename(est = 1,
             se = 2,
             t = 3,
             p = 4,
             var = 5)
  
    esteff.tab <- createTexreg(coef = esteff.tab[,1], # create texreg object
                               coef.names = esteff.tab[,5],
                               se = round(esteff.tab[,2],3),
                               pvalues = esteff.tab[,4])
    
    plot.data.95 <- plot(esteff, "year", method = "pointestimate", cov.value1 = "2016", cov.value2 = "2020") # predict from model
    plot.data.90 <- plot(esteff, "year", method = "pointestimate", cov.value1 = "2016", cov.value2 = "2020", ci.level = 0.9) # predict from model
    frame <- data.frame(value = rev(c(plot.data.95$means[[1]])), # save predictions from model into data frame
                        lwr.95 = rev(c(plot.data.95$cis[[1]][1,])),
                        upr.95 = rev(c(plot.data.95$cis[[1]][2,])),
                        lwr.90 = rev(c(plot.data.90$cis[[1]][1,])),
                        upr.90 = rev(c(plot.data.90$cis[[1]][2,])),
                        year = c("2016","2020"))
    
    setEPS()
    postscript(height = 4, width = 5, "Figures/Final Figures/Allamong 22-0313.R1 Figure 3.eps")    
    ggplot(frame, aes(x = year, y = value)) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymin = lwr.95, ymax = upr.95), width = 0, size = 1) +
      #geom_errorbar(aes(ymin = lwr.90, ymax = upr.90), width = 0, size = 1) +
      labs(x = "Year", y = "Predicted % of Document\nUsing 'Political Outsider' Topic") +
      scale_y_continuous(limits = c(0.00, 0.08),
                         labels = c("0%","2%","4%","6%","8%")) +
      theme_set(theme_classic()) +
      theme(axis.title = element_text(size = 17), 
            axis.text = element_text(size = 15), 
            plot.title = element_text(size = 17), 
            legend.text = element_text(size = 10), 
            legend.title = element_text(size = 12),
            plot.margin = unit(c(0.5,2.25,0.25,0.2), "cm"),
            strip.text = element_text(size = 12),
            panel.border = element_rect(color = "black", size = 1, fill = NA),
            panel.background = element_rect(colour = "white", fill = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_rect(fill = "lightgrey"))
    dev.off()  
    
    
    
    # svyglm from survey package ----
    topics.props.df <- make.dt(selectedMod, metadata)
    topics.props.df <- topics.props.df %>%
      left_join(stm.ids, by = c("year", "id")) %>%
      left_join(design.vars, by = c("year", "caseid"))
    
    mod <- topics.props.df %>%
      as_survey_design(weights = weight,
                       ids = cluster,
                       strata = strata,
                       nest = T) %>%
      svyglm(design = .,
             formula = Topic1 ~ factor(year))
    svyglm.tab <- as.data.frame(summary(mod)$coefficients)
    svyglm.tab <- svyglm.tab %>%
      mutate(var = rownames(svyglm.tab)) %>%
      remove_rownames() %>%
      rename(est = 1,
             se = 2,
             p = 4,
             var = 5)
    
    svyglm.tab <- createTexreg(coef = svyglm.tab[,1], # create texreg object
                               coef.names = svyglm.tab[,5],
                               se = round(svyglm.tab[,2],3),
                               pvalues = svyglm.tab[,4])
    
    
    mod.web <- topics.props.df %>%
      as_survey_design(weights = weight,
                       ids = cluster,
                       strata = strata,
                       nest = T) %>%
      filter(mode != "web") %>%
      svyglm(design = .,
             formula = Topic1 ~ factor(year))
    summary(mod.web)
    svyglm.web.tab <- as.data.frame(summary(mod.web)$coefficients)
    svyglm.web.tab <- svyglm.web.tab %>%
      mutate(var = rownames(svyglm.web.tab)) %>%
      remove_rownames() %>%
      rename(est = 1,
             se = 2,
             t = 4,
             var = 5)
    
    svyglm.web.tab <- createTexreg(coef = svyglm.web.tab[,1], # create texreg object
                                   coef.names = svyglm.web.tab[,5],
                                   se = round(svyglm.web.tab[,2],3),
                                   pvalues = svyglm.web.tab[,4])
    
    texreg(list(esteff.tab, svyglm.tab, svyglm.web.tab),
           digits = 3,
           single.row = F,
           stars = numeric(0),
           custom.coef.names = c("Constant", "Year: 2020"),
           custom.model.names = c("estimateEffect", "svyglm", "svyglm (Web)"),
           custom.gof.rows = list("Observations" = c(dim(esteff$data)[1], length(mod$fitted.values), length(mod.web$fitted.values))),
           reorder.coef = c(2, 1))

    

    

    


    
