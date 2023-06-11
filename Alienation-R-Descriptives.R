# Maxwell B. Allamong
# Political Alienation and the Trump Vote in the 2016-2020 U.S. Presidential Elections
# File: Descriptives 
# Updated: Jun. 11th, 2023

# README ----
# Packages ----
#install.packages(c("polycor","psych"))

# Libraries ----
  library(haven)
  library(tidyverse)
  library(here)
  library(survey)
  library(srvyr)


  library(polycor)
  library(psych)
  library(extrafont)

# Working Directory ----
  #setwd(here())

# Source Data ----
  source("Alienation-R-Source.R")
  mip <- read_dta("Data/MIP/usmisc2015-mipd_ann.dta")

# Descriptives ----
  # MIP ----
  mip %>%
    rename(year = tsy) %>%
    filter(year >= 1980) %>%
    select(year, cap1_perc, cap20_perc, marpor3_perc,
           singer31_perc, singer32_perc, singer33_perc, singer35_perc) %>%
    ggplot() +
    geom_line(aes(x = year, y = marpor3_perc), linewidth = 1) +
    geom_point(aes(x = year, y = marpor3_perc), size = 2) +
    labs(x = "Year", y = "Percent Mentioning\n'Political System' as MIP") +
    scale_x_continuous(breaks = c(seq(1980,2015,5))) +
    scale_y_continuous(limits = c(0,20),
                       breaks = seq(0,20,5),
                       labels = c("0%","5%","10%","15%","20%")) +
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
  ggsave(height = 4, width = 6.5, "Figures/Final Figures/Allamong 22-0313.R1 Figure 1.eps")
  
  # Alienation Measures ----
    # Means ----
    # Inefficacy
    mydata.16 %>%
      drop_na(weight) %>%
      as_survey_design(weights = weight,
                       ids = cluster,
                       strata = strata, 
                       nest = T) %>%
      summarize(mean = survey_mean(eff, na.rm = T)) %>%
      mutate(lwr = mean - 1.98*mean_se,
             upr = mean + 1.98*mean_se)
    
    mydata.20 %>%
      drop_na(weight) %>%
      as_survey_design(weights = weight,
                       ids = cluster,
                       strata = strata, 
                       nest = T) %>%
      summarize(mean = survey_mean(eff, na.rm = T)) %>%
      mutate(lwr = mean - 1.98*mean_se,
             upr = mean + 1.98*mean_se)
    
    
    # Cynicism
    mydata.16 %>%
      drop_na(weight) %>%
      as_survey_design(weights = weight,
                       ids = cluster,
                       strata = strata, 
                       nest = T) %>%
      summarize(mean = survey_mean(cyn, na.rm = T)) %>%
      mutate(lwr = mean - 1.98*mean_se,
             upr = mean + 1.98*mean_se)
    
    mydata.20 %>%
      drop_na(weight) %>%
      as_survey_design(weights = weight,
                       ids = cluster,
                       strata = strata, 
                       nest = T) %>%
      summarize(mean = survey_mean(cyn, na.rm = T)) %>%
      mutate(lwr = mean - 1.98*mean_se,
             upr = mean + 1.98*mean_se)
  
  
    # Histograms ----
    mydata.16 %>%
      select(cyn) %>%
      drop_na() %>%
      ggplot() +
        geom_histogram(aes(x = cyn), binwidth = 0.1, color = "black", fill = "grey") +
        #geom_density(aes(x = cyn), alpha = .6) +
        labs(x = "Cynicism", y = "Count", title = "2016") +
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
    ggsave(height = 4, width = 5, "Figures/Final Figures/Supplementary/Descriptives-Histogram-Cynicism-2016.eps")
    

    mydata.16 %>%
      select(eff) %>%
      drop_na() %>%
      ggplot() +
        geom_histogram(aes(x = eff), binwidth = 0.1, color = "black", fill = "grey") +
        geom_density(aes(x = eff), alpha = .6) +
        labs(x = "Inefficacy", y = "Count", title = "2016") +
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
    ggsave(height = 4, width = 5, "Figures/Final Figures/Supplementary/Descriptives-Histogram-Inefficacy-2016.eps")
    
    
    mydata.20 %>%
      select(cyn) %>%
      drop_na() %>%
      ggplot() +
      geom_histogram(aes(x = cyn), binwidth = 0.1, color = "black", fill = "grey") +
      #geom_density(aes(x = cyn), alpha = .6) +
      labs(x = "Cynicism", y = "Count", title = "2020") +
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
    ggsave(height = 4, width = 5, "Figures/Final Figures/Supplementary/Descriptives-Histogram-Cynicism-2020.eps")
    
    
    mydata.20 %>%
      select(eff) %>%
      drop_na() %>%
      ggplot() +
      geom_histogram(aes(x = eff), binwidth = 0.1, color = "black", fill = "grey") +
      geom_density(aes(x = eff), alpha = .6) +
      labs(x = "Inefficacy", y = "Count", title = "2020") +
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
    ggsave(height = 4, width = 5, "Figures/Final Figures/Supplementary/Descriptives-Histogram-Inefficacy-2020.eps")
    
    
    mydata.cdf %>%
      select(cyn) %>%
      drop_na() %>%
      ggplot() +
        geom_histogram(aes(x = cyn), binwidth = 0.1, color = "black", fill = "grey") +
        geom_density(aes(x = cyn), alpha = .6) +
        labs(x = "Cynicism", y = "Count", title = "1988-2012") +
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
    ggsave(height = 4, width = 5, "Figures/Final Figures/Supplementary/Descriptives-Histogram-Cynicism-CDF.eps")
    
    
    mydata.cdf %>%
      select(eff) %>%
      drop_na() %>%
      ggplot() +
      geom_histogram(aes(x = eff), binwidth = 0.1, color = "black", fill = "grey") +
      geom_density(aes(x = eff), alpha = .6) +
      labs(x = "efficism", y = "Count", title = "1988-2012") +
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
    ggsave(height = 4, width = 5, "Figures/Final Figures/Supplementary/Descriptives-Histogram-Inefficacy-CDF.eps")
    
    
    
    
  # Covariates ----
  # 2020
  desc.20 <- mydata.20 %>%
    select(cyn, eff, rep, ind, dem, pol.int, ideo7, 
             therm.demgroups, child.trait, msi, oppose.trade, 
             econ.retro, income.q, educ, evangel, black, white,
             female, age)
  desc.20 <- as.data.frame(desc.20)

  stargazer(desc.20, 
            covariate.labels = c("Cynicism", "Inefficacy","Republican",
                                 "Independent", "Democrat", "Political Interest",
                                 "Ideology","Democratic-aligned Group Therms.",
                                 "Child-Rearing Authoritarianism", "Modern Sexism",
                                 "Oppose Trade", "Retrospective Econ. Assessments",
                                 "Income (Quartile)", "Education", "Evangelical",
                                 "Black", "White", "Female", "Age"))
  
  # 2016
  desc.16 <- mydata.16 %>%
    select(cyn, eff, rep, ind, dem, pol.int, ideo7, 
           therm.demgroups, child.trait, msi, oppose.trade, 
           econ.retro, income.q, educ, evangel, black, white,
           female, age)
  desc.16 <- as.data.frame(desc.16)
  
  stargazer(desc.16, 
            covariate.labels = c("Cynicism", "Inefficacy","Republican",
                                 "Independent", "Democrat", "Political Interest",
                                 "Ideology","Democratic-aligned Group Therms.",
                                 "Child-Rearing Authoritarianism", "Modern Sexism",
                                 "Oppose Trade", "Retrospective Econ. Assessments",
                                 "Income (Quartile)", "Education", "Evangelical",
                                 "Black", "White", "Female", "Age"))
  
  # CDF
  desc.cdf <- mydata.cdf %>%
    select(cyn, eff, party.strength, educ, age,
           pol.int, female, white, black, income.q) 
  desc.cdf <- as.data.frame(desc.cdf)
  
  stargazer(desc.cdf, 
            covariate.labels = c("Cynicism", "Inefficacy", "Partisan Strength",
                                 "Education", "Age", "Political Interest", "Female", "White",
                                 "Black", "Income (Quartile)"))
                             