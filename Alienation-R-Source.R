# Maxwell B. Allamong
# Political Alienation and the Trump Vote in the 2016-2020 U.S. Presidential Elections
# File: Source 
# Updated: Jun. 11th, 2023

# README ----
# Packages ----
  # install.packages(c("stm","tm","SnowballC","wordcloud","quanteda","readtext",
  #                    "readxl","haven","latex2exp", "ggiraphExtra","effects","MASS","nnet",
  #                    "slam","Matrix","tidyverse","stargazer","corpus","splines",
  #                    "sjPlot","data.table","wordcloud","gtools","here","conflicted"))

# Libraries ----
  library(stm) # 
  library(tm)
  library(tidytext)
  library(plyr)
  library(dplyr)
  library(tidyverse)
  library(stargazer)
  library(readxl) # 
  library(gtools) # 
  library(MASS)
  library(nnet)
  library(hunspell)
  library(haven)
  library(here) # 
  library(conflicted) #
    conflict_prefer("filter","dplyr")
    conflict_prefer("select","dplyr")
    conflict_prefer("annotate","ggplot2")
    conflict_prefer("mutate","dplyr")
    conflict_prefer("summarize","dplyr")
    conflict_prefer("summarise","dplyr")
    conflict_prefer("here","here")
    conflict_prefer("rename","dplyr")

    
# Working Directory ----
  setwd(here())

# Load Data ----
    
  # Survey data  
  mydata.20 <- read.csv("Data/ANES/Survey Data/anes_timeseries_2020_csv_20220210.csv") # read in 2020 ANES
  mydata.16 <- read_dta("Data/ANES/Survey Data/anes_timeseries_2016.dta") # read in 2016 ANES
  mydata.cdf <- read_dta("Data/ANES/Survey Data/anes_timeseries_cdf_stata_20211118.dta") # read in CDF
  
  # Open ends
  trumplikes.20 <- read_excel("Data/ANES/Open Ends/anes_timeseries_2020_redactedopenends_excel_20211118.xlsx", "V201111", 
                              col_names = c("caseid","likes.trump.text"), 
                              col_types = c("text"),
                              skip = 1)
  trumplikes.16 <- read_excel("Data/ANES/Open Ends/anes_timeseries_2016_redacted_openends.xlsx", "V161075",
                              col_names = c("caseid","likes.trump.text"), 
                              col_types = c("text"),
                              skip = 1)
  
# Merge open-ends to survey data ----

  # Prepare ANES Open-Ended Responses
  trumplikes.20$caseid <- as.numeric(trumplikes.20$caseid)
  trumplikes.16$caseid <- as.numeric(trumplikes.16$caseid)
  
  # Prepare ANES Covariates
  mydata.20 <- mydata.20 %>%
    rename(caseid = V200001)
  mydata.16 <- mydata.16 %>% 
    rename(caseid = V160001_orig) %>%
    filter(caseid != 302252) # this ID doesn't have a match in ANES survey data, and doesn't give a text response
  
  # Merge Open-Ended Responses with ID's
  mydata.20 <- mydata.20 %>%
    left_join(trumplikes.20, by = "caseid") 
  mydata.16 <- mydata.16 %>%
    left_join(trumplikes.16, by = "caseid") 
  
  rm(trumplikes.20, trumplikes.16)

# Data cleaning ----
  ## 2020 ----
    
    # Select variables
    mydata.20 <- mydata.20 %>%
      select(V200010b, V200010c, V200010d, 
             likes.trump.text, caseid, V160001_orig,
             V201110,
             V201233, V201234, V201238, 
             V201511x, V201200,
             V201600, V201507x,
             V201152, V201151,
             V201231x, V202109x,
             V201006, V201617x,
             V201549x, V202110x,
             V202077, V201021,
             V201020,
             V201235, V201236,
             V202212, V202213, V202214,
             V201420x, V201423x, V201426x,
             V202266, V202267, V202268, V202269,
             V202480, V202479, V202168, V202166,
             V201327x, V201330x,
             V202361x, V202400, 
             V202291, V202292, 
             V201459)
    
    # Rename variables
    mydata.20 <- mydata.20 %>% 
      rename(weight = V200010b, cluster = V200010c, strata = V200010d, 
             caseid_2016 = V160001_orig,
             likes.trump = V201110,
             trust = V201233, big.interests = V201234, waste = V201235, corrupt = V201236, # cynicism
             dont.care = V202212, complex = V202214, no.say = V202213, # efficacy
             elect.attn = V201238, # responsiveness
             voted.general = V202109x, voted.primary = V201020, # voter turnout
             vote.choice.general = V202110x, vote.choice.primary = V201021, # vote choice
             therm.trump = V201152, therm.biden = V201151, # candidate thermometers
             pres.pref = V202077, # presidential preference
             female = V201600, age = V201507x, educ = V201511x, income = V201617x, race = V201549x, evangel = V201459, # demographics
             pid7 = V201231x, ideo7 = V201200, pol.int = V201006, # political variables
             birthright = V201420x, illg.child = V201423x, mex.wall = V201426x, # anti-immigrant
             child.trait.ind = V202266, child.trait.manners = V202267, child.trait.obedient = V202268, child.trait.behaved = V202269, # authoritarianism
             therm.black = V202480, therm.hispanic = V202479, therm.muslim = V202168, therm.lgbt = V202166, # thermometers
             econ.retro = V201327x, econ.prospect = V201330x, # economic prospects/retrospects
             oppose.trade = V202361x, china.threat = V202400,
             msi.favors = V202291, msi.problems = V202292) # modern sexism
             
    # Reshape variables
    mydata.20 <- mydata.20 %>% 
      mutate(trust = ifelse(trust > 0, trust, NA)) %>% # (1 = always, 5 = never)
      mutate(big.interests = ifelse(big.interests > 0, 2 - big.interests, NA)) %>% # (1 = big interests, 0 = for all people)
      mutate(waste = ifelse(waste > 0, 4 - waste, NA)) %>% # (recoded so that 1 = dont waste much and 3 = waste a lot)
      mutate(corrupt = ifelse(corrupt > 0, 6 - corrupt, NA)) %>% # (recoded so that 1 = none and 5 = all)
      mutate(dont.care = ifelse(dont.care > 0, 6 - dont.care, NA)) %>% # (recoded so that 1 = disagree strongly and 5 = agree strongly)
      mutate(complex = ifelse(complex > 0, 6 - complex, NA)) %>% # (recoded so that 1 = never and 5 = always)
      mutate(no.say = ifelse(no.say > 0, 6 - no.say, NA)) %>% # (recoded so that 1 = disagree strongly and 5 = agree strongly)
      mutate(elect.attn = ifelse(elect.attn > 0, elect.attn, NA)) %>%
      # DEMOGRAPHICS
      mutate(age = ifelse(age >= 18, age, NA)) %>%
      mutate(educ = ifelse(educ == 1, 1,
                    ifelse(educ == 2, 2,
                    ifelse(educ == 3, 3,
                    ifelse(educ == 4 | educ == 5, 4, NA))))) %>%
      mutate(income = ifelse(income == -9 | income == -5, NA, income)) %>% # income
      mutate(income.q = as.numeric(quantcut(income, 5))) %>% # split into quantiles (recoded as 1 = <5k to 22.5k, 2 = 22.5k to 45k, 3 = 45k to 75k, 4 = 75k to 110k, 5 = 110k to >250k)
      mutate(race = ifelse(race > 0, race, NA)) %>% # race
      mutate(white = ifelse(race == 1, 1, 0)) %>% # white indicator
      mutate(black = ifelse(race == 2, 1, 0)) %>% # black indicator
      mutate(hispanic = ifelse(race == 3, 1, 0)) %>% # hispanic indicator
      mutate(female = ifelse(female > 0, female - 1, NA)) %>% # female indicator
      mutate(evangel = ifelse(evangel == 2 | evangel == 3, 1, 0)) %>% # evangelical indicator
      # PARTISAN AND IDEOLOGICAL IDENTIFICATION
      mutate(pid7 = ifelse(pid7 > 0, pid7, NA)) %>%
      mutate(party.strength = ifelse(pid7 == 1 | pid7 == 7, 3,
                              ifelse(pid7 == 2 | pid7 == 6, 2,
                              ifelse(pid7 == 3 | pid7 == 5, 1,
                              ifelse(pid7 == 4, 0, NA))))) %>%
      mutate(rep = ifelse(pid7 > 4, 1, 0)) %>%
      mutate(ind = ifelse(pid7 == 4, 1, 0)) %>%
      mutate(dem = ifelse(pid7 < 4, 1, 0)) %>%
      mutate(ideo7 = ifelse(ideo7 >= 1 & ideo7 <=7, ideo7, NA)) %>%
      mutate(pol.int = ifelse(pol.int > 0, 4 - pol.int, NA)) %>%
      # VOTING BEHAVIOR
      mutate(likes.trump = ifelse(likes.trump == 1, 1,
                          ifelse(likes.trump == 2, 0, NA))) %>%
      mutate(voted.general = ifelse(voted.general >= 0, voted.general, NA)) %>%
      mutate(trump.biden = ifelse(vote.choice.general == 1, "Biden",
                           ifelse(vote.choice.general == 2, "Trump", NA))) %>%
      mutate(vote.choice.general = ifelse(vote.choice.general == 1, "Biden",
                                   ifelse(vote.choice.general == 2, "Trump",
                                   ifelse(vote.choice.general == 3 | vote.choice.general == 4 | vote.choice.general == 5, "Other",
                                   ifelse(voted.general == 0, "Did not vote", NA))))) %>%
      mutate(voter.biden = ifelse(vote.choice.general == "Biden", 1, 0)) %>%
      mutate(voter.trump = ifelse(vote.choice.general == "Trump", 1, 0)) %>%
      mutate(trump.biden = ifelse(vote.choice.general == "Biden", 0,
                           ifelse(vote.choice.general == "Trump", 1, NA))) %>%
      mutate(rep.presvote = ifelse(vote.choice.general == "Trump", 1, 
                            ifelse(vote.choice.general == "Biden", 0, NA))) %>%
      mutate(pref.biden = ifelse(pres.pref == 1, 1, 0)) %>%
      mutate(pref.trump = ifelse(pres.pref == 2, 1, 0)) %>%
      mutate(voter.sanders.prim = ifelse(vote.choice.primary == 5, 1, 0)) %>%
      mutate(voter.trump.prim = ifelse(vote.choice.primary == 8, 1, 0)) %>%
      mutate(voted.primary = ifelse(voted.primary > 0, 2 - voted.primary, NA)) %>%
      # ASSORTED COVARIATES
      mutate(child.trait.ind = ifelse(child.trait.ind == 2, 3, # all child traits coded so that authoritarian answer = 3, both = 2, non-authoritarian = 1
                               ifelse(child.trait.ind == 3, 2,
                               ifelse(child.trait.ind == 1, 1, NA)))) %>%
      mutate(child.trait.manners = ifelse(child.trait.manners == 2, 3,
                                   ifelse(child.trait.manners == 3, 2,
                                   ifelse(child.trait.manners == 1, 1, NA)))) %>%
      mutate(child.trait.obedient = ifelse(child.trait.obedient == 1, 3,
                                    ifelse(child.trait.obedient == 3, 2,
                                    ifelse(child.trait.obedient == 2, 1, NA)))) %>%
      mutate(child.trait.behaved = ifelse(child.trait.behaved == 2, 3,
                                   ifelse(child.trait.behaved == 3, 2,
                                   ifelse(child.trait.behaved == 1, 1, NA)))) %>%
      mutate(child.trait = (child.trait.ind + child.trait.manners + child.trait.obedient + child.trait.behaved)/4) %>%
      mutate(therm.black = ifelse(therm.black >= 0, therm.black, NA)) %>% # black therm
      mutate(therm.lgbt = ifelse(therm.lgbt >= 0 & therm.lgbt <=100, therm.lgbt, NA)) %>% # lgbt therm
      mutate(therm.muslim = ifelse(therm.muslim >= 0 & therm.muslim <=100, therm.muslim, NA)) %>% # muslim therm
      mutate(therm.hispanic = ifelse(therm.hispanic >= 0 & therm.hispanic <=100, therm.hispanic, NA)) %>% # hispanic therm
      mutate(therm.demgroups = (therm.black + therm.muslim + therm.hispanic + therm.lgbt) / 4) %>% # create summary of dem-group therms
      mutate(therm.trump = ifelse(therm.trump >= 0, therm.trump, NA)) %>% # trump therm
      mutate(therm.biden = ifelse(therm.biden >= 0, therm.biden, NA)) %>% # biden therm
      mutate(oppose.trade = ifelse(oppose.trade > 0, oppose.trade, NA)) %>%
      mutate(china.threat = ifelse(china.threat > 0, china.threat, NA)) %>%
      mutate(econ.retro = ifelse(econ.retro > 0, econ.retro, NA)) %>% # economic retrospects (coded so that higher values = better retrospects/prospects)
      mutate(econ.retro = 6 - econ.retro) %>%
      mutate(econ.prospect = ifelse(econ.prospect > 0, econ.prospect, NA)) %>% # economic prospects
      mutate(econ.prospect = 6 - econ.prospect) %>%
      mutate(birthright = ifelse(birthright > 0, birthright, NA)) %>% # anti-immigrant measures (coded so that higher values = more anti-immigrant)
      mutate(birthright = 8 - birthright) %>%
      mutate(illg.child = ifelse(illg.child > 0, illg.child, NA)) %>%
      mutate(illg.child = 7 - illg.child) %>%
      mutate(mex.wall = ifelse(mex.wall > 0, mex.wall, NA)) %>%
      mutate(mex.wall = 8 - mex.wall) %>%
      mutate(anti.immigrant = birthright + illg.child + mex.wall) %>%
      mutate(msi.favors = ifelse(msi.favors > 0, 6 - msi.favors, NA)) %>% # reverse code so that higher value = more sexism, remove NAs
      mutate(msi.problems = ifelse(msi.problems > 0, 6 - msi.problems, NA)) %>% # reverse code so that higher vaue = more sexism, remove NAS
      mutate(msi = msi.favors + msi.problems) %>%
      mutate(year = 2020)
    
    
    # Rescaling to range from 0 to 1
    mydata.20 <- mydata.20 %>% 
      mutate(therm.demgroups = (therm.demgroups - min(therm.demgroups, na.rm = T)) / 
               (max(therm.demgroups, na.rm = T) - min(therm.demgroups, na.rm = T))) %>%
      mutate(econ.retro = (econ.retro - min(econ.retro, na.rm = T)) / 
               (max(econ.retro, na.rm = T) - min(econ.retro, na.rm = T))) %>%
      mutate(econ.prospect = (econ.prospect - min(econ.prospect, na.rm = T)) / 
               (max(econ.prospect, na.rm = T) - min(econ.prospect, na.rm = T))) %>%
      mutate(age = (age - min(age, na.rm = T)) / 
               (max(age, na.rm = T) - min(age, na.rm = T))) %>%
      mutate(educ = (educ - min(educ, na.rm = T)) / 
               (max(educ, na.rm = T) - min(educ, na.rm = T))) %>%
      mutate(income.q = (income.q - min(income.q, na.rm = T)) / 
               (max(income.q, na.rm = T) - min(income.q, na.rm = T))) %>%
      mutate(ideo7 = (ideo7 - min(ideo7, na.rm = T)) / 
               (max(ideo7, na.rm = T) - min(ideo7, na.rm = T))) %>%
      mutate(party.strength = (party.strength - min(party.strength, na.rm = T)) / 
               (max(party.strength, na.rm = T) - min(party.strength, na.rm = T))) %>%
      mutate(pol.int = (pol.int - min(pol.int, na.rm = T)) / 
               (max(pol.int, na.rm = T) - min(pol.int, na.rm = T))) %>%
      mutate(china.threat = (china.threat - min(china.threat, na.rm = T)) / 
               (max(china.threat, na.rm = T) - min(china.threat, na.rm = T))) %>%
      mutate(oppose.trade = (oppose.trade - min(oppose.trade, na.rm = T)) / 
               (max(oppose.trade, na.rm = T) - min(oppose.trade, na.rm = T))) %>%
      mutate(anti.immigrant = (anti.immigrant - min(anti.immigrant, na.rm = T)) / 
               (max(anti.immigrant, na.rm = T) - min(anti.immigrant, na.rm = T))) %>%
      mutate(child.trait = (child.trait - min(child.trait, na.rm = T)) / 
               (max(child.trait, na.rm = T) - min(child.trait, na.rm = T))) %>%
      mutate(msi = (msi - min(msi, na.rm = T)) /
               (max(msi, na.rm = T) - min(msi, na.rm = T)))
      
  ## 2016 ----
    # Select variables
    mydata.16 <- mydata.16 %>% 
      select(V160102, V160201, V160202,V160501,
             likes.trump.text, caseid, V161215, V161216, V161218, V161220,
             V162031x, V162062x, V161021, V161021a, 
             V161158x, V161267, V161270, V161310x, V161361x, V161126,
             V161342, V161004, V161010e, V162038x, V162065x,
             V161074, V161265x, V161245, V161204x, V161198, #
             V161194x, V161195x, V161196x, V161192, V162239,
             V162240,V162239, V162240, V162241, V162242,
             V162268, V162273, V161266d,
             V162103, V162106, V162107, V161244,
             V162311, V162312, V162313, V162314,
             V162176x, V162159,
             V161140x, V161141x,
             V162231x, V162232, V162233,
             V161217, V162034,
             V162078, V162079,
             V162215, V162216, V162217)
    
    # Rename variables
    mydata.16 <- mydata.16 %>% 
      rename(weight = V160102, cluster = V160202, strata = V160201, mode = V160501,
             trust = V161215, big.interests = V161216, corrupt = V161218, waste = V161217, # cynicism
             dont.care = V162215, no.say = V162216, complex = V162217, # efficacy
             elect.attn = V161220, # responsiveness
             voted.general = V162065x, voted.primary = V161021, # vote turnout
             vote.choice.general = V162062x, vote.choice.primary = V161021a, # vote choice
             votepref16 = V162038x, # vote preference
             therm.clinton = V162078, therm.trump = V162079, # candidate thermometers
             likes.trump = V161074, # 'like' the candidates
             age = V161267, educ = V161270, race = V161310x, income = V161361x, female = V161342, state = V161010e, pol.int = V161004, # demographics
             relig = V161265x, evangel = V161266d, attend.relig = V161244, # religion
             pid = V161158x, ideo7 = V161126, # pid/ideology
             birthright = V161194x, illg.child = V161195x, mex.wall = V161196x, # anti-immigrant
             child.trait.ind = V162239, child.trait.manners = V162240, child.trait.obedient = V162241, child.trait.behaved = V162242, # authoritarianism
             imm.good = V162268, imm.english = V162273,
             therm.black = V162312, therm.christ = V162107, therm.hispanic = V162311, therm.illegal = V162313, # thermometers
             therm.lgbt = V162103, therm.muslim = V162106,  therm.white = V162314,
             oppose.trade = V162176x, china.threat = V162159,
             econ.retro = V161140x, econ.prospect = V161141x, # economic prospects/retrospects
             msi.media = V162231x, msi.favors = V162232, msi.problems = V162233, # modern sexism
             therm.clinton = V162078, therm.trump = V162079)
    
    # Reshape variables
    mydata.16 <- mydata.16 %>% 
      mutate(mode = ifelse(mode == 1, "FTF", "Web")) %>%
      # MAIN INDEPENDENT VARIABLES
      mutate(trust = ifelse(trust > 0, trust, NA)) %>% # how often trust gov in Wash to do what is right (kept original coding of 1 = Always, 2 = Most of time, 3 = about half time, 4 = some of time, 5 = Never)
      mutate(big.interests = ifelse(big.interests > 0, 2 - big.interests, NA)) %>% # big interests or for all? (recoded as 0 = for benefit of all, 1 = big interests)
      mutate(elect.attn = ifelse(elect.attn > 0, elect.attn, NA)) %>% # how much do elections make government pay attention? (recoded as 1 = a good deal, 2 = Some, 3 = Not Much)
      mutate(corrupt = ifelse(corrupt > 0, 6 - corrupt, NA)) %>% # how many people running gov are corrupt? (recoded as 1 = None, 2 = A few, 3 = About half, 4 = Most, 5 = All)
      mutate(waste = ifelse(waste > 0, 4 - waste, NA)) %>% # people in government wasting money (recoded as 1 = don't waste very much, 2 = waste some, 3 = waste a lot)
      mutate(dont.care = ifelse(dont.care > 0, 6 - dont.care, NA)) %>% # pub. officials don't care about what people think (recoded so that 1 = disagree strongly to 5 = agree strongly)
      mutate(no.say = ifelse(no.say > 0, 6 - no.say, NA)) %>% # people like me don't have any say (recoded so that 1 = disagree strongly to 5 = agree strongly)
      mutate(complex = ifelse(complex > 0, 6 - complex, NA)) %>% # politics/govt too complicated (recoded so that 1 = disagree strongly to 5 = agree strongly)
      # DEMOGRAPHICS
      mutate(age = ifelse(age > 17, age, NA)) %>% # age (in years)
      mutate(educ = ifelse(educ > 0 & educ < 9, 1, # less than high school diploma
                    ifelse(educ == 9, 2, # high school diploma or equivalent
                    ifelse(educ == 10, 3, # some college, but no degree
                    ifelse(educ == 11 | educ == 12, 3, # associates degree
                    ifelse(educ == 13, 4, # bachelors degree
                    ifelse(educ == 14, 4, # masters degree
                    ifelse(educ == 15 | educ == 16, 4, NA)))))))) %>% # professional or doctorate degree, all else NA
      mutate(income = ifelse(income == -9 | income == -5, NA, income)) %>% # income
      mutate(income.q = as.numeric(quantcut(income, 5))) %>% # split into quantiles (recoded as 1 = <5k to 22.5k, 2 = 22.5k to 45k, 3 = 45k to 75k, 4 = 75k to 110k, 5 = 110k to >250k)
      mutate(race = ifelse(race > 0, as.numeric(race), NA)) %>% # race
      mutate(white = ifelse(race == 1, 1, 0)) %>% # white indicator
      mutate(black = ifelse(race == 2, 1, 0)) %>% # black indicator
      mutate(hispanic = ifelse(race == 3, 1, 0)) %>% # hispanic indicator
      mutate(female = ifelse(female == 1 | female == 2, female, NA)) %>% # female indicator
      mutate(female = ifelse(female == 2, 1, 0)) %>% # female indicator
      mutate(evangel = ifelse(evangel >= 0, evangel, NA)) %>%
      mutate(sup.tues = ifelse(state == "AL" | state == "AR" | state == "GA" | state == "MN" | state == "TN" | # super tuesday state
                               state == "TX" | state == "VT" | state == "VA", 1, 0)) %>%
      # PARTISAN AND IDEOLOGICAL IDENTIFICATION
      mutate(pid = as.numeric(pid)) %>%
      mutate(pid3 = ifelse(pid == 1 | pid == 2 | pid == 3, "Dem", # strong/weak/leaning dems
                    ifelse(pid == 4, "Ind", # independent
                    ifelse(pid == 5 | pid == 6 | pid == 7, "Rep", NA)))) %>% # republicans
      mutate(pid3 = as.factor(pid3)) %>%
      mutate(rep = ifelse(pid3 == "Rep", 1, 0)) %>%
      mutate(dem = ifelse(pid3 == "Dem", 1, 0)) %>%
      mutate(ind = ifelse(pid3 == "Ind", 1, 0)) %>%
      mutate(pid7 = pid) %>%
      mutate(pid7 = ifelse(pid7 > 0, pid7, NA)) %>%
      mutate(party.strength = ifelse(pid7 == 1 | pid7 == 7, 3, # strong 
                              ifelse(pid7 == 2 | pid7 == 6, 2, # weak
                              ifelse(pid7 == 3 | pid7 == 5, 1, # leaning
                              ifelse(pid7 == 4, 0, NA))))) %>%
      mutate(ind = ifelse(pid3 == "Ind", 1, 0)) %>% # indicator for independent
      mutate(ideo7 = ifelse(ideo7 < 1 | ideo7 > 7, NA, ideo7)) %>% # ideology (1 = ext. lib, 7 = ext. conserv)
      mutate(pol.int = ifelse(pol.int == 1, 3, # very much interested
                       ifelse(pol.int == 2, 2, # somehwhat interested
                       ifelse(pol.int == 3, 1, NA)))) %>% # not much interested
      # VOTING BEHAVIOR
      mutate(likes.trump = ifelse(likes.trump == 1, 1,
                          ifelse(likes.trump == 2, 0, NA))) %>%
      mutate(voted.general = ifelse(voted.general == 2, 1,
                             ifelse(voted.general == 0 | voted.general == 1, 0, NA))) %>%
      mutate(voted.primary = ifelse(voted.primary == 1, 1,
                             ifelse(voted.primary == 2, 0, NA))) %>%
      mutate(vote.choice.general = ifelse(vote.choice.general == 1, "Clinton",
                                   ifelse(vote.choice.general == 2, "Trump",
                                   ifelse(vote.choice.general == 3 | vote.choice.general == 4 | vote.choice.general == 5, "Other",
                                   ifelse(voted.general == 0, "Did not vote", NA))))) %>%
      mutate(vote.choice.general = as.factor(vote.choice.general)) %>%
      mutate(vote.choice.primary = ifelse(vote.choice.primary == 2, "Sanders",
                                   ifelse(vote.choice.primary == 4, "Trump",
                                   ifelse(vote.choice.primary == 1 | vote.choice.primary == 3 |
                                          vote.choice.primary == 5 | vote.choice.primary == 6 |
                                          vote.choice.primary == 7 | vote.choice.primary == 8 |
                                          vote.choice.primary == 9, "Other",
                                   ifelse(voted.primary == 0, "Did not vote", NA))))) %>%
      mutate(vote.choice.primary = as.factor(vote.choice.primary)) %>%
      mutate(voter.trump.general = ifelse(vote.choice.general == "Trump", 1, 0)) %>%
      mutate(voter.clinton.general = ifelse(vote.choice.general == "Clinton", 1, 0)) %>%
      mutate(voter.trump.primary = ifelse(vote.choice.primary == "Trump", 1, 0)) %>%
      mutate(voter.clinton.primary = ifelse(vote.choice.primary == "Clinton", 1, 0)) %>%
      mutate(voter.sanders.primary = ifelse(vote.choice.primary == "Sanders", 1, 0)) %>%
      mutate(votepref16 = ifelse(votepref16 == 10 | votepref16 == 11, "Clinton",
                          ifelse(votepref16 == 20 | votepref16 == 21, "Trump",
                          ifelse(votepref16 == 30 | votepref16 == 31 | votepref16 == 40 | 
                                 votepref16 == 41 | votepref16 == 50 | votepref16 == 51, "Other/Third-Party", NA)))) %>%
      mutate(votepref16 = as.factor(votepref16)) %>%
      mutate(trumppref = ifelse(votepref16 == "Trump", 1, 0)) %>%
      
      # ASSORTED COVARIATES
      mutate(child.trait.ind = ifelse(child.trait.ind == 2, 3, # all child traits coded so that authoritarian answer = 3, both = 2, non-authoritarian = 1
                               ifelse(child.trait.ind == 3, 2,
                               ifelse(child.trait.ind == 1, 1, NA)))) %>%
      mutate(child.trait.manners = ifelse(child.trait.manners == 2, 3,
                                   ifelse(child.trait.manners == 3, 2,
                                   ifelse(child.trait.manners == 1, 1, NA)))) %>%
      mutate(child.trait.obedient = ifelse(child.trait.obedient == 1, 3,
                                    ifelse(child.trait.obedient == 3, 2,
                                    ifelse(child.trait.obedient == 2, 1, NA)))) %>%
      mutate(child.trait.behaved = ifelse(child.trait.behaved == 2, 3,
                                   ifelse(child.trait.behaved == 3, 2,
                                   ifelse(child.trait.behaved == 1, 1, NA)))) %>%
      mutate(child.trait = (child.trait.ind + child.trait.manners + child.trait.obedient + child.trait.behaved)/4) %>%
      mutate(therm.black = ifelse(therm.black >= 0, therm.black, NA)) %>%
      mutate(therm.lgbt = ifelse(therm.lgbt >= 0 & therm.lgbt <=100, therm.lgbt, NA)) %>%
      mutate(therm.muslim = ifelse(therm.muslim >= 0 & therm.muslim <=100, therm.muslim, NA)) %>%
      mutate(therm.christ = ifelse(therm.christ >= 0 & therm.christ <=100, therm.christ, NA)) %>%
      mutate(therm.hispanic = ifelse(therm.hispanic >= 0 & therm.hispanic <=100, therm.hispanic, NA)) %>%
      mutate(therm.illegal = ifelse(therm.illegal >= 0 & therm.illegal <=100, therm.illegal, NA)) %>%
      mutate(therm.white = ifelse(therm.white >= 0 & therm.white <=100, therm.white, NA)) %>%
      mutate(therm.demgroups = (therm.black + therm.muslim + therm.hispanic + therm.lgbt) / 4) %>%
      mutate(therm.trump = ifelse(therm.trump >= 0 & therm.trump <=100, therm.trump, NA)) %>%
      mutate(therm.clinton = ifelse(therm.clinton >= 0 & therm.clinton <=100, therm.clinton, NA)) %>%
      mutate(therm.repgroups = (therm.christ + therm.white) / 2) %>%
      mutate(imm.good = ifelse(imm.good > 0, imm.good, NA)) %>%
      mutate(imm.good = 6 - imm.good) %>% # reverse code so that higher value = immigrants good for econ
      mutate(imm.english = ifelse(imm.english > 0, imm.english, NA)) %>%
      mutate(imm.english = 5 - imm.english) %>% # reverse code so that higher value = english very important
      mutate(oppose.trade = ifelse(oppose.trade > 0, oppose.trade, NA)) %>%
      mutate(china.threat = ifelse(china.threat > 0, china.threat, NA)) %>%
      mutate(china.threat = 4 - china.threat) %>% # reverse code so that higher value = china more threatening
      mutate(econ.retro = ifelse(econ.retro > 0, econ.retro, NA)) %>%
      mutate(econ.retro = 6 - econ.retro) %>%
      mutate(econ.prospect = ifelse(econ.prospect > 0, econ.prospect, NA)) %>%
      mutate(econ.prospect = 6 - econ.prospect) %>%
      mutate(birthright = ifelse(birthright > 0, birthright, NA)) %>%
      mutate(birthright = 8 - birthright) %>%
      mutate(illg.child = ifelse(illg.child > 0, illg.child, NA)) %>%
      mutate(illg.child = 7 - illg.child) %>%
      mutate(mex.wall = ifelse(mex.wall > 0, mex.wall, NA)) %>%
      mutate(mex.wall = 8 - mex.wall) %>%
      mutate(anti.immigrant = birthright + illg.child + mex.wall) %>%
      mutate(msi.media = ifelse(msi.media > 0, msi.media, NA)) %>% # reverse code so that higher value = more sexism, remove NAs
      mutate(msi.favors = ifelse(msi.favors > 0, 6 - msi.favors, NA)) %>% # reverse code so that higher value = more sexism, remove NAs
      mutate(msi.problems = ifelse(msi.problems > 0, 6 - msi.problems, NA)) %>% # reverse code so that higher vaule = more sexism, remove NAS
      mutate(msi = msi.media + msi.favors + msi.problems) %>%
      mutate(year = 2016)
    
    # Rescaling to range from 0 to 1
    mydata.16 <- mydata.16 %>% 
      mutate(therm.demgroups = (therm.demgroups - min(therm.demgroups, na.rm = T)) / 
                               (max(therm.demgroups, na.rm = T) - min(therm.demgroups, na.rm = T))) %>%
      mutate(econ.retro = (econ.retro - min(econ.retro, na.rm = T)) / 
                          (max(econ.retro, na.rm = T) - min(econ.retro, na.rm = T))) %>%
      mutate(econ.prospect = (econ.prospect - min(econ.prospect, na.rm = T)) / 
                             (max(econ.prospect, na.rm = T) - min(econ.prospect, na.rm = T))) %>%
      mutate(age = (age - min(age, na.rm = T)) / 
                   (max(age, na.rm = T) - min(age, na.rm = T))) %>%
      mutate(educ = (educ - min(educ, na.rm = T)) / 
                    (max(educ, na.rm = T) - min(educ, na.rm = T))) %>%
      mutate(income.q = (income.q - min(income.q, na.rm = T)) / 
                        (max(income.q, na.rm = T) - min(income.q, na.rm = T))) %>%
      mutate(ideo7 = (ideo7 - min(ideo7, na.rm = T)) / 
                     (max(ideo7, na.rm = T) - min(ideo7, na.rm = T))) %>%
      mutate(party.strength = (party.strength - min(party.strength, na.rm = T)) / 
                       (max(party.strength, na.rm = T) - min(party.strength, na.rm = T))) %>%
      mutate(pol.int = (pol.int - min(pol.int, na.rm = T)) / 
                       (max(pol.int, na.rm = T) - min(pol.int, na.rm = T))) %>%
      mutate(imm.good = (imm.good - min(imm.good, na.rm = T)) / 
                        (max(imm.good, na.rm = T) - min(imm.good, na.rm = T))) %>%
      mutate(imm.english = (imm.english - min(imm.english, na.rm = T)) / 
                        (max(imm.english, na.rm = T) - min(imm.english, na.rm = T))) %>%
      mutate(china.threat = (china.threat - min(china.threat, na.rm = T)) / 
                            (max(china.threat, na.rm = T) - min(china.threat, na.rm = T))) %>%
      mutate(oppose.trade = (oppose.trade - min(oppose.trade, na.rm = T)) / 
                            (max(oppose.trade, na.rm = T) - min(oppose.trade, na.rm = T))) %>%
      mutate(anti.immigrant = (anti.immigrant - min(anti.immigrant, na.rm = T)) / 
                              (max(anti.immigrant, na.rm = T) - min(anti.immigrant, na.rm = T))) %>%
      mutate(child.trait = (child.trait - min(child.trait, na.rm = T)) / 
                           (max(child.trait, na.rm = T) - min(child.trait, na.rm = T))) %>%
      mutate(msi = (msi - min(msi, na.rm = T)) /
                   (max(msi, na.rm = T) - min(msi, na.rm = T)))
    
  ## 1988 - 2008 ----
    # Select variables
    mydata.cdf <- mydata.cdf %>%
      select(VCF0009z,
             VCF0004, VCF0605, VCF0604, VCF0624, VCF0703, 
             VCF0104, VCF0110, VCF0101, VCF0301, VCF0105a, 
             VCF0803, VCF0114, VCF0310, VCF9265, VCF9265,
             VCF0704a, VCF0606, VCF0609, VCF0614, VCF0613, 
             VCF0608, VCF0006)
    
    # Rename variables
    mydata.cdf <- mydata.cdf %>%
      rename(weight = VCF0009z,
             caseid = VCF0006, trust = VCF0604, big.interests = VCF0605, waste = VCF0606, corrupt = VCF0608,
             dont.care = VCF0609, complex = VCF0614, no.say = VCF0613,
             elect.attn = VCF0624, 
             voted.general = VCF0703, voted.primary = VCF9265, # voter turnout
             vote.party.general = VCF0704a, # 2 party vote
             partyid = VCF0301, female = VCF0104, educ = VCF0110, year = VCF0004, age = VCF0101, 
             race = VCF0105a, ideology = VCF0803, income.q = VCF0114, 
             pol.int = VCF0310)
    
    # Reshape variables
    mydata.cdf <- mydata.cdf %>% 
      filter(year == 1988 | year == 1992 | year == 1996 | year == 2000 | year == 2004 | year == 2008 | year == 2012) %>% # filter to 1988-2012 presidential election years
      mutate(trust = ifelse(trust > 0 & trust < 5, 5 - trust, NA)) %>% # how often trust gov in Wash to do what is right (recoded so that 1 = just about always to 4 = never)
      mutate(big.interests = ifelse(big.interests > 0 & big.interests < 3, 2 - big.interests, NA)) %>% # big interests or for all? (recode so that 1 = few big interests, 0 = for all people)
      mutate(waste = ifelse(waste > 0 & waste < 4, 4 - waste, NA)) %>% # how much do people in government waste tax money (recoded as 1 = not very much, 2 = some, 3 = alot)
      mutate(corrupt = ifelse(corrupt > 0 & corrupt < 4, 4 - corrupt, NA)) %>% #how many crooked/corrupt? (recoded so that 1 = hardly any, 2 = not many, 3 = hardly any)
      mutate(dont.care = ifelse(dont.care == 1, 3, # recoded so that 3 = agree
                         ifelse(dont.care == 3, 2, # recoded so that 2 = neither agree nor disagree
                         ifelse(dont.care == 2, 1, NA)))) %>% # recoded so that 1 = disagree
      mutate(complex = ifelse(complex == 1, 3, 
                       ifelse(complex == 3, 2,
                       ifelse(complex == 2, 1, NA)))) %>%
      mutate(no.say = ifelse(no.say == 1, 3,
                      ifelse(no.say == 3, 2,
                      ifelse(no.say == 2, 1, NA)))) %>%
      mutate(elect.attn = ifelse(elect.attn > 0 & elect.attn < 4, 4 - elect.attn, NA)) %>% # how much do elections make government pay attention? (recoded as 1 = a good deal, 2 = Some, 3 = Not Much)
      mutate(dem.presvote = ifelse(vote.party.general == 1, 1,
                            ifelse(vote.party.general == 2, 0, NA))) %>%
      mutate(rep.presvote = ifelse(vote.party.general == 2, 1,
                            ifelse(vote.party.general == 1, 0, NA))) %>%
      mutate(voted.general = ifelse(voted.general < 4 & voted.general > 0, voted.general, NA)) %>% # summary of registration and vote (1 = not registered/did not vote, 2 = registered/did not vote, 3 = voted.general (registered))
      mutate(voted.general = ifelse(voted.general == 3, 1, 0)) %>% # indicator for 'voted'
      mutate(voted.primary = ifelse(voted.primary == 1, 1, # vote in primary (recoded as 1 = Voted, 0 = Didn't vote)
                             ifelse(voted.primary == 2, 0, NA))) %>%
      mutate(pid3 = ifelse(partyid == 1 | partyid == 2 | partyid == 3, "Dem", # democrat
                    ifelse(partyid == 4, "Ind", # independent
                    ifelse(partyid == 5 | partyid == 6 | partyid == 7, "Rep", NA)))) %>% # republican
      mutate(pid7 = ifelse(partyid > 0, partyid, NA)) %>%
      mutate(ind = ifelse(pid3 == "Ind", 1, 0)) %>%
      mutate(party.strength = ifelse(pid7 == 1 | pid7 == 7, 3, # strong
                              ifelse(pid7 == 2 | pid7 == 6, 2, # weak 
                              ifelse(pid7 == 3 | pid7 == 5, 1, # leaning
                              ifelse(pid7 == 4, 0, NA))))) %>%
      mutate(pol.int = ifelse(pol.int <= 3 & pol.int >=1, pol.int, NA)) %>% # 1 = not much interest, 2 = somewhat interested, 3 = very interested
      mutate(female = female - 1) %>% # indicator for female (recoded as 1 = female, 0 otherwise)
      mutate(educ = ifelse(educ > 0, educ, NA)) %>% # remove NA
      mutate(age = ifelse(age > 16, age, NA)) %>% # remove NA
      mutate(white = ifelse(race == 1, 1, 0)) %>% # indicator for white
      mutate(black = ifelse(race == 2, 1, 0)) %>% # indicator for black
      mutate(ideo7 = ifelse(ideology > 0 & ideology < 9, ideology, NA)) %>% # remove NA
      mutate(income.q = ifelse(income.q > 0, income.q, NA)) # remove NA
  
      # Rescaling to range from 0 to 1
      mydata.cdf <- mydata.cdf %>% 
        mutate(age = (age - min(age, na.rm = T)) / 
                     (max(age, na.rm = T) - min(age, na.rm = T))) %>%
        mutate(pol.int = (pol.int - min(pol.int, na.rm = T)) / 
                         (max(pol.int, na.rm = T) - min(pol.int, na.rm = T))) %>%
        mutate(party.strength = (party.strength - min(party.strength, na.rm = T)) / 
                                 (max(party.strength, na.rm = T) - min(party.strength, na.rm = T))) %>%
        mutate(income.q = (income.q - min(income.q, na.rm = T)) / 
                          (max(income.q, na.rm = T) - min(income.q, na.rm = T))) %>%
        mutate(educ = (educ - min(educ, na.rm = T)) / 
                      (max(educ, na.rm = T) - min(educ, na.rm = T))) %>%
        mutate(ideo7 = (ideo7 - min(ideo7, na.rm = T)) / 
                       (max(ideo7, na.rm = T) - min(ideo7, na.rm = T)))
      
      mydata.cdf <- mydata.cdf %>%
        mutate(id = rownames(mydata.cdf))
      
# Alienation Components ----
  ## 2020 ----
  corfa.20 <- mydata.20 %>%
    select(trust, big.interests, waste, corrupt, dont.care, complex, no.say) %>%
    #select(trust, big.interests, waste, corrupt) %>%
    #select(dont.care, complex, no.say) %>%
    drop_na()
  
  cor(corfa.20)
  scree(corfa.20)
  factanal(corfa.20, factors = 2)
  
  df.2020 <- mydata.20 %>%
    #select(trust, big.interests, elect.attn) %>%
    #select(id, dont.care, complex, no.say) %>%
    #select(id, trust, big.interests, waste, corrupt, elect.attn) %>%
    #select(trust, big.interests, waste, corrupt, elect.attn) %>%
    #select(caseid, trust, big.interests, waste, corrupt, elect.attn, dont.care, no.say) %>%
    select(caseid, trust, big.interests, waste, corrupt, elect.attn, dont.care, complex, no.say) %>%
    drop_na()
  
  df.2020$cyn <- prcomp(df.2020[,c('trust','big.interests','waste','corrupt')],center=TRUE,scale=TRUE)$x[,1]
  df.2020$eff <- prcomp(df.2020[,c('dont.care','no.say')],center=TRUE,scale=TRUE)$x[,1]
  
  df.2020$cyn <- (df.2020$cyn - min(df.2020$cyn)) / (max(df.2020$cyn) - min(df.2020$cyn))
  df.2020$cyn <- 1 - df.2020$cyn
  df.2020$eff <- (df.2020$eff - min(df.2020$eff)) / (max(df.2020$eff) - min(df.2020$eff))
  df.2020$eff <- 1 - df.2020$eff
  
  df.2020 <- df.2020 %>%
    select(caseid, cyn, eff)
  
  mydata.20 <- mydata.20 %>%
    left_join(df.2020, by = c("caseid"))
  
  
  ## 2016 ----
  corfa.16 <- mydata.16 %>%
    select(trust, big.interests, waste, corrupt, dont.care, complex, no.say) %>%
    #select(trust, big.interests, waste, corrupt) %>%
    #select(dont.care, complex, no.say) %>%
    drop_na()
  
  cor(corfa.16)
  scree(corfa.16)
  factanal(corfa.16, factors = 2)
  
  df.2016 <- mydata.16 %>%
    #select(trust, big.interests, elect.attn) %>%
    #select(id, dont.care, complex, no.say) %>%
    #select(dont.care, complex, no.say) %>%
    #select(id, trust, big.interests, waste, corrupt, elect.attn) %>%
    #select(trust, big.interests, waste, corrupt, elect.attn) %>%
    #select(id, trust, big.interests, waste, corrupt, elect.attn, dont.care, complex, no.say) %>%
    select(caseid, trust, big.interests, waste, corrupt, elect.attn, dont.care, complex, no.say) %>%
    drop_na()
  
  df.2016$cyn <- prcomp(df.2016[,c('trust','big.interests','waste','corrupt')],center=TRUE,scale=TRUE)$x[,1]
  df.2016$eff <- prcomp(df.2016[,c('dont.care','no.say')],center=TRUE,scale=TRUE)$x[,1]
  
  df.2016$cyn <- (df.2016$cyn - min(df.2016$cyn)) / (max(df.2016$cyn) - min(df.2016$cyn))
  df.2016$cyn <- 1 - df.2016$cyn
  df.2016$eff <- (df.2016$eff - min(df.2016$eff)) / (max(df.2016$eff) - min(df.2016$eff))
  
  df.2016 <- df.2016 %>%
    select(caseid, cyn, eff)
  
  mydata.16 <- mydata.16 %>%
    left_join(df.2016, by = c("caseid"))
  
  
  ## 2016 and 2020 ----
  
  corfa.1620 <- rbind(corfa.16, corfa.20)
  factanal(corfa.1620, factors = 2)
  fa.1620 <- factanal(corfa.1620, factors = 2)
  
  ## CDF ----
  corfa.cdf <- mydata.cdf %>%
    select(trust, big.interests, waste, corrupt, dont.care, complex, no.say) %>%
    #select(trust, big.interests, waste, corrupt) %>%
    #select(dont.care, complex, no.say) %>%
    drop_na()
  
  cor(corfa.cdf)
  scree(corfa.cdf)
  factanal(corfa.cdf, factors = 2)
  
  df.cdf <- mydata.cdf %>%
    #select(trust, big.interests, elect.attn) %>%
    #select(trust, big.interests, waste, corrupt) %>%
    #select(dont.care, complex, no.say) %>%
    #select(id, trust, big.interests, waste, corrupt, elect.attn) %>%
    #select(id, trust, big.interests, waste, corrupt, elect.attn, dont.care, no.say) %>%
    select(id, trust, big.interests, waste, corrupt, elect.attn, dont.care, no.say, year) %>%
    drop_na()
  df.cdf$cyn <- prcomp(df.cdf[,c('trust','big.interests','waste','corrupt')],center=TRUE,scale=TRUE)$x[,1]
  df.cdf$eff <- prcomp(df.cdf[,c('dont.care','no.say')],center=TRUE,scale=TRUE)$x[,1]
  df.cdf$cyn <- (df.cdf$cyn - min(df.cdf$cyn)) / (max(df.cdf$cyn) - min(df.cdf$cyn))
  df.cdf$cyn <- 1 - df.cdf$cyn
  df.cdf$eff <- (df.cdf$eff - min(df.cdf$eff)) / (max(df.cdf$eff) - min(df.cdf$eff))
  df.cdf$eff <- 1 - df.cdf$eff
  
  df.cdf <- df.cdf %>%
    select(id, cyn, eff)
  mydata.cdf <- mydata.cdf %>%
    left_join(df.cdf, by = c("id"))
  
# Write CSVs ----
  
  write.csv(mydata.20, "Data/mydata-20-3.csv")
  write.csv(mydata.16, "Data/mydata-16-3.csv")
  write.csv(mydata.cdf, "Data/mydata-cdf-2.csv")
  