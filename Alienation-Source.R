# Maxwell B. Allamong
# Alie(n)ation: Political Outsiders in the 2016 Election - Source File
# Created: Feb. 22nd, 2020
# Updated: Dec. 12th, 2020

# Packages ----
  # install.packages(c("stm","tm","SnowballC","wordcloud","quanteda","readtext",
  #                    "readxl","haven","latex2exp", "ggiraphExtra","effects","MASS","nnet",
  #                    "slam","Matrix","tidyverse","stargazer","corpus","splines",
  #                    "sjPlot","data.table","wordcloud","gtools","here","conflicted"))

# Libraries ----
  library(stm) # 
  library(tm)
  library(SnowballC)
  library(wordcloud)
  library(quanteda)
  library(readtext)
  library(readxl) # 
  library(haven) # 
  library(latex2exp) #
  library(ggiraphExtra)
  library(effects) # 
  library(MASS)
  library(nnet) #
  library(slam)
  library(Matrix)
  library(tidyverse) # 
  library(stargazer) # 
  library(corpus)
  library(splines)
  library(sjPlot)
  library(data.table)
  library(wordcloud)
  library(gtools) # 
  library(here) # 
  library(conflicted) #
    conflict_prefer("filter","dplyr")
    conflict_prefer("select","dplyr")
    conflict_prefer("annotate","ggplot2")


# Working Directory ----
  setwd(here())

# Load Data ----
  open.ends <- read_excel("Data/anes_timeseries_2016_redacted_openends.xlsx", 
                          sheet = "V161075") # read in open-ended responses
  mydata.16 <- read_dta("Data/anes_timeseries_2016.dta") # read in 2016 ANES
  mydata.12 <- read_dta("Data/anes_timeseries_2012.dta") # read in 2012 ANES
  mydata.cdf <- read_dta("Data/anes_timeseries_cdf.dta") # read in CDF

# Merge 2016 open-ends ----

  # Prepare ANES Open-Ended Responses
  colnames(open.ends)[1] <- "id"
  colnames(open.ends)[2] <- "documents"
  open.ends$id <- as.numeric(open.ends$id)
  open.ends <- open.ends %>%
    filter(id != 302252) # this ID doesn't have a match in ANES, and doesn't give a text response
  
  # Prepare ANES Covariates
  mydata.16 <- mydata.16 %>% 
    rename(id = V160001_orig)
  
  # Merge Open-Ended Responses with ID's
  mydata.16 <- mydata.16 %>%
    left_join(open.ends, by = "id")
  
  rm(open.ends)

# Co-variate Cleaning ----
# 2016 ----
  # Select variables
  mydata.16 <- mydata.16 %>% 
    select(documents, id, V161215, V161216, V161218, V161220,
           V162031x, V162062x, V161021, V161021a, 
           V161158x, V161267, V161270, V161310x, V161361x, V161126,
           V161342, V161004, V161010e, V162038x, V162065x,
           V161074)
  
  # Rename variables
  mydata.16 <- mydata.16 %>% 
    rename(trust = V161215, big.interests = V161216, corrupt = V161218, elect.attn = V161220, # alienation variables
           vote16 = V162031x, votechoice16 = V162062x, voteprimary16 = V161021, voteprimarychoice16 = V161021a, votepref16 = V162038x, # vote variables
           pid = V161158x, age = V161267, educ = V161270, race = V161310x, income = V161361x, ideo7 = V161126, # demographics
           reg.vote = V162065x, sex = V161342, pol.int = V161004, state = V161010e, # demographics and state
           like.trump = V161074) # like trump
  
  # Reshape variables
  mydata.16 <- mydata.16 %>% 
    mutate(trust = ifelse(trust > 0, trust, NA)) %>% # how often trust gov in Wash to do what is right (kept original coding of 1 = Always, 2 = Most of time, 3 = about half time, 4 = some of time, 5 = Never)
    mutate(big.interests = ifelse(big.interests > 0, 2 - big.interests, NA)) %>% # big interests or for all? (recoded as 0 = for benefit of all, 1 = big interests)
    mutate(elect.attn = ifelse(elect.attn > 0, elect.attn - 1, NA)) %>% # how much do elections make government pay attention? (recoded as 1 = a good deal, 2 = Some, 3 = Not Much)
    mutate(corrupt = ifelse(corrupt > 0, 6 - corrupt, NA)) %>% # how many people running gov are corrupt? (recoded as 1 = None, 2 = A few, 3 = About half, 4 = Most, 5 = All)
    mutate(alien.trust = ifelse(trust == "4" | trust == "5", 1, 0)) %>%
    mutate(alien.bigint = big.interests) %>%
    mutate(alien.cynicism = alien.trust + alien.bigint) %>%
    mutate(age = ifelse(age > 17, age, NA)) %>% # age (in years)
    mutate(educ = ifelse(educ > 0 & educ < 9, 1, # less than high school diploma
                  ifelse(educ == 9, 2, # high school diploma or equivalent
                  ifelse(educ == 10, 3, # some college, but no degree
                  ifelse(educ == 11 | educ == 12, 3, # associates degree
                  ifelse(educ == 13, 4, # bachelors degree
                  ifelse(educ == 14, 5, # masters degree
                  ifelse(educ == 15 | educ == 16, 5, NA)))))))) %>% # professional or doctorate degree, all else NA
    mutate(income = ifelse(income == -9 | income == -5, NA, income)) %>% # income
    mutate(income.q = as.numeric(quantcut(income, 5))) %>% # split into quantiles (recoded as 1 = <5k to 22.5k, 2 = 22.5k to 45k, 3 = 45k to 75k, 4 = 75k to 110k, 5 = 110k to >250k)
    mutate(race = ifelse(race > 0, race, NA)) %>% # race
    mutate(white = ifelse(race == "1", 1, 0)) %>% # white indicator
    mutate(ideo7 = ifelse(ideo7 < 1 | ideo7 > 7, NA, ideo7)) %>% # ideology (1 = ext. lib, 7 = ext. conserv)
    mutate(sex = ifelse(sex == 1 | sex == 2, sex, NA)) %>% # sex
    mutate(female = ifelse(sex == 2, 1, 0)) %>% # female indicator
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
    mutate(pol.int = ifelse(pol.int == 1, 3, # very much interested
                     ifelse(pol.int == 2, 2, # somehwhat interested
                     ifelse(pol.int == 3, 1, NA)))) %>% # not much interested
    mutate(like.trump = ifelse(like.trump == 1, 1,
                        ifelse(like.trump == 2, 0, NA))) %>%
    # mutate(vote16 = ifelse(vote16 == 0, 0, 
    #                 ifelse(vote16 == 1, 1, NA))) %>% # 1 = Voted, 0 = Did not vote
    mutate(vote16 = ifelse(reg.vote == 2, 1, 0)) %>%
    mutate(votechoice16 = ifelse(votechoice16 == 1, "Clinton",
                          ifelse(votechoice16 == 2, "Trump",
                          ifelse(votechoice16 == 3 | votechoice16 == 4 | votechoice16 == 5, "Other",
                                               ifelse(vote16 == 0, "Did not vote", NA))))) %>%
    mutate(votechoice16 = as.factor(votechoice16)) %>%
    mutate(voteprimary16 = ifelse(voteprimary16 == 1, 1, # indicator for 'voted in the 2016 primary'
                                  ifelse(voteprimary16 == 2, 0, NA))) %>%
    mutate(voteprimarychoice16 = ifelse(voteprimarychoice16 == 2, "Sanders",
                                 ifelse(voteprimarychoice16 == 4, "Trump",
                                 ifelse(voteprimarychoice16 == 1 | voteprimarychoice16 == 3 |
                                        voteprimarychoice16 == 5 | voteprimarychoice16 == 6 |
                                        voteprimarychoice16 == 7 | voteprimarychoice16 == 8 |
                                        voteprimarychoice16 == 9, "Other",
                                 ifelse(voteprimary16 == 0, "Did not vote", NA))))) %>%
    mutate(voteprimarychoice16 = as.factor(voteprimarychoice16)) %>%
    # mutate(votepref16 = ifelse(votepref16 == 10 | votepref16 == 11, "Clinton",
    #                     ifelse(votepref16 == 20 | votepref16 == 21, "Trump",
    #                     ifelse(votepref16 == 30 | votepref16 == 31, "Johnson",
    #                     ifelse(votepref16 == 40 | votepref16 == 41, "Stein",
    #                     ifelse(votepref16 == 50 | votepref16 == 51, "Other", NA)))))) %>%
    mutate(votepref16 = ifelse(votepref16 == 10 | votepref16 == 11, "Clinton",
                        ifelse(votepref16 == 20 | votepref16 == 21, "Trump",
                        ifelse(votepref16 == 30 | votepref16 == 31 | votepref16 == 40 | 
                               votepref16 == 41 | votepref16 == 50 | votepref16 == 51, "Other/Third-Party", NA)))) %>%
    mutate(votepref16 = as.factor(votepref16)) %>%
    mutate(trumppref = ifelse(votepref16 == "Trump", 1, 0)) %>%
    mutate(general16.trump = ifelse(votechoice16 == "Trump", 1, 0)) %>% # indicator for Trump general election voter
    mutate(primary16.trump = ifelse(voteprimarychoice16 == "Trump", 1, 0)) %>% # indicator for Trump primary voter
    mutate(primary16.sanders = ifelse(voteprimarychoice16 == "Sanders", 1, 0)) %>% # indicator for Sanders primary voter
    mutate(sup.tues = ifelse(state == "AL" | state == "AR" | state == "GA" | state == "MN" | state == "TN" | # super tuesday state
                             state == "TX" | state == "VT" | state == "VA", 1, 0)) %>%
    mutate(year = 2016)

# 2012 ----
  # Select variables
  mydata.12 <- mydata.12 %>% 
    select(libcpre_self, inc_incgroup_pre, dem_edugroup_x, dem_racecps_white,
           trustgov_trustgrev, trustgov_bigintrst, respons_elections, 
           gender_respondent_x, pid_x, rvote2012_x, presvote2012_x, prevote_primv,
           dem_age_r_x, interest_following)
  
  # Reshape variables
  mydata.12 <- mydata.12 %>% 
    mutate(ideo7 = ifelse(libcpre_self < 1 | libcpre_self > 7, NA, libcpre_self)) %>% # ideology (1 = ext. lib, 7 = ext. conserv)
    mutate(income = ifelse(as.numeric(inc_incgroup_pre) < 1, NA, as.numeric(inc_incgroup_pre))) %>%
    mutate(income.q = as.numeric(quantcut(income, 5))) %>% # income quartiles
    mutate(educ = ifelse(dem_edugroup_x < 1, NA, dem_edugroup_x)) %>%
    mutate(age = ifelse(dem_age_r_x > 16, dem_age_r_x, NA)) %>%
    mutate(trust = ifelse(trustgov_trustgrev > 0, trustgov_trustgrev, NA)) %>% # how often trust gov in Wash to do what is right (1 = Always, 2 = Most of time, 3 = about half time, 4 = some of time, 5 = Never)
    mutate(big.interests = ifelse(trustgov_bigintrst > 0, 2 - trustgov_bigintrst, NA)) %>% # big interests or for all? (recoded as 0 = for benefit of all, 1 = big interests)
    mutate(elect.attn = ifelse(respons_elections > 0, respons_elections, NA)) %>% # how much do elections make government pay attention? (1 = a good deal, 2 = Some, 3 = Not Much)
    mutate(alien.trust = ifelse(trust == "4" | trust == "5", 1, 0)) %>%
    mutate(alien.bigint = big.interests) %>%
    mutate(alien.cynicism = alien.trust + alien.bigint) %>%
    mutate(female = ifelse(gender_respondent_x == 2, 1, 0)) %>% # indicator for female
    mutate(white = dem_racecps_white) %>% # indicator for white
    mutate(pid3 = ifelse(pid_x == 1 | pid_x == 2 | pid_x == 3, "Dem", # democrat
                  ifelse(pid_x == 4, "Ind", # repubilcan
                  ifelse(pid_x == 5 | pid_x == 6 | pid_x == 7, "Rep", NA)))) %>% # independent
    mutate(pid7 = ifelse(pid_x > 0, pid_x, NA)) %>% # 7-point pid
    mutate(ind = ifelse(pid3 == "Ind", 1, 0)) %>% # indicator for independent
    mutate(party.strength = ifelse(pid7 == 1 | pid7 == 7, 3, # strong
                            ifelse(pid7 == 2 | pid7 == 6, 2, # weak
                            ifelse(pid7 == 3 | pid7 == 5, 1, # leaning
                            ifelse(pid7 == 4, 0, NA))))) %>%
    mutate(pol.int = ifelse(interest_following == 1, 3, # very much interested
                     ifelse(interest_following == 2, 2, # somehwhat interested
                     ifelse(interest_following == 3, 1, NA)))) %>% # not much interested
    mutate(vote12 = ifelse(rvote2012_x < 1, NA, rvote2012_x)) %>%
    mutate(vote12 = ifelse(vote12 == 1, 1, 0)) %>% # indicator for 'voted'
    mutate(voteprimary12 = ifelse(prevote_primv == 1, 1, # vote in primary (recoded as 1 = Yes, 0 = No)
                           ifelse(prevote_primv == 2, 0, NA))) %>% 
    mutate(year = 2012)

# 1988 - 2008 ----
  # Select variables
  mydata.cdf <- mydata.cdf %>%
    select(VCF0004, VCF0605, VCF0604, VCF0624, VCF0703, 
           VCF0104, VCF0110, VCF0101, VCF0301, VCF0105a, 
           VCF0803, VCF0114, VCF0310, VCF9026, VCF9265)
  
  # Rename variables
  mydata.cdf <- mydata.cdf %>%
    rename(big.interests = VCF0605, trust = VCF0604, elect.attn = VCF0624, 
           voted = VCF0703, voteprimary = VCF9026, partyid = VCF0301,
           female = VCF0104, educ = VCF0110, year = VCF0004, age = VCF0101, 
           race = VCF0105a, ideology = VCF0803, income.q = VCF0114, 
           pol.int = VCF0310, voteprimary2 = VCF9265)
  
  # Reshape variables
  mydata.cdf <- mydata.cdf %>% 
    filter(year == 1988 | year == 1992 | year == 1996 | year == 2000 | year == 2004 | year == 2008 | year == 2012) %>% # filter to 1988-2012 presidential election years
    mutate(trust = ifelse(trust > 0 & trust < 5, trust, NA)) %>% # how often trust gov in Wash to do what is right (1 = none/never, 2  = some of the time, 3 = most of the time, 4 = just about always)
    mutate(big.interests = ifelse(big.interests > 0 & big.interests < 3, big.interests, NA)) %>% # big interests or for all? (1 = few big interests, 2 = benfit of all)
    mutate(elect.attn = ifelse(elect.attn > 0 & elect.attn < 4, 4 - elect.attn, NA)) %>% # how much do elections make government pay attention? (recoded as 1 = a good deal, 2 = Some, 3 = Not Much)
    mutate(alien.trust = ifelse(trust == 1 | trust == 2, 1, 0)) %>%
    mutate(alien.bigint = ifelse(big.interests == 1, 1, 0)) %>%
    mutate(alien.cynicism = alien.trust + alien.bigint) %>%
    mutate(voted = ifelse(voted < 4 & voted > 0, voted, NA)) %>% # summary of registration and vote (1 = not registered/did not vote, 2 = registered/did not vote, 3 = voted (registered))
    mutate(voted = ifelse(voted == 3, 1, 0)) %>% # indicator for 'voted'
    mutate(voteprimary = ifelse(voteprimary2 == 1, 1, # vote in primary (recoded as 1 = Voted, 0 = Didn't vote)
                         ifelse(voteprimary2 == 2, 0, NA))) %>%
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
    mutate(ideo7 = ifelse(ideology > 0 & ideology < 9, ideology, NA)) %>% # remove NA
    mutate(income.q = ifelse(income.q > 0, income.q, NA)) # remove NA


