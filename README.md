# Alienation

Repository for "Alie(n)ation: Political Outsiders in the 2016 U.S. Presidential Election"

## Repository Contents

-   R Scripts

    1.  `Alienation-Source.R`
    2.  `Alienation-Descriptives.R`
    3.  `Alienaiton-STM.R`
    4.  `Alienation-VoteModels.R`

-   R Project

    1.  `Alienation.Rproj`

-   Data Files

    1.  `anes_timeseries_2016_redacted_openends.xslx`
    2.  `anes_timeseries_2016.dta`
    3.  `anes_timeseries_2012.dta`
    4.  `anes_timeseries_cdf.dta`

## Replicating the paper

As noted above, there are four scripts in this repository: `Alienation-Source.R`, `Alienation-Descriptives.R`, `Alienation-STM.R`, and `Alienation-VoteModels.R`. Running these scripts in their entirety will produce all of the empirical results found in the paper and the online appendix. A short description of each script is provided below:

1.  `Alienation-Source.R`

    -   This is the source script where I clean and reshape the four data files that are used in this project `anes_timeseries_2016_redacted_openends.xslx`, `anes_timeseries_2016.dta`, `anes_timeseries_2012.dta`, and `anes_timeseries_cdf.dta`. These four data files come directly from the American National Election Studies and can be accessed by clicking [here](https://electionstudies.org/data-center/). This script begins by merging the 2016 survey data with the redacted open-ended responses. Then, I proceed to isolating the necessary variables from the 2016, 2012, and CDF files, renaming the necessary variables, and finally reshaping the necessary variables. When this script is run in its entirety, it will produce three cleaned data-frames: `mydata.16`, `mydata.12`, and `mydata.cdf`. Each of the remaining scripts in this repository begin by sourcing this script.

2.  `Alienation-Descriptives.R`

    -   

3.  `Alienaiton-STM.R`

    -   This script is for the creating and exploration of the Structural Topic Model.

4.  `Alienation-VoteModels.R`

    -   
