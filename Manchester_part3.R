#' Statistical inference using weights and survey design
#' 3. R and Stata examples: syntax file
#' author: 
#'   name: Pierre Walthéry
#' institute: UK Data Service
#' bibliography: inference.bib
#' date: "October 2025"


#' ## Preparing the data - R  
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls())
library(dplyr)                                                              ### Data manipulation functions
library(haven)                                                              ### Importing stata/SPSS files
library(Hmisc)                                                              ### Extra statistical functions
library(survey)                                                             ### Survey design functions
library(ggplot2)
setwd("~/OneDrive/trainings/Inference_workshops/Manchester_Nov24/")         ### Edit as appropriate
datadir<-"~/Data/"                                                          ### Edit as appropriate
bsa17<-read_dta(paste0(datadir,"bsa/UKDA-8450-stata/bsa2017_for_ukda.dta")) ### Importing Stata format dataset
dim(bsa17)                                                                  ### Check the dataset

### You can also use `View()`

#' 
#' - We can now specify the survey design using:
#' 
#'   - `Spoint` as Primary Sampling Unit ids, 
#'   - `StratID` as strata ids, 
#'   - `WtFactor` as weights. 
#' 
#' ##  Specifying the survey design in R (1)
#' 
#' - We create a `svydesign` object, i.e. a survey design informed copy of the data, which will be used for  subsequent estimation.
#' 
## ----5_2---------------------------------------------------------------------------------------------------------------------------------------------------------------------
bsa17.s<-svydesign(ids=~Spoint, 
                   strata=~StratID, 
                   weights=~WtFactor,
                   data=bsa17)        ### Specifying the survey design
class(bsa17.s)                        ### Examining the svydesign object                                        

#' ##  Specifying the survey design in R (2)
#' 
#' - We can inspect   the content of `bsa17.s`. Beware, the output is really large!
#' 

summary(bsa17.s)                      ### ... And looking at its content


#' 
#' ## Mean age and its 95% CI

svymean(~RAgeE,bsa17.s)

#'  By default  `svymean()` computes the standard error of the mean. We need to  
#'  embed it within `confint()` in order to get a confidence interval. 
#' 

confint(                                  ### Computing the confidence interval...
  svymean(~RAgeE,bsa17.s)                 ### And the mean
  )                   

#' - For a slightly nicer output:

round(                                    ### Rounding the results to one decimal point
  c(
    svymean(~RAgeE,bsa17.s),              ### Computing the mean...
    confint(svymean(~RAgeE,bsa17.s))      ### And its 95% CI
    ),
  1) 

#' ## Question 3
#' - What would be the consequences of:
#' 
#'   - weighing but not accounting for the sample design; 
#'   - neither using  weights or accounting for the sample design?
#' 
#' - When: 
#'   - inferring the mean age in the population?
#'   - computing the uncertainty  of this  estimate? 
#' 
#' ## Q. 3 answer (1): command-based weighting
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
a.m<-wtd.mean(                               ### Weighted mean function from the
  bsa17$RAgeE,                               ### Hmisc package
  bsa17$WtFactor,
  normwt = T)                                ### Option specific to survey weights  

### Computation of the standard error by hand...
a.se<-sqrt(
          wtd.var(bsa17$RAgeE,               ### ... using the weighted variance function from Hmisc
                  bsa17$WtFactor,
                  normwt = T)
          )/
       sqrt(
         nrow(bsa17)                         ### ... shortcut to sample size
         )

c(a.m,                                       ### Concatenating the mean..
  a.m-1.96*a.se,                             ### ... the lower bound of the CI
  a.m+1.96*a.se)                             ### ... and the higher bound

#' 
#'  
#' ## Q. 3 answer (2):  unweighted estimates
#' 
#' 

ua.m<-mean(bsa17$RAgeE)                     ### mean() function from R Base
  
ua.se<-sd(bsa17$RAgeE)/                     ### ... standard errors
      sqrt(nrow(bsa17))    ##

c(ua.m,                                     ### and putting it all together
  ua.m-1.96*ua.se,
  ua.m+1.96*ua.se
  )


#' ## Computing a proportion and its 95% confidence interval

#' ## Let's explore the variable
#' - Phrasing of the question:
#' 

attr(bsa17$Politics,"label")     

#' - Sample distribution


table(
  droplevels(                                    ### We are using droplevels() 
             as_factor(bsa17$Politics, "both")   ### in order to hide categories
             )                                   ### ... without any observations
  ) 


#' - Let's infer the percentage of those who were interested in politics  in 2017


  round(100*
    prop.table(
      svytable(~(Politics==1 | Politics==2),bsa17.s)
      ),1)



  
bsa17$PolC<-as.factor(ifelse(bsa17$Politics==1 | bsa17$Politics==2, "Interested", "Not so much") )
bsa17.s<-svydesign(ids=~Spoint, 
                   strata=~StratID, 
                   weights=~WtFactor,
                   data=bsa17)        

  round(100*
    prop.table(
      svytable(~(PolC),bsa17.s)
      ),1)




#' - Confidence intervals for proportions 
#' 
#' - In R we  specify this via `svyciprop()` and `I()`:
#' 
#'   - The former computes the proportion and its confidence interval (by default 95%)...
#'   - ...  the latter allows us to define the category of interest of a polytomous variable.
#'   - As before, we could have used a recoded dichotomic variable instead
#' 

  p<-svyciprop(
            ~I(Politics==1 | Politics==2),
            bsa17.s)
p

#' 
#' - A neater version:
#' 

round(100*
        c("% interested"= p[[1]],                                          ### Extracts the point estimate
          "95% CI - LB"=attr(p,"ci")[[1]],"95% CI - UB"=attr(p,"ci")[[2]]  ### Extracts the CI
          ),1 
      )

#' 
#' ## Question 4
#' - What is the proportion of respondents aged 17-34 in the sample, as well as its 95% confidence interval? 
#' 
#'   - You can use `RAgecat5`
#' 
#' ## Answer
#' 
#' - The proportion of 17-34 year old in the sample is: 
#' 
## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
a<-svyciprop(~I(RAgecat5 == 1),
              bsa17.s)
a
round(
  100*a[1],
     1)

#' 
#' and its 95% confidence interval:                 
#' 

round(
    100*
    confint(a),  ### Another way of extracting CI from svyciprop objects
    1)


#' ## Computing domain estimates in R
#' 
#' - Say we would like to compute the mean age of BSA respondents by Government Office Regions
#' - We need to specify:
#' 
#'   - The outcome variable whose estimate we want to compute: i.e. `RAgeE`
#'   - The grouping variable(s) `GOR_ID`
#'   - The estimation function we are going to use here: `svymean()`
#'   - And the type of type of variance estimation we would like to see displayed i.e. standard errors or confidence interval  
#' 
#' 

d<-      svyby(~RAgeE,               ### Outcome variable
             by=~as_factor(GOR_ID),  ### Subpopulations
             svymean,                ### Estimation function
             design=bsa17.s,
             vartype = "ci")         ### CI or SE 
round(d[-1],1)


#' 
#' ## Proportions
#' 
c<-svyby(~I(RAgecat5==1),
            by=~as_factor(GOR_ID),
            svyciprop,
            design=bsa17.s,
            vartype = "ci")

round(100*c[-1],1)

#' 
#' ## Visualising CIs


ggplot(c, aes(x = `I(RAgecat5 == 1)`, y = rownames(c))) +
  geom_point(size = 3, color = "#ff9800") +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.1, color = "#ff9800") +
  labs(
    title = "% aged 17-34 by region",
    y = "Government Office Region",
    x = "% Aged between 17 and 34'"
  ) +
  theme_minimal(base_size = 14)


#' ## Question 5
#' - What is the 95% confidence interval for the proportion of people significantly interested in politics in the North East? 
#' - Is the proportion likely to be different in London? In what way? 
#' - What is the region of the UK for which the estimates are likely to be least precise?
#' 
#' Proportion of those interested in politics by region:


c<-svyby(~I(Politics==1 | Politics==2),
            by=~as_factor(GOR_ID),
            svyciprop,
            design=bsa17.s,
            vartype = "ci")

round(100*c[-1],1)

#' ## Plotting CIs

#| output: asis
ggplot(c, aes(x = `I(Politics == 1 | Politics == 2)`, y = rownames(c))) +
  geom_point(size = 3, color = "#ff9800") +
  geom_errorbar(aes(xmin = ci_l, xmax = ci_u), width = 0.1, color = "#ff9800") +
  labs(
    title = "% interested in politics by region",
    y = "Government Office Region",
    x = "% Interested in politics 'a great deal'or 'quite a lot'"
  ) +
  theme_minimal(base_size = 14)


#' ## Question 6
#' - Using interest in politics as before, and three category age `RAgecat5`: 
#' - Produce a table showing the proportion of respondents significantly interested in politics by age group and gender
#' - Assess whether the age difference in interest for politics is similar for each gender.
#' - Is it fair to say that men aged under 35 are more likely to declare being  interested  in politics  than women aged 55 and above?
#' 

round(
      100*
        svyby(~I(Politics==1 | Politics==2),
              by=~as_factor(RAgecat5)+as_factor(Rsex),
              svyciprop,
              design=bsa17.s,
              vartype = "ci")[c(-8,-4),c(-2,-1)],1)


#' 
#' 
#' ##  Estimating employment by region 

#' ## Regional employment rates using R
#' 
#' - Let's first produce uncorrected estimates of the regional population.
#' - We will still use the survey design functions, but declare a SRS design 
#' 

lfs<-read_dta(
              (paste0(
                datadir,
                "lfs/UKDA-8999-stata/lfsp_aj22_eul_pwt22.dta"
                )
               )
              )%>%
     select(PWT22,PIWT22,GOVTOF2,URESMC,ILODEFR)

table(as_factor(lfs$GOVTOF2))

#' For some reason, the ONS use a distinct category for Merseyside, but not the `GOVTOF` variable in our dataset. 
#' We will correct this using another, more detailed region variable: `URESMC`.

lfs<-lfs%>%
     mutate(
            govtof=ifelse(URESMC==15,3,GOVTOF2)
            )        # Identifying Merseyside using URESMC

lfs$govtof.f<-as.ordered(lfs$govtof)                           # Converting into factor
levels(lfs$govtof.f)<-c(names(attr((lfs$GOVTOF2),"labels")[3:4]),
                        "Merseyside",
                        names(attr((lfs$GOVTOF2),"labels")[5:14])) # Adding factor levels from existing labels
table(lfs$govtof.f)

#' Let us now examine  the confidence intervals for the percentage of persons in employment:

lfs.s<-svydesign(ids=~1,weights=~PWT22,data=lfs) 
d<-      svyby(~I(ILODEFR==1),
               by=~govtof.f,
               svyciprop,
               vartype="se",
               design=lfs.s)

df<-100*data.frame(d[-1])
names(df)<-c("Empl.","SE")
df["Low.1"]<-round(df$Empl.-(1.96*df$SE),1)
df["High.1"]<-round(df$Empl.+(1.96*df$SE),1)
df

#' We can now import the design factors from the LFS documentation. This has to be done by hand. 

df$deft<-c(0.8712,1.0857,1.3655,
           1.0051,0.9634,1.0382,
           0.8936,1.3272,0.9677,
           0.9137,1.0012,1.0437,
           0.7113)
df["Low.2"]<-round(df$Empl.-(1.96*df$SE*df$deft),1)
df["High.2"]<-round(df$Empl.+(1.96*df$SE*df$deft),1)

df

