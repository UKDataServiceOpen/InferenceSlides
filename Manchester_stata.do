*************************************************************
  *
  * Statistical inference using weights and survey design
* Stata examples (Exercises only)

** Survey design in Stata


**  Opening the dataset and declaring the survey design (scroll down for full output)

use ~/Data/bsa/UKDA-8450-stata/bsa2017_for_ukda.dta,clear

svyset Spoint [pw=WtFactor], strata(StratID) 

**  Computing the survey design-informed  version of the mean...

svy: mean RAgeE

** And the other two versions: 
  
  mean RAgeE [pw=WtFactor]
mean RAgeE

** Answer - Stata

/*
  - Not using weights results in  overestimating the mean age in the population by about 4 years. 
- This might be due to the fact that older respondents are more likely to take part to surveys. 
- Using command-based weighting does not alter the value of the estimated population mean when compared with SD informed estimates...
- ... but would lead us to overestimating the precision/underestimate the uncertainty of our estimate -- by about plus or minus 3 months. 
*/
  
  ** Computing a proportion and its 95% confidence interval


** Creating a dummy variable for significant interest in politics
quietly recode Politics 1 2 =1 3/8=0,gen(Politics2) 
** Survey-design informed frequencies...
svy:ta Politics2  


** Proportions and CIs

svy:ta Politics2, percent ci 

** Question 4
*What is the proportion of respondents aged 17-34 in the sample, as well as its 95% confidence interval? 
  
  
  ** Answer

svy:ta RAgecat5, percent ci                         


** Question 5
/*- What is the 95% confidence interval for the proportion of people significantly interested in politics in the North East? 
  - Is the proportion likely to be different in London? In what way? 
  - What is the region of the UK for  the estimates are likely to be least precise?*/
  
  ** Not accounting for domain estimation

svy:prop Politics2 if GOR_ID==1, percent  cformat(%9.1f)

** ... And  accounting for it

** % interested in politics in the North East...


svy,subpop(if GOR_ID==1):prop Politics2, percent  cformat(%9.1f)


** ... And in London


svy,subpop(if GOR_ID==7):prop Politics2, percent  cformat(%9.1f)

**  Alternative  (not recommended)


svy:prop Politics2,over(GOR_ID) percent  cformat(%9.1f)


** Question 6

/*Using interest in politics as before, and three category age `RAgecat5`: 
  
  - Produce a table showing the proportion of respondents significantly interested in politics by age group and gender
- Assess whether the age difference in interest for politics is similar for each gender.
- Is it fair to say that men aged under 35 are more likely to declare being  interested  in politics  than women aged 55 and above?*/
  
  ** Q6 - Answer

*  Men under 35
svy,subpop(if RAgecat5==1 & Rsex==1):prop Politics2 ,percent  cformat(%9.1f) 
* Women 55+ 
  svy,subpop(if RAgecat5==3 & Rsex==2):prop Politics2, percent  cformat(%9.1f) 


** Contrast with: 
  
  svy: tab  Politics2 if RAgecat5==1 & Rsex==1, percent ci 
