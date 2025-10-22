use /home/piet/Documents/UKDS/data/bsa/UKDA-8450-stata/bsa2017_for_ukda.dta,clear


svyset Spoint [pw=WtFactor], strata(StratID)

svy:mean RAgeE

* Question 3

mean RAgeE
mean RAgeE [pw=WtFactor]

* Proportions and Question 4

recode Politics 1 2 =1 3/8=2,gen(Politics2)
 
svy:ta Politics2

svy:ta Politics2, percent ci
  
svy:ta RAgecat5, percent ci

* Svy

svy:mean RAgeE,over(GOR_ID)

ta GOR_ID,gen(GorDummy)
svy,subpop(GorDummy1):mean RAgeE

foreach(d in 1/13){
svy,subpop(GorDummy`d'): mean RAgeE
} 
svy:prop Politics2,over(GOR_ID)
svy,subpop(gor_dummy1):prop Politics2
  
  
egen RagSex=group(RAgecat5 Rsex),label
tab RagSex,gen(RagSex)

svy:pPolitics2,over(RagSex )

svy,subpop(RagSex1):prop Politics2
svy,subpop(RagSex1):tab Politics2, percent ci
svy,subpop(RagSex2):prop Politics2
svy,subpop(RagSex3):prop Politics2
