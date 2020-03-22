
##***************************(@MEFWRIT)**************************##
##***************************************************************##  
## This is the Vaccn project - a tentative PNAS paper Fiagbenu & Troia, 2021 :-)
## The aim  of this script is to clean the raw data so that we can both have a 
## common dataset to work with
## 19Nov 2016 ###@Michael Edem Fiagbenu wrote it (@MEFWRIT)

##***************************(@MEFWRIT)***************************##
##****************************************************************##  


  ######### let's get the ball rolling
## We start by loading some packages and reading in the raw wlcome datasets 


########## load packages ############
rm(list = ls()) # clear workspace
set.seed(282018) ## for replication
cat("\f") # cat("\014") clear console
x<-c("data.table","psych", "haven","tidyverse","naniar", "tibble","janitor","foreign","readxl","gdata")  ## load packages
lapply(x, require, character.only = TRUE)
setwd("~/Dropbox/Projects/Jena/vaccine/vaccine//data/")
source("corstarsl.R")   #### for correlation table 


colMax <- function(data) sapply(data, max, na.rm = T)  ## find the max value in each column
colMin <- function(data) sapply(data, min, na.rm = T)  ## find the min value in each column



###### started data cleaning #########

raw_welcomeDat = read_csv2("raw_datawlcme.csv")  ## read/load in raw data
CountryInfo    = read_csv("wlcome_CountryInfo.csv")  ## read/load in raw data
##### add var names to data # btw. I am very bad at namin labels. some labels are unusually long. But I have included the original wlcome questions ## to make for easy identification

setnames(raw_welcomeDat, old = c("WP5","wgt","PROJWT","FIELD_DATE","YEAR_CALENDAR",
                                 "Q1","Q2","Q3","Q4","Q5A","Q5B","Q5C",             
                                 "Q6","Q7","Q8","Q9","Q10A","Q10B","Q11A","Q11B","Q11C","Q11D","Q11E","Q11F","Q11G","Q12","Q13",               
                                 "Q14A","Q14B","Q15A","Q15B","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23","Q24","Q25","Q26",              
                                 "Q27","Q28","D1","Q29","Q30"), 
         
                         new = c("Country","wgt1","wgt2","FIELD_DATE","YEAR_CALENDAR",
                                 "Know_abt_Sci_Q1","Understand_Sci_Q2","Disease_Sci_Q3","Poetry_Sci_Q4","LearnSci_Prim_Q5A","LearnSci_Sec_Q5B","LearnSci_Uni_Q5C",   
                                 "Sci_try_Inform_Q6","Health_try_knowMore_Q7","Sci_want_knowMore_Q8","Health_want_knowMore_Q9","Conf_NGOs_Q10A","Conf_Hosp_10B","Trust_Ppl_Q11A","Trust_Govt_Q11B","Trust_Scientists_Q11C","Trust_Journos_Q11D","Trust_Doctors_Q11E","Trust_NGOs_Q11F","Trust_Healers_Q11G","Trust_Science_Q12","Trust_Sci_Accuracy_Q13",               
                                 "Trust_UniScientists_Benefit_Q14A","Trust_UniScientists_Open_Q14B","Trust_CompnyScientists_benef_Q15A","TrustCompnyScientists_Open_Q15B","Scientists_benef_Q16","Scientists_benefYou_Q17","SciTech_improv_Q18","SciTech_Jobs_Q19","Trust_who_Q20","Trust_med_country_Q21","Trust_NursDocs_country_Q22","Vaccn_Knowledge_Heard_Vaccn_Q23","Vaccn_Knowledge_Child_Q24","Vaccn_RiskPercep_Q25","Vaccn_Knowledge_Effktv_Q26",              
                                 "Have_Child_Q27","Accept_Vaccn_4Child_Q28","Religion_D1","SciRelig_disag_Q29","Sci_Relig_choose_Q30"))

######## compute country means

#### first, the data contains 97,98,99 response which are "Not Sure" or misssing responses, lets  change them to NA (not available)
#### we want to keep in the NAs for now. We do not know why they are missing so we cannot remove them at this stage  

## confirm that there are  97,98,99  in data
colMax(raw_welcomeDat)

#Pls remember that Age contains, vals 97,98,99, and we want to preserve them. So lets first remove the Age column from the dataset to prevent us from replacing them with NAs
raw_welcomeDat_NO_Age =  select(raw_welcomeDat,-c(Age))  
colMax(raw_welcomeDat_NO_Age)

### now proceed to remove 97,98,99,
#raw_welcomeDat_NO_Age=na_if(raw_welcomeDat_NO_Age, 97)
raw_welcomeDat_NO_Age=na_if(raw_welcomeDat_NO_Age, 98)
raw_welcomeDat_NO_Age=na_if(raw_welcomeDat_NO_Age, 99)

## confirm that there are no  97,98,99  in data
colMax(raw_welcomeDat_NO_Age)

##### now add the Age column to the dataset and delete "raw_welcomeDat_NO_Age" 
raw_welcomeDat_Age = raw_welcomeDat_NO_Age
raw_welcomeDat_Age=add_column(raw_welcomeDat_NO_Age, Age = raw_welcomeDat$Age, .before = "AgeCategories"); rm(raw_welcomeDat_NO_Age)
table(raw_welcomeDat_Age$Age)  ### check if age contains 97,98,99


raw_welcomeDat_2 = raw_welcomeDat_Age

############ in Sci_Relig_choose_Q30 97 is coded as "it depends"
table(raw_welcomeDat_2$Sci_Relig_choose_Q30)  ### check it
#recode it as 2 and change 2=Religion Over science to 3 instead 
raw_welcomeDat_2[,c("Sci_Relig_choose_Q30")]  <- lapply(raw_welcomeDat_2[c("Sci_Relig_choose_Q30")], FUN = function(x) car::recode(x, "2=3;97=2"))
table(raw_welcomeDat_2$Sci_Relig_choose_Q30)  ### check finanal result 1 = Science over rel, 2 = it depends 3 = religion over science

########### reverse score items appropraitely #########


## reverse score items on 3-item scale
raw_welcomeDat_2[,c("LearnSci_Prim_Q5A","LearnSci_Sec_Q5B","LearnSci_Uni_Q5C", "Sci_try_Inform_Q6","Health_try_knowMore_Q7","Sci_want_knowMore_Q8","Health_want_knowMore_Q9","Conf_NGOs_Q10A","Conf_Hosp_10B",
                            "Scientists_benefYou_Q17","SciTech_improv_Q18","Vaccn_Knowledge_Heard_Vaccn_Q23","Have_Child_Q27","Accept_Vaccn_4Child_Q28","SciRelig_disag_Q29","Religion_D1","Disease_Sci_Q3")]  = 3 - raw_welcomeDat_2[ ,c("LearnSci_Prim_Q5A","LearnSci_Sec_Q5B","LearnSci_Uni_Q5C", "Sci_try_Inform_Q6","Health_try_knowMore_Q7","Sci_want_knowMore_Q8","Health_want_knowMore_Q9","Conf_NGOs_Q10A","Conf_Hosp_10B",
                                                                                                                                                                                                                      "Scientists_benefYou_Q17","SciTech_improv_Q18","Vaccn_Knowledge_Heard_Vaccn_Q23","Have_Child_Q27","Accept_Vaccn_4Child_Q28","SciRelig_disag_Q29","Religion_D1","Disease_Sci_Q3")] 

## reverse score items on 4-item scale

raw_welcomeDat_2[,c("Scientists_benef_Q16","SciTech_Jobs_Q19","Subjective_Income")]  = 4 - raw_welcomeDat_2[ ,c("Scientists_benef_Q16","SciTech_Jobs_Q19","Subjective_Income")] 

raw_welcomeDat_2[,c("Know_abt_Sci_Q1", "Understand_Sci_Q2","Trust_Ppl_Q11A","Trust_Govt_Q11B","Trust_Scientists_Q11C","Trust_Journos_Q11D","Trust_Doctors_Q11E","Trust_NGOs_Q11F","Trust_Healers_Q11G","Trust_Science_Q12","Trust_Sci_Accuracy_Q13",               
                    "Trust_UniScientists_Benefit_Q14A","Trust_UniScientists_Open_Q14B","Trust_CompnyScientists_benef_Q15A","TrustCompnyScientists_Open_Q15B","Trust_med_country_Q21","Trust_NursDocs_country_Q22")]  = 5 - raw_welcomeDat_2[ ,c("Know_abt_Sci_Q1", "Understand_Sci_Q2","Trust_Ppl_Q11A","Trust_Govt_Q11B","Trust_Scientists_Q11C","Trust_Journos_Q11D","Trust_Doctors_Q11E","Trust_NGOs_Q11F","Trust_Healers_Q11G","Trust_Science_Q12","Trust_Sci_Accuracy_Q13",               
                                                                                                                                                                                                                                                "Trust_UniScientists_Benefit_Q14A","Trust_UniScientists_Open_Q14B","Trust_CompnyScientists_benef_Q15A","TrustCompnyScientists_Open_Q15B","Trust_med_country_Q21","Trust_NursDocs_country_Q22")] 
## reverse score items on 5-item scale

raw_welcomeDat_2[,c("Vaccn_Knowledge_Child_Q24","Vaccn_Knowledge_Effktv_Q26")]  = 6 - raw_welcomeDat_2[ ,c("Vaccn_Knowledge_Child_Q24","Vaccn_Knowledge_Effktv_Q26")] 

###check if reverse scoring worked by comparing raw unreversed vs new reversed data
head(raw_welcomeDat)
head(raw_welcomeDat_2)

tail(raw_welcomeDat)
tail(raw_welcomeDat_2)


######### after reverse scoring, lets create our some composite scores ##############
# Science_Literacy = mean("Know_abt_Sci_Q1"+"Understand_Sci_Q2")
raw_welcomeDat_2=add_column(raw_welcomeDat_2, Science_Literacy = rowMeans(select(raw_welcomeDat_2,c(Know_abt_Sci_Q1,Understand_Sci_Q2))), .before = "Know_abt_Sci_Q1")

# Science_Education = "LearnSci_Prim_Q5A"+"LearnSci_Sec_Q5B"+"LearnSci_Uni_Q5C",
raw_welcomeDat_2=add_column(raw_welcomeDat_2, Science_Education = rowMeans(select(raw_welcomeDat_2,c(LearnSci_Prim_Q5A,LearnSci_Sec_Q5B,LearnSci_Uni_Q5C))), .before = "LearnSci_Prim_Q5A")

# Trust_UniScientists = "Trust_UniScientists_Benefit_Q14A",+"Trust_UniScientists_Open_Q14B"
raw_welcomeDat_2=add_column(raw_welcomeDat_2, Trust_UniScientists = rowMeans(select(raw_welcomeDat_2,c(Trust_UniScientists_Benefit_Q14A,Trust_UniScientists_Open_Q14B))), .before = "Trust_UniScientists_Benefit_Q14A")

# Trust_CompnyScientists = #"Trust_CompnyScientists_benef_Q15A","TrustCompnyScientists_Open_Q15B",
raw_welcomeDat_2=add_column(raw_welcomeDat_2, Trust_CompnyScientists = rowMeans(select(raw_welcomeDat_2,c(Trust_CompnyScientists_benef_Q15A,TrustCompnyScientists_Open_Q15B))), .before = "Trust_CompnyScientists_benef_Q15A")

# Trust_who_Q20 = 1. Fam/Friends 2&5 Religious_leader/Healer 3. Health Prof. 4. Famous person 5. Others
raw_welcomeDat_2[,c("Trust_who_Q20")]  <- lapply(raw_welcomeDat_2[c("Trust_who_Q20")], FUN = function(x) car::recode(x, "5=2;97=5"))


####### since we are exploring vaccine attitudes and knowledge, lets filter out people who have never heard about vaccines before
# this is because, these people were not asked further about whether thier views about safety and efficacy or vaccines
table(raw_welcomeDat_2$Vaccn_Knowledge_Heard_Vaccn_Q23)/nrow(raw_welcomeDat_2)*100 ### check the percentages = only about     10% of people worldwide have not heard about vaccines before

raw_welcomeDat_2 = filter(raw_welcomeDat_2,Vaccn_Knowledge_Heard_Vaccn_Q23 ==2) #filter them out of the data
table(raw_welcomeDat_2$Vaccn_Knowledge_Heard_Vaccn_Q23) # it worked


###### ok! so now we are done with the first part of the cleaning, we now have individual level datasets, lets save it

clean_indlvel_welcomeDat = raw_welcomeDat_2

setwd("~/Dropbox/Projects/Jena/vaccine/vaccine///data/")

write.csv(clean_indlvel_welcomeDat, "clean_indlvel_welcomeDat.csv", quote=FALSE, row.names=FALSE) #


write_sav(clean_indlvel_welcomeDat, "clean_indlvel_welcomeDat.sav") #

### for now we write a small for loop to compute country level means ## for loops are slow, we should find a better/faster way to do this
CountryNO =CountryInfo$CountryID #intitialize number of countries for which you want calculate means
compute_mean <- list() ### initialize an empty list to collect the intermediate results in the loop
mean_results <- list() ### initialize an empty list to collect the finished results 

for (i in 1:length(CountryNO)){
  compute_mean[[i]] = filter(raw_welcomeDat_2, Country == CountryNO[[i]])
   mean_results[[i]]        = colMeans(select(compute_mean[[i]], -c(Country:YEAR_CALENDAR)),na.rm = T)  # remove columns Country to YEAR_CALENDAR (we don't need them in the loop because they are characters and not numerics and we cant compute the means for them)

}

###check your results if you have all the means for all 144 countries
length(mean_results)
mean_results;
mean_results[1] ## check the first country means


##### assign our clean data to mean_results
clean_grplvl_welcomeDat=t(data.frame(mean_results));rownames(clean_grplvl_welcomeDat)=NULL;clean_grplvl_welcomeDat=data.frame(clean_grplvl_welcomeDat)

#clean_grplvl_welcomeDat = cbind(select(raw_welcomeDat, c(Country:YEAR_CALENDAR)),clean_grplvl_welcomeDat)
### ad country code and lables
clean_grplvl_welcomeDat = cbind(CountryInfo,clean_grplvl_welcomeDat)


### are we sure the means were computed correcly by the for loop? lets check manually
N = 202 ## where N = specific country data
country_data =filter(raw_welcomeDat_2, Country == N) # raw_welcomeDat_2 = reverse scores. you can also do for non reverse scores raw_welcomeDat
mean(country_data$Know_abt_Sci_Q1,na.rm = T) ## check for mean for Know_abt_Sci_Q1 or age 

#### so now we have our clean data of country level means with appropropriate lables. check it to be sure
head(clean_grplvl_welcomeDat)

###clean up environmental variables 

keep(clean_grplvl_welcomeDat) #shows you which variables will be removed
keep(clean_grplvl_welcomeDat, sure = TRUE) # setting sure to TRUE removes variables show earlier

## now we have our cleaned data
### Jais already computed the HDI, mean schooling, etc. lets add them to the data set

setwd("~/Dropbox/Projects/Jena/vaccine/vaccine//data/data_Jais/") ## read in Jais'data

raw_welcomeDat = read_csv2("raw_datawlcme.csv")  ## read/load in raw data

WelcomeData <- read_csv2("wellcome_mikky_parentsonly.csv")

WelcomeData = WelcomeData[-c(145:151),] ## remove rows without country data

## add bind the HDI and Meanschooling to clean_data
clean_grplvl_welcomeDat=add_column(clean_grplvl_welcomeDat, HDI_2018 = WelcomeData$HDI_2018[order(WelcomeData$Countnum)], .after = "Country")
clean_grplvl_welcomeDat=add_column(clean_grplvl_welcomeDat, Mean_Schooling_2018 = WelcomeData$Mean_Schooling_2018[order(WelcomeData$Countnum)], .after = "HDI_2018")


####so now we have our cleaned dataset--lets save it

setwd("~/Dropbox/Projects/Jena/vaccine/vaccine//data/")

write.csv(clean_grplvl_welcomeDat, "clean_grplvl_welcomeDat.csv", quote=FALSE, row.names=FALSE) #

write_sav(clean_grplvl_welcomeDat, "clean_grplvl_welcomeDat.sav") #


### explore some correlations

cor.test(clean_grplvl_welcomeDat$Accept_Vaccn_4Child_Q28,clean_grplvl_welcomeDat$Vaccn_Knowledge_Heard_Vaccn_Q23,use = "complete.obs")


