
##***************************(@MEFWRIT)**************************##
##***************************************************************##  
##   This is the Vaccn project - a tentative PNAS paper            ##
##  Fiagbenu & Troia, 2021 :-)
## This script begins the analysis proper
## 
## 19Nov 2016 ###@Michael Edem Fiagbenu wrote it (@MEFWRIT)
##                           ##
##***************************(@MEFWRIT)***************************##
##****************************************************************##  


  ######### let's get the ball rolling
## We start by loading some packages and reading in the raw wlcome datasets 


########## load packages ############
rm(list = ls()) # clear workspace
set.seed(282018) ## for replication
cat("\f") # cat("\014") clear console
x<-c("data.table","psych", "haven","tidyverse","naniar", "tibble","janitor","foreign","readxl","gdata","mlma")  ## load packages
lapply(x, require, character.only = TRUE)
setwd("~/Dropbox/Projects/Jena/vaccine/vaccine/data/")
source("corstarsl.R")   #### for correlation table 


colMax <- function(data) sapply(data, max, na.rm = T)  ## find the max value in each column
colMin <- function(data) sapply(data, min, na.rm = T)  ## find the min value in each column



## read/load in the clean data
WelcomeData = read_csv("clean_grplvl_welcomeDat.csv")  
#WelcomeData = read_csv("clean_indlvel_welcomeDat.csv")  

glimpse(WelcomeData)
table(WelcomeData$Disease_Sci_Q3)

## now lets select some variables of interest and explore some correlations
names(WelcomeData)

WelcomeData_select <- subset(WelcomeData, select = c(Religion_D1,HDI_2018,Mean_Schooling_2018,SciRelig_disag_Q29,Sci_Relig_choose_Q30,Disease_Sci_Q3,Trust_UniScientists,
                                                     Education,Science_Education,Science_Literacy, Trust_Science_Q12,WGM_Index,Trust_CompnyScientists,
                                                     Vaccn_RiskPercep_Q25,Vaccn_Knowledge_Effktv_Q26,Vaccn_Knowledge_Heard_Vaccn_Q23,
                                                     Vaccn_Knowledge_Child_Q24,
                                                     Accept_Vaccn_4Child_Q28 ))


###### Fears average #########
Fears_Ratings.keys.list = make.keys(WelcomeData_select, list( Sci_Relig_choose_Q30 = "Sci_Relig_choose_Q30",Religiosity = "Religion_D1",HDI_2018 ="HDI_2018",Mean_Schooling_2018 ="Mean_Schooling_2018",
                                                        Education = "Education",Science_Education="Science_Education",Science_Literacy="Science_Literacy",Disease_Sci_Q3 ="Disease_Sci_Q3",
                                                        WGM_Index ="WGM_Index",Trust_UniScientists = "Trust_UniScientists",Trust_CompnyScientists= "Trust_CompnyScientists", Trust_Science_Q12="Trust_Science_Q12",
                                                        Vaccn_RiskPercep_Q25="Vaccn_RiskPercep_Q25",Vaccn_Knowledge_Effktv_Q26="Vaccn_Knowledge_Effktv_Q26",
                                                        Vaccn_Knowledge_Heard_Vaccn_Q23 ="Vaccn_Knowledge_Heard_Vaccn_Q23",
                                                        Vaccn_Knowledge_Child_Q24="Vaccn_Knowledge_Child_Q24",
                                                        
                                                        Accept_Vaccn_4Child_Q28 = "Accept_Vaccn_4Child_Q28"
                                                        
                                                        ))
Fears_scoresItem    = scoreItems(Fears_Ratings.keys.list, min = 1, max = 4, WelcomeData_select);  #print(BF.IdeologyData, short = FALSE)            = data.frame(TM_Scaledata_scoresItem$scores)
Fears_scores        = data.frame(Fears_scoresItem$scores)

# compute correlations and means
corr_Fears_scores       = corstarsl(Fears_scores)
mean_Fears_scores       = describeBy(Fears_scoresItem$scores)
alpha_Fears_scores      = data.frame(Fears_scoresItem$alpha)
gg_Fears = rbind(alpha_Fears_scores, t(mean_Fears_scores))
qq_Fears=cbind(corr_Fears_scores, data.frame(t(Fears_scoresItem$alpha)), mean_Fears_scores$mean, mean_Fears_scores$sd )
names(qq_Fears)[names(qq_Fears) == c("mean_Fears_scores$mean")] <- c('mean')
names(qq_Fears)[names(qq_Fears) == c("mean_Fears_scores$sd")]   <- c( "sd")
qq_Fears$N = nrow(Fears_scores)




### explore some descript stats



