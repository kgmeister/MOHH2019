setwd("C:/Users/guan0337f/Desktop/PA_costings")
NHGPFY18_old<-read.csv("NHGPFY18 for PA.csv")
NHGPFY18_new_service_grouping <- read.csv("Copy of NHGP NUP Service Grouping Costing FY2018_ as at 20 Jan.csv")
NHGPFY18_service_grouping_update <- read.csv("Service code grouping update.csv")

### Performing the merge-and-replace #######
NHGPFY18_old[c(NHGPFY18_old$Service.Sub.Code,NHGPFY18_old$Service.Category.Desc,
                NHGPFY18_old$Subsidised., NHGPFY18_old$NHGP.NUP.ACWO,
                NHGPFY18_old$NHGP.NUP.Service.Code.Category, NHGPFY18_old$NHGP.NUP.Special.Category,
                NHGPFY18_old$Pilot, NHGPFY18_old$Orange.shaded..per.MOH.),]<- 
  NHGPFY18_new_service_grouping[
                  c(NHGPFY18_new_service_grouping$Service.Sub.Code, NHGPFY18_new_service_grouping$Service.Sub.Code.Desc,
                     NHGPFY18_new_service_grouping$Subsidised., NHGPFY18_new_service_grouping$NHGP.NUP.ACWO,
                     NHGPFY18_new_service_grouping$NHGP.NUP.Service.Code.Category, NHGPFY18_new_service_grouping$NHGP.NUP.Special.Category,
                     NHGPFY18_new_service_grouping$Pilot, NHGPFY18_new_service_grouping$Orange.shaded..per.MOH.),] 

library(tidyverse)
library(data.table)

#View(NHGPFY18_original)
#Checkpoint 1; this was done with ONLY the removal of non-subsidised, pilot programs, and "Y" in Orange-shaded:
#write.csv(NHGPFY18_clean, file= "NHGPFY18_clean.csv")

#str(NHGPFY18_original) 
#View(NHGPFY18_clean_special_tagging)


#==========================================================================================================#


#-------Removing special tagging/benchmark objects------#

NHGPFY18_original<- NHGPFY18_old[!NHGPFY18_old$NHGP.NUP.Special.Category== "Community Mental Health",] 
NHGPFY18_original<- NHGPFY18_original[!NHGPFY18_original$NHGP.NUP.Special.Category== "Memory Clinic",] 
NHGPFY18_original<- NHGPFY18_original[!NHGPFY18_original$NHGP.NUP.Special.Category== "Immunisation & Vaccinations",] 
NHGPFY18_original<- NHGPFY18_original[!NHGPFY18_original$NHGP.NUP.Special.Category== "Smoking cessation",] 
NHGPFY18_clean_special_tagging<- NHGPFY18_original[!NHGPFY18_original$NHGP.NUP.Special.Category== "CSS",]


########### Clean2: Removing non-subsidised, "Y" indicated rows in orange-shaded column, and pilot programs ############

NHGPFY18_clean_special_tagging <- NHGPFY18_clean_special_tagging[!NHGPFY18_clean_special_tagging$Orange.shaded..per.MOH.== "Y",]
NHGPFY18_clean_special_tagging <- NHGPFY18_clean_special_tagging[NHGPFY18_clean_special_tagging$Subsidised. == "Yes",]
NHGPFY18_clean2<- NHGPFY18_clean_special_tagging[!NHGPFY18_clean_special_tagging$Pilot== "Y",]

#Checkpoint 2: Post-removal of Benchmark objects + non-subsidised, pilot programs, and "Y" in Orange-shaded:
#write.csv(NHGPFY18_clean2, file= "NHGPFY18_clean2.csv")


#==================Removing rows containing (a)-(g) in the exception list from rows labelled "Well"======================#
#library(dplyr)

#------------Subsetting "Well" and removing (a)-(g) in it----------------------#
NHGPFY18_clean_well <- subset(NHGPFY18_clean2, NHGPFY18_clean2$NHGP.NUP.ACWO=="Well" 
                          & !NHGPFY18_clean2$NHGP.NUP.Special.Category=="Medical Social Services" 
                          & !NHGPFY18_clean2$NHGP.NUP.Special.Category=="Immunisation & Vaccinations"
                          & !NHGPFY18_clean2$NHGP.NUP.Special.Category=="Antenatal Care"
                          & !NHGPFY18_clean2$NHGP.NUP.Special.Category=="Child Development Screening"
                          & !NHGPFY18_clean2$NHGP.NUP.Special.Category=="CSS"
                          & !NHGPFY18_clean2$NHGP.NUP.Special.Category=="BSS"
                          & !NHGPFY18_clean2$NHGP.NUP.Special.Category=="Smoking Cessation"
                          )

#-----------------------------------Removing "Well" from clean2------------------------------------------#
NHGPFY18_no_well <- subset(NHGPFY18_clean2, !NHGPFY18_clean2$NHGP.NUP.ACWO=="Well")



########### Clean3: Combining no_well and clean_well to make dataframe with cleaned "Well"#############

NHGPFY18_clean3 <- rbind(NHGPFY18_no_well, NHGPFY18_clean_well)


#Checkpoint 3: Removed Benchmark objects, all non-subsidised and pilot programs, "Y" indicated rows in Orange-shaded column,
#and cleaned "Well" funding objects.
#write.csv (NHGPFY18_clean3, file="NHGPFY18_clean3.csv")

#Extracting unique Visit codes for all General Clinics along with their NAs
NHGPFY18_only_general_clinic <- subset(NHGPFY18_clean3,NHGPFY18_clean3$NHGP.NUP.Service.Code.Category=="Doctor Consult" 
                                       & NHGPFY18_clean3$NHGP.NUP.Special.Category=="N.A.")


###################### Checkpoint 4: Obtaining ALL patients who have at least gone for General Consult ####################################

#---------------------------------------------------------------------------------------------------------------#
# Obtaining the IDs of ALL patients who have at least went through General Clinic:
NHGPFY18_all_general_clinic <- subset(NHGPFY18_clean3, NHGPFY18_clean3$Visit.Code %in% NHGPFY18_only_general_clinic$Visit.Code)

#Splitting the all_general_clinic dataframe into non-drugs and drug:
NHGPFY18_general_clinic_drug <- subset(NHGPFY18_all_general_clinic, NHGPFY18_all_general_clinic$NHGP.NUP.Service.Code.Category=="Drugs")
NHGPFY18_general_clinic_no_drug <- subset(NHGPFY18_all_general_clinic, !NHGPFY18_all_general_clinic$NHGP.NUP.Service.Code.Category=="Drugs")

#Splitting the general clinic non-drugs into Chronic simple, moderate and complex:
NHGPFY18_general_clinic_chronic_simple <- subset(NHGPFY18_general_clinic_no_drug, NHGPFY18_general_clinic_no_drug$Workload.Category=="Chronic"
                                                   & NHGPFY18_general_clinic_no_drug$Visit.Chronic.Category=="Simple")

NHGPFY18_general_clinic_chronic_moderate <- subset(NHGPFY18_general_clinic_no_drug, NHGPFY18_general_clinic_no_drug$Workload.Category=="Chronic"
                                                   & NHGPFY18_general_clinic_no_drug$Visit.Chronic.Category=="Moderate")

NHGPFY18_general_clinic_chronic_complex <- subset(NHGPFY18_general_clinic_no_drug, NHGPFY18_general_clinic_no_drug$Workload.Category=="Chronic"
                                                   & NHGPFY18_general_clinic_no_drug$Visit.Chronic.Category=="Complex")


##Splitting the general clinic drugs into Chronic simple, moderate and complex: 
NHGPFY18_general_clinic_drug_chronic_simple <- subset(NHGPFY18_general_clinic_drug, NHGPFY18_general_clinic_drug$Workload.Category=="Chronic"
                                                 & NHGPFY18_general_clinic_drug$Visit.Chronic.Category=="Simple")

NHGPFY18_general_clinic_drug_chronic_moderate <- subset(NHGPFY18_general_clinic_drug, NHGPFY18_general_clinic_drug$Workload.Category=="Chronic"
                                                      & NHGPFY18_general_clinic_drug$Visit.Chronic.Category=="Moderate")

NHGPFY18_general_clinic_drug_chronic_complex <- subset(NHGPFY18_general_clinic_drug, NHGPFY18_general_clinic_drug$Workload.Category=="Chronic"
                                                        & NHGPFY18_general_clinic_drug$Visit.Chronic.Category=="Complex")

#write.csv (NHGPFY18_general_clinic_chronic_simple, file="NHGPFY18_general_clinic_chronic_simple.csv")
#write.csv (NHGPFY18_general_clinic_chronic_moderate, file="NHGPFY18_general_clinic_chronic_moderate.csv")
#write.csv (NHGPFY18_general_clinic_chronic_complex, file="NHGPFY18_general_clinic_chronic_complex.csv")
#write.csv (NHGPFY18_general_clinic_drug_chronic_simple, file="NHGPFY18_general_clinic_drug_chronic_simple.csv")
#write.csv (NHGPFY18_general_clinic_drug_chronic_moderate, file="NHGPFY18_general_clinic_drug_chronic_moderate.csv")
#write.csv (NHGPFY18_general_clinic_drug_chronic_complex, file="NHGPFY18_general_clinic_drug_chronic_complex.csv")


#================================================================================================================#
#Extracting total cost for each ID associated:
totalcost_NHGPFY18_general_clinic_chronic_simple <- NHGPFY18_general_clinic_chronic_simple %>%
  group_by(Visit.Code) %>% 
  transmute(Total_cost=sum(Amount.Payable)) %>%
  distinct(Visit.Code, .keep_all = TRUE)

totalcost_NHGPFY18_general_clinic_chronic_moderate <- NHGPFY18_general_clinic_chronic_moderate %>%
  group_by(Visit.Code) %>% 
  transmute(Total_cost=sum(Amount.Payable)) %>%
  distinct(Visit.Code, .keep_all = TRUE)

totalcost_NHGPFY18_general_clinic_chronic_complex <- NHGPFY18_general_clinic_chronic_complex %>%
  group_by(Visit.Code) %>% 
  transmute(Total_cost=sum(Amount.Payable)) %>%
  distinct(Visit.Code, .keep_all = TRUE)

totalcost_NHGPFY18_general_clinic_drug_chronic_simple <- NHGPFY18_general_clinic_drug_chronic_simple %>%
  group_by(Visit.Code) %>% 
  transmute(Total_cost=sum(Amount.Payable)) %>%
  distinct(Visit.Code, .keep_all = TRUE)

totalcost_NHGPFY18_general_clinic_drug_chronic_moderate <- NHGPFY18_general_clinic_drug_chronic_moderate %>%
  group_by(Visit.Code) %>% 
  transmute(Total_cost=sum(Amount.Payable)) %>%
  distinct(Visit.Code, .keep_all = TRUE)

totalcost_NHGPFY18_general_clinic_drug_chronic_complex <- NHGPFY18_general_clinic_drug_chronic_complex %>%
  group_by(Visit.Code) %>% 
  transmute(Total_cost=sum(Amount.Payable)) %>%
  distinct(Visit.Code, .keep_all = TRUE)

#write.csv (totalcost_NHGPFY18_general_clinic_chronic_simple, file="totalcost_NHGPFY18_general_clinic_chronic_simple.csv")
#write.csv (totalcost_NHGPFY18_general_clinic_chronic_moderate, file="totalcost_NHGPFY18_general_clinic_chronic_moderate.csv")
#write.csv (totalcost_NHGPFY18_general_clinic_chronic_complex, file="totalcost_NHGPFY18_general_clinic_chronic_complex.csv")
#write.csv (totalcost_NHGPFY18_general_clinic_drug_chronic_simple, file="totalcost_NHGPFY18_general_clinic_drug_chronic_simple.csv")
#write.csv (totalcost_NHGPFY18_general_clinic_drug_chronic_moderate, file="totalcost_NHGPFY18_general_clinic_drug_chronic_moderate.csv")
#write.csv (totalcost_NHGPFY18_general_clinic_drug_chronic_complex, file="totalcost_NHGPFY18_general_clinic_drug_chronic_complex.csv")


###################################### Family Consult ##############################################

#Extracting unique visit codes of all patients who went through Family Consult:
NHGPFY18_only_family_consult <- subset(NHGPFY18_clean3,NHGPFY18_clean3$NHGP.NUP.Service.Code.Category=="FP Consult")

# Obtaining the IDs of ALL patients who have at least went through Family consult:
NHGPFY18_all_family_consult <- subset(NHGPFY18_clean3, NHGPFY18_clean3$Visit.Code %in% NHGPFY18_only_family_consult$Visit.Code)

#Splitting the all_general_clinic dataframe into non-drugs and drug:
NHGPFY18_family_consult_drug <- subset(NHGPFY18_all_family_consult, NHGPFY18_all_family_consult$NHGP.NUP.Service.Code.Category=="Drugs")
NHGPFY18_family_consult_no_drug <- subset(NHGPFY18_all_family_consult, !NHGPFY18_all_family_consult$NHGP.NUP.Service.Code.Category=="Drugs")

#write.csv(NHGPFY18_family_consult_drug,file="NHGPFY18_family_consult_drug.csv")
#write.csv(NHGPFY18_family_consult_no_drug,file="NHGPFY18_family_consult_no_drug.csv")

#Extracting total cost of each associated ID:
totalcost_NHGPFY18_family_consult_drug <- NHGPFY18_family_consult_drug %>%
  group_by(Visit.Code) %>% 
  transmute(Total_cost=sum(Amount.Payable)) %>%
  distinct(Visit.Code, .keep_all = TRUE)


################################################# APN Consult #######################################################

#Extracting unique visit codes of all patients who went through Family Consult:
NHGPFY18_only_apn_consult<- subset(NHGPFY18_clean3,NHGPFY18_clean3$NHGP.NUP.Special.Category=="APN")

# Obtaining the IDs of ALL patients who have at least went through Family consult:
NHGPFY18_all_apn_consult <- subset(NHGPFY18_clean3, NHGPFY18_clean3$Visit.Code %in% NHGPFY18_only_apn_consult$Visit.Code)

#Splitting the all_general_clinic dataframe into non-drugs and drug:
NHGPFY18_all_apn_consult_drug <- subset(NHGPFY18_all_apn_consult, NHGPFY18_all_apn_consult$NHGP.NUP.Service.Code.Category=="Drugs")
NHGPFY18_all_apn_consult_no_drug <- subset(NHGPFY18_all_apn_consult, !NHGPFY18_all_apn_consult$NHGP.NUP.Service.Code.Category=="Drugs")

#write.csv(NHGPFY18_all_apn_consult_drug,file="NHGPFY18_all_apn_consult_drug.csv")
#write.csv(NHGPFY18_all_apn_consult_no_drug,file="NHGPFY18_all_apn_consult_no_drug.csv")

###################################################################################################################

#Extracting unique visit codes of all patients who went through Nursing:
NHGPFY18_only_nursing<- subset(NHGPFY18_clean3,NHGPFY18_clean3$NHGP.NUP.Service.Code.Category=="Nursing")

# Obtaining the IDs of ALL patients who have at least went through Family consult:
NHGPFY18_all_nursing <- subset(NHGPFY18_clean3, NHGPFY18_clean3$Visit.Code %in% NHGPFY18_only_nursing$Visit.Code)

#Removing APN from Nursing dataframe:
NHGPFY18_all_nursing_clean<- subset(NHGPFY18_all_nursing,!NHGPFY18_all_nursing$NHGP.NUP.Special.Category =="APN")



