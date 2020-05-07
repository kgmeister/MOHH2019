setwd("C:/Users/guan0337f/Desktop/PA_costings")
SHPFY18_old<-read.csv("SHPFY18 for PA.csv")
library(dplyr)
library(data.table)

#Filtering out for BEDOK ONLY:
SHPFY18_old<-subset(SHPFY18_old, SHPFY18_old$Clinic=="BP")


#Bedok results summary
BDK_results<-read.csv("Copy 11 BDK Results Summary_v1.csv")
BDK_results<-BDK_results[1:1782,]

#Renaming the Service codes column in Bedok summary
setnames(BDK_results, "Service.Codes", "Compound.svs.code.for.PA")

#==========================================================================================================#


#-------Removing special tagging/benchmark objects------#

SHPFY18_original<- SHPFY18_old[!SHPFY18_old$Special.tagging== "Community Mental Health",] 
SHPFY18_original<- SHPFY18_original[!SHPFY18_original$Special.tagging== "Memory Clinic",] 
SHPFY18_original<- SHPFY18_original[!SHPFY18_original$Special.tagging== "Immunisation & Vaccinations",] 
SHPFY18_original<- SHPFY18_original[!SHPFY18_original$Special.tagging== "Smoking cessation",] 
SHPFY18_clean_special_tagging<- SHPFY18_original[!SHPFY18_original$Special.tagging== "CSS",]


############ Clean2: Removing non-subsidised, "Y" indicated rows in orange-shaded column, and pilot programs ###############

#SHPFY18_clean_special_tagging <- SHPFY18_clean_special_tagging[!SHPFY18_clean_special_tagging$Orange.shaded..per.MOH.== "Y",]
SHPFY18_clean_special_tagging <- SHPFY18_clean_special_tagging[SHPFY18_clean_special_tagging$Subsidised. == "Yes",]
SHPFY18_clean2<- SHPFY18_clean_special_tagging[!SHPFY18_clean_special_tagging$Pilot== "Y",]

#Checkpoint 2: Post-removal of Benchmark objects + non-subsidised, pilot programs, and "Y" in Orange-shaded:
#write.csv(SHPFY18_clean2, file= "SHPFY18_clean2.csv")


#==================Removing rows containing (a)-(g) in the exception list from rows labelled "Well"======================#
#library(dplyr)

#------------Subsetting "Well" and removing (a)-(g) in it----------------------#
SHPFY18_clean_well <- subset(SHPFY18_clean2, SHPFY18_clean2$ACWO.Tagging=="Well" 
                             & !SHPFY18_clean2$Special.tagging=="Immunisation & Vaccinations"
                             & !SHPFY18_clean2$Special.tagging=="Child Development Screening"
                             & !SHPFY18_clean2$Special.tagging=="CSS"
                             & !SHPFY18_clean2$Special.tagging=="BSS"
                             & !SHPFY18_clean2$Special.tagging=="Smoking cessation")


#-----------------------------------Removing "Well" from clean2------------------------------------------#
SHPFY18_no_well <- subset(SHPFY18_clean2, !SHPFY18_clean2$ACWO.Tagging=="Well")



############################ Clean3: Combining no_well and clean_well to make dataframe with cleaned "Well" ###############################

SHPFY18_clean3 <- rbind(SHPFY18_no_well, SHPFY18_clean_well)

########################################################################################################

#Checkpoint 3: Removed Benchmark objects, all non-subsidised and pilot programs, "Y" indicated rows in Orange-shaded column,
#and cleaned "Well" funding objects.
#write.csv (SHPFY18_clean3, file="SHPFY18_clean3.csv")

#Extracting unique Visit codes for all General Clinics along with their NAs
SHPFY18_only_general_clinic <- subset(SHPFY18_clean3,SHPFY18_clean3$Service.code.category..Cost.object.category=="Doctor Consult" 
                                      & SHPFY18_clean3$Special.tagging=="N.A.")


###################### Checkpoint 4: Obtaining ALL patients who have at least gone for General Consult ####################################

#---------------------------------------------------------------------------------------------------------------#
# Obtaining the IDs of ALL patients who have at least went through General Clinic:
SHPFY18_all_general_clinic <- subset(SHPFY18_clean3, SHPFY18_clean3$Case.Number %in% SHPFY18_only_general_clinic$Case.Number)

#Splitting the all_general_clinic dataframe into non-drugs and drug:
SHPFY18_general_clinic_drug <- subset(SHPFY18_all_general_clinic, SHPFY18_all_general_clinic$Service.code.category..Cost.object.category=="Drugs")
SHPFY18_general_clinic_no_drug <- subset(SHPFY18_all_general_clinic, !SHPFY18_all_general_clinic$Service.code.category..Cost.object.category=="Drugs")


#Splitting the general clinic non-drugs into Chronic simple, moderate and complex:
SHPFY18_general_clinic_chronic_simple <- subset(SHPFY18_general_clinic_no_drug, SHPFY18_general_clinic_no_drug$Service.Code.Category.as.per.Service.level=="Chronic"
                                                & SHPFY18_general_clinic_no_drug$Visit.Chronic.Category=="Simple")

SHPFY18_general_clinic_chronic_moderate <- subset(SHPFY18_general_clinic_no_drug, SHPFY18_general_clinic_no_drug$Service.Code.Category.as.per.Service.level=="Chronic"
                                                  & SHPFY18_general_clinic_no_drug$Visit.Chronic.Category=="Moderate")

SHPFY18_general_clinic_chronic_complex <- subset(SHPFY18_general_clinic_no_drug, SHPFY18_general_clinic_no_drug$Service.Code.Category.as.per.Service.level=="Chronic"
                                                 & SHPFY18_general_clinic_no_drug$Visit.Chronic.Category=="Complex")


##Splitting the general clinic drugs into Chronic simple, moderate and complex: 
SHPFY18_general_clinic_drug_chronic_simple <- subset(SHPFY18_general_clinic_drug, SHPFY18_general_clinic_drug$Service.Code.Category.as.per.Service.level=="Chronic"
                                                     & SHPFY18_general_clinic_drug$Visit.Chronic.Category=="Simple")

SHPFY18_general_clinic_drug_chronic_moderate <- subset(SHPFY18_general_clinic_drug, SHPFY18_general_clinic_drug$Service.Code.Category.as.per.Service.level=="Chronic"
                                                       & SHPFY18_general_clinic_drug$Visit.Chronic.Category=="Moderate")

SHPFY18_general_clinic_drug_chronic_complex <- subset(SHPFY18_general_clinic_drug, SHPFY18_general_clinic_drug$Service.Code.Category.as.per.Service.level=="Chronic"
                                                      & SHPFY18_general_clinic_drug$Visit.Chronic.Category=="Complex")

#write.csv (SHPFY18_general_clinic_chronic_simple, file="SHPFY18_general_clinic_chronic_simple.csv")
#write.csv (SHPFY18_general_clinic_chronic_moderate, file="SHPFY18_general_clinic_chronic_moderate.csv")
#write.csv (SHPFY18_general_clinic_chronic_complex, file="SHPFY18_general_clinic_chronic_complex.csv")
#write.csv (SHPFY18_general_clinic_drug_chronic_simple, file="SHPFY18_general_clinic_drug_chronic_simple.csv")
#write.csv (SHPFY18_general_clinic_drug_chronic_moderate, file="SHPFY18_general_clinic_drug_chronic_moderate.csv")
#write.csv (SHPFY18_general_clinic_drug_chronic_complex, file="SHPFY18_general_clinic_drug_chronic_complex.csv")


##############################################################################################################################

#Subsetting using service codes of all patients who are in NO DRUGS general clinic from Bedok summary:
SHPFY18_GC_chronic_simple_service_code <- subset(SHPFY18_general_clinic_chronic_simple, SHPFY18_general_clinic_chronic_simple$Compound.svs.code.for.PA %in% BDK_results$Service.Codes)
SHPFY18_GC_chronic_moderate_service_code <- subset(SHPFY18_general_clinic_chronic_moderate, SHPFY18_general_clinic_chronic_moderate$Compound.svs.code.for.PA %in% BDK_results$Service.Codes)
SHPFY18_GC_chronic_complex_service_code <- subset(SHPFY18_general_clinic_chronic_complex, SHPFY18_general_clinic_chronic_complex$Compound.svs.code.for.PA %in% BDK_results$Service.Codes)


#Subsetting using service codes of all patients who are in DRUGS general clinic from Bedok summary:
SHPFY18_GC_drug_chronic_simple_service_code <- subset(SHPFY18_general_clinic_drug_chronic_simple, SHPFY18_general_clinic_drug_chronic_simple$Compound.svs.code.for.PA %in% BDK_results$Service.Codes)
SHPFY18_GC_drug_chronic_moderate_service_code <- subset(SHPFY18_general_clinic_drug_chronic_moderate, SHPFY18_general_clinic_drug_chronic_moderate$Compound.svs.code.for.PA %in% BDK_results$Service.Codes)
SHPFY18_GC_drug_chronic_complex_service_code <- subset(SHPFY18_general_clinic_drug_chronic_complex, SHPFY18_general_clinic_drug_chronic_complex$Compound.svs.code.for.PA %in% BDK_results$Service.Codes)

View(SHPFY18_GC_drug_chronic_simple_service_code)

#GC no. of attendees, no drug:
SHPFY18_GC_chronic_simple_total_number <- length(unique(SHPFY18_GC_chronic_simple_service_code$Case.Number))
SHPFY18_GC_chronic_moderate_total_number <- length(unique(SHPFY18_GC_chronic_moderate_service_code$Case.Number))
SHPFY18_GC_chronic_complex_total_number <- length(unique(SHPFY18_GC_chronic_complex_service_code$Case.Number))

#GC no. of attendees, WITH drug:
SHPFY18_GC_drug_chronic_simple_total_number <- length(unique(SHPFY18_GC_drug_chronic_simple_service_code$Case.Number))
SHPFY18_GC_drug_chronic_moderate_total_number <- length(unique(SHPFY18_GC_drug_chronic_moderate_service_code$Case.Number))
SHPFY18_GC_drug_chronic_complex_total_number <- length(unique(SHPFY18_GC_drug_chronic_complex_service_code$Case.Number))

SHPFY18_GC_chronic_total<- c(SHPFY18_GC_chronic_simple_total_number,SHPFY18_GC_chronic_moderate_total_number,
                     SHPFY18_GC_chronic_complex_total_number,SHPFY18_GC_drug_chronic_simple_total_number,
                     SHPFY18_GC_drug_chronic_moderate_total_number, SHPFY18_GC_chronic_complex_total_number)

########################################################################################################################

#-------------------------------------- Filtering for Family Physician --------------------------------------------#

#Obtaining unique IDs of patients who have gone for Family Consult:
SHPFY18_only_FP <- subset(SHPFY18_clean3,SHPFY18_clean3$Service.code.category..Cost.object.category=="FP Consult" 
                                      & SHPFY18_clean3$Special.tagging=="N.A.")

#Obtaining the IDs of ALL patients who have at least went through Family Consult:
SHPFY18_all_FP <- subset(SHPFY18_clean3, SHPFY18_clean3$Case.Number %in% SHPFY18_only_FP$Case.Number)


#Splitting the SHPFY18_all_FP dataframe into non-drugs and drug:
SHPFY18_FP_drug <- subset(SHPFY18_all_FP, SHPFY18_all_FP$Service.code.category..Cost.object.category=="Drugs")
SHPFY18_FP_no_drug <- subset(SHPFY18_all_FP, !SHPFY18_all_FP$Service.code.category..Cost.object.category=="Drugs")


#Subsetting using service codes of all patients who are in NO DRUGS Family Consult from Bedok summary:
SHPFY18_FP_service_code <- subset(SHPFY18_FP_no_drug, SHPFY18_FP_no_drug$Compound.svs.code.for.PA %in% BDK_results$Service.Codes)

#Subsetting using service codes of all patients who have drugs in Family Consult from Bedok summary:
SHPFY18_FP_drug_service_code <- subset(SHPFY18_FP_drug, SHPFY18_FP_drug$Compound.svs.code.for.PA %in% BDK_results$Service.Codes)

#FP number of attendees, no drug:
SHPFY18_FP_total_number <- length(unique(SHPFY18_FP_service_code$Case.Number))

#FP number of attendees, WITH drug:
SHPFY18_FP_drug_total_number <- length(unique(SHPFY18_FP_drug_service_code$Case.Number))

SHPFY18_FP_total <- c(SHPFY18_FP_total_number,SHPFY18_FP_drug_total_number)


#Merging Family Physician no drug with Bedok summary:
SHPFY18_FP_merged <- merge(SHPFY18_FP_service_code,BDK_results, 
                           by = "Compound.svs.code.for.PA", all.SHPFY18_FP_service_code = TRUE)
SHPFY18_FP_merged <- subset(SHPFY18_FP_merged, select= -c(2:34))


#Finding average costs from columns "Family Physician GDFM" to "Other cost-Facilities Related"
SHPFY18_FP_merged_cost <- subset(SHPFY18_FP_merged, select = c(5:160))
colMeans(SHPFY18_FP_merged_cost)

View(SHPFY18_FP_merged_cost)
