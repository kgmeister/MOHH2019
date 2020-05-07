############ Extracting dataframes from funding objects ###########################

#Physiotherapy
Physiotherapy <- subset(NHGPFY18_clean3, NHGPFY18_clean3$NHGP.NUP.Service.Code.Category=="Physiotherapy")

#Antenatal care
Antenatal_care <- subset(NHGPFY18_clean3, NHGPFY18_clean3$NHGP.NUP.Special.Category=="Antenatal care")

#DOT
DOT <- subset(NHGPFY18_clean3, NHGPFY18_clean3$NHGP.NUP.Special.Category=="Directly Observed Therapy")

#Minor surgical procedures
Minor_surgical_procedures <- subset(NHGPFY18_clean3, NHGPFY18_clean3$NHGP.NUP.Special.Category=="Minor Surgical Procedure")

#Spirometry
Spirometry <- subset(NHGPFY18_clean3, NHGPFY18_clean3$NHGP.NUP.Service.Code.Category=="Spirometry")

#Child Developmental Screening
Child_developmental_screening <- subset(NHGPFY18_clean3, NHGPFY18_clean3$NHGP.NUP.Special.Category=="Child Development Screening")

#Podiatry
Podiatry <- subset(NHGPFY18_clean3, NHGPFY18_clean3$NHGP.NUP.Service.Code.Category=="Podiatry")

#P5 Screening
P5_screening <- subset(NHGPFY18_clean3, NHGPFY18_clean3$NHGP.NUP.Special.Category=="P5 Screening")

#=========================================================#




#general_clinic_radio <-subset(NHGPFY18_clean3, NHGPFY18_clean3$Visit.Code %in% radiology_visit_code)
#View(general_clinic_radio)
