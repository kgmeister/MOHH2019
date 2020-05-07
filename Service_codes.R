getwd()
setwd("C:/Users/guan0337f/Desktop/PA_costings")
library(readr)
test <- read.csv("service_codes_cost_NHGP_v2.csv")
service_sub_codes <- NHGPFY18_clean3[,c("Service.Sub.Code","Service.Quantity..All.")]
service_codes <- NHGPFY18_clean3[,c("Service.Code","Service.Quantity..All.")]
View(test)

