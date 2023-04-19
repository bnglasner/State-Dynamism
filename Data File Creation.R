# Ben Glasner

##########################
#         Set Up         #  
##########################
library(tidycensus)
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

#################
### Set paths ###
#################
if(Sys.info()[["user"]]=="bglasner"){
  # Root folder
  path_project <- "C:/Users/bglasner/Dropbox/GitHub/State-Dynamism"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox/GitHub/State-Dynamism"
}
if(Sys.info()[["user"]]=="Benjamin Glasner"){
  # Root folder
  path_project <- "C:/Users/Benjamin Glasner/Dropbox/GitHub/State-Dynamism"
}

setwd(path_project)

# Source data can be provided on request if it meets EIGs standards for data sharing

# #####################
# ### Load Dynamism ###
# #####################
# 
# dynamism_wide <- read_excel("Index of State Dynamism.xlsx")
# dynamism_wide <- dynamism_wide %>% janitor::clean_names() 
# dynamism <- gather(dynamism_wide, year, Dyanimsm, x1992:x2020, factor_key=TRUE)
# dynamism$year <- as.numeric(as.character(stringr::str_sub(dynamism$year, 2)))
# dynamism <- dynamism %>% rename("state_name" = "state")
# #################################
# ### FIPS State Crosswalk      ###
# #################################
# state_crosswalk <- tidycensus::fips_codes %>% 
#   select(state,state_code,state_name) %>% 
#   distinct() %>% 
#   rename("fipstate" = "state_code") %>% 
#   mutate(fipstate = as.numeric(as.character(fipstate)))
# 
# #####################
# ### Load CBP      ###
# #####################
# setwd(paste0(path_project,"/CBP"))
# files <- list.files(pattern="cbp*")
# myfiles <- lapply(files, read.csv)
# 
# Years <- c(1992:2020)
# for (i in c(1:10)) {
#   j <- i + 8
#   myfiles[[i]] <- myfiles[[i]] %>% mutate(emp_firm_more_50 = e50_99 + e100_249 + e250_499 + e500_999 + e1000,
#                                           emp_firm_less_50 = e1_4 + e5_9 + e10_19 + e20_49,
#                                           emp_small_firms = emp_firm_less_50/emp_firm_more_50)
#   
#   myfiles[[i]]$year <- Years[[j]]
#   myfiles[[i]] <- myfiles[[i]] %>% select(fipstate,year,naics,emp,ap,est,emp_firm_more_50,emp_firm_less_50, emp_small_firms)
#   
# }
# for (i in c(11:15,17)) {
#   j <- i + 8
#   myfiles[[i]] <- myfiles[[i]] %>% mutate(emp_firm_more_50 = e50_99 + e100_249 + e250_499 + e500_999 + e1000,
#                                           emp_firm_less_50 = e1_4 + e5_9 + e10_19 + e20_49,
#                                           emp_small_firms = emp_firm_less_50/emp_firm_more_50) %>% filter(lfo == "-")
#   
#   myfiles[[i]]$year <- Years[[j]]
#   myfiles[[i]] <- myfiles[[i]] %>% select(fipstate,year,naics,emp,ap,est,emp_firm_more_50,emp_firm_less_50, emp_small_firms)
#   
# }
# 
# for (i in 16) {
#   j <- i + 8
#   myfiles[[i]] <- myfiles[[i]] %>% mutate(emp_firm_more_50 = E50_99 + E100_249 + E250_499 + E500_999 + E1000,
#                                           emp_firm_less_50 = E1_4 + E5_9 + E10_19 + E20_49,
#                                           emp_small_firms = emp_firm_less_50/emp_firm_more_50) %>% filter(LFO == "-")
#   
#   myfiles[[i]]$year <- Years[[j]]  
#   myfiles[[i]] <- myfiles[[i]] %>% select(FIPSTATE,year,NAICS,EMP,AP,EST,emp_firm_more_50,emp_firm_less_50, emp_small_firms)
#   names(myfiles[[i]]) <- c("fipstate","year","naics","emp","ap","est","emp_firm_more_50","emp_firm_less_50", "emp_small_firms")
# }
# 
# for (i in 18:21) {
#   j <- i + 8
#   myfiles[[i]] <- myfiles[[i]] %>% mutate(emp_firm_more_50 = e50_99 + e100_249 + e250_499 + e500_999 + e1000,
#                                           emp_firm_less_50 = e.5 + e5_9 + e10_19 + e20_49,
#                                           emp_small_firms = emp_firm_less_50/emp_firm_more_50) %>% filter(lfo == "-")
#   
#   myfiles[[i]]$year <- Years[[j]]
#   myfiles[[i]] <- myfiles[[i]] %>% select(fipstate,year,naics,emp,ap,est,emp_firm_more_50,emp_firm_less_50, emp_small_firms)
# }
# 
# for (i in 22:27) {
#   j <- i -21
#   myfiles[[i]] <- myfiles[[i]] %>% mutate(emp_firm_more_50 = e50_99 + e100_249 + e250_499 + e500_999 + e1000,
#                                           emp_firm_less_50 = e1_4 + e5_9 + e10_19 + e20_49,
#                                           emp_small_firms = emp_firm_less_50/emp_firm_more_50) %>% 
#                                     filter(sic == "----") %>% 
#                                     mutate(naics = "------")
#   
#   myfiles[[i]]$year <- Years[[j]]
#   myfiles[[i]] <- myfiles[[i]] %>% select(fipstate,year,naics,emp,ap,est,emp_firm_more_50,emp_firm_less_50, emp_small_firms)
# }
# for (i in 28:29) {
#   j <- i -21
#   myfiles[[i]] <- myfiles[[i]] %>% mutate(emp_firm_more_50 = e50_99 + e100_249 + e250_499 + e500_999 + e1000,
#                                           emp_firm_less_50 = e1_4 + e5_9 + e10_19 + e20_49,
#                                           emp_small_firms = emp_firm_less_50/emp_firm_more_50)
#   
#   myfiles[[i]]$year <- Years[[j]]
#   myfiles[[i]] <- myfiles[[i]] %>% select(fipstate,year,naics,emp,ap,est,emp_firm_more_50,emp_firm_less_50, emp_small_firms)
# }
# 
# CBP <- do.call(rbind, myfiles)
# CBP$naics <- gsub('------','00',CBP$naics)
# CBP$naics <- gsub('----','',CBP$naics)
# CBP$naics <- gsub('/','',CBP$naics)
# 
# CBP$emp_small_firms[CBP$emp_small_firms == Inf] <- 1
# 
# CBP$naics_numeric <- as.numeric(CBP$naics)
# CBP <- CBP %>% mutate(four_digit_naics = if_else(between(naics_numeric,1000,9999),1,0),
#                       three_digit_naics = if_else(between(naics_numeric,100,999),1,0),
#                       two_digit_naics = if_else(naics_numeric<100,1,0))
# CBP <- CBP %>% filter(two_digit_naics==1)
# 
# 
# #################################
# ### Merge CBP and Dynamism    ###
# #################################
# 
# CBP <- left_join(CBP,state_crosswalk)
# dynamism <- left_join(CBP,dynamism)
# dynamism$emp_small_firms[dynamism$emp_small_firms=="NaN"] <- NA
# setwd(path_project)
# 
# save(file = "CBP_Dynamism_1992_2020.RData",dynamism)
