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



##################
### Plot Theme ###
##################
Custom_theme <- theme(plot.title = element_text(size=35),
                      plot.subtitle = element_text(size = 25),
                      axis.text.y = element_text(size = 25),
                      axis.text.x = element_text(size = 25, angle = 90),
                      axis.title.y = element_text(size = 30),
                      axis.title.x = element_text(size = 30),
                      strip.text.x = element_text(size = 30),
                      strip.text.y = element_text(size = 30),
                      strip.background = element_rect(fill = "white"),
                      strip.placement = "outside",
                      panel.background = element_rect(fill = "white"),
                      panel.grid = element_line(colour = "grey"),
                      panel.spacing = unit(2, "lines"),
                      axis.line = element_line(colour = "black"),
                      legend.position = "bottom",
                      legend.title = element_text(size = 20),
                      legend.key = element_rect(fill="white"),
                      legend.text = element_text(size = 25),
                      legend.background = element_rect(fill=NA))

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
##########################################################
### Dynamism and share of emp in different industry    ###
##########################################################
setwd(path_project)
load("CBP_Dynamism_1992_2020.RData")
total <- dynamism %>% filter(naics == "00" & year>=2000) %>% rename("total_emp" = "emp") %>% select(fipstate,year,total_emp)
industry_share <- dynamism %>% filter(naics!="00")
industry_share <- inner_join(industry_share, total) 
industry_share <- industry_share %>% mutate(ind_share = emp/total_emp)

naics_list <- sort(unique(industry_share$naics))

results_list <- list()

for (q in seq_along(naics_list)) {
  
  df <- industry_share %>% filter(naics == naics_list[q])

  corr_list <- list()
  
  for(i in seq_along(Years)){
    dat <- df %>% 
      filter(year == Years[[i]]) %>% 
      select(ind_share,Dyanimsm) %>% 
      na.omit()
    
    corr_list[[i]] <- cor(dat)
  }
  
  results <- list()
  for(i in seq_along(Years)){
    results[[i]] <- corr_list[[i]][[2]]
  }
  
  results_list[[q]] <- as.data.frame(cbind(do.call(rbind, results),Years)) %>% rename("Correlation" = "V1")
}

for (q in seq_along(naics_list)) {
P <-
  results_list[[q]] %>% 
  filter(Years>=2000) %>% 
  mutate(Correlation = as.numeric(as.character(Correlation))) %>% 
  ggplot(aes(x = Years,
             y = Correlation)) + 
  ggtitle(label = paste0("NAICS ",naics_list[[q]])) +
  geom_hline(yintercept = 0, color = "black") + 
  geom_point() +
  geom_line() + 
    Custom_theme 

png(filename = paste0("laborshare_dynamism_annual_corr_naics",naics_list[[q]],".png"),
    width = 1000,
    height = 1000)
plot(P)
dev.off()
}

trend <- industry_share %>% group_by(year, fipstate) %>% summarise(total = mean(ind_share))
trend %>% 
  ggplot(aes(x = year,
             y = total,
             group = fipstate)) + 
  geom_line()

############################################
### Dynamism and firm size correlation   ###
############################################
corr_list <- list()

for(i in seq_along(Years)){
  
  dat <- dynamism %>% 
    filter(naics == "00" & year == Years[[i]]) %>% 
    select(emp_small_firms,Dyanimsm) %>% 
    na.omit()
  
  corr_list[[i]] <- cor(dat)
}

results <- list()
for(i in seq_along(Years)){
  results[[i]] <- corr_list[[i]][[2]]
}
results <- as.data.frame(cbind(do.call(rbind, results),Years)) %>% rename("Correlation" = "V1")

results %>% 
  ggplot(aes(x = Years,
             y = Correlation)) + 
  geom_hline(yintercept = 0, color = "black") + 
  geom_point() + 
  geom_line()
