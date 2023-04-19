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
load("CBP_Dynamism_1992_2020.RData")

Years <- c(1992:2020)

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

##########################################################
### Dynamism and share of emp in different industry    ###
##########################################################
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
    ylim(-0.8,0.8) +
    Custom_theme 
  
  png(filename = paste0("laborshare_dynamism_annual_corr_naics",naics_list[[q]],".png"),
      width = 1000,
      height = 1000)
  plot(P)
  dev.off()
  
  # plot(P)
  
}


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

P <- results %>% 
  ggplot(aes(x = Years,
             y = Correlation)) + 
  ggtitle(label = "Share of workers at Small Employers and Dynamism") +
  geom_hline(yintercept = 0, color = "black") + 
  geom_point() +
  geom_line() + 
  ylim(-0.8,0.8) +
  Custom_theme 

png(filename = paste0("smallemp_v_largeemp_dynamism_annual_corr_naics00.png"),
    width = 1000,
    height = 1000)
plot(P)
dev.off()
