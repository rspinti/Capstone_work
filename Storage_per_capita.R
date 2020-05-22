# Storage per capita analysis
## Authors: Rachel A. Spinti, Laura E. Condon, Jun Zhang
## Contact: rspinti@email.arizona.edu

#-------------------------------------------------------

#General Setup 
## Load libraries
library(ggplot2)
library(hrbrthemes)
library(tidyverse)
library(dplyr)

## Data location
directory <- c("/Users/rachelspinti/Documents/R_projects/NABD/Capstone_work")
setwd(directory)

#-------------------------------------------------------

#Grabbing variables 
yr_start = 1900
yr_end = 1901
nabd_data <- read.csv("NABD.csv")  #read in NABD csv to extract years and normal storage
storage_data <- subset(nabd_data, Year_compl > 1899, select = c(Year_compl,Norm_stor)) #select storage where year>1899
pop <- read.csv("Census_data.csv") # read in Census data

# per_capita <- data.frame("Year","Population","Cumulative normal storage")  #create dataframe to append values to from for loop
id <- c(1:2)
per_capita <- data.frame(id)
## Select by 
for (yr in yr_start:yr_end){
  # print(yr)
  test <- subset(storage_data, Year_compl == yr, select = c(Year_compl,Norm_stor))
  # print(test)
  # storage_sum = sum(test$Norm_stor)
  # print(storage_sum)
  year[yr] = pop$Year[yr]
  per_capita$year <- year
  # # people <- subset(pop, Year == yr, select = c(Population))
  # # population = c(people)
  # # per_capita$population <- population
  # storage_per_capita = 
}


#-------------------------------------------------------

#Plotting

## Storage per capita plot
storage_capita_plot=ggplot(percapita, aes(x=year, y=stor_cap)) + #create plot and add features
  geom_line(color="cornflowerblue",size = 4) +
  xlim(1900, 2010) + xlab("Year") + 
  ylab("Reservoir storage per capita (ac-ft/person)")  
  theme(axis.title.x = element_text(size=24,face = "bold"), 
        axis.title.y = element_text(size=24,face = "bold"), 
        axis.text.x=element_text(size=24), 
        axis.text.y=element_text(size=24))+
storage_capita_plot   #show plot 
ggsave(plot = storage_capita_plot, filename ="Storage_per_capita.jpeg", device = "jpeg",
       width = 10, height = 10, dpi = 700)     #save plot as jpeg

## Comparison of cumulative storage and population growth
storageColor <- "turquoise3"   #set color for cumulative storage
peopleColor <- "tomato2"    #set color for population

storage_pop_plot= ggplot(capita_data, aes(x=year)) +   #create plot and add features
  geom_line( aes(y=total_stor/1000000), size=4, color=storageColor) +
  geom_line( aes(y=people/1000000), size=4, color=peopleColor) +
  xlim(1900, 2010) + xlab("Year") + 
  scale_y_continuous(name = "Reservoir storage (million ac-ft)",
                     sec.axis = sec_axis(~.*0.75, name="U.S. population (million people)")) +
  theme( axis.line.y.right = element_line(color = peopleColor),
         axis.ticks.y.right = element_line(color = peopleColor),
         axis.text.y.right = element_text(color = peopleColor),
         axis.title.y.right = element_text(color=peopleColor, size=24, face="bold"),
         axis.line.y.left = element_line(color = storageColor),
         axis.ticks.y.left = element_line(color = storageColor),
         axis.text.y.left = element_text(color = storageColor),
         axis.title.y.left = element_text(color=storageColor, size=24, face="bold"),
         axis.title.x = element_text(size = 24, face = "bold"), 
         axis.text.x=element_text(size=24),
         axis.text.y=element_text(size=24))
storage_pop_plot   #show plot
ggsave(plot = storage_pop_plot, filename ="Storage_and_pop.jpeg", device = "jpeg",
       width = 10, height = 10, dpi = 700)    #save plot as jpeg

