# Storage per capita analysis
## Authors: Rachel A. Spinti, Laura E. Condon, Jun Zhang
## Contact: rspinti@email.arizona.edu

#-------------------------------------------------------

#General Setup 
## Load libraries
library(ggplot2)
library(hrbrthemes)
library(dplyr)

## Data location
directory <- c("/Users/rachelspinti/Documents/R_projects/NABD/Capstone_work")
setwd(directory)

#-------------------------------------------------------

#Grabbing variables 
percapita <- read.csv("percapita_storage.csv")
yr=percapita$Year  #years between 1900-2010
people=percapita$Population  #the population count
res_stor=percapita$Storage  #reservoir storage for each year
total_stor=cumsum(res_stor)  #cumulative storage over time
stor_cap=total_stor/people  #storage per capita (ac-ft/person)
capita_data<- data.frame(year,stor_cap) 

plot(yr,ppl_stor)

#-------------------------------------------------------

#Plotting

## Storage per capita plot
capita_plot=ggplot(capita_data, aes(x=year, y=stor_cap)) +
  geom_line(color="cornflowerblue",size = 4) +
  xlim(1900, 2010) +
  theme(axis.title.x = element_text(size=24,face = "bold"), 
        axis.title.y = element_text(size=24,face = "bold"), 
        axis.text.x=element_text(size=24), 
        axis.text.y=element_text(size=24))+
  xlab("Year") + ylab("Reservoir storage per capita (ac-ft/person)")  #create plot and add features
capita_plot   #show plot 
ggsave(plot = capita_plot, filename ="Storagepercapita.jpeg", device = "jpeg",
       width = 10, height = 10, dpi = 700)     #save plot as jpeg

## Comparison of cumulative storage and population growth
storageColor <- "turquoise3"   #set color for cumulative storage
peopleColor <- "tomato2"    #set color for population

comp_plot= ggplot(capita_data, aes(x=year)) +
  geom_line( aes(y=total_stor/1000000), size=4, color=storageColor) +
  geom_line( aes(y=people/1000000), size=4, color=peopleColor) +
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
         axis.text.y=element_text(size=24)) +
  xlim(1900, 2010) + xlab("Year")    #create plot and add features
comp_plot   #show plot
ggsave(plot = last_plot(), filename ="Storage_and_pop.jpeg", device = "jpeg",
       width = 10, height = 10, dpi = 700)    #save plot as jpeg

