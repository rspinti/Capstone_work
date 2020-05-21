library(ggplot2)
setwd("/Users/rachelspinti/Documents/R projects/NABD")
percapita <- read.csv("percapita_storage.csv")

year=percapita$Year
people=percapita$Population
rstorage=percapita$Storage
tot_stor=cumsum(rstorage)
ppl_stor=tot_stor/people

plot(year,ppl_stor)

# #library
# install.packages("latticeExtra")
# library(latticeExtra)
# 
# # create data
# set.seed(1)
# x <- year
# var1 <- tot_stor
# var2 <- people
# data <- data.frame(x,var1,var2)
# 
# # --> construct separate plots for each series
# obj1 <- xyplot(var1 ~ x, data, type = "l" , lwd=4, col="turquoise3", 
#                key=list(columns=1, 
#                         text=list(lab=c("Storage","Population")), points=list(pch=c(3,NA), col="turquoise3"), 
#                         lines=list(lty=c(0,3), lwd=4, col="indianred2")), main=list(label="Reservoir storage vs. U.S. population"), 
#                xlab ="Year", ylab="Reservoir storage (ac-ft)")
# obj2 <- xyplot(var2 ~ x, data, type = "l", lwd=4, col="indianred2", main="Reservoir storage vs. U.S. population", ylab="U.S. population")
# 
# # --> Make the plot with second y axis:
# doubleYScale(obj1, obj2, add.ylab2 = TRUE, use.style=FALSE )
# # # usual line chart
# # xyplot(var1 + var2 ~ x, data, type = "l", col=c("steelblue", "#69b3a2") , lwd=2)
# # Value used to transform the data
# coeff <- 10

# Double y axis plot
storageColor <- "turquoise3"
peopleColor <- "tomato2"
color_leg=c("Storage"="turquoise3","Population"="tomato2")

ggplot(data, aes(x=year)) +
  geom_line( aes(y=tot_stor/1000000), size=4, fill="Storage", color=storageColor) + 
  geom_line( aes(y=people/1000000), size=4, fill="Population", color=peopleColor) +
  scale_y_continuous(name = "Reservoir storage (million ac-ft)", sec.axis = sec_axis(~.*0.75, name="U.S. population (million people)")) + 
  xlim(1900, 2010) +
  theme(legend.position = c(0.90, 0.90), axis.title.x = element_text(size=24,face = "bold"), 
        axis.title.y = element_text(size=24,face = "bold"), legend.title = element_text(size=18, face="bold")) +
  xlab("Year")+
  scale_colour_manual(breaks = c("Reservoir storage", "U.S. population"),
                      values = color_leg)+ theme(axis.text.x=element_text(size=24))+
  theme(axis.text.y=element_text(size=24))
ggsave(plot = last_plot(), filename ="Storage_and_pop.jpeg", device = "jpeg",
       path="/Users/rachelspinti/Documents/R projects/NABD/Plots",
       width = 10, height = 10, dpi = 700)

### Storage line plot
library(ggplot2)
library(hrbrthemes)
library(dplyr)

#Load dataset
capita_data<- data.frame(year,ppl_stor) 
plot1=ggplot(capita_data, aes(x=year, y=ppl_stor)) +
  geom_line(color="slateblue",size = 4) +
  # scale_color_manual(values=colg)+
  # ggtitle("Storage per capita")+
  xlim(1900, 2010) +
  theme(axis.title.x = element_text(size=24,face = "bold"), axis.title.y = element_text(size=24,face = "bold")) +
  theme(legend.title = element_text(size=18, face="bold")) +
  theme(legend.text = element_text(size =18))+
  scale_fill_manual(values=c(3,10,16,17), name="Group 2")+
  xlab("Year") + ylab("Reservoir storage per capita (ac-ft/person)")+ theme(axis.text.x=element_text(size=24))+
  theme(axis.text.y=element_text(size=24))+ guides(fill=my_data)
plot1
ggsave(plot = last_plot(), filename ="Storagepercapita.jpeg", device = "jpeg",
       path="/Users/rachelspinti/Documents/R projects/NABD/Plots",
       width = 10, height = 10, dpi = 700)

