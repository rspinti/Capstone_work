# CONUS storage analysis
## Authors: Rachel A. Spinti, Laura E. Condon, Jun Zhang
## Contact: rspinti@email.arizona.edu

#--------------------------------------------------------

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
nabd_data <- read.csv("NABD.csv")  #read in NABD csv to extract years and normal storage
storage_data <- subset(nabd_data, Year_compl > 0, select = c(Year_compl,Norm_stor,Purposes)) #select storage where year>1899
## Time
year_comp = nabd_data$Year_compl
year.f = which(year_comp>0) #filter out years w/o a dam completion date
year = year_comp[year.f] #filter so year completed > 0
# number_dams = length(year.f) #how many dams are there after filtering?

## Storage
norm_storage = nabd_data$Norm_stor[year.f] #reservoir storage where year completed > 0
storage.test = nabd_data$Norm_stor[-year.f] #check for reservoir storage where year completed < 0
# print(sum(norm_storage)) #check sums to ensure storage for [year completed < 0] < [year completed > 0]
# print(sum(storage.test))
cum_sum=cbind(year.f, norm_storage, cumsum(norm_storage)) #Total cumulative storage

## Storage purpose
purpose = nabd_data$Purposes[year.f] #filter purpose by year completed > 0
purpose
sort(unique(purpose))      #see number of unique codes
## Purposes:B= Blanks,C= flood control/storm water management, D= Debris control, F= Fish or wildlife pond,
## G= grade stabilization, H= Hydroelectric, I= Irrigation, N= Navigation, O= Other, 
## P= Fire protection, stock, small farm pond, R= Recreation, S= Water supply, T= Tailings

### "C" cumulative storage
C_list = which(substr(purpose,1,1)== "C")   #pick all flood control dams
C_storage = norm_storage[C_list] #extract storage
C_dates = year[C_list] #extract dates of completion
C_sum_stor = cumsum(C_storage)   #Calculate cumulative storage

### "D" cumulative storage
D_list = which(substr(purpose,1,1)== "D")   #pick all debris flow dams
D_storage = norm_storage[D_list] #extract storage
D_dates = year[D_list] #extract dates of completion
D_sum_stor = cumsum(D_storage)   #Calculate cumulative storage
#test[D_list]=test[D_list+1]

### "E" cumulative storage
E_list = which(substr(purpose,1,1)== "E")   #pick all fish/wildlife pond dams
E_storage = norm_storage[E_list] #extract storage
E_dates = year[E_list] #extract dates of completion
E_sum_stor =cumsum(E_storage)   #Calculate cumulative storage

### "F" cumulative storage
F_list = which(substr(purpose,1,1)== "F")   #pick all fish/wildlife pond dams
F_storage = norm_storage[F_list] #extract storage
F_dates = year[F_list] #extract dates of completion
F_sum_stor = cumsum(F_storage)   #Calculate cumulative storage
#test[C_list]=test[C_list+1]

### "G" cumulative storage
G_list = which(substr(purpose,1,1)== "G")   #pick all grade stabilization dams
G_storage = norm_storage[G_list] #extract storage
G_dates = year[G_list] #extract dates of completion
G_sum_stor = cumsum(G_storage)     #Calculate cumulative storage

### "H" cumulative storage
H_list = which(substr(purpose,1,1)== "H")   #pick all hydroelectric dams
H_storage = norm_storage[H_list] #extract storage
H_dates = year[H_list] #extract dates of completion
H_sum_stor = cumsum(H_storage)     #Calculate cumulative storage

### "I" cumulative storage
I_list = which(substr(purpose,1,1)== "I")   #pick all irrigation dams
I_storage = norm_storage[I_list] #extract storage
I_dates = year[I_list] #extract dates of completion
I_sum_stor = cumsum(I_storage)     #Calculate cumulative storage

### "N" cumulative storage
N_list = which(substr(purpose,1,1)== "N")   #pick all navigation dams
N_storage = norm_storage[N_list] #extract storage
N_dates = year[N_list] #extract dates of completion
N_sum_stor = cumsum(N_storage)     #Calculate cumulative storage

### "O" cumulative storage
O_list = which(substr(purpose,1,1)== "O")   #pick all other dams
O_storage = norm_storage[O_list] #extract storage
O_dates = year[O_list] #extract dates of completion
O_sum_stor = cumsum(O_storage)     #Calculate cumulative storage

### "P" cumulative storage
P_list = which(substr(purpose,1,1)== "P")   #pick all fire protection dams
P_storage = norm_storage[P_list] #extract storage
P_dates = year[P_list] #extract dates of completion
P_sum_stor = cumsum(P_storage)     #Calculate cumulative storage

### "p" cumulative storage
p_list = which(substr(purpose,1,1)== "p")   #pick all fire protection dams (This is an error in the NID dataset)
p_storage = norm_storage[p_list] #extract storage
p_dates = year[p_list] #extract dates of completion
p_sum_stor = cumsum(p_storage)     #Calculate cumulative storage

### "R" cumulative storage
R_list = which(substr(purpose,1,1)== "R")   #pick all recreation dams
R_storage = norm_storage[R_list] #extract storage
R_dates = year[R_list] #extract dates of completion
R_sum_stor = cumsum(R_storage)     #Calculate cumulative storage

### "S" cumulative storage
S_list = which(substr(purpose,1,1)== "S")   #pick all water supply dams
S_storage = norm_storage[S_list] #extract storage
S_dates = year[S_list] #extract dates of completion
S_sum_stor = cumsum(S_storage)     #Calculate cumulative storage

### "T" cumulative storage
T_list = which(substr(purpose,1,1)== "T")   #pick all tailings dams
T_storage = norm_storage[T_list] #extract storage
T_dates = year[T_list] #extract dates of completion
T_sum_stor = cumsum(T_storage)     #Calculate cumulative storage

### Blanks cumulative storage
B_list=which(is.na(substr(purpose,1,1)))   #pick all blank purposes
B_storage = norm_storage[B_list] #extract storage
B_dates = year[B_list] #extract dates of completion
B_sum_stor = cumsum(B_storage)     #Calculate cumulative storage
length(B_list)

### Group the purpose list by primary purpose
prim_purpose = purpose
prim_purpose = as.character(prim_purpose)
prim_purpose[C_list] = "Flood control"
prim_purpose[D_list]="Flood control"
prim_purpose[G_list]="Flood control"
prim_purpose[N_list]="Flood control"
prim_purpose[p_list]="Flood control"
prim_purpose[T_list]="Flood control"
prim_purpose[F_list]="Water supply"
prim_purpose[I_list]="Water supply"
prim_purpose[P_list]="Water supply"
prim_purpose[S_list]="Water supply"
prim_purpose[H_list]="Hydroelectric"
prim_purpose[B_list]="Other"
prim_purpose[E_list]="Other"
prim_purpose[O_list]="Other"
prim_purpose[R_list]="Other"
prim_purpose

value<- prim_purpose
count(value, vars ="Flood control")
str_count(prim_purpose, pattern="F")

### create the labels for the various time periods
length_yr=array(0,c(1,10))
# print(length_yr)
length_yr[1]=length(which(year<dates[1]))
for (i in 2:9){
  length_yr[i]=length(which(year<dates[i] & year>=dates[i-1]))
}
length_yr[10]=length(which(year>=2010))

yr.label=c(rep("before 1600",length_yr[1]), rep("1600-1800",length_yr[2]), rep("1800-1900",length_yr[3]),
           rep("1900-1920",length_yr[4]), rep("1920-1940",length_yr[5]), rep("1940-1960",length_yr[6]),
           rep("1960-1980",length_yr[7]), rep("1980-2000",length_yr[8]), rep("2000-2010",length_yr[9]), 
           rep("after 2010",length_yr[10]))

data2 <- data.frame(year.f,yr.label,Purpose,norm_storage)
length_yr
#length_yr

#-------------------------------------------------------
#Plotting

## Stacked bar plot by storage purpose
quartz()
ggplot(data2, aes(fill=Purpose, y=norm_storage, x=yr.label)) + 
  geom_bar(position="stack", stat="identity")+
  ylab("Storage (ac-ft)") +xlab("Year") + 
  theme(axis.title.x = element_text(size=28,face = "bold"), axis.title.y = element_text(size=28,face = "bold")) + 
  theme(legend.title = element_text(size=26, face="bold")) +
  theme(legend.text = element_text(size = 20))+ 
  scale_fill_discrete(name="Primary purpose", breaks = c("Other", "Flood control", "Hydroelectric","Water supply"))+
  theme(axis.text.x=element_text(size=24,angle=45))+
  theme(axis.text.y=element_text(size=24))
#theme(legend.position="topleft")
#+ggtitle("Reservoir storage purpose over time")
ggsave('stacked_bar_final.jpeg', plot = last_plot(), device = "jpeg", 
       path="/Users/rachelspinti/Documents/R projects/NABD/Plots",
       width = 15, height = 10,dpi = 700)

# dev.copy(jpeg, 'stacked_bar_final.jpeg')
# dev.off()

### Pie chart for Purpose
# Load Data
data2
# Basic piechart
ggplot(data2, aes(x="", y=norm_storage, fill=Purpose)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

### Storage line plot


#Load dataset
#length(yr.f)
#cum_sum=as.integer(cumsum(stor.f)) 
#year_f=as.numeric(yr.f[cum_sum])
#length(year_f)
#length(cum_sum)
# range(cum_sum)

#sdata<- data.frame(year_f,cum_sum) 
s=cumsum(stor.f)
sdata<- data.frame(cum_sum[,1],s) 
# Plot
ggplot(sdata, aes(x=cum_sum[,1], y=s)) +
  geom_line(color="deeppink4", size=1) +
  geom_area(fill="deeppink4", alpha=0.4) +
  theme_ipsum() +
  ggtitle("Reservoir storage over time") + xlim(1890, 2010) + ylim(0, 500000000) + 
  xlab("Year") + ylab("Reservior storage (ac-ft)")
ggsave(plot = last_plot(), filename ="fline.jpeg", device = "jpeg",
       width = 10, height = 10, dpi = 700)

# Make the histogram
sdata %>%
  #filter(  ) %>%
  ggplot( aes(x=cum_sum[,1])) +
  geom_density(fill="coral1", color="coral1", alpha=0.6) + xlim(1900, 2010) + ylab("Dam density") +
  xlab("Year")  + theme(axis.title.x = element_text(size=28, face="bold"), axis.title.y = element_text(size=28, face="bold"))+
  theme(axis.text.x=element_text(size=18))+
  theme(axis.text.y=element_text(size=18))

#+ theme(legend.title = element_text(size=20, face="bold")) +
#theme(legend.text = element_text(size = 20))
#+ ggtitle("Dams over time")
ggsave(plot = last_plot(), filename ="densityplot.jpeg", device = "jpeg",
       width = 10, height = 10, dpi = 700)


# Storage per capita
pop_data=read.csv("Census_data.csv")
pop=pop_data$Population
pop_yr=pop_data$Year
time=cum_sum[,1]
picktime=which(time>1899)
qsum=cum_sum[,3]
cumSUM=qsum[picktime]
time2=time[picktime]
print(pop_yr)
# time2
# unique(time2)
# # cumSUM
percapita_stor <- data.frame(pop_yr, pop)
# cum.year=array(0,c(1,length(pop_yr)))
for (i in 1:length(pop_yr)){
  yr.pick=which(yr.f==pop_yr[i])
  cumpick=stor.f[yr.pick]
  sumstor=sum(cumpick)+sum(cumpick[i-1])
  # percapita_stor[i]
}
plot(pop_yr,cum.year)

print(cum.year)

