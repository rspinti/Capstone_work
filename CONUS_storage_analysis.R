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
nabd_data <- read.csv("NABD.csv")

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
# dim(cum_sum) #check dimensions

## Storage purpose
purpose = nabd_data$Purposes[year.f] #filter purpose by year completed > 0
purpose
sort(unique(purpose))      #see number of unique codes
#length(which(is.na(purp)==T))
## Purposes:B= Blanks,C= flood control/storm water management, D= Debris control, F= Fish or wildlife pond,
## G= grade stabilization, H= Hydroelectric, I= Irrigation, N= Navigation, O= Other, 
## P= Fire protection, stock, small farm pond, R= Recreation, S= Water supply, T= Tailings

### "C" cumulative storage
C_list=which(substr(purpose,1,1)== "C")   #pick all flood control dams
C_storage=norm_storage[C_list] #extract storage
C_dates=year.f[C_list] #extract dates of completion
C_sum_stor =cumsum(C_storage)   #Calculate cumulative storage

### "D" cumulative storage
D_list=which(substr(purpose,1,1)== "D")   #pick all debris flow dams
D_storage=norm_storage[D_list] #extract storage
D_dates=year.f[D_list] #extract dates of completion
D_sum_stor =cumsum(D_storage)   #Calculate cumulative storage
#test[D_list]=test[D_list+1]

### "E" cumulative storage
E_list=which(substr(purpose,1,1)== "E")   #pick all fish/wildlife pond dams
E_storage=norm_storage[E_list] #extract storage
E_dates=year.f[E_list] #extract dates of completion
E_sum_stor =cumsum(E_storage)   #Calculate cumulative storage

### "F" cumulative storage
F_list=which(substr(purpose,1,1)== "F")   #pick all fish/wildlife pond dams
F_storage=norm_storage[F_list] #extract storage
F_dates=year.f[F_list] #extract dates of completion
F_sum_stor =cumsum(F_storage)   #Calculate cumulative storage
#test[C_list]=test[C_list+1]

### "G" cumulative storage
G_list=which(substr(purpose,1,1)== "G")   #pick all grade stabilization dams
G_storage=norm_storage[G_list] #extract storage
G_dates=year.fl[G_list] #extract dates of completion
G_sum_stor =cumsum(G_storage)     #Calculate cumulative storage

### "H" cumulative storage
H_list=which(substr(purpose,1,1)== "H")   #pick all hydroelectric dams
H_storage=norm_storage[H_list] #extract storage
H_dates=year.f[H_list] #extract dates of completion
H_sum_stor =cumsum(H_storage)     #Calculate cumulative storage

### "I" cumulative storage
I_list=which(substr(purpose,1,1)== "I")   #pick all irrigation dams
I_storage=norm_storage[I_list] #extract storage
I_dates=year.f[I_list] #extract dates of completion
I_sum_stor =cumsum(I_storage)     #Calculate cumulative storage

### "N" cumulative storage
N_list=which(substr(purpose,1,1)== "N")   #pick all navigation dams
N_storage=norm_storage[N_list] #extract storage
N_dates=year.f[N_list] #extract dates of completion
N_sum_stor =cumsum(N_storage)     #Calculate cumulative storage

### "O" cumulative storage
O_list=which(substr(purpose,1,1)== "O")   #pick all other dams
O_storage=norm_storage[O_list] #extract storage
O_dates=year.f[O_list] #extract dates of completion
O_sum_stor =cumsum(O_storage)     #Calculate cumulative storage

### "P" cumulative storage
P_list=which(substr(purpose,1,1)== "P")   #pick all fire protection dams
P_storage=norm_storage[P_list] #extract storage
P_dates=year.f[P_list] #extract dates of completion
P_sum_stor =cumsum(P_storage)     #Calculate cumulative storage

### "p" cumulative storage
p_list=which(substr(purpose,1,1)== "p")   #pick all fire protection dams (This is an error in the NID dataset)
p_storage=norm_storage[p_list] #extract storage
p_dates=year.f[p_list] #extract dates of completion
p_sum_stor =cumsum(p_storage)     #Calculate cumulative storage

### "R" cumulative storage
R_list=which(substr(purpose,1,1)== "R")   #pick all recreation dams
R_storage=norm_storage[R_list] #extract storage
R_dates=year.f[R_list] #extract dates of completion
R_sum_stor =cumsum(R_storage)     #Calculate cumulative storage

### "S" cumulative storage
S_list=which(substr(purpose,1,1)== "S")   #pick all water supply dams
S_storage=norm_storage[S_list] #extract storage
S_dates=year.f[S_list] #extract dates of completion
S_sum_stor =cumsum(S_storage)     #Calculate cumulative storage

### "T" cumulative storage
T_list=which(substr(purpose,1,1)== "T")   #pick all tailings dams
T_storage=norm_storage[T_list] #extract storage
T_dates=year.f[T_list] #extract dates of completion
T_sum_stor =cumsum(T_storage)     #Calculate cumulative storage

### Blanks cumulative storage
B_list=which(is.na(substr(purpose,1,1)))   #pick all blank purposes
B_storage=norm_storage[B_list] #extract storage
B_dates=year.f[B_list] #extract dates of completion
B_sum_stor =cumsum(B_storage)     #Calculate cumulative storage

### Group the purpose list by primary purpose
prim_purpose=purpose
prim_purpose
prim_purpose[C_list]="Flood control"
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

#create the labels for the periods
length_yr=array(0,c(1,10))
print(length_yr)
length_yr[1]=length(which(year.f<dates[1]))
for (i in 2:9){
  length_yr[i]=length(which(year.f<dates[i] & year.f>=dates[i-1]))
}
length_yr[10]=length(which(year.f>=2010))

yr.label=c(rep("before 1600",length_yr[1]), rep("1600-1800",length_yr[2]), rep("1800-1900",length_yr[3]),
           rep("1900-1920",length_yr[4]), rep("1920-1940",length_yr[5]), rep("1940-1960",length_yr[6]),
           rep("1960-1980",length_yr[7]), rep("1980-2000",length_yr[8]), rep("2000-2010",length_yr[9]), 
           rep("after 2010",length_yr[10]))

data2 <- data.frame(year.f,yr.label,Purpose,norm_storage)
length_yr
#length_yr

#-------------------------------------------------------
#Plotting

#Plotting dams by year built
dates=c(1600, 1800, 1900, 1920, 1940, 1960, 1980, 2000, 2010)

#dates[1]
dates[3:9]
#pdf("Dams_time.pdf", width=10, height=10)
hist(year.f, breaks=dates, main= "Dams over time")
dev.copy(pdf, 'Dams_timeII.pdf')
dev.off()

##Checking storage for the ones we filtered
storage.nf=NABD_work$Norm_stor[-pick_list]
par(mfrow=c(1,1))
hist(storage[pick_list], main="Storage with Dates")
#hist(storage[-pick_list], main="Storage without Dates")

##Checking length
length(which(storage.nf<1000))/length(storage.nf)
length(year_comp)
length(year.f)
min(year.f)


###Plotting
cum_sum[,1]
plot(cum_sum[,1],cum_sum[,3], xlab="Year built", ylab="Storage (af)", type="l", col="red", xlim=c(1900, 2010))
#plot.new(Rec_dates, Rec_sum_stor, type="l", xlim=c(1900, 2010))
lines(R_dates, R_sum_stor, col='blue', xlim=c(1900, 2010))
lines(C_dates, C_sum_stor, col='green', xlim=c(1900, 2010))
lines(D_dates, D_sum_stor, col='purple', xlim=c(1900, 2010))
lines(I_dates, I_sum_stor, col='orange', xlim=c(1900, 2010))
lines(N_dates, N_sum_stor, col='pink', xlim=c(1900, 2010))
lines(S_dates, S_sum_stor, col='light blue',xlim=c(1900, 2010))
lines(T_dates, T_sum_stor, col='yellow', xlim=c(1900, 2010))
lines(O_dates, O_sum_stor, col='dark blue', xlim=c(1900, 2010))
lines(H_dates, H_sum_stor, col='violet', xlim=c(1900, 2010))
lines(P_dates, P_sum_stor, col='magenta', xlim=c(1900, 2010))
lines(F_dates, F_sum_stor, col='dark green', xlim=c(1900, 2010))
lines(G_dates, G_sum_stor, col='brown', xlim=c(1900, 2010))
lines(B_dates, B_sum_stor, col='black', xlim=c(1900, 2010))
lines(E_dates, E_sum_stor, col='black', xlim=c(1900, 2010))
lines(p_dates, p_sum_stor, col='black', xlim=c(1900, 2010))
#lines(cum_sum[,1], cum_sum[,3], col='red')
dev.copy(pdf, 'Cumstor_line.pdf')
dev.off()


###Checking cumulative storage
tot=tail(cumsum(norm_storage) , n=1)
tot
#check_stor= sum(tail(T_sum_stor , n=1), tail(O_sum_stor , n=1), tail(H_sum_stor , n=1), tail(P_sum_stor , n=1), tail(F_sum_stor , n=1), tail(G_sum_stor , n=1), tail(C_sum_stor , n=1), tail(D_sum_stor , n=1), tail(I_sum_stor , n=1), tail(?N_sum_stor , n=1), tail(Rec_sum_stor , n=1), tail(S_sum_stor , n=1))
t=tail(T_sum_stor , n=1)
o=tail(O_sum_stor , n=1)
h=tail(H_sum_stor , n=1)
pp=tail(P_sum_stor , n=1)
f=tail(F_sum_stor , n=1)
g=tail(G_sum_stor , n=1)
c=tail(C_sum_stor , n=1)
d=tail(D_sum_stor , n=1)
i=tail(I_sum_stor , n=1)
n=tail(N_sum_stor , n=1)
r=tail(R_sum_stor , n=1)
s=tail(S_sum_stor , n=1)
b=tail(B_sum_stor , n=1)
e=tail(E_sum_stor , n=1)
p=tail(p_sum_stor , n=1)

check_stor= sum(t,o,h,p,f,g,c,d,i,n,r,s,b,e,pp, na.rm = FALSE)
check_stor
tot-check_stor

# Basic
data <- data.frame(name=c("check_stor","tot") , storage=c(check_stor,tot))
ggplot(data, aes(x=name, y=storage)) + 
  geom_bar(stat = "identity")

dev.copy(jpeg, 'basic_bar.jpeg')
dev.off()

# Stacked bar graph
# #create the purpose list by groups, there will be too many categories if use the original 'purp' list
# purp2=purp
# purp2[T_list]="T"
# purp2[O_list]="O"
# purp2[H_list]="H"
# purp2[P_list]="P"
# purp2[F_list]="F"
# purp2[G_list]="G"
# purp2[C_list]="C"
# purp2[D_list]="D"
# purp2[I_list]="I"
# purp2[N_list]="N"
# purp2[R_list]="R"
# purp2[S_list]="S"
# purp2[B_list]="NA"
# 
# #create the labels for the periods
# length_yr=array(0,c(1,10))
# print(length_yr)
# length_yr[1]=length(which(yr.f<dates[1]))
# for (i in 2:9){
#   length_yr[i]=length(which(yr.f<dates[i] & yr.f>=dates[i-1]))
# }
# length_yr[10]=length(which(yr.f>=2010))
# 
# yr.label=c(rep("before 1600",length_yr[1]), rep("1600-1800",length_yr[2]), rep("1800-1900",length_yr[3]),
#            rep("1900-1920",length_yr[4]), rep("1920-1940",length_yr[5]), rep("1940-1960",length_yr[6]),
#            rep("1960-1980",length_yr[7]), rep("1980-2000",length_yr[8]), rep("2000-2010",length_yr[9]), 
#            rep("after 2010",length_yr[10]))
# 
# data2 <- data.frame(yr.f,yr.label,purp2,norm_storage)
# length_yr
# # Stacked
# quartz()
# ggplot(data2, aes(fill=purp2, y=norm_storage, x=yr.label)) + 
#   geom_bar(position="stack", stat="identity")
# 
# dev.copy(jpeg, 'stacked_bar.jpeg')
# dev.off()



# Stacked
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

# ## Inputs
# yr_start = 1900
# yr_end = 1901
# 
# pop_data=read.csv("Census_data.csv")
# # print(rain)
# Year <- pop_data$Year
# Population <- pop_data$Population
# percapita_stor <- data.frame(Year, Population)
# 
# Storage <- storage.f
# storagedf <- data.frame(yr.f, Storage)
# 
# for (year in range(yr_start,yr_end+1)){
#   storage_filtered = test[test$yr.f == 'year']
#   print(storage_filtered)
#   yr.sum = storage_filtered.cumsum()
#   print(yr.sum)
# }
# #   for month in range(1,13):
# #   rain_filtered = rain[(rain.Year == year) & (rain.Month == month)]
# # # print(rain_filtered)
# #   test = rain_filtered.sum()
# # #print(test)
# # #print(test.Rainfall)
# rain_depth = rain_depth.append({'Year': year, 'Month': month, 'Rainfall': test.Rainfall}, ignore_index=True)
# 
# # rain = np.zeros((year, month, day, Id, msum), dtype=np.float32) 
# # rain[:,:,:] = np.loadtxt('Analysis_1895 to 1924.csv', delimiter=",")
# rain_depth.to_csv('rain_monthsum.csv')