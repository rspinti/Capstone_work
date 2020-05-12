setwd("/Users/rachelspinti/Documents/R projects/NABD")
HUC_data <- read.csv("NABD_HUC_analysis2.csv")
library(ggplot2)
library(hrbrthemes)
library(dplyr)
H=1
fout=paste("mygraph_",H,".jpeg",sep="")
fout

# grabbing variables for HUC 02
#HUC2=as.character(HUC_data$join_REACH)
HUC=floor(HUC_data$join_REACH/(10^12))
HUC[1:20]
HUC1=(which(HUC=="1"))

#list_H2=which(substr(HUC2,1,1)== "2") # HUC2
yr_compl2=HUC_data$Year_compl
yr2=yr_compl2[HUC1]
pick_list2=which(yr2>0)
yr.f2=yr2[pick_list2]

dates2=c(1600, 1800, 1900, 1920, 1940, 1960, 1980, 2000, 2010) # time variables

# Storage variables
stor_H2=HUC_data$Norm_stor
stor_H2.f=HUC_data$Norm_stor[yr.f2]   #storage for HUC 2
#length(stor_H2.f)
#range(stor_H2.f, na.rm=TRUE)

purp_H2=HUC_data$Purposes  # grabbing purpose variables
pH2=purp_H2[yr.f2]   #filtering for yr>0 and code 2
#pH2
csum2=cumsum(stor_H2.f) #Total cumulative storage
sum_H2=cbind(yr.f2, stor_H2.f, csum2)
#dim(sum_H2)

### "C" cumulative storage
C_list2=which(substr(pH2,1,1)== "C")   #pick all flood control dams
C_storage2=stor_H2.f[C_list2]   #extract storage
C_dates2=yr.f2[C_list2]   #extract dates of completion
C_sum_stor2=cumsum(C_storage2)   #Calculate cumulative storage
#print(range(C_dates))

### "D" cumulative storage
D_list2=which(substr(pH2,1,1)== "D")   #pick all debris flow dams
D_storage2=stor_H2.f[D_list2] #extract storage
D_dates2=yr.f2[D_list2] #extract dates of completion
D_sum_stor2=cumsum(D_storage2)   #Calculate cumulative storage
#test[D_list]=test[D_list+1]

### "E" cumulative storage
E_list2=which(substr(pH2,1,1)== "E")   #pick all E dams
E_storage2=stor_H2.f[E_list2] #extract storage
E_dates2=yr.f2[E_list2] #extract dates of completion
E_sum_stor2=cumsum(E_storage2)   #Calculate cumulative storage

### "F" cumulative storage
F_list2=which(substr(pH2,1,1)== "F")   #pick all fish/wildlife pond dams
F_storage2=stor_H2.f[F_list2] #extract storage
F_dates2=yr.f2[F_list2] #extract dates of completion
F_sum_stor2=cumsum(F_storage2)   #Calculate cumulative storage
#test[C_list]=test[C_list+1]

### "G" cumulative storage
G_list2=which(substr(pH2,1,1)== "G")   #pick all grade stabilization dams
G_storage2=stor_H2.f[G_list2] #extract storage
G_dates2=yr.f2[G_list2] #extract dates of completion
G_sum_stor2=cumsum(G_storage2)     #Calculate cumulative storage

### "H" cumulative storage
H_list2=which(substr(pH2,1,1)== "H")   #pick all hydroelectric dams
H_storage2=stor_H2.f[H_list2] #extract storage
H_dates2=yr.f2[H_list2] #extract dates of completion
H_sum_stor2=cumsum(H_storage2)     #Calculate cumulative storage

### "I" cumulative storage
I_list2=which(substr(pH2,1,1)== "I")   #pick all irrigation dams
I_storage2=stor_H2.f[I_list2] #extract storage
I_dates2=yr.f2[I_list2] #extract dates of completion
I_sum_stor2=cumsum(I_storage2)     #Calculate cumulative storage

### "N" cumulative storage
N_list2=which(substr(pH2,1,1)== "N")   #pick all navigation dams
N_storage2=stor_H2.f[N_list2] #extract storage
N_dates2=yr.f2[N_list2] #extract dates of completion
N_sum_stor2=cumsum(N_storage2)     #Calculate cumulative storage

### "O" cumulative storage
O_list2=which(substr(pH2,1,1)== "O")   #pick all other dams
O_storage2=stor_H2.f[O_list2] #extract storage
O_dates2=yr.f2[O_list2] #extract dates of completion
O_sum_stor2=cumsum(O_storage2)     #Calculate cumulative storage
#print(range(O_dates))
#print(length(purp))
#print(length(NABD_work$Year_compl))

### "P" cumulative storage
P_list2=which(substr(pH2,1,1)== "P")   #pick all fire protection dams
P_storage2=stor_H2.f[P_list2] #extract storage
P_dates2=yr.f2[P_list2] #extract dates of completion
P_sum_stor2=cumsum(P_storage2)     #Calculate cumulative storage

###Rec cumulative storage
R_list2=which(substr(pH2,1,1)== "R")   #pick all recreation dams
R_storage2=stor_H2.f[R_list2] #extract storage
R_dates2=yr.f2[R_list2] #extract dates of completion
R_sum_stor2=cumsum(R_storage2)     #Calculate cumulative storage

### "S" cumulative storage
S_list2=which(substr(pH2,1,1)== "S")   #pick all water supply dams
S_storage2=stor_H2.f[S_list2] #extract storage
S_dates2=yr.f2[S_list2] #extract dates of completion
S_sum_stor2=cumsum(S_storage2)     #Calculate cumulative storage

### "T" cumulative storage
T_list2=which(substr(pH2,1,1)== "T")   #pick all tailings dams
T_storage2=stor_H2.f[T_list2] #extract storage
T_dates2=yr.f2[T_list2] #extract dates of completion
T_sum_stor2=cumsum(T_storage2)     #Calculate cumulative storage

### Blanks cumulative storage
B_list2=which(substr(pH2,1,1)=="")   #pick all blank purposes
B_storage2=stor_H2.f[B_list2] #extract storage
B_dates2=yr.f2[B_list2] #extract dates of completion
B_sum_stor2=cumsum(B_storage2)     #Calculate cumulative storage

### Blanks cumulative storage
B_list=which(is.na(substr(purp,1,1)))   #pick all blank purposes
B_storage=stor.f[B_list] #extract storage
B_dates=yr.f[B_list] #extract dates of completion
B_sum_stor =cumsum(B_storage)     #Calculate cumulative storage


pH2   #create the purpose list by groups
p2=pH2
p2[C_list2]="Water control"
p2[D_list2]="Water control"
p2[G_list2]="Water control"
p2[N_list2]="Water control"
p2[T_list2]="Water control"
p2[F_list2]="Water supply"
p2[I_list2]="Water supply"
p2[P_list2]="Water supply"
p2[S_list2]="Water supply"
#p2[H_list2]="Hydroelectric"
p2[B_list2]="Other"
p2[E_list2]="Other"
p2[O_list2]="Other"
p2[R_list2]="Other"
#p2[p2=="p"]="Water control"
#p2
#range(yr.f2, na.rm=TRUE)

range(yr.f2, na.rm= TRUE)  #create the labels for the periods

length_yr2=array(0,c(1,10))
print(length_yr2)

length_yr2[1]=length(which(yr.f2<dates2[1]))
for (i in 2:9){
  length_yr2[i]=length(which(yr.f2<dates2[i] & yr.f2>=dates2[i-1]))
}
length_yr2[10]=length(which(yr.f2>=2010))
sum(length_yr2)

yr.label2=c(rep("before 1600",length_yr2[1]), rep("1600-1800",length_yr2[2]), rep("1800-1900",length_yr2[3]),
            rep("1900-1920",length_yr2[4]), rep("1920-1940",length_yr2[5]), rep("1940-1960",length_yr2[6]),
            rep("1960-1980",length_yr2[7]), rep("1980-2000",length_yr2[8]), rep("2000-2010",length_yr2[9]), 
            rep("after 2010",length_yr2[10]))

data_H2 <- data.frame(yr.f2,yr.label2,Purpose,stor_H2.f)

# Plotting
#dates_H2[3:11]
hist(yr.f2, breaks=dates2, main= "Dams over time")  # Basic histogram
dev.copy(pdf, 'Dams_time2.pdf')
dev.off()

year=c("yr.f2","stor_H2.f") 
bar_H2 <- data.frame(year, storage=c(yr.f2,stor_H2.f)) # Basic bar for stor
ggplot(data, aes(x=year, y=storage)) + 
  geom_bar(stat = "identity")
#dev.copy(jpeg, 'basic_bar_H2.jpeg')
#dev.off()

#Line plot
plot(yr.f2, csum2, xlab="Year built", ylab="Storage (af)", type="l", col="red", xlim=c(1900, 2010))
#plot.new(Rec_dates, Rec_sum_stor, type="l", xlim=c(1900, 2010))
lines(R_dates2, R_sum_stor2, col='blue', xlim=c(1900, 2010))
lines(C_dates2, C_sum_stor2, col='green', xlim=c(1900, 2010))
lines(D_dates2, D_sum_stor2, col='purple', xlim=c(1900, 2010))
lines(I_dates2, I_sum_stor2, col='orange', xlim=c(1900, 2010))
lines(N_dates2, N_sum_stor2, col='pink', xlim=c(1900, 2010))
lines(S_dates2, S_sum_stor2, col='light blue',xlim=c(1900, 2010))
lines(T_dates2, T_sum_stor2, col='yellow', xlim=c(1900, 2010))
lines(O_dates2, O_sum_stor2, col='dark blue', xlim=c(1900, 2010))
lines(H_dates2, H_sum_stor2, col='violet', xlim=c(1900, 2010))
lines(P_dates2, P_sum_stor2, col='magenta', xlim=c(1900, 2010))
lines(F_dates2, F_sum_stor2, col='dark green', xlim=c(1900, 2010))
lines(G_dates2, G_sum_stor2, col='brown', xlim=c(1900, 2010))
lines(B_dates2, B_sum_stor2, col='black', xlim=c(1900, 2010))
lines(E_dates2, E_sum_stor2, col='black', xlim=c(1900, 2010))
#lines(cum_sum[,1], cum_sum[,3], col='red')
#file_name = 'Cumstore_line_H2_'+append_name+'.pdf'
#dev.copy(pdf,file_name)
#dev.copy(pdf, 'Cumstor_line_H2.pdf')
#dev.off()

#Stacked bar graph
quartz()   
ggplot(data_H2, aes(fill=p2, y=stor_H2.f, x=yr.label2)) + 
  geom_bar(position="stack", stat="identity")+
  ylab("Storage (ac-ft)") +xlab("Year")+ theme(legend.title = Purpose)
ggsave(filename= fout, plot = last_plot(), device = "jpeg", 
       path="/Users/rachelspinti/Documents/R projects/NABD/Plots",
       width = 10, height = 10, dpi = 700)


###Checking cumulative storage
tot2=tail(csum2, n=1)
#check_stor= sum(tail(T_sum_stor , n=1), tail(O_sum_stor , n=1), tail(H_sum_stor , n=1), tail(P_sum_stor , n=1), tail(F_sum_stor , n=1), tail(G_sum_stor , n=1), tail(C_sum_stor , n=1), tail(D_sum_stor , n=1), tail(I_sum_stor , n=1), tail(?N_sum_stor , n=1), tail(Rec_sum_stor , n=1), tail(S_sum_stor , n=1))
t=tail(T_sum_stor2 , n=1)
o=tail(O_sum_stor2 , n=1)
h=tail(H_sum_stor2 , n=1)
p=tail(P_sum_stor2 , n=1)
f=tail(F_sum_stor2 , n=1)
g=tail(G_sum_stor2 , n=1)
c=tail(C_sum_stor2 , n=1)
d=tail(D_sum_stor2 , n=1)
i=tail(I_sum_stor2 , n=1)
n=tail(N_sum_stor2 , n=1)
r=tail(R_sum_stor2 , n=1)
s=tail(S_sum_stor2 , n=1)
b=tail(B_sum_stor2 , n=1)
e=tail(E_sum_stor2 , n=1)

check_stor2= sum(t,o,h,p,f,g,c,d,i,n,r,s,b,e, na.rm = FALSE)
check_stor2
tot2-check_stor2