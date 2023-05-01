
####################reading the data####################################
rm(list=ls())
getwd()
setwd("D:/SyamChintala_PhD/Syam_PhD/PhD_2nd Semester/CE6670/TermProject")
manot_data<-read.csv("Manot.csv")


### The Area of Manot CA = 4968 Sq.Km ##
### Converting the Units of Discharge from m3/sec to mm/day ####

manot_data$Q <- manot_data$Q*1000*24*3600/(4968*(10^6))

####################checking the class of data#############################
class(manot_data)
class(manot_data$Date)
class(manot_data$P)
class(manot_data$E)
class(manot_data$Q)

####################reading the date format##########################
manot_data$Date<-as.POSIXct(manot_data[,1],format="%d-%m-%Y")
manot_data$Year<-as.numeric(format(manot_data$D,"%Y"))
manot_data$Month<-as.numeric(format(manot_data$D,"%m"))
manot_data$day<-as.numeric(format(manot_data$D,"%d"))

###################################### Number of years##########################
min_year<-min(manot_data$Year)
max_year<-max(manot_data$Year)
no_of_years_data<-max_year - min_year +1
no_of_years_data

###################################### filling the NA values#########################
is.na(manot_data)           
na_values_at<-which((is.na(manot_data)))
na_values_at

#NA values in 45428 to 59341
#502+14975*3+1=45428
#14975*3
#14975*4
#na_values in 4th column i.e Q values

manot_data$Q[is.na(manot_data$Q)]<-mean(manot_data$Q,na.rm=TRUE)
manot_data

is.na(manot_data)          
na_values_afterfill<-which((is.na(manot_data))) 
na_values_afterfill

###################### summary daily data ####################
summary(manot_data[,c(2,3,4)])

################# Timeseries analysis ################################
############## a)Agrregating the monthly precipitation data ###################
v_1=c()
for( i in min_year: max_year){
  for( j in 1:12){
    d1<-sum(manot_data[which(manot_data$Year==i & manot_data$Month==j),]$P)
    v_1<-append(v_1,d1)
  }
}
v_1
############## b)Agrregating the monthly Q data ###################
v_2=c()
for( i in min_year: max(manot_data$Year)){
  for( j in 1:12){
    d2<-mean(manot_data[which(manot_data$Year==i & manot_data$Month==j),]$Q)
    v_2<-append(v_2,d2)
  }
}
############## Agrregating the monthly PET data ###################
v_3=c()
for( i in min(manot_data$Year): max(manot_data$Year)){
  for( j in 1:12){
    d3<-sum(manot_data[which(manot_data$Year==i & manot_data$Month==j),]$E)
    v_3<-append(v_3,d3)
  }
}
################## Adding months and year to the data #################
v_4=c()
v_5=c()
for(i in min_year:max_year){
  for( j in 1:12){
    v_4<-append(v_4,j)
    v_5<-append(v_5,i)
  }
}
############## Converting the above data into data frames #####################
df_1<-matrix(unlist(v_1),nrow=492,ncol=1)
df_1<-data.frame(df_1)

df_2<-matrix(unlist(v_2),nrow=492,ncol=1)
df_2<-data.frame(df_2)

df_3<-matrix(unlist(v_3),nrow=492,ncol=1)
df_3<-data.frame(df_3)

df_4<-matrix(unlist(v_4),nrow=492,ncol=1)
df_4<-data.frame(df_4)  

df_5<-matrix(unlist(v_5),nrow=492,ncol=1)
df_5<-data.frame(df_5)

############# combining the data frame ####################### 

TimeSeries<-cbind(df_5,df_4,df_1,df_2,df_3)
colnames(TimeSeries)<-c("Year","month","P","Q","E")
TimeSeries

#################################################################################
################## 1) Creating the monthly Time-Series ###############################

months_data<-c("January","February","March","April","May","June","July","August","September","October","November","December")

############a)precipitation############################
TimeSeries$P
a_P<-matrix(TimeSeries$P,nrow = 12,ncol=41)
df_mon_P<-data.frame(a_P)
colnames(df_mon_P)<-1977:2017
Monthly_dataframe_P<-cbind(months_data,df_mon_P)

############ b)streamflow ############################
TimeSeries$Q 
a_Q<-matrix(TimeSeries$Q,nrow = 12,ncol=41)
df_mon_Q<-data.frame(a_Q)
colnames(df_mon_Q)<-1977:2017
Monthly_dataframe_Q<-cbind(months_data,df_mon_Q)

############ C)Evapotranspiration ############################
TimeSeries$E
a_E<-matrix(TimeSeries$E,nrow = 12,ncol=41)
df_mon_E<-data.frame(a_E)
colnames(df_mon_E)<-1977:2017
Monthly_dataframe_E<-cbind(months_data,df_mon_Q)

#######################################################################################
####################### 2)Creating the Yearly Time-Series #######################################

############ a)precipitation ######################
dim(Monthly_dataframe_P)
print(df_mon_P)
Yearly_dataframe_P<-data.frame(colSums(df_mon_P))
colnames(Yearly_dataframe_P)<-c("Annual Precipitation")

############ b) Stream Flow #####################
dim(Monthly_dataframe_Q)
print(df_mon_Q)
Yearly_dataframe_Q<-data.frame(colSums(df_mon_Q))
colnames(Yearly_dataframe_Q)<-c("Annual STREAM FLOW")

############ C) Evapotranspiration  #####################
dim(Monthly_dataframe_E)
print(df_mon_E)
Yearly_dataframe_E<-data.frame(colSums(df_mon_E))
colnames(Yearly_dataframe_E)<-c("Annual EVAPOTRANSPIRATION")


#######################################################################################
####################### 3) Creating the seasonal Time-Series ############################
TimeSeries$P

seasonal_Data<- c("Monsoon","Post Monsoon","Winter","Summer")

################ A)Precipitatioin ###############################
df_mon_P
#------------------1 method:- for loop------------------------------
SeasonalDataP=matrix(ncol=4,nrow=ncol(df_mon_P))
for (icol in 1:ncol(df_mon_P)){
  SeasonalDataP[icol,1]=sum(df_mon_P[1:3,icol])
  SeasonalDataP[icol,2]=sum(df_mon_P[4:5,icol])
  SeasonalDataP[icol,3]=sum(df_mon_P[6:9,icol])
  SeasonalDataP[icol,4]=sum(df_mon_P[10:12,icol])
}
colnames(SeasonalDataP)<-c("Winter_P","Summer_P","Monsoon_P","Post_Monsoon_P")


#------------------1 method:- columns add------------------------------
Monsoon_P<-data.frame(colSums(df_mon_P[c(6,7,8,9), ]))
Post_Monsoon_P<-c(colSums(df_mon_P[c(10,11,12), ]))
Winter_P<-c(colSums(df_mon_P[c(1,2,3), ]))
Summer_P<-c(colSums(df_mon_P[c(4,5), ]))

Monsoon_P<-cbind(Monsoon_P,Post_Monsoon_P,Winter_P,Summer_P)

colnames(Monsoon_P)<-c("Monsoon_P","Post_Monsoon_P","Winter_P","Summer_P")

head(Monsoon_P)
Seasonal_P<-Monsoon_P
head(Seasonal_P)

################ b) Stream Flow #################################
#------------------1 method:- for loop------------------------------
SeasonalDataQ=matrix(ncol=4,nrow=ncol(df_mon_Q))
for (icol in 1:ncol(df_mon_Q)){
  SeasonalDataQ[icol,1]=sum(df_mon_Q[1:3,icol])
  SeasonalDataQ[icol,2]=sum(df_mon_Q[4:5,icol])
  SeasonalDataQ[icol,3]=sum(df_mon_Q[6:9,icol])
  SeasonalDataQ[icol,4]=sum(df_mon_Q[10:12,icol])
}
colnames(SeasonalDataP)<-c("Winter_P","Summer_P","Monsoon_P","Post_Monsoon_P")

#------------------2 method:- By adding rows------------------------------
df_mon_Q
Monsoon_Q<-data.frame(colSums(df_mon_Q[c(6,7,8), ]))
Post_Monsoon_Q<-c(colSums(df_mon_Q[c(9,10,11), ]))
Winter_Q<-c(colSums(df_mon_Q[c(12,1,2), ]))
Summer_Q<-c(colSums(df_mon_Q[c(3,4,5), ]))
Monsoon_Q<-cbind(Monsoon_Q,Post_Monsoon_Q,Winter_Q,Summer_Q)
colnames(Monsoon_Q)<-c("Monsoon_Q","Post_Monsoon_Q","Winter_Q","Summer_Q")
Seasonal_Q<-Monsoon_Q
head(Seasonal_Q)

hist(Seasonal_P$Monsoon_P)
boxplot(Seasonal_Q)

################ C) Evapotranspiration ########################
df_mon_E
Monsoon_E<-data.frame(colSums(df_mon_E[c(6,7,8), ]))
Post_Monsoon_E<-c(colSums(df_mon_E[c(9,10,11), ]))
Winter_E<-c(colSums(df_mon_E[c(12,1,2), ]))
Summer_E<-c(colSums(df_mon_E[c(3,4,5), ]))
Monsoon_E<-cbind(Monsoon_E,Post_Monsoon_E,Winter_E,Summer_E)
colnames(Monsoon_E)<-c("Monsoon_E","Post_Monsoon_E","Winter_E","Summer_E")
head(Monsoon_E)
Seasonal_E<-Monsoon_E

#------------------------------- maximum precipitation -----------------------------------#
################ 1) Maximum Precipitation ##############
mp=c()
for( i in min(manot_data$Year): max(manot_data$Year)){
  {
    mp_1<-max(manot_data[which(manot_data$Year==i),]$P)
    mp<-append(mp,mp_1)
  }
}
################ 2)Maximum Stream flow ##############
mq=c()
for( i in min(manot_data$Year): max(manot_data$Year)){
  {
    mq_1<-max(manot_data[which(manot_data$Year==i),]$Q)
    mq<-append(mq,mq_1)
  }
}
################ 3) Maximum Evapotranspiration ##############
me=c()
for( i in min(manot_data$Year): max(manot_data$Year)){
  {
    me_1<-max(manot_data[which(manot_data$Year==i),]$E)
    me<-append(me,me_1)
  }
}
################## Adding months and year to the data #################
Year<-c(1977:2017)
Year

#################3 converting data into data frame ##########3
mp_1<-matrix(unlist(mp),nrow=41,ncol=1)
mp_1<-data.frame(mp_1)

mq_1<-matrix(unlist(mq),nrow=41,ncol=1)
mq_1<-data.frame(mq_1)

me_1<-matrix(unlist(me),nrow=41,ncol=1)
me_1<-data.frame(me_1)

Year_1<-matrix(unlist(Year),nrow=41,ncol=1)
Year_1<-data.frame(Year_1)  

############# combining the data frame maximum  ####################### 
maximum_data<-cbind(Year_1,mp_1,mq_1,me_1)
colnames(maximum_data)<-c("Year","Maximum yearly P","Maximum Yearly Q", "Maximum yearly ET")

par(mfrow=c(2,3))

#Histogram of Max. Yearly
Hist_max_P<-hist(maximum_data$`Maximum yearly P`, main = "Max Yearly Rainfall", xlab = "Annual Maximum Rainfall", col = "skyblue")  
Hist_max_P<-hist(maximum_data$`Maximum Yearly Q`, main = "Max Yearly Stream flow", xlab = "Annual Maximum Stream Flow", col = "orange")  
Hist_max_P<-hist(maximum_data$`Maximum yearly ET`, main = "Max Yearly Evapotranspiration", xlab = "Annual Maximum ET", col = "light green")  

#BOXPlot of Yearly Maximum
BP_max_P<-boxplot(maximum_data$`Maximum yearly P`, main = "Max Yearly Rainfall", col="skyblue" )        
BP_max_P<-boxplot(maximum_data$`Maximum Yearly Q`, main = "Yearly Stream flow", col="orange" )        
BP_max_P<-boxplot(maximum_data$`Maximum yearly ET`, main = "Yearly Evapotranspiration", col="light green" )        

############################# graph ##########################
############################ 1!11111111111111111111111111111111111111111111111111
par(mfrow=c(2,3))

boxplot(manot_data$P, main="Precipitation",col="skyblue")
boxplot(manot_data$E, main="Evapotranspiration",col="orange")
boxplot(manot_data$Q, main="Stream Flow",col="light green")

hist(manot_data$P, main="Precipitation",xlab="Precipitation",col="skyblue")
hist(manot_data$E, main="Evapotranspiration",xlab="Evapotranspiration",col="orange")
hist(manot_data$Q, main="Stream Flow",xlab="Stream Flow",col="light green")

mtext(" Boxplot and the Histogram of Daily Manot Data ",side=3,col="Red",line=-1.5,outer=TRUE)

max(manot_data$P)
max(manot_data$E)
max(manot_data$Q)

###############################################################
par(mfrow=c(3,2))
################# adding line to the histogram ###############
hist(manot_data$P, xlab="Precipitation", main = "Precipitation",col="skyblue")
hist(manot_data$P, xlab="Precipitation", main = "Precipitation",probability = TRUE,col="skyblue" )
lines(density(manot_data$P))

hist(manot_data$Q, xlab="Stream flow ", main = "Stream flow" ,col="orange")
hist(manot_data$Q, xlab="Stream flow ", main = "Stream flow", probability = TRUE,col="orange" )
lines(density(manot_data$Q))

hist(manot_data$E, xlab="Evaporation", main = "Evaporation" ,col="light green")
hist(manot_data$E, xlab="Evaporation", main = "Evaporation",probability = TRUE  )
lines(density(manot_data$E))

mtext("Manot Daily Data Trend on Histogram ",side=3,col="Red",line=-1.5,outer=TRUE)

################## scatterplot ##################################
par(mfrow=c(1,3))
plot(manot_data$P, manot_data$Q,col="red", xlab = "Precipitation", ylab="Streamflow ")
plot(manot_data$P, manot_data$E,col="orange", xlab = "Precipitation", ylab="Evapotranspiration ")
plot(manot_data$E,manot_data$Q,col="blue", xlab = "Evapotranspiration", ylab="Streamflow ")
mtext("Manot Daily Data ",side=3,col="Black",line=-2,outer=TRUE)

cor(manot_data$P, manot_data$Q,method="pearson")

###################### 1) Monthly data graph ##############################3
############## a) PLotting Precipitation V/s Streamflow ###########################
par(mfrow=c(4,3))
for(i in 1:12){
  df_7<-TimeSeries[which(TimeSeries$month==i),]
  plot(df_7$P,df_7$Q,col="blue",xlab="Precipitation",ylab="Streamflow",pch=19,main=month.abb[i])
}
mtext("Manot Monthly Data of Precipitation and Streamflow ",side=3,col="red",line=-1.5,outer=TRUE)

################## b) Plotting PET V/s Streamflow ##############################
par(mfrow=c(4,3))
for(i in 1:12){
  df_7<-TimeSeries[which(TimeSeries$month==i),]
  plot(df_7$E,df_7$Q,col="red",xlab="PET",ylab="Streamflow",pch=19,main=month.abb[i])
}
mtext("Manot Monthly Data of Evapotranspiration and Streamflow ",side=3,col="blue",line=-1.5,outer=TRUE)

################## c) Plotting Precipitation V/s Evapotranspiration ##############################
par(mfrow=c(4,3))
for(i in 1:12){
  df_7<-TimeSeries[which(TimeSeries$month==i),]
  plot(df_7$P,df_7$Q,col="orange",xlab="Precipitation",ylab="PET",pch=19,main=month.abb[i])
}
mtext("Manot Monthly Data of Precipitation and Evapotranspiration ",side=3,col="Blue",line=-1.5,outer=TRUE)

################### 2) Seasonal graph ########################################
############### 1) Plot between Precipitation v/s Streamflow #################
par(mfrow=c(2,2))
for(i in 1:4){
  df_8<-Seasonal_P[ ,i]
  df_9<-Seasonal_Q[ ,i]
  plot(df_8,df_9,col="blue",xlab="Precipitation",ylab="Stream flow",pch=19)
}
mtext("Manot Seasonal Data of Precipitation & Discharge",side=3,col="red",line=-2,outer=TRUE)

########################### OR #########################
par(mfrow=c(2,2))
plot(Seasonal_P$Monsoon_P,Seasonal_Q$Monsoon_Q,col="blue",xlab="Precipitation",ylab="Stream flow",pch=19,main="Monsoon")
plot(Seasonal_P$Post_Monsoon_P,Seasonal_Q$Post_Monsoon_Q,col="blue",xlab="Precipitation",ylab="Stream flow",pch=19,main="Post Monsoon")
plot(Seasonal_P$Winter_P,Seasonal_Q$Winter_Q,col="blue",xlab="Precipitation",ylab="Stream flow",pch=19,main="Winter")
plot(Seasonal_P$Summer_P,Seasonal_Q$Summer_Q,col="blue",xlab="Precipitation",ylab="Stream flow",pch=19,main="Summer")
mtext("Manot Seasonal Data of Precipitation & Discharge",side=3,col="red",line=-1.5,outer=TRUE)

############### 2) Plot between Precipitation v/s Evapotranspiration  #################
par(mfrow=c(2,2))
for(i in 1:4){
  df_10<-Seasonal_P[ ,i]
  df_11<-Seasonal_E[ ,i]
  plot(df_10,df_11,col="blue",xlab="Precipitation",ylab="Evapotransiration",pch=19)
}
mtext("Manot Seasonal Data of Precipitation & Evapotranspiration",side=3,col="red",line=-2,outer=TRUE)

########################### OR #########################
par(mfrow=c(2,2))
plot(Seasonal_P$Monsoon_P,Seasonal_E$Monsoon_E,col="Orange",xlab="Precipitation",ylab="Evapotransiration",pch=19,main="Monsoon")
plot(Seasonal_P$Post_Monsoon_P,Seasonal_E$Post_Monsoon_E,col="Orange",xlab="Precipitation",ylab="Evapotransiration",pch=19,main="Post Monsoon")
plot(Seasonal_P$Winter_P,Seasonal_E$Winter_E,col="Orange",xlab="Precipitation",ylab="Evapotransiration",pch=19,main="Winter")
plot(Seasonal_P$Summer_P,Seasonal_E$Summer_E,col="orange",xlab="Precipitation",ylab="Evapotransiration",pch=19,main="Summer")
mtext("Manot Seasonal Data of Precipitation & Evapotranspiration",side=3,col="Red",line=-1.5,outer=TRUE)

############### 3) Plot between Evapotranspiration v/s Streamflow #################
par(mfrow=c(2,2))
for(i in 1:4){
  df_12<-Seasonal_E[ ,i]
  df_13<-Seasonal_Q[ ,i]
  plot(df_12,df_13,col="blue",xlab="Evapotranspiration",ylab="Stream flow",pch=19)
}
mtext("Manot Seasonal Data of Evapotranspiration & Streamflow",side=3,col="red",line=-2,outer=TRUE)

########################### OR #########################
par(mfrow=c(2,2))
plot(Seasonal_E$Monsoon_E,Seasonal_Q$Monsoon_Q,col="blue",xlab="Evapotranspiration",ylab="Stream flow",pch=19,main="Monsoon")
plot(Seasonal_E$Post_Monsoon_E,Seasonal_Q$Post_Monsoon_Q,col="blue",xlab="Evapotranspiration",ylab="Stream flow",pch=19,main="Post Monsoon")
plot(Seasonal_E$Winter_E,Seasonal_Q$Winter_Q,col="blue",xlab="Evapotranspiration",ylab="Stream flow",pch=19,main="Winter")
plot(Seasonal_E$Summer_E,Seasonal_Q$Summer_Q,col="blue",xlab="Evapotranspiration",ylab="Stream flow",pch=19,main="Summer")
mtext("Manot Seasonal Data of Evapotranspiration & Streamflow",side=3,col="red",line=-1.5,outer=TRUE)

####################### NORMAL DISTRIBUTION #################################
###################### 1)Precipitation ############################
norm_dist_P<-dnorm(manot_data$P,mean = 3.365229, sd=8.725123)
plot(manot_data$P,norm_dist_P,xlab ="Precipitation", ylab="f(x)", main="Normal Distribution Precipitation", col="red")

################### fitting of normal distribution ##################
corrected_P<-log(manot_data$P)
corrected_norm_dist_P<-dnorm(corrected_P,mean =3.365229 , sd=8.725123)
plot(corrected_P,corrected_norm_dist_P,xlab ="b", ylab="f(x)", main="Fit Normal Distribution _Precipitation")


###################### 2)Stream flow ############################
norm_dist_Q<-dnorm(manot_data$Q,mean = 0, sd=1)
plot(manot_data$Q,norm_dist_Q,xlab ="Stream flow", ylab="f(x)", main="Normal Distribution Stream flow ", col="Blue")

################### fitting of normal distribution ##################
corrected_Q<-log(manot_data$Q)
corrected_norm_dist_Q<-dnorm(corrected_Q,mean = 0, sd=1)
plot(corrected_Q,corrected_norm_dist_Q,xlab ="b", ylab="f(x)", main="Fit Normal Distribution Streamflow")


###################### 3) Evapotranspiration ############################
norm_dist_E<-dnorm(manot_data$E,mean = 0, sd=1)
plot(manot_data$E,norm_dist_E,xlab ="ET", ylab="f(x)", main="Normal Distribution ET", col="Yellow")

################### fitting of normal distribution ##################
corrected_E<-(manot_data$E)
corrected_norm_dist_E<-dnorm(corrected_E,mean = 0, sd=1)
plot(corrected_E,corrected_norm_dist_E,xlab ="ET", ylab="f(x)", main="Fit Normal Distribution ET")

mtext("Manot Daily Data ",side=3,col="Red",line=-1.5,outer=TRUE)

############## normal distribution for monthly data ##############
#par(mfrow=c(3,2))
###################### 1)Precipitation ############################
#norm_dist_P_month<-dnorm(Monthly_dataframe_P$P,mean = 0, sd=1)
#plot(manot_data$P,norm_dist_P,xlab ="Precipitation", ylab="f(x)", main="Normal Distribution Precipitation")

################### fitting of normal distribution ##################
#corrected_P<-log(Monthly_dataframe_P$`1977`)
#corrected_norm_dist_P<-dnorm(corrected_P,mean = 0, sd=1)
#plot(corrected_P,corrected_norm_dist_P,xlab ="b", ylab="f(x)", main="Fit Normal Distribution _Precipitation")


######################### 1) weibull Distribution ############################3
pmp<-maximum_data$`Maximum yearly P`

pmp_sort_dec<-sort(pmp,decreasing = TRUE)
pmp_sort_dec

pmp_sort_asc<-sort(pmp,decreasing = FALSE)
pmp_sort_asc

Rank_pmp_sort_asc<- rank(pmp_sort_asc)
Rank_pmp_sort_asc

weibull_dist_Prob<-Rank_pmp_sort_asc/length(pmp_sort_dec+1)
weibull_dist_Prob

##################### a) Normal distribution #############
#------------------ 1) maximum precipitation ----------------------
mean(mp)
sd(mp)
mp
norm_dist_mp<-dnorm(mp,mean = 73.82795, sd=23.28218)
plot(mp,norm_dist_mp,xlab ="Precipitation", ylab="f(x)", main="Normal Distribution of Maximum Yearl Precipitation", col="red")

#------------------ 2) maximum Streamflow ----------------------
mean(mq)
sd(mq)
mq
norm_dist_mq<-dnorm(mq,mean = 52.33074, sd=30.70647)
plot(mq,norm_dist_mq,xlab ="Streamflow", ylab="f(x)", main="Normal Distribution of maximum Streamflow",col="blue")

#------------------ 3) maximum Evapotranspiration ----------------------
mean(me)
sd(me)
me
norm_dist_me<-dnorm(mp,mean = 73.82795, sd=23.28218)
plot(me,norm_dist_me,xlab ="Evapotranspiration", ylab="f(x)", main="Normal Distribution of maximum annual Evapotranspiration", col="purple")

max(me)
min(me)
######################### b) weibull Distribution ############################
#----------------------1)Precipitation ---------------------------------
mp_sort_dec<-sort(maximum_data$`Maximum yearly P`,decreasing = TRUE)
mp_sort_dec

mp_sort_asc<-sort(maximum_data$`Maximum yearly P`,decreasing = FALSE)
mp_sort_asc

Rank_mp_sort_asc<- rank(mp_sort_asc)
Rank_mp_sort_asc

weibull_dist_Probability<-Rank_mp_sort_asc/length(mp_sort_dec+1)
weibull_dist_Probability

length(weibull_dist_Probability)

return_period<-c(1/weibull_dist_Probability)
return_period

#WeibullsGraph/ExceedenceProbability

df_weibull <- cbind(mp_sort_dec,Rank_mp_sort_asc,return_period,weibull_dist_Probability)
df_weibull
df_weibull <- as.data.frame(df_weibull)
class(df_weibull)

par(mfrow=c(2,1))
plot(df_weibull$return_period, df_weibull$mp_sort_dec, xlab="Return Period(Years)", ylab="Annual Maximum Precipitation", main = "Return Period vs Annual Maximum Precipitation", col = "red")
plot(df_weibull$weibull_dist_Probability, df_weibull$mp_sort_dec, xlab="Probability of Exceedence", ylab="Annual Maximum Precipitation", main = "Exceedence Probability vs Annual Maximum Precipitation", col = "dark blue")

#----------------------2) Streamflow ---------------------------------
mq_sort_dec<-sort(maximum_data$`Maximum Yearly Q`,decreasing = TRUE)
mq_sort_dec

mq_sort_asc<-sort(maximum_data$`Maximum Yearly Q`,decreasing = FALSE)
mq_sort_asc

Rank_mq_sort_asc<- rank(mq_sort_asc)
Rank_mq_sort_asc

weibull_dist_Probability_Q<-Rank_mq_sort_asc/length(mq_sort_dec+1)
weibull_dist_Probability_Q

return_period_Q<-c(1/weibull_dist_Probability_Q)
return_period_Q

#WeibullsGraph/ExceedenceProbability
df_weibull_Q <- cbind(mq_sort_dec,Rank_mq_sort_asc,return_period_Q,weibull_dist_Probability_Q)
df_weibull_Q
df_weibull_Q <- as.data.frame(df_weibull_Q)
class(df_weibull_Q)

par(mfrow=c(2,1))
plot(df_weibull_Q$return_period_Q, df_weibull_Q$mq_sort_dec, xlab="Return Period(Years)", ylab="Annual Maximum Stream flow", main = "Return Period vs Annual Maximum Streamflow", col = "red")
plot(df_weibull_Q$weibull_dist_Probability_Q, df_weibull_Q$mq_sort_dec, xlab="Probability of Exceedence", ylab="Annual Maximum Streamflow", main = "Exceedence Probability vs Annual Maximum Streamflow", col = "dark blue")


#----------------------2) Evapotranspiration  ---------------------------------
me_sort_dec<-sort(maximum_data$`Maximum yearly ET`,decreasing = TRUE)
me_sort_dec

me_sort_asc<-sort(maximum_data$`Maximum yearly ET`,decreasing = FALSE)
me_sort_asc

Rank_me_sort_asc<- rank(me_sort_asc)
Rank_me_sort_asc

weibull_dist_Probability_E<-Rank_me_sort_asc/length(me_sort_dec+1)
weibull_dist_Probability_E

return_period_E<-c(1/weibull_dist_Probability_E)
return_period_E

#WeibullsGraph/ExceedenceProbability
df_weibull_E <- cbind(me_sort_dec,Rank_me_sort_asc,return_period_E,weibull_dist_Probability_E)
df_weibull_E
df_weibull_E <- as.data.frame(df_weibull_E)
class(df_weibull_E)

par(mfrow=c(2,1))
plot(df_weibull_E$return_period_E, df_weibull_E$me_sort_dec, xlab="Return Period(Years)", ylab="Annual Maximum Evapotranspiration", main = "Return Period vs Annual Maximum Evapotranspiration", col = "red")
plot(df_weibull_E$weibull_dist_Probability_E, df_weibull_E$me_sort_dec, xlab="Probability of Exceedence", ylab="Annual Maximum Evapotranspiration", main = "Exceedence Probability vs Annual Maximum Evapotranspiration", col = "dark blue")

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------
#--------------
###################### Annual Time Series Plots ##############################
par(mfrow=c(3,1))

#---------------a)Precipitation--------------------
plot(Year,Yearly_dataframe_P$`Annual Precipitation`, 
     xlab = "Year", ylab = "Annual Precipitation", type="l", 
     main = "Plot Showing Time Series of Annual Precipitation", col=" blue")
abline(h=mean(Yearly_dataframe_P$`Annual Precipitation`), col="red")

#--------------b)Streamflow---------------------
plot(Year,Yearly_dataframe_Q$`Annual STREAM FLOW`, 
     xlab = "Year", ylab = "Annual Streamflows", type="l", 
     main = "Plot Showing Time Series of Annual Streamflows", col=" red")
abline(h=mean(Yearly_dataframe_Q$`Annual STREAM FLOW`), col="red")

summary(Yearly_dataframe_P)
boxplot(Yearly_dataframe_P)



#--------------b)EVAPOTRANSPIRATION--------------------
plot(Year,Yearly_dataframe_E$`Annual EVAPOTRANSPIRATION`, 
     xlab = "Year", ylab = "Annual Evaotranspiration", type="l", 
     main = "Plot Showing Time Series of Annual Evapotranspiration", col="Dark blue")
abline(h=mean(Yearly_dataframe_E$`Annual EVAPOTRANSPIRATION`), col="red")

#################### Daily Timeseries plot ##############################

#--------------------------- Histogram --------------------------
par(mfrow=c(3,1))
hist(manot_data$P, xlab = "Daily Precipitation", ylab ="Frequency", main = "Histogram of Daily Precipitation", col="blue")
hist(manot_data$Q, xlab="Daily Streamflow", ylab="Frequency", main = "Histogram of Daily Stream flow",col="skyblue")
hist(manot_data$E, xlab ="Daily Potential Evapotranspiration", ylab="Frequency", 
     main = "Histogram of Daily Potential Evapotranspiration",col="red")
par(mfrow=c(3,1))

#---------------a)Precipitation--------------------
length(manot_data$P)
days<-c(1:14975)
plot(days,manot_data$P,
     xlab = "Days", ylab = "Daily Precipitation", type="l", 
     main = "Plot Showing Time Series of Daily Precipitation", col=" blue")
abline(h=mean(manot_data$P), col="red")

#--------------b)Streamflow---------------------
length(manot_data$Q)
days<-c(1:14975)
plot(days,manot_data$Q,
     xlab = "Days", ylab = "Daily Streamflow", type="l", 
     main = "Plot Showing Time Series of Daily Streamflow", col=" Red")
abline(h=mean(manot_data$Q), col="blue")

#---------------C)EVAPOTRANSPIRATION--------------------
length(manot_data$E)
days<-c(1:14975)
plot(days,manot_data$E,
     xlab = "Days", ylab = "Daily EVAPOTRANSPIRATION", type="l", 
     main = "Plot Showing Time Series of Daily EVAPOTRANSPIRATION", col="blue")
abline(h=mean(manot_data$E), col="red")


###################### monthly P Time Series Plots ##############################
install.packages("reshape2")                 
library("reshape2") 

install.packages("ggplot2")                  
library("ggplot2")

dim(df_mon_P)
colnames(df_mon_P)<-1977:2017
Months<-c(1:12)

Monthly_data_months_P<-data.frame(Months,df_mon_P)

data_month_long_P <- melt(Monthly_data_months_P, id.vars = "Months")    
head(data_month_long_P)

ggp_P<-ggplot(data_month_long_P,                            
              aes(x = Months,
                  y = value,
                  col = variable)) +
  geom_line()+geom_point()

ggp_P +
  xlab("Months")+
  ylab("Precipitation")+
  ggtitle("Monthly Precipitation")+
  theme(plot.title = element_text(size=20))

######################  monthly  Q Time Series Plots ##############################
dim(df_mon_Q)
colnames(df_mon_Q)<-1977:2017
Months<-c(1:12)

Monthly_data_months_Q<-data.frame(Months,df_mon_Q)

data_month_long_Q <- melt(Monthly_data_months_Q, id.vars = "Months")    
head(data_month_long_Q)

ggp_P<-ggplot(data_month_long_Q,                            
              aes(x = Months,
                  y = value,
                  col = variable)) +
  geom_line()+geom_point()

ggp_P +
  xlab("Months")+
  ylab("Streamflow")+
  ggtitle("Monthly Streamflow")+
  theme(plot.title = element_text(size=20))

###################### monthly  ET Time Series Plots ##############################
dim(df_mon_E)
colnames(df_mon_E)<-1977:2017
Months<-c(1:12)

Monthly_data_months_E<-data.frame(Months,df_mon_E)

data_month_long_E <- melt(Monthly_data_months_E, id.vars = "Months")    
head(data_month_long_E)

ggp_P<-ggplot(data_month_long_E,                            
              aes(x = Months,
                  y = value,
                  col = variable)) +
  geom_line()+geom_point()

ggp_P +
  xlab("Months")+
  ylab("Evapotranspiration")+
  ggtitle("Monthly evapotranspiration")+
  theme(plot.title = element_text(size=20))

summary(Monthly_dataframe_P)
summary(Monthly_dataframe_Q)
summary(Monthly_dataframe_E)
summary(Seasonal_P)
summary(Seasonal_Q)
summary(Seasonal_E)
summary(Yearly_dataframe_P)
summary(Yearly_dataframe_Q)
summary(Yearly_dataframe_E)
###################### Seasonal Time Series Plots ##############################
#---------------a)Precipitation--------------------
Years<-c(1977:2017)
seasonal_data_year<-data.frame(Years,Seasonal_P)

############ 1) base plot method ############33
plot(seasonal_data_year$Years,                              
     seasonal_data_year$Monsoon_P,
     type = "l",
     col = 2,
     xlab = "Year",
     ylab = "Seasonal Precipitation")
lines(seasonal_data_year$Years,                             
      seasonal_data_year$Post_Monsoon_P,
      type = "l",
      col = 3)
lines(seasonal_data_year$Years,                             
      seasonal_data_year$Winter_P,
      type = "l",
      col = 4)
lines(seasonal_data_year$Years,                             
      seasonal_data_year$Summer_P,
      type = "l",
      col = 5)
legend("topright",                           
       c("Monsoon_P", "Post_Monsoon_P", "Winter_P","Summer_P"),
       lty = 1,
       col = 2:5)

########  2nd method - ggplot2 ####################
install.packages("reshape2")                 
library("reshape2") 
install.packages("ggplot2")                  
library("ggplot2")

Years<-c(1977:2017)
seasonal_data_year_P<-data.frame(Years,Seasonal_P)

data_long_P <- melt(seasonal_data_year_P, id.vars = "Years")    
head(data_long_P)

ggp_P<-ggplot(data_long_P,                            
       aes(x = Years,
           y = value,
           col = variable)) +
  geom_line()+geom_point()

ggp_P +
  xlab("Years")+
  ylab("Precipitation")+
  ggtitle("Seasonal Precipitation")+
  theme(plot.title = element_text(size=20))

################################################################################
#---------------b) Streamflow--------------------
Years<-c(1977:2017)
seasonal_data_year_Q<-data.frame(Years,Seasonal_Q)

data_long_Q <- melt(seasonal_data_year_Q, id.vars = "Years")    
head(data_long_Q)

ggp_Q<-ggplot(data_long_Q,                            
              aes(x = Years,
                  y = value,
                  col = variable)) +
  geom_line()+geom_point()

ggp_Q +
  xlab("Years")+
  ylab("Streamflow")+
  ggtitle("Seasonal Streamflow")+
  theme(plot.title = element_text(size=20))

#---------------C) Evapotranspiration--------------------
Years<-c(1977:2017)
seasonal_data_year_E<-data.frame(Years,Seasonal_E)

data_long_E <- melt(seasonal_data_year_E, id.vars = "Years")    
head(data_long_E)

ggp_E<-ggplot(data_long_E,                            
              aes(x = Years,
                  y = value,
                  col = variable)) +
  geom_line()+geom_point()

ggp_E +
  xlab("Years")+
  ylab("Evapotranspiration")+
  ggtitle("Seasonal Evapotranspiration")+
  theme(plot.title = element_text(size=20))


#################################
#1) Seasonal
boxplot(Seasonal_P,xlab="Seasons",ylab="Precipitation",col="skyblue")
mtext("Manot Seasonal Data of Precipitation ",side=3,col="Red",line=-2,outer=TRUE)

boxplot(Seasonal_Q,xlab="Seasons",ylab="Stream flow",col="yellow")
mtext("Manot Seasonal Data of Stream flow ",side=3,col="Red",line=-2,outer=TRUE)

boxplot(Seasonal_E,xlab="Seasons",ylab="Evapotranspiration",col="green")
mtext("Manot Seasonal Data of Evapotranspiration ",side=3,col="Red",line=-2,outer=TRUE)

#b)Yearly
boxplot(df_mon_P,xlab="Years",ylab="Precipitation",col="skyblue")
mtext("Manot Yearly Data of Precipitation ",side=3,col="Red",line=-2,outer=TRUE)

boxplot(df_mon_Q,xlab="Years",ylab="Stream flow",col="yellow")
mtext("Manot Yearly Data of Stream flow ",side=3,col="Red",line=-2,outer=TRUE)

boxplot(df_mon_E,xlab="Years",ylab="Evapotranspiration",col="green")
mtext("Manot Yearly Data of Evapotranspiration ",side=3,col="Red",line=-2,outer=TRUE)

#c)monthly
boxplot(t(df_mon_P),xlab="Months",ylab="Precipitation",col="skyblue")
mtext("Manot Monthly Data of Precipitation ",side=3,col="Red",line=-2,outer=TRUE)

boxplot(t(df_mon_Q),xlab="Months",ylab="Stream flow",col="yellow")
mtext("Manot Monthly Data of Stream flow ",side=3,col="Red",line=-2,outer=TRUE)

boxplot(t(df_mon_E),xlab="Months",ylab="Evapotranspiration",col="green")
mtext("Manot Monthly Data of Evapotranspiration ",side=3,col="Red",line=-2,outer=TRUE)

#D)Daily
manot_PQE<-data.frame(manot_data$P,manot_data$Q,manot_data$E)
boxplot(manot_PQE, col="orange",xlab="Days",ylab="Measured data in mm/Day")
mtext("Manot Daily Data ",side=3,col="Red",line=-2,outer=TRUE)

################-------------ENTROPY--------------###########################
#----------------------Shannon Entropy--------------------------------------#
#                        Entropy = −Σpi log pi                               #

############ 1) Method ########################################
Entropy_P<-data.frame(table(manot_data$P))
Entropy_P$Probability <- table(manot_data$P)/length(manot_data$P)
Entropy_P$entopy_vec <- as.data.frame(Entropy_P$Probability)[,2]
Entropy_value<- -sum(Entropy_P$Probability* log(Entropy_P$Probability))
Entropy_value

############ 2) method by function ######################
entropy <- function(Data) {
  freq <- table(Data)/length(Data)
  vec <- as.data.frame(freq)[,2]
  vec<-vec[vec>0]
  -sum(vec * log(vec))
}

########### 1) Entropy Of Daily scale ###########################
entropy(manot_data$P)
entropy(manot_data$Q)
entropy(manot_data$E)

entropy(maximum_data$`Maximum yearly P`)
entropy(maximum_data$`Maximum Yearly Q`)
entropy(maximum_data$`Maximum yearly ET`)

########## Method of Entropy Calculation is by assigning variable#####
freq <- table(maximum_data$`Maximum yearly P`)/length(maximum_data$`Maximum yearly P`)
vec <- as.data.frame(freq)[,2]
#vec<-vec[vec>0]
-sum(vec * log(vec))

#------------------------ entropy Daily data plot ------------------------
DA<-entropy(manot_data$P)
DQ<-entropy(manot_data$Q)
DE<-entropy(manot_data$E)
Entropy_daily<-c(DA,DQ,DE)

Entropy_daily_df<-data.frame(a1,Entropy_daily)


plot(Entropy_daily,xlab="P, Q, ET", ylab="Daily Entropy", pch=16,col=1:3,cex=2)
legend("topleft",
       legend=c("Precipitation","Streamflow","PET"),
       col=c("black","red","green"),
       pch=16)

#------------------------2) entropy maximum Yearly data ------------------------
myp<-entropy(maximum_data$`Maximum yearly P`)
myq<-entropy(maximum_data$`Maximum Yearly Q`)
mye<-entropy(maximum_data$`Maximum yearly ET`)
Entropy_max_yearly<-c(myp,myq,mye)

plot(Entropy_max_yearly,xlab="P, Q, ET", ylab="Maximum Yearly Entropy", pch=16,col=1:3,cex=2)
legend("top",
       legend=c("Precipitation","Streamflow","PET"),
       col=c("black","red","green"),
       pch=16)

#-------
#------------------------3) entropy Yearly data ------------------------
Y_P<-entropy(Yearly_dataframe_P$`Annual Precipitation`)
Y_Q<-entropy(Yearly_dataframe_Q$`Annual STREAM FLOW`)
Y_E<-entropy(Yearly_dataframe_E$`Annual EVAPOTRANSPIRATION`)
Entropy_yearly<-c(Y_P,Y_Q,Y_E)

plot(Entropy_yearly,xlab="P, Q, ET", ylab="Yearly Entropy", pch=16,col=1:3,cex=2)
legend("top",
       legend=c("Precipitation","Streamflow","PET"),
       col=c("black","red","green"),
       pch=16)


# Entropy 
# 1- Daily, 2- yearly data, 3-maximum yearly data




