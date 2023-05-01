
rm(list = ls())
####### Setting the working Directory ###########################

setwd("C:/Users/CIVIL-IITH/Desktop")

##### reading the data as dataframe #############
df_1<-read.csv("Manot.csv") 
df_1

## The Data Consists of three Variables: P, PET and Q in units mm/day, mm/day and m3/sec respectively ##


### The Area of Manot CA = 4968 Sq.Km ##

### Converting the Units of Discharge from m3/sec to mm/day ####

df_1$Q <- df_1$Q*1000*24*3600/(4968*(10^6))

####   Checking for NA Values in #####

is.na(df_1)           ###NA values are present in the Data ###
which((is.na(df_1)))  ###Checking which values are NA's ### 

### 40 Records are found to be filled with NA's in the flows Dataset####


### Replacing NA Values with Mean ###

df_1$Q[is.na(df_1$Q)]<-mean(df_1$Q,na.rm=TRUE)
df_1
head(df_1)

### Again Rechecking for NA Values in Data frame ###

is.na(df_1)          ###NA values are replaced with Mean in the Data ###
                       ###integer(0)###

### No Records are found to be with NA's ###


#####Data Range####

range(df_1$P)
range(df_1$Q)
range(df_1$E)

##### converting date and time #####################

df_1$Date<-as.Date(df_1$Date,format="%d-%m-%Y")
df_1$day<-as.numeric(format(df_1$Date,'%d'))
df_1$month<-as.numeric(format(df_1$Date,"%m"))
df_1$year<-as.numeric(format(df_1$Date,"%Y"))

head(df_1)
#### Period of Record (YEARS)###

POR <- (max(df_1$year)-min(df_1$year))+1

class(df_1$Date)   # For checking the class of df_1$Date



####### Creating vectors for Extracting Monthly Data & Annual Max. Rainfall ###########################
s_1=c()
s_2=c()
s_3=c()
s_4=c()
s_5=c()

############## Agrregating the monthly precipitation data ###################
for( i in min(df_1$year): max(df_1$year)){
  for( j in 1:12){
    d1<-sum(df_1[which(df_1$year==i & df_1$month==j),]$P)
    s_1<-append(s_1,d1)
  }
}

####------------------------Weibulls Distribution---------------#####

### Analysing Maximum Precipitation ####
s_1maxP=c()

############## Extracting the Maximum Daily precipitation data ###################
for( i in min(df_1$year): max(df_1$year)){
  
    d1_max<-max(df_1[which(df_1$year==i),]$P)
    s_1maxP<-append(s_1maxP,d1_max)
  
}

s_1maxP 

HS_maxP<-hist(s_1maxP, main = "histogram of Max Yearly Rainfall", xlab = "Annual Maximum Rainfall(mm/day)", col = "red")  #Histogram of Max. YearlyRainfall
BP_maxP<-boxplot(s_1maxP, main = " Boxplot of Max Yearly Rainfall(mm/day)", col="red" )        #BOXPlotofYearlyMaximumRainfall


sortedData_inc_maxP <- sort(s_1maxP,decreasing = FALSE)   #WeibullsGraph/ExceedenceProbability
sortedData_inc_maxP

Y_rank <- rank(sortedData_inc_maxP)



pr_maxP<- Y_rank/(length(sortedData_inc_maxP)+1)      #Probability
pr_maxP

length(pr_maxP)
rp_maxP <- c(1/pr_maxP)                              #ReturnPeriod
rp_maxP

sortedData_desc_maxP <- sort(s_1maxP,decreasing = TRUE)   #WeibullsGraph/ExceedenceProbability
sortedData_desc_maxP

df_weibull_maxP <- cbind(sortedData_desc_maxP, Y_rank, rp_maxP, pr_maxP)
df_weibull_maxP

df_weibull_maxP <- as.data.frame(df_weibull_maxP)
class(df_weibull_maxP)

par(mfrow=c(2,1))

plot(df_weibull_maxP$rp_maxP, df_weibull_maxP$sortedData_desc_maxP, xlab="Return Period(Years)", ylab="Annual Maximum Precipitation", main = "Return Period vs Annual Maximum Precipitation", col = "red")

plot(df_weibull_maxP$pr_maxP, df_weibull_maxP$sortedData_desc, xlab="Probability of Exceedence", ylab="Annual Maximum Precipitation", main = "Exceedence Probability vs Annual Maximum Precipitation", col = "dark blue")


  ### Analysing Maximum Runoff, Design Flood ####
s_1maxQ=c()

############## Extracting the Maximum Daily precipitation data ###################
for( i in min(df_1$year): max(df_1$year)){
  
  d1_max<-max(df_1[which(df_1$year==i),]$Q)
  s_1maxQ<-append(s_1maxQ,d1_max)
  
}

s_1maxQ 

HS_maxQ<-hist(s_1maxQ, main = "histogram of Max Yearly Runoff", xlab = "Annual Maximum Rainfall", col = "red")  #Histogram of Max. YearlyRainfall
BP_maxQ<-boxplot(s_1maxQ, main = " Boxplot of Max Yearly Runoff", col="red" )        #BOXPlotofYearlyMaximumRainfall


sortedData_inc_maxQ <- sort(s_1maxQ,decreasing = FALSE)   #WeibullsGraph/ExceedenceProbability
sortedData_inc_maxQ

Y_rankQ <- rank(sortedData_inc_maxQ)



pr_maxQ<- Y_rankQ/(length(sortedData_inc_maxQ)+1)      #Probability
pr_maxQ

length(pr_maxQ)
rp_maxQ <- c(1/pr_maxQ)                              #ReturnPeriod
rp_maxQ

sortedData_desc_maxQ <- sort(s_1maxQ,decreasing = TRUE)   #WeibullsGraph/ExceedenceProbability
sortedData_desc_maxQ

df_weibull_maxQ <- cbind(sortedData_desc_maxQ, Y_rankQ, rp_maxQ, pr_maxQ)
df_weibull_maxQ

df_weibull_maxQ <- as.data.frame(df_weibull_maxQ)
class(df_weibull_maxQ)

par(mfrow=c(2,1))

plot(df_weibull_maxQ$rp_maxQ, df_weibull_maxP$sortedData_desc_maxP, xlab="Return Period(Years)", ylab="Annual Maximum Runoff(mm/day)", main = "Return Period vs Annual Maximum Runoff", col = "red")

plot(df_weibull_maxQ$pr_maxQ, df_weibull_maxP$sortedData_desc, xlab="Probability of Exceedence", ylab="Annual Maximum Runoff(mm/day)", main = "Exceedence Probability vs Annual Maximum Runoff", col = "dark blue")



###---------------------------------------------------------######
############## Agrregating the monthly PET data ###################
for( i in min(df_1$year): max(df_1$year)){
  for( j in 1:12){
    d2<-sum(df_1[which(df_1$year==i & df_1$month==j),]$E)
    s_2<-append(s_2,d2)
  }
}
############## Agrregating the monthly Streamflow data ###################
for( i in min(df_1$year): max(df_1$year)){
  for( j in 1:12){
    d3<-mean(df_1[which(df_1$year==i & df_1$month==j),]$Q)
    s_3<-append(s_3,d3)
  }
}
############## Converting the above data into data frames #####################
df_3<-matrix(unlist(s_2),nrow=492,ncol=1)
df_3<-data.frame(df_3)

df_2<-matrix(unlist(s_1),nrow=492,ncol=1)
df_2<-data.frame(df_2)

df_4<-matrix(unlist(s_3),nrow=492,ncol=1)
df_4<-data.frame(df_4)

################## Adding months and year to the data #################
for(i in min(df_1$year):max(df_1$year)){
  for( j in 1:12){
    s_4<-append(s_4,j)
    s_5<-append(s_5,i)
}
}

df_5<-matrix(unlist(s_4),nrow=492,ncol=1) ######### Converting it to data frame ############
df_6<-matrix(unlist(s_5),nrow=492,ncol=1)
df_5<-data.frame(df_5)  
df_6<-data.frame(df_6)

############# Creating the monthly Time-Series ####################### 

TimeSeries<-cbind(df_6,df_5,df_2,df_3,df_4)
colnames(TimeSeries)<-c("Year","month","P","E","Q ")
############## Plotting the Graphs ###############################################
par(mfrow=c(4,3))
############## PLotting Precipitation V/s Streamflow ###########################
for(i in 1:12){
  df_7<-TimeSeries[which(TimeSeries$month==i),]
  plot(df_7$P,df_7$Q, col="Blue", xlab="Precipitation",ylab="Streamflow", main = month.abb[i],pch=19)
  }
 
mtext("Monthly Precipitation vs Monthly Streamflow", side = 3, col="red", line=-1.5, outer=TRUE)

par(mfrow=c(4,3))
################## Plotting PET V/s Streamflow ##############################
for(i in 1:12){
  df_7<-TimeSeries[which(TimeSeries$month==i),]
  plot(df_7$E,df_7$Q,col="Red",ylab="Streamflow",xlab="PET", main = "PET v/s Streamflow",pch=19)
  
  }


####-----------------------------------------------------------########

########----------------------------------------------------######

head(df_1)
df_1
boxplot(df_1$Q, main = "Box Plot of Daily Stream flow", col="dark blue")

hist(df_1$Q, xlab="Daily Streamflow", ylab="Frequency", main = "Histogram of Daily Stream flow")

boxplot(df_1$P, main = "Box Plot of Daily Precipitation")

hist(df_1$P, xlab = "Daily Precipitation", ylab ="Frequency", main = "Histogram of Daily Precipitation")

boxplot(df_1$E, main = " Box Plot of Daily Potential Evapotranspiration")

hist(df_1$E, xlab ="Daily Potential Evapotranspiration", ylab="Frequency", 
     main = "Histogram of Daily Potential Evapotranspiration")

####--------------------------------------------------------############


summary(df_1$P)    #Summary of Precipitation

summary(df_1$Q)    #Summary of Streamflows

summary(df_1$E)    #Summary of Evapotranspiration

########### Histograms of Daily Precipitation, Streamflows, Evapotranspiration ######

par(mfrow=c(3,1))

hist(df_1$P,xlab = "Daily Precipitation(mm/day)", main ="Histogram of Daily Precipitation", col=" dark blue")

hist(df_1$Q,xlab = "Daily Streamflows(mm/day)", main ="Histogram of Daily Streamflows", col=" light blue")

hist(df_1$E,xlab = "Daily Evapotranspiration(mm/day)", main ="Histogram of Daily Evapotranspiration", col=" red")



#############-----------------------------------------------------------#####################

class(df_1$P)

length(df_1$P)
       
                  ###### Auto Correlation #####
par(mfrow=c(3,1))
acf(df_1$Q, lag.max = 365, plot = TRUE, col="darkblue", main = " Auto Correlation Graph of Streamflows")
acf(df_1$P, lag.max=365, plot = TRUE, col="blue", main = " Auto Correlation Graph of Daily Precipitation")
acf(df_1$E, lag.max=365, plot= TRUE, col="red", main = "Auto Correlation Graph of Daily Evapotranspiration")

par(mfrow=c(3,1))
acf(df_1$Q, lag.max = 10, plot = TRUE, col="darkblue", main = " Auto Correlation Graph of Streamflows")
acf(df_1$P, lag.max=10, plot = TRUE, col="blue", main = " Auto Correlation Graph of Daily Precipitation")
acf(df_1$E, lag.max=10, plot= TRUE, col="red", main = "Auto Correlation Graph of Daily Evapotranspiration")

                 #### Time Series Analysis - Annual, Seasonal ###

months_data<-c("January","February","March","April","May","June","July","August","September","October","November","December")


class(TimeSeries)
head(TimeSeries)
############a)precipitation############################
TimeSeries$P
a_P <- matrix(TimeSeries$P, nrow = 12, ncol=41)
a_P
df_mon_P <- data.frame(a_P)
colnames(df_mon_P) <- 1977:2017
Monthly_dataframe_P<-cbind(months_data,df_mon_P)
head(Monthly_dataframe_P)

###########b)Streamflows############################
TimeSeries$`Q `
a_Q <- matrix(TimeSeries$`Q `,nrow=12,ncol=41)
a_Q
df_mon_Q <- data.frame(a_Q)
colnames(df_mon_Q) <- 1977:2017
Monthly_dataframe_Q <- cbind(months_data, df_mon_Q)
head(Monthly_dataframe_Q)

#############c)Evapotranspiration##############
TimeSeries$E
a_E <- matrix(TimeSeries$E, nrow=12, ncol=41)
a_E
df_mon_E <- data.frame(a_E)
colnames(df_mon_E) <- 1977:2017
Monthly_dataframe_E<- cbind(months_data, df_mon_E)
head(Monthly_dataframe_E)

############# Creating the Yearly Time-Series ###############################################


############ a)precipitation ######################
dim(Monthly_dataframe_P)

print(df_mon_P)

Yearly_dataframe_P<-data.frame(colSums(df_mon_P))
Year<- 1977:2017
Yearly_dataframe_P <- cbind(Year, Yearly_dataframe_P)

colnames(Yearly_dataframe_P)<-c("Year","Annual Precipitation")
Yearly_dataframe_P

class(Yearly_dataframe_P)
mean(Yearly_dataframe_P$`Annual Precipitation`)

#1229.129 mm

Flood_Year <- 1.2 * mean(Yearly_dataframe_P$`Annual Precipitation`)
Flood_Year

#1474.955 mm

Drought_Year <- 0.8 *mean(Yearly_dataframe_P$`Annual Precipitation`)
Drought_Year

#983.3035 mm

library(tidyverse)
ggplot(data=Yearly_dataframe_P,aes(x=Yearly_dataframe_P$Year,Yearly_dataframe_P$`Annual Precipitation`))+geom_point()+
  geom_line(colour="orange")+
  geom_abline(h=1474.955)+abline(h=1474.955)
############ b) Stream Flow #####################
dim(Monthly_dataframe_Q)

print(df_mon_Q)

Yearly_dataframe_Q<-data.frame(colSums(df_mon_Q))
Yearly_dataframe_Q <- cbind(Year, Yearly_dataframe_Q)

colnames(Yearly_dataframe_Q)<-c("Year","Annual Streamflows")

############ C) Evapotranspiration  #####################
dim(Monthly_dataframe_E)

print(df_mon_E)

Yearly_dataframe_E<-data.frame(colSums(df_mon_E))

Yearly_dataframe_E <- cbind(Year, Yearly_dataframe_E)

colnames(Yearly_dataframe_E)<-c("Year","Annual Evapotranspiration")

######## Combining all the Data into an Annual Time Series Dataframe #####

Yearly_dataframe <- cbind(Yearly_dataframe_P$Year, Yearly_dataframe_P$`Annual Precipitation`, Yearly_dataframe_Q$`Annual Streamflows`, Yearly_dataframe_E$`Annual Evapotranspiration`)
Yearly_dataframe

colnames(Yearly_dataframe) <- c("Year", "Annual Precipitation", "Annual Streamflows", "Annual PET")
Yearly_dataframe <- as.data.frame(Yearly_dataframe)

               ####### Annual Time Series Plots #######

par(mfrow=c(3,1))

plot(Year, Yearly_dataframe_P$`Annual Precipitation`, 
     xlab = "Year", ylab = "Annual Precipitation", type="l", 
     main = "Plot Showing Time Series of Annual Precipitation", col=" blue")

library(tidyverse)
P_vs_QYearly <- ggplot(data=df_1,mapping=aes(x=P,y=Q))+
  geom_point()+
  geom_line(colour="orange")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE)

abline(h=mean(Yearly_dataframe_P$`Annual Precipitation`), col="red")

plot(Year, Yearly_dataframe_Q$`Annual Streamflows`, 
     xlab = "Year", ylab = "Annual Streamflows", type="l", 
     main = "Plot Showing Time Series of Annual Streamflows", col=" dark blue")

abline(h=mean(Yearly_dataframe_Q$`Annual Streamflows`), col="red")

plot(Year, Yearly_dataframe_E$`Annual Evapotranspiration`, 
     xlab = "Year", ylab = "Annual Evaotranspiration", type="l", 
     main = "Plot Showing Time Series of Annual Evapotranspiration", col=" green")

abline(h=mean(Yearly_dataframe_E$`Annual Evapotranspiration`), col="red")

######-------------------------------------------------------------------------------------#####
                      
##############################################################################################
############# Creating the seasonal Time-Series ###############################################
TimeSeries$P

seasonal_Data<- c("Monsoon","Post Monsoon","Winter","Summer")

seasonal_Data
################ A)precipitatioin ##################
df_mon_P
Monsoon_P<-data.frame(colSums(df_mon_P[c(6,7,8), ]))
Post_Monsoon_P<-c(colSums(df_mon_P[c(9,10,11), ]))
Winter_P<-c(colSums(df_mon_P[c(12,1,2), ]))
Summer_P<-c(colSums(df_mon_P[c(3,4,5), ]))

range(Monsoon_P)

Seasonal_P<-cbind(Year,Monsoon_P,Post_Monsoon_P,Winter_P,Summer_P)

colnames(Seasonal_P)<-c("Year","Monsoon_P","Post_Monsoon_P","Winter_P","Summer_P")

head(Seasonal_P)

########### Histograms of Monsoon Precipitation ######

par(mfrow=c(3,1))

hist(Seasonal_P$Monsoon_P,xlab = "Monsoon Precipitation", main ="Histogram of Daily Precipitation", col=" dark blue")

h
hist(df_1$E,xlab = "Daily Evapotranspiration(mm/day)", main ="Histogram of Daily Evapotranspiration", col=" red")

################ b) Stream Flow ##################
df_mon_Q
Monsoon_Q<-data.frame(colSums(df_mon_Q[c(6,7,8), ]))
Post_Monsoon_Q<-c(colSums(df_mon_Q[c(9,10,11), ]))
Winter_Q<-c(colSums(df_mon_Q[c(12,1,2), ]))
Summer_Q<-c(colSums(df_mon_Q[c(3,4,5), ]))

Seasonal_Q<-cbind(Monsoon_Q,Post_Monsoon_Q,Winter_Q,Summer_Q)
range(Monsoon_Q)
colnames(Seasonal_Q)<-c("Monsoon_Q","Post_Monsoon_Q","Winter_Q","Summer_Q")

head(Seasonal_Q)

hist(Seasonal_Q$Monsoon_Q,xlab = "Monsoon Streamflows", main ="Histogram of Daily Streamflows", col=" light blue")

boxplot(Seasonal_Q$Monsoon_Q)
################ C) Evapotranspiration ##################
df_mon_E
Monsoon_E<-data.frame(colSums(df_mon_E[c(6,7,8), ]))
Post_Monsoon_E<-c(colSums(df_mon_E[c(9,10,11), ]))
Winter_E<-c(colSums(df_mon_E[c(12,1,2), ]))
Summer_E<-c(colSums(df_mon_E[c(3,4,5), ]))

Seasonal_E<-cbind(Monsoon_E,Post_Monsoon_E,Winter_E,Summer_E)

colnames(Seasonal_E)<-c("Monsoon_E","Post_Monsoon_E","Winter_E","Summer_E")

head(Seasonal_E)


#####------------------------SIMPLE LINEAR REGRESSION ANALYSIS------------------------#####

######---------Daily Scale Analysis----------------------####

####Daily Precipitation vs Daily Streamflows#####

text(paste("Correlation:", round(cor(df_1$P, df_1$Q), 2)), x = 95, y = 6000)

library(tidyverse)
library(ggpubr)
P_vs_QDaily <- ggplot(data=df_1,mapping=aes(x=P,y=Q))+
  geom_point()+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE)
print(P_vs_QDaily+ggtitle("Daily Precipitation vs Daily Streamflows")+labs(x="Daily Precipitation (mm/day)",y="Daily Streamflows(mm/day) ")+stat_cor(method = "pearson"))

#### Scatter Plot of Daily Precipitation vs Daily Streamflows ####


plot(df_1$P,df_1$Q, xlab = "Daily Precipitation", ylab= "Daily Stream flow", main = "Daily Rainfall vs Runoff", col="red")

text(paste("Correlation:", round(cor(df_1$P, df_1$Q), 2)), x = 120, y = 100)

#### Simple Linear Regression Analysis###

SLR_Q_vs_P<- lm(df_1$Q~df_1$P)
SLR_Q_vs_P

summary(SLR_Q_vs_P)                   ###Streamflow vs Precipitation ####

###Call:                                    ###Linear Regression Results
###lm(formula = df_1$Q ~ df_1$P)

###Coefficients:
### (Intercept)       df_1$P  
###0.4106       0.3540  ###


####Daily Evapotranspiration vs Daily Precipitation #####

library(tidyverse)
library(ggpubr)
E_vs_PDaily <- ggplot(data=df_1,mapping=aes(x=E,y=P))+
  geom_point()+
  geom_line(colour="green")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE)
print(E_vs_PDaily+ggtitle("Daily Evapotranspiration vs Daily Precipitation")
      +labs(x="Daily Evapotranspiration (mm/day)",y="Daily Precipitation(mm/day) ")+stat_cor(method = "pearson"))

#### Scatter Plot of Daily Evapotranspiration vs Daily Precipitation ####

plot(df_1$E,df_1$P, xlab = "Daily Evapotranspiration", ylab= "Daily Precipitation", main = "Evapotranspiration vs Daily Precipitation", col="red")

text(paste("Correlation:", round(cor(df_1$E, df_1$P), 2)), x = 8, y = 120)

#### Simple Linear Regression Analysis###

SLR_E_vs_P<- lm(df_1$E~df_1$P)
SLR_E_vs_P

summary(SLR_E_vs_P)   

#Call:
#  lm(formula = df_1$E ~ df_1$P)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-4.0831 -1.7924 -0.3079  1.2647  6.8846 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 3.736410   0.018182  205.50   <2e-16 ***
#  df_1$P      0.026719   0.001944   13.74   <2e-16 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.076 on 14973 degrees of freedom
#Multiple R-squared:  0.01245,	Adjusted R-squared:  0.01239 
#F-statistic: 188.8 on 1 and 14973 DF,  p-value: < 2.2e-16

####Daily Evapotranspiration vs Daily Streamflows/Runoff #####

library(tidyverse)
library(ggpubr)
E_vs_QDaily <- ggplot(data=df_1,mapping=aes(x=E,y=Q))+
  geom_point()+
  geom_line(colour="blue")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE)
print(E_vs_QDaily+ggtitle("Daily Evapotranspiration vs Daily Precipitation")+labs(x="Daily Evapotranspiration (mm/day)",y="Daily Streamflows(mm/day) ")+stat_cor(method = "pearson"))


#### Scatter Plot of Daily Evapotranspiration vs Streamflows/Runoff ####

plot(df_1$E,df_1$Q, xlab = "Daily Evapotranspiration", ylab= "Daily Streamflows/Runoff", main = "Evapotranspiration vs Daily Streamflows/Runoff", col="red")

text(paste("Correlation:", round(cor(df_1$E, df_1$P), 2)), x = 8, y = 120)

#### Simple Linear Regression Analysis###

SLR_E_vs_Q<- lm(df_1$E~df_1$Q)
SLR_E_vs_Q

summary(SLR_E_vs_Q)   

#Call:
#  lm(formula = df_1$E ~ df_1$Q)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.9520 -1.8341 -0.2063  1.3513  6.8211 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 3.799886   0.017969 211.471  < 2e-16 ***
#  df_1$Q      0.016507   0.003526   4.682 2.87e-06 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.087 on 14973 degrees of freedom
#Multiple R-squared:  0.001462,	Adjusted R-squared:  0.001395 
#F-statistic: 21.92 on 1 and 14973 DF,  p-value: 2.867e-06

  
####----------- Monsoon Seasonal Scale Analysis---------------------#####


SLR_Seasonal<- cbind(Year,Seasonal_P$Monsoon_P,Seasonal_E$Monsoon_E, Seasonal_Q$Monsoon_Q)
dim(SLR_Seasonal)
class(SLR_Seasonal)
SLR_Seasonal<-as.data.frame(SLR_Seasonal)
colnames(SLR_Seasonal) <-c("Year", "MonsoonP", "MonsoonQ","MonsoonE")
head(SLR_Seasonal)

#### Scatter Plot of Annual Monsoon Rainfall vs Annual Monsoon Runoff ####

plot(SLR_Seasonal$MonsoonP,SLR_Seasonal$MonsoonQ, xlab = "Annual Monsoon Precipitation", ylab= "Annual Monsoon Runoff", 
      main = "Annual Monsoon Rainfall vs Annual Monsoon Runoff", col="blue")

text(paste("Correlation:", round(cor(SLR_Seasonal$MonsoonP, SLR_Seasonal$MonsoonQ), 2)), x=1400, y=540, col="red")


#####----------------------------------####

library(tidyverse)
library(ggpubr)
P_vs_QMonsoon <- ggplot(data=SLR_Seasonal, mapping=aes(x=SLR_Seasonal$MonsoonP,y=SLR_Seasonal$MonsoonQ),col="orange")+
  geom_point()+
  geom_line(colour="blue")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE)


P_vs_QMonsoon
print(P_vs_QMonsoon+ggtitle("Monsoon Precipitation vs Monsoon Streamflows ")
      +labs( x="Monsoon Precipitation", y="Monsoon Streamflows ",)+stat_cor(method = "pearson"))


SLR_Mons_P_vs_Q<- lm(SLR_Seasonal$Monsoon_Q~SLR_Seasonal$Monsoon_P)

SLR_Mons_P_vs_Q
summary(SLR_Mons_P_vs_Q)

#Call:
#  lm(formula = SLR_Seasonal$Monsoon_Q ~ SLR_Seasonal$Monsoon_P)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-6.7797 -1.9536 -0.1838  1.3513  8.5714 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            -9.001081   2.282012  -3.944 0.000323 ***
#  SLR_Seasonal$Monsoon_P  0.024272   0.002527   9.605 7.92e-12 ***
---
  #  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
  #Residual standard error: 3.25 on 39 degrees of freedom
  #Multiple R-squared:  0.7029,	Adjusted R-squared:  0.6952 
  #F-statistic: 92.25 on 1 and 39 DF,  p-value: 7.922e-12
  #### GG Plot of  Monsoon Precipitation vs Monsoon Avg. Streamflows ####

#### GG Plot of  Monsoon Evapotranspiration vs Monsoon Precipitation ####

library(tidyverse)
library(ggpubr)
E_vs_PMonsoon <- ggplot(data=SLR_Seasonal, mapping=aes(x=SLR_Seasonal$Monsoon_E,y=SLR_Seasonal$Monsoon_P)) +
  geom_point()+
  geom_line(colour="green")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE)


E_vs_PMonsoon
print(E_vs_PMonsoon+ggtitle("Monsoon Evapotranspiration vs Monsoon Precipitation ")
      +labs( x="Monsoon Evapotranspiration", y="Monsoon Precipitation ",)+stat_cor(method = "pearson"))


#### GG Plot of  Monsoon Evapotranspiration vs Monsoon Streamflows ####

library(tidyverse)
library(ggpubr)
E_vs_QMonsoon <- ggplot(data=SLR_Seasonal, mapping=aes(x=SLR_Seasonal$Monsoon_E,y=SLR_Seasonal$Monsoon_Q)) +
  geom_point()+
  geom_line(colour="cyan")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE)


E_vs_QMonsoon
print(E_vs_QMonsoon+ggtitle("Monsoon Evapotranspiration vs Monsoon Streamflows ")
      +labs( x="Monsoon Evapotranspiration", y="Monsoon Streamflows ",)+stat_cor(method = "pearson"))


#######-----------------------Annual Scale Analysis--------------######


plot(Yearly_dataframe_E$`Annual Evapotranspiration`, Yearly_dataframe_Q$`Annual Streamflows`,
     xlab = "Annual Evapotranspiration", ylab = "Annual Streamflows", 
     main = "Plot Showing Annual Evapotranspiration vs Annual Streamflows", col="red")

plot(Yearly_dataframe_P$`Annual Precipitation`, Yearly_dataframe_E$`Annual Evapotranspiration`,
     xlab = "Annual Evapotranspiration", ylab = "Annual Streamflows", 
     main = "Plot Showing Annual Evapotranspiration vs Annual Streamflows", col="red")


  
                 ####### Annual Correlation Plots #######

#####---GG Plot of Annual Precpitation vs Annual Sreamflows -----######
head(Yearly_dataframe)

class(Yearly_dataframe)

library(tidyverse)
library(ggpubr)
P_vs_QYearly <- ggplot(data=Yearly_dataframe, mapping=aes(x=Yearly_dataframe$`Annual Precipitation`,y=Yearly_dataframe$`Annual Streamflows`)) +
  geom_point()+
  geom_line(colour="blue")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE)

P_vs_QYearly
print(P_vs_QYearly+ggtitle(" Annual Precipitation vs Annual Streamflows  ")
      +labs( x="Annual Precipitation", y="Annual Streamflows ")+stat_cor(method = "pearson"))

SLR_P_vs_QYearly<- lm(Yearly_dataframe$`Annual Streamflows`~Yearly_dataframe$`Annual Precipitation`)
SLR_P_vs_QYearly

summary(SLR_P_vs_QYearly) 

#Call:
#  lm(formula = Yearly_dataframe$`Annual Streamflows` ~ Yearly_dataframe$`Annual Precipitation`)

#Residuals:
#  Min     1Q Median     3Q    Max 
#-7.825 -3.327  1.028  2.823 11.046 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                             -10.396581   3.829448  -2.715  0.00982 ** 
#  Yearly_dataframe$`Annual Precipitation`   0.023974   0.003062   7.830  1.6e-09 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 4.533 on 39 degrees of freedom
#Multiple R-squared:  0.6112,	Adjusted R-squared:  0.6012 
#F-statistic: 61.31 on 1 and 39 DF,  p-value: 1.599e-09


  #####---GG Plot of Annual Evapotranspiration vs Annual Precipitation -----######

head(Yearly_dataframe)

class(Yearly_dataframe)

library(tidyverse)
library(ggpubr)
E_vs_PYearly <- ggplot(data=Yearly_dataframe, mapping=aes(x=Yearly_dataframe$`Annual PET`,y=Yearly_dataframe$`Annual Precipitation`)) +
  geom_point()+
  geom_line(colour="green")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE)

E_vs_PYearly
print(E_vs_PYearly+ggtitle(" Annual Evapotranspiration vs Annual Precipitation  ")
      +labs( x="Annual Evapotranspiration", y="Annual Precipitation ")+stat_cor(method = "pearson"))

SLR_E_vs_PYearly<- lm(Yearly_dataframe$`Annual Precipitation`~Yearly_dataframe$`Annual PET`)
SLR_E_vs_PYearly

summary(SLR_P_vs_QYearly) 

#####---GG Plot of Annual Evapotranspiration vs Annual Streamflows-----######

head(Yearly_dataframe)

class(Yearly_dataframe)

library(tidyverse)
library(ggpubr)
E_vs_QYearly <- ggplot(data=Yearly_dataframe, mapping=aes(x=Yearly_dataframe$`Annual PET`,y=Yearly_dataframe$`Annual Streamflows`)) +
  geom_point()+
  geom_line(colour="red")+
  geom_smooth(method=lm, formula = y ~ x, se=FALSE)

E_vs_QYearly
print(E_vs_QYearly+ggtitle(" Annual Evapotranspiration vs Annual Streamflows  ")
      +labs( x="Annual Evapotranspiration", y="Annual Streamflows ")+stat_cor(method = "pearson"))

SLR_E_vs_QYearly<- lm(Yearly_dataframe$`Annual Streamflows`~Yearly_dataframe$`Annual PET`)
SLR_E_vs_QYearly

summary(SLR_E_vs_QYearly) 



##################################################################################


####    CORRELATION COEFFICIENTS  ####

### Checking for Pearson Correlation between Precipitation & Streamflows ###

cor.test(df_1$P,df_1$Q, method = "pearson",exact = FALSE, continuity = TRUE)

### Checking for Pearson Correlation between Evapotranspiration & Streamflows ###

cor.test(df_1$E,df_1$Q, method = "pearson",exact = FALSE, continuity = TRUE)

### Checking for Pearson Correlation between Evapotranspiration & Precipitation ###

cor.test(df_1$P,df_1$E, method = "pearson",exact = FALSE, continuity = TRUE)

####--------------------------------------------------------------###########

###   Checking for Spearman Correlation between variables Precipitation & Streamflows ### 

cor.test(df_1$P,df_1$Q, method = "spearman",exact = FALSE, continuity = TRUE)

###   Checking for Spearman Correlation between variables Evapotranspiration & Streamflows ### 

cor.test(df_1$E,df_1$Q, method = "spearman",exact = FALSE, continuity = TRUE)

## Checking for Spearman Correlation between Evapotranspiration & Precipitation ###

cor.test(df_1$P,df_1$E, method = "spearman",exact = FALSE, continuity = TRUE)

###############------------------------------------------########################












