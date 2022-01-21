#install.packages("dplyr")
#install.packages("SciViews")
#install.packages("ggplot2")
library(dplyr)
library(SciViews)
library(ggplot2)
start=1985
end=2017

daily_data<- read.csv("sylhet.csv")
#summary(daily_data)
###Omit leap year values
leap<-which(daily_data$Day_month=="29-Feb")
daily_data<-daily_data[-leap,]




one_day_table<- data.frame(rep(NA,365))

for (i in start:end)
{
  data<-daily_data[daily_data$Year==i,]
  data<-data.frame(data$TSVALUE_UPDATED)
  
  one_day_table<-cbind(one_day_table,data)

}
one_day_table<-one_day_table[-1]
colnames(one_day_table)<-seq(start,end)

one_day_max<-one_day_table %>% summarise_if(is.numeric, max, na.rm=TRUE)
one_day<-rbind(one_day_table,one_day_max)

###############################################################

two_day_table <- data.frame(rep(NA,365))

for (i in names(one_day_table)){
  
  two_day_lag<-data.frame(x=RcppRoll::roll_sum(one_day_table[, i],2, fill=NA, align="right"))
  two_day_table<-cbind(two_day_table,two_day_lag)
}
two_day_table<-two_day_table[-1]
colnames(two_day_table)<-seq(start,end)

two_day_max<-two_day_table %>% summarise_if(is.numeric, max, na.rm=TRUE)
two_day<-rbind(two_day_table,two_day_max)

################################################################

three_day_table <- data.frame(rep(NA,365))

for (i in names(one_day_table)){
  
  three_day_lag<-data.frame(x=RcppRoll::roll_sum(one_day_table[, i],3, fill=NA, align="right"))
  three_day_table<-cbind(three_day_table,three_day_lag)
}
three_day_table<-three_day_table[-1]
colnames(three_day_table)<-seq(start,end)

three_day_max<-three_day_table %>% summarise_if(is.numeric, max, na.rm=TRUE)
three_day<-rbind(three_day_table,three_day_max)

################################################################

five_day_table <- data.frame(rep(NA,365))

for (i in names(one_day_table)){
  
  five_day_lag<-data.frame(x=RcppRoll::roll_sum(one_day_table[, i],5, fill=NA, align="right"))
  five_day_table<-cbind(five_day_table,five_day_lag)
}
five_day_table<-five_day_table[-1]
colnames(five_day_table)<-seq(start,end)

five_day_max<-five_day_table %>% summarise_if(is.numeric, max, na.rm=TRUE)
five_day<-rbind(five_day_table,five_day_max)


################################################################

ten_day_table <- data.frame(rep(NA,365))

for (i in names(one_day_table)){
  
  ten_day_lag<-data.frame(x=RcppRoll::roll_sum(one_day_table[, i],10, fill=NA, align="right"))
  ten_day_table<-cbind(ten_day_table,ten_day_lag)
}
ten_day_table<-ten_day_table[-1]
colnames(ten_day_table)<-seq(start,end)

ten_day_max<-ten_day_table %>% summarise_if(is.numeric, max, na.rm=TRUE)
ten_day<-rbind(ten_day_table,ten_day_max)


final_max_table<- data.frame(rep(NA, end-start+1))  



final_max_table<-cbind(final_max_table,t(one_day[366,]))
final_max_table<-cbind(final_max_table,t(two_day[366,]))
final_max_table<-cbind(final_max_table,t(three_day[366,]))
final_max_table<-cbind(final_max_table,t(five_day[366,]))
final_max_table<-cbind(final_max_table,t(ten_day[366,]))

final_max_table<-final_max_table[-1]

colnames(final_max_table)<-c("one day","two day","three day","five day","ten day")


final_max_table_hr<- cbind(final_max_table$`one day`/24,final_max_table$`two day`/(24*2),
                           final_max_table$`three day`/(24*3), final_max_table$`five day`/(24*5),
                           final_max_table$`ten day`/(24*10))

colnames(final_max_table_hr)<-c("one day","two day","three day","five day","ten day")

sd_vec <- apply(final_max_table_hr, 2, sd)

return_periods<- c(5,10,30,50,100)

freq_factor <- function(t) {
  KT =  -sqrt(6)/pi * ( 0.5772 + ln(ln(t/(t-1)) ))
  
}

kT<- freq_factor(return_periods)


mean_vec <- apply(final_max_table_hr, 2, mean)


one_day_XT<- mean_vec[1] + kT * sd_vec[1]
two_day_XT<- mean_vec[2] + kT * sd_vec[2] 
three_day_XT<- mean_vec[3] + kT * sd_vec[3] 
five_day_XT<- mean_vec[4] + kT * sd_vec[4] 
ten_day_XT<- mean_vec[5] + kT * sd_vec[5] 

final<- rbind(one_day_XT, two_day_XT, three_day_XT, five_day_XT, ten_day_XT)
final<- data.frame(final)


days<-c(1,2,3,5,10)

final<- cbind(days,final)
colnames(final)<- c("days",  "RP5year","RP10year","RP30year","RP50year","RP100year")


IDF_curve<- ggplot(data = final, aes(x = days)) +
  geom_line(aes(y = RP5year, colour = "RP 5 year"),lwd=2) +
  geom_line(aes(y = RP10year, colour = "RP 10 year"),lwd=2) +
  geom_line(aes(y = RP30year, colour = "RP 30 year"),lwd=2) +
  geom_line(aes(y = RP50year, colour = "RP 50 year"),lwd=2) +
  geom_line(aes(y = RP100year, colour = "RP 100 year"),lwd=2) +
  
  
  scale_colour_manual("", 
                      breaks = c("RP 5 year", "RP 10 year","RP 30 year","RP 50 year","RP 100 year"),
                      values = c("darkblue", "red", "orange", "black", "green")) +
  labs(y = "Rainfall Intensity", title = "IDF Curve")  

IDF_curve


mean_vec
sd_vec

#write.csv(final_max_table, "input_kutubdia.csv")




