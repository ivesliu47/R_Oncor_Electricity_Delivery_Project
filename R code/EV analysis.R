library(lubridate)
library(ggplot2)
library(dplyr)


data <- read.csv("C:/Study/Capstone/Data/8_Interval_Usage.csv")
data[which(data$consumption==0),3] <- NaN

str(data)
ev_user <- data

str(ev_user)

library(lubridate)
ev_user$read_date <- as_datetime(ev_user$read_date)


##draw a image
library(ggplot2)
library(dplyr)
ggplot(ev_user[114048:228096,],aes(x=read_date,y=consumption*4000,group=1)) %>%
  +geom_line(color="blue") %>%
  +scale_x_datetime(breaks="12 month")%>%
  +scale_y_continuous(breaks=seq(0,10000,1000)) %>% 
  +theme(axis.text.x = element_text(size=12)) %>% 
  +ylab("Consumption by PowerW")


   
## define a function to show every day, week data

showdata <- function(a=1){
  # a means how may days you want to move
  p=ggplot(ev_user[(1+96*a):(300+96*a),],aes(x=read_date,y=consumption*4000,group=1)) %>%
    +geom_line(color="#56B4E9",size=0.7) %>%
    +scale_x_datetime(date_labels = "%b %d",breaks=("24 hours"))%>%
    +scale_y_continuous(breaks=seq(0,15000,1000)) %>% 
    +theme(axis.text.x = element_text(angle=90, size=12)) %>% 
    +ylab("Consumption by PowerW")%>%
    +theme(panel.background = element_rect(fill = "white", colour = "grey50"))
  return(p)
}

a2=showdata(a=1003)
b1=showdata(a=1256)


library(gridExtra)
grid.arrange(a2,b1,ncol=2)



##change the shape for the matlab model

data$consumption <- data$consumption*4000
s_data <- matrix(ev_user$consumption[114048:228096])

write.csv(s_data, "C:/Study/Capstone/Newtest.csv")

#################################
# Analyze the result of the model
##################################

# look at certain month result


d5200912=d1 %>% filter(esi_id=="EV5",year=="2019")



ggplot(d5200912,aes(x=read_date,y=consumption*4000,group=1)) %>%
  +geom_line(color="red") %>%
  +scale_x_datetime(breaks="12 month")%>%
  +scale_y_continuous(breaks=seq(0,10000,1000)) %>% 
  +theme(axis.text.x = element_text(size=12)) %>% 
  +ylab("Consumption by PowerW")


d5=d1%>%filter(esi_id=="EV5")

#set the input and coefficient in this model
#===========================================
interval=15
width_Percent=0.8
# We suppose that the height of EV segment should be higher than EVAMP
EVAMP=8000
# We suppose that appliance which power is below 3000W must not be EV 
Threshold_value=3000
# We believe that EV charging duration must be longer than 180 minutes, and the maximum is 900 minutes
min_duration = 60
max_duration = 900
#==========================================



                                        