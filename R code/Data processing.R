library(lubridate)
library(ggplot2)
library(dplyr)
library(gridExtra)
#8_Interval_Usage.csv
#EV_Team_Sample_5.csv

data <- read.csv("C:/Study/Capstone/Data/EV_Team_Sample_5.csv")
colnames(data)[1]="id"

data[which(is.na(data$consumption)),3] <- 0
data$read_date <- as_datetime(data$read_date)

head(data)
str(data)
#slice the data 
d1 <- data

#extract all the years and months
d1 <- d1 %>%
  mutate(year=year(d1$read_date),month=month(d1$read_date))

y_m=d1%>%
  select(year,month)%>%
  distinct()

#set the input and coefficient in this model
#===========================================
interval=15
width_Percent=0.8
# We suppose that the height of EV segment should be higher than EVAMP
EVAMP=3500
# We suppose that appliance which power is below 3000W must not be EV 
Threshold_value=2000
# We believe that EV charging duration must be longer than 180 minutes, and the maximum is 900 minutes
min_duration = 180
max_duration = 900
#==========================================

### EV estimate applicationin R 
#this will be a function; input is a datarame contains two variable(POSIXct date,consumption); calculte EV segment; return number,frequency,power

estEV <- function(power_data){
  
  #  power_data=d5200912
  
  # the first column should be time and the second column should be the power data
  org_time=power_data$read_date
  orgAgg=power_data$consumption*4000
  #==================================================================================
  #STEP 1 Get the residual and remove it
  #==================================================================================
  res=min(orgAgg)
  ts=orgAgg-res
  
  #==================================================================================
  #STEP 2 Threholding
  #==================================================================================
  Threshold=Threshold_value
  EVsignal = ts;
  EVsignal[EVsignal<Threshold]=0
  
  # #draw a graphic
  # EV=tibble(a=org_time,EVsignal)[755:850]
  # ggplot(EV,aes(x=a,y=EVsignal),group=1)+geom_line(color='steelblue')
  # 
  
  #Record the thresholded signal
  EV_step2 = EVsignal
  
  
  #==================================================================================
  #STEP 3 Get segments
  #==================================================================================
  
  # Define getSegment function, return segment
  ## the first row is N/A, if there is no segment, there will only be one row
  getSegment <- function(EVsignal){
    idx=as.numeric(EVsignal>0)
    idx2=c(0,idx,0)
    prePt = c("")
    postPt = c("")
    
    #get the segment starts and ends
    for (i in 2:length(idx)){
      if (idx2[i-1] == 0 & idx2[i] == 1 & idx2[i+1] == 1){
        prePt=c(prePt,i-1)
      }
      if (idx2[i-1] == 1 & idx2[i] == 1 & idx2[i+1] == 0){
        postPt=c(postPt,i-1)
      }
    }
    #if the segment extend this month signal, we set the last index as the postPt
    if(length(prePt)==length(postPt)+1){
      postPt=c(postPt,length(idx))
    }
    segment=tibble(prePt,postPt)
    return (segment)
  }  
  
  segment=getSegment(EVsignal)
  segment <- as.data.frame(lapply(segment, function(x) as.numeric(as.character(x))))
  
  # if no segement, returen NA
  if (nrow(segment)==1){
    return(list("EV_user_flag"=0,
                "Number of Signal"=0,
                "Duration Variance"=0,
                "mean estPower"=0,
                "estPower Variance"=0))
    
  }
  
  #==================================================================================
  #STEP 4 Filter segments based on duration
  #==================================================================================
  
  min_Duration = min_duration
  max_Duration = max_duration
  
  #calculate the duraton
  segment= segment %>% mutate(duration=interval*(postPt-prePt))
  #filter segments
  Dur_segment = filter(segment,duration>min_duration&duration<max_duration)
  
  # if no Dur_segement, returen NA
  if (nrow(Dur_segment)==0){
    return(list("EV_user_flag"=0,
                "Number of Signal"=0,
                "Duration Variance"=0,
                "mean estPower"=0,
                "estPower Variance"=0))
    
  }
  
  
  
  #==================================================================================
  #STEP 5 Get width and height of segment
  #==================================================================================
  width_percent=width_Percent
  
  
  #Define getHeight function
  calHeight = function(signal,width_percent){
    for (i in 1:300){
      height=i/300*max(signal)
      if (sum(as.numeric(signal>=height))/length(signal)<width_percent){
        return(height)
      }
    }
  }  
  
  #################################
  #debug
  #################################
  # signal1=EVsignal[1436:1452]
  # max(signal1)
  # for (i in 1:300){
  #   height=i*max(signal1)/300
  #   print(i)
  #   print(sum(as.numeric(signal1>=height))/length(signal1))
  #   print(height)
  # }
  # 
  # #draw a graphic
  # EV=tibble(a=org_time,EVsignal)[1436:1452,]
  # ggplot(EV,aes(x=a,y=EVsignal),group=1)+geom_line(color='steelblue')
  
  ##########################
  
  getHeight = function(segment,signal){
    height=c()
    for (i in 1:nrow(segment)){
      height=c(height,calHeight(signal[segment[i,1]:segment[i,2]],width_percent))
    }
    return(mutate(segment,estPower=height))
  }
  
  Height_segment=getHeight(Dur_segment,EVsignal)
  
  #Filter segment which height is below EVAMP
  EV_segment=Height_segment %>% filter(estPower>EVAMP)
  
  # if no Dur_segement, returen NA
  if (nrow(EV_segment)==0){
    return(list("EV_user_flag"=0,
                "Number of Signal"=0,
                "Duration Variance"=0,
                "mean estPower"=0,
                "estPower Variance"=0))
  }
  
  #==================================================================================
  #STEP 6 Return ouptputs
  #==================================================================================
  num_evsigal=nrow(EV_segment)
  mean_estPower=mean(EV_segment$estPower)
  sd_estPower=sd(EV_segment$estPower)
  #EV charging interval
  Char_interval=(EV_segment$prePt[2:nrow(EV_segment)]-EV_segment$postPt[1:nrow(EV_segment)-1])/(24*60/interval)
  mean_char_interval=mean(Char_interval)
  sd_char_interval=sd(Char_interval)
  
  #EV charging start date and time
  day(org_time[EV_segment$prePt])
  hour(org_time[EV_segment$prePt])
  # the time unit is hour
  mean_Duration=mean(EV_segment$duration)/60
  sd_Duration=sd(EV_segment$duration)
  
  # is there any relationship between start time and duration?
  tibble(hour(org_time[EV_segment$prePt]),EV_segment$duration/60)
  
  
  output= list("EV_user_flag"=1,
               "Number of Signal"=num_evsigal,
               "Duration Variance"=sd_Duration,
               "mean estPower"=mean_estPower,
               "estPower Variance"=sd_estPower)
  
  return(output)
}

#group the data by year and month and then get the result
result=group_by(d1,id,year,month)%>% do(as.data.frame(estEV(.)))
print(tbl_df(result),n=100)

###############################################
#SELECT PROCESS
###############################################

#remove na in variance
result$Duration.Variance[is.na(result$Duration.Variance)]=0
result$estPower.Variance[is.na(result$estPower.Variance)]=0


#make the last month for every user to be 0
for(i in 2:nrow(result)){
  if(result[i,1]!=result[i-1,1]){
    result[i,4]=0
  }
}

##Calculate the longest time of continuent signal
library(data.table)
result=as.data.table(result)[, maxperiod := sequence(.N), by = rleid(EV_user_flag)][EV_user_flag == "0", maxperiod := 0][]
result$maxperiod

#build a new table for selecting 
r1=group_by(result,id)%>%summarise(total_month=sum(EV_user_flag),
                                   data_period=n(),
                                   mean_times=mean(Number.of.Signal),
                                   sd_times=sd(Number.of.Signal),
                                   maxperiod=max(maxperiod),
                                   mean_dur_var=mean(Duration.Variance),
                                   mean_estP_mean=mean(mean.estPower),
                                   mean_estP_var=mean(estPower.Variance))
r2=r1[order(r1$maxperiod,decreasing = TRUE),]
print(r2,n=nrow(r2))

########select rules##############
#total month > 12
#signals should continue appearing for 6 months
#mena_times>4 & <15
#mean_estP_mean>2800

final_result=r2[which(r2$total_month/r2$data_period>=0.5
                     &r2$maxperiod/r2$total_month>=0.6
                     &r2$mean_times>1
                     &r2$mean_times<15
                     &r2$sd_times<10
                     &r2$mean_estP_mean>2800),]
show(final_result$id)










#group_by(d1[which(d1$id=="RDM2377306"),],id,year,month)%>% do(as.data.frame(estEV(.)))
