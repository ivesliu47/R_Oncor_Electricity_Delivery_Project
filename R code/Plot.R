### EV estimate applicationin R 
#this will be a function; input is a datarame contains two variable(POSIXct date,consumption); calculte EV segment; return number,frequency,power

# estEV_segment <- function(power_data){




  library(lubridate)
  library(ggplot2)
  library(dplyr)
  
  power_data=d1%>%
    filter(year==2015,month==9)
  
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
                "Number of Signal"=NA,
                "Average Interval"=NA,
                "estPower"=NA,
                "estPower Variance"=NA))
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
                "Number of Signal"=NA,
                "Average Interval"=NA,
                "estPower"=NA,
                "estPower Variance"=NA))
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
  EV_segment=Height_segment %>% filter(height>EVAMP)
  
  # if no Dur_segement, returen NA
  if (nrow(EV_segment)==0){
    return(list("EV_user_flag"=0,
                "Number of Signal"=NA,
                "Average Interval"=NA,
                "estPower"=NA,
                "estPower Variance"=NA))
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
  
  # is there any relationship between start time and duration?
  tibble(hour(org_time[EV_segment$prePt]),EV_segment$duration/60)
  
  
  output= list("EV_user_flag"=1,
               "Number of Signal"=num_evsigal,
               "Average Interval"=mean_char_interval,
               "estPower"=mean_estPower,
               "estPower Variance"=sd_estPower,
               "EV_segment"=EV_segment)
  
#   return(output)
# }

###plot all the EV segment signals
  for (i in nrow(EV_segment)){
    
  }
  
  
  
  
  
