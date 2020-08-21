data <- read.csv("C:/Study/Capstone/Interval_Usage.csv")
str(data)
colnames(data)[1]="id"
data[,1]=0

for(i in 2:nrow(data)){
  if(data[i,2]>data[i-1,2]){
    data[i,1]=data[i-1,1]
  }else{
    data[i,1]=data[i-1,1]+1
  }
}

str(data$id)
