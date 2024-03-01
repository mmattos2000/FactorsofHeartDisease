install.packages("readxl")
library(readxl)
heartdisease<-data.frame(readxl::read_excel("Heart Disease Data Cleaned.xlsx",
                                            col_names=TRUE))
heartdisease$Heart.Disease<-(heartdisease$Heart.Disease*1)

model.BMI<-glm(Heart.Disease~BMI,data=heartdisease,family=binomial)

library(ggplot2)
ggplot(heartdisease,aes(x=BMI,y=Heart.Disease))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm",formula=y~x,
              se=FALSE,method.args=list(family=binomial),col='magenta1')

model.Sleeptime<-glm(Heart.Disease~Sleeptime,data=heartdisease,family=binomial)

library(ggplot2)
ggplot(heartdisease,aes(x=Sleeptime,y=Heart.Disease))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm",formula=y~x,
              se=FALSE,method.args=list(family=binomial),col='blue')
