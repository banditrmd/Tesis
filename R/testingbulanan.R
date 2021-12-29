#Install Package
install.packages("devtools", dependencies = TRUE)
devtools::install_github("hrbrmstr/ggalt")

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("data.table")
install.packages("ggalt")
install.packages("ggfortify")


options(scipen =999)

#Library
library(tidyverse)
library(ggplot2)
library(data.table)
library(ggalt)
library(ggfortify)

theme_set(theme_bw())

#Intial Data
data<-data.table(bisni4)

#Filter data
data1<- filter(data, BLTH == 201901)
data2<- filter(data, BLTH == 201902)
data3<- filter(data, BLTH == 201903)
data4<- filter(data, BLTH == 201904)
data5<- filter(data, BLTH == 201905)
data6<- filter(data, BLTH == 201906)
data7<- filter(data, BLTH == 201907)
data8<- filter(data, BLTH == 201908)
data9<- filter(data, BLTH == 201909)
data10<- filter(data, BLTH == 201910)
data11<- filter(data, BLTH == 201911)
data12<- filter(data, BLTH == 201912)
data13<- filter(data, BLTH == 202001)
data14<- filter(data, BLTH == 202002)
data15<- filter(data, BLTH == 202003)
data16<- filter(data, BLTH == 202004)
data17<- filter(data, BLTH == 202005)
data18<- filter(data, BLTH == 202006)
data19<- filter(data, BLTH == 202007)
data20<- filter(data, BLTH == 202008)
data21<- filter(data, BLTH == 202009)
data22<- filter(data, BLTH == 202010)
data23<- filter(data, BLTH == 202011)
data24<- filter(data, BLTH == 202012)

#Basic Scatter Plot
package="ggplot2"


#Advanced Scatter Plot
gg1 <- ggplot(data1,aes(x=DAYA,y= KWHWBP)) +
  geom_point(aes(col=TARIP))+
  geom_smooth(method ="loess", se=F)
plot(gg1)


