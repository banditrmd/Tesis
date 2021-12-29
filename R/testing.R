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
theme_set(theme_classic())

#Intial Data
data<-data.table(bisnis3)


#Sample Row
data1<- data[sample(.N,100000)]
data4<- data[sample(.N,250000)]
data5<- data[sample(.N,200000)]
data6<- data[sample(.N,90000)]

#Filter data
data2<- filter(data, BLTH == 202007)
data3<- filter(data, BLTH == 202009)

#Basic Scatter Plot
package="ggplot2"

ggplot(data,aes(x=PEMKWH,y=RPPLN))+
          geom_point(size=5) + geom_line()

#Advanced Scatter Plot
gg1 <- ggplot(data5,aes(x=JAMNYALA,y= RPPLN)) +
      geom_point(aes(col=KWHWBP,size=KWHLWBP))+
      geom_smooth(method ="loess", se=F)
plot(gg1)
  


#Boxplot
ggplot(data1,aes(x=BLTH,Y=PEMKWH)) +
  geom_boxplot(varwidth = T, fill="plum") +
  labs(title="Boxplot",
       x="TARIP",
       y="METER")

#Timeseries
ggplot(data1,aes(x=BLTH))+geom_line(aes(y=PEMKWH))
