install.packages("plotly")
library(plotly)
library(ggplot2)
library(corrplot)
library(datasets)
library(tidyverse)
library(psych)
library(tidyr)
library(dplyr)

c<-read.csv("imports-85.data.csv")

names(c)[1]<-"symboling"
names(c)[2]<-"normalized_losses"
names(c)[3]<-"make"
names(c)[4]<-"fuel_type"
names(c)[5]<-"aspiration"
names(c)[6]<-"num_of_doors"
names(c)[7]<-"body_style"
names(c)[8]<-"drive_wheels"
names(c)[9]<-"engine_location"
names(c)[10]<-"wheel_base"
names(c)[11]<-"length"
names(c)[12]<-"width"
names(c)[13]<-"height"
names(c)[14]<-"curb_weight"
names(c)[15]<-"engine_type"
names(c)[16]<-"num_of_cylinders"
names(c)[17]<-"engine_size"
names(c)[18]<-"fuel_system"
names(c)[19]<-"bore"
names(c)[20]<-"stroke"
names(c)[21]<-"compression_ratio"
names(c)[22]<-"horsepower"
names(c)[23]<-"peak_rpm"
names(c)[24]<-"city_mpg"
names(c)[25]<-"highway_mpg"
names(c)[26]<-"price"

describe(c,na.rm=TRUE,skew=FALSE)
sapply(c,class)

c <- c[c$'symboling' != '?',]
c <- c[c$'normalized_losses' != '?',]
c <- c[c$'make' != '?',]
c <- c[c$'fuel_type' != '?',]
c <- c[c$'aspiration' != '?',]
c <- c[c$'num_of_doors' != '?',]
c <- c[c$'body_style' != '?',]
c <- c[c$'drive_wheels' != '?',]
c <- c[c$'engine_location' != '?',]
c <- c[c$'wheel_base' != '?',]
c <- c[c$'length' != '?',]
c <- c[c$'width' != '?',]
c <- c[c$'height' != '?',]
c <- c[c$'curb_weight' != '?',]
c <- c[c$'engine_type' != '?',]
c <- c[c$'num_of_cylinders' != '?',]
c <- c[c$'engine_size' != '?',]
c <- c[c$'fuel_system' != '?',]
c <- c[c$'bore' != '?',]
c <- c[c$'stroke' != '?',]
c <- c[c$'compression_ratio' != '?',]
c <- c[c$'horsepower' != '?',]
c <- c[c$'peak_rpm' != '?',]
c <- c[c$'city_mpg' != '?',]
c <- c[c$'highway_mpg' != '?',]
c <- c[c$'price' != '?',]

cc<-subset(c, select = c('normalized_losses',
                         'wheel_base', 'length', 'width', 'height', 'curb_weight',
                         'engine_size', 'bore', 'stroke',
                         'compression_ratio', 'horsepower', 'peak_rpm','city_mpg',
                         'highway_mpg','price')) 

categ<-subset(c, select = c("symboling","make","fuel_type","aspiration","num_of_doors","body_style","drive_wheels","engine_location","engine_type","num_of_cylinders","fuel_system"))

cc[]<-lapply(cc, function(x) as.numeric(x))
c_cor <- cor(x=cc,y=cc,method=c('pearson'))
as.matrix(c_cor)
fig1 <-heatmap(c_cor,sym=TRUE)

ggplot(data=cc, mapping=aes(x=width, y=length, color=height)) +
  geom_point() + ggtitle("Width vs. Length (by height)")

#Continuous Variables
ggplot(data=cc, mapping=aes(x=normalized_losses)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Normalized Losses')
ggplot(data=cc, mapping=aes(x=wheel_base)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Wheel Base')
ggplot(data=cc, mapping=aes(x=horsepower)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Horsepower')
ggplot(data=cc, mapping=aes(x=length)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Length')
ggplot(data=cc, mapping=aes(x=width)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Width')
ggplot(data=cc, mapping=aes(x=height)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Height')
ggplot(data=cc, mapping=aes(x=curb_weight)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Curb Weight')
ggplot(data=cc, mapping=aes(x=engine_size)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Engine Size')
ggplot(data=cc, mapping=aes(x=bore)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Bore')
ggplot(data=cc, mapping=aes(x=stroke)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Stroke')
ggplot(data=cc, mapping=aes(x=compression_ratio)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Compression Ratio')
ggplot(data=cc, mapping=aes(x=peak_rpm)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Peak RPM')
ggplot(data=cc, mapping=aes(x=city_mpg)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of City MPG')
ggplot(data=cc, mapping=aes(x=highway_mpg)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Highway MPG')
ggplot(data=cc, mapping=aes(x=price)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Price')

#Categorical Distributions
ggplot(data=categ, mapping=aes(x=symboling)) + geom_histogram(fill="blue", color='black') + ggtitle('Distribution of Symboling')
ggplot(data=categ, mapping=aes(x=make)) + geom_bar(fill="blue", color='black') + ggtitle('Distribution of Make')
ggplot(data=categ, mapping=aes(x=fuel_type)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Fuel Type')
ggplot(data=categ, mapping=aes(x=aspiration)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Aspiration')
ggplot(data=categ, mapping=aes(x=num_of_doors)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Number of Doors')
ggplot(data=categ, mapping=aes(x=body_style)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Body Style')       
ggplot(data=categ, mapping=aes(x=drive_wheels)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Drive Wheels')
ggplot(data=categ, mapping=aes(x=engine_location)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Enigne Location')
ggplot(data=categ, mapping=aes(x=engine_type)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Engine Type')
ggplot(data=categ, mapping=aes(x=num_of_cylinders)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Number of Cylinders')
ggplot(data=categ, mapping=aes(x=fuel_system)) + geom_histogram(fill="blue", color='black',stat="count") + ggtitle('Distribution of Fuel System')

#PAIRPLOT

pairs(cc)


#BOX PLOT
ggplot(c, aes(x=make, y=price)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(c, aes(x=make, y=engine_size)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
#ggplot(c, aes(x=make, y=normalized_losses)) + 
 # geom_boxplot(outlier.colour="red", outlier.shape=8,
        #       outlier.size=4)
ggplot(c, aes(x=make, y=wheel_base)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
#ggplot(c, aes(x=make, y=bore)) + 
#  geom_boxplot(outlier.colour="red", outlier.shape=8,
              # outlier.size=4)
#ggplot(c, aes(x=make, y=stroke)) + 
 # geom_boxplot(outlier.colour="red", outlier.shape=8,
  #             outlier.size=4)
ggplot(c, aes(x=make, y=compression_ratio)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
#ggplot(c, aes(x=make, y=horsepower)) + 
 # geom_boxplot(outlier.colour="red", outlier.shape=8,
  #             outlier.size=4)
#ggplot(c, aes(x=make, y=peak_rpm)) + 
 # geom_boxplot(outlier.colour="red", outlier.shape=8,
  #             outlier.size=4)
ggplot(c, aes(x=make, y=city_mpg)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
ggplot(c, aes(x=make, y=highway_mpg)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
#ggplot(c, aes(x=make, y=price)) + 
  #geom_boxplot(outlier.colour="red", outlier.shape=8,
              # outlier.size=4)
