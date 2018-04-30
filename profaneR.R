setwd("~/Box Sync/TEMPLE/WBCA/R stuff/R Workshops/R Users Group/Feb.8.2018")
install.packages("reshape2")
install.packages("tidyverse")
install.packages("ggplot")

require(ggplot2)
require(dplyr)
require(reshape2)
require(tidyverse)

linear.df <- read.csv("linear.csv", header=T)

head(linear.df)

colnames(linear.df)
linmelt <- linear.df %>% melt(measure.var=c(15:54), id.var=c(2,7,9))
linmelt

lms <- mutate(linmelt, time=substr(variable, start=4, stop=5))
table(lms$time)



lms$time<-as.numeric(lms$time)
table(lms$Cond)
dim(lms)
colnames(lms)
rownames(head(lms))


length(unique(lms$Subject))
table(lms[lms$Subject==2,]$variable)

?unique()

lincond <- group_by(lms, Cond, time)
lincond <- summarize(lincond, mean=mean(value, na.rm=T))
lincond
str(lincond)
require(ggplot2)
ggplot(data=lincond, aes(x=time, y=mean, color=Cond)) +
  geom_smooth(fill=NA)+
  geom_point()
  


# Setting working directory
setwd("c:/data/downloads/")
setwd("C:/Users/klugman/Dropbox/r/data")
# Reading in data
linear<-read.csv("linear.csv", header=T)

# Installing formatR package to do RMarkdown
# have to choose 0-Cloud mirror for lab computers
chooseCRANmirror(ind=1)
install.packages("formatR")
library(formatR)
library(dplyr)
library(reshape2)
library(tidyverse)
library(ggplot2)


# converting linear to tbl
linear<-as.tbl(linear)

# looking at data
head(linear)

# creating vector of bin variable names
linnames=colnames(linear)[15:54]





# reshaping data
# "linear" is a subject*word*decisecond dataset, there are 22 subjects, 49 words, and 40 deciseconds per subject*word
# "binxx" variables contain data on pupil diameter
# there are four categories of words ("Cond"): Profane, Technical, Non-Word, Neutral
# we are reshaping so there are now 49*40 lines per subject
linmelt<-melt(linear, id.vars=c("ID", "Subject", "Item", "Cond"), measure.vars=linnames)

# creating clock variable that just contains numerals 1-40 (as opposed to "Bin1", "Bin2", etc)
linmelt<-mutate(linmelt, time=substr(variable, start=4, stop=5))
linmelt$time<-as.numeric(linmelt$time)

# Tracing pupil diameters over time for each profane word
ggplot(linmelt[linmelt$Cond=="Profane",], aes(x=time, y=value, color=Item))+
  geom_smooth(fill=NA)


# collapsing the dataset to average across all subjects and words within condition*time
# so we will have a dataset that is 40 lines per condition, for a total of 160 lines
lincond<-group_by(linmelt, Cond, time)
lincond<-summarize(lincond, mean=mean(value, na.rm=T))
lincond

# plotting average pupil length by time for each condition
ggplot(data=lincond, aes(x=time, y=mean, color=Cond))+
  geom_smooth(fill=NA)+
  geom_point()

