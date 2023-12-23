rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/modelDB/data")

# load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
#install.packages("devtools")
library(devtools)
#devtools::install_github("kassambara/easyGgplot2")
library(easyGgplot2)
library(openintro)
#source('A:/Yale/PS003/reopening/archive-code/multiplot.R')
library(plotrix)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(plotrix)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(reshape2)
library(openintro)
library(lubridate)
library(anytime)

# read data
pandemic <- read.csv('pandemic.csv')

#### WSR, Mu3 and protesting in required vs recommended states
pandemic.reopening <- pandemic[which(pandemic$Period == "Reopening"),]


pandemic.reopening.df <- data.frame(mwr = pandemic.reopening$MWR,
                                    wsr.block1 = pandemic.reopening$WSR.block1,
                                    wsr.block2 = pandemic.reopening$WSR.block2,
                                    lsr.block1 = pandemic.reopening$LSR.block1,
                                    lsr.block2 = pandemic.reopening$LSR.block2,
                                    mu3.block1 = pandemic.reopening$mu03_1,
                                    mu3.block2 = pandemic.reopening$mu03_2,
                                    mu2.block1 = pandemic.reopening$mu02_1,
                                    mu2.block2 = pandemic.reopening$mu02_2)

pandemic.reopening.df$wsr.avg <- rowMeans(pandemic.reopening.df[,2:3])
pandemic.reopening.df$lsr.avg <- rowMeans(pandemic.reopening.df[,4:5])
pandemic.reopening.df$mu3.avg <- rowMeans(pandemic.reopening.df[,6:7])
pandemic.reopening.df$mu2.avg <- rowMeans(pandemic.reopening.df[,8:9])

### 3.1.2: Win-switch rate (top figure)
pandemic.reopening.df.wsravg <- pandemic.reopening.df %>%
  group_by(mwr) %>%
  summarise(wsr.mean = mean(wsr.avg),
            wsr.se = std.error(wsr.avg)) %>%
  mutate(mwr.group = c("Recommended","Required"))

p2<-ggplot(pandemic.reopening.df.wsravg, aes(x = mwr, y= wsr.mean, fill = as.factor(mwr))) +
  geom_bar(stat="identity", width=.5) +
  geom_errorbar(aes(x=mwr, ymin=wsr.mean-wsr.se, ymax=wsr.mean+wsr.se), width=0.4, colour="black", alpha=0.9, size=1.3) +
  geom_point(data=pandemic.reopening.df, aes(x=mwr, y=wsr.avg), position = position_jitter(width = .15),
             shape=20, color="black", size=1) +
  scale_fill_manual("",values = c("#FFA500","#008000")) +
  labs(x="",
       y="") +
  scale_x_continuous(breaks = c(0,1)) +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        #panel.border = element_rect(colour = "black", fill = NA, size = 2),
        aspect.ratio = 1,
        axis.text.x=element_blank(),
        axis.text.y = element_blank())

t.test(pandemic.reopening.df$wsr.avg~ pandemic.reopening.df$mwr,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)


### 3.3: Mu3 (middle figure)
pandemic.reopening.df.mu3block1 <- pandemic.reopening.df %>%
  group_by(mwr) %>%
  summarise(mu3.mean = mean(mu3.block1),
            mu3.se = std.error(mu3.block1)) %>%
  mutate(mwr.group = c("Recommended","Required"))

p3<-ggplot(pandemic.reopening.df.mu3block1, aes(x = mwr, y= mu3.mean, fill = as.factor(mwr))) +
  geom_bar(stat="identity", width=.5) +
  geom_errorbar(aes(x=mwr, ymin=mu3.mean-mu3.se, ymax=mu3.mean+mu3.se), width=0.4, colour="black", alpha=0.9, size=1.3)+
  geom_point(data=pandemic.reopening.df, aes(x=mwr, y=mu3.block1), position = position_jitter(width = .15),
             shape=20, color="black", size=1) +
  scale_fill_manual("",values = c("#FFA500", "#008000")) +
  labs(x="",
       y="") +
  scale_x_continuous(breaks = c(0,1)) +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        #panel.border = element_rect(colour = "black", fill = NA, size = 2),
        aspect.ratio = 1,
        axis.text.x=element_blank(),
        axis.text.y = element_blank()) 


t.test(pandemic.reopening.df$mu3.block1~ pandemic.reopening.df$mwr,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)


## Protests
dat <- read.csv("protest.csv")
protest <- dat[which(dat$EVENT_TYPE == "Protests"),]

protest.df <- data.frame(date = protest$EVENT_DATE,
                         date2 = anydate(protest$EVENT_DATE),
                         type = protest$EVENT_TYPE,
                         association = protest$ASSOC_ACTOR_1,
                         state = protest$ADMIN1)

protest.df$state <- state2abbr(protest.df$state)

june.masks.req.nine <- c("CA","NM","MI","IL","NY","MA","RI","MD","VA","ME","DE")
june.states.mask.rec <- c("AK","AL","AR","AZ","CO","CT","DC","FL","GA","HI","IA",
                          "ID","IN","KS","KY","LA","MN","MO","MS","MT","NC","ND",
                          "NE","NH","NJ","NV","OH","OK","OR","PA","SC","SD","TN",
                          "TX","UT","VT","WA","WI","WV","WY")


protest.df$mask <- ifelse(protest.df$state %in% june.masks.req.nine, "mandate",
                          ifelse(protest.df$state %in% june.states.mask.rec, "recommend","")
)
#8656 - subset at this value because this corresponds to the date where we stopped recruiting the pandemic data
protest.df.subset <- protest.df[c(1:8656),]


protest.df.summarize <- protest.df.subset %>%
  group_by(date2,mask) %>%
  summarise(count = n())

protest.df.summarize$mask <- factor(protest.df.summarize$mask, levels = c("recommend","mandate"))

protest.df.stats <- protest.df.summarize %>%
  group_by(mask) %>%
  summarise(mean.protest = mean(count, na.rm = TRUE),
            se.protest = std.error(count, na.rm = TRUE))


ggplot(protest.df.stats, aes(x = mask, y= mean.protest, fill = as.factor(mask))) +
  geom_bar(stat="identity", width=.5) +
  geom_errorbar(aes(x=mask, ymin=mean.protest-se.protest, ymax=mean.protest+se.protest), width=0.4, colour="black", alpha=0.9, size=1.3)+
  geom_point(data=protest.df.summarize, aes(x=mask, y=count), position = position_jitter(width = .15),
             shape=20, color="black", size=1) +
  scale_fill_manual("",values = c("#FFA500", "#008000")) +
  labs(x="",
       y="") +
  #scale_x_continuous(breaks = c(0,1)) +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        #panel.border = element_rect(colour = "black", fill = NA, size = 2),
        aspect.ratio = 1,
        axis.text.x=element_blank(),
        axis.text.y = element_blank()) 


t.test(protest.df.summarize$count ~ protest.df.summarize$mask,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)



