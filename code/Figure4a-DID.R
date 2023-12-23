rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/modelDB/data")

# load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(devtools)
library(easyGgplot2)
library(openintro)
library(plotrix)
library(gridExtra)


# read data
pandemic <- read.csv('pandemic.csv')



#### Analysis 2: Impact Assessment using DiD
#pandemic.did.startPre <- pandemic[73:605,]
pandemic.did.startPost <- pandemic[203:605,]


pandemic.did.startPost = pandemic.did.startPost %>%
  mutate(postMay = month >= 3,
         mask.policy = MWR >= 1)

#pandemic$month <- factor(pandemic$month, levels = c("Mar","Apr","Jun","Jul"))

pandemic.plot <- pandemic.did.startPost
pandemic.plot$mask.policy[which(pandemic.plot$mask.policy == "FALSE")] <- "Recommended"
pandemic.plot$mask.policy[which(pandemic.plot$mask.policy == "TRUE")] <- "Required"

#pandemic.plot$mask.policy <- factor(pandemic.plot$mask.policy, levels = c("Required (n=275)","Recommended (n=128)"))


p1<- ggplot(pandemic.plot, aes(x=month, y=Paranoia.score, color = mask.policy)) +
  stat_summary(geom = 'smooth') +
  geom_vline(xintercept = 2.5, linetype = 'dashed') +
  geom_text(aes(x=2.5, label="", y=1.5),
            colour="#303030",
            angle=90,
            size=5) +
  labs(x = "",
       y = "",
       color = "")+ #ylim(0,1) +
  scale_color_manual(values=c("#FFA500","#008000"))+
  scale_x_continuous(breaks = c(1,2,3,4),
                     labels = c("","","","")) +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        axis.text.x = element_blank()#,
        #axis.text.y = element_blank()
        #plot.margin = margin(t=.5,r=.4,b=.67,l=.4,"cm")
  )