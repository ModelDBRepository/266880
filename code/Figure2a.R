rm(list=ls())

setwd("C:/Pandemic_2020/analysis/data")

library(dplyr)
library(tidyverse)
library(ggpubr)
library(openintro)
library(rstatix)


pandemic <- read.csv("pandemic.csv", 
                     stringsAsFactors = FALSE)
pandemic$Period <- factor(pandemic$Period, levels = c("Pre-lockdown", "Lockdown", "Reopening"))

pandemic.cleaned <- pandemic[-c(1:72),]

pandemic.df <- data.frame(id = 1:nrow(pandemic.cleaned),
                          period = pandemic.cleaned$Period,
                          paranoia = pandemic.cleaned$Paranoia.score.normalized,
                          group = pandemic.cleaned$Paranoia.group)




pandemic.pda.df <- data.frame(id = 1:nrow(pandemic.cleaned),
                              date.group = pandemic.cleaned$date.group,
                              period = pandemic.cleaned$Period,
                              paranoia = as.numeric(pandemic.cleaned$Paranoia.score.normalized),
                              depression = as.numeric(pandemic.cleaned$BDI.score.normalized),
                              anxiety = as.numeric(pandemic.cleaned$BAI.score.normalized))




pda <- pandemic.pda.df %>%
  gather(key = "condition", value = "score", paranoia, depression, anxiety) %>%
  convert_as_factor(id,condition)




# anxiety
boxplot(pandemic.cleaned$BAI.score ~ pandemic.cleaned$Period)
ANOVA1 <- aov(pandemic.cleaned$BAI.score ~ pandemic.cleaned$Period)
summary(ANOVA1)

# depression
boxplot(pandemic.cleaned$BDI.score ~ pandemic.cleaned$Period)
ANOVA2 <- aov(pandemic.cleaned$BDI.score ~ pandemic.cleaned$Period)
summary(ANOVA2)

# paranoia
boxplot(pandemic.cleaned$Paranoia.score ~ pandemic.cleaned$Period)
ANOVA3 <- aov(pandemic.cleaned$Paranoia.score ~ pandemic.cleaned$Period)
summary(ANOVA3)



pandemic.nonormalized <- data.frame(id = 1:nrow(pandemic.cleaned),
                                    date.group = pandemic.cleaned$date.group,
                                    period = pandemic.cleaned$Period,
                                    paranoia = as.numeric(pandemic.cleaned$Paranoia.score),
                                    depression = as.numeric(pandemic.cleaned$BDI.score),
                                    anxiety = as.numeric(pandemic.cleaned$BAI.score))




pda.nonnormalized <- pandemic.nonormalized %>%
  gather(key = "condition", value = "score", paranoia, depression, anxiety) %>%
  convert_as_factor(id,condition)

pda.df <- pda.nonnormalized %>%
  group_by(date.group,period, condition) %>%
  summarise(mean = mean(score, na.rm = TRUE),
            sem = std.error(score, na.rm = TRUE))


# covid cases
cases.deaths <- read.csv('confirmed_covid_cases_matchedDate.csv')
cases.deaths$state <- state2abbr(cases.deaths$state)

df3 <- cases.deaths %>%
  group_by(date.group) %>%
  summarise(num.cases = sum(cases),
            mean = mean(cases),
            sd = std.error(cases))

df3$normalized.mean = (df3$mean-min(df3$mean))/(max(df3$mean)-min(df3$mean))
df3$normalized.sd = (df3$sd-min(df3$sd))/(max(df3$sd)-min(df3$sd))

df3$log.mean = log(df3$mean)
df3$log.sd = log(df3$sd)
df3$colorGroup = c("darkorchid")


plot1 <- ggplot(pda.df, aes(x=date.group, y=mean)) +
  geom_bar(aes(fill=condition, alpha=period), stat="identity", position=position_dodge(width=0.9)) +
  geom_errorbar(aes(fill=condition, alpha=period, ymin=mean - sem, ymax=mean + sem), 
                width=0.2, 
                position=position_dodge(width=0.9),
                width=0.4,
                colour="black",
                alpha=0.9,
                size=1.3) +
  geom_point(data=pda,aes(x=date.group,y=score),position=position_jitter(width =.15),
             shape=20, color="black", size=1)+
  geom_line(data=df3,aes(x=date.group, y = normalized.mean, color="colorGroup"),size=1.5) + 
  geom_point(data=df3,aes(x=date.group, y = normalized.mean)) +
  scale_color_manual(name="", labels = "COVID-19", values=c("#696969")) +
  scale_alpha_manual(values = c(0.6, 0.8, 1)) +
  scale_fill_manual(values = c("#0f7718","#1c1189","#E74C3C")) +
  scale_x_continuous(breaks = c(0,1,2)) +
  scale_y_continuous(sec.axis = dup_axis(name = ""))+
  labs(x="",
       y="") + theme(panel.background = element_rect(fill = "white"),
                     #panel.border = element_rect(colour = "black", fill = NA, size = 1),
                     axis.line = element_line( colour = "black"),
                     legend.position = "none",
                     #legend.box = "vertical",
                     axis.text.x=element_blank(),
                     axis.text.y = element_blank(),
                     aspect.ratio = 1)



