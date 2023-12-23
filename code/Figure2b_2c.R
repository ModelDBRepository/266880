rm(list=ls())

setwd("C:/Users/prave/Desktop/tables")

library(dplyr)
library(ggplot2)
library(plotrix)
library(tidyverse)
library(ggpubr)
library(gridExtra)

pandemic <- read.csv("pandemic.csv")


lockdown.pre <- pandemic[which(pandemic$Period == "Pre-lockdown"),]
lockdown.post <- pandemic[which(pandemic$Period == "Post-lockdown"),]
reopening <- pandemic[which(pandemic$Period == "Reopening"),]

lockdown.pre.df <- data.frame(period = "Pre-lockdown",
                              date.group = lockdown.pre$date.group,
                              paranoia.group = lockdown.pre$Paranoia.group,
                              wsr.b1 = lockdown.pre$WSR.block1,
                              wsr.b2 = lockdown.pre$WSR.block2,
                              wsr.avg = rowMeans(cbind(lockdown.pre$WSR.block1,lockdown.pre$WSR.block2)),
                              lsr.b1 = lockdown.pre$LSR.block1,
                              lsr.b2 = lockdown.pre$LSR.block2,
                              lsr.avg = rowMeans(cbind(lockdown.pre$LSR.block1,lockdown.pre$LSR.block2)),
                              mu02.b1 = lockdown.pre$mu02_1,
                              mu02.b2 = lockdown.pre$mu02_2,
                              mu02.avg = rowMeans(cbind(lockdown.pre$mu02_1,lockdown.pre$mu02_2)),
                              mu03.b1 = lockdown.pre$mu03_1,
                              mu03.b2 = lockdown.pre$mu03_2,
                              mu03.avg = rowMeans(cbind(lockdown.pre$mu03_1,lockdown.pre$mu03_2)),
                              kappa.b1 = lockdown.pre$kappa2_1,
                              kappa.b2 = lockdown.pre$kappa2_2,
                              kappa.avg = rowMeans(cbind(lockdown.pre$kappa2_1,lockdown.pre$kappa2_2)))
lockdown.pre.df <- lockdown.pre.df[73:202,]

lockdown.post.df <- data.frame(period = "Post-lockdown",
                              date.group = lockdown.post$date.group,
                              paranoia.group = lockdown.post$Paranoia.group,
                              wsr.b1 = lockdown.post$WSR.block1,
                              wsr.b2 = lockdown.post$WSR.block2,
                              wsr.avg = rowMeans(cbind(lockdown.post$WSR.block1,lockdown.post$WSR.block2)),
                              lsr.b1 = lockdown.post$LSR.block1,
                              lsr.b2 = lockdown.post$LSR.block2,
                              lsr.avg = rowMeans(cbind(lockdown.post$LSR.block1,lockdown.post$LSR.block2)),
                              mu02.b1 = lockdown.post$mu02_1,
                              mu02.b2 = lockdown.post$mu02_2,
                              mu02.avg = rowMeans(cbind(lockdown.post$mu02_1,lockdown.post$mu02_2)),
                              mu03.b1 = lockdown.post$mu03_1,
                              mu03.b2 = lockdown.post$mu03_2,
                              mu03.avg = rowMeans(cbind(lockdown.post$mu03_1,lockdown.post$mu03_2)),
                              kappa.b1 = lockdown.post$kappa2_1,
                              kappa.b2 = lockdown.post$kappa2_2,
                              kappa.avg = rowMeans(cbind(lockdown.post$kappa2_1,lockdown.post$kappa2_2)))

reopening.df <- data.frame(period = "Reopening",
                              date.group = reopening$date.group,
                              paranoia.group = reopening$Paranoia.group,
                              wsr.b1 = reopening$WSR.block1,
                              wsr.b2 = reopening$WSR.block2,
                              wsr.avg = rowMeans(cbind(reopening$WSR.block1,reopening$WSR.block2)),
                           lsr.b1 = reopening$LSR.block1,
                           lsr.b2 = reopening$LSR.block2,
                           lsr.avg = rowMeans(cbind(reopening$LSR.block1,reopening$LSR.block2)),
                           mu02.b1 = reopening$mu02_1,
                           mu02.b2 = reopening$mu02_2,
                           mu02.avg = rowMeans(cbind(reopening$mu02_1,reopening$mu02_2)),
                           mu03.b1 = reopening$mu03_1,
                           mu03.b2 = reopening$mu03_2,
                           mu03.avg = rowMeans(cbind(reopening$mu03_1,reopening$mu03_2)),
                           kappa.b1 = reopening$kappa2_1,
                           kappa.b2 = reopening$kappa2_2,
                           kappa.avg = rowMeans(cbind(reopening$kappa2_1,reopening$kappa2_2)))


fig1.df <- rbind(lockdown.pre.df,lockdown.post.df,reopening.df)

fig1.df$period <- factor(fig1.df$period, level = c("Pre-lockdown","Post-lockdown","Reopening"))
fig1.df$paranoia.group <- factor(fig1.df$paranoia.group, level = c("low","high"))


## Figure 2b
wsr.avg.df <- fig1.df %>%
  group_by(period, paranoia.group) %>% # removed paranoia.group
  summarise(mean = mean(wsr.avg, na.rm = TRUE),
            se = std.error(wsr.avg, na.rm = TRUE)) %>%
  mutate(param = c("Win-switch rate"))
lsr.avg.df <- fig1.df %>%
  group_by(period, paranoia.group) %>% # removed paranoia.group
  summarise(mean = mean(lsr.avg, na.rm = TRUE),
            se = std.error(lsr.avg, na.rm = TRUE)) %>%
  mutate(param = c("Lose-stay rate"))




# wsr
g1 <- ggplot(wsr.avg.df, aes(x=paranoia.group, y=mean, fill=period)) +
   geom_bar(stat = "identity", width = 0.7) +
   geom_errorbar(data=wsr.avg.df, aes(x=paranoia.group, ymin=mean-se, ymax=mean+se), width=0.4, colour="black", alpha=0.9, size=1.3) +
   geom_point(data=fig1.df, aes(x=paranoia.group, y=wsr.avg), position = position_jitter(width = .15),
              shape=20, color="black", size=1) +
   facet_grid(. ~ period) +
   scale_fill_manual(name="Pandemic period",
                     values = c("#FADBD8","#F1948A","#E74C3C"),
                     labels = c("Pre-lockdown",
                                "Lockdown",
                                "Reopening")) +
  #scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1)) +
   labs(x="",
        y="") +
   theme(panel.background = element_rect(fill = "white"),
         legend.position = "none",
         axis.line = element_line(colour = "black"),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         strip.background = element_blank(),
         strip.text.x = element_blank())


 # lsr
g2 <- ggplot(lsr.avg.df, aes(x=paranoia.group, y=mean, fill=period)) +
   geom_bar(stat = "identity", width = 0.7) +
   geom_errorbar(data=lsr.avg.df, aes(x=paranoia.group, ymin=mean-se, ymax=mean+se), width=0.4, colour="black", alpha=0.9, size=1.3) +
   geom_point(data=fig1.df, aes(x=paranoia.group, y=lsr.avg), position = position_jitter(width = .15),
              shape=20, color="black", size=1) +
   facet_grid(. ~ period) +
   scale_fill_manual(name="Pandemic period",
                     values = c("#FADBD8","#F1948A","#E74C3C"),
                     labels = c("Pre-lockdown",
                                "Lockdown",
                                "Reopening")) +
  #scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1)) +
   labs(x="",
        y="") + 
   theme(panel.background = element_rect(fill = "white"),
         legend.position = "none",
         axis.line = element_line(colour = "black"),
         axis.text.x = element_blank(),
         axis.text.y = element_blank(),
         strip.background = element_blank(),
         strip.text.x = element_blank())



## Figure 2c 
mu2.avg.df <- fig1.df %>%
  group_by(period, paranoia.group) %>% # removed paranoia.group
  summarise(mean = mean(mu02.avg, na.rm = TRUE),
            se = std.error(mu02.avg, na.rm = TRUE)) %>%
  mutate(param = c("Contingency belief"))
mu3.avg.df <- fig1.df %>%
  group_by(period,paranoia.group) %>% # removed paranoia.group
  summarise(mean = mean(mu03.avg, na.rm = TRUE),
            se = std.error(mu03.avg, na.rm = TRUE)) %>%
  mutate(param = c("Volatility belief"))




# mu2
g3 <- ggplot(mu2.avg.df, aes(x=paranoia.group, y=mean, fill=period)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_errorbar(data=mu2.avg.df, aes(x=paranoia.group, ymin=mean-se, ymax=mean+se), width=0.4, colour="black", alpha=0.9, size=1.3) +
  geom_point(data=fig1.df, aes(x=paranoia.group, y=mu02.avg), position = position_jitter(width = .15),
             shape=20, color="black", size=1) +
  facet_grid(. ~ period) +
  scale_fill_manual(name="Pandemic period",
                    values = c("#FADBD8","#F1948A","#E74C3C"),
                    labels = c("Pre-lockdown",
                               "Lockdown",
                               "Reopening")) +
  labs(x="",
       y="") + 
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())
 


g4 <-  ggplot(mu3.avg.df, aes(x=paranoia.group, y=mean, fill=period)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_errorbar(data=mu3.avg.df, aes(x=paranoia.group, ymin=mean-se, ymax=mean+se), width=0.4, colour="black", alpha=0.9, size=1.3) +
  geom_point(data=fig1.df, aes(x=paranoia.group, y=mu03.avg), position = position_jitter(width = .15),
             shape=20, color="black", size=1) +
  facet_grid(. ~ period) +
  scale_fill_manual(name="Pandemic period",
                    values = c("#FADBD8","#F1948A","#E74C3C"),
                    labels = c("Pre-lockdown",
                               "Lockdown",
                               "Reopening")) +
  labs(x="",
       y="") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank())
