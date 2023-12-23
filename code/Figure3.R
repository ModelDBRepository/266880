rm(list=ls())

setwd("C:/Users/prave/Desktop/tables")

library(dplyr)
library(ggplot2)
library(plotrix)
library(tidyverse)
library(ggpubr)

pandemic <- read.csv("pandemic.csv")


lockdown.pre <- pandemic[which(pandemic$Period == "Pre-lockdown"),]
lockdown.post <- pandemic[which(pandemic$Period == "Post-lockdown"),]
reopening <- pandemic[which(pandemic$Period == "Reopening"),]

lockdown.pre.df <- data.frame(period = "Pre-lockdown",
                              date.group = lockdown.pre$date.group,
                              task.condition = lockdown.pre$Task.condition,
                              sabotage.belief = lockdown.pre$Sabotage.belief + 3)
lockdown.pre.df <- lockdown.pre.df[73:202,]

lockdown.post.df <- data.frame(period = "Post-lockdown",
                              date.group = lockdown.post$date.group,
                              task.condition = lockdown.post$Task.condition,
                              sabotage.belief = lockdown.post$Sabotage.belief + 3)

reopening.df <- data.frame(period = "Reopening",
                               date.group = reopening$date.group,
                               task.condition = reopening$Task.condition,
                               sabotage.belief = reopening$Sabotage.belief + 3)


sabotage.df <- rbind(lockdown.pre.df, lockdown.post.df, reopening.df)
sabotage.df.social <- sabotage.df[which(sabotage.df$task.condition == "Social"),]

sabotage.df.social$normalized <- (sabotage.df.social$sabotage.belief-min(sabotage.df.social$sabotage.belief,na.rm = TRUE))/(max(sabotage.df.social$sabotage.belief,na.rm = TRUE)-min(sabotage.df.social$sabotage.belief, na.rm = TRUE))

sabotage.df.social$period <- factor(sabotage.df.social$period,
                                    level = c("Pre-lockdown",
                                              "Post-lockdown",
                                              "Reopening"))

sabotage.df.stats <- sabotage.df.social %>%
                     group_by(period) %>%
                     summarise(sabotage.mean = mean(sabotage.belief, na.rm = TRUE),
                               sabotage.se = std.error(sabotage.belief, na.rm = TRUE),
                               sabotage.mean.norm = mean(normalized, na.rm = TRUE),
                               sabotage.se.norm = std.error(normalized, na.rm = TRUE)) %>%
                     mutate(date.group = c(0,1,2))


# sabotage across period (geom_smooth)

# g1 <- ggplot(sabotage.df.social, aes(x=date.group, y=sabotage.belief)) + 
#              geom_smooth(method = "lm") + geom_jitter(width=0.25) +
#   labs(x = "Pandemic Period",
#       y = "Sabotage belief" ) + stat_cor(label.x = 1.3, label.y = 4) +
#   scale_x_continuous(name = "Pandemic Period",breaks = c(0,1,2),
#                      labels = c("Pre-lockdown",
#                                 "Lockdown",
#                                 "Reopening")) +
#   theme(panel.background = element_blank(),
#         panel.border = element_rect(colour = "black", fill = NA))



# sabotage across period (geom_bar)

# g1 <- ggplot() +
#       geom_bar(data=sabotage.df.stats,aes(x=date.group,y=sabotage.mean, fill=period), stat = "identity",width = 0.5) +
#       geom_errorbar(data=sabotage.df.stats, aes(x=date.group, ymin=sabotage.mean-sabotage.se, ymax=sabotage.mean+sabotage.se), width=0.4, colour="black", alpha=0.9, size=1.3) +
#       geom_point(data=sabotage.df.social, aes(x=date.group, y=sabotage.belief), position = position_jitter(width = .15)) +
#       scale_fill_manual("", values = c("#A9A9A9","#add8e6","#659EC7"),
#                         labels = c("Pre-lockdown",
#                                    "Lockdown",
#                                    "Reopening")) +
#       scale_x_continuous(name = "Pandemic Period", breaks = c(0,1,2),
#                          labels = c("Pre-lockdown",
#                                     "Lockdown",
#                                     "Reopening")) +
#       labs(y = "Sabotage belief") + theme(panel.background = element_blank(),
#                                           panel.border = element_rect(colour = "black", fill = NA), 
#                                           axis.text.x=element_blank(),
#                                           #legend.position = "bottom",
#                                           aspect.ratio = 1)
#                                     #     axis.ticks.x=element_blank())


# normalized

g1 <- ggplot() +
  geom_bar(data=sabotage.df.stats,aes(x=date.group,y=sabotage.mean.norm, fill=period), stat = "identity",width = 0.5) +
  geom_errorbar(data=sabotage.df.stats, aes(x=date.group, ymin=sabotage.mean.norm-sabotage.se.norm, ymax=sabotage.mean.norm+sabotage.se.norm), width=0.4, colour="black", alpha=0.9, size=1.3) +
  geom_point(data=sabotage.df.social, aes(x=date.group, y=normalized), position = position_jitter(width = .15),
             shape=20, color="black", size=1) +
  scale_fill_manual("", values = c("#A9A9A9","#add8e6","#659EC7"),
                    labels = c("Pre-lockdown",
                               "Lockdown",
                               "Reopening")) +
  scale_x_continuous(name = "", breaks = c(0,1,2),
                     labels = c("Pre-lockdown",
                                "Lockdown",
                                "Reopening")) +
  labs(y = "") + theme(panel.background = element_rect(fill = "white"),
                                      #panel.border = element_rect(colour = "black", fill = NA, size = 1),
                                      axis.line = element_line( colour = "black"),
                                      legend.position = "none",
                                      #legend.box = "vertical",
                                      axis.text.x = element_blank(),
                                      axis.text.y = element_blank(),
                                      aspect.ratio = 1)
#     axis.ticks.x=element_blank())



boxplot(sabotage.df.social$normalized ~ sabotage.df.social$period)

sabotage.df.social.pre.reopening <- sabotage.df.social[which(sabotage.df.social$date.group == 0 | sabotage.df.social$date.group == 2),]

t.test(sabotage.df.social.pre.reopening$normalized ~ sabotage.df.social.pre.reopening$period,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)



pandemic.lockdown <- pandemic[which(pandemic$Period == "Post-lockdown"),]


#anno.g2 <- paste("list(italic(r) ==", "-0.26","\n", "p==0.027)")
#anno.g3 <- paste("list(italic(r) ==", "0.16", "p==0.017)")
#anno.g4 <- paste("list(italic(r) ==", "-0.14", "p==0.031)")
#anno.g5 <- paste("list(italic(r) ==", "-0.15", "p==0.020)")
#anno.g6 <- paste("list(italic(r) ==", "-0.18", "p==0.007)")


# stat_cor <- function (mapping = NULL, data = NULL, method = "pearson", cor.coef.name = c("r", 
#                                                                              "rho", "tau"), label.sep = ", ", label.x.npc = "left", label.y.npc = "top", 
#           label.x = NULL, label.y = NULL, output.type = "expression", 
#           digits = 2, r.digits = digits, p.digits = digits, geom = "text", 
#           position = "identity", na.rm = FALSE, show.legend = NA, 
#           inherit.aes = TRUE, ...) 
# {
#   parse <- ifelse(output.type == "expression", TRUE, FALSE)
#   cor.coef.name = cor.coef.name[1]
#   layer(stat = StatCor, data = data, mapping = mapping, geom = geom, 
#         position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
#         params = list(label.x.npc = label.x.npc, label.y.npc = label.y.npc, 
#                       label.x = label.x, label.y = label.y, label.sep = label.sep, 
#                       method = method, output.type = output.type, digits = digits, 
#                       r.digits = r.digits, p.digits = p.digits, cor.coef.name = cor.coef.name, 
#                       parse = parse, na.rm = na.rm, ...))
# }

# sabotage and state proactivity

pandemic.lockdown.social <- pandemic.lockdown[which(pandemic.lockdown$Task.condition == "Social"),]

sabotage.proactivity.df <- data.frame(proactivity = pandemic.lockdown.social$Proactivity.score,
                                      sabotage = pandemic.lockdown.social$Sabotage.belief + 3)

sabotage.proactivity.df$normalized <- (sabotage.proactivity.df$sabotage-min(sabotage.proactivity.df$sabotage,na.rm = TRUE))/(max(sabotage.proactivity.df$sabotage,na.rm = TRUE)-min(sabotage.proactivity.df$sabotage, na.rm = TRUE))


anno.g2 <- paste("list(italic(r) ==", -0.26, ", p==.027)")

g2 <- ggplot(sabotage.proactivity.df, aes(proactivity,normalized)) + 
      geom_smooth(method = "lm") + geom_point(shape=20, color="black", size=1)+
      #stat_cor(label.x = 30, label.y = 0.94, size=2, color="red") +
      #annotate("label", x = 70, y = .92, label = anno.g2, size=7, parse=T) +
      labs(y="",
           subtitle = ) +
      #labs(subtitle = "In social tasks only") +
      theme(panel.background = element_blank(),
            #panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = "none",
            #panel.border = element_rect(colour = "black", fill = NA, size = 1),
            #aspect.ratio = 2,
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x = element_blank(),
            aspect.ratio = 1
            )


# lsr, wsr, mu2, mu3 and state proactivity (block 1)

lockdown.proactivity.df <- data.frame(proactivity = pandemic.lockdown$Proactivity.score,
                                          lsr.b1 = pandemic.lockdown$LSR.block1,
                                          lsr.b2 = pandemic.lockdown$LSR.block2,
                                          lsr.avg = rowMeans(cbind(pandemic.lockdown$LSR.block1,pandemic.lockdown$LSR.block2)),
                                          wsr.b1 = pandemic.lockdown$WSR.block1,
                                          wsr.b2 = pandemic.lockdown$WSR.block2,
                                          wsr.avg = rowMeans(cbind(pandemic.lockdown$WSR.block1,pandemic.lockdown$WSR.block2)),
                                          mu2.b1 = pandemic.lockdown$mu02_1,
                                          mu2.b2 = pandemic.lockdown$mu02_2,
                                          mu2.avg = rowMeans(cbind(pandemic.lockdown$mu02_1,pandemic.lockdown$mu02_2)),
                                          mu3.b1= pandemic.lockdown$mu03_1,
                                          mu3.b2= pandemic.lockdown$mu03_2,
                                          mu3.avg = rowMeans(cbind(pandemic.lockdown$mu03_1,pandemic.lockdown$mu03_2)))


anno.g3 <- paste("list(italic(r) ==", -0.14, ", p==.031)")

g3 <- ggplot(lockdown.proactivity.df, aes(proactivity,wsr.b1)) + 
  geom_smooth(method = "lm") + geom_point(shape=20, color="black", size=1) +
  #stat_cor(label.x = 30, label.y = 0.83, size=2, color="red")  +
  #annotate("label", x = 70, y = .92, label = anno.g3, size=7, parse=T) +
  labs(y="") + ylim(0,1) +
  theme(panel.background = element_blank(),
        #panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        #panel.border = element_rect(colour = "black", fill = NA, size = 1),
        #aspect.ratio = 2,
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        aspect.ratio = 1
        )

anno.g4 <- paste("list(italic(r) ==", 0.16, ", p==.017)")

g4 <- ggplot(lockdown.proactivity.df, aes(proactivity,lsr.b1)) + 
  geom_smooth(method = "lm") + geom_point(shape=20, color="black", size=1) +
  #stat_cor(label.x = 30, label.y = 0.94, size=2, color="red")  +
  #annotate("label", x = 70, y = .92, label = anno.g4, size=7, parse=T) +
  labs(y="") + ylim(0,1) +
  theme(panel.background = element_blank(),
        #panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        #panel.border = element_rect(colour = "black", fill = NA, size = 1),
        #aspect.ratio = 2,
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        aspect.ratio = 1
  )

anno.g5 <- paste("list(italic(r) ==", -0.15, ", p==.02)")

g5 <- ggplot(lockdown.proactivity.df, aes(proactivity,mu2.b1)) + 
  geom_smooth(method = "lm") + geom_point(shape=20, color="black", size=1) +
  #stat_cor(label.x = 30, label.y = -1.1, size=2, color="red")   +
  #annotate("label", x = 70, y = 0.4, label = anno.g5, size=7, parse=T) +
  labs(y="") + ylim(-1.5,0.5) +
  theme(panel.background = element_blank(),
        #panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        #panel.border = element_rect(colour = "black", fill = NA, size = 1),
        #aspect.ratio = 2,
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        aspect.ratio = 1
        )


anno.g6 <- paste("list(italic(r) ==", -0.18, ", p==.0071)")

g6 <- ggplot(lockdown.proactivity.df, aes(proactivity,mu3.b1)) + 
  geom_smooth(method = "lm") + geom_point(shape=20, color="black", size=1) +
  #stat_cor(label.x = 30, label.y = 2, size=2, color="red")  +
  #annotate("label", x = 70, y = 2, label = anno.g6, size=7, parse=T) +
  labs(y="") +
  theme(panel.background = element_blank(),
        #panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none",
        #panel.border = element_rect(colour = "black", fill = NA, size = 1),
        #aspect.ratio = 2,
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        aspect.ratio = 1
        )

# library(gridExtra)
# grid.arrange(g1,g2,g3,g4,g5,g6, layout_matrix = rbind(c(1,1,1,1,1),c(1,1,1,1,1),c(NA,NA,NA,NA,NA),c(2,3,4,5,6)))


# # lsr, wsr, mu2, mu3 and state proactivity (block 2)
# 
# g6 <- ggplot(lockdown.proactivity.df, aes(proactivity,lsr.b2)) + 
#   geom_smooth(method = "lm") +
#   stat_cor()
# 
# g7 <- ggplot(lockdown.proactivity.df, aes(proactivity,wsr.b2)) + 
#   geom_smooth(method = "lm") +
#   stat_cor()
# 
# g8 <- ggplot(lockdown.proactivity.df, aes(proactivity,mu2.b2)) + 
#   geom_smooth(method = "lm") +
#   stat_cor()
# 
# g9 <- ggplot(lockdown.proactivity.df, aes(proactivity,mu3.b2)) + 
#   geom_smooth(method = "lm") +
#   stat_cor()
# 
# 
# # lsr, wsr, mu2, mu3 and state proactivity (avg)
# 
# 
# g10 <- ggplot(lockdown.proactivity.df, aes(proactivity,lsr.avg)) + 
#   geom_smooth(method = "lm") +
#   stat_cor()
# 
# g11 <- ggplot(lockdown.proactivity.df, aes(proactivity,wsr.avg)) + 
#   geom_smooth(method = "lm") +
#   stat_cor()
# 
# g12 <- ggplot(lockdown.proactivity.df, aes(proactivity,mu2.avg)) + 
#   geom_smooth(method = "lm") +
#   stat_cor()
# 
# g13 <- ggplot(lockdown.proactivity.df, aes(proactivity,mu3.avg)) + 
#   geom_smooth(method = "lm") +
#   stat_cor()
# 
# 
# ggarrange(g2,g3,g4,g5,
#           g6,g7,g8,g9,
#           g10,g11,g12,g13, nrow = 4, ncol = 4)