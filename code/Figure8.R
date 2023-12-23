rm(list=ls())

setwd("C:/Pandemic_2020/analysis/data")

library(dplyr)
library(tidyverse)
library(ggpubr)


dat <- read.csv("replication.csv", 
                stringsAsFactors = FALSE)
#tmp <- read.csv("tmp1.csv")
# tmp2 <- read.csv("tmp2.csv")

dat.df <- dat %>%
  select(ID, 
         Score, Group,
         covid.conspiracy.39:covid.conspiracy.43)


dat.df$convert.covid.conspiracy.39 <- ifelse(dat.df$covid.conspiracy.39 == "Strongly disagree",1,
                                             ifelse(dat.df$covid.conspiracy.39 == "Disagree",2,
                                                    ifelse(dat.df$covid.conspiracy.39 == "Somewhat disagree",3,
                                                           ifelse(dat.df$covid.conspiracy.39 == "Neutral",4,
                                                                  ifelse(dat.df$covid.conspiracy.39 == "Somewhat agree",5,
                                                                         ifelse(dat.df$covid.conspiracy.39 == "Agree",6,
                                                                                ifelse(dat.df$covid.conspiracy.39 == "Strongly agree",7,"")))))))
dat.df$convert.covid.conspiracy.39 <- as.numeric(dat.df$convert.covid.conspiracy.39)


dat.df$convert.covid.conspiracy.40 <- ifelse(dat.df$covid.conspiracy.40 == "Strongly disagree",1,
                                             ifelse(dat.df$covid.conspiracy.40 == "Disagree",2,
                                                    ifelse(dat.df$covid.conspiracy.40 == "Somewhat disagree",3,
                                                           ifelse(dat.df$covid.conspiracy.40 == "Neutral",4,
                                                                  ifelse(dat.df$covid.conspiracy.40 == "Somewhat agree",5,
                                                                         ifelse(dat.df$covid.conspiracy.40 == "Agree",6,
                                                                                ifelse(dat.df$covid.conspiracy.40 == "Strongly agree",7,"")))))))
dat.df$convert.covid.conspiracy.40 <- as.numeric(dat.df$convert.covid.conspiracy.40)


dat.df$convert.covid.conspiracy.41 <- ifelse(dat.df$covid.conspiracy.41 == "Strongly disagree",1,
                                             ifelse(dat.df$covid.conspiracy.41 == "Disagree",2,
                                                    ifelse(dat.df$covid.conspiracy.41 == "Somewhat disagree",3,
                                                           ifelse(dat.df$covid.conspiracy.41 == "Neutral",4,
                                                                  ifelse(dat.df$covid.conspiracy.41 == "Somewhat agree",5,
                                                                         ifelse(dat.df$covid.conspiracy.41 == "Agree",6,
                                                                                ifelse(dat.df$covid.conspiracy.41 == "Strongly agree",7,"")))))))
dat.df$convert.covid.conspiracy.41 <- as.numeric(dat.df$convert.covid.conspiracy.41)



dat.df$convert.covid.conspiracy.42 <- ifelse(dat.df$covid.conspiracy.42 == "Strongly disagree",1,
                                             ifelse(dat.df$covid.conspiracy.42 == "Disagree",2,
                                                    ifelse(dat.df$covid.conspiracy.42 == "Somewhat disagree",3,
                                                           ifelse(dat.df$covid.conspiracy.42 == "Neutral",4,
                                                                  ifelse(dat.df$covid.conspiracy.42 == "Somewhat agree",5,
                                                                         ifelse(dat.df$covid.conspiracy.42 == "Agree",6,
                                                                                ifelse(dat.df$covid.conspiracy.42 == "Strongly agree",7,"")))))))
dat.df$convert.covid.conspiracy.42 <- as.numeric(dat.df$convert.covid.conspiracy.42)


dat.df$convert.covid.conspiracy.43 <- ifelse(dat.df$covid.conspiracy.43 == "Strongly disagree",1,
                                             ifelse(dat.df$covid.conspiracy.43 == "Disagree",2,
                                                    ifelse(dat.df$covid.conspiracy.43 == "Somewhat disagree",3,
                                                           ifelse(dat.df$covid.conspiracy.43 == "Neutral",4,
                                                                  ifelse(dat.df$covid.conspiracy.43 == "Somewhat agree",5,
                                                                         ifelse(dat.df$covid.conspiracy.43 == "Agree",6,
                                                                                ifelse(dat.df$covid.conspiracy.43 == "Strongly agree",7,"")))))))
dat.df$convert.covid.conspiracy.43 <- as.numeric(dat.df$convert.covid.conspiracy.43)



dat.df$covidvaccineconspiracy.sum <- rowSums(dat.df[,9:13], na.rm = TRUE)


Total.Ans <- list()
for (i in 1:nrow(dat.df)){
  Total.Ans[[i]] <- length(which(dat.df[i,9:13] >= 0))
}
dat.df$Total.Ans <- unlist(Total.Ans)

#dat.df$covidvaccineconspiracy.totalAns <- tmp$Total.Ans
dat.df$covidvaccineconspiracy.score <- dat.df$covidvaccineconspiracy.sum/dat.df$Total.Ans

dat.df$WSR.block1 <- dat$WSR.block1
dat.df$WSR.block2 <- dat$WSR.block2
dat.df$WSR <- rowMeans(cbind(dat.df$WSR.block1,dat.df$WSR.block2))

dat.df$LSR.block1 <- dat$LSR.block1
dat.df$LSR.block2 <- dat$LSR.block2
dat.df$LSR <- rowMeans(cbind(dat.df$LSR.block1,dat.df$LSR.block2))

dat.df$mu2.block1 <- dat$mu02_1
dat.df$mu2.block2 <- dat$mu02_2
dat.df$mu2 <- rowMeans(cbind(dat.df$mu2.block1,dat.df$mu2.block2))

dat.df$omega2.block1 <- dat$omega2_1
dat.df$omega2.block2 <- dat$omega2_2
dat.df$omega2 <- rowMeans(cbind(dat.df$omega2.block1,dat.df$omega2.block2))

dat.df$mu3.block1 <- dat$mu03_1
dat.df$mu3.block2 <- dat$mu03_2
dat.df$mu3 <- rowMeans(cbind(dat.df$mu3.block1,dat.df$mu3.block2))

dat.df$omega3.block1 <- dat$omega3_1
dat.df$omega3.block2 <- dat$omega3_2
dat.df$omega3 <- rowMeans(cbind(dat.df$omega3.block1,dat.df$omega3.block2))

dat.df$kappa.block1 <- dat$kappa2_1
dat.df$kappa.block2 <- dat$kappa2_2
dat.df$kappa <- rowMeans(cbind(dat.df$kappa.block1,dat.df$kappa.block2))


p9 <- ggplot(dat.df, aes(x=covidvaccineconspiracy.score, y=Score)) + 
  geom_smooth(method="lm" , color="red", fill="#69b3a2") + 
  geom_point(shape=16,color="#8A2BE2",size=2,alpha=0.4) + #stat_cor(label.x=2) + 
  labs(title ="",x="",y="") +
    theme(panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none",
          #panel.border = element_rect(colour = "black", fill = NA, size = 2),
          #aspect.ratio = 1,
          axis.text.x=element_blank(),
          axis.text.y = element_blank()) 

p10 <- ggplot(dat.df, aes(x=covidvaccineconspiracy.score, y=WSR)) + 
  geom_smooth(method="lm" , color="red", fill="#69b3a2") + 
  geom_point(shape=16,color="#8A2BE2",size=2,alpha=0.4) + #stat_cor(label.x = 1,label.y = 0.5) + 
  labs(title ="",x="",y="") + 
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

p11 <- ggplot(dat.df, aes(x=covidvaccineconspiracy.score, y=LSR)) + 
  geom_smooth(method="lm" , color="red", fill="#69b3a2") + 
  geom_point(shape=16,color="#8A2BE2",size=2,alpha=0.4) + #stat_cor(label.x = 3,label.y = 0.6) + 
  labs(x="", y="") +
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


p12 <- ggplot(dat.df, aes(x=covidvaccineconspiracy.score, y=mu3)) + 
  geom_smooth(method="lm" , color="red", fill="#69b3a2") + 
  geom_point(shape=16,color="#8A2BE2",size=2,alpha=0.4) + #stat_cor(label.x = 3,label.y = -1.5) + 
  labs(title ="", x="",y="") + 
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

# p13 <- ggplot(dat.df, aes(x=covidvaccineconspiracy.score, y=omega3)) + 
#   geom_smooth(method = "lm") + geom_point() + stat_cor(label.x = 3,label.y = 0) + labs(x="COVID-19 vaccine conspiracy belief",y=expression(omega[3]))

p13 <- ggplot(dat.df, aes(x=covidvaccineconspiracy.score, y=mu2)) + 
  geom_smooth(method="lm" , color="red", fill="#69b3a2") + 
  geom_point(shape=16,color="#8A2BE2",size=2,alpha=0.4) + #stat_cor(label.x = 3,label.y = -0.6) + 
  labs(title ="", x="",y="") + 
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


# p15 <- ggplot(dat.df, aes(x=covidvaccineconspiracy.score, y=omega2)) + 
#   geom_smooth(method = "lm") + geom_point() + stat_cor(label.x = 2,label.y = -0.5) + labs(x="COVID-19 vaccine conspiracy belief",y=expression(omega[2]))
# 
# p16 <- ggplot(dat.df, aes(x=covidvaccineconspiracy.score, y=kappa)) + 
#   geom_smooth(method = "lm") + geom_point() + stat_cor(label.x = 2,label.y = 0.55) + labs(title ="Coupling",x="COVID-19 vaccine conspiracy belief",y=expression(kappa)) + theme(plot.title = element_text(hjust = 0.5))
# 

# ggarrange(p10,p12,p14,p16,
#           p11,p13,p15,ncol = 4, nrow = 2)

library(gridExtra)
grid.arrange(p9,p10,p12,
                          p11,p13, layout_matrix = rbind(c(1,1,2,3),
                                                         c(1,1,4,5)))
