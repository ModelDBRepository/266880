rm(list=ls())

setwd("C:/Pandemic_2020/modelDB/data")

library(dplyr)
library(lessR)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(plyr)


# read data
demo.pandemic <- read.csv("pandemic.csv")
#demo.network <- read.csv("demographics_network.csv")

#demo <- rbind(demo.psyarxiv,demo.network)
demo.df <- data.frame(record_id = demo.pandemic$ï..Study.id,
                      period = demo.pandemic$Period, 
                      Age = demo.pandemic$Age,
                      gender = demo.pandemic$Gender.1m2f,
                      race = demo.pandemic$Race,
                      income = demo.pandemic$Income)

demo.df$period <- factor(demo.df$period, levels = c("Pre-lockdown",
                                                    "Lockdown",
                                                    "Reopening"))

demo.df.pre <- demo.df[which(demo.df$period == "Pre-lockdown"),]
demo.df.lock <- demo.df[which(demo.df$period == "Lockdown"),]
demo.df.reopen <- demo.df[which(demo.df$period == "Reopening"),]


## GENDER
# Data prep
demo.df.pre$genderLabel <- ifelse(demo.df.pre$gender == 1, "Male",
ifelse(demo.df.pre$gender == 2, "Female",
       ifelse(demo.df.pre$gender == "Other","Other","")))
demo.df.lock$genderLabel <- ifelse(demo.df.lock$gender == 1, "Male",
                                   ifelse(demo.df.lock$gender == 2, "Female",
                                          ifelse(demo.df.lock$gender == "Other","Other","")))
demo.df.reopen$genderLabel <- ifelse(demo.df.reopen$gender == 1, "Male",
                                     ifelse(demo.df.reopen$gender == 2, "Female",
                                            ifelse(demo.df.reopen$gender == "Other","Other","")))


demo.df$genderLabel <- ifelse(demo.df$gender == 1, "Male",
                              ifelse(demo.df$gender == 2, "Female",
                                     ifelse(demo.df$gender == "Other","Other","")))

demo.df.other <- demo.df[which(demo.df$genderLabel == "Other"),]

demo.df <- demo.df[-c(as.numeric(rownames(demo.df.other))),]


# Statistics
demo.df.prop <- ddply(demo.df, .(period), summarise,
                      prop = table(gender),
                      gender = names(table(genderLabel)))

demo.df.prop$gender <- factor(demo.df.prop$gender, levels = c("Male", "Female"))

G1_M = 108
G1_F = 94
G2_M = 138
G2_F = 87
G3_M = 101
G3_F = 70

genderG = data.frame(G1 = c(G1_M,G1_F),
                     G2 = c(G2_M, G2_F),
                     G3 = c(G3_M, G3_F))

rownames(genderG) <- c("Male","Female")
colnames(genderG) <- c("Pre-lockdown","Lockdown","Reopening")

chisq.test(genderG, correct = F)

# Plot
PieChart(genderLabel, data = demo.df.pre, fill = c("hotpink","lightblue","#B19CD9"),
         values_size = 2, main = "",labels_cex = 0)

PieChart(genderLabel, data = demo.df.lock, fill = c("hotpink","lightblue","#B19CD9"),
         values_size = 2, main = "",labels_cex = 0)

PieChart(genderLabel, data = demo.df.reopen, fill = c("hotpink","lightblue","#B19CD9"),
         values_size = 2, main = "",labels_cex = 0)

########################################
########################################
########################################


## AGE
# Date prep
# remove NA
missing.age.pre <- is.na(demo.df.pre$Age)
missing.age.lock <- is.na(demo.df.lock$Age)
missing.age.reopen <- is.na(demo.df.reopen$Age)

df.pre <- demo.df.pre[!missing.age.pre,]
df.lock <- demo.df.lock[!missing.age.lock,]
df.reopen <- demo.df.reopen[!missing.age.reopen,]

# name age group
labs <- c(paste(seq(0,95,by=5), seq(0+5-1,100-1,by=5),
                sep = "-"), paste(100,"+",sep = ""))
labs

df.pre$AgeGroup <- cut(df.pre$Age, breaks = c(seq(0,100,by=5),Inf),labels = labs,right = FALSE)
df.lock$AgeGroup <- cut(df.lock$Age, breaks = c(seq(0,100,by=5),Inf),labels = labs,right = FALSE)
df.reopen$AgeGroup <- cut(df.reopen$Age, breaks = c(seq(0,100,by=5),Inf),labels = labs,right = FALSE)

#head(df[c("Age","AgeGroup")],15)

df <- rbind(df.pre,df.lock,df.reopen)

# Statistics
mean(df.pre$Age)
mean(df.lock$Age)
mean(df.reopen$Age)

# Plot
stats <- aggregate(Age~period, df, function(x) c(mean=mean(x), sd=sd(x)))
stats <- data.frame(period=stats[,1],stats[,2])
stats


ggplot(df, aes(x=Age, fill=period)) +
  #geom_histogram(binwidth = .5, colour = "blue", fill = "white") +
  geom_histogram(aes(y=..density..), color="grey30")+ 
  with(stats[stats$period=="Pre-lockdown" ,],stat_function(data=df[df$period=="Pre-lockdown",],fun=dnorm, args=list(mean=mean, sd=sd), color="black",size=2))+
  with(stats[stats$period=="Lockdown" ,],stat_function(data=df[df$period=="Lockdown",],fun=dnorm, args=list(mean=mean, sd=sd), color="black",size=2))+
  with(stats[stats$period=="Reopening" ,],stat_function(data=df[df$period=="Reopening",],fun=dnorm, args=list(mean=mean, sd=sd), color="black",size=2))+
  #with(stats.merge[stats.merge$period=="Lockdown",],stat_function(data=df[df$period=="Lockdown",],fun=dnorm, args=list(mean=mean, sd=sd)))+
  #with(stats.merge[stats.merge$period=="Lockdown",],stat_function(fun=dnorm, n=177, args=list(mean=38.989, sd=2.187))) +
  #with(stats.merge[stats.merge$period=="Reopening",],stat_function(data=df[df$period=="Reopening",],fun=dnorm, args=list(mean=mean, sd=sd)))+
  #with(stats.merge[stats.merge$period=="Reopening",],stat_function(fun=dnorm, n=468, args=list(mean=38.344, sd=2.916))) +
  facet_grid(period~.)+ geom_vline(data = stats, aes(xintercept=mean), linetype = "dashed",
                                   colour = "red4") +
  scale_fill_manual(name="Pandemic period",
                    values = c("#ffaf7b","#d76d77","#7d4a95")) +
  labs(x="",y="") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.line = element_line( colour = "black"),
        legend.position = "none",
        legend.box = "vertical",
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        #strip.background = element_rect(fill="blue"),
        strip.text.y = element_blank()
        #strip.background =element_rect(fill="grey")
        #aspect.ratio = 1
  )






########################################
########################################
########################################

## RACE
# Data prep
demo.df$race.name <- ifelse(demo.df$race == 0, "Other",
                            ifelse(demo.df$race == 1, "White",
                                   ifelse(demo.df$race == 2, "Black",
                                          ifelse(demo.df$race == 3, "Asian",
                                                 ifelse(demo.df$race == 4, "American Indian",
                                                        ifelse(demo.df$race == 5, "Multiracial",""))))))

demo.df$race.name <- factor(demo.df$race.name, levels = c("Other","White","Black","Asian","American Indian","Multiracial"))


# Statistics
demo.df.prop <- ddply(demo.df, .(period), summarise,
                      prop = table(race),
                      race = names(table(race.name)))

G1_O = 4
G1_W = 161
G1_B = 19
G1_A = 8
G1_Am = 2
G1_M = 8

G2_O = 6
G2_W = 186
G2_B = 25
G2_A = 9
G2_Am = 1
G2_M = 4

G3_O = 3
G3_W = 137
G3_B = 15
G3_A = 7
G3_Am = 5
G3_M = 5

raceG = data.frame(G1 = c(G1_O,G1_W,G1_B,G1_A,G1_Am,G1_M),
                   G2 = c(G2_O,G2_W,G2_B,G2_A,G2_Am,G2_M),
                   G3 = c(G3_O,G3_W,G3_B,G3_A,G3_Am,G3_M))

rownames(raceG) <- c("Other","White","Black","Asian","American Indian","Multiracial")
colnames(raceG) <- c("Pre-lockdown","Lockdown","Reopening")

chisq.test(raceG, correct = F)

# Plot
demo.df.perc <- ddply(demo.df, .(period), summarise,
                      percent = prop.table(table(race.name))*100,
                      race = names(table(race.name)))

demo.df.perc$period <- factor(demo.df.perc$period, levels = c("Reopening","Lockdown","Pre-lockdown"))

ggplot(demo.df.perc, aes(x = period, y = percent)) +
  geom_col(aes(fill = race), width = 0.4) + coord_flip() +  scale_fill_manual("",values = c("#9897A8","#E9B606","#E97E06","#069010","#082465","#5595D4")#, 
                                                                              #labels = c("Pre-lockdown",
                                                                              #           "Lockdown",
                                                                              #           "Reopening")
  ) +
  labs(x="",y="") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.line = element_line( colour = "black"),
        #legend.position = "none",
        legend.box = "vertical",
        #axis.text.x=element_blank(),
        #axis.text.y = element_blank(),
        aspect.ratio = 1)


########################################
########################################
########################################

## INCOME
# Data prep
demo.df$income.name <- ifelse(demo.df$income == 1, "Less than $20,000",
                              ifelse(demo.df$income == 2, "$20,000 to $34,999",
                                     ifelse(demo.df$income == 3, "$35,000 to $49,999",
                                            ifelse(demo.df$income == 4, "$50,000 to $74,999",
                                                   ifelse(demo.df$income == 5, "$75,000 to $99,999",
                                                          ifelse(demo.df$income == 6, "Over $100,000",""))))))

demo.df$income.name <- factor(demo.df$income.name, levels = c("Less than $20,000","$20,000 to $34,999","$35,000 to $49,999",
                                                              "$50,000 to $74,999","$75,000 to $99,999","Over $100,000"))

# Statistics
demo.df.prop <- ddply(demo.df, .(period), summarise,
                      prop = table(income),
                      income = names(table(income.name)))

G1_1 = 29
G1_2 = 56
G1_3 = 31
G1_4 = 47
G1_5 = 23
G1_6 = 12

G2_1 = 39
G2_2 = 55
G2_3 = 42
G2_4 = 57
G2_5 = 22
G2_6 = 14

G3_1 = 22
G3_2 = 33
G3_3 = 41
G3_4 = 47
G3_5 = 17
G3_6 = 10

incomeG = data.frame(G1 = c(G1_1,G1_2,G1_3,G1_4,G1_5,G1_6),
                     G2 = c(G2_1,G2_2,G2_3,G2_4,G2_5,G2_6),
                     G3 = c(G3_1,G3_2,G3_3,G3_4,G3_5,G3_6))

rownames(incomeG) <- c("Less than $20,000","$20,000 to $34,999","$35,000 to $49,999","$50,000 to $74,999","$75,000 to $99,999","Over $100,000")
colnames(incomeG) <- c("Pre-lockdown","Lockdown","Reopening")

chisq.test(incomeG, correct = F)

# Plot
demo.df.Inc.perc <- ddply(demo.df, .(period), summarise,
                          percent = prop.table(table(income.name))*100,
                          income = names(table(income.name)))

demo.df.Inc.perc$income <- factor(demo.df.Inc.perc$income, levels = c("Less than $20,000",
                                                                      "$20,000 to $34,999",
                                                                      "$35,000 to $49,999",
                                                                      "$50,000 to $74,999",
                                                                      "$75,000 to $99,999",
                                                                      "Over $100,000"))



ggplot(demo.df.Inc.perc, aes(x = period, y = percent, fill=income)) +
  geom_bar(position="dodge", stat="identity", width = 0.4) + scale_fill_manual("",values = c("#605F6B","#343549","#864E3D","#D8A460","#CA6A36","#99170F")#, 
                                                                               #labels = c("Pre-lockdown",
                                                                               #           "Lockdown",
                                                                               #           "Reopening")
  ) +
  labs(x="",y="") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = 1),
        axis.line = element_line( colour = "black"),
        #legend.position = "none",
        legend.box = "vertical",
        #axis.text.x=element_blank(),
        #axis.text.y = element_blank(),
        aspect.ratio = 1)
