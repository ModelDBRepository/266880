#Empties Global Environment cache
rm(list = ls())

#Set working directory to current file location
setwd("C:/Pandemic_2020/analysis/data")

#Importing packages
library(tidyverse)
#install.packages("data.table")
library(data.table)
library(ggpubr)
library(plotrix)

#Read in data
# qualtrics.data <- read.csv("qualtrics_sept.csv", 
#                            stringsAsFactors = FALSE)
qualtrics.data <- read.csv("replication.csv", 
                           stringsAsFactors = FALSE)

#Stores "qualtrics.data" as a table data frame for easier reading
qualtrics.data <- tbl_df(qualtrics.data)

# remove unnecessary rows: 1 and 2 [extra column headers]
#qualtrics.data <- qualtrics.data[-c(1:2),]

# calculate paranoia score and group
rGPTS.data <- qualtrics.data[,c(1,18:35)] #959:994 #1048:1083
rGPTS.cleaned <- rGPTS.data[,c(2:9,10:19)]
rGPTS.cleaned.df <- as.data.frame(matrix(as.numeric(unlist(rGPTS.cleaned)), nrow = nrow(rGPTS.cleaned)))
# rename colnames [Reference vs Persecution]
colnames(rGPTS.cleaned.df) <- c("Ref.1","Ref.2","Ref.3","Ref.4","Ref.5","Ref.6","Ref.7","Ref.8",
                                "Per.1","Per.2","Per.3","Per.4","Per.5","Per.6","Per.7","Per.8","Per.9","Per.10")
rGPTS.cleaned.df$Total.Ref <- rowSums(rGPTS.cleaned.df[,1:8], na.rm = TRUE)
rGPTS.cleaned.df$Total.Per <- rowSums(rGPTS.cleaned.df[,9:18], na.rm = TRUE)
rGPTS.cleaned.df$Sum <- rowSums(rGPTS.cleaned.df[,1:18], na.rm = TRUE)

Total.Ans <- list()
for (i in 1:nrow(rGPTS.cleaned.df)){
  Total.Ans[[i]] <- length(which(rGPTS.cleaned.df[i,1:18] >= 0))
}
rGPTS.cleaned.df$Total.Ans <- unlist(Total.Ans)
rGPTS.cleaned.df$Score <- rGPTS.cleaned.df$Sum/rGPTS.cleaned.df$Total.Ans
rGPTS.cleaned.df$Group <- ifelse(rGPTS.cleaned.df$Total.Per > 10, "high","low")
rGPTS.cleaned.df$ID <- rGPTS.data$ID
rGPTS.cleaned.df <- rGPTS.cleaned.df[,c(25,1:24)]

#id.df <- data.frame(id = qualtrics.data$ID,
#                    group = rGPTS.cleaned.df$Group)
#id.df[which(id.df$group == "high"),]$id

# count of low and high paranoia
count.low <- length(which(rGPTS.cleaned.df$Group == "low"))
count.high <- length(which(rGPTS.cleaned.df$Group == "high"))

#Format all percents into a table
table <- data.frame(count.low, count.high)
#Transposes dataframe to verticle orientation
table <- t(table)
#Names each row of the data table
rownames(table) <- c("Low paranoid individuals",
                     "High paranoid individuals")
#Eliminates names of the table columns
colnames(table) <- "Count"
#Prints the table
table <- print(table, quote = FALSE)


# high.paranoia.id <- rGPTS.cleaned.df[which(rGPTS.cleaned.df$Group == "high"),]$ID
# high.paranoia.id

# pdi calculation
mwb.df <- data.frame(record_id = 1:nrow(rGPTS.cleaned.df),
                     gpts.score = rGPTS.cleaned.df$Score,
                     gpts.group = rGPTS.cleaned.df$Group,
                     qualtrics.data[,135:150]) #1260:1281

mwb.cleaned.df <- mwb.df[,-c(5,12)] #5,12:18 # may need to include 18:20, later

#Q9.2
mwb.cleaned.df[,4] <- ifelse(mwb.cleaned.df[,4] == "Yes",1,
                             ifelse(mwb.cleaned.df[,4] == "No",-1,""))
#Q9.4
mwb.cleaned.df[,5:10] <- ifelse(mwb.cleaned.df[,5:10] == "Extremely unlikely",-1,
                                ifelse(mwb.cleaned.df[,5:10] == "Unlikely",-0.5,
                                       ifelse(mwb.cleaned.df[,5:10] == "No plans to go",0,
                                              ifelse(mwb.cleaned.df[,5:10] == "Likely",0.5,
                                                     ifelse(mwb.cleaned.df[,5:10] == "Extremely likely",1,"")))))


#Q9.7
mwb.cleaned.df[,14] <- ifelse(mwb.cleaned.df[,14] == "More comfortable",1,
                              ifelse(mwb.cleaned.df[,14] == "Indifferent",0,
                                     ifelse(mwb.cleaned.df[,14] == "Less comfortable",-1,"")))

#Q9.8
mwb.cleaned.df[,15] <- ifelse(mwb.cleaned.df[,15] == "More comfortable",1,
                              ifelse(mwb.cleaned.df[,15] == "Indifferent",0,
                                     ifelse(mwb.cleaned.df[,15] == "Less comfortable",-1,"")))

#Q9.9
mwb.cleaned.df[,16] <- ifelse(mwb.cleaned.df[,16] == "Yes, a lot",1,
                              ifelse(mwb.cleaned.df[,16] == "Yes, some",0.5,
                                     ifelse(mwb.cleaned.df[,16] == "Not sure",0,
                                            ifelse(mwb.cleaned.df[,16] == "No, it does nothing",-0.5,
                                                   ifelse(mwb.cleaned.df[,16] == "No, it increases the spread",-1,"")))))

#Q9.10
mwb.cleaned.df[,17] <- ifelse(mwb.cleaned.df[,17] == "Yes",-1,
                              ifelse(mwb.cleaned.df[,17] == "Not sure",0,
                                     ifelse(mwb.cleaned.df[,17] == "No",1,"")))

mwb.scores.df <- as.data.frame(matrix(as.numeric(unlist(mwb.cleaned.df[,4:17])), nrow = nrow(mwb.df)))

#Q9.6
mwb.scores.df[,8:10] <- ifelse(mwb.scores.df[,8:10] >= 0 & mwb.scores.df[,8:10] < 25,-1,
                               ifelse(mwb.scores.df[,8:10] >=25 & mwb.scores.df[,8:10] < 50,-0.5,
                                      ifelse(mwb.scores.df[,8:10] == 50,0,
                                             ifelse(mwb.scores.df[,8:10] > 50 & mwb.scores.df[,8:10] <= 75,0.5,
                                                    ifelse(mwb.scores.df[,8:10] > 75 & mwb.scores.df[,8:10] <= 100,1,"")))))

mwb.scores.df <- as.data.frame(matrix(as.numeric(unlist(mwb.scores.df[,1:14])), nrow = nrow(mwb.df)))


mwb.cleaned.df$mean <- rowMeans(mwb.scores.df[,1:14], na.rm = TRUE)

mwb.cleaned.df$wsr.score <- rowMeans(cbind(as.numeric(qualtrics.data$WSR.block1),
                                           as.numeric(qualtrics.data$WSR.block2)),na.rm = TRUE)
mwb.cleaned.df$lsr.score <- rowMeans(cbind(as.numeric(qualtrics.data$LSR.block1),
                                           as.numeric(qualtrics.data$LSR.block2)),na.rm = TRUE)
mwb.cleaned.df$mu3.score <- rowMeans(cbind(as.numeric(qualtrics.data$mu03_1),
                                           as.numeric(qualtrics.data$mu03_2)),na.rm = TRUE)
mwb.cleaned.df$mu2.score <- rowMeans(cbind(as.numeric(qualtrics.data$mu02_1),
                                           as.numeric(qualtrics.data$mu02_2)),na.rm = TRUE)


mwb.cleaned.df$gpts.group.coded = ifelse(mwb.cleaned.df$gpts.group == "high",1,
                                         ifelse(mwb.cleaned.df$gpts.group == "low",0,""))


mwb.cleaned.df1 <- mwb.cleaned.df %>%
  group_by(gpts.group.coded) %>%
  summarise(mwb.mean = mean(mean, na.rm = TRUE),
            mwb.se = std.error(mean, na.rm = TRUE),
            wsr.mean = mean(wsr.score, na.rm = TRUE),
            wsr.se = std.error(wsr.score, na.rm = TRUE),
            mu3.mean = mean(mu3.score, na.rm = TRUE),
            mu3.se = std.error(mu3.score, na.rm = TRUE)) %>%
  mutate(gpts.group = c("low","high"))



# Mask attitude
t.test(mwb.cleaned.df$mean ~ mwb.cleaned.df$gpts.group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

p1 <- ggplot(mwb.cleaned.df1, aes(x=gpts.group.coded, y=mwb.mean, group=gpts.group.coded, fill=gpts.group)) +
  geom_bar(stat = "identity", width = .5) +
  geom_errorbar(aes(x=gpts.group.coded, ymin=mwb.mean-mwb.se, ymax=mwb.mean+mwb.se),
                width=0.4, colour="black", alpha=0.9, size=1.3) +
  geom_point(data=mwb.cleaned.df, aes(x=gpts.group.coded,y=mean),position = position_jitter(width = .15),
             shape=20, color="black", size=1)+
  scale_fill_manual("", values = c("#E74C3C","#FADBD8")) +
  labs(x="",
       y="",
       title = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 8))


###### WIN-SWITCH RATE
t.test(mwb.cleaned.df$wsr.score ~ mwb.cleaned.df$gpts.group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

p2 <- ggplot(mwb.cleaned.df1, aes(x=gpts.group.coded, y=wsr.mean, group=gpts.group.coded, fill=gpts.group)) +
  geom_bar(stat = "identity", width = .5) +
  geom_errorbar(aes(x=gpts.group.coded, ymin=wsr.mean-wsr.se, ymax=wsr.mean+wsr.se),
                width=0.4, colour="black", alpha=0.9, size=1.3) +
  geom_point(data=mwb.cleaned.df, aes(x=gpts.group.coded,y=wsr.score),position = position_jitter(width = .15),
             shape=20, color="black", size=1)+
  scale_fill_manual("", values = c("#E74C3C","#FADBD8")) +
  labs(x="",
       y="",
       title = "") +
  theme(plot.title = element_text(hjust = 0.5))


###### MU3
t.test(mwb.cleaned.df$mu3.score ~ mwb.cleaned.df$gpts.group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

p3 <- ggplot(mwb.cleaned.df1, aes(x=gpts.group.coded, y=mu3.mean, group=gpts.group.coded, fill=gpts.group)) +
  geom_bar(stat = "identity", width = .5) +
  geom_errorbar(aes(x=gpts.group.coded, ymin=mu3.mean-mu3.se, ymax=mu3.mean+mu3.se),
                width=0.4, colour="black", alpha=0.9, size=1.3) +
  geom_point(data=mwb.cleaned.df, aes(x=gpts.group.coded,y=mu3.score),position = position_jitter(width = .15),
             shape=20, color="black", size=1)+
  scale_fill_manual("", values = c("#E74C3C","#FADBD8")) +
  labs(x="",
       y="",
       title = "") +
  theme(plot.title = element_text(hjust = 0.5))

