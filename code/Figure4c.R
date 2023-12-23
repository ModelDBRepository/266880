rm(list=ls())

# set working directory
setwd("C:/Pandemic_2020/modelDB/data")

# load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(plotrix)

# read data
pandemic <- read.csv('pandemic.csv')
dynata <- read.csv('dynata.csv')

pandemic.reopening <- pandemic[which(pandemic$Period == "Reopening"),]

june.masks.req.nine <- c("CA","NM","MI","IL","NY","MA","RI","MD","VA")
june.states.mask.rec <- c("AK","AL","AR","AZ","CO","CT","DC","FL","GA","HI","IA",
                          "ID","IN","KS","KY","LA","MN","MO","MS","MT","NC","ND",
                          "NE","NH","NJ","NV","OH","OK","OR","PA","SC","SD","TN",
                          "TX","UT","VT","WA","WI","WV","WY")

# subset only the data from the above 9 states
#pandemic.reopening <- pandemic.reopening[which(pandemic.reopening$State %in% june.states.mask.rec),]
#dynata <- dynata[which(dynata$STATE %in% june.states.mask.rec),]

# pandemic.reopening.req <- pandemic.reopening[which(pandemic.reopening$State %in% june.masks.req.nine),]
# pandemic.reopening.rec <- pandemic.reopening[which(pandemic.reopening$State %in% june.states.mask.rec),]

# pandemic.reopening.df <- pandemic.reopening %>%
#   group_by(State) %>%
#   summarise(paranoia.mean = mean(Paranoia.score),
#             tightness.mean = mean(Tightness.score),
#             mu2.mean = mean(mu02_1),
#             mu3.mean = mean(mu03_1))
# colnames(pandemic.reopening.df) <- c("state","paranoia.mean","tightness.mean","mu2.mean","mu3.mean")

dynata <- dynata[which(dynata$STATE %in% june.states.mask.rec),]
dynata.df <- data.frame(state = dynata$STATE,
                        mask.response = dynata$MASK,
                        value = dynata$RESPONDENTS)

dynata.df$mask.response <- ifelse(dynata.df$mask.response == "Always",5,
                                  ifelse(dynata.df$mask.response == "Frequently",4,
                                         ifelse(dynata.df$mask.response == "Sometimes",3,
                                                ifelse(dynata.df$mask.response == "Rarely",2,
                                                       ifelse(dynata.df$mask.response == "Not at all",1,"")))))

dynata.df$mask.response <- as.numeric(dynata.df$mask.response)

dynata.count <- dynata.df %>%
  group_by(state, mask.response) %>%
  summarise(count.respondents = sum(value, na.rm = TRUE))


dynata.sum <- dynata.count %>%
  group_by(state) %>%
  mutate(sum.count = sum(count.respondents, na.rm = TRUE))

dynata.relFreq <- dynata.sum %>%
  group_by(state) %>%
  mutate(rel.freq = count.respondents/sum.count)

dynata.fiveAlways <- dynata.relFreq[which(dynata.relFreq$mask.response == 5),]


colnames(dynata.fiveAlways) <- c("State","mask.response","count.respondents","sum.count","rel.freq")
join1 <- left_join(pandemic.reopening, dynata.fiveAlways)


join1$tightness.group <- ifelse(join1$Tightness.score <= median(join1$Tightness.score, na.rm = TRUE), "loose","tight")


join1.rec <- join1[which(join1$MWR == 0),]
join1.req <- join1[which(join1$MWR == 1),]


## Are mandated vs recommended states significantly different in mask-wearing belief depending on tightness of a state?
# In recommended states
t.test(join1.rec$rel.freq ~ join1.rec$tightness.group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

# In mandated states
t.test(join1.req$rel.freq ~ join1.req$tightness.group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)


