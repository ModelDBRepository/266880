
setwd("C:/Users/prave/Desktop/tables")


#install.packages("usmap")
library(usmap)
library(ggplot2)
library(dplyr)

mask <- read.csv("ustates.csv")

mask$mwr <- as.factor(mask$mwr)

plot_usmap(data = mask, values = "mwr", color="black", labels = TRUE, label_color = "black") + 
  scale_fill_manual(values = c('0' = "#FFA500", '1' = "#008000"), name = "Mask Policy",
                    labels = c("Recommended","Mandated")) + 
  #ggtitle("Mask-wearing requirement by state") +
  theme(#panel.background = element_rect(fill = "grey"),
        #plot.background = element_rect(fill = "grey"),
        #panel.border = element_blank(),
        #plot.title = element_text(
        #  size = 15, hjust = 0.5, family = "Times"),
        #legend.background = element_rect(fill = "grey"),
        legend.position = "none")



