# Corinne Klohmann 
# cak268@uw.edu

# clear workspace
rm(list = ls())

# read in the data 
data <- read.csv("Vertical Sampling - Sheet1.csv")

# create bar chart \
library(tidyverse)
count <- data %>%
  group_by(shallow.deep) %>%
  summarize(CFUs = mean(avgent), #Fv.Fm is the column with the data in it
            sd = sd(avgent))
name <- c("Floor", "Surface")
barplot(count$CFUs, main="Enterococcus counts by Depth",
        xlab="Depth", ylab = "CFU/100ml", names.arg = name)

#create seperate vectors
shallow <- data$avgent[which(data$shallow.deep == "s")]
deep <- data$avgent[which(data$shallow.deep == "d")]
# run stats
test <- t.test(shallow, deep)
# p-value = .11

# boxplot 
# box plot with eelgrass, control, and ribbon \
boxplot(avgent~shallow.deep,
        data=data,
        main="Enterococcus counts by depth",
        xlab="Depth",
        ylab="CFUs/100ml",
        col="blue",
        border="brown"
)

