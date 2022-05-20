# Corinne Klohmann 
# cak268@uw.edu

# clear workspace
rm(list = ls())

# read in the data 
data <- read.csv("Vertical Sampling - Sheet1.csv")

# create bar chart 
library(tidyverse)
# bar chart with error bars
data_summary <- data %>% # the names of the new data frame and the data frame to be summarised
  group_by(shallow.deep) %>% # the grouping variable
  summarise(
    mean_ent = mean(avgent), # calculates the mean of each group
    sd_ent = sd(avgent), # calculates the standard deviation of each group
    n_ent = n(), # calculates the sample size per group
    SE_ent = sd(avgent) / sqrt(n())
  ) # calculates the standard error of each group
name <- c("Floor", "Surface")
#plot data
EntPlot <- ggplot(data_summary, aes(shallow.deep, mean_ent)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_ent - sd_ent, ymax = mean_ent + sd_ent), width = 0.2)

EntPlot + labs(y = "Enterococcus abundance (CFU/100ml)", x = "Depth") + 
  theme_classic() + ggtitle("Enterococcus Counts by Depth") + scale_x_discrete(labels= name) +
  theme(plot.title = element_text(hjust = 0.5))

  



#bar chart withouth error bars 
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

