library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)

#change this to desired directory, likely downloads
#setwd("Downloads/ToxPi-Weight-Optimization")

################# plot MAE Diff distributions ####################################
#load in data for consistent convergence limit, number of response levels, and number of samples
load("simulation_studies/MAEData/dists500Converge3Bins500SamplesV2.rdata")

#prep difference dataframe for ordinal and GA performance
diffData <- data.frame(matrix(, ncol = 8))
colnames(diffData) <- colnames(allDist)

##### get difference values from loaded in data
###every 2000 indices consists of a common parameter
#the first 1000 are ordinal results
#the second 1000 are ga results that, in order, coincide to the ordinal results
#subtract all ga results from ordinal results 
for(i in seq(1,(nrow(allDist)-1000),2000)){ #iterate through each parameter combination
  df1 <- allDist[c(i:(i + 999)),] #get ordinal results
  df2 <- allDist[c((i + 1000):(i + 1999)),] #get ga results
  df2[,'Errors'] <- df1[,'Errors'] - df2[,'Errors'] #subtract results
  diffData <- rbind(diffData,df2) #append all differences to df with same parameter combination
}

#remove first row containing NA
diffData <- diffData[-1,]

#####Plot 1: Varying # slices
#get consistent known ratio
diffData_6Ratio <- diffData[diffData$KnownRatio==6,]

#get Error differences that are not equal to 0
diffData_6Ratio <- diffData_6Ratio[diffData_6Ratio$Errors!=0,]

#plot error differences when a change occurred for varying number of slices
p1 <- ggplot(diffData_6Ratio, aes(Errors, factor(NoSlices), colour = factor(NoSlices), fill = factor(NoSlices))) + 
  geom_violin(size = 1, alpha = .3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5), legend.position = "none", legend.direction = 'vertical') + 
  xlim(-.12, .12) +
  geom_vline(xintercept = 0) +
  labs(title = paste("A: Varying # Slices"), fill = "A: # of Slices", color = "A: # of Slices", x = "Difference in Error", y = "# of Slices")

#annotate plot with the percentage of times a change occurred for each slice #, separated by improvement or worsening
for(i in 1:5){ #i ~ slice # / 3
  #worsening annotation
  p1 <- p1 + annotate("text", x = -.12, y = i, label =paste0(round(nrow(diffData_6Ratio[diffData_6Ratio$NoSlices==i*3 & diffData_6Ratio$Errors<0,])/4000*100, 2), "%"))
  #improvement annotation
  p1 <- p1 + annotate("text", x = .12, y = i, label =paste0(round(nrow(diffData_6Ratio[diffData_6Ratio$NoSlices==i*3 & diffData_6Ratio$Errors>0,])/4000*100, 2), "%"))
}
#annotate plot with labels for decrease and increase
p1 <- p1 + annotate("text", x = -.105, y = 5.45, label = "Percent Decreased")
p1 <- p1 + annotate("text", x = .105, y = 5.45, label = "Percent Increased")


#####Plot 2: Varying known ratios
#get consistent number of slices
diffData_6Slices <- diffData[diffData$NoSlices==6,]

#get Error differences that are not equal to 0
diffData_6Slices <- diffData_6Slices[diffData_6Slices$Errors!=0,]

#plot error differences when a change occurred for varying ratios of knowns
p2 <- ggplot(diffData_6Slices, aes(Errors, factor(KnownRatio), colour = factor(KnownRatio),fill = factor(KnownRatio))) + 
  geom_violin(size = 1, alpha = .3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = .5), legend.position = "none", legend.direction = 'vertical') + 
  xlim(-.12, .12) +
  geom_vline(xintercept =  0) +
  labs(title = paste("B: Varying # Knowns"), fill = "B: # of Known\nSamples/# of Slices", color = "B: # of Known\nSamples/# of Slices", x = "Difference in Error", y = "Known Samples/\n# of Slices")

#annotate plot with the percentage of times a change occurred for each ratio, separated by improvement or worsening
for(i in 1:5){ #i ~ ratio / 3
  #worsening annotation
  p2 <- p2 + annotate("text", x = -.12, y = i, label =paste0(round(nrow(diffData_6Slices[diffData_6Slices$KnownRatio==i*3 & diffData_6Slices$Errors<0,])/4000*100,2), "%"))
  #improvement annotation
  p2 <- p2 + annotate("text", x = .12, y = i, label =paste0(round(nrow(diffData_6Slices[diffData_6Slices$KnownRatio==i*3 & diffData_6Slices$Errors>0,])/4000*100,2), "%"))
}
#annotate plot with labels for decrease and increase
p2 <- p2 + annotate("text", x = -.105, y = 5.5, label = "Percent Decreased")
p2 <- p2 + annotate("text", x = .105, y = 5.5, label = "Percent Increased")

#####Plot 3: Varying # samples
#load in and combine varying sample data for consistent converge and response levels
allDists_500Samples <- allDist

load("simulation_studies/MAEData/dists500Converge3Bins1000SamplesV2.rdata")
allDists_1000Samples <- allDist

load("simulation_studies/MAEData/dists500Converge3Bins5000SamplesV2.rdata")
allDists_5000Samples <- allDist

load("simulation_studies/MAEData/dists500Converge3Bins10000SamplesV2.rdata")
allDists_10000Samples <- allDist

allDist <- rbind(allDists_500Samples, allDists_1000Samples, allDists_5000Samples, allDists_10000Samples)

#get consistent number of slices and known ratios
allDists_6Ratio6Slices <- allDist[allDist$NoSlices == 6,]
allDists_6Ratio6Slices <- allDists_6Ratio6Slices[allDists_6Ratio6Slices$KnownRatio == 6,]

#prep difference dataframe for ordinal and GA performance
diffData <- data.frame(matrix(, ncol = 8))
colnames(diffData) <- colnames(allDist)

##### get difference values from loaded in data
###every 2000 indices consists of a common parameter
#the first 1000 are ordinal results
#the second 1000 are ga results that, in order, coincide to the ordinal results
#subtract all ga results from ordinal results 
for(i in seq(1,(nrow(allDists_6Ratio6Slices)-1000),2000)){
  df1 <- allDists_6Ratio6Slices[c(i:(i + 999)),]
  df2 <- allDists_6Ratio6Slices[c((i + 1000):(i + 1999)),]
  df2[,'Errors'] <- df1[,'Errors'] - df2[,'Errors']
  diffData <- rbind(diffData,df2)
}

#remove first row containing NA
diffData <- diffData[-1,]

#get Error differences that are not equal to 0
diffData <- diffData[diffData$Errors!=0,]

#get # of known samples as a % of total number of samples
#diffData$Percent <- diffData$KnownRatio*diffData$NoSlices/diffData$NoSamples*100

#plot error differences when a change occurred for varying number of samples
p3 <- ggplot(diffData, aes(Errors, factor(NoSamples), colour = factor(NoSamples), fill = factor(NoSamples))) + 
  theme_bw() +
  geom_violin(size = 1, alpha = .3) +
  theme(plot.title = element_text(hjust = .5), legend.position = "none", legend.direction = 'vertical') + 
  xlim(-.12, .12) +
  geom_vline(xintercept =  0) +
  labs(title = paste("C: Varying # Total Samples"), fill = "C: # of Total Samples", color = "C: # of Total Samples", x = "Difference in Error", y = "# of Total Samples")

#annotate plot with the percentage of times a change occurred for each sample #, separated by improvement or worsening
count <- 1 #count denotes height of graph to place percentages on
for(i in c(500,1000,5000,10000)){ #i ~ NoSamples
  #worsening annotation
  p3 <- p3 + annotate("text", x = -.12, y = count, label =paste0(round(nrow(diffData[diffData$NoSamples==i & diffData$Errors<0,])/4000*100,2), "%"))
  #improving annotation
  p3 <- p3 + annotate("text", x = .12, y = count, label =paste0(round(nrow(diffData[diffData$NoSamples==i & diffData$Errors>0,])/4000*100,2), "%"))
  #increment height of annotation
  count <- count + 1
}
#annotate plot with labels for decrease and increase
p3 <- p3 + annotate("text", x = -.105, y = 4.5, label = "Percent Decreased")
p3 <- p3 + annotate("text", x = .105, y = 4.5, label = "Percent Increased")

#####Plot 4: Varying # response levels
#load in and combine varying response level data for consistent converge and number of samples
load("simulation_studies/MAEData/dists500Converge2Bins500SamplesV2.rdata")
allDists_2Bins <- allDist

load("simulation_studies/MAEData/dists500Converge3Bins500SamplesV2.rdata")
allDists_3Bins <- allDist

load("simulation_studies/MAEData/dists500Converge4Bins500SamplesV2.rdata")
allDists_4Bins <- allDist

allDist <- rbind(allDists_2Bins, allDists_3Bins, allDists_4Bins)

#get consistent number of slices and known ratios
allDists_6Ratio6Slices <- allDist[allDist$NoSlices == 6,]
allDists_6Ratio6Slices <- allDists_6Ratio6Slices[allDists_6Ratio6Slices$KnownRatio == 6,]

#prep difference dataframe for ordinal and GA performance
diffData <- data.frame(matrix(, ncol = 8))
colnames(diffData) <- colnames(allDist)

##### get difference values from loaded in data
###every 2000 indices consists of a common parameter
#the first 1000 are ordinal results
#the second 1000 are ga results that, in order, coincide to the ordinal results
#subtract all ga results from ordinal results
for(i in seq(1,(nrow(allDists_6Ratio6Slices)-1000),2000)){
  df1 <- allDists_6Ratio6Slices[c(i:(i + 999)),]
  df2 <- allDists_6Ratio6Slices[c((i + 1000):(i + 1999)),]
  df2[,'Errors'] <- df1[,'Errors'] - df2[,'Errors']
  diffData <- rbind(diffData,df2)
}

#remove first row containing NA
diffData <- diffData[-1,]

#get Error differences that are not equal to 0
diffData <- diffData[diffData$Errors!=0,]

#plot error differences when a change occurred for varying response levels
p4 <- ggplot(diffData, aes(Errors, factor(NoBins), colour = factor(NoBins), fill = factor(NoBins))) + 
  theme_bw() +
  geom_violin(size = 1, alpha = .3) +
  theme(plot.title = element_text(hjust = .5), legend.position = "none", legend.direction = 'vertical') + 
  xlim(-.12, .12) +
  geom_vline(xintercept =  0) +
  labs(title = paste("D: Varying # Response Levels"), fill = "D: # Response Levels", color = "D: # Response Levels", x = "Difference in Error", y = "# of Response Levels")
#annotate plot with the percentage of times a change occurred for each sample #, separated by improvement or worsening
count <- 1 #count denotes height of graph to place percentages on
for(i in 2:4){ #i ~ # of response levels
  #worsening annotation
  p4 <- p4 + annotate("text", x = -.12, y = count, label =paste0(round(nrow(diffData[diffData$NoBins==i & diffData$Errors<0,])/4000*100,2), "%"))
  #improving annotation
  p4 <- p4 + annotate("text", x = .12, y = count, label =paste0(round(nrow(diffData[diffData$NoBins==i & diffData$Errors>0,])/4000*100,2), "%"))
  #increment height of annotation
  count <- count + 1
}
#annotate plot with labels for decrease and increase
p4 <- p4 + annotate("text", x = -.105, y = 3.5, label = "Percent Decreased")
p4 <- p4 + annotate("text", x = .105, y = 3.5, label = "Percent Increased")

#####Plot 4: Varying # response levels
#load in varying distribution data for consistent converge, response level, and number of samples
load("simulation_studies/MAEData/dists500Converge3Bins500SamplesV2.rdata")

#get consistent number of slices and known ratios
allDists_6Ratio6Slices <- allDist[allDist$NoSlices == 6,]
allDists_6Ratio6Slices <- allDists_6Ratio6Slices[allDists_6Ratio6Slices$KnownRatio == 6,]

#prep difference dataframe for ordinal and GA performance
diffData <- data.frame(matrix(, ncol = 8))
colnames(diffData) <- colnames(allDist)

##### get difference values from loaded in data
###every 2000 indices consists of a common parameter
#the first 1000 are ordinal results
#the second 1000 are ga results that, in order, coincide to the ordinal results
#subtract all ga results from ordinal results
for(i in seq(1,(nrow(allDists_6Ratio6Slices)-1000),2000)){
  df1 <- allDists_6Ratio6Slices[c(i:(i + 999)),]
  df2 <- allDists_6Ratio6Slices[c((i + 1000):(i + 1999)),]
  df2[,'Errors'] <- df1[,'Errors'] - df2[,'Errors']
  diffData <- rbind(diffData,df2)
}

#remove first row containing NA
diffData <- diffData[-1,]

#get Error differences that are not equal to 0
diffData <- diffData[diffData$Errors!=0,]

#plot error differences when a change occurred for varying distribution
p5 <- ggplot(diffData, aes(Errors, factor(Distribution), colour = factor(Distribution), fill = factor(Distribution))) + 
  theme_bw() +
  geom_violin(size = 1, alpha = .3) +
  theme(plot.title = element_text(hjust = .5), legend.position = "none", legend.direction = 'vertical') + 
  xlim(-.12, .12) +
  geom_vline(xintercept = 0) +
  labs(title = paste("E: Varying Data Distributions"), fill = "E: Distribution", color = "E: Distribution", x = "Difference in Error", y = "Data Distribution") +
  scale_y_discrete(labels = c("Normal", "Gamma", "Uniform", "Mixed")) + 
  scale_fill_discrete(labels = c("Normal", "Gamma", "Uniform", "Mixed")) + 
  scale_color_discrete(labels = c("Normal", "Gamma", "Uniform", "Mixed"))
#annotate plot with the percentage of times a change occurred for each distribution, separated by improvement or worsening
for(i in 1:4){ #i ~ distribution placeholder
  #worsening annotation
  p5 <- p5 + annotate("text", x = -.12, y = i, label =paste0(round(nrow(diffData[diffData$Distribution==i & diffData$Errors<0,])/1000*100,2), "%"))
  #improving annotation
  p5 <- p5 + annotate("text", x = .12, y = i, label =paste0(round(nrow(diffData[diffData$Distribution==i & diffData$Errors>0,])/1000*100,2), "%"))
}
#annotate plot with labels for decrease and increase
p5 <- p5 + annotate("text", x = -.105, y = 4.5, label = "Percent Decreased")
p5 <- p5 + annotate("text", x = .105, y = 4.5, label = "Percent Increased")

#get legends for each plot
g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs
l1 <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
g <- ggplotGrob(p2 + theme(legend.position="bottom"))$grobs
l2 <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
g <- ggplotGrob(p3 + theme(legend.position="bottom"))$grobs
l3 <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
g <- ggplotGrob(p4 + theme(legend.position="bottom"))$grobs
l4 <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
g <- ggplotGrob(p5 + theme(legend.position="bottom"))$grobs
l5 <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

#arrange plots and legends
grid.arrange(
  do.call("arrangeGrob", c(list(p1,p2,p3,p4,p5,do.call("arrangeGrob", c(list(l1,l2,l3,l4,l5),nrow = 1, ncol = 5)),nrow = 3, ncol = 2))))

  