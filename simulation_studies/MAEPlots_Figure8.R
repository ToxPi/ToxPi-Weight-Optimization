library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)

#set desired directory, likely downloads
#setwd("Downloads/ToxPi-Weight-Optimization")

################# plot MAE distributions ####################################
#load in changing ratio and changing # slices data
load("simulation_studies/MAEData/dists500Converge3Bins500SamplesV2.rdata")

#get consistent known ratio
allDists_6Ratio <- allDist[allDist$KnownRatio==6,]

#plot MAE for changing number of slices
p1 <- ggplot(allDists_6Ratio, aes(factor(NoSlices), Errors, fill = factor(Method), dodge = Method)) + 
      stat_boxplot(geom='errorbar') + geom_boxplot() + theme_bw() + 
      theme(legend.position = "None", axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5)) + 
      ylim(0,.15) + labs(title = 'A: Varying # Slices', x = '# of Slices', fill = 'Method') +
      scale_fill_discrete(labels = c("Ordinal", "GA")) 
   
#get consistent number of slices
allDists_6Slices <- allDist[allDist$NoSlices==6,]

#plot MAE for changing known ratio
p2 <- ggplot(allDists_6Slices, aes(factor(KnownRatio), Errors, fill = factor(Method), dodge = Method)) + 
      stat_boxplot(geom='errorbar') + geom_boxplot() + theme_bw() + 
      theme(legend.position = "None", axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5)) + 
      ylim(0,.15) + labs(title = 'B: Varying # Knowns', x = 'Known Samples/# of Slices', fill = 'Method') +
      scale_fill_discrete(labels = c("Ordinal", "GA")) 


#load in and combine data for changing number of samples
allDists_500Samples <- allDist

load("simulation_studies/MAEData/dists500Converge3Bins1000SamplesV2.rdata")
allDists_1000Samples <- allDist

load("simulation_studies/MAEData/dists500Converge3Bins5000SamplesV2.rdata")
allDists_5000Samples <- allDist

load("simulation_studies/MAEData/dists500Converge3Bins10000SamplesV2.rdata")
allDists_10000Samples <- allDist

allDist <- rbind(allDists_500Samples, allDists_1000Samples, allDists_5000Samples, allDists_10000Samples)

#get consistent number of slices and known ratio
allDists_6Ratio6Slices <- allDist[allDist$NoSlices == 6,]
allDists_6Ratio6Slices <- allDists_6Ratio6Slices[allDists_6Ratio6Slices$KnownRatio == 6,]

#plot MAE for changing number of samples
p3 <- ggplot(allDists_6Ratio6Slices, aes(factor(NoSamples), Errors, fill = factor(Method), dodge = Method)) + 
      stat_boxplot(geom='errorbar') + geom_boxplot() + theme_bw() + 
      theme(legend.position = "None", axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5)) + 
      ylim(0,.15) + labs(title = 'C: Varying # Total Samples', x = '# of Samples', fill = 'Method') +
      scale_fill_discrete(labels = c("Ordinal", "GA")) 


#load in and combine data for varying response levels
load("simulation_studies/MAEData/dists500Converge2Bins500SamplesV2.rdata")
allDists_2Bins <- allDist

load("simulation_studies/MAEData/dists500Converge3Bins500SamplesV2.rdata")
allDists_3Bins <- allDist

load("simulation_studies/MAEData/dists500Converge4Bins500SamplesV2.rdata")
allDists_4Bins <- allDist

allDist <- rbind(allDists_2Bins, allDists_3Bins, allDists_4Bins)

#get consistent number of slices and known ratio
allDists_6Ratio6Slices <- allDist[allDist$NoSlices == 6,]
allDists_6Ratio6Slices <- allDists_6Ratio6Slices[allDists_6Ratio6Slices$KnownRatio == 6,]

#plot MAE for changing number of response levels
p4 <- ggplot(allDists_6Ratio6Slices, aes(factor(NoBins), Errors, fill = factor(Method), dodge = Method)) + 
      stat_boxplot(geom='errorbar') + geom_boxplot() + theme_bw() + 
      theme(legend.position = "None", axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5)) + 
      ylim(0,.15) + labs(title = 'D: Varying # Response Levels', x = '# of Bins', fill = 'Method') +
      scale_fill_discrete(labels = c("Ordinal", "GA")) 

#load in data for varying distributions
load("simulation_studies/MAEData/dists500Converge3Bins500SamplesV2.rdata")

#get consistent number of slices and known ratio
allDists_6Ratio6Slices <- allDist[allDist$NoSlices == 6,]
allDists_6Ratio6Slices <- allDists_6Ratio6Slices[allDists_6Ratio6Slices$KnownRatio == 6,]

#plot MAE for the different data distributions
p5 <- ggplot(allDists_6Ratio6Slices, aes(factor(Distribution), Errors, fill = factor(Method), dodge = Method)) + 
  stat_boxplot(geom='errorbar') + geom_boxplot() + theme_bw() + 
  theme(legend.position = "None", axis.title.y=element_blank(),plot.title = element_text(hjust = 0.5)) + 
  ylim(0,.15) + labs(title = 'E: Varying Data Distributions', x = 'Data Distribution', fill = 'Method') +
  scale_fill_discrete(labels = c("Ordinal", "GA")) +
  scale_x_discrete(labels = c("Normal", "Gamma", "Uniform", "Mixed"))

#get common legend
g <- ggplotGrob(p1 + theme(legend.position="bottom"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)

#arrange plots
grid.arrange(
  do.call("arrangeGrob", c(list(p1,p2,p3,p4,p5,legend), nrow = 2, ncol = 3)), left = 'Error')
