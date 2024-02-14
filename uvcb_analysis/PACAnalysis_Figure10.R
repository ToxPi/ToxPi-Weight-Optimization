#load packages
library(gridExtra)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

#set working directory, likely change to downloads
#setwd("G:/Other computers/My Laptop/ReifResearch/Dissertation/OptiPi/Github/uvcb_analysis")

########## Prepare user data as would be used for GA ###########################
#load in true ranking results
toxpi_results <- read.csv("uvcb_analysis/qcpheno.out_results.csv")

#convert Source response to match paper
toxpi_results <- toxpi_results %>% mutate(Source = ifelse(Source %in% c("BITUMENS","BITUMEN","OXASPH"), "BIT",
                                         ifelse(Source == "gasoline", "NAPTHA",
                                                ifelse(Source == "Paraffinwax", "P.WAX",
                                                       ifelse(Source == "slackwax", "S.WAX",
                                                              ifelse(Source == "kerosine","KER",
                                                                     ifelse(Source == "petrolatum", "P.LAT",
                                                                            ifelse(Source == "VHGOa", "VHGO",
                                                                                   ifelse(Source == "dieselfuel", "D.FUEL", Source)))))))))

toxpi_results <- toxpi_results %>% mutate(Source = ifelse(Source %in% c("D.FUEL", "KER"), "KER",
                                       ifelse(Source %in% c("HRBO","OLBO"), "BO",
                                              ifelse(Source %in% c("Hwax","S.WAX","P.WAX"), "WAX", Source))))

#assign rank to Toxpi results
toxpi_results$Rank <- c(1:nrow(toxpi_results))

#set response level percent sizes 
BinPercents <- c(20,30,50)

#set number of slices
NoSlices <- 6

#set number of known samples per slice
knownratio <- 6

#set number of known samples
noKnown <- NoSlices * knownratio

#get number of samples
NoSamples <- nrow(toxpi_results)

#get list of response levels based on percents
#1 is highest ToxPi score (most bioactive)
response <- rep(1, round(NoSamples*BinPercents[1]/100))
BinCounts <- length(response)
response <- c(response, rep(2, round(NoSamples*BinPercents[2]/100)))
BinCounts <- c(BinCounts, length(response)-BinCounts)
response <- c(response, rep(3,NoSamples - length(response)))
BinCounts <- c(BinCounts, NoSamples-BinCounts[1]-BinCounts[2])

#append response levels to true data
truedata <- cbind(toxpi_results, response)

#read in testing data
data <- read.csv('uvcb_analysis/1994-House_SupTab4.csv')

#get slice names
slicenames <- c('P1_2_index','P3_index','P4_index','P5_index','P6_index','P7_index')

#append 0-1 to true data to identify samples with no PAC results
truedata$allzeroes <- rep(0, NoSamples)
for(i in 1:nrow(truedata)){
  name <- truedata$Name[i]
  if(sum(data[data$Substance == name,slicenames]) == 0){
    truedata$allzeroes[i] <- 1
  }
}

#assign number of known profiles per response level proportionally
noKnownBins <- round(noKnown*(BinCounts/NoSamples))
if(sum(noKnownBins) < noKnown){
  noKnownBins[NoBins] <- noKnownBins[NoBins]+1
} else if(sum(noKnownBins) > noKnown){
  noKnownBins[NoBins] <- noKnownBins[NoBins] - 1
}

#set seed for selecting known data
set.seed(5)

#select known samples for testing
knownData <- matrix(ncol = ncol(truedata))
colnames(knownData) <- colnames(truedata)
count <- 1
for(i in 1:3){
  candidates <- truedata[truedata[,'response']==i & truedata$allzeroes == 0,]
  knownData <- rbind(knownData, candidates[sample(nrow(candidates), size = noKnownBins[i]),])
}
knownData <- knownData[-1,]

#remove excess info
data_min <- data[,c('Substance','Class', 'P3_7_index', slicenames)]

#get rank from assay analysis
data_min$OriginRank <- rep(0, NoSamples)
for(i in 1:nrow(data_min)){
  data_min$OriginRank[i] <- toxpi_results[toxpi_results$Name == data_min$Substance[i],'Rank'] 
}

#append response levels to data
data_min$response <- rep(0, NoSamples)
for(i in 1:nrow(data_min)){
  if(data_min$Substance[i] %in% knownData$Name){
    data_min$response[i] <- knownData[knownData$Name==data_min$Substance[i],'response']
  }
}

############## Start Weight Estimation Process #####################
#load packages
library(ordinalNet)

#read in data, already done above
#data_min

#get slicenames, already done above
#slicenames

#set timer
ptm <- proc.time()

#get identifier column
Name <- 'Substance'

#rescale slice scores to range 0-1
for(i in slicenames){
  data_min[,i] <- (data_min[,i]-min(data_min[,i]))/(max(data_min[,i])-min(data_min[,i]))
}

#get known samples from user data
knownData <- data_min[data_min$response != 0,]

#get number of response levels
NoBins <- length(unique(knownData$response))

#get number of slices
NoSlices <- length(slicenames)

#get bin percent sizes from user, already done above
#BinPercents

#get counts and list of response levels, already done above
#BinCounts
#response

################# Perform Ordinal Regression ################################

#set ordering of response levels as factor
orderedBins <- vector()
for(i in 1:NoBins){
  orderedBins <- c(orderedBins, as.character(i))
}
Bins_factor <- factor(knownData[,'response'], ordered = TRUE, levels = orderedBins)
trues <- rep(TRUE, NoSlices)
  
#run the regression
result <- ordinalNet(as.matrix(knownData[,slicenames]), Bins_factor, positiveID = trues, lambdaVals = 0, maxiterOut = 100)
testW <- result$coefs[NoBins:(NoSlices+NoBins-1)]
testW <- testW/sum(testW)
ordrunTime <- proc.time()-ptm

#get ToxPi scores using regression model
score <- rowSums(data_min[,slicenames]*testW[col(data_min[,slicenames])])
ordResults <- cbind(data_min, score)
ordResults <- ordResults[order(ordResults[,'score'], decreasing = TRUE),]
ordResults$ordRank <- c(1:NoSamples)
  
############## Predict Weight sets Using Genetic Algorithm starting With Ordinal Weights######################
#get response level thresholds as a starting rank (ie 1 starts at 1, 2 starts at 29...)
BinRanks <- vector(length = NoBins)
BinRanks[1] <- 1 
for(i in 2:NoBins){
  BinRanks[i] <- BinRanks[i-1] + BinCounts[i-1]
}

#set convergence criteria
converge <- 2000

#assign starting weightset to regression results
bestW <- testW
ordW <- bestW

#determine the scores of all profiles using the proposed weights
score <- rowSums(data_min[,slicenames]*bestW[col(data_min[,slicenames])])
testData <- cbind(data_min,score)

#sort the data based on new scores
testData <- testData[order(testData[,'score'], decreasing = TRUE),]

#determine the rank and responses of "known" profiles using the proposed weights
index <- c(1:NoSamples)
testData <- cbind(testData, index)
observedResponse <- response
testData <- cbind(testData, observedResponse)

testKnowns <- testData[testData[,Name] %in% knownData[,Name],]

#get fitness of weights based on deviation of bins*(indices from bin) for known profiles
deviation <- 0
for(i in 1:noKnown){
  id <- testKnowns[i,Name]
  trueBin <- knownData[knownData[,Name] == id,'response']
  observedBin <- testKnowns[testKnowns[,Name] == id,'observedResponse']
  observedRank <- testKnowns[testKnowns[,Name] == id,'index']
  if(trueBin < observedBin){
    binDist <- abs(BinRanks[trueBin+1] - observedRank - 1)
  } else if(trueBin > observedBin){
    binDist <- BinRanks[trueBin] - observedRank
  } else {
    binDist <- 0
  }
  deviation <- deviation + abs(trueBin - observedBin)*binDist
}

#set fitness to best for starting GA
bestDeviation <- deviation
ordDeviation <- bestDeviation

#check to see if ordinal results already converged
if(bestDeviation == 0){
  print(bestW)
  sys.exit()
}

#run GA
count <- 1
while(count <= converge){
  testW <- bestW
  
  #get slice weights to inherit from Parent 1
  inherited <- sample(c(1:NoSlices), size = NoSlices-2)
  inheritedWeights <- bestW[inherited]
  
  #determine leftover weights needed to sum to 1
  currentSum <- sum(inheritedWeights)
  notInherited <- c(1:NoSlices)[!(c(1:NoSlices) %in% inherited)]
  neededSum <- 1-currentSum
  
  #determine new weights to inherit from Parent 2 (random generation)
  newW <- rgamma(2,1)
  newW <- newW/sum(newW)*neededSum
  
  #assign new weights to proper slices
  wPos <- 1
  for(j in notInherited){
    testW[j] <- newW[wPos]
    wPos <- wPos + 1
  }
  
  #determine the scores of all profiles using the proposed weights
  score <- rowSums(data_min[,slicenames]*testW[col(data_min[,slicenames])])
  testData <- cbind(data_min,score)
  
  #sort the data based on new scores
  testData <- testData[order(testData[,'score'], decreasing = TRUE),]
  
  #determine the rank and responses of "known" profiles using the proposed weights
  testData <- cbind(testData, index)
  observedResponse <- response
  testData <- cbind(testData, observedResponse)
  
  testKnowns <- testData[testData[,Name] %in% knownData[,Name],]
  
  #get fitness of weights based on deviation of bins*(indices from bin) for known profiles
  deviation <- 0
  for(i in 1:noKnown){
    id <- testKnowns[i,Name]
    trueBin <- knownData[knownData[,Name] == id,'response']
    observedBin <- testKnowns[testKnowns[,Name] == id,'observedResponse']
    observedRank <- testKnowns[testKnowns[,Name] == id,'index']
    if(trueBin < observedBin){
      binDist <- abs(BinRanks[trueBin+1] - observedRank - 1)
    } else if(trueBin > observedBin){
      binDist <- BinRanks[trueBin] - observedRank
    } else {
      binDist <- 0
    }
    deviation <- deviation + abs(trueBin - observedBin)*binDist
  }
  
  #check to see if proposed weights are better than last best weightset
  #if so start next generation, otherwise increase count for convergance criteria and try again
  if(deviation < bestDeviation){
    bestDeviation <- deviation
    bestW <- testW
    count <- 0
  } else {
    count = count + 1
  }
  #check to see if weight set converges to zero fitness
  if(bestDeviation==0){
    break
  }
}

#give a warning if GA does not converge to an optimum solution (ie deviation 0)
if(bestDeviation != 0){
  print("Warning: GA did not converge to a fitness of zero")
}
runTime <- proc.time()-ptm
print(ordrunTime)
print(runTime)

score <- rowSums(data_min[,slicenames]*bestW[col(data_min[,slicenames])])
gaResults <- cbind(data_min, score)
gaResults <- gaResults[order(gaResults[,'score'], decreasing = TRUE),]
gaResults$gaRank <- c(1:NoSamples)

#combine true and observed results for correlation testing
df_ord <- cbind(toxpi_results,ordResults[order(ordResults[,'OriginRank']),])
df_ga <- cbind(toxpi_results,gaResults[order(gaResults[,'OriginRank']),])

#view weightsets
print(ordW)
print(bestW)
print(ordDeviation)
print(bestDeviation)

#print score correlation
cor(df_ord$ToxPi.Score, df_ord$score, method = "spearman")
cor(df_ga$ToxPi.Score, df_ga$score, method = "spearman")

#print rank correlation
cor(df_ord$Rank, df_ord$ordRank)
cor(df_ga$Rank, df_ga$gaRank)

#print MAE percent of data size
mean(abs(df_ord[,'Rank'] - df_ord[,'ordRank'])/NoSamples)
mean(abs(df_ga[,'Rank'] - df_ga[,'gaRank'])/NoSamples)

#append ord rank results to ga dataframe
ordRank <- df_ord$ordRank
rankDF <- cbind(df_ga, ordRank)

#remove movement lines for samples that have no PAC data (ga score of 0)
for(i in 1:nrow(rankDF)){
  if(rankDF$gaRank[i] >= nrow(df_ga[df_ga$score != 0,])+1){
    rankDF$ordRank[i] <- rankDF$gaRank[i]
  }
}

#plot correlation results
mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(16)
ggplot(rankDF, aes(x = gaRank, y = Rank, color = Source)) + 
  geom_point(size = 2) + 
  theme_bw() +
  ggtitle("Correlation Plot For QC Assay vs PAC Data Rank: Ordinal Regression vs GA") +
  theme(plot.title = element_text(face = "bold", size = 12,hjust = 0.5),
        axis.text.x = element_text(color = c('black','black','black','red','red','black')),
        axis.text = element_text(size = 12)) +
  labs(x = "PAC Data Rank", y = "QC Assay Data Rank") +
  scale_colour_manual(values = mycolors) +
  geom_vline(aes(xintercept = nrow(df_ga[df_ga$score != 0,])), colour = "Red") +
  geom_vline(aes(xintercept = nrow(df_ord[df_ord$score != 0,])), colour = "Red") +
  geom_vline(aes(xintercept = BinRanks[2]), linetype = "dashed") +
  geom_vline(aes(xintercept = BinRanks[3]), linetype = "dashed")+
  geom_hline(aes(yintercept = BinRanks[2]), linetype = "dashed") +
  geom_hline(aes(yintercept = BinRanks[3]), linetype = "dashed")+
  geom_segment(aes(x=gaRank, xend = ordRank, y = OriginRank, yend = OriginRank)) + 
  scale_y_reverse(breaks = c(0, 40, 80, 120)) +
  scale_x_reverse(breaks = c(0, 40, 80, nrow(df_ord[df_ord$score != 0,]), nrow(df_ga[df_ga$score != 0,]), 120)) +
  geom_abline(slope = 1, intercept = 0)
