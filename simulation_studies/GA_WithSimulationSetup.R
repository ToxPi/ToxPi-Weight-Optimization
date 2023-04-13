set.seed(5)
count = 0
################## Load In Required Packages ###################################
library(ordinalNet)
library(ggplot2)
library(gridExtra)
library(gtable) 
library(grid)


############## Set parameters to test across ###################################
#read in NoSamples and NoBins for running in parallel
args = commandArgs(trailingOnly = TRUE)
NoSamples = as.integer(args[1])
NoBins = as.integer(args[2])

#set other scenario parameters
NoSlicesList <- c(3,6,9,12,15)
SlicesKnownsRatio <- c(3,6,9,12,15)
dataTypes <- c(1,2,3,4)

#get list of parameter combinations
parameterSweep <- expand.grid(dataTypes, NoSlicesList, SlicesKnownsRatio, NoSamples)

####################### Get Other Parameters ###################################
#set convergence for GA for running in parallel
converge <- as.integer(args[3])

#get bin percent sizes
if(NoBins == 2){
  BinPercents <- c(35, 65)
} else if(NoBins == 3){
  BinPercents <- c(20, 30, 50)
} else if(NoBins == 4){
  BinPercents <- c(10, 20, 30, 40)
}

################# Create Required Functions ####################################

################# Function to extract legend ###################################
grid_arrange_shared_legend <- function(plots) {
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call("arrangeGrob", c(plotlist[[i]], nrow = 1, ncol = 5)), top = paste(i*3,'X Ratio', sep = ""), left = 'Error'
    ,
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight*2, lheight))
}
############## Generate Perfect Testing Data ###################################
GenerateData <- function(NoSlices, noKnowns, NoSamples, NoBins, dataType){
  #set seed for data replication
  set.seed(5)
  
  #assign true test weights
  trueW <- rep(1/NoSlices, NoSlices)
  
  #prep data matrix
  data <- matrix(nrow = NoSamples, ncol = NoSlices)
  
  #generate random values from any distribution and add as slice column
  distribution <- dataType
  for(i in 1:NoSlices){
    if(dataType == 4){
      distribution <- sample(c(1:3),1)
    }
    if(distribution == 1){
      x <- rnorm(NoSamples, 10)
    } else if(distribution == 2){
      x <- rgamma(NoSamples,1)
    } else if(distribution == 3){
      x <- runif(NoSamples)
    } 

    data[,i] <- x
  }
  
  #rescale slice scores to range 0-1
  for(i in 1:NoSlices){
    data[,i] <- (data[,i]-min(data[,i]))/(max(data[,i])-min(data[,i]))
  }
  
  #generate scores for data using true weights
  trueScores <- rowSums(data*trueW[col(data)]/100)
  data <- cbind(data,trueScores)
  
  #label index to remember sample starting position
  index <- c(1:NoSamples)
  data <- cbind(data, index)
  
  #sort scores to obtain sample ranks
  data <- data[order(data[,"trueScores"], decreasing = TRUE),]
  
  #label samples with an id based on their true rank
  id <- c(1:NoSamples)
  data <- cbind(data, id)
  
  #unsort the data to avoid any bias in later methods
  data <- data[order(data[,"index"]),]
  
  #remove excess columns from data
  data <- data[,c(1:NoSlices, NoSlices+3)]
  
  return(data)
}

############## Create Testing Function #########################################
TestWeights <- function(w,data, knownData){
  
  #determine the scores of all profiles using the proposed weights
  score <- rowSums(data[,1:NoSlices]*w[col(data[,1:NoSlices])])
  testData <- cbind(data,score)
  
  #sort the data based on new scores
  testData <- testData[order(testData[,'score'], decreasing = TRUE),]
  
  #determine the rank and bins of "known" profiles using the proposed weights
  NoSamples <- nrow(data)
  index <- c(1:NoSamples)
  testData <- cbind(testData, index)
  testData <- cbind(testData, bins)
  testKnowns <- testData[testData[,'id'] %in% knownData[,'id'],]

  #get fitness of weights based on deviation of bins*(indices from bin) for known profiles
  deviation <- 0
  for(i in 1:noKnown){
    #get sample info
    id <- testKnowns[i,'id']
    trueBin <- knownData[knownData[,'id'] == id,'bins']
    observedBin <- testKnowns[testKnowns[,'id'] == id,'bins']
    observedRank <- testKnowns[testKnowns[,'id'] == id,'index']
    #get rank distance of sample from true bin
    if(trueBin < observedBin){
      binDist <- abs(BinRanks[trueBin+1] - observedRank - 1)
    } else if(trueBin > observedBin){
      binDist <- BinRanks[trueBin] - observedRank
    } else {
      binDist <- 0
    }
    #add sample deviations together
    deviation <- deviation + abs(trueBin - observedBin)*binDist
  }
  return(deviation)
}

################## Create Main Function ######################################
main <- function(seed){
  set.seed(seed)
  ###############assign knowns ######################################
  #get known profile ids and slice scores generated from testing data
  knownData <- matrix(nrow = noKnown, ncol = NoSlices+2)
  colnames(knownData) <- c(1:NoSlices,'id','bins')
  count <- 1
  for(i in 1:(NoBins-1)){ 
    #get ids for randomly sampled knowns based on number needed and bin rank range
    samples <- sample(data[data[,'id'] %in% c(BinRanks[i]:(BinRanks[i+1]-1)),'id'], size = noKnownBins[i]) 
    #append all known samples into known data list and add their response level i
    for(j in samples){
      knownData[count,] <- c(data[data[,'id']==j,], i) #add sample j to known data and append response level i
      count = count + 1 #increment position of next known sample to be added
    }
  }
  #repeat above for last bin
  samples <- sample(data[data[,'id'] %in% c(tail(BinRanks,1):NoSamples),'id'], size = tail(noKnownBins,1))
  for(j in samples){
    knownData[count,] <- c(data[data[,'id']==j,],NoBins)
    count = count + 1
  }
  
  ################# Perform Ordinal Regression ################################
  #set a timer 
  ptm <- proc.time()
  
  #get ordering of response levels, 1 is always the smallest rank or highest score(ie rank 1/250)
  orderedBins <- vector()
  for(i in 1:NoBins){
    orderedBins <- c(orderedBins, as.character(i))
  }
  #assign ordering with known data as a factor 
  Bins_factor <- factor(knownData[,'bins'], ordered = TRUE, levels = orderedBins)
  
  #set all coefficients to be positive
  trues <- rep(TRUE, NoSlices)
  
  #run ordinal regression on known data
  #lambdavals <- 0 for unpenalized regression
  result <- ordinalNet(as.matrix(knownData[,1:NoSlices]), Bins_factor, positiveID = trues, lambdaVals = 0)
  
  #get beta coefficients as weights
  testW <- result$coefs[NoBins:(NoSlices+NoBins-1)]
  
  #rescale coefficients to sum to 1
  testW <- testW/sum(testW)
  
  #get running time
  ordrunTime <- proc.time()-ptm
  
  #get ToxPi scores using assigned model
  score <- rowSums(data[,1:NoSlices]*testW[col(data[,1:NoSlices])])
  ordResults <- cbind(data[,1:(NoSlices+1)], score)
  
  #sort based on score
  ordResults <- ordResults[order(ordResults[,'score'], decreasing = TRUE),]
  
  #append new rank as index to results
  index <- c(1:NoSamples)
  ordResults <- cbind(ordResults, index)
  
  ############## Predict Weight sets Using Genetic Algorithm starting With Ordinal Weights######################

  #assign starting weightset to regression
  bestW <- testW
  
  #get starting best fitness 
  bestDeviation <- TestWeights(bestW, data, knownData)[1]
  
  if(bestDeviation == 0){
    #state ordinal regression results already converged
    OrdinalConvergence = 1
  } else { 
    #state ordinal regression hasn't converged
    OrdinalConvergence = 0
  }
  
  #set crossover rate
  i <- NoSlices-2
  
  #initialize number of current iterations within a generation to 1 for hill climbing approach
  count <- 1
  
  #breed generations until convergence criterion is met
  while(count <= converge){
    #get previous best weightset
    testW <- bestW
    
    #randomly select slice weights to inherit
    inherited <- sample(c(1:NoSlices), size = i)
    inheritedWeights <- bestW[inherited]
    
    #get current sum of inherited slice weights
    currentSum <- sum(inheritedWeights)
    
    #determine slice weights still needed
    notInherited <- c(1:NoSlices)[!(c(1:NoSlices) %in% inherited)]
    
    #determine sum of weights still needed
    neededSum <- 1-currentSum
    
    #randomly generate needed weights to represent Dirichlet distribution
    newW <- rgamma(NoSlices-i,1)
    
    #rescale new weights to sum to remaining needed sum
    newW <- newW/sum(newW)*neededSum
    
    #update test weights by assigning new weights
    wPos <- 1
    for(j in notInherited){
      testW[j] <- newW[wPos]
      wPos <- wPos + 1
    }
    
    #get fitness of new weightset
    testDeviation <- TestWeights(testW, data, knownData)
    
    #if fitness has improved, start a new generation, else continue generation
    if(testDeviation < bestDeviation){ #improved fitness
      #update best weight set and fitness
      bestDeviation <- testDeviation
      bestW <- testW
      #reset current number of iterations within generation
      count <- 1
      
    } else {#same or worsened fitness
      #increment iterations attempted
      count = count + 1
    }
    
    #if perfect fitness end the GA
    if(bestDeviation==0){
      break
    }
  }
  
  #get GA run time
  runTime <- proc.time()-ptm
  
  #assign scores for GA weightset
  score <- rowSums(data[,1:NoSlices]*bestW[col(data[,1:NoSlices])])
  gaResults <- cbind(data[,1:(NoSlices+1)], score)
  
  #order results by score and assign rank as index
  gaResults <- gaResults[order(gaResults[,'score'], decreasing = TRUE),]
  gaResults <- cbind(gaResults, index)

  #get MAE as percent of dataset size for ordinal regression
  ordinalError <- mean(abs(ordResults[,'index'] - ordResults[,'id'])/NoSamples)
  
  #get MAE as percent of dataset size for GA
  gaError <- mean(abs(gaResults[,'index'] - gaResults[,'id'])/NoSamples)
  
  #check if GA converged to optimal solution (fitness = 0)
  if(bestDeviation != 0){
    GAConvergence = 0 #failed
  } else {
    GAConvergence = 1 #succeeded
  }

  return(c(ordinalError, gaError, ordrunTime['elapsed'], runTime['elapsed'], GAConvergence, OrdinalConvergence))
}

################ Sweep across Parameters to Test Results #######################
#set num of trials for benchmark study
iterations <- 1000

#set seeds for each iteration to keep consistency in selecting knowns between scenarios
seedlist <- sample(c(1:100000), size = iterations)

#prepare plots
filename <- paste(converge, "Converge", NoBins, "Bins", NoSamples, "SamplesV2", sep = "")
plotlist <- list()
plotrow <- 1
plotcolumn <- 1

#prepare MAE distribution df individually and combined for all parameter sweeps
distributionDF <- as.data.frame(matrix(ncol = 3))
colnames(distributionDF) <- c('Errors', 'Distribution', 'Method')
allDist <- as.data.frame(matrix(ncol = 8))
colnames(allDist) <- c('Errors', 'Distribution', 'Method', 'NoSlices','KnownRatio','NoSamples','NoBins', 'ConvergeLim')

#prepare distribution stats df
errorDF <- as.data.frame(matrix(nrow = nrow(parameterSweep), ncol = 15))
errorDF[,1:4] <- parameterSweep[,1:4]
errorDF[,5] <- rep(converge, size = nrow(parameterSweep))
colnames(errorDF) <- c('Distribution', 'NoSlices', 'KnownRatio', 'NoSamples', 'ConvergeLim', 'OrdinalMed', 'Ordinal2.5','Ordinal97.5', 'GaMed', 'Ga2.5', 'Ga97.5', 'OrdRunningTime', 'GaRunningTime', 'GAConvergence', 'OrdinalConvergence')

for(row in 1:nrow(parameterSweep)){
  ##### Prep work for simulation parameters
  #get parameters for testing
  NoSamples <- parameterSweep[row,4]
  distribution <- parameterSweep[row,1]
  NoSlices <- parameterSweep[row,2]
  knownratio <- parameterSweep[row,3]
  noKnown <- parameterSweep[row,3] * NoSlices
  
  #get size and starting rank of each bin (This is needed in the actual GA as well)
  BinCounts <- c(NoSamples*BinPercents/100)
  BinRanks <- vector(length = NoBins)
  BinRanks[1] <- 1 
  for(i in 2:NoBins){
    BinRanks[i] <- BinRanks[i-1] + BinCounts[i-1]
  }
  
  #get list of responses to place in bins based on rank
  bins <- vector()
  for(i in 1:NoBins){
    bins <- c(bins, rep(i,BinCounts[i]))
  }
  trueBins <- bins
  
  #assign number of known profiles per bin
  noKnownBins <- round(noKnown*(BinCounts/NoSamples))
  if(sum(noKnownBins) < noKnown){
    noKnownBins[NoBins] <- noKnownBins[NoBins]+1
  } else if(sum(noKnownBins) > noKnown){
    noKnownBins[NoBins] <- noKnownBins[NoBins] - 1
  }
  
  #Generate Testing Data
  data <- GenerateData(NoSlices, noKnowns, NoSamples, NoBins, distribution)
  
  #Get test results for 1,000 different knownsets
  ordinalListMAE <- vector()
  gaListMAE <- vector()
  ordrunTimeList <- vector()
  runTimeList <- vector()
  GAConvergeList <- vector()
  OrdinalConvergeList <- vector()
  
  #perform trials for defined simulation
  for(i in 1:iterations) {
    #run ordinal regression and GA
    results <- main(seedlist[i])
    
    #append results to lists
    ordinalListMAE <- c(ordinalListMAE, results[1])
    gaListMAE <- c(gaListMAE, results[2])
    ordrunTimeList <- c(ordrunTimeList, results[3])
    runTimeList <- c(runTimeList, results[4])
    GAConvergeList <- c(GAConvergeList, results[5])
    OrdinalConvergeList <- c(OrdinalConvergeList, results[6])
  }
  
  #append data distribution used and method performed(ordinal) numeric placeholders to list of ordinal errors
  tmplist <- cbind(ordinalListMAE, rep(distribution, iterations), rep(1,iterations))
  colnames(tmplist) <- c('Errors', 'Distribution', 'Method')
  
  #append ordinal errors to current parameter combination error list
  distributionDF <- rbind(distributionDF, tmplist)
  
  #append data distribution used and method performed(GA) numeric placeholders to list of ga errors
  tmplist <- cbind(gaListMAE, rep(distribution, iterations), rep(2,iterations))
  colnames(tmplist) <- c('Errors', 'Distribution', 'Method')
  
  #append ga errors to current parameter combination error list
  distributionDF <- rbind(distributionDF, tmplist)

  #append distribution statistics for current parameter to stats df
  errorDF[row,6:15] <- c(quantile(ordinalListMAE, probs = c(.5,.025,.975)), quantile(gaListMAE, probs = c(.5,.025,.975)), mean(ordrunTimeList), mean(runTimeList), sum(GAConvergeList), sum(OrdinalConvergeList))
  
  #plot results if on last data type distribution
  if(distribution == length(dataTypes)){
    
    plot <- ggplot(distributionDF[-1,], aes(factor(Distribution), Errors, fill = factor(Method), dodge = Method)) + stat_boxplot(geom='errorbar') + geom_boxplot() + theme_bw() + theme(legend.position = "None", axis.title.y=element_blank(), plot.title = element_text(hjust = .5)) + ylim(0,.15) + labs(x = 'Distribution', title = paste(NoSlices, noKnown, NoSamples, NoBins), fill = 'Method') + scale_fill_discrete(labels = c("Ordinal", "GA")) 
    
    #set up list of plots based on position in grid, each row is a list within the main list
    if(plotcolumn == 1){
      plotlist[[plotrow]] <- list()
    }
    
    plotlist[[plotrow]][[plotcolumn]] <- plot
    
    if(plotcolumn == length(NoSlicesList)){
      plotcolumn <- 1
      plotrow <- plotrow + 1
    } else{
      plotcolumn <- plotcolumn + 1
    }
    
    #add parameter information to current error distribution 
    # *length(datatypes) is for distributions, *2 is for methods
    addinfo <- cbind(distributionDF[-1,], rep(NoSlices,iterations*length(dataTypes)*2), rep(knownratio,iterations*length(dataTypes)*2), rep(NoSamples, iterations*length(dataTypes)*2),rep(NoBins,iterations*length(dataTypes)*2),rep(converge,iterations*length(dataTypes)*2))
    colnames(addinfo) <- c('Errors', 'Distribution', 'Method', 'NoSlices','KnownRatio','NoSamples','NoBins','ConvergeLim')
    
    #add current error distribution to all error distribution df
    allDist <- rbind(allDist,addinfo)
    
    #reset current error distribution to empty
    distributionDF <- as.data.frame(matrix(ncol = 3))
    colnames(distributionDF) <- c('Errors', 'Distribution', 'Method')
  }
}

####uncomment the following and optionally add pathing to get plots for current parameters being read in from the command line
# #output pdf location
# pdf(paste("SliceVsRatioMAE", filename, ".pdf", sep = ""), onefile = TRUE, width = 11, height = 9.5)
# 
# #place plots into pdf
# for(i in seq(length(plotlist))){
#   grid_arrange_shared_legend(plotlist[[i]])
# }
# 
# #close pdf
# dev.off()

#save stats information, optionally add pathing
save(errorDF, file = paste("stats", filename, ".Rdata", sep = ""))

#remove empty first row
allDist <- allDist[-1,]

#save individual error results for all trials and distributions, optionally add pathing
save(allDist, file = paste("dists", filename, ".Rdata", sep = ""))
