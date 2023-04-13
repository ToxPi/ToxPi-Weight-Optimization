#!/bin/bash

#load R
module load R

#run the simulation study using the following command
# RScript GA_WithSimulationSetup.R NoSamples NoBins ConvergenceLimit
#for every combination of the following parameters
# NoSamples = c(500, 1000, 5000, 10000)
# NoBins = c(2, 3, 4)
# ConvergenceLimit = c(50, 100, 500, 1000)
# ie:
# NoSamples|NoBins|ConvergenceLimit
# 500      |2     |50
# 500      |2     |100
# 500      |2     |500
# 500      |2     |1000
# 500      |3     |50
#...
# 10000    |4     |1000
#Note: Due to high running time, each combination was run in a separate Bash script sent to a computing cluster. Results are were taken and placed in stats_data folder and MAEData folder.
#      Due to different power nodes on the cluster, to get comparable running time plots for Figure 6: row 3, the 16 combinations containing a fixed NoBins = 3 were rerun from one common bash script, 
#          and GA_WithSimulationSetup.R was altered to only run models for 9 slices, 9 known/slice ratio, and normally distributed data to save running time, results placed in stats_data/runningTime_sameNode.
#          The convergence statistics for all other slice, ratio, and distribution combinations can be found and compared in stats_data and MAEData, but running time should not be compared across files       

Rscript GA_WithSimulationSetup.R 500 3 50
Rscript GA_WithSimulationSetup.R 500 3 100
Rscript GA_WithSimulationSetup.R 500 3 500
Rscript GA_WithSimulationSetup.R 500 3 1000
Rscript GA_WithSimulationSetup.R 1000 3 50
Rscript GA_WithSimulationSetup.R 1000 3 100
Rscript GA_WithSimulationSetup.R 1000 3 500
Rscript GA_WithSimulationSetup.R 1000 3 1000
Rscript GA_WithSimulationSetup.R 5000 3 50
Rscript GA_WithSimulationSetup.R 5000 3 100
Rscript GA_WithSimulationSetup.R 5000 3 500
Rscript GA_WithSimulationSetup.R 5000 3 1000
Rscript GA_WithSimulationSetup.R 10000 3 50
Rscript GA_WithSimulationSetup.R 10000 3 100
Rscript GA_WithSimulationSetup.R 10000 3 500
Rscript GA_WithSimulationSetup.R 10000 3 1000