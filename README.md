# ToxPi Weight Optimization: Guided Optimization of ToxPi Model Weights using a Semi-Automated Approach  

## **Coming Soon:**  
Easily accessible weight optimization methods to run on your own data, including the methods shown in this paper, will be available soon in a new ToxPi web application. The web application is a new software built upon Rshiny that houses several new methods and visualizations surrounding the ToxPi framework. This will allow users novel analysis options, increased model flexibility and sharing, and customizable visualizations, all without the need to download any software! Please check back soon for the final product and how to access it!  

## **GitHub Directory:**  
The following scripts and data are used to explore and recreate results from Fleming et al, Guided Optimization of ToxPi Model Weights using a Semi-Automated Approach. The results can be used to explore the expected error of weight optimization based on user model complexity. To run weight optimization on your own data, please see the toxpiR package upon deployment of version 2.0.  

### **Simulation Studies:**    
Simulation studies were run on several different models with varying data to explore the estimated error of weight estimation based on model complexity. The selected factors to test model complexity based on common use cases were as follows:  
* Number of slices - (3, 6, 9, 12, 15)  
* Ratio defining number of samples with prior response knowledge compared to number of slices - (3x, 6x, 9x, 12x, 15x)  
* Total number of samples - (500, 1000, 5000, 10000)  
* Number of response levels - (2, 3, 4)  
* Underlying slice data distributions - (Normal, Gamma, Uniform, Mixed)  
* Convergence criteria for the genetic algorithm - (50, 100, 500, 1000)

All possible combinations of selected factors were tested 1000 times to produce an empirical error distribution for each combination. All methods and results are located within the folder labeled simulation_studies. Information within the folder:  
* MAEData - inidividual error results for every trial and every combination  
* stats_data - some summary statistics for distributions of error and running time  
* GA_WithSimulationSetup.R - R script for running simulation study for a specified set of factors, takes in 3 ordered parameters for parallelization:    
    * Total number of samples
    * Number of response levels
    * Convergence criteria for the genetic algorithm  
    * Note: Each run automatically tests across all combinations for number of slices, ratio of known data, and underlying slice data distributions, and outputs the results for MAE data and stats data to the current working directory   
* run_Simulations.sh - example bash script for running R simulation script, used to send tests to a cluster  
    * Note: The bash script provided contains 16 runs, ensuring that all runs were sent to the same node for accurate comparison of running time when analyzing varying total number of samples in Figure 7. The results for this script are located within stats_data/runningTime_sameNode. The results solely within stats_data are for all parameter combinations and were obtained by 1 run per bash script, allowing for analysis and comparison of error summary statistics but not running time.  
* SupplementalPlots.pdf - plots containing distributions for MAE error as a percent of dataset size, and plots containing distributions for MAE improvement by the GA over ordinal regression, for all factor combinations tested. These are provided to users for expected error measurements based on their model complexity.  
* ConvergencePlots_Figure7.R - script for reading in and plotting simulation study results to reproduce Figure 7.  
* MAE_Plots_Figure8.R - script for reading in and plotting simulation study results to reproduce Figure8.  
* GADifferencePlots_Figure9.R - script for reading in and plotting simulation study results to reproduce Figure 9.  

## **UVCB Analysis:** 
Weight optimization methods were tested on published data to verify viability and to show a real life use case. The script for analysis and input data are located within the folder labeled uvcb_analysis. Information within the folder:  
* qcpheno.out_results.csv - ToxPi results from House et al containing ranking for tested UVCB substances, used as the ground truth of ranking for comparison against  
* 1994-House_SupTab4.csv - Data from House et al containing PAC information for tested UVCB substances, used to estimate weights and build a model that can be compared to the ground truth  
* PACAnalysis_Figure10.R - script used to estimate weights and compare models. Reads in the above files, estimates weights and rankings, and plots the correlation plot shown in Figure 10.  

Citation for House et al: 
House, J. S., Grimm, F. A., Klaren, W. D., Dalzell, A., Kuchi, S., Zhang, S.-D., Lenz, K., Boogaard, P. J., Ketelslegers, H. B., Gant, T. W., Wright, F. A. and Rusyn, I. (2021) “Grouping of UVCB substances with new approach methodologies (NAMs) data”, ALTEX - Alternatives to animal experimentation, 38(1), pp. 123–137. doi: 10.14573/altex.2006262.  

## **Figures:**  
This folder contains high resolution images for each of the figures presented in the paper, as well as the supplemental plots. The supplemental plots can be used to estimate predicted error for weight optimization based on model and data complexity, allowing users to have a general idea of how accurate their results may be.  