# ToxPi Weight Optimization: Guided Optimization of ToxPi Model Weights using a Semi-Automated Approach  
## **GitHub Directory:**  
The following scripts and data are used to explore and recreate results from Fleming et al, Guided Optimization of ToxPi Model Weights using a Semi-Automated Approach. The results can be used to explore the expected error of weight optimization based on user model complexity. To run weight optimization on your own data, please see the toxpiR package upon deployment of version 2.0.  

### Simulation Studies:   
Simulation studies were run on several different models with varying data to explore the estimated error of weight estimation based on model complexity. The selected factors to test model complexity based on common use cases were as follows:  
* Number of slices - (3, 6, 9, 12, 15)  
* Ratio defining number of samples with prior response knowledge compared to number of slices - (3x, 6x, 9x, 12x, 15x)  
* Total number of samples - (500, 1000, 5000, 10000)  
* Number of response levels - (2, 3, 4)  
* Underlying slice data distributions - (Normal, Gamma, Uniform, Mixed)  

All possible combinations of selected factors were tested 1000 times to produce an empirical error distribution for each combination. All methods and results are located within the folder labeled simulation_studies. Information within the folder:
* MAEData - inidividual error results for every trial and every combination
* statsData - 
The  are located within the folder labeled MAEData. Selected factors included the following:  
* MAE Data
* For users looking to generate a predesigned layer file of ToxPi profiles with Python, or users who are unsure, see [Method 1](#method-1-toxpi_creationpy--toxpi_creation_customizedpy).  

* For users looking to use an ArcGIS Toolbox(e.g., for integration into existing ArcGIS workflows), see [Method 2](#method-2-toxpitoolboxtbx).  

* For users looking to see example walkthroughs, see the vignettes in [Examples](./Examples/).  

* For users looking to visualize existing maps, see [Visualizations](./Examples/Visualizations.md).  

* For users requiring help with data manipulation and formatting requirements, see [Utilities](./Utilities/).  

<a id="ToxPi-Description"></a>

## **ToxPi Description:** 

**Definition**  
The ToxPi framework provides a method for transparently integrating and visualizing data across disparate information domains and is often used to determine risk values for the data being analyzed.  
<br>

**Description**  
Data that are not normally compared are combined into a data matrix comprising various data domains, or slice categories, with varying weights that represent the different data categories. Each slice category, represented by a color scheme, can then be separated into subdomains. The data matrix is then analyzed to produce a risk score ranging from 0 to 1 for each slice category, as well as a corresponding overall score that represents the record’s total risk from all categories. These scores are determined via a rank system for easy comparison between different data points and are displayed in a ToxPi profile for each record. If the data record's are locations, the figures can be mapped geographically. The calculation process, as well as ToxPi profile interpretation for Covid-19 data, are depicted below. For a more in depth look at the ToxPi framework, visit www.toxpi.org

<p align = "center">
Calculation  
<br>
<img src="./Images/ToxPiDescription.PNG" data-canonical-  
src="./Images/ToxPiDescription.PNG" width="550" height="425" />  
</p>  

<p align = "center">
Interpretation
<br>
<img src="./Images/ToxPiInterpretation.PNG" data-canonical-  
src="./Images/ToxPiInterpretation.PNG" width="550" height="425" />  
</p> 

<a id="Map-Creation-Workflow"></a>

## **Map Creation Workflow:**  

<p align = "center">
<img src="./Images/MapCreationWorkflow.PNG" data-canonical-  
src="./Images/MapCreationWorkflow.PNG" width="600" height="300" />  
</p>  

<a id="method-1-toxpi_creationpy--toxpi_creation_customizedpy"></a>
  
## **Method 1: ToxPi_creation.py & ToxPi_creation_customized.py**   
Use the script [ToxPi_creation.py](ToxPi_creation.py)  to automatically produce predesigned feature layers containing interactive ToxPi profiles using the output of the ToxPi GUI as input. This is the suggested method, unless you are skilled with ArcGIS Pro and have a specific need for a Toolbox. An example walkthrough is shown in [Vignette 1](./Examples/Vignette1-%20Using%20ToxPi_creation.md). 
<br></br>
[ToxPi_creation_customized.py](ToxPi_creation_customized.py) can be used with county or census tract data for a more data rich map and acts as an example of how ToxPi_creation.py can be customized with further geoprocessing steps to create more advanced maps for specific data. An example walkthrough is shown in [Vignette 3](./Examples/Vignette3-%20Using%20ToxPi_creation_customized.md).  

Steps:  
1A. Load raw data into the [ToxPi GUI](https://toxpi.org/)  
1B. Analyze data and output results file to a CSV, and make sure file meets data requirements    
1C. Run python script from windows command prompt using the required parameters  
1D. Open output layer file in ArcGIS Pro  
1E. Share resulting map to ArcGIS Online  

<a id="method-2-toxpitoolboxtbx"></a>

## **Method 2: ToxPiToolbox.tbx**  
The ToxPiToolbox.tbx file is an ArcToolbox that contains a custom tool called ToxPi Construction for drawing the polygons that make up ToxPi profiles. It requires more manual steps than the script; however, a model can be created for the automation of map creation, and the toolbox allows for more customization than the script, including drawing a subset of slices for ToxPi profiles. This, along with a walkthrough example, are described under [Vignette 2](./Examples/Vignette2-%20Using%20ToxPiToolbox.md).  

Steps:  
2A. Load raw data into the [ToxPi GUI](https://toxpi.org/)  
2B. Analyze data and output results file to a CSV, and split the coordinates into two separate columns  
2C. Add results file to ArcGIS Pro  
2D. Add ToxPiToolbox.tbx to ArcGIS Pro  
2E. Run required analysis steps including the ToxPi tool and share resulting map to ArcGIS Online   

<a id="Acknowledgements/References:"></a>

## **Acknowledgements:**  
We thank the PVI project team for data provision and the Baker Lab at NCSU for software testing. We also thank Dan Schmitt and Logan Wenzel for their expertise in implementation. This research was supported by the National Institutes of Health (NIH) grant awards ES030007 and ES025128, and by intramural funds from the National Institute of Environmental Health Sciences and the National Institute for Allergy and Infectious Diseases. Portions of the code for drawing slices were adapted from the Coxcomb tool for ArcGIS Pro. 

## Citation:
Fleming J, Marvel SW, Supak S, Motsinger-Reif AA, Reif DM. ToxPi*GIS Toolkit: creating, viewing, and sharing integrative visualizations for geospatial data using ArcGIS. J Expo Sci Environ Epidemiol. 2022 Apr 26. doi: 10.1038/s41370-022-00433-w. PMID: 35474345.

Open-access link to full-text article: [https://www.nature.com/articles/s41370-022-00433-w](https://www.nature.com/articles/s41370-022-00433-w) 
