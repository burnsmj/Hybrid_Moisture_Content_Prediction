# Nixtamalization Moisture Content Prediction Application
Michael Burns  
University of Minnesota - Applied Plant Sciences

## Description:
To improve the utility of this research project, as well as that of Burns et al. (2021), we have integrated the nixtamalization moisture content prediction models of this paper and Burns et al. (2021) into a Rshiny application which runs locally and is freely available. To utilize this application, please have R (and probably RStudio) installed locally. Clone this repository and open the file named app.R. In the upper right corner of the scripts window in RStudio, click the button that says 'Run App'. The application will start (the first time running may take extra time as it will check for and install needed packages). Once the Shiny application is running, click on the instructions tab in the top ribbon for more detailed use information.

## Included objects:
- app.R: 
  - The shiny application  
- hybrid_model.rda:
  - R object containing the machine learning model from this manuscript  
- inbred_model.rda:
  - R object containing the machine learning model from Burns et al. 2021  
- hybrid_data_example.csv:
  - A csv file containing hybrid sample spectra to be used as an example of using the application  
- inbred_data_example.csv:
  - A csv file containing inbred sample spectra to be used as an example of using the application  
- custom_model_example.rda:
  - R object containing a basic pls model to show how the custom model option can be used in the application.
  - Note: predictions from this model are not meant to be meaningful  
