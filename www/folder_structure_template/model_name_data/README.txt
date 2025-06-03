README for model data folder

This folder can either contain folders containing each data set or a data_locator.csv file with an absolute local path or url to there the data is stored. To avoid duplication when a dataset is included in multiple models you can include the path in the data_locator.csv. The data_locator.csv path column will be checked first and if it is empty data in the sub-folders will be used. 

Sub-folders:
	- observation_data: A csv file of species counts with 4 letter species codes as headers and a shapefile, gpkg, or csv containing survey location information.
	- predictor_data: A csv file of covariate values at each survey location with covariate names as headers and one row per survey location.
	- transfer_data: Covariate grid for making predictions. One raster file per covariate of a single multilayer raster files.  
	- prediction_data: A raster of predicted values of the response given the transfer data.
	- uncertainty_data: A raster quantifying uncertainty in the predictions. 
