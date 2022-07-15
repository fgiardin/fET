This folder contains the functions used in the analysis.

# Model definition and tuning

* **prepare_trainingdata_fvar.R**: prepare data for training (outlier removal, etc.)

* **train_predict_fvar.R**: Trains models for potential and actual fluxes. Construct data frame from validation results. 

* **predict_nn_keras.R**: built, train, predict and cross-validate deep learning model with package 'keras'. Function called in the script train_predict_fvar.R

* **predict_nn_keras.R**: built, train, predict and cross-validate single layer machine learning model with package 'caret' (not used here). Function called in the script train_predict_fvar.R

* **keras_grid_search.R**: script to launch grid search of hyperparameters (called by the function tuning_run())


# Soil moisture threshold

* **profile_soilmthreshold_fvar.R**: function to find optimal soil moisture threshold to divide between moist and dry days

* **get_opt_threshold.R**: function to extract best soil moisture threshold 

* **test_performance_fvar.R**: function to calculate metrics to test the performance of the models. Called by profile_soilmthreshold_fvar.R


# Other analyses

* **calc_cwd_lue0.R**: Function to fit a bilinear regression and test its BIC against a linear model (used for the fET vs CWD charts)

* **mct.R**: function to calculate the cumulative water deficit

* **gapfill_nn.R**: Function used to gapfill time series of fluxnet variables with neural networks (such gapfilled time series were used to calculate the CWD)

* **process_ldas.R**: Function to extract GLDAS data from a specific layer of the GLDAS map and at a specific fluxnet2015 site. 

* **remove_outliers.R**: Simple function to remove outliers



# Visualizations

* **LSD.heatscatter.R**: function to hack heatscatterpoints() from LSD package and transform the output in a ggplot object

* **scatterheat.R**: function to print scatter plots used to print model performance, based on heatscatterpoints() function from LSD package




