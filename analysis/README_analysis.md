To reproduce the analysis, run the scripts in the order. Scripts 1 and 2 need to be run on a HPC cluster (here: Euler from ETHZ Zürich)


## 1.data_screening_euler.R
Prepare fluxnet2015 raw data in the right format for the deep learning model.


## 2.run_ML_model_euler.R
Runs the deep learning model.


## 3.summary_plots_euler.R
Generates results on the performance of the model (Fig. 1). It divides the results in groups and prints the grouping statistics (Fig. 4). It also prints the fET vs CWD multi-panel figure (Fig. 5). 


## 4.other_figures
Contains the scripts to generate the other figures. 
### ET_bias_profile.R: prints "fET vs relative soil moisture" (current Fig. 3)
### ET_time_series.R: prints "Seasonality of predicted and observed ET for sample sites" (current Fig. 2)
### interpret_fET_groups.R: prints "Analysis of soil and climate variables per fET group" and "Minimum fET as a function of the aridity index per site" (current Figs 6 and 7)
### print_fET_vs_CWD_allsites_bygroup.R: prints "fET vs CWD for sites grouped according to their median fET" (current Supplementary Figs 3-4-5) 