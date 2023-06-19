To reproduce the analysis, run the following scripts in order. Scripts 1 and 2 need to be run on a HPC cluster (we used Euler from ETHZ Zürich).

* **How_to_run_scripts_on_Euler.md**: Instructions to run scripts 1 and 2 on Euler.

* **1.data_screening_euler.R**: Prepares FLUXNET2015 raw data in the right format for the deep learning model.

* **2.run_ML_model_euler.R**: Runs the deep learning model for every site and saves model outputs.

* **3.summary_plots.R**: Generates results on the performance of the model (Fig. 2). It divides the results in groups and prints the grouping statistics (Fig. 4). It also prints the fET vs CWD multi-panel figure (Fig. 5). It also produces Supplementary Figs 1-2).

* **commands.txt**: Contains the list of jobs to be submitted to the cluster for the script "2.run_ML_model_euler.R" (see "How_to_run_scripts_on_Euler.md" for instructions).

### `4.other_figures`
Folder that contains the scripts to generate all other figures. 

* **ET_bias_profile.R**: prints "fET vs relative soil moisture" (not shown)
* **EVI_distributions.R**: plots EVI distribution used to train ET-NN vs distribution to train PET-NN (Fig. S8)
* **interpret_fET_groups.R**: prints "Analysis of soil and climate variables per fET group" (current Fig. 7)
* **map_allsites.R**: prints global map of CWD with location of flux sites (Fig. 1)
* **print_fET_vs_CWD_allsites_bygroup.R**: prints "fET vs CWD for sites grouped according to their median fET" (current Fig. 6 and Figs S3-S4)

#### Subfolder `timeseries`
* **ET_timeseries.R**: prints "Seasonality of predicted and observed ET for sample sites" (Fig. 3)
* **EVI_timeseries.R**: similar to "ET_time_series", plots EVI too on top of ET timeseries
* **SPLASH_timeseries.R**: looks into details of seasonality of PET-NN vs PET from SPLASH at some dry sites (reply to reviews)
