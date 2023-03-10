This folder contains data produced from original datasets by other scripts in the repo.

-   `output`: folder containing the output dataframes from script 1 and 2 of the analysis

-   `output_GLDAS`: folder containing the output dataframes extracted from GLDAS maps. Files are in the GLDAS subfolders.

-   `dataframes`: folder containing dataframes outputted by script **analysis/3.summary_plots**. These dataframes are used also by script **analysis/4.other_figures/interpret_fET_groups.R** and **data-raw/extract_HWSD.R**

-   **PET_output.rds**: contains PET from the SPLASH model (used in the script "3.summary_plots.R"").

-   **SM_thresholds_allsites.rds**: contains soil moisture thresholds for all sites.

-   **modobs_fluxnet2015_s11_s12_s13_with_SWC_v3.rds**: dataframe containing gapfilled data and modelled soil moisture from Stocker et al., 2018. PET_output.rds: dataframe containing PET outputs from SPLASH model. SM_thresholds_allsites.rds: dataframe containing soil moisture thresholds for all sites (see Methods).

-   **soilm_data_usability_fluxnet2015.csv**: table providing usability of soil moisture data from FLUXNET2015

-   **table1_final.rds**: table outputted by script **analysis/4.other_figures/interpret_fET_groups.R** containing all the data used to produce Figure 6

-   **fET_timeseries.rds**: summary dataframe containing the output of the deep learning model together with site-level soil and climate variables (available also in .csv format)
