[![DOI](https://zenodo.org/badge/491055054.svg)](https://zenodo.org/badge/latestdoi/491055054)
For full details on the methodology and results please refer to the above manuscript folder. For a step-by-step guide on how to reproduce the analysis, refer to the 'Instructions' below. All code is licensed under AGPL-v3, and the manuscript and data are licensed as CC-BY. Please review the individual directories and their LICENSE file for more information. 

![Fig. 1 Location of sites considered in this study, colored by their respective group of fractional reduction in evapotranspiration (fET group, see section 2.5). Blue dots: 'high fET' group. Yellow dots: 'medium fET' group. Red dots: 'low fET' group. Black crosses: sites that were excluded from the initial list of 135 sites considered in this study. Coastlines were downloaded using R package 'rnaturalearth' (Massicotte & South, 2023).](manuscript/Figures/world_map.png)
###### Fig. 1 Location of sites considered in this study, colored by their respective group of fractional reduction in evapotranspiration (fET group, see section 2.5). Blue dots: 'high fET' group. Yellow dots: 'medium fET' group. Red dots: 'low fET' group. Black crosses: sites that were excluded from the initial list of 135 sites considered in this study. Coastlines were downloaded using R package 'rnaturalearth' (Massicotte & South, 2023).

# Diagnosing evapotranspiration responses to water deficit across biomes using deep learning
•	Accounting for water limitation is key to determining vegetation sensitivity to drought. Quantifying water limitation effects on ET is challenged by the heterogeneity of vegetation types, climate zones and vertically along the rooting zone. 

•	Here, we train deep neural networks using flux measurements to study ET responses to progressing drought conditions. We determine a water stress factor (fET) that isolates ET reductions from effects by atmospheric aridity and other co-varying drivers. We regress fET against the cumulative water deficit (CWD), which reveals the control of whole-column moisture availability. 

•	We find a variety of ET responses to water stress. Responses range from rapid declines of fET to 10% of its water-unlimited rate at several savannah and grassland sites, to mild fET reductions in most forests, despite substantial water deficits. Most sensitive responses are found at the most arid and warm sites.

•	A combination of regulation of stomatal and hydraulic conductance and access to belowground water reservoirs, whether in groundwater or deep soil moisture, could explain the different behaviors observed across sites. This variety of responses is not captured by a standard land surface model, likely reflecting simplifications in its representation of belowground water storage. 


# Instructions
First things first, clone this repo to your local computer:

```
git clone https://github.com/computationales/fET
```

To reproduce the analysis and figures, you can follow the steps described in the `analysis` folder and its 'README.md' file. To avoid overwriting the dataframes already loaded in this repo, the scripts contained in the `analysis` folder will save their output in the main directory of the project (aka the directory where this README also is). 

Below is an overview of the content of the others directories. You can refer to the READMEs inside each directory for detailed instructions. 

* `R`: contains all the R functions used in the analysis.
* `data-raw`: contains raw data and the scripts used to download and extract raw data, as well as other scripts to process the raw data.
* `data`: contains processed data, ready to use for the analysis. It also contains the outputs of the deep neural networks (DNN) model.
* `manuscript`: contains manuscript and figures.
* `vignettes`: contains the vignette to produce Box 1 (now shown in the manuscript). 

