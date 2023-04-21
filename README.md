For full details on the methodology and results please refer to the above manuscript folder. For a step-by-step guide on how to reproduce the analysis, refer to the 'Instructions' below. All code is licensed under a AGPL-v3 open license, while the manuscript and data is licensed as CC-BY. Consult the individual directories and their LICENSE file. Please be mindful of the license restrictions, as they will be enforced.


# Diagnosing evapotranspiration responses to water deficit across biomes using deep learning
•	Accounting for water limitation is key to determining vegetation sensitivity to drought. Quantifying water limitation effects on ET is challenged by the heterogeneity of vegetation types, climate zones and vertically along the rooting zone. 
•	Here, we train deep neural networks using flux measurements to study ET responses to progressing drought conditions. We determine a water stress factor (fET) that isolates ET reductions from effects by atmospheric aridity and other co-varying drivers. We regress fET against the cumulative water deficit (CWD), which reveals the control of whole-column moisture availability. 
•	We find a variety of ET responses to water stress. Responses range from rapid declines of fET to 10% of its water-unlimited rate at several savannah and grassland sites, to mild fET reductions in most forests, despite substantial water deficits. Most sensitive responses are found at the most arid and warm sites.
•	A combination of regulation of stomatal and hydraulic conductance and access to belowground water reservoirs, whether in groundwater or deep soil moisture, could explain the different behaviors observed at some sites. This variety of responses is not captured by a state-of-the-art land surface model, likely reflecting simplifications in its representation of belowground water storage. 

# Instructions
[![DOI](https://zenodo.org/badge/491055054.svg)](https://zenodo.org/badge/latestdoi/491055054)

First things first, clone this repo to your local computer:

```
git clone https://github.com/computationales/fET
```

To reproduce the analysis and figures, you can follow the steps described in the `analysis` folder and its 'README.md' file. To avoid overwriting the dataframes already loaded in this repo, the scripts contained in the `analysis` folder will save their output in the main directory of the project (aka the directory where this README also is). 

Below is an overview of the content of the others directories. You can refer to the READMEs inside each directory for detailed instructions. You can find [here](https://github.com/computationales/R-proj-template) the project template used for this repo, explained in detail (highly suggested to keep your project tidy!). 

* `R`: contains all the R functions (not scripts) used in the analysis.
* `data-raw`: contains raw data and the scripts used to download and extract raw data, as well as other scripts to process the raw data.
* `data`: contains processed data, ready to use for the analysis. It also contains the outputs of the deep neural networks (DNN) model.
* `manuscript`: contains manuscript and figures.
* `vignettes`: contains the vignette to produce Box 1. 

