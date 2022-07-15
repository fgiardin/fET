# The fET repo 

This repo contains scripts and instructions to reproduce the analysis presented in "Plant access to belowground moisture allows sustained evapotranspiration during drought" by *Giardina et al.*, submitted to the journal Global Change Biology (GCB) in July 2022. 

# Instructions

First things first, clone this repo to your local computer:

```
git clone https://github.com/computationales/fET
```

To reproduce the analysis and figures, you can then follow the steps described in the `analysis` folder and its 'README.md' file. 

Below is an overview of the content of the others directories. You can refer to the READMEs inside each directory for detailed instructions. You can find [here](https://github.com/computationales/R-proj-template) the project template used for this repo, explained in detail (highly suggested to keep your project tidy!). 

* `The R folder`: contains all the R functions (not scripts) used in the analysis.
* `data-raw`: contains raw data and the scripts used to download and pre-process the data.
* `data`: contains pre-processed data, ready to use for the analysis. It also contains the outputs of the deep neural networks (DNN) model.
* `manuscript`: contains the manuscript and figures as submitted to GCB. 
* `vignettes`: contains the vignette to produce Box 1. 



