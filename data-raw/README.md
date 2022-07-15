This folder contains raw data and the scripts to download and pre-process the data.

* `GLDAS`: folder that contains some GLDAS data provided as example 

* **FLX_AU-Wom_FLUXNET2015_FULLSET_HH_2010-2012_1-3.csv**: table containing FLUXNET2015 data for site AU-Wom (provided here as an example to run the data screening script locally)

* **ai_fluxnet2015.Rdata**: dataframe containing aridity index

* **extract_GTI.R.ç**: Script to extract global topographic index (GTI) values at FLUXNET2015 locations. 

* **extract_HWSD.R**: Script to extract soil data from the HWSDR dataset. 

* **extract_gldas.R**: Script to extract GLDAS variables and put them in the right format. Designed to be run site-by-site on a cluster (see intstructions below).

* **extract_WorldClim.R**: Script to extract MAT and MAP from WorldClim at FLUXNET2015 locations. 

### Instructions to download and extract GLDAS data at FLUXNET sites using ETHZ's HPC cluster (Euler):

1. On your terminal, connect to the cluster and navigate to the folder where you would like to download the data. Then use wget: 

```
wget --load-cookies ~/.urs_cookies --save-cookies ~/.urs_cookies --auth-no-challenge=on --keep-session-cookies --content-disposition -i ~/data/LDAS/subset4.txt
```

Where 'subset4.txt' is a text file containing the list of download links of the GLDAS files as produced on the LDAS website. 


2. Load the following modules (needed to run the R script extract_gldas.R):

```
env2lmod
module load gcc/6.3.0 r/4.0.2 gdal/3.1.2 udunits2/2.2.24 proj/4.9.2 geos/3.6.2 python_gpu/3.8.5 netcdf/4.4.1.1
module load eth_proxy
```

3. Define vector of FLUXNET sites where SM is consistent (see Methods):

```
#!/bin/bash
declare -a vec_site=("AR-SLu" "AR-Vir" "AT-Neu" "AU-Ade" "AU-ASM" "AU-Cpr" "AU-Cum" "AU-DaP" "AU-DaS" "AU-Dry" "AU-Emr" "AU-Fog" "AU-Gin" "AU-GWW" "AU-How" "AU-RDF" "AU-Rob" "AU-Stp" "AU-Tum" "AU-Wac" "AU-Whr" "AU-Wom" "AU-Ync" "BE-Bra" "BE-Lon" "BE-Vie" "BR-Sa3" "CH-Cha" "CH-Dav" "CH-Fru" "CH-Lae" "CH-Oe1" "CH-Oe2" "CN-Cng" "CN-Dan" "CN-Din" "CN-Du2" "CN-Qia" "CZ-BK1" "CZ-BK2" "CZ-wet" "DE-Akm" "DE-Geb" "DE-Gri" "DE-Hai" "DE-Kli" "DE-Lkb" "DE-Obe" "DE-RuR" "DE-RuS" "DE-Seh" "DE-SfN" "DE-Spw" "DE-Tha" "DK-Fou" "DK-NuF" "DK-Sor" "ES-LgS" "ES-Ln2" "FI-Hyy" "FI-Jok" "FI-Sod" "FR-Fon" "FR-Gri" "FR-LBr" "FR-Pue" "GF-Guy" "IT-BCi" "IT-CA1" "IT-CA2" "IT-CA3" "IT-Col" "IT-Cp2" "IT-Cpz" "IT-Isp" "IT-La2" "IT-Lav" "IT-MBo" "IT-Noe" "IT-PT1" "IT-Ren" "IT-Ro1" "IT-Ro2" "IT-SR2" "IT-SRo" "IT-Tor" "JP-MBF" "JP-SMF" "NL-Hor" "NL-Loo" "NO-Adv" "RU-Fyo" "SD-Dem" "SN-Dhr" "US-AR1" "US-AR2" "US-ARb" "US-ARc" "US-ARM" "US-Blo" "US-Cop" "US-GLE" "US-Ha1" "US-Los" "US-Me2" "US-Me6" "US-MMS" "US-Myb" "US-Ne1" "US-Ne2" "US-Ne3" "US-ORv" "US-PFa" "US-SRG" "US-SRM" "US-Syv" "US-Ton" "US-Tw1" "US-Tw2" "US-Tw3" "US-Tw4" "US-Twt" "US-UMB" "US-UMd" "US-Var" "US-WCr" "US-Whs" "US-Wi0")
```

4. We can then run the script to extract the wanted data at FLUXNET locations from the maps we downloaded at point 1:

```
for val in "${vec_site[@]}"; do
  echo $val
  bsub -W 1:00 -u fgiardina -J "job_name $val" -R "rusage[mem=2000]" "Rscript --vanilla extract_gldas.R $val"
done
```
