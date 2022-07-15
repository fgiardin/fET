# Script to exctract WorldClim data at FLUXNET2015 locations

# load vector of final sites
load("manuscript/Figures/dataframes/vec_sites.RData")

settings_worldclim <- list(varnam = c("bio"))

# directly extract from website using package ingestr
df_worldclim <- ingest(
  siteinfo_fluxnet2015 %>% dplyr::filter(sitename %in% vec_sites), # take only 58 sites of analysis
  source    = "worldclim",
  settings  = settings_worldclim,
  dir       = "~/data/worldclim"
)

df_worldclim_reformat <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("name_site", "MAT_worldclim", "MAP_worldclim"))


for (site_ID in vec_sites){
  print(site_ID)
  # filter row with our site
  BIO = df_worldclim %>%
    dplyr::filter(sitename==site_ID)

  if(dim(BIO)[1] == 0){} # first check if the dataframe exists
  else{
    # extract the dataframe
    df_BIO = BIO[[2]][[1]] %>%
      mutate(name_site = site_ID) %>%
      rename(MAT_worldclim = bio_1) %>% # bio_1 and bio_12 are the corresponding
      rename(MAP_worldclim = bio_12)    # variable names according to metadata

    df_worldclim_reformat <- rbind(df_worldclim_reformat, df_BIO)
  }
}

# save dataframe
save(df_worldclim_reformat, file = "./df_WorldClim.RData")

