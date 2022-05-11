#' Prepares data for training
#' 
#' Removes NAs and outliers.
#' 
#' @param df A data frame containing observational data for all the predictors and training variables. One variable among predictors has to be soil moisture \code{"soilm"}. 
#' @param settings A list
#' 
prepare_trainingdata_fvar <- function(df, settings){

  df <- df %>% 
    
    ##------------------------------------------------
    ## Remove outliers in target variable
    ##------------------------------------------------
    ## xxx help: this needs to do either or, not both, depending on what is settings$target!

    #dplyr::mutate_at(vars(settings$target), ~remove_outliers(.) ) %>%
  
    # dplyr::mutate_at(vars(nomi), ~remove_outliers(.) ) %>% # remove outliers to everything

  	##------------------------------------------------
  	## Get observational soil moisture data (not the same number of layers available for all sites)
  	##------------------------------------------------
  	## normalize soil moisture within zero and one
    mutate_at(vars(one_of(settings$varnams_soilm)), list(~norm_to_max(.)) ) %>%

    # mutate_at(vars(nomi), list(~norm_to_max(.)) ) %>% # normalize everything

	
    # # ## get mean observational soil moisture across different depths (if available)
    # mutate( soilm_mean = apply( dplyr::select( ., starts_with("SWC_")), 1, FUN = mean, na.rm = TRUE ) ) %>%
    # mutate( soilm_mean = ifelse( is.nan(soilm_mean), NA, soilm_mean ) ) %>%

#   ##------------------------------------------------
#   ## Temperature filter (below that, weird things happening)
#   ##------------------------------------------------
# 	## Use only days where temperature is above 5 degrees
    # dplyr::filter( temp > 5.0 ) %>%

    ##------------------------------------------------
    ## removing NA, necessary for NN training
    ##------------------------------------------------
    dplyr::filter_at( settings$target, all_vars(!is.na(.)) )
  
  ##------------------------------------------------
  ## Remove soil moisture layers if too many values are missing (>25% of layer with maximum data points)
  ##------------------------------------------------
  # ## get number of data points per layer
  # lengths <- apply( dplyr::select( df, one_of(settings$varnams_soilm), -ends_with("QC") ), 2, function(x) sum(!is.na(x)) ) %>% t() %>% as_tibble()
  # if (ncol(lengths)>0) settings$varnams_soilm <- lengths %>% names() # update settings
  #   
  # ## drop layer swc obs data if length of data is less than 75% of length of maximum
  # idx <- 0
  # drop_idx <- c()
  # for (ivar in settings$varnams_soilm){
  #   idx <- idx + 1
  #   if (lengths[ivar]<0.75*max(lengths)){
  #     df[[ ivar ]] <- NULL
  #     drop_idx <- c(drop_idx, idx)
  #   }
  # }
  # if ( length(drop_idx)>0 ) { settings$varnams_soilm <- settings$varnams_soilm[-drop_idx] } #update settings
  
  ## finally actually remove NAs in observed soil moisture data
  df <- df %>% 
    dplyr::filter_at( settings$varnams_soilm, all_vars(!is.na(.)) ) %>% 
    
    ## retain only target and predictors
    dplyr::select(settings$rowid, one_of(settings$predictors), one_of(settings$varnams_soilm), one_of(settings$target)) %>% 
    
    ## remove rows with NA values
    drop_na()  

  # ## rename soil moisture column to 'soilm'
  # df$soilm <- df[ settings$varnams_soilm ]
  # df <- df[,-which(names(df)==settings$varnams_soilm)]

  return( df )

}


norm_to_max <- function( vec ){
  vec <- ( vec - min( vec, na.rm=TRUE ) ) / ( max( vec, na.rm=TRUE ) - min( vec, na.rm=TRUE ) )
  return( vec )
}
