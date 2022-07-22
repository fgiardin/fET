#' Prepares data for training
#'
#' @param df A data frame containing observational data for all the predictors and training variables. One variable among predictors has to be soil moisture \code{"soilm"}.
#' @param settings A list
#'
prepare_trainingdata_fvar <- function(df, settings){

  df <- df %>%

  	##------------------------------------------------
  	## Get observational soil moisture data (not the same number of layers available for all sites)
  	##------------------------------------------------
  	## normalize soil moisture within zero and one
    mutate_at(vars(one_of(settings$varnams_soilm)), list(~norm_to_max(.)) ) %>%

    # # ## get mean observational soil moisture across different depths (if available)
    # mutate( soilm_mean = apply( dplyr::select( ., starts_with("SWC_")), 1, FUN = mean, na.rm = TRUE ) ) %>%
    # mutate( soilm_mean = ifelse( is.nan(soilm_mean), NA, soilm_mean ) ) %>%

    ##------------------------------------------------
    ## removing NA, necessary for NN training
    ##------------------------------------------------
    dplyr::filter_at( settings$target, all_vars(!is.na(.)) )


  ## finally remove NAs in observed soil moisture data
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
