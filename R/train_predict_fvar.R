#' Trains models for potential and actual fluxes. 
#' 
#' @param df A data frame containing observational data for all the predictors and training variables with all NAs removed.
#' @param settings A character string defining which variable (column name in \code{df}) is to be used as target variable.
#' 
train_predict_fvar <- function( df, settings, soilm_threshold, weights=NA, plot=FALSE, verbose = FALSE ){
  
  ##------------------------------------------------
  ## Determine "moist days", i.e. where soil moisture is above threshold.
  ## Get respective indices.
  ##------------------------------------------------
  ## If multiple layer's soil moisture data is available, do subset w.r.t. soil layer with highest value
  # df <- df %>% mutate( maxsoilm = apply( dplyr::select( df, one_of(settings$varnams_soilm) ), 1, FUN = max, na.rm = TRUE ) )
  # idxs_moist <- which( df$maxsoilm > soilm_threshold )
  
  ## simple
  idxs_moist <- which( df[[settings$varnams_soilm]] > soilm_threshold )
  
  ## identify and record moist days for this threshold
  vec_moist <- rep( NA, nrow(df) )
  vec_moist[idxs_moist]  <- TRUE
  vec_moist[-idxs_moist] <- FALSE
  
  ## If no weights are specified, use 1 for all data points
  if (is.na(weights)) weights <- rep( 1.0, nrow(df) )
  
  ##------------------------------------------------
  ## Loop and aggregate over repetitions
  ##------------------------------------------------
  out <- purrr::map( # map -> apply a function to each element of a list and returning an object of the same length
    as.list(seq(settings$nrep)),
    ~train_predict_fvar_byrep( 
      ., 
      df,
      idxs_moist,
      settings, 
      weights = weights, 
      plot = FALSE,
      verbose = verbose
    )
  )
  
  names(out) <- paste0("rep", seq(settings$nrep))
  
  df_all <- out %>% 
    purrr::map_dfr("df_all") %>% # purr:map_dfr -> returns a data frame created by row-binding
    rename(idx = !!settings$rowid) %>% 
    dplyr::group_by(idx) %>%
    dplyr::summarize(
      nn_pot = mean(nn_pot, na.rm = TRUE),
      nn_act = mean(nn_act, na.rm = TRUE),
      fvar   = mean(nn_fxx, na.rm = TRUE)
    ) %>%
    dplyr::mutate(moist = vec_moist) %>%
    dplyr::mutate(fvar = remove_outliers_fXX(fvar, coef=3.0)) %>% 
    rename(!!settings$rowid := idx) %>% 
    
    ## complement with target variable and threshold variable (soil moisture)
    mutate(obs = df[[ settings$target ]],
           soilm = df[[ settings$varnams_soilm ]])
  
  df_cv <- out %>% 
    purrr::map_dfr("df_cv") %>% 
    rename(obs = !!settings$target) %>% #rename target variable to obs
    rename(idx = !!settings$rowid) %>% 
    dplyr::group_by(idx) %>%
    dplyr::summarize(
      nn_pot = mean(pred_pot, na.rm = TRUE),
      nn_act = mean(pred_act, na.rm = TRUE),
      obs    = mean(obs, na.rm = TRUE)
    ) %>%
    dplyr::mutate(moist = vec_moist) %>% 
    rename(!!settings$rowid := idx)
  
  ## return only models from first repetitions
  return( list(nn_act = out$rep1$nn_act, nn_pot = out$rep1$nn_moist, df_all = df_all, df_cv = df_cv) )
}



train_predict_fvar_byrep <- function( irep, df, idxs_moist, settings, weights=NA, plot=FALSE, verbose, ... ){
  
  ## Do training only if sufficient data is available
  if ( length(idxs_moist)>30 && (nrow(df) - length(idxs_moist))>30 ){
    
    if (verbose) rlang::inform(paste("NN repetition", irep))
    
    ##------------------------------------------------
    ## Train model on moist days' data, not using soil moisture as predictor ("_moist")
    ##------------------------------------------------
    ## Remove all variables starting with "soilm_" from predictors
    # settings$varnams_soilm <- df %>% dplyr::select( starts_with("soilm") ) %>% names()
    #settings$predictors_without_soilm <- settings$predictors[ !(settings$predictors %in% settings$varnams_soilm) ]
    
    if (settings$package == "nnet") { 
      
      out_nn_moist <- predict_nn(
        data       = df[ idxs_moist, ],
        weights    = weights[ idxs_moist ],
        predictors = settings$predictors,
        nam_target = settings$target,
        do_predict = TRUE,
        package    = settings$package,
        lifesign   = "full",
        seed       = irep,
        hidden     = settings$num_units_per_layer[1] #number of units per layer (only one layer in caret - no deep NN)
      )
      
      # ## Evaluate predictions of good days model
      # stats_nn_moist <- rsofun::analyse_modobs(
      #   out_nn_moist$vals,
      #   df[[settings$target]][idxs_moist],
      #   plot.title="NN pot, moist only",
      #   do.plot=plot
      # )
      
      ##------------------------------------------------
      ## Get potential values (fvar_pot) with moist-days model, predicting all days
      ##------------------------------------------------
      ## Note: model is passed on as argument 'nn', therefore no training is done. 
      out_nn_pot <- predict_nn( 
        data       = df, 
        weights    = weights,
        predictors = settings$predictors,
        nam_target = settings$target, 
        nn         = out_nn_moist$nn, 
        do_predict = TRUE, 
        package    = settings$package
      )
      
      # ## Evaluate predictions of moist days model
      # stats_nn_pot <- rsofun::analyse_modobs(
      #   out_nn_pot$vals,
      #   df[[settings$target]],
      #   plot.title="NN pot",
      #   do.plot=plot
      # )
      
      ##------------------------------------------------
      ## Train full model, including soil moisture as a predictor ("act")
      ##------------------------------------------------
      out_nn_act <- predict_nn( 
        data       = df, 
        weights    = weights,
        predictors = settings$predictors, 
        nam_target = settings$target, 
        do_predict = TRUE, 
        package    = settings$package,
        seed       = irep,
        hidden     = settings$num_units_per_layer[1] #number of units per layer (only one layer in caret - no deep NN)
      )
      
      # ## get statistics of mod vs. obs of all-days full model
      # stats_nn_act <- rsofun::analyse_modobs( 
      #   out_nn_act$vals, 
      #   df[[settings$target]],
      #   plot.title = "NN act", 
      #   do.plot    = plot
      # )
      
      
    } else if (settings$package == "keras") {
      ##------------------------------------------------
      ## Train full model, including soil moisture as a predictor ("act")
      ##------------------------------------------------
      out_nn_act <- predict_nn_keras(   df                  = df,
                                        nam_target          = settings$target, 
                                        #predictors          = settings$predictors,
                                        predictors          = c(settings$predictors, settings$varnams_soilm), 
                                        nfolds              = settings$nfolds,
                                        num_epochs          = settings$num_epochs, 
                                        batch_size          = settings$batch_size, 
                                        val_size            = settings$val_size,
                                        learning_rate       = settings$learning_rate,
                                        num_layers          = settings$num_layers,
                                        num_units_per_layer = settings$num_units_per_layer,  
                                        dropout_rate        = settings$dropout_rate,
                                        nn = NULL,
      )
      
      # ### USE NNact MODEL WITH SM SET TO 1 TO PREDICT PET (commentare codice sotto se si usa)
      # out_nn_pot <- predict_nn_keras(   df                  = df %>% mutate(SWC_F_MDS_1 = 1), # force SM to be = 1
      #                                   nam_target          = settings$target,
      #                                   predictors          = c(settings$predictors, settings$varnams_soilm),
      #                                   nfolds              = NA,
      #                                   num_epochs          = NA,
      #                                   batch_size          = NA,
      #                                   val_size            = NA,
      #                                   learning_rate       = NA,
      #                                   num_layers          = NA,
      #                                   num_units_per_layer = NA,  # settings$nnodes_pot,
      #                                   dropout_rate        = settings$dropout_rate,
      #                                   nn                  = out_nn_act$nn, # use NNact model
      #                                   pp                  = out_nn_act$pp
      # )
      # 
      # out_nn_moist <- out_nn_pot
      
      # model trained on moist days only (then use it to predict PET on all days)
      out_nn_moist <- predict_nn_keras(       df                  = df[ idxs_moist, ],
                                              nam_target          = settings$target,
                                              predictors          = settings$predictors,
                                              nfolds              = settings$nfolds,
                                              num_epochs          = settings$num_epochs,
                                              batch_size          = settings$batch_size,
                                              val_size            = settings$val_size,
                                              learning_rate       = settings$learning_rate,
                                              num_layers          = settings$num_layers,
                                              num_units_per_layer = settings$num_units_per_layer,
                                              dropout_rate        = settings$dropout_rate,
                                              nn = NULL,
      )


      ##------------------------------------------------
      ## Get potential values (fvar_pot) with moist-days model, predicting all days
      ##------------------------------------------------

      out_nn_pot <- predict_nn_keras(   df                  = df,
                                        nam_target          = settings$target,
                                        predictors          = settings$predictors,
                                        nfolds              = NA,
                                        num_epochs          = NA,
                                        batch_size          = NA,
                                        val_size            = NA,
                                        learning_rate       = NA,
                                        num_layers          = NA,
                                        num_units_per_layer = NA,  # settings$nnodes_pot,
                                        dropout_rate        = settings$dropout_rate,
                                        nn                  = out_nn_moist$nn,
                                        pp                  = out_nn_moist$pp
      )

      
    } else {
      rlang::warn("Package not supported")
    }
    
    
    ##------------------------------------------------
    ## Construct data frame from validation results
    ##------------------------------------------------
    
    df_cv_act <- out_nn_act$df_cv %>%
      rename(obs := !!settings$target) %>%  # cambio nome "obs" in "ET" - i punti esclamativi servono a far funzionare l'espressione nel tidyverse (altrimenti solo character non lo prende)
      left_join(df %>% 
                  dplyr::select(settings$rowid),
                by = settings$rowid
      ) %>% 
      rename(pred_act = pred)
    
    df_cv_pot <- out_nn_moist$df_all %>%
      rename(obs := !!settings$target) %>%  # cambio nome "obs" in "ET" - i punti esclamativi servono a far funzionare l'espressione nel tidyverse (altrimenti solo character non lo prende)
      left_join(df %>%
                  dplyr::select(settings$rowid),
                by = settings$rowid) %>%
      rename(pred_pot = pred)
    
    df_cv <- df %>% 
      dplyr::select(!!settings$rowid, settings$target) %>% 
      left_join(df_cv_act, by = settings$rowid) %>% 
      left_join(df_cv_pot, by = settings$rowid) %>% 
      # pivot_longer(cols = c(obs_act, obs_pot), names_to = "model", values_to = "obs", names_prefix = "obs_") %>% 
      dplyr::select(settings$rowid, settings$target, pred_act, pred_pot) %>%  
      mutate(irep = irep)
    
    df_cv$moist <- rep(FALSE, nrow(df_cv))
    df_cv$moist[idxs_moist] <- TRUE
    
    out_nn_act$df_cv <- NULL
    out_nn_moist$df_cv <- NULL

    ##------------------------------------------------
    ## Construct predictions data frame with all
    ##------------------------------------------------
    df_all <- df %>% 
      dplyr::select(!!settings$rowid, settings$target) %>% 
      left_join(out_nn_pot$df_all %>% 
                  dplyr::select(settings$rowid, nn_pot = pred), 
                by = settings$rowid) %>% 
      left_join(out_nn_act$df_all %>% 
                  dplyr::select(settings$rowid, nn_act = pred), 
                by = settings$rowid) %>% 
      mutate(nn_fxx = nn_act / nn_pot,
             irep = irep)
    
    # df_all <- tibble(
    #   rowid  = df[[ settings$rowid ]],
    #   nn_pot = as.vector(out_nn_pot$df_all$pred),
    #   nn_act = as.vector(out_nn_act$df_all$pred),
    #   nn_fxx = as.vector(out_nn_act$df_all$pred) / as.vector(out_nn_pot$df_all$pred),
    #   irep   = irep) %>% 
    #   mutate(idx = 1:n())
    
    # ## rename row ID
    # df_all <- df_all %>% setNames(c(settings$rowid, names(.)[-1]))
    
  } else {
    
    rlang::warn("Too few data points with current soil moisture threshold")
    
    df_all <- tibble(
      date  = df[[ settings$rowid ]],
      ET = NA,
      pred_pot = NA,
      pred_act = NA,
      moist = NA,
      nn_pot = NA, 
      nn_act = NA, 
      nn_fxx = NA, 
      irep   = irep
    )
    
    df_cv <- tibble(  # added empty df for the other objects returned by the function 
      date  = df[[ settings$rowid ]], # (otherwise optimal SM threshol algorithm won't work)
      ET = NA,
      pred_pot = NA,
      pred_act = NA,
      moist = NA,
      nn_pot = NA, 
      nn_act = NA, 
      nn_fxx = NA, 
      irep   = irep
    )
    
    out_nn_act <- tibble(
      date  = df[[ settings$rowid ]],
      ET = NA,
      pred_pot = NA,
      pred_act = NA,
      moist = NA,
      nn_pot = NA, 
      nn_act = NA, 
      nn_fxx = NA, 
      irep   = irep
    )
    
    out_nn_moist <- tibble(
      date  = df[[ settings$rowid ]],
      ET = NA,
      pred_pot = NA,
      pred_act = NA,
      moist = NA,
      nn_pot = NA, 
      nn_act = NA, 
      nn_fxx = NA, 
      irep   = irep
    )
    
    # ## rename row ID
    # df_all <- df_all %>% setNames(c(settings$rowid, names(.)[-1]))
    # df_cv <- df_all %>% setNames(c(settings$rowid, names(.)[-1]))
    # out_nn_act <- df_all %>% setNames(c(settings$rowid, names(.)[-1]))
    # out_nn_moist <- df_all %>% setNames(c(settings$rowid, names(.)[-1]))
    
  }
  
  return(list(df_all = df_all, df_cv = df_cv, nn_act = out_nn_act, nn_moist = out_nn_moist))	
  
}


