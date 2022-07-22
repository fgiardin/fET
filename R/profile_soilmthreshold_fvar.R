profile_soilmthreshold_fvar <- function(df_train,
                                        settings,
                                        weights = NA,
                                        len = NA,
                                        verbose = FALSE){

  ## profile threshold
  thresh_seq <- seq(0.3, 0.7, len)

  list_eval <- purrr::map(
    as.list(thresh_seq),
    ~profile_soilmthreshold_fvar_bythreshold(
      .,
      df_train = df_train,
      settings = settings,
      weights = weights,
      verbose = verbose
      )
    )
  names(list_eval) <- as.character(thresh_seq)

  return(list_eval)
}

profile_soilmthreshold_fvar_bythreshold <- function(threshold,
                                                    df_train,
                                                    settings,
                                                    weights = NA,
                                                    verbose = FALSE){

  ## Train/predict
  out <- train_predict_fvar(
    df_train,
    settings,
    soilm_threshold    = threshold,
    weights            = NA,
    verbose            = verbose
  )

  ## Performance evaluation
  list_eval <- test_performance_fvar(out, settings)

  return(list_eval)
}
