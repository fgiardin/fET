predict_nn <- function( data, predictors, nam_target, weights=NULL, nn=NULL, do_predict=TRUE,
                        do_modobs=FALSE, lifesign="none", package="nnet", seed=1, hidden=NULL ){

  if (package=="nnet"){

    require( nnet )
    require( caret )

    downscale <- FALSE
    if (is.null(nn)){

      forml  <- as.formula( paste( nam_target, "~", paste( predictors, collapse=" + " ) ) )

      ## this has caused a problem before due to values being too high
      if (mean(data[[ nam_target ]], na.rm = TRUE)>1e6){
        data[[ nam_target ]] <- data[[ nam_target ]] * 1e-6
        downscale <- TRUE
      }

      traincotrlParams <- caret::trainControl(
        method="repeatedcv",
        number=5,
        repeats=5,
        verboseIter=FALSE,
        savePredictions = "final"
        )

      if (is.null(hidden)){
        tune_grid <- expand.grid( .decay = c(0.05, 0.01, 0.005), .size = seq(5,30,5) )
        # tune_grid <- expand.grid( .decay = c(0.01), .size = 14 )
      } else {
        tune_grid <- expand.grid( .decay = c(0.01), .size = c(hidden) )
      }

      set.seed(seed)

      nn <- caret::train(
        forml,
        data      = data,
        metric    = "RMSE",
        weights   = weights,
        method    = "nnet",
        linout    = TRUE,
        tuneGrid  = tune_grid,
        preProc   = "range", # preprocessParams,
        trControl = traincotrlParams,
        trace     = FALSE,
        na.action = na.omit
        )

      ## get predicted values from cross-validation resamples, take mean across repetitions
      df_cv <- nn$pred %>%
        as_tibble() %>%
        dplyr::filter(size == nn$bestTune$size, decay == nn$bestTune$decay) %>%
        separate(Resample, into = c("fold", "rep")) %>%
        group_by(size, decay, rowIndex) %>%
        summarise(pred = mean(pred), obs = mean(obs)) %>%
        dplyr::rename(idx = rowIndex)

      nn$pred <- NULL

    } else {
      df_cv <- NULL
    }

    if (do_predict){
      ## make predictions (on same data as used for training)
      vals <- as.vector( predict( nn, data ) )

      if (downscale){ vals <- vals * 1e6 }

      ## create data frame with predictions on all data using final model, trained on all data
      df_all <- tibble(idx = seq(nrow(data)),
                       pred = vals,
                       obs = data[[ nam_target ]])

    } else {
      df_all <- NULL
    }


  } else {

    rlang::abort("predict_nn(): No other training package implemented than nnet.")

  }

  return( list( nn = nn, df_cv = df_cv, df_all = df_all ) )

}
