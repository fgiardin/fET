gapfill_nn <- function( df, predictors, nam_target, package="neuralnet" ){
  ##--------------------------------------------------------------------
  ## Gap-fill using NN 
  ##
  ## Arguments:
  ## df: df frame with gaps in column corresponding to nam_target
  ## predictors: name of columns in 'df' used as input to the NN
  ## nam_target: name of the column in 'df' used as target for the NN training
  ##--------------------------------------------------------------------
  ## retain only necessary ones
  df_date <- df %>% select(date)
  df <- df %>% dplyr::select(one_of(predictors, nam_target))
  
  if (package=="neuralnet"){

    if (!requireNamespace("neuralnet", quietly = TRUE)) rlang::abort("Please, install 'neuralnet' package")

    ##--------------------------------------------------------------------
    ## Using package neuralnet
    ##--------------------------------------------------------------------

    ## identify NAs
    na_idxs <- which( is.na( df$le_f_mds ) )
    
    ## for training use df with NAs removed
    train <- df[ -na_idxs, ]

    ## scale variables to within [0,1]
    maxs       <- apply(train, 2, max) 
    mins       <- apply(train, 2, min)
    train_     <- as.data.frame( scale( train, center = mins, scale = maxs - mins ) )
    df_        <- as.data.frame( scale( df,  center = mins, scale = maxs - mins ) )

    ## train the NN
    forml  <- as.formula(  paste( nam_target, "~", paste( predictors, collapse=" + " ) ) )

    nn <- neuralnet( forml, data=train_, hidden=6, linear.output=TRUE, lifesign="full", threshold=0.02, rep=1 )
    # nn <- repeat_neuralnet( forml, train_, test_, predictors, hidden=6, linear.output=TRUE, lifesign=lifesign, threshold=threshold, rep=nrep )

    ## predicting
    pred_nn_ <- try( compute( nn, subset( df_, select=predictors ) ) )

    if (class(pred_nn_)!="try-error"){
      
      ## scaling back
      range   <- max( train[[ nam_target ]] ) - min( train[[ nam_target ]] )
      offset  <- min( train[[ nam_target ]] )
      pred_nn <- pred_nn_$net.result * range + offset

    }

    # ## plot for verification
    # plot( seq(nrow(df)), df[[ nam_target]], type="l" ) ## with gaps
    # # lines( na_idxs, pred_nn[na_idxs], col='red' )  ## gap-filled
    # lines( seq(nrow(df)), pred_nn, col='red' )  ## gap-filled

    ## fill gaps
    df[[ nam_target ]][ na_idxs ] <- pred_nn[ na_idxs ]


  } else if (package=="caret"){

    if (!requireNamespace("caret", quietly = TRUE) | !requireNamespace("nnet", quietly = TRUE)) rlang::abort("Please, install 'caret' and 'nnet' packages")

    ##--------------------------------------------------------------------
    ## Using package caret
    ##--------------------------------------------------------------------

    # ## some predictors may be NA. Approximate by linear interpolation.
    # for (ipred in predictors){
    # 
    #   ## if first value is NA, fill with second
    #   if ( is.na(df[[ ipred ]][1]) ) { df[[ ipred ]][1] <- df[[ ipred ]][2] }
    # 
    #   ## if last value is NA then fill with second-last
    #   if ( is.na(df[[ ipred ]][nrow(df)]) ) { df[[ ipred ]][nrow(df)] <- df[[ ipred ]][nrow(df)-1] }
    # 
    #   ## fill NAs in between time series (linear interpolation)
    #   df[[ ipred ]][ which(is.na(df[[ ipred ]])) ] <- approx( 1:nrow(df), df[[ ipred ]] )$y[ which(is.na(df[[ ipred ]])) ]
    # 
    # }

    ## this has caused a problem before due to values being too high -> weird
    norm_target <- mean(df[[ nam_target ]], na.rm=TRUE)
    df[[ nam_target ]] <- df[[ nam_target ]] / norm_target

    ## identify NAs to be filled in target variable
    na_idxs <- which( is.na( df[[ nam_target ]] ) )

    ## for training use df with all NAs removed in predictors and target variable
    train <- df %>% tidyr::drop_na()
    #train <- df[ -na_idxs, ]

    ## Scale by range
    preprocessParams <- caret::preProcess( train, method=c("range") )

    ## take best of 10 repetitions of training with 75% used for training (25% for testing)
    traincotrlParams <- caret::trainControl( method="repeatedcv", number=10, repeats=3, verboseIter=FALSE, p=0.75 )

    forml  <- as.formula(  paste( nam_target, "~", paste( predictors, collapse=" + " ) ) )
    nn <- caret::train(
                forml,
                data      = train, #training,
                method    = "nnet",
                linout    = TRUE,
                tuneGrid  = expand.grid( .decay = c(1e-3), .size = c(20) ),
                preProc   = "range", # c("center", "scale"), # "range", # preProc  = preprocessParams
                trControl = traincotrlParams,
                trace     = FALSE
                )

    pred_nn <- as.vector( predict( nn, df ) )   
    # pred_nn <- pred_nn * 1e6 
    
    # ## plot for verification
    # plot( seq(nrow(df)), df[[ nam_target]], type="n" ) ## with gaps
    # lines( seq(nrow(df)), pred_nn, col='red' )  ## gap-filled
    # lines( seq(nrow(df)), df[[ nam_target]] )  ## with gaps

    ## fill gaps
    df[[ nam_target ]][ na_idxs ] <- pred_nn[ na_idxs ]

    ## revert normalisation
    df[[ nam_target ]] <- df[[ nam_target ]] * norm_target

  }

  df <- df %>% bind_cols(df_date, .)
  return( df )

}
