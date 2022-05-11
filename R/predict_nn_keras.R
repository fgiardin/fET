predict_nn_keras <- function(df, 
                             nam_target, 
                             predictors, 
                             nfolds,
                             num_epochs,
                             batch_size,
                             val_size,
                             learning_rate,
                             num_layers,
                             num_units_per_layer,
                             nn = NULL,
                             pp = NULL,
                             dropout_rate,
                             print_model_summary = FALSE){
  
  out <- list()
  
  
  if (identical(NULL, nn)){ # if we don't provide a previous model through nn argument
    ## First shuffle the dataset
    # df_shffld <- df[sample(nrow(df)),]
    
    #do NOT shuffle (otherwise: auto-correlation; values are not temporally independent)
    df_shffld <- df
    
    ## get indexes
    # ind <- sample(2, nrow(df_shffld), replace=TRUE, prob = c(prop,(1-prop))) #per ogni row: 1 se è traning e 2 se testing
    # idx <- which(ind == 1) #get the positions of the training points (dove sono le righe "1")
    
    ## k-fold CV
    folds_idx <- caret::createFolds(seq(nrow(df)), k = nfolds, list = TRUE, returnTrain = FALSE) #lista con 5 vettori con indexes
    
    ##---------------------------------------
    ## Train, given indices for training
    ##---------------------------------------
    ## get results from cross-validation
    out_cv <- purrr::map(folds_idx, 
                         ~predict_nn_keras_byfold(df_shffld, 
                                                  nam_target, 
                                                  predictors, 
                                                  idx = ., 
                                                  num_epochs,
                                                  batch_size,
                                                  val_size,
                                                  learning_rate,
                                                  num_layers,
                                                  num_units_per_layer,
                                                  dropout_rate,
                                                  print_model_summary = FALSE))
    
    ## extract and combine df_cv (validation result) from each fold
    df_cv <- out_cv %>% purrr::map("df_cv") %>% 
      bind_rows()
    out$df_cv <- df %>%
      dplyr::select(settings$rowid, settings$target) %>% 
      left_join(df_cv %>% 
                  dplyr::select(settings$rowid, pred), 
                by = settings$rowid)
    
    ##---------------------------------------
    ## Predict with trained model on all data
    ##---------------------------------------
    ## get results from model trained on all data
    out_all <- predict_nn_keras_byfold(df_shffld, 
                                       nam_target, 
                                       predictors, 
                                       idx = c(), # no validation data, fit on all
                                       num_epochs,
                                       batch_size,
                                       val_size,
                                       learning_rate,
                                       num_layers,
                                       num_units_per_layer,
                                       dropout_rate,
                                       print_model_summary = FALSE
    )
    
    ## retain model trained on all for output and pre-processing params
    nn <- out_all$nn
    pp <- out_all$pp
    
  }
  
  ## predict on all data with model trained on all data
  df_all <- df %>% dplyr::select(one_of(predictors))
  df_all <- predict(pp, df_all)    # transform the test data to center and scale it
  df_all <- as.matrix(df_all)
  vec_pred_all <- predict(nn, df_all)
  df_all <- as_tibble(df_all)
  df_all$pred <- as.vector(vec_pred_all)
  df_all[[settings$rowid]] <- df[[settings$rowid]]
  
  ## construct data frame with results on all data
  out$df_all <- df %>%
    dplyr::select(settings$rowid, settings$target) %>% 
    left_join(df_all %>% 
                dplyr::select(settings$rowid, pred), 
              by = settings$rowid)
  
  ## return model and preprocessing params
  out$nn <- nn
  out$pp <- pp
  
  return(out)
  }

predict_nn_keras_byfold <- function(df, 
                                    nam_target, 
                                    predictors, 
                                    idx, 
                                    num_epochs, 
                                    batch_size, 
                                    val_size,
                                    learning_rate,
                                    num_layers,
                                    num_units_per_layer,
                                    dropout_rate,
                                    print_model_summary = FALSE
){
  
  if (length(idx) > 0){
    ## Use the indexes to get test and train splits
    df_train <- df[-idx, ]  ## include all columns
    df_test <- df[idx, ]  ## include all columns
  } else {
    df_train <- df
    df_test <- tibble()
  }    
  
  ##---------------------------------------
  ## Data preparation
  ##---------------------------------------
  
  ## Separate the splits to get train_data, train_target, test_data and test_target. 
  train_target <-  df_train %>% dplyr::select(nam_target)
  df_train <- df_train %>% dplyr::select(one_of(predictors)) # "one_of" take only variables specified in string file (here: predictors)
  
  # test_target <- df_test %>% dplyr::select(nam_target)
  # df_test <- df_test %>% dplyr::select(one_of(predictors))
  
  ## scale and center each of the columns
  pp_stats <- caret::preProcess(df_train, method = c("center","scale"))    # get the statistics (mean, variance, etc) of numeric cols
  df_train_pp <- predict(pp_stats, df_train)  # transform the train data to center and scale it
  # df_test_pp <- predict(pp_stats, df_test)  # transform the test data to center and scale it (using train data stats, to avoid leakage)
  
  ##---------------------------------------
  ## Model definition
  ##---------------------------------------

  # build model
  model <- keras_model_sequential() %>% 
    layer_dense(units = num_units_per_layer[1], activation = "relu", input_shape = ncol(df_train_pp)) 
    #layer_batch_normalization() %>% # normalize after every transformation performed by the network (improve performance)
    #layer_dropout(rate = dropout_rate[1]) # randomly set some weights to 0 to reduce overfitting
    
  # Add the hidden layers. Note this requires just a simple for loop that calls the function "layer_dense"
  if (num_layers>1){
    for (i in 1:(num_layers-1)){
      model = model %>%
        layer_dense(units = num_units_per_layer[i+1], activation = "relu") 
        #layer_batch_normalization() %>%
        #layer_dropout(rate = dropout_rate[i+1])
    }
  }
  
  # Add the output layer
  model = model %>% 
    layer_dense(units = 1, activation = "linear") # normally linear activation function for output (regression problem)
                                                                  # only 1 unit here because we predict one value at a time 
  
  # Print the model description
  if (print_model_summary){
    summary(model)
  }
  
  # Compile the model, defining loss and metric to measure during training
  model %>% 
    compile(
      loss = 'mse', # mse for regression problem
      #optimizer = optimizer_rmsprop(),
      optimizer_adam(lr = learning_rate), #learning rate for stochastic gradient descent
        
      #optimizer_rmsprop(),
      metrics = list('mean_absolute_percentage_error') # or 'mae'
      # c('accuracy')
    ) #list('accuracy'))
  
  set.seed(1982)
  
  ##---------------------------------------
  ## Model training
  ##---------------------------------------
  history = model %>% 
    fit(
      x                = df_train_pp %>% as.matrix(), # convert df to matrix to feed the model 
      y                = train_target %>% as.matrix(),
      batch_size       = batch_size,
      validation_split = val_size, 
      epochs = num_epochs,
      #shuffle = TRUE, # do not shuffle
      callbacks = list(
        #callback_reduce_lr_on_plateau(factor = 0.1) # to find global minimum (instead of local)
        callback_early_stopping(monitor = "val_loss", patience = 5)
        ),
      verbose = FALSE # plot the output
    )
  
  ##---------------------------------------
  ## Predict with trained model on test set only
  ##---------------------------------------
  if (length(idx) > 0){
    df_test <- df_test %>% dplyr::select(one_of(predictors))
    df_test <- predict(pp_stats, df_test)    # scale test data USING STATS FROM THE TRAINING
    df_test <- as.matrix(df_test)
    vec_pred <- predict(model, df_test) # same function as for applying scaling transform
    
    ## construct data frame with test results
    df_cv <- df[idx, ] %>%    # confusing to call CV here: there is no validation split
      dplyr::select(-one_of(c(predictors))) %>% 
      mutate(pred = as.vector(vec_pred))
  } else {
    df_cv <- NULL
  }
  
  
  return(list(nn = model, 
              df_cv = df_cv,
              pp = pp_stats
  ))
  
}
