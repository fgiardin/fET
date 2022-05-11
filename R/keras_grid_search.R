#' Script to launch grid search of hyperparameters 
#'
#'
#'Before running: define settings and prepare_trainingdata_fvar.R

train_test_model <- function(hparams) {
  
  # Data Preparation ---------------------------------------------------
  
  # get test and train splits [sample without shuffling]
  train_split <- df_train[1:ceiling(0.8*nrow(df_train)), ] # take first 80% of the data
  test_split <- df_train[(ceiling(0.8*nrow(df_train))+1):nrow(df_train), ] # remaining 20%
  
  ## Separate the splits to get train_data, train_target, test_data and test_target. 
  train_target <-  train_split %>% dplyr::select(settings$target)
  train_data <- train_split %>% dplyr::select(one_of(settings$predictors)) # "one_of" take only variables specified in string file (here: predictors)
  
  test_target <- test_split %>% dplyr::select(settings$target)
  test_data <- test_split %>% dplyr::select(one_of(settings$predictors))
  
  ## scale and center each of the columns
  train_data_stats <- caret::preProcess(train_data, method = c("center","scale"))    # get the statistics (mean, variance, etc) of numeric cols
  df_train_pp <- predict(train_data_stats , train_data)  # transform the train data to center and scale it
  df_test_pp <- predict(train_data_stats , test_data)  # transform the test data to center and scale it (using train data stats, to avoid leakage)
  
  # convert to matrix (needed for keras)
  
  
  # Define Model --------------------------------------------------------------
  
  model <- keras_model_sequential() %>% 
    layer_dense(py_to_r(hparams[HP_NUM_UNITS]), activation = "relu", input_shape = ncol(df_train_pp)) %>% 
    layer_batch_normalization() %>% # normalize after every transformation performed by the network (improve performance)
    layer_dropout(py_to_r(hparams[HP_DROPOUT])) %>% 
    layer_dense(py_to_r(hparams[HP_NUM_UNITS]), activation = "relu") %>% 
    layer_batch_normalization() %>% 
    layer_dropout(py_to_r(hparams[HP_DROPOUT])) %>% 
    layer_dense(py_to_r(hparams[HP_NUM_UNITS]), activation = "relu") %>% 
    layer_batch_normalization() %>% 
    layer_dropout(py_to_r(hparams[HP_DROPOUT])) %>% 
    layer_dense(units = 1, activation = "linear") #output
    
  # # flexible number of layers
  # model <- keras_model_sequential()
  # 
  # # build model
  # model = model %>% 
  #   layer_dense(units = settings$num_units_per_layer[1], activation = "relu", input_shape = ncol(df_train_pp)) %>% 
  #   layer_batch_normalization() %>% # normalize after every transformation performed by the network (improve performance)
  #   layer_dropout(rate = 0.2) # randomly set some weights to 0 to reduce overfitting
  # 
  # # Add the hidden layers. Note this requires just a simple for loop that calls the function "layer_dense"
  # if (num_layers>1){
  #   for (i in 1:(num_layers-1)){
  #     model = model %>% 
  #       layer_dense(units = settings$num_units_per_layer[i+1], activation = "relu") %>% 
  #       layer_batch_normalization() %>% 
  #       layer_dropout(rate = 0.2)
  #   }
  # }
  # 
  # # Add the output layer
  # model = model %>% 
  #   layer_dense(units = 1, activation = "linear") # normally linear activation function for output (regression problem)
  # # only 1 unit here because we predict one value at a time 
  
  # Print the model description
  if (settings$print_model_summary){
    summary(model)
  }
  
  # Compile the model, defining loss and metric to measure during training
  model %>% 
    compile(
      loss = 'mse', # mse for regression problem
      optimizer = py_to_r(hparams[HP_OPTIMIZER]),
      #optimizer_rmsprop(), # source: hands-on ML (default one is good enough)
      #optimizer_adam(lr = learning_rate), #learning rate for stochastic gradient descent
      metrics = list('mae')
      # c('accuracy')
    ) #list('accuracy'))
  
  
  # Training ----------------------------------------------------
  
  history = model %>% 
    fit(
      x                = df_train_pp %>% as.matrix(), # convert df to matrix to feed the model 
      y                = train_target %>% as.matrix(),
      batch_size       = settings$batch_size,
      validation_split = settings$val_size, 
      epochs = settings$num_epochs,
      #shuffle = FALSE, # do not shuffle
      callbacks = list(
        callback_early_stopping(patience = 5),
        #callback_early_stopping(monitor = "val_loss",  patience = 5),
        callback_reduce_lr_on_plateau(factor = 0.001), # to find global minimum (instead of local)
        callback_tensorboard(logdir), # log metrics
        hp$KerasCallback(logdir, hparams), # log hparams
      ),
      verbose = FALSE # plot the output
    )
  
  #plot(history)
  
  # Evaluation ----------------------------------------------------
  results <- model %>% evaluate(
    x = test_data,
    y = test_target, 
    verbose = 0
  )
  
  results$mae

}
# cat('Test loss:', score$loss, '\n')
# cat('Test accuracy:', score$acc, '\n')