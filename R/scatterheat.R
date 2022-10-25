scatterheat <- function(df, X, Y, title, path){
  df <- df %>%
    as_tibble() %>%
    ungroup() %>%
    dplyr::select(X=X, Y=Y) %>%
    tidyr::drop_na(X, Y)

  # linear regression
  lm_model <- lm(Y ~ X , data=df, na.action = na.omit)

  # calculate metrics
  sd_X = sd(df$X, na.rm = TRUE) #calculate standardised slope
  sd_Y = sd(df$Y, na.rm = TRUE)
  sd_slope = lm_model$coefficients[2]*(sd_X/sd_Y) # multipliant par le rapport des std dev

  r2 = df %>% na.omit %>% yardstick::rsq(Y,X) # R2 - to double check
  rmse = df %>% na.omit %>% yardstick::rmse(Y,X)
  N = nrow(lm_model[["model"]])

  # # to double check calculate R2 in two ways (from model and from yardstick)
  # subtitle = sprintf("R2 = %.3f (%.3f), RMSE = %.3f, Slope = %.3f, N = %1.0f", summary(lm_model)$r.squared, r2$.estimate, rmse$.estimate, sd_slope, N)

  # calculate R2 in one way (checked two methods above and are consistent)
  #subtitle = sprintf("R2 = %.2f, RMSE = %.2f, N = %1.0f", r2$.estimate, rmse$.estimate, N)
  subtitle = sprintf("R2 = %.2f, RMSE = %.2f", r2$.estimate, rmse$.estimate)

  # adjust axis names
  if(X == "nn_act"){
    axisX = expression(paste(ET[NN], " (mm d"^"-1"*")"))
  } else if(X == "obs"){
    axisX =  expression(paste(ET[obs], " (mm d"^"-1"*")"))
  } else if(X== "nn_pot"){
    axisX =  expression(paste(PET[NN], " (mm d"^"-1"*")"))
  } else if(X== "NETRAD_mass_coeff"){
    axisX = expression(paste(PET[lm], " (mm d"^"-1"*")"))
  } else if(X== "pet_splash_coeff"){
    axisX = expression(paste(PET[PT], " (mm d"^"-1"*")"))
  } else{
    axisX = X
  }

  if(Y == "nn_act"){
    axisY = expression(paste(ET[NN], " (mm d"^"-1"*")"))
  } else if(Y == "obs"){
    axisY = expression(paste(ET[obs], " (mm d"^"-1"*")"))
  } else if(Y == "nn_pot"){
    axisY = expression(paste(PET[NN], " (mm d"^"-1"*")"))
  } else if(Y== "NETRAD_mass_coeff"){
    axisY =  expression(paste(PET[lm], " (mm d"^"-1"*")"))
  } else if(Y== "pet_splash_coeff"){
    axisY = expression(paste(PET[PT], " (mm d"^"-1"*")"))
  } else{
    axisY = Y
  }

  # density plot
  png(filename = path, width = 4, height = 4.2, units = 'in', res = 300)
  par(mar=c(5 ,4.5,2,1)+.1, font.main = 1, cex.lab = 1.1) # The 'mar' argument of 'par' sets the width of the margins in the order: 'bottom', 'left', 'top', 'right'. The default is to set 'left' to 4
  plot.new()
  plot.window(xlim = c(min(df$X, na.rm=TRUE),max(df$X, na.rm=TRUE)),
              ylim = c(min(df$Y, na.rm=TRUE),max(df$Y, na.rm=TRUE)))
  heatscatterpoints(x = df$X, y = df$Y)
  abline(lm_model, col = 'red', lwd = 1)
  abline(c(0,1), lty=3)
  axis(1)
  axis(2)
  title(xlab = axisX, ylab = axisY, main = title, sub = subtitle, cex.sub=0.8)
  box()
  dev.off()
}
