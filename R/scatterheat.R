scatterheat <- function(df, X, Y, title, path){

  df <- df %>%
    as_tibble() %>%
    ungroup() %>%
    dplyr::select(X=X, Y=Y) %>%
    tidyr::drop_na(X, Y)

  # linear regression
  lm_model <- lm(Y ~ X , data=df, na.action = na.omit)
  slope <- coef(lm_model)["X"]
  intercept <- coef(lm_model)["(Intercept)"]

  # calculate metrics
  sd_X = sd(df$X, na.rm = TRUE) #calculate standardised slope
  sd_Y = sd(df$Y, na.rm = TRUE)
  sd_slope = lm_model$coefficients[2]*(sd_X/sd_Y) # multipliant par le rapport des std dev

  r2 = df %>% na.omit %>% yardstick::rsq(Y,X) # R2 - to double check
  rmse = df %>% na.omit %>% yardstick::rmse(Y,X)
  N = nrow(lm_model[["model"]])

  # # to double check calculate R2 in two ways (from model and from yardstick)
  # subtitle = sprintf("R2 = %.3f (%.3f), RMSE = %.3f, Slope = %.3f, N = %1.0f", summary(lm_model)$r.squared, r2$.estimate, rmse$.estimate, sd_slope, N)

  # calculate R2 with yardstick (both methods above are consistent)
  # subtitle = sprintf("R2 = %.2f, RMSE = %.2f", r2$.estimate, rmse$.estimate)
  subtitle <- bquote(italic(R^2)~" = " ~ .(round(r2$.estimate, 2)) ~ ", RMSE = " ~ .(round(rmse$.estimate, 2)))

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

  # density plot on ggplot
  a <- heatscatter(x = df$X, y = df$Y, ggplot = TRUE)
  a <- a +
    labs(
      y = axisY,
      x = axisX,
      title = title,
      subtitle = subtitle
    ) +
    scale_y_continuous(breaks = c(0, 2, 4, 6), limits = c(-0.1,6.3), expand = c(0, 0)) +
    scale_x_continuous(breaks = c(0, 2, 4, 6), limits = c(-0.1,6.3), expand = c(0, 0)) +
    theme_classic() +
    theme(
      axis.text=element_text(size = 14),
      axis.title=element_text(size = 14),
      legend.text=element_text(size=14),
      plot.title = element_text(hjust = 0.5, size = 16), # center and bold title
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      axis.line=element_blank(), # remove axis line (otherwise overlap with box)
      plot.subtitle=element_text(hjust=0.5),
      axis.ticks.length = unit(0.25, "cm"), # longer ticks
      panel.border = element_rect(  # box around figure
        color = "black",
        fill = NA,
        size = 1),
    ) +
    geom_abline(slope = slope, intercept = intercept, color = "red") +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed")


  return(a = a)
}
