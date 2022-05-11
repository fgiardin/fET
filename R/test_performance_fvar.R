test_performance_fvar <- function(out, settings){
  
  # ## rename soil moisture column to 'soilm'
  # df$soilm <- df[[ settings$varnams_soilm ]]
  
  ## 1. NNact has no systematic bias related to the level of soil moisture.
  out$df_all <- out$df_all %>% 
    mutate(bias_act = nn_act - obs,
           bias_pot = nn_pot - obs,
           soilm_bin = cut(soilm, 10),
           ratio_act = nn_act / obs,
           ratio_pot = nn_pot / obs
    ) 
  
  gg_boxplot_bias_vs_soilm <- out$df_all %>% 
    tidyr::pivot_longer(cols = c(bias_act, bias_pot), names_to = "source", values_to = "bias") %>% 
    ggplot(aes(x = soilm_bin, y = bias, fill = source)) +
    geom_boxplot() +
    geom_hline(aes(yintercept = 0.0), linetype = "dotted") +
    labs(title = "Bias vs. soil moisture")
  
  ## test whether slope is significantly different from zero (0 is within slope estimate +/- standard error of slope estimate)
  linmod <- lm(bias_act ~ soilm, data = out$df_all)
  testsum <- summary(linmod)
  slope_mid <- testsum$coefficients["soilm","Estimate"]
  slope_se  <- testsum$coefficients["soilm","Std. Error"]
  passtest_bias_vs_soilm <- ((slope_mid - slope_se) < 0 && (slope_mid + slope_se) > 0)
  
  ## record for output
  df_out <- tibble(slope_bias_vs_soilm_act = slope_mid, passtest_bias_vs_soilm = passtest_bias_vs_soilm)
  
  gg_points_bias_vs_soilm <- out$df_all %>% 
    ggplot(aes(x = soilm, y = bias_act)) +
    geom_point() +
    geom_smooth(method = "lm")
  
  ## 2. NNpot and NNact have no bias during moist days.
  gg_boxplot_bias_moisdays <- out$df_all %>% 
    tidyr::pivot_longer(cols = c(bias_act, bias_pot), names_to = "source", values_to = "bias") %>% 
    dplyr::filter(moist) %>% 
    ggplot(aes(y = bias, fill = source)) +
    geom_boxplot() +
    geom_hline(aes(yintercept = 0), linetype = "dotted")
  
  ## get values of bias during moist days for NNact and NNpot  
  df_bias_moist <- out$df_all %>% 
    dplyr::filter(moist) %>%
    summarise(bias_moist_act = mean(bias_act, na.rm = TRUE), bias_moist_pot = mean(bias_pot, na.rm = TRUE))
  
  ## record for output
  df_out <- df_out %>% 
    bind_cols(., df_bias_moist)
  
  ## 3. NNpot and NNact have a high R2 and low RMSE during moist days.
  # out_modobs_act_pot <- out$df_cv %>% 
  #   dplyr::filter(moist) %>% 
  #   rbeni::analyse_modobs2("nn_pot", "nn_act", type = "heat")
  
  # df_out <- df_out %>%
  #   mutate(nnpot_vs_nnact_moist_rsq   = out_modobs_act_pot$df_metrics %>% filter(.metric=="rsq")   %>% pull(.estimate),
  #          nnpot_vs_nnact_moist_rmse  = out_modobs_act_pot$df_metrics %>% filter(.metric=="rmse")  %>% pull(.estimate),
  #          nnpot_vs_nnact_moist_slope = out_modobs_act_pot$df_metrics %>% filter(.metric=="slope") %>% pull(.estimate))

  ## 4. Fit of NN$_\text{act}$ vs. observed (target) values.
  # out_modobs <- out$df_cv %>% rbeni::analyse_modobs2("nn_act", "obs", type = "heat")
  
  # df_out <- df_out %>% 
  #   mutate(mod_vs_obs_rsq   = out_modobs$df_metrics %>% filter(.metric=="rsq")   %>% pull(.estimate),
  #          mod_vs_obs_rmse  = out_modobs$df_metrics %>% filter(.metric=="rmse")  %>% pull(.estimate),
  #          mod_vs_obs_slope = out_modobs$df_metrics %>% filter(.metric=="slope") %>% pull(.estimate))
  
  ## Metrics for use as in Stocker et al. (2018)
  ## 1. Determine the difference in the bias of NNpot between dry and moist days
  df_ratio_moist <- out$df_all %>% 
    dplyr::filter(moist) %>%
    summarise(ratio_act = median(ratio_act, na.rm = TRUE), ratio_pot = median(ratio_pot, na.rm = TRUE))
  
  df_ratio_dry <- out$df_all %>% 
    dplyr::filter(!moist) %>%
    summarise(ratio_act = median(ratio_act, na.rm = TRUE), ratio_pot = median(ratio_pot, na.rm = TRUE))
  
  ## 2. Determine RMSE of NNpot vs. NNact during moist days
  rmse_nnpot_nnact_moist <- out$df_all %>% 
    dplyr::filter(moist) %>%
    yardstick::rmse(nn_act, nn_pot) %>% 
    pull(.estimate)
  
  df_out <- df_out %>% 
    mutate(
      diff_ratio_dry_moist = abs(df_ratio_dry$ratio_pot - df_ratio_moist$ratio_pot),
      rmse_nnpot_nnact_moist = rmse_nnpot_nnact_moist
    )
  
  return(list(df_metrics = df_out, 
              list_gg = list(
                gg_boxplot_bias_vs_soilm = gg_boxplot_bias_vs_soilm, 
                gg_points_bias_vs_soilm= gg_points_bias_vs_soilm,
                gg_boxplot_bias_moisdays = gg_boxplot_bias_moisdays
                #gg_modobs_nn_moist = out_modobs_act_pot$gg,
                #gg_out_modobs = out_modobs$gg
              )))
  
}
