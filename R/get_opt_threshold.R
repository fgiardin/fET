get_opt_threshold <- function(df_profile_performance){
  out <- purrr::map(df_profile_performance, "df_metrics") %>% 
    bind_rows(.id = "threshold") %>% 
    arrange(-diff_ratio_dry_moist) %>% 
    slice(1:3) %>% 
    arrange(rmse_nnpot_nnact_moist) %>% 
    slice(1) %>% 
    pull(threshold) %>% 
    as.numeric()
  return(out)
}

