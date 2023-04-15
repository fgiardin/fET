#' Reads a NetCDF file
#'
#' Reads the full content of a NetCDF file, given only the file name
#'
#' @param filn A character string specifying the full path of the NetCDF
#' file to be read.
#' @param varnam A character string specifying the variable name of the NetCDF file.
#' Can also be a vector of character strings. Defaults to \code{NA}, that is: all
#' available variables are read.
#' @param date_origin A character string of format \code{"YYYY-MM-DD"}
#' specifying the origin, day 0, of the time values provided in the NetCDF file.
#' @param time_is_years A logical specifying whether the values provided by dimension
#' \code{'time'} is years. Defaults to \code{FALSE}.
#' @param ignore_time A logical specifying whether file has a time dimension Use this to
#' ignore if it has a time dimension of length 1 by \code{has_time=TRUE}. Defaults to
#' \code{FALSE}.
#' @param check_flip A logical specifying whether order of latitude values should be checked.
#'
#' @return A list, containing \code{"lon"} (vector of longitudes of
#' gridcell mid-points), \code{"lat"} (vector of latitudes of gridcell
#' mid-points), \code{"time"} (vector of lubridate::ymd dates),
#' \code{"varnams"} (a vector of all variable names as strings), and a
#' named (nested) list of the data arrays (lon x lat x time) for each
#' variable.
#' @export
#'
read_nc_onefile <- function(filn, varnam = NA, date_origin = NA, time_is_years = FALSE, ignore_time = FALSE,
                            check_flip = FALSE){
  
  require(dplyr)
  
  nc <- ncdf4::nc_open(filn)
  # Save the print(nc) dump to a text file
  {
    sink(paste0(filn, ".txt"))
    print(nc)
    sink()
    unlink(paste0(filn, ".txt"))
  }
  
  ## get names of longitude and latitude dimensions
  dimnames <- ls(nc$dim)
  if (!("lon" %in% dimnames)){
    if ("LON" %in% dimnames){
      lonname <- "LON"
    } else if ("longitude" %in% dimnames){
      lonname <- "longitude"
    } else if ("Longitude" %in% dimnames){
      lonname <- "Longitude"
    } else if ("LONGITUDE" %in% dimnames){
      lonname <- "LONGITUDE"
    } else if ("x" %in% dimnames){
      lonname <- "x"
    }
  } else {
    lonname <- "lon"
  }
  
  if (!("lat" %in% dimnames)){
    if ("LAT" %in% dimnames){
      latname <- "LAT"
    } else if ("latitude" %in% dimnames){
      latname <- "latitude"
    } else if ("Latitude" %in% dimnames){
      latname <- "Latitude"
    } else if ("LATITUDE" %in% dimnames){
      latname <- "LATITUDE"
    } else if ("y" %in% dimnames){
      latname <- "y"
    }
  } else {
    latname <- "lat"
  }
  
  if (!("time" %in% dimnames)){
    if ("TIME" %in% dimnames){
      timename <- "TIME"
    } else if ("Time" %in% dimnames){
      timename <- "Time"
    }
  } else {
    timename <- "time"
  }
  
  
  if (!any(c("TIME", "time", "Time") %in% names(nc$dim)) || ignore_time){
    ## no time dimension
    out <- list(
      lon = ncdf4::ncvar_get(nc, nc$dim[[lonname]]$name),
      lat = ncdf4::ncvar_get(nc, nc$dim[[latname]]$name)
    )
    
  } else {
    ## with time dimension
    out <- list(
      lon = ncdf4::ncvar_get(nc, nc$dim[[lonname]]$name),
      lat = ncdf4::ncvar_get(nc, nc$dim[[latname]]$name),
      time = ncdf4::ncvar_get(nc, nc$dim[[timename]]$name)
    )
    
    ## Conversion to ymd object requires out$time to be integer
    ## usually it is, but in some cases it's not (e.g. when
    ## cdo timmean is applied before).
    ## Round down.
    out$time <- floor(out$time)
    
    ## convert to date
    if (time_is_years){
      
      ## interpret time as first of january
      out$time <- lubridate::ymd(paste0(out$time, "-01-01"))
      
    } else if (!is.na(date_origin)){
      
      time_origin <- lubridate::ymd(date_origin)
      out$time <- lubridate::days(out$time) + time_origin
      
    } else {
      
      ## get time units
      units_long <- ncmeta::nc_atts(filn, timename) %>%
        tidyr::unnest(cols = c(value)) %>%
        dplyr::filter(name == "units") %>%
        dplyr::pull(value)
      
      if (stringr::str_detect(units_long, "days since")){
        time_origin <- units_long %>%
          stringr::str_remove("days since ") %>%
          stringr::str_remove(" 00:00:00") %>%
          stringr::str_remove(" 0:0:0") %>%
          lubridate::ymd()
        
        out$time <- time_origin + lubridate::days(out$time)
        
      } else if (stringr::str_detect(units_long, "seconds since")){
        time_origin <- units_long %>%
          stringr::str_remove("seconds since ") %>%
          lubridate::ymd_hms()
        
        out$time <- time_origin + lubridate::seconds(out$time)
        
      }
      
      
      # if (nc$dim[[timename]]$units=="days since 2001-1-1 0:0:0"){
      #
      #   out$time <- conv_noleap_to_ymd(out$time, origin = lubridate::ymd("2001-01-01"))
      #
      # } else if (nc$dim[[timename]]$units=="days since 2000-01-01"){
      #
      #   time_origin <- lubridate::ymd("2000-01-01")
      #   out$time <- lubridate::days(out$time) + time_origin
      #
      # } else if (nc$dim[[timename]]$units=="days since 2001-01-01"){
      #
      #   time_origin <- lubridate::ymd("2001-01-01")
      #   out$time <- lubridate::days(out$time) + time_origin
      #
      # } else if (nc$dim[[timename]]$units=="days since 1900-1-1"){
      #
      #   time_origin <- lubridate::ymd("1900-01-01")
      #   out$time <- lubridate::days(out$time) + time_origin
      #
      # } else if (nc$dim[[timename]]$units=="days since 1970-01-01 00:00:00"){
      #
      #   time_origin <- lubridate::ymd("1970-01-01")
      #   out$time <- lubridate::days(out$time) + time_origin
      #
      # } else if (nc$dim[[timename]]$units=="years"){
      #
      #   ## interpret time as first of january
      #   out$time <- lubridate::ymd(paste0(out$time, "-01-01"))
      #
      # } else {
      #
      #   rlang::abort(paste("units of time not recognized for file", filn))
      #
      # }
      
    }
    
  }
  
  # get variables
  if (is.na(varnam)) varnam <- ls(nc$var)
  getvar <- function(varnam){
    #tibble( !!varnam := ncdf4::ncvar_get(nc, varnam) )
    out <- list()
    out[[varnam]] <- ncdf4::ncvar_get(nc, varnam)
  }
  vars <- purrr::map(as.list(varnam), ~getvar(.)) %>%
    setNames(varnam)
  
  nc$var[[varnam[1]]]$units
  
  out[["vars"]] <- vars
  out[["varnams"]] <- varnam
  
  if (check_flip){
    if (length(out$lat)>1){
      if (out$lat[1]>out$lat[2]){
        ## Flip latitudes
        out <- nc_flip_lat(out)
      }
    }
  }
  
  return(out)
}

nc_flip_lat <- function(nc){
  
  nc$lat <- rev(nc$lat)
  
  # nlat <- length(nc$lat)
  # nc$vars[[1]] <- nc$vars[[1]][,nlat:1]
  
  arr_flip_lat <- function(arr){
    nlat <- dim(arr)[2]
    arr <- arr[,nlat:1]
    return(arr)
  }
  nc$vars <- purrr::map(nc$vars[1], ~arr_flip_lat(.))
  
  return(nc)
}