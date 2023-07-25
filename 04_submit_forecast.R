##' run aquatic forecast into the future
##' @param model site-specific list of forecast models
##' @param met_forecast weather forecast dataframe
##' @param site_data dataframe of site metadata
##' @return dataframe in EFI standard format
run_forecast <- function(model,met_forecast,site_data){
  
  forecast <- NULL
  sites <- names(model)
  
  for(i in 1:length(sites)){
    
    # Get site information for elevation
    site_info <- site_data %>% filter(field_site_id == sites[i]) 
    
    met_future_site <- met_future |> 
      filter(site_id == sites[i])
    
    if(!is.null(model[[i]])){
      
      #use model to forecast water temperature for each ensemble member
      forecasted_temperature <- predict(model[[i]],met_future_site)
      
      #use forecasted temperature to predict oyxgen by assuming that oxygen is saturated.  
      forecasted_oxygen <- rMR::Eq.Ox.conc(forecasted_temperature, 
                                           elevation.m = site_info$field_mean_elevation_m, 
                                           bar.press = NULL, 
                                           bar.units = NULL,
                                           out.DO.meas = "mg/L",
                                           salinity = 0, 
                                           salinity.units = "pp.thou")
      ## organize outputs
      temperature <- tibble(datetime = met_future_site$date,
                            site_id = sites[i],
                            parameter = met_future_site$parameter,
                            prediction = forecasted_temperature,
                            variable = "temperature")
      
      oxygen <- tibble(datetime = met_future_site$date,
                       site_id = sites[i],
                       parameter = met_future_site$parameter,
                       prediction = forecasted_oxygen,
                       variable = "oxygen")
      
      
      #Build site level dataframe.
      forecast <- dplyr::bind_rows(forecast, temperature, oxygen)
      
    }
    
  }
  
  ## reorganize into EFI standard
  forecast <- forecast |> 
    mutate(reference_datetime = forecast_date,
           family = "ensemble") |> 
    select(reference_datetime, datetime, site_id, family, parameter, variable, prediction)
  
  return(forecast)
}

```{r}
##' Save forecast and metadata to file, submit forecast to EFI
##' @param forecast dataframe
##' @param team_info list, see example
##' @param submit boolean, should forecast be submitted to EFI challenge
submit_forecast <- function(forecast,team_info,submit=FALSE){
  
  #Forecast output file name in standards requires for Challenge.  
  # csv.gz means that it will be compressed
  forecast_file <- paste0("aquatics","-",min(forecast$datetime),"-",team_info$team_name,".csv.gz")
  
  #Write csv to disk
  write_csv(forecast, forecast_file)
  
  #Confirm that output file meets standard for Challenge
  neon4cast::forecast_output_validator(forecast_file)
  
  # Generate metadata
  model_metadata = list(
    forecast = list(
      model_description = list(
        forecast_model_id =  system("git rev-parse HEAD", intern=TRUE), ## current git SHA
        name = "Air temperature to water temperature linear regression plus assume saturated oxygen", 
        type = "empirical",  
        repository = "https://github.com/ecoforecast/EF_Activities"   ## put your REPO here *******************
      ),
      initial_conditions = list(
        status = "absent"
      ),
      drivers = list(
        status = "propagates",
        complexity = 1, #Just air temperature
        propagation = list( 
          type = "ensemble", 
          size = 31) 
      ),
      parameters = list(
        status = "data_driven",
        complexity = 2 # slope and intercept (per site)
      ),
      random_effects = list(
        status = "absent"
      ),
      process_error = list(
        status = "absent"
      ),
      obs_error = list(
        status = "absent"
      )
    )
  )
  
  #  TODO: needs updating because this function has been depricated  
  #  metadata_file <- neon4cast:::generate_metadata(forecast_file, team_info$team_list, model_metadata)
  
  if(submit){
    neon4cast::submit(forecast_file = forecast_file, ask = FALSE)
  }
  
}
```