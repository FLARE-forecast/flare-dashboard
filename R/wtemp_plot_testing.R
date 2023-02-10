### FIRST PART IS CODE FROM WHITNEY - BASED ON NETCDF INPUTS (SAVED FOR REFERENCE)

# simple_plot <- function(forecast_file_name,
#                         output_file_name,
#                         qaqc_data_directory,
#                         focal_depths_plotting,
#                         num_days_plot = config$run_config$forecast_horizon,
#                         historical_days = 5){
#
#   ####
#   pdf_file_name <- paste0(tools::file_path_sans_ext(output_file_name),".pdf")
#   csv_file_name <- paste0(tools::file_path_sans_ext(output_file_name),".csv")
#
#   output <- FLAREr::combine_forecast_observations(file_name = forecast_file_name,
#                                                   target_file = file.path(config$file_path$qaqc_data_directory, paste0(config$location$site_id, "-targets-insitu.csv")),
#                                                   extra_historical_days = 0,
#                                                   ncore = 1)
#   obs <- output$obs
#   full_time_extended <- output$full_time_extended
#   diagnostic_list <- output$diagnostic_list
#   state_list <- output$state_list
#   forecast <- output$forecast
#   par_list <- output$par_list
#   obs_list <- output$obs_list
#   state_names <- output$state_names
#   par_names <- output$par_names
#   diagnostics_names <- output$diagnostics_names
#   full_time <- output$full_time
#   obs_long <- output$obs_long
#   depths <- output$depths
#   obs_names <- output$obs_names
#
#
#   if(length(which(forecast == 1)) > 0){
#     forecast_index <- which(forecast == 1)[1]
#   }else{
#     forecast_index <- 0
#   }
#
#   if(forecast_index > 0){
#     forecast_start_day <- full_time[forecast_index-1]
#     forecast_start_day_alpha <- 1.0
#   }else{
#     forecast_start_day <- dplyr::last(full_time)
#     forecast_start_day_alpha <- 0.0
#   }
#
#   hist_dates <- seq.Date(as.Date(forecast_start_day - historical_days*25*60*60), as.Date(forecast_start_day), by = 'day')
#
#   obs_hist <- obs_long %>%
#     dplyr::filter(date %in% hist_dates) %>%
#     dplyr::filter(depth %in% focal_depths_plotting) %>%
#     dplyr::filter(hour==0)
#
#   if(length(focal_depths_plotting) < 4){
#     plot_height <- 6
#   }else{
#     plot_height <- 8
#   }
#   pdf(pdf_file_name,width = 11, height = plot_height)
#
#   evaluation_df <- NULL
#
#   for(i in 1:length(state_names)){
#
#     curr_var <- state_list[[i]]
#     message(state_names[i])
#
#
#     mean_var <- array(NA, dim = c(length(depths), length(full_time)))
#     upper_var <- array(NA, dim = c(length(depths), length(full_time)))
#     lower_var <- array(NA,dim = c(length(depths), length(full_time)))
#     sd_var <- array(NA,dim = c(length(depths), length(full_time)))
#     for(j in 1:length(full_time)){
#       for(ii in 1:length(depths)){
#         mean_var[ii, j] <- mean(curr_var[j,ii , ], na.rm = TRUE)
#         sd_var[ii, j] <- sd(curr_var[j,ii , ], na.rm = TRUE)
#         upper_var[ii, j] <- quantile(curr_var[j,ii , ], 0.05, na.rm = TRUE)
#         lower_var[ii, j] <- quantile(curr_var[j,ii , ], 0.95, na.rm = TRUE)
#       }
#     }
#
#     date <- c()
#     for(j in 1:length(full_time)){
#       date <- c(date, rep(full_time[j], length(depths)))
#     }
#
#     if(state_names[i] %in% unlist(obs_names)){
#       obs_index <- which(obs_names == state_names[i])
#       obs_curr <- as.numeric(c(t(obs[, ,obs_index])))
#     }else{
#       obs_curr <- as.numeric(rep(NA, length(date)))
#     }
#
#     if(forecast_index > 0){
#       forecast_start_day <- full_time[forecast_index-1]
#       forecast_start_day_alpha <- 1.0
#     }else{
#       forecast_start_day <- dplyr::last(full_time)
#       forecast_start_day_alpha <- 0.0
#     }
#
#     curr_tibble <- tibble::tibble(date = lubridate::as_datetime(date),
#                                   forecast_mean = round(c(mean_var),4),
#                                   forecast_sd = round(c(sd_var),4),
#                                   forecast_upper_90 = round(c(upper_var),4),
#                                   forecast_lower_90 = round(c(lower_var),4),
#                                   observed = round(obs_curr,4),
#                                   depth = rep(depths, length(full_time)),
#                                   state = state_names[i],
#                                   forecast_start_day = forecast_start_day) %>%
#       dplyr::filter(depth %in% focal_depths_plotting)
#
#     # limit observations to only days before forecast start day
#     if(length(obs_hist$date) < 1){
#       p <- ggplot2::ggplot(curr_tibble, ggplot2::aes(x = as.Date(date))) +
#         ggplot2::geom_line(ggplot2::aes(y = forecast_mean, color = as.factor(depth)), size = 0.5)+
#         ggplot2::geom_ribbon(ggplot2::aes(ymin = forecast_lower_90, ymax = forecast_upper_90,
#                                           fill = as.factor(depth)),
#                              alpha = 0.2) +
#         ggplot2::geom_point(data = obs_hist, ggplot2::aes(y = value, color = as.factor(depth)), size = 2) +
#         ggplot2::geom_vline(aes(xintercept = as.Date(forecast_start_day),
#                                 linetype = "solid"),
#                             alpha = forecast_start_day_alpha) +
#         ggplot2::annotate(x = as.Date(forecast_start_day - 2*24*60*60), y = max(curr_tibble$forecast_lower_90), label = 'Past', geom = 'text') +
#         ggplot2::annotate(x = as.Date(forecast_start_day + 3*24*60*60), y = max(curr_tibble$forecast_lower_90), label = 'Future', geom = 'text') +
#         ggplot2::theme_light() +
#         ggplot2::scale_fill_manual(name = "Depth (m)",
#                                    values = c("#D55E00", '#009E73', '#0072B2'),
#                                    labels = c('0.1', '5.0', '10.0')) +
#         ggplot2::scale_color_manual(name = "Depth (m)",
#                                     values = c("#D55E00", '#009E73', '#0072B2'),
#                                     labels = c('0.1', '5.0', '10.0')) +
#         ggplot2::scale_linetype_manual(name = "",
#                                        values = c('solid'),
#                                        labels = c('Forecast Date')) +
#         ggplot2::scale_y_continuous(name = 'Temperature (°C)',
#                                     sec.axis = sec_axis(trans = (~.*(9/5) + 32), name = 'Temperature (°F)')) +
#         ggplot2::labs(x = "Date",
#                       y = "Temperature (°C)", #state_names[i],
#                       fill = 'Depth (m)',
#                       color = 'Depth',
#                       title = paste0("Lake Sunapee water temperature forecast, ", lubridate::date(forecast_start_day)),
#                       caption = 'Points represent sensor observations of water temperature. Lines represents the mean prediction from the forecast ensembles, or the most likely outcome.\n The shaded areas represent the 90% confidence interval of the forecast, or the possible range of outcomes based on the forecast.') +
#         ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
#                        plot.title = element_text(size = 16))
#
#     }else{
#       p <- ggplot2::ggplot(curr_tibble, ggplot2::aes(x = as.Date(date))) +
#         ggplot2::geom_line(ggplot2::aes(y = forecast_mean, color = as.factor(depth)), size = 0.5)+
#         ggplot2::geom_ribbon(ggplot2::aes(ymin = forecast_lower_90, ymax = forecast_upper_90,
#                                           fill = as.factor(depth)),
#                              alpha = 0.2) +
#         ggplot2::geom_point(data = obs_hist, ggplot2::aes(y = value, color = as.factor(depth)), size = 2) +
#         ggplot2::geom_vline(aes(xintercept = as.Date(forecast_start_day),
#                                 linetype = "solid"),
#                             alpha = forecast_start_day_alpha) +
#         ggplot2::annotate(x = as.Date(forecast_start_day - 2*24*60*60), y = max(curr_tibble$forecast_lower_90), label = 'Past', geom = 'text') +
#         ggplot2::annotate(x = as.Date(forecast_start_day + 3*24*60*60), y = max(curr_tibble$forecast_lower_90), label = 'Future', geom = 'text') +
#         ggplot2::theme_light() +
#         ggplot2::scale_fill_manual(name = "Depth (m)",
#                                    values = c("#D55E00", '#009E73', '#0072B2'),
#                                    labels = c('0.1', '5.0', '10.0')) +
#         ggplot2::scale_color_manual(name = "Depth (m)",
#                                     values = c("#D55E00", '#009E73', '#0072B2'),
#                                     labels = c('0.1', '5.0', '10.0')) +
#         ggplot2::scale_x_date(date_breaks = '4 days',
#                               date_labels = '%b %d\n%a',
#                               limits = c(as.Date(min(obs_hist$date)), as.Date(max(curr_tibble$date)))) +
#         #limits = c(as.Date(config$run_config$start_datetime) - 1, as.Date(config$run_config$forecast_start_datetime) + num_days_plot)) +
#         ggplot2::scale_linetype_manual(name = "",
#                                        values = c('solid'),
#                                        labels = c('Forecast Date')) +
#         ggplot2::scale_y_continuous(name = 'Temperature (°C)',
#                                     sec.axis = sec_axis(trans = (~.*(9/5) + 32), name = 'Temperature (°F)')) +
#         ggplot2::labs(x = "Date",
#                       y = "Temperature (°C)", #state_names[i],
#                       fill = 'Depth (m)',
#                       color = 'Depth',
#                       title = paste0("Lake Sunapee water temperature forecast, ", lubridate::date(forecast_start_day)),
#                       caption = 'Points represent sensor observations of water temperature. Lines represents the mean prediction from the forecast ensembles, or the most likely outcome.\n The shaded areas represent the 90% confidence interval of the forecast, or the possible range of outcomes based on the forecast.') +
#         ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
#                        plot.title = element_text(size = 16))
#
#     }
#     p
#
#
#     print(p)
#     dev.off()
#
#   }
#   invisible(pdf_file_name)
# }

# ## check and see what data from arrow looks like
# s3_score <- arrow::s3_bucket(bucket = "scores/parquet", endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)
#
# most_recent <-  arrow::open_dataset(s3_score) |>
#   filter(site_id %in% c("sunp")) |>
#   summarize(max = max(reference_datetime)) |>
#   collect() |>
#   pull()
#
# df_insitu_scores <- arrow::open_dataset(s3_score) |>
#   filter(variable == "temperature",
#          # depth %in% c(0.5),
#          site_id %in% c("sunp"),
#          reference_datetime == most_recent) |>
#   dplyr::collect()



install.packages('devtools')
library(devtools)
library(tidyverse)
install_github("FLARE-forecast/flareVis")
library(flareVis)

site <- 'sunp'
model <- 'test_runS3'
depths <- c(0,5,10)
lake_name <- 'Lake Sunapee'
y_limits <- c(-5,30)
y_axis_limits <- c(-5,30)
#depth_values <- c(0,5,10)

score_data <- arrow::s3_bucket(bucket = "scores/parquet", endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)



## do data subsetting
# find most recent forecast run date for the model and site
most_recent <-  arrow::open_dataset(score_data) |>
  filter(site_id %in% site,
         model_id %in% model) |>
  summarize(max = max(reference_datetime)) |>
  collect() |>
  pull()

# subset and collect data based off of site, model, depth, and reference datetime
score_df <- arrow::open_dataset(score_data) |>
  filter(variable == "temperature",
         # depth %in% c(0.5),
         depth %in% depths,
         site_id %in% site,
         model_id %in% model,
         reference_datetime == most_recent) |>
  dplyr::collect()


flareVis::plot_temp_single_panel(data = score_df, depths = depths, ylims = y_limits, site_name = site)


### testing code taken from plot_temp function -- code built off of freya's existing plot_temp() function
test_df <- plot_temp_test(s3_score,'sunp','test_runS3',depths,lake_name,y_limits)

plot_temp_test <- function(score_data,site_identifier, model_identifier, depth_values,site_name,y_axis_limits) {

  # Fix dates and rename columns to match plotting code
  plot_df <- score_df |>
    dplyr::mutate(datetime = lubridate::with_tz(lubridate::as_datetime(datetime), "America/New_York"),
                  reference_datetime = lubridate::with_tz(lubridate::as_datetime(reference_datetime), "America/New_York")) |>#,
    dplyr::filter(datetime >= reference_datetime) |>
    rename(date = datetime, forecast_mean = mean, forecast_sd = sd, forecast_upper_90 = quantile90, forecast_lower_90 = quantile10,
           observed = observation, forecast_start_day = reference_datetime)


  curr_tibble <- plot_df

  p <- ggplot2::ggplot(curr_tibble, ggplot2::aes(x = as.Date(date))) +
    ggplot2::ylim(y_axis_limits) +
    ggplot2::geom_line(ggplot2::aes(y = forecast_mean, color = as.factor(depth)), size = 0.5)+
    ggplot2::geom_ribbon(ggplot2::aes(ymin = forecast_lower_90, ymax = forecast_upper_90,
                                      fill = as.factor(depth)),
                         alpha = 0.2) +
    #ggplot2::geom_point(data = obs_hist, ggplot2::aes(y = value, color = as.factor(depth)), size = 2) +
    ggplot2::geom_vline(aes(xintercept = as.Date(forecast_start_day),
                            linetype = "solid"),
                        alpha = 1) +
    #alpha = forecast_start_day_alpha) +
    #ggplot2::annotate(x = as.Date(curr_tibble$forecast_start_day - 2*24*60*60), y = max(curr_tibble$forecast_upper_90), label = 'Past', geom = 'text') +
    #ggplot2::annotate(x = as.Date(curr_tibble$forecast_start_day + 3*24*60*60), y = max(curr_tibble$forecast_upper_90), label = 'Future', geom = 'text') +
    ggplot2::annotate(x = as.Date(curr_tibble$forecast_start_day - 24*60*60), y = max(curr_tibble$forecast_upper_90), label = 'Past', geom = 'text') +
    ggplot2::annotate(x = as.Date(curr_tibble$forecast_start_day + 24*60*60), y = max(curr_tibble$forecast_upper_90), label = 'Future', geom = 'text') +
    ggplot2::theme_light() +
    ggplot2::scale_fill_manual(name = "Depth (m)",
                               values = c("#D55E00", '#009E73', '#0072B2'),
                               labels = as.character(depth_values)) +
                               #labels = c('0.1', '5.0', '10.0')) +
    ggplot2::scale_color_manual(name = "Depth (m)",
                                values = c("#D55E00", '#009E73', '#0072B2'),
                                labels = as.character(depth_values)) +
                                #labels = c('0.1', '5.0', '10.0')) +
    ggplot2::scale_x_date(date_breaks = '4 days',
                          date_labels = '%b %d\n%a',
                          limits = c(as.Date(min(curr_tibble$date) - 1), as.Date(max(curr_tibble$date)))) +
    #limits = c(as.Date(min(obs_hist$date)), as.Date(max(curr_tibble$date)))) +
    #limits = c(as.Date(config$run_config$start_datetime) - 1, as.Date(config$run_config$forecast_start_datetime) + num_days_plot)) +
    ggplot2::scale_linetype_manual(name = "",
                                   values = c('solid'),
                                   labels = c('Forecast Date')) +
    ggplot2::scale_y_continuous(name = 'Temperature (°C)',
                                sec.axis = sec_axis(trans = (~.*(9/5) + 32), name = 'Temperature (°F)')) +
    ggplot2::labs(x = "Date",
                  y = "Temperature (°C)", #state_names[i],
                  fill = 'Depth (m)',
                  color = 'Depth',
                  title = paste0(site_name," water temperature forecast, ", lubridate::date(curr_tibble$forecast_start_day)),
                  caption = 'Points represent sensor observations of water temperature. Lines represents the mean prediction from the forecast ensembles, or the most likely outcome.\n The shaded areas represent the 90% confidence interval of the forecast, or the possible range of outcomes based on the forecast.') +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
                   plot.title = element_text(size = 16))

  print(p)

}


## GENERAL TESTING OF FUNCTION
site_id_test <- 'sunp'
model_id_test <- 'test_runS3'
depth_test <- c(0,5,10)

most_recent <-  arrow::open_dataset(s3_score) |>
  filter(site_id %in% site_id_test,
         model_id %in% model_id_test) |>
  summarize(max = max(reference_datetime)) |>
  collect() |>
  pull()

score_df <- arrow::open_dataset(s3_score) |>
  filter(variable == "temperature",
         # depth %in% c(0.5),
         depth %in% depth_test,
         site_id %in% site_id_test,
         model_id %in% model_id_test,
         reference_datetime == most_recent) |>
  dplyr::collect()




s3_score <- arrow::s3_bucket(bucket = "scores/parquet", endpoint_override = "s3.flare-forecast.org", anonymous = TRUE)

site <- 'sunp'
model <- 'test_runS3'
#depths <- c(0,5,10)
depths <- c(0,5,10)
lake_name <- 'Lake Sunapee'
y_limits <- c(-1,8.5)

test_df <- plot_temp_test(s3_score,'sunp','test_runS3',depths,lake_name,y_limits)
