#' Read in one nurses' stress data file.
#'
#' @param file_path Path to the data file.
#' @param max_rows max number of rows loaded
#'
#' @returns Outputs a data frame/tibble.
#'
read <- function(file_path, max_rows = Inf) {
  data <- file_path |>
    readr::read_csv(
      show_col_types = FALSE,
      name_repair = snakecase::to_snake_case,
      n_max = max_rows
    )
  return(data)
}

#' Read all data based on filename
#'
#' @param filename tyoe of data
#'
#' @returns Returns a dataframe including all data of the specific type
read_all <- function(filename) {
  files <- here::here("data-raw/nurses-stress/") |>
    fs::dir_ls(regexp = filename, recurse = TRUE)
  data <- files |>
    purrr::map(read) |>
    purrr::list_rbind(names_to = "file_path_id")
  return(data)
}

#' Get participant ID
#'
#' @param data The data we want to extract id from
#'
#' @returns new dataframe with a id column and no file_path_id

get_participant_id <- function(data) {
  data <- data |>
    dplyr::mutate(
      id = stringr::str_extract(
        file_path_id,
        pattern = "(?<=/stress/)[:alnum:]{2}(?=/)" # {2} -> find exactly 2 of the thing we asked for
      ),
      .before = file_path_id
    ) |>
    dplyr::select(-file_path_id)
  return(data)
}


#' Summarise to get mean, median, sd
#'
#' @param data dataframe to summarise on
#'
#' @returns summarised data by id and collection_datetime (minutes)

summarise_by_datetime <- function(data, fns, unit) {
  summarised_data <- data |>
    dplyr::mutate(
      collection_datetime = lubridate::round_date(
        collection_datetime,
        unit = unit
      )
    ) |>
    dplyr::summarise(
      dplyr::across(
        tidyselect::where(is.numeric),
        fns
      ),
      .by = c(id, collection_datetime)
    )
  return(summarised_data)
}


# modify to be able to change unit
#' Read_sensor_data
#'
#' @param filename name of file
#' @param unit unit of datetime
#'
#' @returns dataframe that has id and is summarised by datetime

read_sensor_data <- function(filename, fns, unit) {
  data <- read_all(filename) |>
    get_participant_id() |>
    summarise_by_datetime(unit = unit, fns=fns)
  return(data)
}

#' tidy_survey_dates
#'
#' @param data dataframe survey_data
#'
#' @returns a tidy dataframe
tidy_survey_dates <- function(data) {
  tidied <- data |>
    dplyr::mutate(
      date = lubridate::mdy(date),
      start_datetime = lubridate::as_datetime(paste(date, start_time)),
      end_datetime = lubridate::as_datetime(paste(date, end_time)),
      datetime_id = start_datetime,
      .before = start_time
    ) |>
    dplyr::select(-c(date, start_time, end_time, duration))
  return(tidied)
}

#' survey_to_long
#'
#' @param data tidy dataframe on survey
#'
#' @returns survey data in a longer format dataframe
survey_to_long <- function(data) {
  longer <- data |>
    dplyr::select(id, datetime_id, start_datetime, end_datetime) |>
    tidyr::pivot_longer(c(start_datetime, end_datetime),
                        names_to = NULL,
                        values_to = "collection_datetime"
    ) |>
    dplyr::group_by(dplyr::pick(-collection_datetime)) |>
    tidyr::complete(
      collection_datetime = seq(
        min(collection_datetime),
        max(collection_datetime),
        by = 60
      )
    ) |>
    dplyr::ungroup()
  return(longer)
}
