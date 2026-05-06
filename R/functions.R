#' Read in one nurses' stress data file.
#'
#' @param file_path Path to the data file.
#' @param max_rows max number of rows loaded
#'
#' @returns Outputs a data frame/tibble.
#'
read <- function(file_path, max_rows = 100) {
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
  files <- here("data-raw/nurses-stress/") |>
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
