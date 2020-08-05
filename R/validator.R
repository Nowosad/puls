#' Validates Data
#'
#' @param x Path to a `.dat` file
#' @param site_code Code of the site, either `"KU"`, `"BR"`, `"RZ"`
#' @param start_date The start date of the validation period
#' @param end_date The end state of the validation period
#'
#' @return A data frame with possible errors
#' @export
#'
#' @examples
#' dat_filepath = system.file("dat/CR3000_Rain.dat", package = "puls")
#' validator(dat_filepath, site_code = "BR",
#'     start_date = as.Date("2017-07-25"), end_date = as.Date("2019-07-30"))
#'
validator = function(x, site_code = NULL, start_date = as.Date("2019-07-25"), end_date = as.Date("2019-07-30")){
  message("DATASET: ", x)
  x %>%
    cleaner(site_code = site_code) %>%
    dplyr::group_split(parameter) %>%
    purrr::map(single_validator, start_date, end_date) %>%
    purrr::map_df("error_df") %>%
    dplyr::mutate(filename = basename(x)) %>%
    dplyr::select(filename, dplyr::everything())
}
single_validator = function(x, start_date = as.Date("2019-07-25"), end_date = as.Date("2019-07-30")){
  parameter = unique(x$parameter)

  x = x %>%
    dplyr::filter(dplyr::between(as.Date(TIMESTAMP), start_date, end_date)) %>%
    dplyr::filter(parameter == !!parameter)

  param_df = basic_params %>%
    dplyr::filter(parameter == !!parameter) %>%
    dplyr::filter(site == unique(x$site))

  error_df_template = data.frame(TIMESTAMP = x$TIMESTAMP[0],
                                 RECORD = numeric(),
                                 name = character(),
                                 value = numeric(),
                                 problem = character())
  x = list(correct_df = x, error_df = error_df_template)

  # if(nrow(param_df) == 0) stop("Parameter ", parameter, " does not exist.\nTry a different one.", call. = FALSE)
  if(nrow(param_df) == 0) {
    message("Parameter: ", parameter, " validation does not exist!")
    # return(x)
  } else {
    message("Parameter: ", parameter, " validated!")
    x = validate_nan(x)
    x = validate_min(x, param_df = param_df)
    x = validate_max(x, param_df = param_df)
  }

  param_df_ext = extended_params  %>%
    dplyr::filter(parameter == !!parameter)

  x = validate_longterm(x, param_df_ext = param_df_ext, howmanysd = 3)
  x = validate_delta(x, param_df_ext = param_df_ext, howmany = 3)

  x

}


