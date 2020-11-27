#' Validates Data
#'
#' @param x Path to a `.dat` file
#' @param site_code Code of the site, either `"KU"`, `"BR"`, `"RZ"`
#' @param start_date The start date of the validation period
#' @param end_date The end state of the validation period
#' @param validation_table Additional argument: A path to a .xlsx file
#' with a basic validation table.
#' By default, a built-in table is used.
#' @param validation_table_ext Additional argument: A path to a .xlsx file
#'with an extended validation table.
#' By default, a built-in table is used.
#'
#' @return A data frame with possible errors
#' @export
#'
#' @examples
#' dat_filepath = system.file("dat/CR3000_Rain.dat", package = "puls")
#' validator(dat_filepath, site_code = "BR",
#'     start_date = as.Date("2017-07-25"), end_date = as.Date("2019-07-30"))
#'
validator = function(x,
                     site_code = NULL,
                     start_date = as.Date(-Inf, origin = "1970-01-01"),
                     end_date = as.Date(Inf, origin = "1970-01-01"),
                     validation_table,
                     validation_table_ext){
  x_basename = basename(x)
  message("DATASET: ", x_basename)
  x = x  %>%
    dplyr::filter(dplyr::between(as.Date(TIMESTAMP),
                                 as.Date(start_date),
                                 as.Date(end_date)))
  x = x %>%
    cleaner(site_code = site_code)

  if(nrow(x) == 0) {
    message("No data exists between ", start_date, " and ", end_date,  "!")
    invisible(return(NULL))
  }

  if (!missing(validation_table)){
    basic_params = prep_basic_validation(validation_table)
  } else {
    basic_params = puls:::basic_params
  }

  if (!missing(validation_table_ext)){
    extended_params = prep_extended_validation(validation_table_ext)
  } else {
    extended_params = puls:::extended_params
  }

  x %>%
    dplyr::group_split(parameter) %>%
    purrr::map(single_validator,
               basic_params = basic_params,
               extended_params = extended_params) %>%
    purrr::map_df("error_df") %>%
    dplyr::mutate(filename = x_basename) %>%
    dplyr::select(filename, dplyr::everything())
}
single_validator = function(x,
                            basic_params,
                            extended_params){
  parameter = unique(x$parameter)

  x = x %>%
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
    message("Parameter: ", parameter, ". Basic validation does not exist!")
    # return(x)
  } else {
    message("Parameter: ", parameter, ". Basic validation!")
    x = validate_nan(x)
    x = validate_min(x, param_df = param_df)
    x = validate_max(x, param_df = param_df)
  }

  param_df_ext = extended_params  %>%
    dplyr::filter(parameter == !!parameter)

  if(nrow(param_df_ext) == 0) {
    message("Parameter: ", parameter, ". Extended validation does not exist!")
    # return(x)
  } else {
    message("Parameter: ", parameter, ". Extended validation!")
    x = validate_longterm(x, param_df_ext = param_df_ext, howmanysd = 3)
    x = validate_delta(x, param_df_ext = param_df_ext, howmany = 3)
  }
  x
}
