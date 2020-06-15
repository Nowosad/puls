#' Title
#'
#' @param x
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
validator = function(x, start_date = as.Date("2019-07-25"), end_date = as.Date("2019-07-30")){
  x %>%
    cleaner() %>%
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
    warning("Parameter ", parameter, " does not exist.\nTry a different one.", call. = FALSE)
    return(x)
  }

  x = validate_nan(x)
  x = validate_min(x, param_df = param_df)
  x = validate_max(x, param_df = param_df)
  x = validate_longterm(x, param_df = param_df, howmanysd = 3)

  x

}
