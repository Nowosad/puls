#' Title
#'
#' @param x
#' @param parameter
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
validator = function(x, parameter, start_date = as.Date("2019-07-25"), end_date = as.Date("2019-07-30")){
  x = cleaner(x) %>%
    dplyr::filter(dplyr::between(as.Date(TIMESTAMP), start_date, end_date)) %>%
    dplyr::filter(parameter == !!parameter)

  param_df = basic_params %>%
    dplyr::filter(parameter == !!parameter) %>%
    dplyr::filter(site == unique(x$site))

  if(nrow(param_df) == 0) stop("Parameter ", parameter, " does not exist.\nTry a different one.", call. = FALSE)


  error_df_template = data.frame(TIMESTAMP = .POSIXct(character()),
                                 RECORD = numeric(),
                                 name = character(),
                                 value = numeric(),
                                 problem = character())
  x = list(correct_df = x, error_df = x[0, ])

  x = validate_nan(x)
  x = validate_min(x, param_df = param_df)
  x = validate_max(x, param_df = param_df)
  x = validate_longterm(x, param_df = param_df, howmanysd = 3)

  x

}
