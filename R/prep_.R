# x = "inst/tables/basic_validation_BR_RZ_MS_pp_.xlsx"
prep_basic_validation = function(x){
  basic_params = suppressMessages(readxl::read_excel(x))

  basic_params = basic_params %>%
    dplyr::select(site:longterm_sd) %>%
    dplyr::filter(!is.na(site)) %>%
    dplyr::mutate(longterm_avg = {suppressWarnings(as.numeric(longterm_avg))}) %>%
    dplyr::mutate(longterm_sd = {suppressWarnings(as.numeric(longterm_sd))}) %>%
    dplyr::mutate(parameter = stringr::str_to_upper(parameter)) %>%
    as.data.frame()
  return(basic_params)
}

# x = "inst/tables/Tabela_walidacyjna.xlsx"
prep_extended_validation = function(x){
  tw = suppressWarnings(readxl::read_excel(x))
  tw2 = tw %>%
    tidyr::pivot_longer(cols = PPFD_AVG:RH_DELTA)%>%
    dplyr::mutate(property = stringi::stri_extract_last_regex(str = name,
                                                              pattern = "[A-Za-z0-9]+"),
           parameter = stringi::stri_replace_last_regex(str = name,
                                               pattern = "_[A-Za-z0-9]+",
                                               replacement = "")) %>%
    dplyr::mutate(parameter = stringr::str_to_upper(parameter)) %>%
    dplyr::select(DOY, HOUR, MIN, parameter, property, value)
  tw_366 = tw2 %>%
    dplyr::filter(DOY == 365) %>%
    dplyr::mutate(DOY = 366)
  tw3 = dplyr::bind_rows(tw2, tw_366)
  extended_params = tidyr::pivot_wider(tw3, values_from = value, names_from = property)
  return(extended_params)
}
