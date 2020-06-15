#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
cleaner = function(x){

  folder_name = basename(dirname(x))
  x_site = switch (folder_name,
    "Kusowo" = "KU",
    "Brody" = "BR",
    "Rzecin" = "RZ"
  )

  x %>%
    read_dat() %>%
    tidyr::pivot_longer(-c("TIMESTAMP", "RECORD")) %>%
    dplyr::mutate(parameter = stringr::str_extract(name, "^[A-Z]*[\\_]?[A-Z]*")) %>%
    dplyr::mutate(parameter = stringr::str_replace(parameter, "[\\_]$", "")) %>%
    dplyr::mutate(site = x_site)
}
