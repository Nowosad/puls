run_validation = function(){
  brody_files = dir("/home/jn/Documents/up-data-validation/data/Brody",
                    pattern = ".dat", full.names = TRUE)
  kusowo_files = dir("/home/jn/Documents/up-data-validation/data/Kusowo",
                     pattern = ".dat", full.names = TRUE)
  rzecin_files = dir("/home/jn/Documents/up-data-validation/data/Rzecin",
                     pattern = ".dat", full.names = TRUE) %>%
    stringr::str_subset(pattern = "TOB1_CR3000_ts_data_2020_02_20_0700.dat", negate = TRUE) %>%
    stringr::str_subset(pattern = "202002202330.dat", negate = TRUE)

  brody_error = purrr::map_df(brody_files,
                              validator,
                              start_date = as.Date(-Inf, origin = "1970-01-01"),
                              end_date = as.Date(Inf, origin = "1970-01-01"))
  # saveRDS(brody_error, "brody_error.rds")
  fst::write_fst(brody_error, "brody_error.fst")

  kusowo_error = purrr::map_df(kusowo_files,
                               validator,
                               start_date = as.Date(-Inf, origin = "1970-01-01"),
                               end_date = as.Date(Inf, origin = "1970-01-01"))
  # saveRDS(kusowo_error, "kusowo_error.rds")
  fst::write_fst(kusowo_error, "kusowo_error.fst")

  rzecin_error = purrr::map_df(rzecin_files,
                               validator,
                               start_date = as.Date(-Inf, origin = "1970-01-01"),
                               end_date = as.Date(Inf, origin = "1970-01-01"))
  # saveRDS(rzecin_error, "rzecin_error.rds")
  fst::write_fst(rzecin_error, "rzecin_error.fst")

}


