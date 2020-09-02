#' @export
run_validation = function(data_folder = "data",
                          result_folder = "validation",
                          start_date = as.Date(-Inf, origin = "1970-01-01"),
                          end_date = as.Date(Inf, origin = "1970-01-01"),
                          validation_table,
                          validation_table_ext){

  brody_files = dir(paste0(data_folder, "/Brody"),
                    pattern = ".dat", full.names = TRUE)
  kusowo_files = dir(paste0(data_folder, "/Kusowo"),
                     pattern = ".dat", full.names = TRUE)
  rzecin_files = dir(paste0(data_folder, "/Rzecin"),
                     pattern = ".dat", full.names = TRUE) %>%
    stringr::str_subset(pattern = "TOB1_CR3000_ts_data_2020_02_20_0700.dat", negate = TRUE) %>%
    stringr::str_subset(pattern = "202002202330.dat", negate = TRUE)


  ifelse(!dir.exists(result_folder),
         dir.create(result_folder),
                    FALSE)
  brody_error = purrr::map_df(brody_files,
                              validator,
                              start_date = start_date,
                              end_date = end_date,
                              validation_table = validation_table,
                              validation_table_ext = validation_table_ext)
  # saveRDS(brody_error, "brody_error.rds")
  fst::write_fst(brody_error, paste0(result_folder, "/brody_error.fst"))

  kusowo_error = purrr::map_df(kusowo_files,
                               validator,
                               start_date = start_date,
                               end_date = end_date)
  # saveRDS(kusowo_error, "kusowo_error.rds")
  fst::write_fst(kusowo_error, paste0(result_folder, "/kusowo_error.fst"))

  rzecin_error = purrr::map_df(rzecin_files,
                               validator,
                               start_date = start_date,
                               end_date = end_date)
  # saveRDS(rzecin_error, "rzecin_error.rds")
  fst::write_fst(rzecin_error, paste0(result_folder, "/rzecin_error.fst"))
}
