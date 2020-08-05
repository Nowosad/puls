library(readxl)
library(dplyr)
library(tidyr)
library(stringi)

# basic params ------------------------------------------------------------
basic_params = read_excel("inst/tables/basic_validation_BR_RZ_MS_pp_.xlsx") %>%
  select(site:longterm_sd) %>%
  filter(!is.na(site)) %>%
  mutate(longterm_avg = as.numeric(longterm_avg)) %>%
  mutate(longterm_sd = as.numeric(longterm_sd)) %>%
  mutate(parameter = stringr::str_to_upper(parameter)) %>%
  as.data.frame()

# extended params ---------------------------------------------------------
# vars
# PPFD
# PPFD_d
# PPFD_bc
# PPFD_r
# SW_in
# LW_out
# TS
# TA
# RH
tw = read_excel("inst/tables/Tabela_walidacyjna.xlsx")
tw2 = tw %>%
  pivot_longer(cols = PPFD_AVG:RH_DELTA)%>%
  mutate(property = stri_extract_last_regex(str = name, pattern = "[A-Za-z0-9]+"),
         parameter = stri_replace_last_regex(str = name, pattern = "_[A-Za-z0-9]+", replacement = "")) %>%
  mutate(parameter = stringr::str_to_upper(parameter)) %>%
  select(DOY, HOUR, MIN, parameter, property, value)
tw_366 = tw2 %>%
  filter(DOY == 365) %>%
  mutate(DOY = 366)
tw3 = bind_rows(tw2, tw_366)
extended_params = pivot_wider(tw3, values_from = value, names_from = property)

# save --------------------------------------------------------------------
save(basic_params, extended_params,
     file = "R/sysdata.rda",
     compress = "bzip2",
     version = 2)
