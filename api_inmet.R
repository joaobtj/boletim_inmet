library(httr2)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(janitor)
devtools::install_github("joaobtj/hydirrig")
#source("ET0_calc.R")

## Download from INMET API - Estações
## https://portal.inmet.gov.br/manual/manual-de-uso-da-api-esta%C3%A7%C3%B5es

 # data_ini <- "2021-01-01" # "2016-02-20"
 # data_fim <- "2021-09-01" # "2016-02-22"
 # station <- "A860" # Curitibanos-SC
 # alt = 978.10
 # lat = -27.288624
 # tst <- get_inmet(data_ini, data_fim, station, alt, lat)


get_inmet <- function(data_ini, data_fim, station, alt, lat) {

  ## Hora
  url_base_hora <- "https://apitempo.inmet.gov.br/estacao"
  url_hora <- glue::glue(url_base_hora, data_ini, data_fim, station, .sep = "/")

  dados_hora <- httr2::request(url_hora) %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      data = lubridate::ymd_hm(paste(DT_MEDICAO, HR_MEDICAO)),
      data = lubridate::with_tz(data, tzone = "Etc/GMT+3")
    ) %>%
    janitor::clean_names() %>%
    dplyr::select(any_of(c(
      "data", "chuva", "pre_ins", "pre_max", "pre_min", "pto_ins", "pto_max", "pto_min", "rad_glo",
      "tem_ins", "tem_max", "tem_min", "umd_ins", "umd_max", "umd_min", "ven_raj", "ven_vel"
    ))) %>%
    dplyr::mutate(across(.cols = !data, as.numeric)) %>%
    dplyr::relocate(data)


  ## Dia
  dados_dia <- dados_hora %>%
    mutate(data = lubridate::date(data)) %>%
    group_by(data) %>%
    summarise(
      chuva = sum(chuva),
      pre_med = mean(pre_ins),
      pre_max = max(pre_max),
      pre_min = min(pre_min),
      pto_med = mean(pto_ins),
      pto_max = max(pto_max),
      pto_min = min(pto_min),
      rad_glo = sum(rad_glo),
      tem_med = mean(tem_ins),
      tem_max = max(tem_max),
      tem_min = min(tem_min),
      umd_med = mean(umd_ins),
      umd_max = max(umd_max),
      umd_min = min(umd_min),
      ven_raj_max = max(ven_raj),
      ven_vel_med = mean(ven_vel),
      et0 = hydirrig::et0_calc(tem_max, tem_min, umd_max, umd_min, uv=ven_vel_med, rs=rad_glo/1000, lat=lat, alt=alt, date=data)
    ) %>%
    dplyr::slice_head(n = -1) %>%
    dplyr::slice_tail(n = -1) %>%
    dplyr::arrange(data) %>%
    distinct(data, .keep_all = TRUE)
}



