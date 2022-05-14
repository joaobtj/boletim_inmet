file_tempo <- function(cidade) {
  obj <- readxl::read_excel("estacoes.xlsx", sheet = "SC") %>% dplyr::filter(DC_NOME == cidade)

  anos <- with(
    obj,
    seq(
      DT_INICIO_OPERACAO %>% year() %>% add(1),
      now() %>% year() %>% subtract(1),
      1
    )
  )

  return(list(anos=anos, 
              obj=obj))
}

file_cidade <- function(cidade) {
 tempo <-  file_tempo(cidade)
 

  a <- list()
  for (i in seq_along(tempo$anos)) {
    a[[i]] <- with(tempo$obj, get_inmet(
      data_ini = paste0(tempo$anos[i], "-01-01"),
      data_fim = paste0(tempo$anos[i] + 1, "-01-01"),
      station = CD_ESTACAO,
      alt = VL_ALTITUDE,
      lat = VL_LATITUDE
    )) %>% filter(year(data) == tempo$anos[i])
  }

  readr::write_csv(bind_rows(a), paste0("data/", cidade, ".csv"))


}
