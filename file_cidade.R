file_cidade <- function(cidade){
  
  obj <- readxl::read_excel("estacoes.xlsx", sheet = "SC") %>% dplyr::filter(DC_NOME == cidade)
  
  anos <- with(
    obj,
    seq(
      DT_INICIO_OPERACAO %>% year() %>% add(1),
      now() %>% year() %>% subtract(1),
      1
    )
  )
  
  a <- list()
  for (i in seq_along(anos[1])) {
    a[[i]] <- with(obj, get_inmet(
      data_ini = paste0(anos[i], "-01-01"),
      data_fim = paste0(anos[i] + 1, "-01-01"),
      station = CD_ESTACAO,
      alt = VL_ALTITUDE,
      lat = VL_LATITUDE
    )) %>% filter(year(data) == anos[i])
  }
  
  readr::write_csv(bind_rows(a), paste0("data/",cidade,".csv"))
  
  return(obj)
}
