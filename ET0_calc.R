#' Evapotranspiracao de referencia
#' 
#' funcao para calcular a evapotranspiracao de referencia
#'
#' @param TMax temperatura maxima
#' @param TMin temperatura minima
#' @param TMed temperatura media
#' @param UR umidade relativa
#' @param Vv velocidade do vento
#' @param Rn saldo de radiacao
#' @param alt altitude da estavao
#' @param zv altura em que foi medido o vento
#' 
#' @return ETO
#' 
#' @examples 
#' 
#' ET0_calc (TMax = 26.6, TMin = 17.3, TMed = 20.3, UR = 87.3, Vv = 1.96, Rg = 13.7, alt = 1000, zv = 2, lat = -0.4712)
#' ## Dados estacao INMET Curitibanos 01/01/2009
#' TMax = 27.6
#'  TMin = 17.2
#'   TMed = 21.0
#'    UR = 78.7
#'     Vv = 3.4
#'      Rg = 19149.62
#'       alt = 978
#'       zv=2
#'        lat = -27.288624
#' ET0_calc (TMax = 27.6, TMin = 17.2, TMed = 21.0, UR = 78.7, 
#'           Vv = 3.4, Rg = 19149.62/1000, alt = 978, zv = 2, lat = -27.288624)
#'           ## ET0 boletim deveria ser 4,89 ???




et0 <- (D*(rn-g)+pa*cp*(es-ea)/ra)/(D+y*(1+rs/ra))



ET0_calc <- function(TMax, TMin, TMed, UR, Vv, Rg, alt, lat, zv=2){
    
    e0_TMax <- 0.6108*exp((17.27*TMax)/(TMax+237.3))
    e0_TMin <- 0.6108*exp((17.27*TMin)/(TMin+237.3))
    e0_TMed <- 0.6108*exp((17.27*TMed)/(TMed+237.3))
    #Delta TMedia: declinacao da curva de saturacao
    D_TMed <- 4098*e0_TMed/(TMed+237.3)^2
    #pressao de saturacao de vapor
    es <- (e0_TMax+e0_TMin)/2
    #pressao de vapor atual do ar
    ea <- e0_TMed*UR/100 
    #pressao atmosferica
    P <- 101.3 * ((293-0.0065*alt)/293)^5.26
    #constante psicrometrica
    Y <- 0.001013/(0.622*2.45)*P
    #dia juliano: a cada linha(dia) soma mais um
    J <- 1
    # #declinacao solar
    sigma <- 0.409*sin(((2*pi*J)/365)-1.39)
    # 
    # #distancia relativa inversa terra-sol
    dr <- 1+0.033*cos(2*pi*J/365)
    # #angulo da radiacao do sol
    ws <- acos(-tan(lat)*tan(sigma))
    # #fotoperiodo
    N <- 24/pi*ws
    # #radiacao solar extraterrestre
    Ra <- (24*60/pi)*0.082*dr*(ws*sin(lat)*sin(sigma)+cos(lat)*cos(sigma)*sin(ws))
    if (is.null(Rg)) {
        #verificar se essa operacao que usa somente as temperaturas e adequada
        Rs <- 0.16 * sqrt(TMax - TMin) * Ra
    } else{
        Rs <- Rg * 0.0864
        #conferir se o Rg da estacao e o Rs abaixo
    }
    # #radiacao solar de ondas curtas
    Rns <- (1 - 0.23)*Rs
    # #radiacao solar em dias sem nuvens
    Rso <- (0.75+0.00002*alt)*Ra
    # #radiacao solar em ondas longas
    Rnl <- 0.000000004903*(((TMax+273.16)^4+(TMin+273.16)^4)/2)*(0.34-0.14*sqrt(ea))*(1.35*Rs/Rso-0.35)
    # #saldo de radiacao a superficie
    Rn <- Rns - Rnl
    # #alturada de medicao da velocidade do vento
    #zv <- 10
    #velocidade do vento a 2m do solo
    U2 <- Vv * 4.87/(log(67.8*zv-5.42))
    #******Calculo ETO*******
    ETO <- (0.408*D_TMed*(Rn-0)+Y*(900/(TMed+273.16))*U2*(es-ea))/(D_TMed+Y*(1+0.34*U2))
    return(ETO)
}

