
# -- -------------------------------------------------------------------------------------------- #
# -- Initial Developer: FranciscoME ------------------------------------------------------------- #
# -- Codigo: R_Data_Procesor -------------------------------------------------------------------- #
# -- GitHub Repossitory: https://github.com/TradingPal/R_Data_Processor ------------------------- #
# -- License: TradingPal ------------------------------------------------------------------------ #
# -- -------------------------------------------------------------------------------------------- #

# -- -------------------------------------------------------------------------------------------- #
# -- AutoCorrelation and Partial AutoCorrelation Calculations and Graph ------------------------- #
# -- -------------------------------------------------------------------------------------------- #

AutoCorrelation <- function(x, type, LagMax, IncPlot)  {

  ciline <- 2/sqrt(length(x))
  bacf   <- acf(x, plot = IncPlot, lag.max = LagMax, type = type)
  bacfdf <- with(bacf, data.frame(lag, acf))
  #bacfdf <- bacfdf[-seq(1, 0),]
  Sig_nc <- (abs(bacfdf[,2]) > abs(ciline))^2
  bacfdf <- cbind(bacfdf, Sig_nc)

return(bacfdf)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Stationarity Dickey-Fuller Test ------------------------------------------------------------ #
# -- -------------------------------------------------------------------------------------------- #

ADFTestedSeries <- function(DataFrame,Column,CnfLvl)  {

  df_precios <- DataFrame
  d <- 0
  adfSeries <- adf.test(DataFrame[,Column])
  if (adfSeries$p.value > CnfLvl)
  { 
    d <- 1
    Serie1D <- data.frame(df_precios[-1,1],
                          diff(DataFrame[,2],diff = 1))
    aSerie1D  <- adf.test(Serie1D[,2])
    if (aSerie1D$p.value > CnfLvl)
    {
      d <- 2
      Serie2D <- data.frame(Serie1D[-1,1],
                            diff(Serie1D[,2],diff = 1))
      aSerie2D <- adf.test(Serie2D[,2])
      if (aSerie2D$p.value > CnfLvl)
      {
        d <- 3
        Serie3D <- data.frame(Serie2D[-1,1],
                              diff(Serie2D[,2],diff = 1))
        aSerie3D <- adf.test(Serie3D[,2])
        if (aSerie3D$p.value > CnfLvl)
        {
          message <- "Serie de Tiempo Muy Inestable para este Metodo"
          StationarySeries <- data.frame(DataFrame)
        } else StationarySeries <- data.frame(Serie3D,d)
      } else StationarySeries <- data.frame(Serie2D,d)
    } else StationarySeries <- data.frame(Serie1D,d)
  } else StationarySeries <- data.frame(DataFrame[,1],DataFrame[,Column],d)

return (StationarySeries)
}

# -- -------------------------------------------------------------------------------------------- #
# -- DrawDown y DrawUp POR HISTORICOS ----------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

DD_DU <- function(Fecha_Inicial, Fecha_Final, Instrumento, Historicos, Grafica)  {
  
  # Fecha_Inicial = as.POSIXct("2017-12-07 15:05:00", origin="1970-01-01")
  # Fecha_Final = as.POSIXct("2017-12-07 22:15:00", origin="1970-01-01")
  # Instrumento = "EUR_USD"
  # Historicos =  OA_Historicos[[ENT_DD_DU_OP$Symbol[i]]]
  # Grafica = FALSE
  
  # -- Fecha_Inicial: Fecha en formato tipo POSIXct
  # -- Fecha_Final: Fecha en formato tipo POSIXct
  # -- Instrumento: Nombre de instrumento de FOREX, EUR_USD
  # -- Historicos: Data.Frame con c(TimeStamp, Open, High, Low, Close), TimeStamp tipo POSIXct
  # -- Formatos de fechas en UTC-5 y si es tipo character "2016-01-01 00:00"
    
  Pip_S_M <- data.frame("NZD_USD" = c("PipSize"=4, "MultPip"=10000), 
                        "AUD_USD" = c("PipSize"=4, "MultPip"=10000),
                        "EUR_USD" = c("PipSize"=4, "MultPip"=10000),
                        "GBP_USD" = c("PipSize"=4, "MultPip"=10000),
                        "USD_CHF" = c("PipSize"=4, "MultPip"=10000),
                        "USD_CAD" = c("PipSize"=4, "MultPip"=10000),
                        "USD_MXN" = c("PipSize"=2, "MultPip"=10000),
                        "USD_JPY" = c("PipSize"=2, "MultPip"=100),
                        "GBP_JPY" = c("PipSize"=2, "MultPip"=100),
                        "EUR_JPY" = c("PipSize"=2, "MultPip"=100),
                        "AUD_JPY" = c("PipSize"=2, "MultPip"=100),
                        "NZD_JPY" = c("PipSize"=2, "MultPip"=100),
                        "CAD_JPY" = c("PipSize"=2, "MultPip"=100),
                        "EUR_CAD" = c("PipSize"=2, "MultPip"=10000),
                        "EUR_CHF" = c("PipSize"=2, "MultPip"=10000),
                        "EUR_GBP" = c("PipSize"=2, "MultPip"=10000),
                        "WTICO_USD" = c("PipSize"=2, "MultPip"=100),
                        "AUD_CAD" = c("PipSize"=4, "MultPip"=10000), 
                        "EUR_NZD" = c("PipSize"=4, "MultPip"=10000), 
                        "AUD_NZD" = c("PipSize"=4, "MultPip"=10000) )
    
  MultPip <- Pip_S_M[2,which(colnames(Pip_S_M) == Instrumento)]
    
  F_Ini <- as.POSIXct(Fecha_Inicial, origin = "1970-01-01")
  F_Fin <- as.POSIXct(Fecha_Final, origin = "1970-01-01")
  F_Ins <- Instrumento
  F_His <- Historicos
  
  if(is.null(F_His)){

    HisSpreads(OA_At,OA_Gn,OA_Da,OA_Ta,OA_Ak,F_Ins,F_Ini[x],F_Fin[x+1], Count=NULL)
    
  } else

  F1 <- which(abs(F_His$TimeStamp-F_Ini) == min(abs(F_His$TimeStamp - F_Ini)))
  F2 <- which(abs(F_His$TimeStamp-F_Fin) == min(abs(F_His$TimeStamp - F_Fin)))
    
  F_His_Nvos <- F_His[F1:F2,]
    
  MaxHigh <- which.max(F_His_Nvos$High_Ask)
  MinLow  <- which.min(F_His_Nvos$Low_Bid)
    
  P0_OrdenMaxMin <- ifelse(MinLow <= MaxHigh, "MinLow", "MaxHigh")
    
  P1_DD <- round(abs((F_His_Nvos$Open_Ask[1] - min(F_His_Nvos$Low_Bid))*MultPip),1)
  P2_DU <- round(abs((max(F_His_Nvos$High_Ask) - F_His_Nvos$Open_Bid[1])*MultPip),1)
    
  if(Grafica)  {
    
    yscala <- seq(min(F_His_Nvos$Close_Ask), max(F_His_Nvos$Close_Ask),
                  (max(F_His_Nvos$Close_Ask) - min(F_His_Nvos$Close_Ask))/10)
    
    MaxHigh_G <- which.max(F_His_Nvos$Close_Ask)
    MinLow_G  <- which.min(F_His_Nvos$Close_Bid)
    
    Grafica <- ggplot(F_His_Nvos, aes(x=F_His_Nvos$TimeStamp)) + 
               geom_line(aes(y=F_His_Nvos$Close_Ask), size=.75) +
               geom_vline(xintercept = as.numeric(F_His_Nvos$TimeStamp[MaxHigh_G]), 
                          linetype="dashed", color="red", size=.75)  +
               geom_vline(xintercept = as.numeric(F_His_Nvos$TimeStamp[MinLow_G]), 
                          linetype="dashed", color="blue", size=.75) +
               geom_hline(yintercept = F_His_Nvos$Close_Ask[MinLow_G], 
                          linetype="dashed", color="blue", size=.75) +
               geom_hline(yintercept = F_His_Nvos$Close_Ask[MaxHigh_G], 
                          linetype="dashed", color="red", size=.75) + 
              labs(x=NULL, y=NULL, title=NULL) +
              scale_y_continuous(breaks = round(seq(
              round(min(F_His_Nvos$Close_Ask),6),
              round(max(F_His_Nvos$Close_Ask),6),
              (round(max(F_His_Nvos$Close_Ask),6) - round(min(F_His_Nvos$Close_Ask),6))/10),4))
    
  } else Grafica <- NULL
    
  DDU_Final <- list(P0_OrdenMaxMin = P0_OrdenMaxMin,
                    P1_DU = P1_DD,
                    P2_DD = P2_DU,
                    P3_MaxHigh = F_His_Nvos$High_Ask[MaxHigh],
                    P4_MinLow  = F_His_Nvos$Low_Bid[MinLow],
                    P5_Grafica = Grafica)

return(DDU_Final)
}

# -- -------------------------------------------------------------------------------------------- #
# -- DrawDown y DrawUp de PnL en cuenta --------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

DD_DU_PnL <- function(Datos)  {

  ts <- as.vector(Datos[,1])
  cumsum <- cumsum(c(0, ts))
  cmaxx <- cumsum - cummax(cumsum)
  cmaxx <- cmaxx[-1]
  cmaxx <- as.matrix(cmaxx)
  row.names(cmaxx) <- row.names(Datos)
  cmaxx <- timeSeries(cmaxx)

return(cmaxx)
}


# -- -------------------------------------------------------------- Draw Down de una operacion -- #
# -- -------------------------------------------------------------------------------------------- #

DDU <- function(F_Ini, F_Fin, F_In, F_Ta, F_At, F_Ak, F_Grafica) {
  
  F_Ini <- as.POSIXct("2018-12-05 11:05:00 CST", origin = "1970-01-01")
  F_Fin <- as.POSIXct("2018-12-05 15:45:00 CST", origin = "1970-01-01")
  F_In <- "EUR_USD"
  F_Ta <- "America/Mexico_City"
  F_At <- "practice"
  F_Ak <- "ada4a61b0d5bc0e5939365e01450b614-4121f84f01ad78942c46fc3ac777baa6" 
  F_Grafica <- TRUE
  
  Fecha_Inicial <- F_Ini
  Fecha_Final   <- F_Fin
  
  if(weekdays.Date(as.Date(substr(Fecha_Inicial,1,10)), abbreviate = TRUE) == "mar") {
    Fecha_Inicial <- as.Date(substr(Fecha_Inicial,1,10))-3
  } else Fecha_Inicial <- as.Date(substr(Fecha_Inicial,1,10))-2
  
  if(weekdays.Date(as.Date(substr(Fecha_Final,1,10)), abbreviate = TRUE) == "jue") {
    Fecha_Final   <- as.Date(substr(Fecha_Final,1,10))+3
  } else Fecha_Final <- as.Date(substr(Fecha_Final,1,10))+2
  
  OA_HistMax <- Fecha_Inicial + 3
  
  if(Fecha_Final > OA_HistMax) {
    
    NPet <- ceiling(as.numeric(Fecha_Final - Fecha_Inicial)/2)
    Indices <- sort(seq(0, 3*NPet, 3), decreasing = TRUE)
    Fechas  <- c()
    for(i in 1:length(Indices)) Fechas[i] <- as.character(Fecha_Final-Indices[i])
    
  } else {
    Fechas  <- c(Fecha_Inicial, Fecha_Final)
  }
  
  PreciosHist <- lapply(2:(length(Fechas)), function(x)
    HisPrices(AccountType = F_At, Granularity = "M1",
              DayAlign = 17, TimeAlign = F_Ta, Token = F_Ak,
              Instrument = F_In ,Start =  Fechas[x-1],
              End = Fechas[x], Count = NULL))
  OA_Totales  <- do.call(rbind,PreciosHist)
  OA_Totales  <- OA_Totales[,1:5]
  
  FechaCercana1 <- which.min(abs(OA_Totales$TimeStamp - F_Ini))
  FechaCercana2 <- which.min(abs(OA_Totales$TimeStamp - F_Fin))
  PreciosHist   <- OA_Totales[FechaCercana1:FechaCercana2,]
  
  dd_min <- which.min(PreciosHist$Low)
  dd_max <- which.max(PreciosHist$High)
  
  y_escala <- as.numeric(seq(min(PreciosHist$TimeStamp), max(PreciosHist$TimeStamp),
                             (max(PreciosHist$TimeStamp) - min(PreciosHist$TimeStamp))/6))
  
  x_escala <- c(first(y_escala)-2*(y_escala[1]-y_escala[2]),
                y_escala, last(y_escala)+2*(y_escala[1]-y_escala[2]))
  
  if(F_Grafica)  {
    
    MaxHigh_G <- which.max(PreciosHist$Close)
    MinLow_G  <- which.min(PreciosHist$Close)
    
    Grafica <- ggplot(PreciosHist, aes(x=PreciosHist$TimeStamp)) + 
                      geom_line(aes(y=PreciosHist$Close), size=.75) +
      
      # -- Vertical High
      geom_segment(x = as.numeric(PreciosHist$TimeStamp[MaxHigh_G]),
                   xend = as.numeric(PreciosHist$TimeStamp[MaxHigh_G]),
                   y = 0, yend = as.numeric(PreciosHist$Close[MaxHigh_G]),
                   linetype= "dashed", size=.5, colour = "dark grey", alpha=0.5) +
      
      # -- Vertical Low
      geom_segment(x = as.numeric(PreciosHist$TimeStamp[MinLow_G]),
                   xend = as.numeric(PreciosHist$TimeStamp[MinLow_G]),
                   y = 0, yend = as.numeric(PreciosHist$Close[MinLow_G]),
                   linetype= "dashed", size=.5, colour = "dark grey", alpha=0.35)
      
      if(MaxHigh_G <= MinLow_G) {
       
        Grafica_1 <- Grafica +  
          
          # -- Horizontal High
          geom_segment(x = 0, xend = PreciosHist$TimeStamp[MaxHigh_G],
                       y = as.numeric(PreciosHist$Close[MaxHigh_G]),
                       yend = as.numeric(PreciosHist$Close[MaxHigh_G]),
                       linetype= "dashed", size=.5, colour = "dark grey", alpha=0.5) +

          # -- Horizontal Low
          geom_segment(x = PreciosHist$TimeStamp[MinLow_G],
                       xend = last(PreciosHist$TimeStamp),
                       y = as.numeric(PreciosHist$Close[MinLow_G]),
                       yend = as.numeric(PreciosHist$Close[MinLow_G]),
                       linetype= "dashed", size=.5, colour = "dark grey", alpha=0.35)
         
      } else {
        
        Grafica_1 <- Grafica + 
          
          # -- Horizontal High
          geom_segment(x = PreciosHist$TimeStamp[MaxHigh_G],
                       xend = last(PreciosHist$TimeStamp),
                       y = as.numeric(PreciosHist$Close[MaxHigh_G]),
                       yend = as.numeric(PreciosHist$Close[MaxHigh_G]),
                       linetype= "dashed", size=.5, colour = "dark grey", alpha=0.5) +
          
          # -- Horizontal Low
          geom_segment(x = 0,
                       xend = PreciosHist$TimeStamp[MinLow_G],
                       y = as.numeric(PreciosHist$Close[MinLow_G]),
                       yend = as.numeric(PreciosHist$Close[MinLow_G]),
                       linetype= "dashed", size=.5, colour = "dark grey", alpha=0.35)

      }

    Grafica_2 <- Grafica_1 + 
      
      # -- Punto Exterior High
      geom_point(aes(x = PreciosHist$TimeStamp[MaxHigh_G],
                     y=as.numeric(PreciosHist$Close[MaxHigh_G])), size=4, colour="steel blue") +
      
      # -- Punto Interior High
      geom_point(aes(x = PreciosHist$TimeStamp[MaxHigh_G],
                     y=as.numeric(PreciosHist$Close[MaxHigh_G])), size=2, colour="white") + 
      
      # -- Punto Exterior Low
      geom_point(aes(x = PreciosHist$TimeStamp[MinLow_G],
                     y=as.numeric(PreciosHist$Close[MinLow_G])),
                 size=4, colour="steel blue") +
      
      # -- Punto Interior Low
      geom_point(aes(x = PreciosHist$TimeStamp[MinLow_G],
                     y=as.numeric(PreciosHist$Close[MinLow_G])),
                 size=2, colour="white") +
      
      # -- Punto Exterior Close
      geom_point(aes(x = last(PreciosHist$TimeStamp),
                     y = as.numeric(last(PreciosHist$Close))),
                 size=4, colour="steel blue") +
      
      # -- Punto Interior Close
      geom_point(aes(x = last(PreciosHist$TimeStamp),
                     y = as.numeric(last(PreciosHist$Close))),
                 size=2, colour="white") +
      
      # -- Punto Exterior Open
      geom_point(aes(x = first(PreciosHist$TimeStamp),
                     y = as.numeric(first(PreciosHist$Close))),
                 size=4, colour="steel blue") +
      
      # -- Punto Interior Open
      geom_point(aes(x = first(PreciosHist$TimeStamp),
                     y = as.numeric(first(PreciosHist$Close))),
                 size=2, colour="white") +
    
      # -- Open to Low
      geom_segment(x = first(PreciosHist$TimeStamp), xend = PreciosHist$TimeStamp[MinLow_G],
                   y = as.numeric(first(PreciosHist$Close)),
                   yend = as.numeric(PreciosHist$Close[MinLow_G]),
                   linetype= "dashed", size=.5, colour = "dark grey", alpha=0.35) +
      
      # -- Open to High
      geom_segment(x = first(PreciosHist$TimeStamp), xend = PreciosHist$TimeStamp[MaxHigh_G],
                   y = as.numeric(first(PreciosHist$Close)),
                   yend = as.numeric(PreciosHist$Close[MaxHigh_G]),
                   linetype= "dashed", size=.5, colour = "dark grey", alpha=0.35) +
      
      labs(x=NULL, y=NULL, title=NULL) +  scale_y_continuous(
        breaks = round(seq(round(min(PreciosHist$Close),5),
                           round(max(PreciosHist$Close),5),
        (round(max(PreciosHist$Close),5) - round(min(PreciosHist$Close),5))/12),5)) +
  
      scale_x_datetime(breaks = as.POSIXct(x_escala, origin ="1970-01-01"),
                       labels = date_format("%d/%m/%y %H:%M"))
    
    

  } else Grafica <- NULL
  
  DD_Final <- list("Datos" = PreciosHist, "Grafica" = Grafica, 
                   "DD" = list("Orden" = ifelse(dd_min<dd_max, "Low", "High"),
                               "Valores" = list("Low" = list("V_low" = PreciosHist$Low[dd_min],
                                                             "F_low" = PreciosHist$TimeStamp[dd_min]),
                                                "High" = list("V_high" = PreciosHist$High[dd_max],
                                                              "F_high" = PreciosHist$TimeStamp[dd_max]))))
  return(DD_Final)
}

# -- -------------------------------------------------------------------- PnL de una operacion -- #
# -- -------------------------------------------------------------------------------------------- #

PnL <- function(p_tipo, p_orden, p_open, p_high, p_low, p_close, p_mpip, p_vpip, p_tp, p_sl) {
  
  i <- 10
  p_tipo  <- p_OA_Totales$Operacion_g[i]
  p_orden <- p_OA_Totales$Orden[i]
  p_open  <- p_OA_Totales$Open[i]
  p_high  <- p_OA_Totales$High[i]
  p_low   <- p_OA_Totales$Low[i]
  p_close <- p_OA_Totales$Close[i]
  p_mpip  <- 10000
  p_vpip  <- 1
  p_tp    <- 40
  p_sl    <- 20
  
  vl <- (p_open - p_low)*p_mpip
  vc <- (p_open - p_close)*p_mpip
  vh <- (p_open - p_high)*p_mpip
  
  ch <- (p_high - p_open)*p_mpip
  cc <- (p_close - p_open)*p_mpip
  cl <- (p_low - p_open)*p_mpip
  
  #  -- -------------------------------------------------- Venta + High primero + SI SL -- (1) -- #
  if (p_tipo == -1 & p_orden == "High" & vh <= -p_sl){
    mensaje <- "Caso (1): Venta & High primero & SL si alcanzado"
    pnl_pip <- -p_sl
    pnl_usd <- -p_sl*p_vpip
    
    #  -- ------------------------------------------ Venta + High primero + NO SL + SI TP -- (2) -- #
  } else if (p_tipo == -1 & p_orden == "High" & vh > -p_sl & vl >= p_tp) {
    mensaje <- "Caso (2): Venta & High primero & SL no alcanzado & TP si alcanzado"
    pnl_pip <- p_tp
    pnl_usd <- p_tp*p_vpip
    
    #  -- ---------------------------------- Venta + High primero + NO SL + NO TP + Close -- (3) -- #
  } else if (p_tipo == -1 & p_orden == "High" & vh > -p_sl & vl < p_tp) {
    mensaje <- "Caso (3): Venta & High primero & SL no alcanzado & TP no alcanzado & Cierre al close"
    pnl_pip <- vc
    pnl_usd <- vc*p_vpip
    
    #  -- --------------------------------------------------- Venta + Low primero + SI TP -- (4) -- #
  } else if (p_tipo == -1 & p_orden == "Low" & vl >= p_tp) {
    mensaje <- "Caso (4): Venta & Low primero & TP si alcanzado"
    pnl_pip <- p_tp
    pnl_usd <- p_tp*p_vpip
    
    #  -- ------------------------------------------- Venta + Low primero + NO TP + SI SL -- (5) -- #
  } else if (p_tipo == -1 & p_orden == "Low" & vl < p_tp & vh <= -p_sl) {
    mensaje <- "Caso (5): Venta & Low primero & TP no alcanzado & SL si alcanzado"
    pnl_pip <- -p_sl
    pnl_usd <- -p_sl*p_vpip
    
    #  -- ----------------------------------- Venta + Low primero + NO TP + NO SL + Close -- (6) -- #
  } else if (p_tipo == -1 & p_orden == "Low" & vl < p_tp & vh > -p_sl) {
    mensaje <- "Caso (6): Venta & Low primero & TP no alcanzado & SL no alcanzado & Cierre al close"
    pnl_pip <- vc
    pnl_usd <- vc*p_vpip
    
    #  -- ------------------------------------------------- Compra + High primero + SI TP -- (7) -- #
  } else if (p_tipo == 1 & p_orden == "High" & p_tp <= ch) {
    mensaje <- "Caso (7): Compra & High primero & TP si alcanzado"
    pnl_pip <- p_tp
    pnl_usd <- p_tp*p_vpip
    
    #  -- ----------------------------------------- Compra + High primero + NO TP + SI SL -- (8) -- #
  } else if (p_tipo == 1 & p_orden == "High" & p_tp > ch & -p_sl > cl) {
    mensaje <- "Caso (8): Compra & High primero & TP no alcanzado & SL si alcanzado"
    pnl_pip <- -p_sl
    pnl_usd <- -p_sl*p_vpip
    
    #  -- --------------------------------- Compra + High primero + NO TP + NO SL + Close -- (9) -- #
  } else if (p_tipo == 1 & p_orden == "High" & p_tp > ch & -p_sl <= cl) {
    mensaje <- "Caso (9): Compra & High primero & TP no alcanzado & SL no alcanzado & Cierre al close"
    pnl_pip <- cc
    pnl_usd <- cc*p_vpip
    
    #  -- ------------------------------------------------- Compra + Low primero + SI SL -- (10) -- #
  } else if (p_tipo == 1 & p_orden == "Low" & -p_sl > cl) {
    mensaje <- "Caso (10): Compra & Low primero & SL si alcanzado"
    pnl_pip <- -p_sl
    pnl_usd <- -p_sl*p_vpip
    
    #  -- ----------------------------------------- Compra + Low primero + NO SL + SI TP -- (11) -- #
  } else if (p_tipo == 1 & p_orden == "Low" & -p_sl <= cl & p_tp <= ch) {
    mensaje <- "Caso (11): Compra & Low primero & SL no alcanzado & TP si alcanzado"
    pnl_pip <- p_tp
    pnl_usd <- p_tp*p_vpip
    
    #  -- --------------------------------- Compra + Low primero + NO SL + NO TP + Close -- (12) -- #
  } else if (p_tipo == 1 & p_orden == "Low" & -p_sl <= cl & p_tp > ch) {
    mensaje <- "Caso (12): Compra & Low primero & SL no alcanzado & TP no alcanzado & Cierre al close"
    pnl_pip <- cc
    pnl_usd <- cc*p_vpip
    
  }
  
  Resultado <- list("mensaje" = mensaje, "pnl_pip" = pnl_pip, "pnl_usd" = pnl_usd)
  
  return(Resultado)
}

# -- -------------------------------------------------------- Bayes : P(H|E) = P(E|H)P(H)/P(E) -- #
# -- -------------------------------------------------------------------------------------------- #

BayesProb <- function(Datos, Estado, Direcc) {
  
  # Datos  <- p_Datos
  # Estado <- p_Estado
  # Direcc <- p_Direcc
  
  # -- -- P(E|H) = Proporción de veces que estuvo en estado actual y continuo la tendencia
  D_EH_1 <- Datos[which(Datos[, Direcc] == Estado)+1, ] # Veces que estuvo en estado actual
  D_EH_2 <- D_EH_1[which(D_EH_1[, Direcc] > Estado), ]  # Dado que estuvo, que si continuo
  B_EH   <- round(length(D_EH_2[,1])/length(Datos[,1]), 6)
  
  # -- -- P(H) = Proporción de veces continuo la tendencia desde cualquier estado
  D_H <- Datos[which(Datos[, Direcc] >= 2), ]
  B_H <- round(length(D_H[,1])/length(Datos[,1]), 6)
  
  # -- -- P(E) = Proporción de veces que estuvo en el estado actual
  D_E <- Datos[which(Datos[, Direcc] == Estado), ]
  B_E <- round(length(D_E[,1])/length(Datos[,1]), 6)
  
  # -- -- P(H|E) = Probabilidad de continuar la tendencia dado que esté en un estado actual
  B_HE <- round(B_EH*B_H/B_E, 6)
  
  # P(E|H) dado que esta en 3 cuantas veces siguio a 4
  # P(H) cuantas veces siguio con una tendencia de n a n+1
  # P(E) cuantas veces estuvo en 3
  # P(H|E) que siga a 4 dado que esta en 3
  
  # P(E|H) can be greater than P(E), but P(E|H)P(H) should never be greater than P(E),
  
  Resultado <- list("EH" = list("B" = B_EH, "D" = D_EH_2,
                                "M" = "P(E|H) dado estado actual haya continuado con la tendencia"),
                    "H"  = list("B" = B_H,  "D" = D_H,
                                "M" = "P(H) que continue con la tendencia"),
                    "E"  = list("B" = B_E,  "D" = D_E,
                                "M" = "P(E) de estado actual"),
                    "HE" = list("B" = B_HE,
                                "M" = "P(H|E) continue la tendencia dado que esta en estado actual"))
  return(Resultado)
}
