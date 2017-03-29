
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
  
  # Fecha_Inicial = as.character(ENT_DD_DU_OP$OpenTime[i])
  # Fecha_Final = as.character(ENT_DD_DU_OP$CloseTime[i])
  # Instrumento = ENT_DD_DU_OP$Symbol[i]
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
    
    HisSpreads(OA_At,OA_Gn,OA_Da,OA_Ta,OA_Ak,Instrumento,Fechas[x],Fechas[x+1], Count=NULL)
    
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
