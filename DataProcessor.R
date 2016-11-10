# -- -------------------------------------------------------------------------------------------- #
# -- Initial Developer: FranciscoME ------------------------------------------------------------- #
# -- GitHub Repossitory: https://github.com/IFFranciscoME/DataProcessor ------------------------- #
# -- License: TradingPal ------------------------------------------------------------------------ #
# -- -------------------------------------------------------------------------------------------- #

# -- -------------------------------------------------------------------------------------------- #
# -- AutoCorrelation and Partial AutoCorrelation Calculations and Graph ------------------------- #
# -- -------------------------------------------------------------------------------------------- #

AutoCorrelation <- function(x, type, LagMax, IncPlot) {
  ciline <- 2/sqrt(length(x))
  bacf   <- acf(x, plot = IncPlot, lag.max = LagMax, type = type)
  bacfdf <- with(bacf, data.frame(lag, acf))
  #bacfdf <- bacfdf[-seq(1, 0),]
  Sig_nc <- (abs(bacfdf[,2]) > abs(ciline))^2
  bacfdf <- cbind(bacfdf, Sig_nc)
  return(bacfdf) }

# -- -------------------------------------------------------------------------------------------- #
# -- Stationarity Dickey-Fuller Test ------------------------------------------------------------ #
# -- -------------------------------------------------------------------------------------------- #

ADFTestedSeries <- function(DataFrame,Column,CnfLvl) {
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
  return (StationarySeries) }

# -- -------------------------------------------------------------------------------------------- #
# -- DrawDown y DrawUp -------------------------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

DDMaxMin <- function(Data,l) {
  
  Fecha_HP1 <- substr(as.character(Data$Open.TimeStamp[l]),1,16)
  Fecha_HP2 <- substr(as.character(Data$Close.TimeStamp[l]),1,16)
  
  Fecha_V1 <- as.Date(Fecha_HP1)-1
  Fecha_V2 <- as.Date(Fecha_HP2)+3
  
  ifelse(as.POSIXlt(Fecha_V2)$wday == 6, Fecha_V2 <- Fecha_V2+1, Fecha_V2 <- Fecha_V2)
  ifelse(as.POSIXlt(Fecha_V2)$wday == 6, Fecha_V1 <- Fecha_V1-1, Fecha_V1 <- Fecha_V1)
  
  PreciosHist <- HisPrices(OA_At,OA_Gn,OA_Da,OA_Ta,OA_Ak,OA_In, Start=Fecha_V1, End=Fecha_V2,
                           Count = NULL)
  
  PreciosHist$TimeStamp <- as.POSIXct(PreciosHist$TimeStamp, origin ="1970-01-01",
                                      format = "%Y-%m-%d %H:%M", tz = "America/Monterrey")
  PreciosHist[,2:5] <- round(PreciosHist[,2:5],5)
  
  FechasCH <- PreciosHist$TimeStamp
  
  Fecha_HP1 <- as.POSIXct(Fecha_HP1, origin = "1970-01-01", tz = "America/Monterrey")
  Fecha_HP2 <- as.POSIXct(Fecha_HP2, origin = "1970-01-01", tz = "America/Monterrey")
  
  FechaCercana1 <- which(abs(FechasCH -  Fecha_HP1) == min(abs(FechasCH - Fecha_HP1)))
  FechaCercana2 <- which(abs(FechasCH -  Fecha_HP2) == min(abs(FechasCH - Fecha_HP2)))
  
  PreciosHist_DD <- PreciosHist[FechaCercana1:FechaCercana2,]
  
  MaxHigh <- which.max(PreciosHist_DD$High)
  MinLow  <- which.min(PreciosHist_DD$Low)
  
  P0_OrdenMaxMin <- ifelse(MinLow <= MaxHigh, "MinLow", "MaxHigh")
  
  P1_DD <- abs((Data$Open[l]  - Data$Low[l])*MultPip)
  P2_DU <- abs((Data$High[l] - Data$Open[l])*MultPip)
  
  DDU_Final <- list(P0_OrdenMaxMin = P0_OrdenMaxMin,
                    P1_DDCompra = P1_DD,
                    P2_DDVenta  = P2_DU,
                    P3_MaxHigh = Data$High[l],
                    P4_MinLow  = Data$Low[l])
  
  return(DDU_Final)
}
