# -- -------------------------------------------------------------------------------------------- #
# -- Initial Developer: FranciscoME ------------------------------------------------------------- #
# -- GitHub Repossitory: https://github.com/IFFranciscoME/DataProcessor ------------------------- #
# -- License: GNU General Public License -------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

# -- -------------------------------------------------------------------------------------------- #
# -- AutoCorrelation and Partial AutoCorrelation Calculations and Graph ------------------------- #
# -- -------------------------------------------------------------------------------------------- #

AutoCorrelation <- function(x, type, LagMax)
{
  ciline <- 2/sqrt(length(x))
  bacf   <- acf(x, plot = FALSE, lag.max = LagMax, type = type)
  bacfdf <- with(bacf, data.frame(lag, acf))
  #bacfdf <- bacfdf[-seq(1, 0),]
  Sig_nc <- (abs(bacfdf[,2]) > abs(ciline))^2
  bacfdf <- cbind(bacfdf, Sig_nc)
  return(bacfdf)
}

# -- -------------------------------------------------------------------------------------------- #
# -- Stationarity Dickey-Fuller Test ------------------------------------------------------------ #
# -- -------------------------------------------------------------------------------------------- #

ADFTestedSeries <- function(DataFrame,Column,CnfLvl)
{
  d <- 0
  Column <- Column
  CnfLvl <- CnfLvl
  DataFrame <- DataFrame
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
          message <- "Very Unstable Series"
          StationarySeries <- data.frame(DataFrame)
        } else StationarySeries <- data.frame(Series3D,d)
      } else StationarySeries <- data.frame(Series2D,d)
    } else StationarySeries <- data.frame(Series1D,d)
  } else StationarySeries <- data.frame(DataFrame,d)
  return (StationarySeries)
}