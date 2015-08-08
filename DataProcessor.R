# -- -------------------------------------------------------------------------------------------- #
# -- Initial Developer: FranciscoME ------------------------------------------------------------- #
# -- GitHub Repossitory: https://github.com/IFFranciscoME/DataProcessor ------------------------- #
# -- License: GNU General Public License -------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

# -- AutoCorrelation and Partial AutoCorrelation Calculations and Graph ------------------------- #

AutoCorrelation <- function(x, type, YAxisText)
{
  ejeX   <- "Lags"
  ciline <- 2/sqrt(length(x))
  bacf   <- acf(x, plot = FALSE, lag.max = 22, type = type)
  bacfdf <- with(bacf, data.frame(lag, acf))
  bacfdf <- bacfdf[-seq(1, 0),]
  Sig_nc <- (abs(bacfdf[,2]) > abs(ciline))^2
  bacfdf <- cbind(bacfdf, Sig_nc)
  
  facgg <- qplot(lag,acf,data=bacfdf,geom="bar",width=.75, stat = "identity",
  position = "identity", ylab = "Statistical Significance", xlab = ejeX,
  fill = factor(Sig_nc)) + labs(title = YAxisText)                                     +  
  geom_hline(yintercept=-ciline, color="dark grey", size=0.35, linetype = "dashed")    +
  geom_hline(yintercept= ciline, color="dark grey", size=0.35, linetype = "dashed")    +
  geom_hline(yintercept = 0, color = "dark grey",size = 0.25)                          +
  scale_fill_hue(breaks = 0:1,labels=c("Not Significative",
  "Significative"),h=c(260, 235)) + scale_x_continuous(breaks = seq(1,22,1))           +
  theme(panel.background = element_rect(size = .75,colour = "dark grey",fill="white"), 
  axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
  plot.title = element_text(size = 14,vjust = 1), legend.position = "bottom",
  legend.title = element_blank(),legend.text = element_text(colour="blue",size = 10))
  return(facgg)
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