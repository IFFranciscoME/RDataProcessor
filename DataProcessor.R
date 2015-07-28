# -- -------------------------------------------------------------------------------------------- #
# -- meXBT Machine Learning, Artificial Intelligence and Stochastic calculus codes -------------- #
# -- License: PRIVATE and Right Reserved -------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

# -- Install and/or load packages dependencies -------------------------------------------------- #

Pkg <- c("base","digest","downloader","fBasics","forecast","grid","gridExtra",
"jsonlite","lubridate","moments","orderbook","openssl","PerformanceAnalytics","plyr","quantmod",
"Quandl","reshape2","RCurl","stats","scales","simsalapar","tseries","TTR","TSA","xts","xts","zoo")

inst <- Pkg %in% installed.packages()
if(length(Pkg[!inst]) > 0) install.packages(Pkg[!inst])
instpackages <- lapply(Pkg, library, character.only=TRUE)

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
