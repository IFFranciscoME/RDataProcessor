# -- -------------------------------------------------------------------------------------------- #
# -- meXBT Machine Learning, Artificial Intelligence and Stochastic calculus codes -------------- #
# -- License: PRIVATE and Right Reserved -------------------------------------------------------- #
# -- -------------------------------------------------------------------------------------------- #

if (!require(base)) install.packages('base', quiet = TRUE)
suppressMessages(library (base))       # basic functions
if (!require(downloader)) install.packages('downloader', quiet = TRUE)
suppressMessages(library (downloader)) # basic functions
if (!require(grid)) install.packages('grid', quiet = TRUE)
suppressMessages(library (grid))       # Grid modifications for plotting
if (!require(gridExtra)) install.packages('gridExtra', quiet = TRUE)
suppressMessages(library (gridExtra))  # Extra grid for text positioning 
if (!require(ggplot2)) install.packages('ggplot2', quiet = TRUE)
suppressMessages(library (ggplot2))    # Gramatics of Graphics
if (!require(httr)) install.packages('httr', quiet = TRUE)
suppressMessages(library (httr))       # http utilities
if (!require(jsonlite)) install.packages('jsonlite', quiet = TRUE)
suppressMessages(library (jsonlite))   # JSON parser
if (!require(knitr)) install.packages('knitr', quiet = TRUE)
suppressMessages(library (knitr))      # LaTeX + R Typesetting
if (!require(lubridate)) install.packages('lubridate', quiet = TRUE)
suppressMessages(library (lubridate))  # treatment and modification for dates
if (!require(orderbook)) install.packages('orderbook', quiet = TRUE)
suppressMessages(library (orderbook))  # calculations and visuaization for orderbook
if (!require(printr)) install.packages('printr', quiet = TRUE)
suppressMessages(library (printr))     # Printer functions for knitr
if (!require(quantmod)) install.packages('quantmod', quiet = TRUE)
suppressMessages(library (quantmod))   # Stock Prices and Dividends from YAHOO
if (!require(RCurl)) install.packages('RCurl', quiet = TRUE)
suppressMessages(library (RCurl))      # URL reading utilities
if (!require(reshape2)) install.packages('reshape2', quiet = TRUE)
suppressMessages(library (reshape2))   # Use the function MELT
if (!require(simsalapar)) install.packages('simsalapar', quiet = TRUE)
suppressMessages(library (simsalapar)) # Special Error Handler "TryCatch()"
if (!require(scales)) install.packages('scales', quiet = TRUE)
suppressMessages(library (scales))     # Add comma separator Y-axis en plots
if (!require(tseries)) install.packages('tseries', quiet = TRUE)
suppressMessages(library (tseries))    # Time series utilities
if (!require(xts)) install.packages('xts', quiet = TRUE)
suppressMessages(library (xts))        # Time series utilities
if (!require(zoo)) install.packages('zoo', quiet = TRUE)
suppressMessages(library (zoo))        # Time series utilities

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
  geom_hline(yintercept=-ciline, color="dark grey", size=0.35, linetype = "dashed")                                                     +
  geom_hline(yintercept= ciline, color="dark grey", size=0.35, linetype = "dashed")                                                     +
  geom_hline(yintercept = 0, color = "dark grey",size = 0.25)                          +
  scale_fill_hue(breaks = 0:1,labels=c("Not Significative",
  "Significative"),h=c(260, 235)) + scale_x_continuous(breaks = seq(1,22,1))           +
  theme(panel.background = element_rect(size = .75,colour = "dark grey",fill="white"), 
  axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
  plot.title = element_text(size = 14,vjust = 1), legend.position = "bottom",
  legend.title = element_blank(),legend.text = element_text(colour="blue",size = 10))
  return(facgg)
}
