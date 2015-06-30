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

downloader::source_url("http://bit.ly/BTCAPIsConnector",prompt=FALSE,quiet=TRUE)

# -- AMERICA Stocks & Indices ------------------------------------------------------------------- #

AmazonD1 <- OYStockD1("AMZN","yahoo",Sys.Date()-100,Sys.Date())
EbayD1   <- OYStockD1("EBAY","yahoo",Sys.Date()-100,Sys.Date())
ExpedD1  <- OYStockD1("EXPE","yahoo",Sys.Date()-100,Sys.Date())
IPCD1    <- OYStockD1("^MXX","yahoo",Sys.Date()-100,Sys.Date())

DJIH1    <- HisPrices("H1","US30_USD",250)            # Dow Jones
NasH1    <- HisPrices("H1","NAS100_USD",250)          # Nasdaq
SP500H1  <- HisPrices("H1","SPX500_USD",250)          # SP500
US2000H1 <- HisPrices("H1","US2000_USD",250)          # Russel 2000

USB05YH1 <- HisPrices("H1","USB05Y_USD",250)          # USA 5yr
USB10YH1 <- HisPrices("H1","USB10Y_USD",250)          # USA 10yr
USB30YH1 <- HisPrices("H1","USB30Y_USD",250)          # USA 30yr

# -- EUROPE Stocks, Indices & Bonds ------------------------------------------------------------- #

CH20H1   <- HisPrices("H1","CH20_CHF",250)            # Swiss Stock Market Index
FR40H1   <- HisPrices("H1","FR40_EUR",250)            # French CAC 40 
EU50H1   <- HisPrices("H1","EU50_EUR",250)            # EuroStoxx 50
UK100H1  <- HisPrices("H1","UK100_GBP",250)           # FETSE 100 
DE30H1   <- HisPrices("H1","DE30_EUR",250)            # DAX 30

DE10YBH1 <- HisPrices("H1","DE10YB_EUR",250)          # Germany 10Yr 
UK10YBH1 <- HisPrices("H1","UK10YB_GBP",250)          # UK 10Yr

# -- ASIA Stocks, Indices & Bonds --------------------------------------------------------------- #

HK33H1  <- HisPrices("H1","HK33_HKD",250)             # Hang Seng Index
JP225H1 <- HisPrices("H1","JP225_USD",250)            # Nikkei 225
SG30H1  <- HisPrices("H1","SG30_SGD",250)             # Singapore 30

# -- ForEx Market ------------------------------------------------------------------------------- #

EURHKDH1 <- HisPrices("H1","EUR_HKD",250)             # Euro Vs Hong Kong Dollar
EURSGDH1 <- HisPrices("H1","EUR_SGD",250)             # Euro Vs Singapore Dollar
GBPHKDH1 <- HisPrices("H1","GBP_HKD",250)             # Great Britain Pound Vs Hong Kong Dollar
GBPSGDH1 <- HisPrices("H1","GBP_SGD",250)             # Great Britain Pound Vs Singapore Dollar
GBPUSDH1 <- HisPrices("H1","GBP_USD",250)             # Great Britain Pound Vs USA Dollar
SGDHKDH1 <- HisPrices("H1","SGD_HKD",250)             # Singapore Dollar Vs Hong Kong Dollar
USDCNHH1 <- HisPrices("H1","USD_CNH",250)             # USA Dollar Vs Chinesse Ramnibi
USDCNYH1 <- HisPrices("H1","USD_CNY",250)             # USA Dollar Vs Chinesse Yuan
USDMXNH1 <- HisPrices("H1","USD_MXN",250)             # USA Dollar Vs Mexican Peso
USDSGDH1 <- HisPrices("H1","USD_SGD",250)             # USA Dollar Vs Singapore Dollar
USDTWDH1 <- HisPrices("H1","USD_TWD",250)             # USA Dollar Vs Tawainesse Dollar

UsdBrlD1 <- OYFxPairD1("BRL=X","yahoo",Sys.Date()-100,Sys.Date())  # USA Dollar Vs Brazilean Real

# -- Order Book --------------------------------------------------------------------------------- #

meXBTBtcMxnOB <- meXBTOrderBook("btcmxn")
meXBTBtcUsdOB <- meXBTOrderBook("btcusd")
BitexlaOB <- BitexlaOrderBook(Sys.time())
BaseBitOB <- BaseBitOrderBook(Sys.time())
btcxeOB  <- btcxeOrderBook(Sys.date())
BitsoOB  <- BitsoOrderBook(Sys.time())
FoxBitOB <- FoxBitOrderBook(Sys.time())

# -- Historical BitCoin Prices ------------------------------------------------------------------ #

meXBTBtcUsd <- meXBTHistoricPrices("btcusd","America/Mexico_City",650)
meXBTBtcMxn <- meXBTHistoricPrices("btcmxn","America/Mexico_City",747)
btcxeH  <- btcxeHistoricPrices(Sys.date())

# -- BitCoin Tickers ---------------------------------------------------------------------------- #

btcxeT   <- btcxeTrades(Sys.time())
BitsoT   <- BitsoTrades(Sys.time())
FoxBitT  <- FoxBitTrades(Sys.date())
BitexlaT <- BitexlaTrades(Sys.time())
BitexlaT24 <- Bitexla24HistTrades(Sys.time())
BaseBitTrades <- BaseBitTrades(Sys.time())
btcxeSt <- btcxeStats(Sys.date())

meXBTTickBtcMxnTicker <- meXBTTicker("btcmxn")
BitsoBtcMxnTicker <- BitsoT[1,]
BaseBitTicker <- BaseBitTicker(Sys.time())

meXBTTickBtcUsdTicker <- meXBTTicker("btcusd")
BitexlaTicker <- BitexlaTicker(Sys.time())
btcxeTTicker  <- btcxeT[1,]

# -- Regional Time Series Indices plot ---------------------------------------------------------- #

NewSG30H1 <- data.frame(paste(SG30H1$Date,SG30H1$Time,sep=" "),SG30H1[,3:7])
colnames(NewSG30H1) <- c("TimeStamp","Open","High","Low","Close","TickVolume")

NewJP225H1 <- data.frame(paste(JP225H1$Date,JP225H1$Time,sep=" "),JP225H1[,3:7])
colnames(NewJP225H1) <- c("TimeStamp","Open","High","Low","Close","TickVolume")

NewHK33H1 <- data.frame(paste(HK33H1$Date,HK33H1$Time,sep=" "),HK33H1[,3:7])
colnames(NewHK33H1) <- c("TimeStamp","Open","High","Low","Close","TickVolume")

MultipleData <- data.frame(seq(1,250,1), NewSG30H1$Close, NewJP225H1$Close, NewHK33H1$Close,
                NewJP225H1$Close, NewHK33H1$Close)
colnames(MultipleData) <- c("TimeStamp","SG30","JP225","HK33","JP225(2)","HK33(2)")

MultiData <<- data.frame(MultipleData[,1],
MultipleData[,2]/max(MultipleData[,2]),MultipleData[,3]/max(MultipleData[,3]),
MultipleData[,4]/max(MultipleData[,4]),MultipleData[,5]/max(MultipleData[,5]),
MultipleData[,6]/max(MultipleData[,6]))

colnames(MultiData) <- colnames(MultipleData)
MultiData <- melt(MultiData, id = "TimeStamp", 
variable.name = "Assets", value.name = "NormalizedPrice")

ggplot(MultiData, aes(x=TimeStamp,y=NormalizedPrice),group=TimeStamp)   +
geom_line(aes(colour = Assets), linetype = 1, size = 1.5)    +
labs(title = "Stocks", x = "TimeStamp", y = "Normalized Price") +
scale_color_manual(values=c("blue","dark grey","dark blue","dark grey","dark blue"))


