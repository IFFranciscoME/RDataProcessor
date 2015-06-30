# -- ----------------------------------------------------------------------------------- #
# -- meXBT Machine Learning, Artificial Intelligence and Stochastic calculus codes ----- #
# -- License: PRIVATE and Right Reserved ----------------------------------------------- #
# -- ----------------------------------------------------------------------------------- #

#downloader::source_url("http://bit.ly/BTCAPIsConnector",prompt=FALSE,quiet=TRUE)

AmazonD1 <- OYStockD1("AMZN","yahoo",Sys.Date()-100,Sys.Date())
EbayD1   <- OYStockD1("EBAY","yahoo",Sys.Date()-100,Sys.Date())
ExpedD1  <- OYStockD1("EXPE","yahoo",Sys.Date()-100,Sys.Date())
IPCD1    <- OYStockD1("^MXX","yahoo",Sys.Date()-100,Sys.Date())
DJIAD1   <- OYStockD1("^DJI","yahoo",Sys.Date()-100,Sys.Date())
NasdaqD1 <- OYStockD1("^IXIC","yahoo",Sys.Date()-100,Sys.Date())
SP500D1  <- OYStockD1("^GSPC","yahoo",Sys.Date()-100,Sys.Date())

UsdSgdD1 <- OYFxPairD1("SGD=X","yahoo",Sys.Date()-100,Sys.Date())
UsdBrlD1 <- OYFxPairD1("BRL=X","yahoo",Sys.Date()-100,Sys.Date())
UsdCnyD1 <- OYFxPairD1("CNY=X","yahoo",Sys.Date()-100,Sys.Date())
UsdMxnD1 <- OYFxPairD1("MXN=X","yahoo",Sys.Date()-100,Sys.Date())
EurUsdD1 <- OYFxPairD1("EUR=X","yahoo",Sys.Date()-100,Sys.Date())
XauUsdD1 <- OYFxPairD1("XAU/USD","oanda",Sys.Date()-100,Sys.Date())
EurUsdD1[,2:5] <- 1/EurUsdD1[,2:5]
XauUsdD1$Price <- 1/XauUsdD1$Price

meXBTBtcMxnOB <- meXBTOrderBook("btcmxn")
meXBTBtcUsdOB <- meXBTOrderBook("btcusd")
BitexlaOB  <- BitexlaOrderBook(Sys.time())
BaseBitOB  <- BaseBitOrderBook(Sys.time())
btcxeOB  <- btcxeOrderBook(Sys.date())
BitsoOB  <- BitsoOrderBook(Sys.time())
FoxBitOB <- FoxBitOrderBook(Sys.time())

meXBTBtcUsd <- meXBTHistoricPrices("btcusd","America/Mexico_City",650)
meXBTBtcMxn <- meXBTHistoricPrices("btcmxn","America/Mexico_City",747)
btcxeH  <- btcxeHistoricPrices(Sys.date())

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

