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

