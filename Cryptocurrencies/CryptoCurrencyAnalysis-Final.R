# Load libraries
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(magrittr)) install.packages("magrittr")
if (!require(corrplot)) install.packages("corrplot")
if (!require(treemap)) install.packages("treemap")
if (!require(zoo)) install.packages("zoo")

library(ggplot2)
library(magrittr)
library(corrplot)
library(treemap)
library(zoo)

# set current directory as working directory
setwd(".")

print("Loading Crytocurrencys...")
# All cryptocurrencies dataset
df.crypto.currencies <- read.csv(file="Coins-All.csv", header=TRUE, sep=",")

print("Loading Cryptocurrencies market...")
# Daily market dataset
df.crypto.daily.market <- read.csv(file="crypto-markets-all.csv", header=TRUE, sep=",")

print("Cleaning Data...")
# drop id, type, and convert dates
df.crypto.daily.market$id <- NULL
df.crypto.daily.market$type <- NULL
df.crypto.daily.market$datetime <- as.Date(df.crypto.daily.market$datetime)
str(df.crypto.daily.market)

# remove duplicates in any particular day
df.crypto.daily.market <- df.crypto.daily.market[!duplicated(df.crypto.daily.market[,c(6, 7)]),]
# sample of a particular date
df.crypto.daily.market[df.crypto.daily.market$datetime == '2018-08-01',]
str(df.crypto.daily.market)

print("Introducing missing values by approximation...")
# check and fix missing values
IntroduceMissingData <- function(data) {
  currencies <- unique(data$currency_slug)
  newrows <- do.call("rbind", lapply(currencies, FUN=GetMissingDateRows, data))
  data <- rbind(data, newrows)
  data <- data[order(data$currency_slug,data$datetime),]; rownames(data) <- 1:nrow(data) # Sort
  for (currency in currencies) {
    idx <- colSums(!is.na(data[data$currency_slug==currency,1:5])) > 1
    data[data$currency_slug==currency,c(idx,FALSE,FALSE)] <- na.approx(data[data$currency_slug==currency,c(idx,FALSE,FALSE)], na.rm=FALSE)
  }
  return(data)
}
GetMissingDateRows <- function(currency, data) {
  dates <- unique(data[data$currency_slug==currency,6])
  alldates <- seq(dates[1],dates[length(dates)],by="+1 day")
  missingdates <- setdiff(alldates, dates)
  return(data.frame(price_usd=rep(NA, length(missingdates)),
                    price_btc=rep(NA, length(missingdates)),
                    volume_usd=rep(NA, length(missingdates)),
                    market_cap_usd=rep(NA, length(missingdates)),
                    available_supply=rep(NA, length(missingdates)),
                    datetime=as.Date(missingdates, origin="1970-01-01"),
                    currency_slug=rep(currency, length(missingdates))))
}
df.crypto.daily.market <- IntroduceMissingData(df.crypto.daily.market) # For missing dates, insert fields and interpolate values (takes some time)

# taking into consideration the top 100 coins
df.crypto.currencies.top100 <- df.crypto.currencies[df.crypto.currencies$rank <= 100 & df.crypto.currencies$type == 'coin',]
df.crypto.currencies.top100$slug <- droplevels(df.crypto.currencies.top100$slug)

# filter by coin type only, dropping the tokens
df.crypto.currencies <- df.crypto.currencies[df.crypto.currencies$type == 'coin',]
df.crypto.currencies$name <- droplevels(df.crypto.currencies$name)
df.crypto.currencies$slug <- droplevels(df.crypto.currencies$slug)

# check structure of coins dataframes; all coins, top 100 coins
str(df.crypto.currencies.top100)
str(df.crypto.currencies)
df.crypto.currencies$id <- NULL

# re-level daily market columns
df.crypto.daily.market$currency_slug <- droplevels(df.crypto.daily.market$currency_slug)

# structure of daily market dataframe
str(df.crypto.daily.market)

# filter only those that are in the coins dataframe
df.crypto.daily.market <- df.crypto.daily.market[df.crypto.daily.market$currency_slug %in% df.crypto.currencies$slug,]
# filter on the other hand, coins in dataframe should be the same ones in 
# daily market data frame
df.crypto.currencies <- df.crypto.currencies[df.crypto.currencies$slug %in% df.crypto.daily.market$currency_slug, ]

# relevel after dropping coins slugs
df.crypto.daily.market$currency_slug <- droplevels(df.crypto.daily.market$currency_slug)
df.crypto.currencies$slug <- droplevels(df.crypto.currencies$slug)

df.crypto.daily.market[1:5,]
df.crypto.daily.market[1:5,c(6, 7)]

str(df.crypto.daily.market)
str(df.crypto.currencies)

print("Calculating statistics...")
## Calculate market statistics
# returns: return(t) = (price(t) - price(t-1)) / price(t-1)
# logreturns: logreturn(t) = ln(price(t)/price(t-1))
# annualized volatility: sd(logreturns per x days)*sqrt(trading days=365)
# herfindahl: sum of squares of competitor market shares
CalculateMarketStatistics <- function(data) {
  dates <- sort(unique(data$datetime))
  cap <- sapply(dates, FUN=function(date) sum(data[data$datetime==date, c(4)]))
  returns <- c(0,diff(cap)/cap[-length(cap)])
  logreturns <- c(0,log(cap[-1]/cap[-length(cap)]))
  volatility.30d <- sapply(1:length(logreturns), FUN=function(i) sd(logreturns[(max(i-30,0):i)]))*sqrt(365)
  volatility.90d <- sapply(1:length(logreturns), FUN=function(i) sd(logreturns[(max(i-90,0):i)]))*sqrt(365)
  herfindahl <- sapply(dates, FUN=function(date) sum((data[data$datetime==date,2]/sum(data[data$datetime==date,c(4)]))^2))
  data.frame(datetime=dates, cap=cap, return=returns, logreturn=logreturns, volatility.30d=volatility.30d, volatility.90d=volatility.90d, herfindahl=herfindahl)
}
market <- CalculateMarketStatistics(df.crypto.daily.market)

# market sample data
market[1:5,]

# Need a subset of the coins dataset
currencies <- df.crypto.currencies[df.crypto.currencies$type == 'coin',]
currencies$id <- NULL
currencies$rank <- NULL
currencies$slug <- droplevels(currencies$slug)

currencies <- currencies[currencies$slug %in% df.crypto.daily.market$currency_slug,]
currencies$slug <- droplevels(currencies$slug)
currencies$slug <- as.character(currencies$slug)

str(df.crypto.daily.market)
str(currencies)

# Calculate Market Capitalisation per currency
currencies$mcap <- sapply(currencies$slug, FUN=function(x) df.crypto.daily.market[df.crypto.daily.market$currency_slug==x & df.crypto.daily.market$datetime==max(df.crypto.daily.market[df.crypto.daily.market$currency_slug==x,]$datetime),]$market_cap_usd)

# Sort currencies by Market Capitalisation in descending order
currencies <- currencies[order(currencies$mcap,currencies$slug, decreasing=TRUE),]; rownames(currencies) <- 1:nrow(currencies)

# sample currencies data
currencies[1:5,]

# Calculate returns
df.crypto.daily.market$return <- Reduce(c,sapply(unique(df.crypto.daily.market$currency_slug), FUN=function(x) c(0,diff(df.crypto.daily.market[df.crypto.daily.market$currency_slug==x,]$price_usd)/(df.crypto.daily.market[df.crypto.daily.market$currency_slug==x,]$price_usd)[-length(df.crypto.daily.market[df.crypto.daily.market$currency_slug==x,]$price_usd)])))

# Calculate log return
df.crypto.daily.market$logreturn <- Reduce(c,sapply(unique(df.crypto.daily.market$currency_slug), FUN=function(x) c(0,log(df.crypto.daily.market[df.crypto.daily.market$currency_slug==x,]$price_usd[-1]/df.crypto.daily.market[df.crypto.daily.market$currency_slug==x,]$price_usd[-length(df.crypto.daily.market[df.crypto.daily.market$currency_slug==x,]$price_usd)]))))

df.crypto.daily.market[1:5,]

# Generates a dataframe with daily returns for a set of currencies, to be used further on for correlation
AnalyseReturnMarket <- function(currencies, data) {
  data <- reshape(data[data$currency_slug %in% currencies,c(6,7,9)], direction="wide", idvar="datetime", timevar="currency_slug")
  #colnames(data) <- c("datetime", sort(currencies))
  colnames(data) <- gsub(".*\\.","",colnames(data))
  data <- data[,c("datetime", currencies)]
  return(data)
}

df.to.correlate <- AnalyseReturnMarket(currencies[1:25,]$slug,df.crypto.daily.market[df.crypto.daily.market$datetime > as.Date("2016-12-31"),])

# Generates a dataframe with complete daily information for a set of currencies
AnalyseData <- function(currencies, data, market=NULL) {
  temp <- lapply(currencies, FUN=function(x) subset(data, currency_slug==x))
  temp <- Reduce(function(df1, df2) merge(df1, df2, by="datetime"), temp)
  if (length(currencies) > 1)
    colnames(temp) <- c("datetime", sapply(currencies, function(slug) sapply(colnames(data)[c(1, 2, 3, 4, 5, 7, 8, 9)], function(x) paste(x, slug, sep="_"))))
  if (!is.null(market))
    temp <- merge(temp, market, by="datetime")
  data.frame(temp)
}

data <- AnalyseData(c("bitcoin", "ethereum"), df.crypto.daily.market[df.crypto.daily.market$datetime>as.Date("2016-12-31"),])
data[1:5,]

# Calculate betas - https://en.wikipedia.org/wiki/Capital_asset_pricing_model
CalculateCurrencyBeta <- function(currency, data, market) {
  dates <- intersect(data[data$currency_slug==currency,]$datetime, market$datetime)
  return(cov(data[data$currency_slug==currency & data$datetime %in% dates,]$logreturn,
             market[market$datetime %in% dates,]$logreturn)/var(market[market$datetime %in% dates,]$logreturn))
}

currencies$beta <- sapply(currencies$slug, FUN=CalculateCurrencyBeta, df.crypto.daily.market[df.crypto.daily.market$datetime>as.Date("2016-12-31"),], market)

df.to.correlate[1:5,]

currencies[1:10,]

df.crypto.daily.market[1:5,]

# Sample Correlation for bitcoin vs ethereum return
df.correlation.bitcoin.etherium <- cor.test(df.to.correlate$bitcoin, df.to.correlate$ethereum, 
                    method = "pearson", use = "pairwise.complete.obs")
df.correlation.bitcoin.etherium

# significant correlation assuming alpha of 0.05
df.correlation.bitcoin.etherium$p.value

# A function to compute all the p-values given a matrix of values
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
CalculateCorrelationMTest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    matrix.correlation.pvalue<- matrix(NA, n, n)
    diag(matrix.correlation.pvalue) <- 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            tmp <- cor.test(mat[, i], mat[, j], ...)
            matrix.correlation.pvalue[i, j] <- matrix.correlation.pvalue[j, i] <- tmp$p.value
        }
    }
  colnames(matrix.correlation.pvalue) <- rownames(matrix.correlation.pvalue) <- colnames(mat)
  matrix.correlation.pvalue
}

matrix.correlation.pvalue <- CalculateCorrelationMTest(mat = as.matrix(df.to.correlate[,-1]))

df.market.today <- df.crypto.daily.market[df.crypto.daily.market$datetime == max(df.crypto.daily.market$datetime),]
df.market.today <-na.omit(df.market.today[,c(7, 4)])
df.market.today$market_cap_usd <- as.numeric(df.market.today$market_cap_usd)
df.market.today$formatted_market_cap <-  paste0(df.market.today$currency_slug,'\n','$',format(df.market.today$market_cap_usd,big.mark = ',',scientific = F, trim = T))
png(filename="PlotTreeMap.png", width=800, height=700, units="px")
treemap(df.market.today, index = 'formatted_market_cap', vSize = 'market_cap_usd', title = 'Cryptocurrency Market Cap', fontsize.labels=c(12, 8), palette='RdYlGn')
dev.off()

df.crypto.currencies.top10 <- df.crypto.currencies.top100[df.crypto.currencies.top100$rank <= 10,]
df.crypto.currencies.top10$slug <- droplevels(df.crypto.currencies.top10$slug)

# Some Descriptive statistics on cryptocurrencies available
#png(filename="PlotBarPlotAllCurrencies.png", width=800, height=700, units="px")
#qplot(data = df.crypto.currencies, x = type, xlab = 'Type', ylab = 'Count') + ggtitle("Plot of all cryptocurrencies counts")
#dev.off()
#png(filename="PlotBarPlotTop10CurrenciesDailyMarket.png", width=800, height=700, units="px")
#qplot(data = df.crypto.daily.market[df.crypto.daily.market$currency_slug %in% df.crypto.currencies[1:10,]$slug,], x = currency_slug, ylab = 'Count of daily market entries', xlab = 'Currency', main = 'Number of entries in the daily market per currency')
#dev.off()
#summary(df.crypto.daily.market)
#nrow(df.crypto.currencies[df.crypto.currencies$type == 'coin',])
#nrow(df.crypto.currencies[df.crypto.currencies$type == 'token',])
#nrow(df.crypto.daily.market)
#nrow(df.crypto.daily.market[df.crypto.daily.market$type == 'coin',])
#nrow(df.crypto.daily.market[df.crypto.daily.market$type == 'token',])

# Boxplot of Close Prices for Cryptocurrency Market
df.crypto.market.boxplot <- df.crypto.daily.market[df.crypto.daily.market$datetime >= '2017-01-01' & df.crypto.daily.market$currency_slug %in% df.crypto.currencies.top10$slug,]
df.crypto.market.boxplot$currency_slug <- droplevels(df.crypto.market.boxplot$currency_slug)
png(filename="PlotBoxPlot.png", width=800, height=700, units="px")
boxplot(market_cap_usd~currency_slug,data=df.crypto.market.boxplot, main="Cryptocurrencies Market > 2017-01-01", 
  	xlab="Market Cap", ylab="Market Cap USD", horizontal=TRUE, las = 1)


# Plot market cap, market return, market volatility 
# and herfindahl index https://en.wikipedia.org/wiki/Herfindahl_index
PlotMarket <- function(market) {
  p1 <- ggplot(market, aes(datetime, cap)) +
    geom_line() +
    labs(x="Date", y="Market cap", title="Overall market") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p2 <- ggplot(market, aes(datetime, logreturn)) +
    geom_line() +
    labs(x="Date", y="Log return") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p3 <- ggplot(market, aes(datetime, volatility.30d)) +
    geom_line() +
    labs(x="Date", y="Annualized volatility") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p4 <- ggplot(market, aes(datetime, herfindahl)) + geom_line() + labs(x="Date", y="Herfindahl index")
  ## convert plots to gtable objects
  library(gtable)
  library(grid) # low-level grid functions are required
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  g4 <- ggplotGrob(p4)
  g <- rbind(g1, g2, g3, g4, size="first") # stack the plots
  g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths) # use the largest widths
  # center the legend vertically
  g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
  grid.newpage()
  grid.draw(g)
  ggsave("PlotMarketStatistics.png", g, width=8, height=6, dpi=100, units="in")
}
PlotMarket(market)

plot.market.bitcoin <- ggplot(df.crypto.daily.market[df.crypto.daily.market$datetime >= '2017-01-01' & df.crypto.daily.market$currency_slug == 'bitcoin',], aes(datetime, price_usd)) +
      geom_line (color="royalblue3", size=1)+
      labs(y = "Price")+
      labs(x = 'Date')+
      stat_smooth(color = "red", fill = "red", method = "loess") + labs(y = "price") + labs(title = "Bitcoin USD price graph")
ggsave("PlotMarketStatisticsBitcoin.png", plot.market.bitcoin, width=8, height=6, dpi=100, units="in")
plot.market.bitcoin

plot.market.top25 <- ggplot(df.crypto.daily.market[df.crypto.daily.market$datetime >= '2017-01-01' & df.crypto.daily.market$currency_slug %in% currencies[1:25,]$slug,], aes(x=datetime, y=price_usd, colour=currency_slug)) +
  geom_line (size=1)+
  labs(y = "Price")+ 
  labs(x = "Date") +
  labs(title = "Top 25 Cryptocurrencies in USD price graph")
ggsave("PlotMarketStatisticsTop25.png", plot.market.top25, width=8, height=6, dpi=100, units="in")
plot.market.top25

# Plot currency cap, return and volatility for multiple currencies
PlotCurrencies <- function(data, slugs, plotfilename) {
  data <- data[data$currency_slug %in% slugs,]
  data$volatility.30d <- Reduce(c,sapply(unique(data$currency_slug), FUN=function(x) sapply(1:length(data[data$currency_slug==x,]$logreturn), FUN=function(i) sd(data[data$currency_slug==x,]$logreturn[(max(i-30,0):i)]))))*sqrt(365)
  p1 <- ggplot(data, aes(datetime, market_cap_usd, color=factor(currency_slug))) +
    geom_line() +
    labs(x="Date", y="Market cap", title=paste(slugs, collapse=", ")) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.title=element_blank())
  p2 <- ggplot(data, aes(datetime, logreturn, color=factor(currency_slug))) +
    geom_line() +
    labs(x="Date", y="Log return") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.title=element_blank())
  p3 <- ggplot(data, aes(datetime, volatility.30d, color=factor(currency_slug))) +
    geom_line() +
    labs(x="Date", y="Annualized volatility")
  ## convert plots to gtable objects
  library(gtable)
  library(grid) # low-level grid functions are required
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  g <- rbind(g1, g2, g3, size="first") # stack the plots
  g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths) # use the largest widths
  # center the legend vertically
  g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
  grid.newpage()
  grid.draw(g)
  ggsave(plotfilename, g, width=8, height=6, dpi=100, units="in")
}
PlotCurrencies(df.crypto.daily.market[df.crypto.daily.market$datetime >= '2017-01-01',], c("bitcoin"), 'PlotCurrenciesStatistics-Bitcoin.png')
PlotCurrencies(df.crypto.daily.market[df.crypto.daily.market$datetime >= '2017-01-01',], c("bitcoin","ethereum", "ripple"), 'PlotCurrenciesStatistics-Top3.png')
PlotCurrencies(df.crypto.daily.market[df.crypto.daily.market$datetime >= '2017-01-01',], df.crypto.currencies.top10$slug, 'PlotCurrenciesStatistics-Top10.png')


# Plot betas of top currencies against latest market cap
PlotBetaVSMarketCap <- function(num, currencies) {
  data <- currencies[order(currencies$mcap, decreasing=TRUE),] # Sort
  data <- data[0:num,]
  p <- ggplot(data, aes(x=mcap, y=beta))
  p + geom_point() +
    scale_x_log10() +
    geom_text(aes(label=name),hjust=0, vjust=0) +
    labs(title="Beta vs Market capitalisation", x="Market capitalisation [USD] (log scale)", y="Beta") +
    theme(legend.title=element_blank())
  ggsave("PlotBetaVSMarketCap.png", width=8, height=5, dpi=100, units="in")
}
PlotBetaVSMarketCap(10, currencies)

# Plot betas over time
PlotBetaTimeline <- function(currencies, mindays, maxdays, data, market) {
  data <- data[data$currency_slug %in% currencies,]
  dates <- intersect(data$datetime, market$datetime)
  result <- data.frame(datetime=as.Date(rep(dates, times=length(currencies)), origin="1970-01-01"), currency=rep(currencies,each=length(dates)))
  result$beta <- Reduce(c, sapply(currencies,
                           function(currency) sapply(dates,
                                          function(date) if(nrow(data[data$currency_slug==currency & date-maxdays<data$datetime & data$datetime<=date,])<mindays) return(NA) else CalculateCurrencyBeta(currency, data[data$currency_slug==currency & date-maxdays<data$datetime & data$datetime<=date,], market))))
  p <- ggplot(result, aes(datetime, beta, color=factor(currency)))
  p + geom_line() + labs(x="Date", y="Beta", title=paste("Beta timeline: ", paste(currencies, collapse=", "))) + theme(legend.title=element_blank())
  ggsave("PlotBetaTimeline.png", width=8, height=4, dpi=100, units="in")
}
PlotBetaTimeline(c("bitcoin","ethereum","ripple"), 30, 90, df.crypto.daily.market[df.crypto.daily.market$datetime >= '2017-01-01',], market)

# Plot returns against each other
PlotReturnVSReturn <- function(currency1, currency2, data) {
  data <- AnalyseData(c(currency1, currency2), data)
  cor_ <- cor(data[[paste("logreturn_",currency1,sep="")]], data[[paste("logreturn_",currency2,sep="")]])
  p <- ggplot(data, aes_string(x=paste("logreturn_",currency1,sep=""), y=paste("logreturn_",currency2,sep="")))
  p + geom_point() +
    stat_smooth(color = "red", fill = "red", method = "lm") +
    labs(title=paste("Returns: ",currency1," vs ",currency2," (cor = ",round(cor_, digits=4),")",sep=""), x=paste(currency1, "Return"), y=paste(currency2, "Return")) +
    theme(legend.title=element_blank())
  ggsave(paste(paste('Plot', currency1, 'vs', currency2, 'returns', sep = '-'), 'png', sep = '.'), width=8, height=4, dpi=100, units="in")
}

PlotReturnVSReturn("bitcoin", "ethereum", df.crypto.daily.market[df.crypto.daily.market$datetime>as.Date("2016-12-31"),])
PlotReturnVSReturn("bitcoin", "litecoin", df.crypto.daily.market[df.crypto.daily.market$datetime>as.Date("2016-12-31"),])
PlotReturnVSReturn("bitcoin", "ripple", df.crypto.daily.market[df.crypto.daily.market$datetime>as.Date("2016-12-31"),])
PlotReturnVSReturn("bitcoin", "eos", df.crypto.daily.market[df.crypto.daily.market$datetime>as.Date("2016-12-31"),])

# Plot the correlation matrix for top 25 currency returns
png(filename="PlotCorrelationAll.png", width=800, height=700, units="px")
corrplot(cor(df.to.correlate[,-1],
             use = "pairwise.complete.obs"), method = "color")
dev.off()

png(filename="PlotCorrelationAllWithValues.png", width=800, height=700, units="px")
corrplot.mixed(cor(df.to.correlate[,-1],
             use = "pairwise.complete.obs"), lower.col = "black", number.cex = .8, tl.col = "red", tl.cex = 0.5,)
dev.off()

png(filename="PlotCorrelationAllWithSigValue.png", width=800, height=700, units="px")
corrplot(cor(df.to.correlate[,-1],
             use = "pairwise.complete.obs"), type="upper", p.mat = matrix.correlation.pvalue, sig.level = 0.05)
dev.off()

# Plot the correlation of two currencies over time
PlotCorrelationTimeline <- function(currency1, currency2, mindays, maxdays, datain) {
  data <- AnalyseData(c(currency1, currency2), datain)
  data$corr <- sapply(1:nrow(data), FUN=function(i) if(i<mindays) return(NA) else cor(data[max(1,i-maxdays):i,9],data[max(1,i-maxdays):i,17]))
  p <- ggplot(data, aes(datetime, corr))
  p + geom_line() + labs(x="Date", y="Correlation", title=paste("Correlation timeline: ", paste(c(currency1, currency2), collapse=" vs ")))
  ggsave(paste(paste('Plot', currency1, 'vs', currency2, 'CorrelationTimeline', sep = '-'), 'png', sep = '.'), width=8, height=4, dpi=100, units="in")
}

PlotCorrelationTimeline("bitcoin", "ethereum", 30, 90, df.crypto.daily.market[df.crypto.daily.market$datetime >= '2017-01-01',])
PlotCorrelationTimeline("bitcoin", "ripple", 30, 90, df.crypto.daily.market[df.crypto.daily.market$datetime >= '2017-01-01',])
PlotCorrelationTimeline("ethereum", "ripple", 30, 90, df.crypto.daily.market[df.crypto.daily.market$datetime >= '2017-01-01',])
PlotCorrelationTimeline("bitcoin", "litecoin", 30, 90, df.crypto.daily.market[df.crypto.daily.market$datetime >= '2017-01-01',])

CorrelationTestAll <- function(x){
    FUN <- function(x, y) cor.test(x, y)[["p.value"]]
    z <- outer(
      colnames(x), 
      colnames(x), 
      Vectorize(function(i,j) FUN(x[,i], x[,j]))
    )
    dimnames(z) <- list(colnames(x), colnames(x))
    z
}

df.corr.all <- CorrelationTestAll(df.to.correlate[,-1])


# ------ Use to document in Latex ------ #
if (!require(xtable)) install.packages("xtable")
library(xtable)

print(xtable(matrix.correlation.pvalue, type = "latex"))

print(xtable(currencies, type = "latex"))

print(xtable(df.crypto.daily.market[df.crypto.daily.market$datetime == '2018-08-01',], type = "latex"))

print(xtable(df.crypto.daily.market[df.crypto.daily.market$datetime == '2018-08-01',1:6], type = "latex"))

res <- cor.test(df.to.correlate$bitcoin, df.to.correlate$ethereum, 
                method = "pearson")
res
