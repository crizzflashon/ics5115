
if (!require(crypto)) install.packages("crypto", dependencies = TRUE)
library(crypto)

df_coins <- crypto_list()

write.csv(df_coins, file='Coins-All.csv', row.names=FALSE, na="")

df_coins <- read.csv(file="Coins.csv", header=TRUE, sep=",")
df_coins <- df_coins[df_coins$rank <= 10,]

df_crypto_daily_market <- data.frame()
for(i in df_coins$slug) {
    cat("Getting history for ", as.character(i[1]))
    df_crypto_daily_market_curr <- daily_market(coin = i)
    df_crypto_daily_market <- rbind(df_crypto_daily_market, df_crypto_daily_market_curr)
    # sleep randomly between 10 to 60 seconds between each request
    # to not overload server
    Sys.sleep(sample(10:60, 1))
}

write.csv(df_crypto_daily_market, file='crypto-markets-all', row.names=FALSE, na="")
