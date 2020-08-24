library(quantmod)
library(gplots)
library(sm)
library(vioplot)
library(reshape2)
library(lubridate)
library(RColorBrewer)

## ---- only run once
if (FALSE) {
  getSymbols.yahoo(
    Symbols = c("SPY", "EEM", "IWM"),
    env = ".GlobalEnv",
    from = "1995-01-01",
    to = "2020-08-20"
  )

  data.all.SPY <- clean_data(SPY)
  data.all.EEM <- clean_data(EEM)
  data.all.IWM <- clean_data(IWM)

  saveRDS(data.all.SPY, file = "./data/spy.rds")
  saveRDS(data.all.EEM, file = "./data/eem.rds")
  saveRDS(data.all.IWM, file = "./data/iwm.rds")
}

##---- Functions Definitions
clean_data <- function(data) {
  header <- names(data)

  # remove symbol name from header
  split_data <- strsplit(header, ".", fixed = TRUE)
  colnames(data) <- sapply(split_data, "[", length(split_data[[1]]))

  return(data)
}

price_plot <- function(symbol, data, col = "black") {
  plot(
    data,
    xlab = "Date",
    ylab = "Price",
    col = col,
    main = paste(
      symbol,
      " Price"
    )
  )
}

summary_plot <- function(symbol, summary_rets, col, subs) {
  header <- c("Min.", "Max.", "Median", "Mean")
  data <- data.frame(Month = summary_rets$Month, summary_rets[, symbol])

  par(mfrow = c(2, 2))
  par(oma = c(oma = c(1, 1, 2, 1)))
  for (col_name in header) {
    barplot(
      data[, col_name] * 100,
      ylab = paste(col_name, "Returns (%)"),
      yaxt = "n",
      col = col,
      main = col_name
    )
    axis(
      side = 2,
      at = axTicks(2),
      labels = formatC(axTicks(2), format = "d", big.mark = ",")
    )
  }
  mtext(paste(symbol, "Annual Return (%)"),
    outer = TRUE,
    font = 2,
    cex = 1.3
  )
  mtext(
    subs,
    side = 1,
    outer = TRUE,
    cex = 0.8
  )
}

summary_grp_plot <- function(summary_rets, summary_sds, symbols, colors, subs) {

  header <- c("Min.", "Max.", "Median", "Mean")

  par(mfrow = c(2, 2))
  par(oma = c(oma = c(2, 1, 2, 1)))

  for (col_name in header) {
    data <- sapply(symbols, function(symbol) {
      summary_rets[, symbol][, col_name] * 100
    })
    data <- t(data)

    if (col_name == "Mean") {
      sds <- summary_sds[, symbol]
    } else {
      sds <- matrix(0, nrow = nrow(data), ncol = ncol(data))
    }

    barplot2(
      data,
      col = colors,
      beside = TRUE,
      legend = symbols,
      names.arg = month.abb,
      yaxt = "n",
      ylab = paste(col_name, "Returns (%)"),
      main = col_name,
      plot.ci = TRUE,
      ci.l = (data - 1.96 * sds),
      ci.u = (data + 1.96 * sds)
    )

    axis(
      side = 2,
      at = axTicks(2),
      labels = formatC(axTicks(2), format = "d", big.mark = ",")
    )
  }
  mtext(
    paste(paste(symbols, collapse = ", "), "Annual Return (%)"),
    outer = TRUE,
    font = 2,
    cex = 1.3
  )

  mtext(
    subs,
    side = 1,
    outer = TRUE,
    cex = 0.8
  )
}

##---- Exploratory Analysis
options(scipen = 100)
opar <- par(no.readonly = TRUE)

symbols <- c("SPY", "EEM", "IWM")
colors <- brewer.pal(length(symbols), "Set1")

data.all.SPY <- readRDS("./data/spy.rds")
data.all.EEM <- readRDS("./data/eem.rds")
data.all.IWM <- readRDS("./data/iwm.rds")

first_date <- as.Date(
  max(
    sapply(
      list(data.SPY, data.EEM, data.IWM),
      function(x) index(first(x))
    )
  )
)

data.SPY <- data.all.SPY[index(data.all.SPY) >= first_date]
data.EEM <- data.all.EEM[index(data.all.EEM) >= first_date]
data.IWM <- data.all.IWM[index(data.all.IWM) >= first_date]

class(data.SPY)
head(data.SPY)

data.normalized <- cbind(
  data.SPY$Adjusted / as.numeric(data.SPY$Adjusted[1]),
  data.EEM$Adjusted / as.numeric(data.EEM$Adjusted[1]),
  data.IWM$Adjusted / as.numeric(data.IWM$Adjusted[1])
)
names(data.normalized) <- symbols

## --- Log Returns
log_rets <- diff(log(data.normalized), lag = 1)
log_rets <- log_rets[-1]
months <- month(log_rets, label = TRUE, abbr = TRUE)

log_rets <- as.data.frame(log_rets)
log_rets$Month <- months

head(log_rets)
str(log_rets)

## --- Plot all datapoints
png(file = "./images/plot1.png", width = 1000)

par(oma = c(oma = c(1, 1, 2, 1)))
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
price_plot("SPY", data.all.SPY$Adjusted, col = colors[1])
price_plot("EEM", data.all.EEM$Adjusted, col = colors[2])
price_plot("IWM", data.all.IWM$Adjusted, col = colors[3])
mtext("All Data Points", outer = TRUE)

dev.off()

## --- Plot matched datapoints

png(file = "./images/plot2.png", width = 1000)

par(opar)
par(mfrow = c(3, 1))
price_plot("SPY", data.SPY$Adjusted, col = colors[1])
price_plot("EEM", data.EEM$Adjusted, col = colors[2])
price_plot("IWM", data.IWM$Adjusted, col = colors[3])

dev.off()

## --- Normalize first price to 1
png(file = "./images/plot3.png", width = 1000)

par(opar)
plot(
  data.normalized,
  col = colors,
  main = "SPY vs EEM vs IWM",
  legend.loc = "topleft"
)

dev.off()

## --- Barplots
summary_rets <- aggregate(
  log_rets[symbols] * 252,
  by = list(log_rets$Month),
  FUN = summary
)
names(summary_rets)[1] <- "Month"

summary_sds <- aggregate(
  log_rets[symbols] * 252,
  by = list(log_rets$Month),
  FUN = sd
)
names(summary_sds)[1] <- "Month"

subs <- paste(
  row.names(first(log_rets)),
  "/",
  row.names(last(log_rets))
)

png(file = "./images/plot4.png", width = 1000)
summary_plot("SPY", summary_rets, colors[1], subs)
dev.off()
png(file = "./images/plot5.png", width = 1000)
summary_plot("EEM", summary_rets, colors[2], subs)
dev.off()
png(file = "./images/plot6.png", width = 1000)
summary_plot("IWM", summary_rets, colors[3], subs)
dev.off()

png(file = "./images/plot7.png", width = 1000)

summary_grp_plot(summary_rets, summary_sds, symbols, colors, subs)

dev.off()

par(opar)

## --- Histograms

png(file = "./images/plot8.png", width = 1000)

par(mfrow = c(1, 3))
for (i in seq_along(symbols)) {
  symbol <- symbols[i]
  rets <- log_rets[, symbol]
  dist <- density(rets)
  hist(rets,
       freq = FALSE,
       col = colors[i],
       xlab = "Returns",
       main = symbol,
       ylim = c(0, max(dist$y)))
  rug(jitter(rets))
  lines(dist, col = "blue")

  xfit <- seq(min(rets), max(rets), length = 500)
  yfit <- dnorm(xfit, mean = mean(rets), sd = sd(rets))
  lines(xfit, yfit, col = "red", lwd = 2)
  legend("topright", c("Normal", "KDE"), fill = c("red", "blue"))
}

dev.off()

## --- sm Plot
log_rets <- as.data.frame(log_rets)
log_rets$month <- factor(log_rets$month)

par(mfrow = c(1, 1))
data.melt <- melt(log_rets, id = c("month"))
names(data.melt) <- c("month", "symbol", "ret")
sm.options(ngrid = 1000)

png(file = "./images/plot9.png", width = 1000)
sm.density.compare(data.melt$ret, data.melt$symbol)
title(main = "Return Distribution by Symbol")
legend("topright", levels(data.melt$variable), fill = c(2:(1 + length(levels(data.melt$variable)))))
dev.off()

## -- Box Plots
png(file = "./images/plot10.png", width = 1000)
boxplot(ret ~ symbol,
        data = data.melt,
        varwidth = TRUE,
        col = colors,
        ylab = "Return",
        main = "Returns",
        notch = TRUE)
dev.off()

##-- No Outliers
png(file = "./images/plot11.png", width = 1000)
boxplot(ret ~ symbol,
  data = data.melt,
  varwidth = TRUE,
  col = colors,
  ylab = "Return",
  main = "Returns",
  outline = FALSE,
  notch = TRUE
)
dev.off()

## -- Violin Plots
png(file = "./images/plot12.png", width = 1000)
vioplot(data.melt$ret[data.melt$symbol == "SPY"],
  data.melt$ret[data.melt$symbol == "EEM"],
  data.melt$ret[data.melt$symbol == "IWM"],
  names = symbols,
  col = colors
  )
title("Returns", ylab = "Return")
dev.off()
