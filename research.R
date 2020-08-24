library(quantmod)
library(RColorBrewer)
library(gplots)
library(lubridate)
library(sm)
library(reshape2)
library(vioplot)

##---- SPY
getSymbols.yahoo(
  Symbols = c("SPY", "EEM", "IWM"),
  env = ".GlobalEnv",
  from = "1995-01-01",
  to = "2020-08-20"
)

clean_data <- function(data) {

  header  <- names(data)

  split_data <- strsplit(header, ".", fixed = TRUE)
  colnames(data) <- sapply(split_data, "[", length(split_data[[1]]))

  return(data)
}

myplot <- function(symbol, data) {
  plot(
    data,
    xlab = "Date",
    ylab = "Price",
    main = paste(
      symbol,
      " Price from ",
      start(data),
      " to ",
      end(data)
    )
  )
}


data.SPY <- clean_data(SPY)
data.EEM <- clean_data(EEM)
data.IWM <- clean_data(IWM)

saveRDS(data.SPY, file = "./data/spy.rds")
saveRDS(data.EEM, file = "./data/eem.rds")
saveRDS(data.IWM, file = "./data/iwm.rds")

data.SPY <- readRDS("./data/spy.rds")
data.EEM <- readRDS("./data/eem.rds")
data.IWM <- readRDS("./data/iwm.rds")

## --- Plot all series
png(file = "./images/plot1.png", width = 1000)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE))
myplot("SPY", data.SPY)
myplot("EEM", data.EEM)
myplot("IWM", data.IWM)
dev.off()

## --- Plot series with common dates
first_date <- as.Date(max(sapply(list(data.SPY, data.EEM, data.IWM), function(x) index(first(x)))))

data.SPY <- data.SPY[index(data.SPY) >= first_date]
data.EEM <- data.EEM[index(data.EEM) >= first_date]
data.IWM <- data.IWM[index(data.IWM) >= first_date]

opar <- par(no.readonly = TRUE)
png(file = "./images/plot2.png", width = 1000)
par(mfrow = c(3, 1))
myplot("SPY", data.SPY)
myplot("EEM", data.EEM)
myplot("IWM", data.IWM)
par(opar)
dev.off()

## -- Plot all series in 1 plot
data <- cbind(
  data.SPY$Adjusted / as.numeric(data.SPY$Adjusted[1]),
  data.EEM$Adjusted / as.numeric(data.EEM$Adjusted[1]),
  data.IWM$Adjusted / as.numeric(data.IWM$Adjusted[1])
)
names(data) <- c("SPY", "EEM", "IWM")

png(file = "./images/plot3.png", width = 1000)
plot(
  data,
  main = "SPY vs EEM vs IWM",
  legend.loc = "topleft"
)
dev.off()

## --- Barplots

log_rets <- diff(log(data), lag = 1)
log_rets <- log_rets[-1]


log_rets <- cbind(log_rets, month = month(index(log_rets)))

aggregate_month <- function(symbol, log_rets) {
  as.data.frame(aggregate(log_rets[, symbol],
    by = list(log_rets$month),
    FUN = summary
  ) * 252)
}

mybarplots <- function(symbol, log_rets) {

  log_rets_summary <- aggregate_month(symbol, log_rets)

  log_rets_summary <- cbind(log_rets_summary, month = as.numeric(row.names(log_rets_summary)))
  log_rets_summary <- log_rets_summary[order(log_rets_summary$month), ]

  col_names <- c("Min.", "Max.", "Median", "Mean")

  opar <- par(no.readonly = TRUE)
  par(mfrow = c(2, 2))

  for (col in col_names) {
    barplot(
      log_rets_summary[, col] * 100,
      names.arg = month.abb,
      ylab = paste(col, "Returns (%)"),
      yaxt = "n",
      main = paste0(symbol, " Annual '", col, "' Returns"),
      sub = paste(
          index(first(log_rets)),
          "to",
          index(last(log_rets))
        )
    )
    axis(
      side = 2,
      at = axTicks(2),
      labels = formatC(axTicks(2), format = "d", big.mark = ",")
    )
  }

  par(opar)
}

png(file = "./images/plot4.png", width = 1000)
mybarplots("SPY", log_rets)
dev.off()

png(file = "./images/plot5.png", width = 1000)
mybarplots("EEM", log_rets)
dev.off()

png(file = "./images/plot6.png", width = 1000)
mybarplots("IWM", log_rets)
dev.off()

mygroupbarplots <- function(col, log_rets) {

  symbols <- c("SPY", "EEM", "IWM")
  data <- aggregate_month(symbols[1], log_rets)
  month <- as.numeric(row.names(data))

  data <- cbind(
    SPY = data[, col],
    EEM = aggregate_month(symbols[2], log_rets)[, col],
    IWM = aggregate_month(symbols[3], log_rets)[, col]
  )

  data <- data[order(month), ]
  data <- t(data)

  if (col == "Mean") {
    sds <- as.data.frame(
      aggregate(log_rets[, symbols],
        by = list(log_rets$month),
        FUN = sd
      )
    )
    sds <- sds[order(as.numeric(row.names(sds))), ]
    sds <- t(sds)
  } else {
    sds <- matrix(0, nrow = nrow(data), ncol = ncol(data))
  }

  barplot2(data * 100,
    col = brewer.pal(3, "Set1"),
    beside = TRUE,
    legend = rownames(data),
    names.arg = month.abb,
    yaxt = "n",
    ylab = paste("Median Returns (%)"),
    main = paste0("Annual '", col, "' Returns"),
    sub = paste(
      index(first(log_rets)),
      "to",
      index(last(log_rets))
    ),
    plot.ci = TRUE,
    ci.l = (data - 1.96 * sds) * 100,
    ci.u = (data + 1.96 * sds) * 100
    )

  axis(side = 2,
       at = axTicks(2),
       labels = formatC(axTicks(2), format = "d", big.mark = ","))
}


png(file = "./images/plot7.png", width = 1000)
par(mfrow = c(2, 2))
mygroupbarplots("Min.", log_rets)
mygroupbarplots("Max.", log_rets)
mygroupbarplots("Median", log_rets)
mygroupbarplots("Mean", log_rets)
dev.off()

par(opar)

## --- Histograms

symbols <- c("SPY", "EEM", "IWM")
colors <- brewer.pal(length(symbols), "Set1")

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

log_rets <- as.data.frame(log_rets)
log_rets$month <- factor(log_rets$month)
log_ann_rets <- as.data.frame(log_rets)

log_ann_rets$SPY <- log_rets$SPY * 252
log_ann_rets$EEM <- log_rets$EEM * 252
log_ann_rets$IWM <- log_rets$IWM * 252


par(mfrow = c(1, 1))
data.melt <- melt(log_rets, id = c("month"))
names(data.melt) <- c("month", "symbol", "ret")
sm.options(ngrid = 1000)

png(file = "./images/plot9.png", width = 1000)
sm.density.compare(data.melt$ret, data.melt$symbol)
title(main = "Return Distribution by Symbol")
legend("topright", levels(data.melt$variable), fill = c(2:(1 + length(levels(data.melt$variable)))))
dev.off()

png(file = "./images/plot10.png", width = 1000)
boxplot(ret ~ symbol,
        data = data.melt,
        varwidth = TRUE,
        col = colors,
        ylab = "Return",
        main = "Returns",
        notch = TRUE)
dev.off()

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

png(file = "./images/plot12.png", width = 1000)
vioplot(data.melt$ret[data.melt$symbol == "SPY"],
  data.melt$ret[data.melt$symbol == "EEM"],
  data.melt$ret[data.melt$symbol == "IWM"],
  names = symbols,
  col = colors
  )
title("Returns", ylab = "Return")
dev.off()
