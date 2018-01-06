setwd('H:/Sandbox/top100/Data/')
library(data.table)
fileNames = list.files(pattern="*.csv")
CoinNames = gsub(" ", "", substr(fileNames,1,nchar(fileNames)-4), fixed = TRUE)
dataset = as.data.frame(read.csv('Bitcoin.csv')[ , "Date"])
colnames(dataset) = "Date"
for(i in 1:length(fileNames)){
  temp = read.csv(fileNames[i])
  temp$Volume = as.numeric(gsub(",", "", as.character(temp$Volume)))
  temp$Market.Cap = as.numeric(gsub(",", "", as.character(temp$Market.Cap)))
  colnames(temp) = c("Date", paste(colnames(temp)[!(colnames(temp) %in% "Date")], CoinNames[i], sep = "_"))
  dataset = merge(dataset, temp, all.x = T, by = "Date")
}
class(dataset$Date)
dataset$Date = as.Date(as.character(dataset$Date), format = "%b %d, %Y")
colsToKeep = paste0("Market.Cap_", CoinNames)
# as.numeric(as.character(dataset$Market.Cap_Bitcoin[1]))
ranking = data.frame(dataset$Date, t(apply(-dataset[ , colsToKeep], 1, rank, ties.method='average', na.last = "keep")))
colnames(ranking) = c("Date", paste0("rank_", CoinNames))
# ranking[is.na(ranking)] = 100
ranking = as.data.table(ranking)
rankcols = paste0("rank_", CoinNames)
ranking = ranking[order(Date)]
# myFunc = function(x) {x - shift(x,1)}
ranking = ranking[, paste0(rankcols, "1d") := lapply(.SD, function(x) {x - shift(x,1)}), .SDcols = rankcols][]
ranking = ranking[, paste0(rankcols, "2d") := lapply(.SD, function(x) {x - shift(x,2)}), .SDcols = rankcols][]
ranking = ranking[, paste0(rankcols, "3d") := lapply(.SD, function(x) {x - shift(x,3)}), .SDcols = rankcols][]
ranking = ranking[, paste0(rankcols, "5d") := lapply(.SD, function(x) {x - shift(x,5)}), .SDcols = rankcols][]
ranking = ranking[, paste0(rankcols, "7d") := lapply(.SD, function(x) {x - shift(x,7)}), .SDcols = rankcols][]
ranking = ranking[, paste0(rankcols, "10d") := lapply(.SD, function(x) {x - shift(x,10)}), .SDcols = rankcols][]
ranking = ranking[, paste0(rankcols, "15d") := lapply(.SD, function(x) {x - shift(x,15)}), .SDcols = rankcols][]
ranking = ranking[, paste0(rankcols, "20d") := lapply(.SD, function(x) {x - shift(x,20)}), .SDcols = rankcols][]
ranking = ranking[, paste0(rankcols, "30d") := lapply(.SD, function(x) {x - shift(x,30)}), .SDcols = rankcols][]

ranking = as.data.frame(ranking)
dataset = dataset[order(dataset$Date), ]
dataset = merge(dataset, ranking, all.x = T, by = "Date")

colsToKeep = colnames(dataset)[grep("*Lisk", colnames(dataset))]
temp = dataset[ , c("Date", colsToKeep)]
# dataset$dash
