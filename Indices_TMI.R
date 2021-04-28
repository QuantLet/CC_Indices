#########################################################
##### Indices on CCs - an Evaluation ####################
#########################################################

#authors: Konstantin Häusler, Hongyu Xia

#load packages
library(lubridate)
library(zoo)
library(quantmod)
library(moments)
library(corrplot)
library(xtable)
library(vioplot)
library(RColorBrewer)
display.brewer.all()
colors <- brewer.pal(11, "Paired")

#load cryptocurrency (CC) data
CCs <- read.csv("/Users/konstantin/Desktop/dateien/IRTG 1792/econCRIX/updated index data/test.csv")

remove_empty_rows <- function(x){
  #x <- x[x$prices >0 & x$total_volumes >0 & x$market_caps >0,] 
  x <- x[ x$market_caps >0,] 
  x
}
CCs <- remove_empty_rows(CCs)

transform_date <- function(x){
  x$Datetime <- as.character(x$Datetime)
  x$Datetime <- factor(x$Datetime)
  x
}
CCs <- transform_date(CCs)
write.csv(CCs, file = "/Users/konstantin/Desktop/dateien/IRTG 1792/CoinGecko/updated index data/CC_data.csv")

number_active_coins <- function(x){
  w <- NULL
  for (i in levels(x$Datetime)) {
    tmp.w <- nrow(x[x$Datetime == as.character(i),])
    w <- rbind(w, tmp.w)
  }
  w
}
w <- number_active_coins(CCs)


w <- as.data.frame(w)
rownames(w) <- levels(CCs$Datetime)
colnames(w) <- "number_of_coins"
w$Date <- levels(CCs$Datetime)
w$Date <- as.Date(w$Date)

par(bg=NA)
plot(w$number_of_coins~w$Date, type = "l" , ylab = "", xlab= "", 
     lwd=2,
     col="#9E9AC8")

#########################################################
##### Total Market Index ################################
#########################################################

#calculate daily market cap (sum of market cap of all coins)
TMI <- aggregate(CCs$market_caps, by = list(CCs$Datetime), FUN = sum)
TMI$Group.1 <- as.Date(TMI$Group.1)
#TMI$log_mc <- log(TMI$x) 
#TMI$st_mc <- (TMI$x - mean(TMI$x)) / sd(TMI$x)
#TMI$st_sc <- NULL
#colnames(TMI) <- c("date", "total_mc", "log_mc", "st_mc")
names(TMI) <- c("Date", "TMI")

#TMI.201808<- TMI[TMI$date > "2018-08-18",]

write.csv(TMI, file = "TMI_csv.csv")
save(TMI, file="TMI_R.rda")

#########################################################
############ CC Indices #################################
#########################################################

#read index data

bbgci <- read.csv2("/Users/konstantin/Desktop/dateien/IRTG 1792/CoinGecko/updated index data/BBGCI.csv")
bbgci <- bbgci[,1:2]
bbgci$Date <- parse_date_time(bbgci$Date, orders = "mdy")
bbgci <- bbgci[order(as.Date(bbgci$Date, format="%Y-%m-%d")),]
bbgci$Date <- as.Date(bbgci$Date)
names(bbgci) <- c("Date", "BBGCI")

cci30 <- read.csv2("/Users/konstantin/Desktop/dateien/IRTG 1792/CoinGecko/updated index data/CCI30.csv")
cci30 <- cci30[,c("Date", "Close")]
cci30$Date <- as.Date(cci30$Date, format="%Y-%m-%d")
cci30$Close <- as.numeric(as.character(cci30$Close))
names(cci30) <- c("Date", "CCI30")
cci30 <- cci30[order(cci30$Date),]

crix <- read.csv2("/Users/konstantin/Desktop/dateien/IRTG 1792/CoinGecko/updated index data/crix_data_csv.csv",
                  header = T)
crix <- crix[-1,1:2]
names(crix) <- c("Date", "CRIX")
crix$CRIX <- as.numeric(as.character(crix$CRIX))
crix$Date <- as.Date(crix$Date)

f5 <- read.csv2("/Users/konstantin/Desktop/dateien/IRTG 1792/CoinGecko/updated index data/F5.csv")
f5$Date <- as.Date(f5$Date, format="%Y-%m-%d")
f5$F5.Crypto.Index <- as.numeric(as.character(f5$F5.Crypto.Index))
names(f5) <- c("Date", "F5")

hodl <- read.csv2("/Users/konstantin/Desktop/dateien/IRTG 1792/CoinGecko/updated index data/hodl_csv.csv")
hodl <- hodl[,1:2]
names(hodl) <- c("Date", "HODL5")
hodl$Date <- as.Date(hodl$Date, format= "%d/%m/%Y")

#compute indices whose data is not available
#>>>>> Hongyu

#merge datasets
index.data <- merge(TMI, bbgci, by.x = "Date", by.y = "Date", all.x = T)
index.data$BBGCI <- na.locf(index.data$BBGCI, na.rm = F)

index.data <- merge(index.data, cci30, by.x = "Date", by.y = "Date", all.x = T)
index.data <- merge(index.data, crix, by.x = "Date", by.y = "Date", all.x = T)
index.data <- merge(index.data, f5, by.x = "Date", by.y = "Date", all.x = T) #2018-08-15
index.data <- merge(index.data, hodl, by.x = "Date", by.y = "Date", all.x = T)
index.data <- index.data[index.data$Date >"2018-08-14",]
#names(index.data)[2] <- "TMI"

#norm indices
index.i <- names(index.data)[-1]
for (i in index.i) {
  index.data[, paste(i, "_normed", sep = "")] <-index.data[, i] / index.data[1,i] *1000
}

write.csv(index.data, file = "/Users/konstantin/Desktop/dateien/IRTG 1792/CoinGecko/updated index data/index_data.csv")

##################################################
###descriptive statistics and Sharpe ratios#######
##################################################

#compute returns
for (i in index.i) {
  index.data[, paste(i, "_return", sep ="")] <- Delt(index.data[,i])
}

#moments
avg_returns <- apply(index.data[,paste(index.i, "_return", sep = "")], 2, FUN = function(x){mean(x, na.rm = T)})
standarddeviation_returns <- apply(index.data[,paste(index.i, "_return", sep = "")], 2, FUN = function(x){sd(x, na.rm = T)})
skewness <- apply(index.data[,paste(index.i, "_return", sep = "")], 2, FUN = function(x){skewness(x, na.rm = T)})
kurtosis <- apply(index.data[,paste(index.i, "_return", sep = "")], 2, FUN = function(x){kurtosis(x, na.rm = T)})
sharpe_ratios <- avg_returns / standarddeviation_returns

#PSR
psr <- NULL
for (i in seq(1:length(index.i)) ) {
  input <- (sharpe_ratios[[i]] * sqrt(len -1)) / sqrt(1-skewness[[i]] * sharpe_ratios[[i]] + ((kurtosis[[i]] -1)/4)*sharpe_ratios[[i]]^2)
  tmp <- pnorm(input)
  print(tmp)
  psr <- rbind(psr, tmp)
}


#max drawdown
library("PMwR")
library("zoo")
max_drawdown<-function(x){
 dd<-drawdowns(x) 
 dd <- dd[order(dd$max, decreasing = TRUE), ]
 dd$duration<-dd$trough-dd$peak
 return(dd[1,])
}
maxdrawdown<-data.frame()
for (i in seq(1:length(index.i)) ) {
  index<- zoo(index.data[,paste(index.i[i], "_normed", sep = "")], as.Date(index.data$Date))
  a<-data.frame(max_drawdown(index))
  a$index<-index.i[i]
  maxdrawdown<-rbind(maxdrawdown,a)
}



output <- data.frame(returns = avg_returns, 
                     vola = standarddeviation_returns,
                     skewness = skewness,
                     kurtosis = kurtosis,
                     sharpe_ratio = sharpe_ratios, 
                     PSR = psr,
                     row.names = index.i)
output$index<-row.names(output)
output<-merge(output,maxdrawdown,by="index",all=F)

xtable(output, digits = 3)


##############################
###Visualization of Indices###
##############################

#plot normed indices
par(xpd = T, mar = par()$mar + c(0,0,0,7),bg=NA)

plot(index.data$TMI_normed ~index.data$Date,  type ="l", ylim = c(0, 2000), xlab="", 
     ylab="", lwd = 2, lty = 1)
lines(index.data[,"CRIX_normed"] ~ index.data$Date, type = "l",pch=3, lty=1, lwd=1, col= "#A6CEE3") #pch=3, colors[1]
#lines(index.data[,"Bitwise10_normed"] ~ index.data$Date, type="l", col= colors[3], lwd=1.5, lty=1) #15
lines(index.data[,"CCI30_normed"] ~ index.data$Date, type="l", col= "#1F78B4", lwd=1.5, lty=1)  #, pch=4 colors[2]              #19
lines(index.data[,"F5_normed"] ~ index.data$Date, type="l", col= "#E31A1C", lwd=1.5, lty=1)  #, pch=8  colors[6]             #23
lines(index.data[,"BBGCI_normed"] ~ index.data$Date, type="l", col= "#FF7F00", lwd=1.5, lty=1)    #25 colors[8]
lines(index.data[,"HODL5_normed"] ~ index.data$Date, type="l", col= "blue", lwd=1.5, lty=1) 
lines(index.data$TMI_normed ~index.data$Date,  type ="l", ylim = c(0, 2000), xlab="", 
      ylab="", lwd = 2, lty = 1)
legend("right",
       inset=c(-0.3,0), xpd = T, #cancel out when not outside
       legend=index.names, 
       col=c("black",colors[c(3,2,6,8)], "black"),
       #pch=c(19), pt.cex=1, 
       #bg='lightgrey',
       cex = 0.8,
       lwd = 3,
       lty = c(rep(3.5,5),1.5)
       #text.col = c("black", colors)
)

##############################
#Corrletation Indices & TMI###
##############################

cors <- cor(index.data[, index.i], use="complete.obs")
corrplot(cors,method = "color", order = "AOE")
par(bg=NA)
corrplot.mixed(cors, 
               order = "alphabet", 
               lower = "number", 
               upper = "circle",
               tl.col = "black",
               cl.lim = c(0,1), bg=NA, add=TRUE
)

#violin plots

# Creating Function to obtain Correlations from the data
corr_tmi_to_index <- function(sel.index, sample.i) {
  dt <- index.data[sample.i,c(sel.index,"TMI" )] 
  fit.cor <- cor(dt[,sel.index], dt[,"TMI"], use="complete.obs")
  return(fit.cor)
} 
corr_tmi_to_index(sel.index = "CRIX", sample.i= c(1:100))

#now with random draws
cor.vec.matrix <- data.frame()
for (j in index.i) {
  for (i in c(1:1000)) {
    ind <- sample(c(1:nrow(index.data)), 100)
    tmp.cor <- corr_tmi_to_index(sel.index = j, sample.i=ind)
    cor.vec.matrix[i,j] <- tmp.cor
  }
}
par(bg=NA)
vioplot(cor.vec.matrix[2:6], col=1:5)
# , col = 2:6, border = 2:6

#############################################
### representation of CC sector##############
#############################################

plot(index.data$TMI~index.data$Date, type="l")
plot()

bitcoin <- data.frame(date = CCs$Datetime[CCs$Id=="bitcoin"], MC.bitcoin = CCs$market_caps[CCs$Id=="bitcoin"])
  CCs$market_caps[CCs$Id=="bitcoin", c("Datetime")]

top10 <- aggregate(CCs$market_caps, by= list(CCs$Datetime), FUN = function(x){sum(sort(x, decreasing = TRUE)[1:10])})
names(top10) <- c("Date", "Top10_MC")

top30 <- aggregate(CCs$market_caps, by= list(CCs$Datetime), FUN = function(x){sum(sort(x, decreasing = TRUE)[1:30])})
names(top30) <- c("Date", "Top30_MC")

cc.rep <- data.frame(date=TMI$Date, 
                     Bitcoin = bitcoin$MC.bitcoin,
                     TMI = TMI$TMI,
                     top10 = top10$Top10_MC, 
                     top30 = top30$Top30_MC)

plot(TMI~date, data= cc.rep[cc.rep$date>"2018-08-15",], type="l",  ylim=c(1e+10, 7e+11), xlab="", ylab="")
lines(top10~date, data= cc.rep[cc.rep$date>"2018-08-15",], type="l", col="blue")
lines(top30~date, data= cc.rep[cc.rep$date>"2018-08-15",], type="l", col="red")
lines(Bitcoin~date, data= cc.rep[cc.rep$date>"2018-08-15",], type="l", col="green")

################################################
## CC sector is under constant transformation###
################################################
#  i) composition of top 10 coins is steadily changing
shares <- CCs
shares$Datetime <- as.Date(shares$Datetime, format="%Y-%m-%d")
shares <- merge(shares, TMI, by.x="Datetime",by.y = "Date", all.x=T )
shares$weight <- shares$market_caps / shares$TMI
weight.dates <- seq(as.Date("2018-01-01"), as.Date("2020-12-30"), by="3 mon")
shares <- shares[shares$Datetime %in% weight.dates,]



#  ii) market share of top 10 coins is not 100%










