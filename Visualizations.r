
library(magrittr)
library(dplyr) 
library(tidyverse)

if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if(!require('Rmisc')) install.packages('Rmisc'); library('Rmisc')
if(!require('dplyr')) install.packages('dplyr'); library('dplyr')
if(!require('xfun')) install.packages('xfun'); library('xfun')
if(!require('Rmisc')) install.packages('Rmisc'); library('Rmisc')


###################################################################################

##############################  ScatterPlot  ######################################

###################################################################################


setwd("C:/Users/danie/Desktop/Master/Visualización de datos/PECs/PEC2/archive (2)")

data <-read.csv("kc_house_data.csv",  sep=",")

cprice <- data$price/max(data$price)
csqrt <- data$sqft_living/max(data$sqft_living)
cbedr <- data$bedrooms/max(data$bedrooms)



plot(cprice, csqrt, main="Scatterplot House price vs sqrt feet ", xlab="price", ylab="sqft_living/nbedrooms ", pch=19,data=iris,col="dodgerblue1")
points(cprice, cbedr, col='lightgreen', pch=19)
abline(lm(csqrt ~ cprice, data = data),col='blue') #regression line
abline(lm(cbedr ~ cprice, data = data),col='green') #regression line


#add legend
legend(0.025, 0.9, legend=c('SQRT_Feet', 'N Bedrooms'), pch=c(19, 19), col=c('blue', 'green'))






###################################################################################

####################### Marimekko Chart S&P 500 ###################################

###################################################################################



setwd("C:/Users/danie/Desktop/Master/Visualización de datos/PECs/PEC2/archive (1)")
data <-read.csv("financials.csv",  sep=",")

attach(data)

sectors = unique(data$Sector)

data$Name <- str_remove_all(data$Name, " Inc")
data$Name <- str_remove_all(data$Name, " Int")
data$Name <- str_remove_all(data$Name, " Corporation")
data$Name <- str_remove_all(data$Name, " Company")
data$Name <- str_remove_all(data$Name, " Corp")
data$Name <- str_remove_all(data$Name, " Co")
data$Name <- str_remove_all(data$Name, " Group")
data$Name <- str_remove_all(data$Name, " Service")
data$Name <- str_remove_all(data$Name, " Technologies")
data$Name <- str_remove_all(data$Name, "\\.")
data$Name <- str_remove_all(data$Name, "artin")
data$Name <- str_remove_all(data$Name, "ctric")


percents = list()

for (sector in sectors){
  percents <- append(percents,sum(data$Market.Cap[data$Sector==sector]))
}

percents <- c(sum(data$Market.Cap[data$Sector=="Information Technology"]), 
                 sum(data$Market.Cap[data$Sector=="Health Care"]),
                 sum(data$Market.Cap[data$Sector=="Industrials"]),
                 sum(data$Market.Cap[data$Sector=="Financials"]))
percents <- unlist(percents)
percents <- (percents*100)/sum(percents)

##Information Technology Sector
df <- data.frame(
  "names"= data$Name[data$Sector=="Information Technology"],
  "caps" = as.numeric(data$Market.Cap[data$Sector=="Information Technology"])
)


df <-df[order(df$caps,decreasing=TRUE),]
IT <- head(df,9)
IT[10,] <- c("Others",sum(tail(df$caps,-10)))
IT$caps <- as.numeric(IT$caps)
 
tot = sum(IT$caps)

for(el in IT$caps){
  IT$caps[IT$caps==el]= el*100/tot
}



##Health Care Sector
df <- data.frame(
  "names"= data$Name[data$Sector=="Health Care"],
  "caps" = as.numeric(data$Market.Cap[data$Sector=="Health Care"])
)

df <-df[order(df$caps,decreasing=TRUE),]
hc <- head(df,9)
hc[10,] <- c("Others",sum(tail(df$caps,-10)))
hc$caps <- as.numeric(hc$caps)

tot = sum(hc$caps)

for(el in hc$caps){
  hc$caps[hc$caps==el]= el*100/tot
}



##Industrials Sector
df <- data.frame(
  "names"= data$Name[data$Sector=="Industrials"],
  "caps" = as.numeric(data$Market.Cap[data$Sector=="Industrials"])
)

df <-df[order(df$caps,decreasing=TRUE),]
#industrials <- head(df,9)
industrials <- head(df,10)
#industrials[10,] <- c("Others",sum(tail(df$caps,-10)))
#$caps <- as.numeric(industrials$caps)

tot = sum(industrials$caps)

for(el in industrials$caps){
  industrials$caps[industrials$caps==el]= el*100/tot
}



##Financial Sector
df <- data.frame(
  "names"= data$Name[data$Sector=="Financials"],
  "caps" = as.numeric(data$Market.Cap[data$Sector=="Financials"])
)

df <-df[order(df$caps,decreasing=TRUE),]
finance <- head(df,9)
finance[10,] <- c("Others",sum(tail(df$caps,-10)))
finance$caps <- as.numeric(finance$caps)

tot = sum(finance$caps)

for(el in finance$caps){
  finance$caps[finance$caps==el]= el*100/tot
}

IT$caps <- round(IT$caps, 2)
hc$caps <- round(hc$caps, 2)
industrials$caps <- round(industrials$caps, 2)
finance$caps <- round(finance$caps, 2)




df <- data.frame(segment = c("IT", "Health Care", "Industrials","Finance"), segpct = c(percents[1], percents[2],percents[3], percents[4]), 
                  Alpha = c(IT$caps[1],hc$caps[1],industrials$caps[1],finance$caps[1]), 
                  Beta  = c(IT$caps[2],hc$caps[2],industrials$caps[2],finance$caps[2]),
                  Gamma = c(IT$caps[3],hc$caps[3],industrials$caps[3],finance$caps[3]), 
                  Delta = c(IT$caps[4],hc$caps[4],industrials$caps[4],finance$caps[4]),
                  Epsilon = c(IT$caps[5],hc$caps[5],industrials$caps[5],finance$caps[5]),
                  Zeta  = c(IT$caps[6],hc$caps[6],industrials$caps[6],finance$caps[6]),
                  Eta   = c(IT$caps[7],hc$caps[7],industrials$caps[7],finance$caps[7]),
                  Theta = c(IT$caps[8],hc$caps[8],industrials$caps[8],finance$caps[8]),
                  Iota  = c(IT$caps[9],hc$caps[9],industrials$caps[9],finance$caps[9]),
                  Kappa = c(IT$caps[10],hc$caps[10],industrials$caps[10],finance$caps[10]))
                



df$xmax <- cumsum(df$segpct)
df$xmin <- df$xmax - df$segpct
df$segpct <- NULL

options(digits=2)

library(reshape)
library(ggplot2)


dfm <- melt(df, id = c("segment", "xmin", "xmax"))


dfm1 <- ddply(dfm, .(segment), transform, ymax = cumsum(value))
dfm1 <- ddply(dfm1, .(segment), transform, ymin = ymax - value)

dfm1$SectorPercent <- with(dfm1, xmin + (xmax - xmin)/2)
dfm1$MarketCap <- with(dfm1, ymin + (ymax - ymin)/2)

p <- ggplot(dfm1, aes(ymin = ymin, ymax = ymax,
                      xmin = xmin, xmax = xmax, fill = variable))

p1 <- p + geom_rect(colour = I("grey"), show.legend = FALSE)


ll <- finance$names
ll <- append(ll,hc$names)
ll <- append(ll,industrials$names)
ll <- append(ll,IT$names)

p2 <- p1 + geom_text(aes(x = SectorPercent, y = MarketCap,
                         label = ifelse(segment == "IT", paste(ll," - ", value, "%", sep = ""), 
                                 ifelse(segment == "Health Care", paste(ll," - ", value, "%", sep = ""),
                                 ifelse(segment == "Industrials", paste(ll," - ", value, "%", sep = ""),
                                 paste(ll," - ", value, "%", sep = ""))))), size = 3.5, show.legend = FALSE)

p3 <- p2 + geom_text(aes(x = SectorPercent, y = 103,
                         label = paste("Seg ", segment)), size = 4, show.legend = FALSE)

p3 + theme(legend.position = "none")


p3






###################################################################################

##############################  Radar Plot  #######################################

###################################################################################



setwd("C:/Users/danie/Desktop/Master/Visualización de datos/PECs/PEC2/archive")

data <-read.csv("ndtv_data_final.csv",  sep=",")

attach(data)

vars <- colnames(data)

data$X <- NULL
data$Brand <- NULL
data$Model <- NULL
data$Touchscreen <- NULL
data$Operating.system <- NULL
data$Bluetooth <- NULL
data$Wi.Fi <- NULL
data$GPS <- NULL
data$Number.of.SIMs <- NULL
data$X3G <- NULL
data$X4G..LTE <- NULL

data$Resolution.x <- data$Resolution.x/max(data$Resolution.x)
data$Resolution.y <- data$Resolution.y/max(data$Resolution.y)
data$Screen.size..inches. <- data$Screen.size..inches./max(data$Screen.size..inches.)

data$Screen <- (data$Resolution.x+ data$Resolution.y + data$Screen.size..inches.)/3

data$Resolution.x <- NULL
data$Resolution.y <- NULL
data$Screen.size..inches. <- NULL

data$Rear.camera <- data$Rear.camera/max(data$Rear.camera)
data$Front.camera <- Front.camera/max(Front.camera)
data$Camera <- (data$Rear.camera + data$Front.camera)/2

data$Rear.camera <- NULL
data$Front.camera <- NULL


data$Battery.capacity..mAh. <- data$Battery.capacity..mAh./max(data$Battery.capacity..mAh.)
data$Processor <- data$Processor/max(data$Processor)
data$RAM..MB. <- data$RAM..MB/max(data$RAM..MB)
data$Internal.storage..GB. <- data$Internal.storage..GB./max(data$Internal.storage..GB.)

data$Price <- data$Price/max(data$Price)
data$Price <- 1 - data$Price

# install.packages("fmsb")
library(fmsb)

cdata <- data
cdata$Name <- NULL

colnames(cdata) <- c('Battery','CPU','RAM','Storage','Price','Screen','Camera')




areas <- c(rgb(1, 0, 0, 0.25),
           rgb(0, 1, 0, 0.25),
           rgb(0, 0, 1, 0.25))


df <- data.frame(rbind(rep(1, 8), rep(0, 8), cdata[1,], cdata[2,], cdata[3,]))
colnames(df) <- colnames(cdata)

radarchart(df,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = 2:4,      # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,        # Line type for each line
           pfcol = areas)   # Color of the areas  

legend("topright",
       legend =  data[,1][1:3],
       bty = "n", pch = 20, col = areas,
       text.col = "grey25", pt.cex = 2)






