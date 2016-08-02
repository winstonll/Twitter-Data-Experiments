#Running this script requires two parameters, 1) location of data file and
#2)Confidence level (0 to 1) indicating roughly the amount of data points to be
#flagged.

args = commandArgs(trailingOnly=TRUE)
options(warn = -1)

library(AnomalyDetection)
library(lubridate)
library(plyr)

convertTSV = function(data){
	tweet = data.frame(matrix(0, nrow = nrow(data), ncol = 3))
	colnames(tweet) = c("entity", "id", "time")
	tweet$entity = data[, 1]
	tweet$id = data[, 2]
	tweet$time = as.POSIXct(data[,3], format="%a %b %d %H:%M:%S %z %Y", tz="UTC")
	tweet = tweet[order(tweet[,3]), ]
	tweet$time = floor_date(tweet$time, "hour")
	tweet.summary = ddply(tweet,.(time), summarize, count=length(entity))
	tweet.summary
}

data = read.delim(args[1], header = F, sep = "\t", stringsAsFactors = F)
tweet.summary = convertTSV(data)

#Tweet's own anomaly detection package
res = AnomalyDetectionTs(tweet.summary, max_anoms = 2*(1-args[2]), direction = 'both', plot = TRUE)


#Holt Winters

counter = 1
holtWinters = c()

for (i in round(0.1*nrow(tweet.summary), 0):(nrow(tweet.summary) - 1)){
	train = ts(tweet.summary[1:i, 2], freq = 24)
	model = HoltWinters(train)
	test = forecast.HoltWinters(model, h=1, level= args[2]*100)
	if (tweet.summary[i+1, 2] > test$upper | tweet.summary[i+1, 2] < test$lower){
		holtWinters[counter] = tweet.summary[i+1, 2]
		counter = counter + 1
	}
}

anoms = cbind(res$anoms[res$anoms[,2] %in% holtWinters,])
colnames(anoms) = c("Timestamp", "Hourly Traffic")
print(anoms)