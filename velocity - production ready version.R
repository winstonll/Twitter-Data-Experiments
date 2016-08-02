#Require 4 parameters from command line, 1) data file location, 2) report ID 
#3) Global vs. Tribal (TRUE/FALSE), 4) Name of handle

args = commandArgs(trailingOnly=TRUE)

library(plyr)
library(lubridate)
library(twitteR)
library(stringr)
library(RCurl)
library(boot)
library(rjson)
library(httr)

#The list of variables we are able to investigate
varNames = c("Tu", "We", "Th", "Fr", "Sa", "Su", "1a", "2a", "3a", "4a",
						"5a", "6a", "7a", "8a", "9a", "10a", "11a", "12p", "1p",
						"2p", "3p", "4p", "5p", "6p", "7p", "8p", "9p", "10p",
						"11p", "rt", "link", "vid", "img", "@", "#", "Hourly")

#This function aggregates the input data (in TSV) into hourly metadata

convertTSV = function(data){
	tweet = data.frame(matrix(0, nrow = nrow(data), ncol = 3))
	colnames(tweet) = c("entity", "id", "time")
	tweet$entity = data[, 1]
	tweet$id = data[, 2]
	tweet$time = as.POSIXct(data[,3], format="%a %b %d %H:%M:%S %z %Y", tz="UTC")
	tweet = tweet[order(tweet[,3]), ]
	tweet$time = floor_date(tweet$time, "hour")
	tweet.summary = ddply(tweet,.(time), summarize, count=length(entity))
	tweet.summary$week = factor(weekdays(tweet.summary[,1]), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
											"Saturday", "Sunday"))
	tweet.summary$hour = as.factor(format(tweet.summary[,1], format="%H"))
	tweet.summary
}

#The following functions and commands will download timeline data from Twitter and
#Pre-process and aggregate into hourly data as well to match the follower data format
#We should be able to get rid of all these once we are able to deal with timeline data outside of R

options(httr_oauth_cache = F)
consumer_key = "consumer key"
consumer_secret = "consumer secret"
access_token = "access token"
access_secret = "access secret"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

url_pattern = "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"


extraire <- function(entree,motif){
	res <- regexec(motif,entree)
	if(length(res[[1]])==2){
		debut <- (res[[1]])[2]
		fin <- debut+(attr(res[[1]],"match.length"))[2]-1
		return(substr(entree,debut,fin))
	}else return(NA)}

unshorten <- function(url){
	status = tryCatch(getURL(url, header=TRUE, nobody=TRUE, followlocation=FALSE, 
      cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")),
	error = function(e) e)

	if(!inherits(status, "error")){
		uri <- getURL(url, header=TRUE, nobody=TRUE, followlocation=FALSE, 
      	cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
		res <- try(extraire(uri,"\r\nlocation: (.*?)\r\nserver"))
		return(res)
	}
	return("")
}

getHandleTweet = function(handle, n = 3200, begin, end){
	tl.raw = userTimeline(handle, 3200, includeRts = T)
	tl = do.call(rbind, lapply(tl.raw, function(x) x$toDataFrame()))

	tl = tl[, c("created", "text", "isRetweet")]
	tl$created = floor_date(tl$created, "hour")
	tl = tl[tl$created >= begin & tl$created <= end,]

	tl$at = 0
	tl$link = 0
	tl$image = 0
	tl$video = 0
	tl$hashtag = 0
	tl$count = 1

	for(i in 1:nrow(tl)){
		url = unlist(str_extract_all(tl$text[i], url_pattern))
		url.dec = unlist(lapply(url, unshorten))
		vid = sum(apply(data.frame(grepl("youtube.com", url.dec), 
						grepl("youtu.be", url.dec), 
						grepl("vine.co", url.dec), 
						grepl("video", url.dec), 
						grepl(".gif", url.dec)), 1, any))

		img = sum(apply(data.frame(grepl(".png", url.dec), 
						grepl(".jpg", url.dec), 
						grepl(".jpeg", url.dec),
						grepl("photo", url.dec)), 1, any))

		at = sum(apply(data.frame(grepl("@", tl$text[i])), 1, any))

		hash = sum(apply(data.frame(grepl("#", tl$text[i])), 1, any))

		tl$link[i] = length(url.dec)
		tl$image[i] = img
		tl$video[i] = vid
		tl$at[i] = at
		tl$hashtag[i] = hash
	}

	return(tl)
}



#This function combines timeline data and follower data into one big data frame
getMeta = function(tl, tweet.summary, tribe_level = FALSE){
	tl.summary = ddply(tl,.(created), summarize, rt = sum(isRetweet), link_count = sum(link), 
				video_count = sum(video), image_count = sum(image), 
				at_count = sum(at), hash_count = sum(hashtag), hourly_total = sum(count))

	meta = merge(x = tweet.summary, y = tl.summary, by.x = "time", by.y = "created", all.x = T)
	
	if(tribe_level == F){
		meta[is.na(meta)] = 0
		return(meta)
	} else{
		return(meta[complete.cases(meta),])
	}
}


#This function calculates the coefficients for each variable
lr_coef = function(data, index, tribe_level = FALSE){
	d = data[index, ]
	model = glm(cbind(count, max(count)) ~ week + factor(hour) + rt + 
		link_count + video_count + image_count + at_count + hash_count + hourly_total, 
		data = d, family = "binomial")
	if(tribe_level == FALSE){
		return(exp(coef(model)))
	} else{
		return(cbind(exp(coef(model)),confint(model)))
	}
}


#Given name of handle and report ID, we 
id = args[2]
tribe = args[3]
name = args[4]

data = read.delim(args[1], header = F, sep = "\t", stringsAsFactors = F)
tweet.summary = convertTSV(data)
tl = getHandleTweet(name, begin = tweet.summary[1,1], end = tweet.summary[nrow(tweet.summary),1])

if (tribe ==  FALSE){	
	#global level analysis
	meta = getMeta(tl, tweet.summary)
	r = boot(meta, statistic = lr_coef, R = 5000)$t[,2:37]
	colnames(r) = varNames
	print(summary(r))
} else{
	numCluster = fromJSON(getURL(paste(
		"https://api.affin.io/v1/campaigns/campaigns?api_key=winstonkey&id=",id, 
		sep = "")))$num_of_clusters
	tribeResults = list()
	for(j in 0:(numCluster-1)){
		r = GET(paste("elastic search link", j, sep = ""))
		json =  fromJSON(content(r, as = "text"))$hits$hits[[1]]$`_source`$members
		data$tribe[data$V2 %in% json] = j
		tribeData = data[data$tribe == j,]
		if(nrow(tribeData >= 1000)){
			tribe.summary = convertTSV(tribeData)
			meta = getMeta(tl, tribe.summary, tribe_level = TRUE)
			tribeResults[[j+1]] = lr_coef(meta, index = 1:(nrow(meta)), tribe_level = TRUE)
		}
	
	}
	print(tribeResults)
}
