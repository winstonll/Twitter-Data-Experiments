library(mclust)
library(stringr)
library(plyr)
options(dplyr.width = Inf)
library(RCurl)
library(RJSONIO)
library(stringr)
library(depth)
library("digest")
library("base64enc")
library("utils")
library("httr")
library("rjson")


setwd("C:/Users/Zheng/Documents/GitHub/data/")

getAuthorizationTokenUsingMasterKey <- function(verb, resourceId, resourceType, date, masterKey) {
	key <- base64decode(masterKey)
	text <- paste(tolower(verb), "\n", tolower(resourceType), "\n", resourceId, "\n", tolower(date), "\n\n", sep = "")
	signature = base64encode(hmac(key, text, "sha256", raw=TRUE))
    	MasterToken = "master";
	TokenVersion = "1.0";
	URLencode(paste("type=", MasterToken, "&ver=", TokenVersion, "&sig=", signature, sep = ""), TRUE)
}

docdbaccount <- "docdb account name goes here"
masterkey <- "master key goes here"
db <- "LocationDB"
collection <- "LocationCollection"

users = read.csv("users.cleaned.csv", header=T, stringsAsFactors = F, encoding = "UTF-8")
friends = read.csv("friends.cleaned.csv", header=T, stringsAsFactors = F, encoding = "UTF-8")
network = read.table("friends.txt", header=FALSE, sep = "\t", as.is=TRUE, col.names = c("User", "Following"))
users = users[users$location != "", ]

domesticTime = c("Atlantic", "Pacific", "Mountain", "Eastern", "Central", "Alaska", "Arizona")

getUIDs = function(userID, data, following = TRUE){
	if(following == TRUE){
		subset = data[data$User == userID, ]
	} else{
		subset = data[data$Following == userID, ]
	}
	subset
}

getStatus = function(network, userData){
	merge(network, userData, by.x = "Following", by.y = "id")
}

getLocationInfo = function(locations){
	locations = str_replace_all(locations, "#", "")
	locInfo = matrix(0, nrow = length(locations), ncol = 7)
	locInfo[,1] = locations

	for(i in 1:nrow(locInfo)){
		loc = locations[i]
		loc = paste0(toupper(substr(loc, 1, 1)), substr(loc, 2, nchar(loc)))
		print(paste("User", j, "out of", iter, "users;", "record", i, "/", nrow(locInfo), sep = " "))
		if(grepl("/", loc)){
			loc = substr(loc, 1, str_locate_all(pattern ="/",loc)[[1]][1,1] - 1)
		}
		
		if(grepl(" and ", loc)){
			loc = substr(loc, 1, str_locate_all(pattern =" and ",loc)[[1]][1,1] - 1)
		}

		if(grepl(" 2 ", loc)){
			loc = substr(loc, 1, str_locate_all(pattern =" 2 ",loc)[[1]][1,1] - 1)
		}
		
		if(grepl(" & ", loc)){
			loc = substr(loc, 1, str_locate_all(pattern =" & ",loc)[[1]][1,1] - 1)
		}

		if(grepl(" • ", loc)){
			loc = substr(loc, 1, str_locate_all(pattern =" • ",loc)[[1]][1,1] - 1)
		}

		if(grepl("\u2708", loc)){
			loc = substr(loc, 1, str_locate_all(pattern ="\u2708",loc)[[1]][1,1] - 1)
		}
		
		docid <- loc
		date <- http_date(Sys.time())
		auth <- getAuthorizationTokenUsingMasterKey("GET", paste("dbs/", db, "/colls/", collection, "/docs/", docid, sep=""), "docs", date, masterkey)
		response <- GET(paste("https://", docdbaccount, ".documents.azure.com/dbs/", db, "/colls/", collection, "/docs/", URLencode(docid), sep=""), 
			add_headers(Authorization = auth, "x-ms-version" = "2015-12-16", "x-ms-date" = date))

		docResp <- fromJSON(rawToChar(response$content))

		if(any(docResp$code == "NotFound", !is.null(docResp$code))){
			url.temp = paste("http://dev.virtualearth.net/REST/v1/Locations?q=", loc, "&o=json&key=AkoJChxlmptBbHxus56KYPJjUTImFOBhcTwJMUSMsS1Y816baqQDa9vGsPwMHopI", sep = "")
			url.temp = gsub(" ", "%20", url.temp)
			bingResp = fromJSON(getURL(url.temp))
			
			if(bingResp$statusCode == "400"){
				bingResults = 0
			} else{
				bingResults = bingResp$resourceSets[[1]]
			}

			if(length(unlist(bingResults)) > 1){
				full = bingResults$resources[[1]]$address$formattedAddress
				
				if(bingResults$resources[[1]]$confidence == "High"){
					bingResp$id = loc
					bingResp = toJSON(bingResp)
					date <- http_date(Sys.time())
					auth <- getAuthorizationTokenUsingMasterKey("POST", paste("dbs/", db, "/colls/", collection, sep=""), "docs", date, masterkey)
					response = POST(paste("https://", docdbaccount, ".documents.azure.com/dbs/", db, "/colls/", collection, "/docs", sep=""), 
						add_headers(Authorization = auth, "x-ms-version" = "2015-12-16", "x-ms-date" = date, "x-ms-documentdb-is-upsert" = "true"), body=bingResp)
				}
			
			} else
				next
		} else{
			if(is.null(docResp$address$formattedAddress)){
				full = docResp$resourceSets[[1]]$resources[[1]]$address$formattedAddress
			} else
				full = docResp$address$formattedAddress
		}
			
		url = paste("https://geonames.com/search.php?q=", full, sep = "")
		url = gsub(" ", "%20", url)
		info = fromJSON(getURL(url))
		
		if(length(info) != 0){
			locInfo[i, 2] = as.numeric(info[[1]]$location[1])
			locInfo[i, 3] = as.numeric(info[[1]]$location[2])
			locInfo[i, 4] = as.numeric(info[[1]]$population)
			locInfo[i, 5] = info[[1]]$countryCode
			locInfo[i, 6] = info[[1]]$fullName[1]
			locInfo[i, 7] = info[[1]]$timezone				
		}
	}

	locInfo = data.frame(matrix(locInfo, nrow = length(locInfo)/7), stringsAsFactors=FALSE)
	locInfo = locInfo[locInfo[,5] != 0,]
	colnames(locInfo) = c("Locations", "Lat", "Long", "Population", "Country", "Name", "Timezone")
	locInfo = locInfo[locInfo$Population != 0, ]
	locInfo
}

getCluster = function(locData){
  	cl = Mclust(locData)
	tab = table(cl$classification)
	count = tab[tab == max(tab)]
	freq = round(tab[tab == max(tab)]/sum(tab), 3)
  	largestCl = names(tab)[tab == max(tab)][1]
	median = med(locData[cl$classification == largestCl,], method = "Spatial")
	median = round(unlist(median), 3)
	url = paste("https://geonames.com/search.php?lat=", median[1], "&lon=", median[2], sep = "")
	info = fromJSON(getURL(url))
	clCity = (rep("", 6))
	if(length(unlist(info)) > 1){
		clCity[1] = paste(info[[1]]$name, ", ", info[[1]]$admin1Code, ", ", info[[1]]$countryCode, sep = "")
		clCity[2] = info[[1]]$countryCode
		clCity[3] = info[[1]]$population
		clCity[4] = info[[1]]$timezone
		clCity[5] = count
		clCity[6] = freq
	}
	clCity
}

getTopLocations = function(locInfo){
	topLoc = matrix(0, nrow = 5, ncol = 6)
	colnames(topLoc) = c("Name", "Country", "Population", "Timezone", "Count", "Freq")
	rownames(topLoc) = c("Loc1", "Loc2", "Loc3", "Clustering", "Timezone")
	
	if(nrow(locInfo) > 10){
		locInfo[,2] = round(as.numeric(locInfo[,2]), 3)
		locInfo[,3] = round(as.numeric(locInfo[,3]), 3)
	
		locData = data.frame(lat=locInfo[,2], lng=locInfo[,3])
  		topLoc["Clustering", ] = getCluster(locData)

		freq1 = locInfo %>% group_by(Lat, Long)
		freq2 = ddply(freq1, .(Name, Country, Population, Timezone), summarise, Count = length(Locations))
		freq3 = freq2[order(freq2$Count, as.numeric(freq2$Population), decreasing = T), ]
		freq3$Freq = round(freq3$Count/sum(freq3$Count), 3)
		freq3 = freq3[order(freq3$Freq, decreasing = T),]

		topLoc[1,] = unlist(freq3[1,])
		topLoc[2,] = unlist(freq3[2,]) 
		topLoc[3,] = unlist(freq3[3,])
	}

	topLoc[5, 4] = users$time_zone[j]
	return(topLoc)
}

predictLocation = function(topLoc){
	result = ""
	confidence = "Low"
	timeZone = topLoc[5,4]

	if(any(grepl(timeZone, domesticTime, ignore.case = TRUE)) | timeZone == ""){
		if(topLoc[1,6] > 0.15){
			result = topLoc[1,1]
			confidence = "High"
		} else if(topLoc[4,6] > 0.3){
			result = topLoc[4,1]
			confidence = "Medium"
		} else{
			result = topLoc[topLoc[,6] == max(topLoc[,6]), 1]
		}
	
	} else if(!any(grepl(timeZone, domesticTime, ignore.case = TRUE) & timeZone != "")){

		if(grepl("/", timeZone)){
			result = substr(timeZone, str_locate_all(pattern ="/",timeZone)[[1]][1,2] + 1, nchar(timeZone))

		} else if(topLoc[1,6] > 0.15){
			result = topLoc[1,1]
			confidence = "High"
		} else if(topLoc[4,6] > 0.3){
			result = topLoc[4,1]
			confidence = "Medium"
		} else{
			result = topLoc[topLoc[,6] == max(topLoc[,6]), 1]
		}

	} else{
		result = "Not Enough Information"
	}
	
	list(result = result, confidence = confidence)
}

iter = nrow(users)
prediction = matrix("", nrow = iter, ncol = 2)

for (j in 1:iter){
	network.sub = getUIDs(users[j, 4], network)
	networkInfo = getStatus(network.sub, friends)
	networkInfo = networkInfo[networkInfo$verified == "False", ]
	locations = networkInfo$location[networkInfo$location != ""]
	if(length(locations) > 0){
		locInfo = getLocationInfo(locations)
		topLoc = getTopLocations(locInfo)
		prediction[j,] = predictLocation(topLoc)
	} else{
		prediction[j,] = c("Not enough Data", "Low")
	}
}

result = cbind(users$id, prediction)
colnames(result) = c("ID", "Predicted Location", "Confidence")

