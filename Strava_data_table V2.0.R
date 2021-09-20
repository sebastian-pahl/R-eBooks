setwd("/Users/sebastianpahl/Git/Strava-project/")

library("XML")
library("geosphere") #need for distCosine

#ZONES: if change, also change in R markdown script
zone_names <- c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5")
power_zones <- c(0, 150, 200, 300, 400)
hr_zones <- c(0, 110, 135, 150, 165)
cadence_zones <- c(0, 70, 80, 90, 100)
zones <- data_frame(zone_names, power_zones, hr_zones, cadence_zones)
power_zone_checker <- function(x, z = zones) {
        
        if(x < z$power_zones[2]) {
                power = z$zone_names[1]
        }
        else if(x < z$power_zones[3]) {
                power = z$zone_names[2]
        }
        else if(x < z$power_zones[4]) {
                power = z$zone_names[3]
        }
        else if(x < z$power_zones[5]) {
                power = z$zone_names[4]
        }
        else (power = z$zone_names[5])
}
hr_zone_checker <- function(x, z = zones) {
        
        if(x < z$hr_zones[2]) {
                hr = z$zone_names[1]
        }
        else if(x < z$hr_zones[3]) {
                hr = z$zone_names[2]
        }
        else if(x < z$hr_zones[4]) {
                hr = z$zone_names[3]
        }
        else if(x < z$hr_zones[5]) {
                hr = z$zone_names[4]
        }
        else (hr = z$zone_names[5])
}
cadence_zone_checker <- function(x, z = zones) {
        
        if(x < z$cadence_zones[2]) {
                cadence = z$zone_names[1]
        }
        else if(x < z$cadence_zones[3]) {
                cadence = z$zone_names[2]
        }
        else if(x < z$cadence_zones[4]) {
                cadence = z$zone_names[3]
        }
        else if(x < z$cadence_zones[5]) {
                cadence = z$zone_names[4]
        }
        else (cadence = z$zone_names[5])
}
strava_table_creator <- function(data.gpx) {
        
        data_xml <- xmlTreeParse(data.gpx)
        data_list <- xmlToList(data_xml)
        data_matrix <- t(data_list[["trk"]][["trkseg"]])
        
        #ELEVATION, TIME, COORDINATES---------------------------------------------
        elevation <- as.numeric(data_matrix[,"ele"])
        
        date <- strptime(as.character(data_matrix[,"time"]), "%Y-%m-%dT%H:%M:%SZ")
        time_passed <- c(0, difftime(date[-1], date[1]))
        hours <- as.numeric(time_passed)/60/60
        
        latitude <- as.numeric(sapply(data_matrix[,".attrs"],"[", "lat"))
        longitude <- as.numeric(sapply(data_matrix[,".attrs"],"[", "lon"))
        
        strava_data <- data.frame(hours, elevation, latitude, longitude)
        
        #DISTANCE----------------------------------------------------------------
        meters <- distCosine(strava_data[3:4])
        distance_function <- function(m) {
                distance <- as.numeric()
                distance[1] = 0
                for(i in 0:(length(m)-1)) {
                        distance[i+2] = distance[i+1] + m[i+1]
                }
                distance/1000
        }
        strava_data$distance <- distance_function(meters)
        
        #POWER-------------------------------------------------------------------
        denominator <- length(latitude)
        try(power_raw <- sapply(data_matrix[,"extensions"], "[", "power"), silent=TRUE)
        power_null <- is_empty(unlist(power_raw))
        if(!power_null) {
                null_to_avg <- function(x) {
                        x[is_empty(x)] = round(mean(as.numeric(unlist(power_raw))))
                        x
                }
                power <- as.numeric(lapply(power_raw, null_to_avg))
                strava_data$power <- power
                strava_data$power_zone <- sapply(power, power_zone_checker)
        } 
        
        #HR, CADENCE, TEMPERATURE------------------------------------------------
        #trackpoint extension looks different if power is recorded to when it is not... join these:
        trackpoint_fun <- function(x, tp) {
                y=as.numeric()
                for (i in 1:length(x)) {
                        if(is.null(x[i][["trkpt"]][["TrackPointExtension"]][[tp]])) {
                                y[i] = x[i][["trkpt"]][tp,]
                        }
                        else {y[i] = x[i][["trkpt"]][["TrackPointExtension"]][[tp]]}
                }
                y        
        }
        
        hr_try <- try(hr <- as.numeric(trackpoint_fun(data_matrix[,"extensions"], tp="hr")), silent=TRUE)
        if(class(hr_try)!="try-error") {
                strava_data$hr <- hr
                strava_data$hr_zone <- sapply(hr, hr_zone_checker)
        }
        
        cadence_try <- try(cadence <- as.numeric(trackpoint_fun(data_matrix[,"extensions"], tp="cad")), silent=TRUE)
        if(class(cadence_try)!="try-error") {
                strava_data$cadence <- cadence
                strava_data$cadence_zone <- sapply(cadence, cadence_zone_checker)
        }
        
        
        temperature_try <- try(temperature <- as.numeric(trackpoint_fun(data_matrix[,"extensions"], tp="atemp")), silent=TRUE)
        if(class(temperature_try)!="try-error") {
                strava_data$temperature <- temperature
        }
        strava_data
}

#create data frame from gpx file (load your own gpx file here)
#Morning_Ride_Kandel.gpx
#Tuesday_Trails.gpx
#Evening_Swim.gpx
#Lunch_Run.gpx
strava_data <- strava_table_creator("./gpx_files/Morning_Ride_Kandel.gpx")
write_csv(strava_data, "strava_data.csv")

#power and hr indicators

avg_finder <- function(interval, my_data) {
        vector <- as.numeric()
        for (i in (interval*60):length(my_data)) {
                vector[i] = mean(my_data[(i-(interval*60)+1):i])
        }
        vector
}
max_finder <- function(intervals, parm_data) {
        vector <- as.numeric()
        for (i in 1:length(intervals)) {
                vector[i] = max(avg_finder(interval = intervals[i], my_data = parm_data), na.rm=TRUE)
        }
        vector
}
intervals <- c(1/60,5/60,15/60, 1,2,3,4,5, 10,15,20,30)

power <- max_finder(intervals, parm_data = strava_data$power)
hr <- max_finder(intervals, parm_data = strava_data$hr)
cadence <- max_finder(intervals, parm_data = strava_data$cadence)
parm_curves <- data_frame(intervals, power, hr, cadence)
write_csv(parm_curves, "parm_curves.csv")

#FIGURE OUT#
#max(avg_finder(60, my_data = strava_data$hr), na.rm=TRUE)

#-----------------------TO DO LIST---------------------------

#add power zones and hr zones
#power curve and 25w distribution
#give summary stats: total time, total elevation, avg power, avg speed, avg hr, avg cadence
#manual inputs like weight, max HR, FTP
#interesting stats: avg stats (gradient, hr, power) for given distance/time
#calories
#first check if power, hr and cadence data is available
#download from either original data or gpx

#COORDINATE PLOTS

#coordinates plotted on google maps
library("ggmap")
register_google(key = "<key>") #generate own Maps API on Google Cloud Platform
StravaMap <- get_googlemap(center = c(mean(strava_data$longitude), mean(strava_data$latitude)),
                           zoom =11)
ggmap(StravaMap) +
        geom_point(data=strava_data, aes(longitude, latitude, colour=elevation), 
                   size = 1) +
        scale_color_gradient2(low = "yellow", high = "black", mid = "red", midpoint=500)
#ggplotly(y)  #can use ggplotly to create interactive plot in Viewer, but quite laggy