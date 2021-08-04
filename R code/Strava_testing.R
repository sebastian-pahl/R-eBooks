

#-----------------------TO DO LIST---------------------------


#plot vs meters OR time
#first check if power, hr and cadence data is available
#download from either original data or gpx
#add power zones and hr zones
#power curve and 25w distribution
#give summary stats: total time, total elevation, avg power, avg speed, avg hr, avg cadence
#manual inputs like weight, max HR, FTP
#interesting stats: avg stats (gradient, hr, power) for given distance/time
#calories


#------------------------TESTING-----------------------------


data.gpx <- "Morning_Ride_Kandel.gpx"
data_table <- read.csv(data.gpx, strip.white = TRUE, skip=1,col.names = c("column1"))
data_character <- as.character(data_table[,1])

find_time <- grep("<time>", data_character, value=TRUE)
activity_date <- strptime(find_time, "<time>%Y-%m-%dT%H:%M:%SZ</time>")
time_passed <- difftime(activity_date[-1], activity_date[1])
hours <- as.numeric(time_passed)/60/60

find_elevation <- grep("<ele>", data_character, value=TRUE)
elevation <- as.numeric(gsub("</?ele>","",find_elevation))

find_coordinates <- grep("<trkpt lat", data_character, value=TRUE)
latitude <- as.numeric(substr(find_coordinates,12,21))
longitude <- as.numeric(substr(find_coordinates,27,35))

#extensions
data_string <- toString(data_table)
extensions <- c(str_split(data_string, "/extensions", simplify=TRUE))

#power
power_function <- function(x) {
        extract_power <- str_extract(x, "<power>.*</power>")
        power <- str_extract(extract_power, "\\d+")
        power
}
power <- head(na.locf(as.numeric(lapply(extensions, power_function))),-1)

#pulse
pulse_function <- function(x) {
        extract_pulse <- str_extract(x, "<gpxtpx:hr>.*</gpxtpx:hr>")
        pulse <- str_extract(extract_pulse, "\\d+")
        pulse
}
pulse <- head(na.locf(as.numeric(lapply(extensions, pulse_function))),-1)

#cadence
cadence_function <- function(x) {
        extract_cadence <- str_extract(x, "<gpxtpx:cad>.*</gpxtpx:cad>")
        cadence <- str_extract(extract_cadence, "\\d+")
        cadence
}
cadence <- head(na.locf(as.numeric(lapply(extensions, cadence_function))),-1)

#temperature
temperature_function <- function(x) {
        extract_temperature <- str_extract(x, "<gpxtpx:atemp>.*</gpxtpx:atemp>")
        temperature <- str_extract(extract_temperature, "\\d+")
        temperature
}
temperature <- head(na.locf(as.numeric(lapply(extensions, temperature_function))),-1)

strava_data <- strava_table_creator(data.gpx)


cat("Total elevation gain =", elevation_total, "m\n", "Total distance =", distance_total, "km")

y <- matrix(1:6,2,3)
z <- c(1,2)
merge(y,z)

