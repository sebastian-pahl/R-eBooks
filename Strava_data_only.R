data.gpx <- "Morning_Ride_Kandel.gpx"
data_table <- read.csv(data.gpx, strip.white = TRUE, skip=1,col.names = c("column1"))
data_character <- as.character(data_table[,1])

find_time <- grep("<time>", data_character, value=TRUE)
activity_date <- strptime(find_time, "<time>%Y-%m-%dT%H:%M:%SZ</time>")
hours <- difftime(activity_date[-1], activity_date[1])/60/60

find_elevation <- grep("<ele>", data_character, value=TRUE)
elevation <- as.numeric(gsub("</?ele>","",find_elevation))

find_coordinates <- grep("<trkpt lat", data_character, value=TRUE)
latitude <- as.numeric(substr(find_coordinates,12,21))
longitude <- as.numeric(substr(find_coordinates,27,35))

find_power <- grep("<power>", data_character, value=TRUE)
power <- as.numeric(gsub("</?power>","",find_power))

find_pulse <- grep("<gpxtpx:hr>", data_character, value=TRUE)
pulse <- as.numeric(gsub("</?gpxtpx:hr>","",find_pulse))

strava_data <- strava_table_creator(data.gpx)
