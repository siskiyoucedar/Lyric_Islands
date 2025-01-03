
library(tidyverse)
library(tmap)
library(sf)

# read in all the relevant objects

artist_data <- read.csv("_Processed_data/artist_data.csv") |>
  select(-X)|> rename("artists_id" = id)
song_data <- read.csv("_Processed_data/song_data.csv") |>
  select(-X) |> tail(-1)
song_matches_cleaned <- read.csv("_Processed_data/_unique_songs.csv") |>
  select(-X) 
London_PN_sf <- st_read("_Processed_data/_London_Placenames_sf.gpkg")

# join dataframes up

song_matches_temp <- left_join(song_matches_cleaned, artist_data, by = "artists_id")
# for some reason the below code is losing a lot of data - seems the song ID lookup couldn't return data for a lot of songs
song_matches <- left_join(song_matches_temp, song_data, by = "id")
rm(song_matches_temp, song_matches_cleaned)

# after the fact code: capture missing urls

ID_clearance <- song_matches |> filter(is.na(api_url))
clearance_test <- ID_clearance$id |> unique()

number <- 0
clearance_data <- tibble(id = "", external_url = "", 
                    api_url = "")
for(id in clearance_test){
  data <- get_track(id)
  # using the collapse package here to get the list return to play ball
  data <- unlist2d(data)
  # bit of a fudge but it works!
  data2 <- tibble(id = data[25,4], external_url = data[23,4], api_url = data[24,4])
  # uri is just spotify id plus spotify:track: at the beginning
  clearance_data <- bind_rows(clearance_data, data2)
  number <- number + 1
  print(paste0(((number/805)*100), "% done"))
}

## this is not working because every dataset is formatted differently. need to find a way to clue exact bits of data

# get rid of the bad eggs

clearance_data <- clearance_data |> mutate(
  "Detection" = ifelse((substr(external_url, 1, 1) == "h"), "Yes", "No")
) |>
  filter(
  Detection == "Yes"
  ) |> 
  select(
    -Detection
  )

# bind back into the old dataset

song_data <- song_data |>
  rbind(
    clearance_data
  )

song_matches_genres <- song_matches
song_matches_genres[genre_list] <- NA
  
song_matches_long <- song_matches |>
  pivot_longer(cols = starts_with("genres_"), 
               names_to = "genre_type", 
               values_to = "genre") |>
  filter(!is.na(genre)) 

# group by place and genre, count occurrences

genre_counts <- song_matches_long |>
  group_by(place21nm, genre) |>
  summarise(no_of_songs = n(), .groups = 'drop')

# find the most popular genre per placename

most_popular_genre <- genre_counts |>
  group_by(place21nm) |>
  slice_max(order_by = no_of_songs, n = 1) |>
  slice_sample(n = 1) |>
  ungroup()

# now that is interesting - but it illustrates we need to manuallt scrap some placenames
# that are too simple to have accurate reads

### final mergy

# summarise data per place name
borough_mergy <- song_matches |>
  group_by(place21nm) |>
  summarise(
    "Song Count" = n()
  )

# merge data with placename point object
song_PN_sf <- merge(London_PN_sf, borough_mergy, all.x = TRUE)
# rm(borough_mergy)

# get the geometries right! 
song_PN_sf <- st_set_crs(song_PN_sf, 4326)
song_PN_sf <- st_transform(song_PN_sf, 27700)

# get rid of all placenames outside camden
song_PN_sf <- st_intersection(song_PN_sf, London_LADs)

# replace NA values with 0
song_PN_sf <- song_PN_sf |> mutate(
  "Song Count" = replace_na(`Song.Count`, 0)
)

tmap_mode("view")

tm_shape(London_LADs) +
  tm_fill(col = "maroon", alpha = 0.5) +
  tm_basemap(c(StreetMap = "OpenStreetMap", TopoMap = "OpenTopoMap")) +
  tm_shape(song_PN_sf) +
  tm_symbols(size = "Song Count", col = "beige", alpha = 0.8, scale = 3)