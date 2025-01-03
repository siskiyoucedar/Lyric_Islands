# run after 4_Filtering_results.R
# do not need to run 5_2_Spotify_songs once ran once

library(spotifyr)
library(collapse)

### SPOTIFY API DETAILS
your_spotify_id <- your_spotify_id
your_spotify_secret <- your_spotify_secret
# https://www.rdocumentation.org/packages/spotifyr/versions/2.2.4

Sys.setenv(SPOTIFY_CLIENT_ID = your_spotify_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = your_spotify_secret)
access_token <- get_spotify_access_token()

song_matches <- read.csv("_all_songs.csv") |>
  select(-X) |>
  tail(-1)

# this bit of code clears out any duplicates, or songs that haven't quite matched right (i.e. don't exist on Spotify)

song_matches_unique <- song_matches |> unique()

song_matches_cleaner <- song_matches_unique  |>
  mutate(
    "first_word_genius" = str_to_upper(word(song_name, 1)),
    "first_word_spotify" = str_to_upper(word(spotify_song_name, 1)),
  ) |>
  # this part is a bit more unfair but it gets rid of songs where first words don't match
  # (rare enough that it's valid) 
  filter(
    first_word_genius == first_word_spotify
  ) |>
  # this gets rid of songs where the name was only present in the genius song title, but is not present in the
  # equivalent on spotify
  mutate(
    "genius_name_test" = str_detect(str_to_upper(song_name), coll(str_to_upper(place21nm))),
    "spotify_name_test" = str_detect(str_to_upper((spotify_song_name)), coll(str_to_upper(place21nm)))
  ) |>
  mutate(
    "song_name_match" = ifelse(genius_name_test == TRUE, ifelse(spotify_name_test == TRUE, "Yes", "No"), "Yes")
  ) |>
  filter(
    song_name_match != "No"
  ) |>
  select(
    -song_name_match, -genius_name_test, -spotify_name_test
  )

### need a good way to clear out songs that have transparently the same name and I can see it but R can't

# another bit to remove songs that have been duplicated in multiple vers

song_matches_cleaned <- song_matches_cleaner |>
  mutate(
    "dupe_name" = paste0(artist_name, first_word_spotify)
  ) |>
  mutate("song_name_length" = nchar(spotify_song_name)) |>          
  group_by(dupe_name, place21nm) |>                    
  slice_min(song_name_length, with_ties = FALSE) |>       
  ungroup() |>
  group_by(id, place21nm) |>                    
  slice_sample(n = 1) |>       
  ungroup() |>
  select(-dupe_name, - first_word_spotify, - first_word_genius, -song_name_length)

rm(song_matches, song_matches_unique, song_matches_cleaner)
write.csv(song_matches_cleaned, "_unique_songs.csv")

### TO DO

# genres will come from a query for get_artists (using the specific artists id).
# spotify links will come from a query for get_track (using the song id).

IDs <- song_matches_cleaned$id |> unique()
artist_IDs <- song_matches_cleaned$artists_id |> unique()

# short forms for testing
ID_test <- head(IDs, 5)
artist_test <- head(artist_IDs, 5)

# song id lookup loop:

number <- 0
song_data <- tibble(id = "", external_url = "", 
                    api_url = "")
for(id in IDs){
  data <- get_track(id)
  # using the collapse package here to get the list return to play ball
  data <- unlist2d(data)
  # bit of a fudge but it works!
  data2 <- tibble(id = data[24,4], external_url = data[22,4], api_url = data[23,4])
  # uri is just spotify id plus spotify:track: at the beginning
  song_data <- bind_rows(song_data, data2)
  number <- number + 1
  print(paste0(((number/4686)*100), "% done"))
}

# write to disk!
write.csv(song_data, "_Processed_data/song_data.csv")
rm(ID_test, IDs)

# artist id lookup loop:

number <- 0
artist_data <- tibble(
  # genres_1 = "w", genres_2 = "W", genres_3 = "y", genres_4 = "Y", genres_5 = "z",id = "Z", popularity = 0, followers.total = 0
  )
for(id in artist_IDs){
  data <- get_artists(id)
  data <- data |> unnest_wider(genres, names_sep = "_") |>
    select(-href, -images, -type, -uri, -external_urls.spotify, -followers.href)
  artist_data <- bind_rows(artist_data, data)
  number <- number + 1
  print(paste0(((number/3311)*100), "% done"))
}
artist_data <- artist_data |>
  select(
    -genres_6, -genres_7, -genres_8, -genres_9, -genres_10, -genres_11, -genres_12, -genres_13, -genres_14
  )

# write to disk!
write.csv(artist_data, "_Processed_data/artist_data.csv")
rm(artist_test, artist_IDs)


### before the final viz you need to find a way to merge back in e.g. st anns, 
### st. ann's so they match those in the list


# summarise data per place name
borough_mergy <- song_matches_cleaned |>
  group_by(place21nm) |>
  summarise(
    "Song Count" = n()
  )

# merge data with placename point object
song_PN_sf <- merge(London_PN_sf, borough_mergy, all.x = TRUE)

# get the geometries right! 
song_PN_sf <- st_set_crs(song_PN_sf, 4326)
song_PN_sf <- st_transform(song_PN_sf, 27700)

# get rid of all placenames outside camden
song_PN_sf <- st_intersection(song_PN_sf, London_LADs)

# replace NA values with 0
song_PN_sf <- song_PN_sf |> mutate(
  "Song Count" = replace_na(`Song.Count`, 0)
)

# bounding box

# Inner_LDN <- London_LADs |> filter(LAD23NM %in% boroughs)

#bbox <- st_bbox(Inner_LDN)

tmap_mode("view")

tm_shape(London_LADs, #bbox = bbox
) +
  tm_fill(col = "maroon", alpha = 0.1) +
  
  tm_shape(borough_shape) +
  tm_fill(col = "maroon", alpha = 0.5) +
  tm_basemap(c(StreetMap = "OpenStreetMap", TopoMap = "OpenTopoMap")) +
  tm_shape(song_PN_sf) +
  tm_symbols(size = "Song Count", col = "beige", alpha = 0.8, scale = 2)


# in terms of the problem with things that aren't songs - could it be resolved with max no. of lines? 
# (no - transparently not...)

