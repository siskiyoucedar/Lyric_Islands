# run after 4_Filtering_results.R
# do not need to run 5_2_Spotify_songs once ran once

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

### TO DO

# genres will come from a query for get_artists (using the specific artists id).

# get genres for songs
IDs <- song_matches_cleaned$id
genres <- c()

for(id in IDs){
  data <- get_track(id)
  # need to know nature of output to finish this piece of code
}


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

