# Run 1_placenames before this

### SPOTIFY API DETAILS
your_spotify_id <- your_spotify_id
your_spotify_secret <- your_spotify_secret
# https://www.rdocumentation.org/packages/spotifyr/versions/2.2.4

# add borough here once completed

boroughs <- c("Camden", "Islington", "Haringey", "Hackney")

for (borough in boroughs) {
  df <- read.csv(paste0(borough,"_songs.csv")) |> select(-X)
  df_name <- paste0("songs_", borough)
  assign(df_name, df, envir = .GlobalEnv)
}

# to get the right columns - start with Camden

all_boroughs <- songs_Camden

# pull all the DFs together

for (borough in boroughs) {
df <- get(paste0("songs_", borough))
all_boroughs <- bind_rows(all_boroughs, df) |> unique()
}


# summarise data per place name
borough_mergy <- all_boroughs |>
  group_by(place21nm) |>
  summarise(
    "Song Count" = n()
  )

# merge data with placename point object
song_PN_sf <- merge(London_PN_sf, borough_mergy, all.x = TRUE)

# get the geometries right! 
song_PN_sf <- st_set_crs(song_PN_sf, 4326)
song_PN_sf <- st_transform(song_PN_sf, 27700)

# get a shape of just the boroughs we've done
borough_shape <- London_LADs |> filter(LAD23NM %in% boroughs) |> select("Name" = LAD23NM)

# get rid of all placenames outside camden
song_PN_sf <- st_intersection(song_PN_sf, borough_shape)

# replace NA values with 0
song_PN_sf <- song_PN_sf |> mutate(
  "Song Count" = replace_na(`Song.Count`, 0)
)

tmap_mode("view")

tm_shape(borough_shape) +
  tm_fill(col = "maroon", alpha = 0.3) +
  tm_basemap(c(StreetMap = "OpenStreetMap", TopoMap = "OpenTopoMap")) +
  tm_shape(song_PN_sf) +
  tm_symbols(size = "Song Count", col = "gold", alpha = 0.8, scale = 4)


# artist_name - for expulsion later...
artists <- c("H.G. Wells", "Sir Arthur Conan Doyle", "William Shakespeare", 
             "Virginia Woolf", "Daniel Defoe", "Charles Dickens", 
             "Oscar Wilde", "Martin Amis", "George Bernard Shaw", 
             "D.H. Lawrence", "Ben Jonson", "William Hazlitt", 
             "Arthur Edward Waite", "Jane Austen", "Dorothy L. Sayers", 
             "Frances Burney", "David Hume", "George Eliot", 
             "Ford Madox Ford", "Robert Louis Stevenson", "Henry James", 
             "George Gissing", "Mary Elizabeth Braddon", "Jonathan Swift", 
             "Henry Fielding")