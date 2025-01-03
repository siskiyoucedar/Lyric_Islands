# Run 1_placenames before this

# westminster has no "victoria" because of an error, this may be completed manually

boroughs <- London_PN_sf$lad21nm |> unique() 

for (borough in boroughs) {
  df <- read.csv(paste0("_Processed_data/", borough,"_songs.csv")) |> select(-X)
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

# don't need them anymore so get rid of the individual borough ones

rm_list <- c()
for (borough in boroughs) {
  rm_list <- append(rm_list, paste0("songs_", borough))
}

for (i in rm_list) {
  rm(list = i)
}

# summarise data per place name
borough_mergy <- all_boroughs |>
  group_by(place21nm) |>
  summarise(
    "Song Count" = n()
  )

# merge data with placename point object
song_PN_sf <- merge(London_PN_sf, borough_mergy, all.x = TRUE)
rm(borough_mergy)

# get the geometries right! 
song_PN_sf <- st_set_crs(song_PN_sf, 4326)
song_PN_sf <- st_transform(song_PN_sf, 27700)

# get rid of all placenames outside camden
song_PN_sf <- st_intersection(song_PN_sf, London_LADs)

# replace NA values with 0
song_PN_sf <- song_PN_sf |> mutate(
  "Song Count" = replace_na(`Song.Count`, 0)
)

# bounding box - not currently used

# Inner_LDN <- London_LADs |> filter(LAD23NM %in% boroughs)

# bbox <- st_bbox(Inner_LDN)

tmap_mode("view")

#tm_shape(London_LADs, 
         #bbox = bbox
         #) +
  #tm_fill(col = "maroon", alpha = 0.1) +

tm_shape(London_LADs) +
  tm_fill(col = "maroon", alpha = 0.5) +
  tm_basemap(c(StreetMap = "OpenStreetMap", TopoMap = "OpenTopoMap")) +
  tm_shape(song_PN_sf) +
  tm_symbols(size = "Song Count", col = "beige", alpha = 0.8, scale = 3)

# in terms of the problem with things that aren't songs - could it be resolved with max no. of lines? 
# (no - transparently not...)

rm(df)

