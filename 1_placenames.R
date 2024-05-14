library(tidyverse)
library(tmap)
library(sf)

### importing placenames

# This is a nice OS dataset, available here:
# https://geoportal.statistics.gov.uk/datasets/8f8b561f256b40c3a6df71e400bb54f0/about 

OS_placenames <- read.csv("_Census_data/IPN_GB_2022.csv")

# select just the columns for our purposes:
# basic character name, full name, county name, country name, LAD code, LAD name, region name, latitude & longitude

placenames_crop <- OS_placenames |>
  select(
    placesort, place21nm, ctyltnm, ctry21nm, lad21cd, lad21nm, rgn21nm, lat, long
  )
rm(OS_placenames)

# just select London places

London_placenames <- placenames_crop |>
  filter(rgn21nm == "London") 

# gonna remove the wider placename set for now - but may need later

rm(placenames_crop)

# 2456 obs! that's more than I inspected...

# going to quickly write this for inspection

write.csv(London_placenames, "_Census_data/London.csv", row.names = FALSE, append = TRUE)

# what about a plot to check what's going on?

# first import an LAD map of the UK. Available here:
# https://geoportal.statistics.gov.uk/datasets/127c4bda06314409a1fa0df505f510e6_0/explore 

LADs <- st_read("_Spatial_data/Local_Authority_Districts_December_2023_Boundaries_UK_BFC.gpkg")

# specifically London LADs

London_LADs <- LADs |> filter(
  str_detect(
    LAD23CD, "^E09"
    )
  )
rm(LADs)

#London_PN_sf <- London_placenames|>
#  st_as_sf(coords = c("long", "lat"))

#tmap_mode("view")

#tm_shape(London_LADs) +
#  tm_fill(col = "maroon", alpha = 0.3) +
#  tm_basemap(c(StreetMap = "OpenStreetMap", TopoMap = "OpenTopoMap")) +
#  tm_shape(London_PN_sf) +
#  tm_symbols(size = 0.1, col = "black")

# seems like borough names are massively overstated.
# let's kick em out!

kickable_names = c()
vars = c(London_LADs$LAD23NM)

for (NM in vars) {
  kickable_names <- kickable_names |>
    append(NM) |>
    append(
      paste0(
        NM, ", unparished area"
      )
    )
}

#London_PN_2 <- London_PN_2 |>
#  filter(!(place21nm %in% kickable_names))

London_placenames_2 <- London_placenames |>
  filter(!(place21nm %in% kickable_names))
rm(kickable_names)

# try that again:

#tm_shape(London_LADs) +
#  tm_fill(col = "maroon", alpha = 0.3) +
#  tm_basemap(c(StreetMap = "OpenStreetMap", TopoMap = "OpenTopoMap")) +
#  tm_shape(London_PN_2) +
#  tm_symbols(size = 0.1, col = "black")

# what I'm keen to do is avoid duplicate names
# but I'm conscious that some names recur based on LAD and are different
# so we'll create 33 different DFs and then merge back in.

#for (NM in vars) {
#  df <- London_placenames_2 |>
#    filter(lad21nm == NM)
#  df_name <- paste0("df_", NM)
#  assign(df_name, df, envir = .GlobalEnv)
#}

# test plot: Camden

#London_PN_cam <- df_Camden|>
#  st_as_sf(coords = c("long", "lat"))

#tm_shape(London_LADs) +
#  tm_fill(col = "maroon", alpha = 0.3) +
#  tm_basemap(c(StreetMap = "OpenStreetMap", TopoMap = "OpenTopoMap")) +
#  tm_shape(London_PN_cam) +
#  tm_symbols(size = 0.1, col = "black")

# looks good.

# another for loop - this time summarising by neighbourhood name per DF

#for (NM in vars) {
#  df <- London_placenames_2 |>
#    filter(lad21nm == NM)
#  df <- df |>
#    group_by(place21nm) |>
#    summarise(
#      "lat" = mean(lat),
#      "long" = mean(long)
#    ) |>
#    mutate(
#      "lad21nm" = NM
#    )
#  df_name <- paste0("df_", NM, "2")
#  assign(df_name, df, envir = .GlobalEnv)
#}

# test for camden - see if there's only one belsize between the two belsizes

#London_PN_cam2 <- df_Camden2 |>
#  st_as_sf(coords = c("long", "lat"))

#tm_shape(London_LADs) +
#  tm_fill(col = "maroon", alpha = 0.3) +
#  tm_basemap(c(StreetMap = "OpenStreetMap", TopoMap = "OpenTopoMap")) +
#  tm_shape(London_PN_cam2) +
#  tm_symbols(size = 0.1, col = "black")

# it works!

# merge back in

df_all <- tibble("place21nm" = c(), "lat" = c(), "long" = c(), "lad21nm" = c())

for (NM in vars) {
  df <- London_placenames_2 |>
    filter(lad21nm == NM)
  df <- df |>
    group_by(place21nm) |>
    summarise(
      "lat" = mean(lat),
      "long" = mean(long)
    ) |>
    mutate(
      "lad21nm" = NM
    )
  df_all <- df_all |>
    rbind(df)
}
rm(df, NM, London_placenames)

London_PN_sf <- df_all |>
  st_as_sf(coords = c("long", "lat"))

tmap_mode("view")

tm_shape(London_LADs) +
  tm_fill(col = "maroon", alpha = 0.3) +
  tm_basemap(c(StreetMap = "OpenStreetMap", TopoMap = "OpenTopoMap")) +
  tm_shape(London_PN_sf) +
  tm_symbols(size = 0.05, col = "black")

# still some minor botheration with e.g. "St. Ann's" vs "St Ann's" but otherwise good

# so... 1633 variables. that's doable, right?

# shall we go borough by borough?
  
for (NM in vars) {
  df <- London_placenames_2 |>
    filter(lad21nm == NM)
  df <- df |>
    group_by(place21nm) |>
    summarise(
      "lat" = mean(lat),
      "long" = mean(long)
    ) |>
    mutate(
      "lad21nm" = NM
    )
  df_name <- paste0("df_", NM)
  assign(df_name, df, envir = .GlobalEnv)
}

### querying placenames in the API from genius.com

## HOW DO YOU DO THAT

# I have no idea what's going to happen here - so we'll run a test first.

# get the url right...

# ID:
# 82pQcD_kh-FLJNLtC_lqQ1zdRt79U6akDH5-qeTvOwRoM10hOyQCzqhe-yewRJtu
# key:
# 4UqWZEShwKF4pqNdc7db9cU2E0YKH92VcwBpTIwtbQAu6qz88lvWUl1xku-9RuuptTsfvkOODq_CHLLik9cwlA
# access token:
# 4BmAaA0il4sQauwJKTnY4-oBh_bpPxtgDJQ2bwQOvhJCPcaawAeIH6aWB_oTGlQ5

# Option 1 - call the API itself via httr

library(httr)
# response = GET("https://api.genius.com/search?q=Belsize&page=2&access_token=4BmAaA0il4sQauwJKTnY4-oBh_bpPxtgDJQ2bwQOvhJCPcaawAeIH6aWB_oTGlQ5")
# note that "page=" in the above is necessary to extract more than 7-10 results. This is annoying!
# data <- fromJSON(rawToChar(response$content))
# result <- data$response$hits$result

# the problem with this is that it seems to only return the first few results.
# I need another arg for scope. let's try and find it.

## Option 2 - use the GeniusR package to root around.

library(geniusr)
my_token <- "4BmAaA0il4sQauwJKTnY4-oBh_bpPxtgDJQ2bwQOvhJCPcaawAeIH6aWB_oTGlQ5"

## bug workaround via https://github.com/ewenme/geniusr/issues/17 

library(rvest)
library(xml2)
library(rlang)

get_lyrics <- function (session) {
  lyrics <-  session %>% html_nodes(xpath = '//div[contains(@class, "Lyrics__Container")]')
  song <-  session %>% html_nodes(xpath = '//span[contains(@class, "SongHeaderdesktop__")]') %>% html_text(trim = TRUE)
  artist <-  session %>% html_nodes(xpath = '//a[contains(@class, "HeaderArtistAndTracklistdesktop__Artist")]') %>% html_text(trim = TRUE)
  xml_find_all(lyrics, ".//br") %>% xml_add_sibling("p", "\n")
  xml_find_all(lyrics, ".//br") %>% xml_remove()
  lyrics <- html_text(lyrics, trim = TRUE)
  lyrics <- unlist(strsplit(lyrics, split = "\n"))
  lyrics <- grep(pattern = "[[:alnum:]]", lyrics, value = TRUE)
  if (is_empty(lyrics)) {
    return(tibble(line = NA, section_name = NA, section_artist = NA, 
                  song_name = song, artist_name = artist))
  }
  section_tags <- nchar(gsub(pattern = "\\[.*\\]", "", lyrics)) == 0
  sections <- geniusr:::repeat_before(lyrics, section_tags)
  sections <- gsub("\\[|\\]", "", sections)
  sections <- strsplit(sections, split = ": ", fixed = TRUE)
  section_name <- sapply(sections, "[", 1)
  section_artist <- sapply(sections, "[", 2)
  section_artist[is.na(section_artist)] <- artist
  tibble(line = lyrics[!section_tags], section_name = section_name[!section_tags], 
         section_artist = section_artist[!section_tags], song_name = song,
         artist_name = artist)
}
assignInNamespace("get_lyrics", get_lyrics, "geniusr")

# maybe we can get more results out using the "n_results = " argument..?
Belsize <- search_song(search_term = "Belsize", 
                      n_results = 149, 
                      access_token = my_token)
# that is annoying. it starts returning wrong results pretty fast.
# what about "Camden"?
Camden <- search_song(search_term = "Camden", 
                      n_results = 149, 
                      access_token = my_token)
# different problem - loads of artists called "Camden". 
# maybe there's a fix here...?
# Camden <- search_genius(search_term = "Camden", 
#                       n_results = 99, 
#                       access_token = my_token)

# no that's too much info and doesn't have what I need
# ok so this is processor intensive as a suggestion , but what if....

# use this - https://ewenme.github.io/geniusr/reference/index.html#lyrics-data
# for each song returned for each location, query *again* to get full lyrics.
# if - in those lyrics - we find the geographic indicator, we say yes! move on
# if we don't - cull that result.
# else I think a max of 149 is sound. anything over seems to generate errors

## PULL LYRICS

# need to define location

# keep things simple for now - just Camden

locations <- df_Camden$place21nm

for (location in locations) {
  # first, create a df of all the results for the location
  df <- search_song(search_term = location, 
                    n_results = 149, 
                    access_token = my_token)
  # then, create a list of all the IDs of these songs
  ids <- df$song_id
  # then, another for loop:
  lyric_proof = c()
  for(id in ids) {
    lyric_output <- get_lyrics_id(
      id, 
      access_token = my_token
      )
    # create one string that is all the lyrics
    lyric <- paste(lyric_output$line, collapse = " ")
    song_name <- (df |> filter(song_id == id))$song_name
    # test if the location appears in the lyric
    lyric_proof <- lyric_proof |>
      append(
        ifelse(
          (str_detect(lyric, location)),
          "Yes",
          ifelse(
            (str_detect(song_name, location)),
            "Yes",
            "No"
          )
        )
      )
  }
  # then cbind this test to the original df
  df <- df |>
    cbind(lyric_proof) |>
    # and remove any songs that don't feature
    filter(
      lyric_proof == "Yes"
    )
  df_name <- paste0("lyrics_", location)
  assign(df_name, df, envir = .GlobalEnv)
}

# df <- tibble("Test" = c(1, 2, 3), "Response" = c(1,2,3))
# binder <- c(1,2,3)
# df <- df |> cbind(binder)

# test <- (Camden |> filter(song_id == 6061308))$song_name
# test$song_name

# I don't want to spaff all my creds up the wall so here's a test run of the code that just uses one

location <- "Kilburn"
  # first, create a df of all the results for the location
  df <- search_song(search_term = location, 
                    n_results = 149, 
                    access_token = my_token)
  # then, create a list of all the IDs of these songs
  ids <- df$song_id
  # then, another for loop:
  lyric_proof = c()
  for(id in ids) {
    lyric_output <- get_lyrics_id(
      id, 
      access_token = my_token
    )
    # create one string that is all the lyrics
    lyric <- paste(lyric_output$line, collapse = " ")
    song_name <- (df |> filter(song_id == id))$song_name
    # test if the location appears in the lyric
    lyric_proof <- lyric_proof |>
      append(
        ifelse(
          (str_detect(lyric, location)),
          "Yes",
          ifelse(
            (str_detect(song_name, location)),
            "Yes",
            "No"
          )
        )
      )
  }
  # then cbind this test to the original df
  df <- df |>
    cbind(lyric_proof) |>
    # and remove any songs that don't feature
    filter(
      lyric_proof == "Yes"
    )
  df_name <- paste0("lyrics_", location)
  assign(df_name, df, envir = .GlobalEnv)
