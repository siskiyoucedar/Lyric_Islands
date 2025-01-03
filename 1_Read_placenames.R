library(tidyverse)
library(tmap)
library(sf)

### importing placenames

## What we want names to look like:
# Clapham Junction -> Clapham Junction
# Merton -> Merton
# Abbey -> NA
# Barking -> Barking (problem - this is a noun but also an accurate place indicator. May need individual assessment)
# St. Ann's / St Anns / St. Ives / St Ives -> St Anns / St Ives (i.e. remove period, remove apostrophes)
# A' Ní Aireann -> A' Ní Aireann (do not remove apostrophes here)

# the solution to the above was to simply have two queries - one with no apostrophes, and one with all.

## Placename dataset

# This is a nice OS dataset, available here:
# https://geoportal.statistics.gov.uk/datasets/8f8b561f256b40c3a6df71e400bb54f0/about 

OS_placenames <- read.csv("_Census_data/IPN_GB_2022.csv")

# Postcode dataset - available here:
# https://www.doogal.co.uk/london_postcodes
# postcodes <- st_read("_Spatial_data/postcodes.kml") # don't need for now

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

# seems like borough names are massively overstated.
# let's kick em out!

kickable_names = c()
vars = c(London_LADs$LAD23NM)

for (NM in vars) {
  kickable_names <- kickable_names |>
    append(NM) |>
    # as well as these "unparished area" names that mean nothing
    append(
      paste0(
        NM, ", unparished area"
      )
    )
}

London_placenames <- London_placenames |>
  filter(!(place21nm %in% kickable_names))
rm(kickable_names)

# it works!

# merge back in

df_all <- tibble("place21nm" = c(), "lat" = c(), "long" = c(), "lad21nm" = c())

for (NM in vars) {
  df <- London_placenames |>
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
rm(df, NM)

# make a spatial object

London_PN_sf <- df_all |>
  st_as_sf(coords = c("long", "lat"))

rm(df_all, London_placenames)

st_write(London_PN_sf, "_Processed_data/_London_Placenames_sf.gpkg")

# tmap_mode("view")
# 
# tm_shape(London_LADs) +
#   tm_fill(col = "maroon", alpha = 0.3) +
#   tm_basemap(c(StreetMap = "OpenStreetMap", TopoMap = "OpenTopoMap")) +
#   tm_shape(London_PN_sf) +
#   tm_symbols(size = 0.05, col = "black")

# still some minor botheration with e.g. "St. Ann's" vs "St Ann's" but otherwise good

# ^ note for later - when processing the output data, best remove all the apostrophes, periods etc. so it can be merged back in, 
# with another summarise to get the right lat_long

# this would need to be done when you have the individual dfs but before you put it through to the api
# then any shared placenames across boroughs would need to be eked out from the London_PN_sf object

# so... 1633 variables. that's doable, right?
