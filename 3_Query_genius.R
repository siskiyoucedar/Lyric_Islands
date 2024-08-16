library(tidyverse)
library(rvest)
library(xml2)
library(rlang)
# run after 1_placenames.R

borough_name <- "Boroughs" # your borough name goes here - then you can run all the code

## PULL LYRICS

# querying placenames in the API from genius.com

# ID:
# 82pQcD_kh-FLJNLtC_lqQ1zdRt79U6akDH5-qeTvOwRoM10hOyQCzqhe-yewRJtu
# key:
# 4UqWZEShwKF4pqNdc7db9cU2E0YKH92VcwBpTIwtbQAu6qz88lvWUl1xku-9RuuptTsfvkOODq_CHLLik9cwlA
# access token:
# 4BmAaA0il4sQauwJKTnY4-oBh_bpPxtgDJQ2bwQOvhJCPcaawAeIH6aWB_oTGlQ5

## use the GeniusR package to root around.

library(geniusr)
my_token <- "4BmAaA0il4sQauwJKTnY4-oBh_bpPxtgDJQ2bwQOvhJCPcaawAeIH6aWB_oTGlQ5"

# this works as a workaround, but it removes artist details (artist details is what causes the error)

get_lyrics <- function (session) {
  lyrics <-  session %>% html_nodes(xpath = '//div[contains(@class, "Lyrics__Container")]')
  song <-  session %>% html_nodes(xpath = '//span[contains(@class, "SongHeaderdesktop__")]') %>% html_text(trim = TRUE)
  xml_find_all(lyrics, ".//br") %>% xml_add_sibling("p", "\n")
  xml_find_all(lyrics, ".//br") %>% xml_remove()
  lyrics <- html_text(lyrics, trim = TRUE)
  lyrics <- unlist(strsplit(lyrics, split = "\n"))
  lyrics <- grep(pattern = "[[:alnum:]]", lyrics, value = TRUE)
  if (is_empty(lyrics)) {
    return(tibble(line = NA, 
                  song_name = song))
  }
  tibble(line = lyrics, song_name = song)
}
assignInNamespace("get_lyrics", get_lyrics, "geniusr")

## error tests

# test <- search_song(search_term = location, 
#                     n_results = 149, 
#                     access_token = my_token)
# get_lyrics(3061514)
# get_lyrics_id(3061514, 
#               access_token = my_token)

## THE RUN

# for each song returned for each location, 
# check songname for geo indicator.
# if not - query *again* to get full lyrics.
# if - in those lyrics - we find the geographic indicator, we say yes! move on
# if not - cull that result.
# else I think a max of 149 is sound. anything over seems to generate errors

# need to define location

# keep things simple for now - just one borough

# change the R timeout so we can get through the whole thing

options(timeout = max(1000, getOption("timeout")))

# grab the borough's df

borough_df <- get(paste0("df_",borough_name))

# postcodes work below

# postcodes_lite <- postcodes |> filter(
#   !(Name %in% c("WC1A",  "WC1B",  "WC1E",  "WC1H",  "WC1N",  "WC1R",  "WC1V",  "WC1X", "WC2A", "WC2B", "WC2E", "WC2H", "WC2N", "WC2R",
#                 "EC1A",  "EC1M",  "EC1N",  "EC1P",  "EC1R",  "EC1V",  "EC1Y", "EC2A",  "EC2M",  "EC2N",  "EC2P",  "EC2R",  "EC2V",  "EC2Y",
#                 "EC3A",  "EC3M",  "EC3N",  "EC3P",  "EC3R",  "EC3V", "EC4A",  "EC4M",  "EC4N",  "EC4P",  "EC4R",  "EC4V",  "EC4Y", 
#                 "SW1A", "SW1Y", "SW1X", "SW1W", "SW1V", "SW95",
#                 "W1U", "W1W", "W1T", "W1S", "W1F", "W1H", "W1G", "W1B", "W1A", "W1D", "W1C", "W1J", "W1K", "NW1W", "N1P"))
# )
# 
# locations <- postcodes_lite$Name |> append(c("WC1", "EC1", "SW1", "W1"))

locations <- borough_df$word # change df per borough

#locations <- London_PN_sf$lad21nm |> unique() # change df per borough

# below line is used when there is an error and we have to start from a later point

# locations2 <-tail(locations, -19)

tot_requests <- 0

for (location in locations#2
     ) {
  # first, create a df of all the results for the location (request #1)
  df <- search_song(search_term = location, 
                    n_results = 149, 
                    access_token = my_token)
  requests <- 1
  # then, create a list of all the IDs of these songs
  ids <- df$song_id
  # then, another for loop:
  location_proof = c()
  for(id in ids) {
    # first, create an object that is the song name associated with a given ID
    song_name <- (df |> filter(song_id == id))$song_name
    ifelse(
      # test if the given location appears in the song name
      (str_detect(song_name, location)),
      # if it does, say Yes
      var <- "Yes",
      ifelse(
        (str_detect(
          # create one string that is all the lyrics (request #2)
          (paste((
            get_lyrics_id(
              id, 
              access_token = my_token
            )
          )$line,
          collapse = " ")
          )
          # check for the location in that string
          , location)),
        # if it appears, say we did a request and got a return
        var <- "Request",
        # if it doesn't appear, say we didn't get a return
        var <- "No"
      )
    )

    # change the request no. if lyrics were requested
    ifelse((var == "Yes"), var <- var, requests <- (requests + 1))

    # make the request cat match the yes cat
    ifelse((var == "Request"), var <- "Yes", var <- var) 
    
    # append that song's Y/N to the location proof list
    location_proof <- location_proof |>
      append(var)
  }
  
  # then cbind the location proof Y/N to the original df
  df <- df |>
    cbind(location_proof) |>
    # and remove any songs that don't feature, giving a complete list of all soncgs featuring the location
    filter(
      location_proof == "Yes"
    )
  # spit out a df per location with all relevant songs
  df_name <- paste0("lyrics_", location)
  assign(df_name, df, envir = .GlobalEnv)
  # get a measure of how much this is p!ssing off the top brass
  print(paste0(
    location, " search done."
  )
  )
  print(paste0(
    "This operation used ", requests, " Genius API requests."
  )
  )
  Sys.sleep(0.1)
  flush.console() 
  tot_requests <- tot_requests + requests
}
rm(borough_df)

# depending on how far we get...

# this code collects all the dfs into one dataframe

dfs <- ls() |>
  as.data.frame() |> 
  rename(
  "object" = `ls()`
  ) |> mutate(
  value = str_detect(object, "lyrics_")
  )|>
    filter(
      value == "TRUE"
    ) |>
    select(
      object
    ) |>
  pull()

df_list <- mget(dfs)

# Convert all objects to dataframes (if they aren't already)
df_list <- lapply(df_list, as.data.frame)

# Filter out empty dataframes
df_list <- df_list[sapply(df_list, function(x) nrow(x) > 0)]

# Bind all non-empty dataframes together
borough_data <- bind_rows(df_list, .id = "place21nm")

# Fix ids (remove lyrics_ in the id column)

borough_data <- borough_data |> mutate(
  place21nm = gsub("lyrics_", "", place21nm)
)

# get the name for the csv

borough_name <- paste0(borough_name, "_songs.csv") 

# write a .csv for that borough

write.csv(borough_data, borough_name)

# clear the env so you can go again with a borough of your choice
rm(list = ls()[grepl("lyrics_", ls())])
