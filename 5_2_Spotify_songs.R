# run after 4_Filtering_results.R
# do not need to run 5_1_Spotify_artists once ran once

library(spotifyr)

### SPOTIFY API DETAILS
your_spotify_id <- your_spotify_id
your_spotify_secret <- your_spotify_secret
# https://www.rdocumentation.org/packages/spotifyr/versions/2.2.4

Sys.setenv(SPOTIFY_CLIENT_ID = your_spotify_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = your_spotify_secret)
access_token <- get_spotify_access_token()
artist_match_frame <- read.csv("artist_matches.csv")

# artist_name - for expulsion later...
artists_exclude <- c("H.G. Wells", "Sir Arthur Conan Doyle", "William Shakespeare", 
                     "Virginia Woolf", "Daniel Defoe", "Charles Dickens", 
                     "Oscar Wilde", "Martin Amis", "George Bernard Shaw", 
                     "D.H. Lawrence", "Ben Jonson", "William Hazlitt", 
                     "Arthur Edward Waite", "Jane Austen", "Dorothy L. Sayers", 
                     "Frances Burney", "David Hume", "George Eliot", 
                     "Ford Madox Ford", "Robert Louis Stevenson", "Henry James", 
                     "George Gissing", "Mary Elizabeth Braddon", "Jonathan Swift", 
                     "Henry Fielding", "Alan Moore", "William Cowper",
                     "William Makepeace Thackeray")

# bind that to the original dataset then get rid of all the bad eggs
all_boroughs_clean <- merge(all_boroughs, artist_match_frame, by.x = "artist_name", by.y = "check_list") |>
  filter(artist_matches == "yes") |>
  # this is causing some errors, so...
  mutate(
    "Fix Artist Length" = (nchar(artist_name)
    ) 
  ) |>
  mutate(
    "Fix Song Length" = nchar(song_name)
  ) |>
  filter(`Fix Song Length` < 150 & `Fix Artist Length` < 100) |>
  filter(!(artist_name %in% artists_exclude)) 

# get spotify IDs for songs

songs_and_artists <- select(all_boroughs_clean, c(artist_name, song_name, place21nm,song_lyrics_url))
rm(all_boroughs_clean, all_boroughs, artist_match_frame)

# need a new list that excludes the artists that aren't on spotify

check_list_2 <- unique(songs_and_artists$artist_name) 

# get a blank table to fill out

song_matches = tibble(artists_id = "w", artist_name = "x", song_name = "y", spotify_song_name = "Y", id = "z", place21nm = "Z")

# an extra df to help cut stuff down

check_list_3 <- head(check_list_2, 5)

# for each artist, find the songs that correspond to them in the song list.
for(artist in check_list_2){
  song_list_long <- filter(songs_and_artists, artist_name == artist)
  song_list <- song_list_long$song_name
  # then for each of those songs, get its spotify ID
  for(song in song_list){
    query <- search_spotify(paste0(song, " ", artist), type = "track", limit = 10) 
    if(nrow(query) == 0){break}
    query <- query |>
        # get just the ones by the artist
        unnest(cols = artists, names_sep = "_") |>
        filter(artists_name == artist)
    # don't check for exact names, but check that the first letter matches (this has worked for me so far)
        first_letter <- substr(song, 1, 1)
        first_letter_matches <- query |> mutate( "Match" =
          ifelse(str_starts(name, coll(first_letter)), "Yes", "No")
          ) |> filter(Match == "Yes")
        query <- query |> filter(name %in% first_letter_matches$name) |>
        # get just one (the most widely available one)
        mutate(
          "song_name_length" = nchar(name)
        ) |>
        arrange(desc(available_markets)) |>
        head(5) |>
          # get the one least likely to be a remix
        arrange(song_name_length) |>
        head(1) |>
        # get it to match the tibble
        select("spotify_song_name" = name, id, artists_id) |>
        mutate("song_name" = song)
    # add it to that tibble
        query <- left_join(query, song_list_long, by = "song_name")
    song_matches <- bind_rows(song_matches, query)
  }
  print(paste0(artist, " search done."))
}

# save that so we don't have to run it again

write.csv(song_matches, "_all_songs.csv")
rm(first_letter_matches, query, song_list_long, songs_and_artists)
