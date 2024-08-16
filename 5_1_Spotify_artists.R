# run after 4_Filtering_results.R

library(spotifyr)

### SPOTIFY API DETAILS
your_spotify_id <- your_spotify_id
your_spotify_secret <- your_spotify_secret
# https://www.rdocumentation.org/packages/spotifyr/versions/2.2.4

Sys.setenv(SPOTIFY_CLIENT_ID = your_spotify_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = your_spotify_secret)
access_token <- get_spotify_access_token()

# steps we need to take:

# for each artist name in all_boroughs, check there is an artist in spotify that corresponds to it.

check_list <- unique(all_boroughs$artist_name)

# ah, okay, we have a problem, virginia woolf is on spotify....
# but tottenham hotspur is not.

# there's a workaround and I'm going to figure it out!

# function to search for artists in spotify with a name matching the genius artist
artist_matches = c()
for(artist in check_list){
  query <- (search_spotify(artist, type = "artist", limit = 1))
  name <- query$name
  ifelse(name == artist, var <- "yes", var <- "no")
  artist_matches <- artist_matches |> append(var)
  print(paste0(artist, " search done."))
}

# bind that output to the checklist
artist_match_frame <- as.data.frame(cbind(check_list, artist_matches))

# writing this as it's quite API intensive
write.csv(artist_match_frame, "artist_matches.csv")

# sadly it kept austen, thackeray, etc... will need manually removing 
