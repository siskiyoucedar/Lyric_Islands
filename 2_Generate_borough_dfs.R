## BOROUGH DFs of PLACE NAMES

#expect many objects in environment!

# load in an entire dictionary with no proper nouns

library(words)

# make sure the words are capitalised, like in the placenames

words_title <- words |> select(
  word
) |> 
  mutate(
    word = (
      str_to_title(
        word
      )
    )
  )

# borough by borough - summarise, filter...

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
      "lad21nm" = NM,
      "word" = place21nm
    ) |>
    filter(
      !(word %in% words_title$word)
    ) |>
    select(
      word
    ) |> 
    
    # clear all commas and brackets (no chance these words will appear)
    
    # do not clear apostrophes but add an additional row for the same thing, with no apostrophes
    # do not clear periods but add an additional row for the same thing, with no periods
    # thereby giving us the full range:
    # st. james's park (no alterations)
    # st. jamess park (no apostrophe)
    # st james's park (no period)
    # st jamess park (no neither) - haven't added capacity for this, how?
    
  
  mutate(
    "value" = str_detect(word, "\\("),
    "value2" = str_detect(word, ","),
    "value3" = str_detect(word, "\\'"),
    "value4" = str_detect(word, "\\.")
  ) |>
    filter(
      value == "FALSE"
    ) |>
    filter(
      value2 == "FALSE"
    )
  
  df_apos <- df |> 
    filter(
      value3 == "TRUE"
    ) |> 
    mutate(
      word = gsub("\\'", "", word)
    )
  
  df_period <- df |> 
    filter(
      value4 == "TRUE"
    ) |> 
    mutate(
      word = gsub("\\.", "", word)
    ) 
  
  df_both <- df |>
    filter(
      value3 == "TRUE" & value4 =="TRUE"
    ) |> 
    mutate(
      word = gsub("\\.", "", word)
    ) |> 
    mutate(
      word = gsub("\\'", "", word)
    )
  
  # bind apostrophes to non apostrophes
  
  df <- rbind(df, df_apos)
  df <- rbind(df, df_period)
  df <- rbind(df, df_both)
  
  df <- df |> select(
    word
  ) |>
    unique() |>
    arrange(word)
  df_name <- paste0("df_", NM)
  assign(df_name, df, envir = .GlobalEnv)
}
rm(df, df_apos, df_period, df_both)
