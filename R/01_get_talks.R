# Load packages
library(tidyverse)
library(rjson)

# Call the raw json object I saved from the posit api response tab
# containing only Keynotes, Breakout Sessions, and Lightning
day1_raw_json_schedule <- fromJSON(file = "data/2024-08-10_1253pm_PositConf2024_Schedule_Talks_Day1.json")
day2_raw_json_schedule <- fromJSON(file = "data/2024-08-10_1253pm_PositConf2024_Schedule_Talks_Day2.json")

# combine the two json objects into one
raw_schedule <- c(day1_raw_json_schedule, day2_raw_json_schedule)

# Create data frames from the json
schedule <- tibble(section = raw_schedule) |> 
unnest_wider(section) |> 
  as.data.frame()

# Create a function that, given the schedule, section number, and item number, 
# will go get all the stuff I want

get_talk_data <- function(raw_schedule, sectionNum, itemNum) {
  
  raw_json_schedule <- raw_schedule
  # Get type
  talk_type <- raw_json_schedule |> 
    purrr::pluck(sectionNum, "items", itemNum, "type") 
  # Get title
  talk_title <- raw_json_schedule |> 
    pluck(sectionNum, "items", itemNum, "title") 
  # Get date
  talk_date <- raw_json_schedule |> 
    pluck(sectionNum, "items", itemNum, "times", 1, "daySort") |>
    lubridate::ymd() |>
    format("%m/%d/%Y")
  # Get start time
  talk_start_time <- raw_json_schedule |> 
    pluck(sectionNum, "items", itemNum, "times", 1, "startTimeFormatted")
  # Get end time
  talk_end_time <- raw_json_schedule |> 
    pluck(sectionNum, "items", itemNum, "times", 1, "endTimeFormatted")
  # Get room/location
  talk_location <- raw_json_schedule |> 
    pluck(sectionNum, "items", itemNum, "times", 1, "room")
  # Get abstract, but drop all the html tags that get picked up
  talk_abstract <- gsub("<.*?>", "", raw_json_schedule |> 
                          pluck(sectionNum, "items", itemNum, "abstract"))
  
  # Check for multiple speakers!
  all_speakers <- map_chr(raw_schedule |> pluck(sectionNum, "items", itemNum, "participants"), ~ .x$fullName)
  num_speakers <- length(all_speakers)
  
  # Join speaker names based on the number of speakers
  talk_speaker <- switch(
    as.character(num_speakers),
    "1" = all_speakers[1],
    "2" = paste(all_speakers, collapse = " & "),
    # For 3 or more speakers:
    paste0(
      paste(all_speakers[1:(num_speakers - 1)], collapse = ", "),
      ", & ",
      all_speakers[num_speakers]
    )
  )
  
  
  # output a row of info as a data frame, which we will rbind to an existing df
  data.frame(type = talk_type, 
             title = talk_title, 
             date = talk_date,
             start_time = talk_start_time,
             end_time = talk_end_time,
             room = talk_location,
             speaker = talk_speaker,
             abstract = talk_abstract)
}

# create an empty data frame to fill with talk info

talk_df <- data.frame(matrix(ncol = 8, nrow = 0))


# loop through the talks and grab stuff!

# for each section of the json item (each row of the data.frame)
for (i in 1:nrow(schedule)) {
  # for each talk/session in the section
  for (j in 1:schedule[i, "numItems"]) {
    # add a row to the df with the talk details
    talk_df <- rbind(talk_df, get_talk_data(raw_schedule, i, j))
  }
}


# Save the data frame of talk details as a csv file
write_csv(talk_df, "output/PositConf2024_talks.csv")
