# Load packages
library(tidyverse)

# This code is just if anyone's curious, I don't expect anyone to fine it useful 😂

# It turns the output csv into a google calendar compatible csv. 
# The csv import doesn't preserve/include time zones, 
# so only use this if you're in Pacific Time Zone.
# Just use the ics file I created: https://datahumans.notion.site/Posit-Conf-2024-Google-Calendar-0af73e68ee94448bb901a937e3e54acc?pvs=4

# Import the output csv containing the talks only (from get_talks.R)
talks_csv <- read_csv("output/PositConf2024_talks.csv")


# Now we need the columns in a format google will understand for our csv:

# Subject
# --(Required) The name of the event
# --Example: Final exam
# Start Date
# --(Required) The first day of the event
# --Example: 05/30/2020
# Start Time
# --The time the event begins
# --Example: 10:00 AM
# End Date
# --The last day of the event
# --Example: 05/30/2020
# End Time
# --The time the event ends
# --Example: 1:00 PM
# All Day Event
# --Whether the event is an all-day event. 
# --If it’s an all-day event, enter True. 
# --If it isn’t an all-day event, enter False.
# --Example: False
# Description
# --Description or notes about the event
# --Example: "A meeting to discuss ABC and XYZ."
# Location
# --The location for the event
# --Example: "Columbia, Schermerhorn 614"


# Most of our columns are in the right format, but we will want to combine
# speaker and abstract into one value if we want to add it as the description
# for the google calendar events.

all_events <- talks_csv

all_events$description <- paste0(
  "Speakers:\n",
  all_events$speaker,
  "\nAbstracts:\n",
  all_events$abstract
)


# It's super annoying that google calendar orders concurrent events alphabetically,
# meaning they end up in nonsensical order for each set of sesssions.

# So, I'm going to add room letters in front of each subject line for all Breakout
# Session types. That means I need to harvest the room letters from the room var.

# Grab all the "C" "A/B" and "D" values from rooms, and do "E" for Elwha.

all_events$room_letter <-
  case_when(
    all_events$room %in% c(
      "300 | Columbia C",
      "Columbia A/B",
      "300 | Columbia D"
    ) ~ sub(".*\\|\\s*|.*\\s", "", all_events$room),
    all_events$room == "Elwha" ~ "E",
    .default = ""
  )

# Create subject lines by putting the room letter in front, but skip this for
# the Lunch and Learns since they have no concurrent events

all_events$subject <-
  case_when(
    all_events$type == "Lightning" ~ paste0(
      all_events$room_letter,
      ": ",
      all_events$title
    ),
    all_events$type == "Keynote" ~ all_events$title,
    all_events$type == "Breakout Session" &
      all_events$room != "500 | Quinault" ~ paste0(
      all_events$room_letter,
      ": ",
      all_events$title,
      " - ",
      all_events$speaker
    ),
    .default = all_events$title
  )

# now we can drop the speaker and abstract columns, 
# as well as title, letter, and type.

all_events <-
  all_events |> 
  select(-c(speaker, abstract, title, type, room_letter))

# I don't think we *need* an end date, but just to be sure, let's add one that's 
# the duplicate of the start date (which is just called "date" right now).

all_events$end_date <- all_events$date

# Let's add an all day event column to match the formatting above

all_events$all_day_event <- "False"

# Let's rearrange the columns to be in the order of
# subject, start date, start time, end date, end time, all day event, 
# description, location

all_events <- all_events[c(6, 1, 2, 7, 3, 8, 5, 4)]

# Now, let's make each of our columns into what they need to be in our exported
# csv file, which means they need spaces in their column names

colnames(all_events) <- c("Subject", "Start Date", "Start Time", "End Date", "End Time",
                       "All Day Event", "Description", "Location")


# write the csv file!

write_csv(all_events, "output/PositConf2024_all_events_cal.csv",
          col_names = TRUE)

# DO NOT, I REPEAT, DO NOT GO OPEN THE CSV IN EXCEL, IT WILL RUIN IT IF YOU SAVE IT THERE
# See below for link to .ics file that you should really use instead of this csv.

# The resulting csv file can then be uploaded into google calendar, but it will
# load in YOUR CURRENT TIME ZONE, so if you're attending in person, load it when 
# your TZ is already set to pacific. 

# 1. Go create a new calendar called Posit Conf 2024
# 2. Click "Create calendar" and then 
# 3. In the menu on the left, click Import & Export.
# 4. Click "Select file from your computer" and select the csv file we made.
# 5. Under "Add to calendar" choose the Posit Conf 2024 calendar you just created
# 6. Click Import.

# But you don't actually have to use any of this code.
# I've created a .ics for folks attending anywhere in the world, which will load
# with events in the correct time zone: https://datahumans.notion.site/Posit-Conf-2024-Google-Calendar-0af73e68ee94448bb901a937e3e54acc?pvs=4