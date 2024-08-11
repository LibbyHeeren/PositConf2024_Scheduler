# PositConf2024 Google Calendar Events


HEYYYY! This is the code I used to create the .csv file, which was used
to create the [.ics file which you can download and add to your own
Google Calendar
here](https://datahumans.notion.site/Posit-Conf-2024-Google-Calendar-0af73e68ee94448bb901a937e3e54acc?pvs=4).

Here’s what my resulting calendar looks like once I’ve color coded the
events. So satisfying!  
[![](images/cal_example_image.png)](https://datahumans.notion.site/Posit-Conf-2024-Google-Calendar-0af73e68ee94448bb901a937e3e54acc?pvs=4)

It’s a partly manual process of harvesting json objects from the
conference schedule page, sifting through that messy json, and then
building out a csv file. Once I had the csv of events imported into
Google Calendar, I realized that I’d left all the events in Pacific
time, and the csv format doesn’t include time zones, so I modified the
time zones manually and then exported a .ics file for folks to use. It
will have the events in the correct time zone no matter where you are.
Theoretically.

If you’re super curious, feel free to check out the code, and here’s a
[quick explainer video from 2023 (previous year) in lieu of an actual
ReadMe for the moment.](https://youtu.be/XzHaBQR4DB0)
