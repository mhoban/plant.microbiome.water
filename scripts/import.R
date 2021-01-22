# temp loggers script

library(fs)
library(here)
library(lubridate)
library(tidyverse)

# the here library takes advantage of the fact that 
# it knows where your project is located on your hard drive
# that was you can avoid having to call setwd()

# enumerate all your CSV files recursively
# this picks up anything that ends with .csv
# and is somewhere under the data directory
# so it'll work no matter how many csv file you have
csv_files <- dir_ls(here::here("data"),glob='*.csv',recurse=TRUE)

# map_dfr will call the function for each element of csv files
# it expects a dataframe to be returned, and all the dataframes
# returned from each time the function is called are bound rowwise
# into one big dataframe
loggers <- map_dfr(csv_files,function(csv) {
  # first let's pull out the logger ID from the filename
  # these path_* files come from the library fs
  logger_id <- path_ext_remove(path_file(csv))
  if (str_detect(logger_id,"^([^_]+)_")) {
    # using a regular expression, we pull out the first set of characters
    # that aren't underscores
    logger_id <- str_match(logger_id,"^([^_]+)_")[1,2]
  } else {
    # if the filename doesn't look like xxx_xxx.csv, bail out 
    # because we can't do anything with it
    return(NULL)
  }
  
  # I call suppressWarnings here because there is some variability 
  # in the number of columns in your CSV files. The good thing is 
  # that we only care about the first 4 columns, and those are the 
  # same in all the files. Without suppressWarnings, R will scream at
  # us about a bunch of 'parsing errors', but we don't actually care.
  suppressWarnings(
    # load the csv file, starting with the third line
    # name the columns num, datetime, temp, and light
    # make sure the reader parses the columns as:
    # numeric, character, numeric, numeric
    read_csv(csv,
             skip = 2,
             col_names = c("num","datetime","temp","light"),
             col_types = cols_only(
               num = "n",
               datetime = "c",
               temp = "n",
               light = "n"
             )) %>%
      drop_na(datetime,temp,light) %>% # make sure we only have lines with complete data
      mutate(across(where(is.character),str_trim)) %>% # trim whitespace from character fields
      mutate(
        logger = factor(str_c("logger_",logger_id)), # create a factor column with the logger ID
        datetime = parse_date_time( # make sure our datetime field is parsed correectly
          datetime,
          orders = c("m/d/y I:M:S p","m/d/y H:M:S"), 
          # the previous line specifies the formats we expect to see
          # looking at the CSVs, there appear to be two datetime formats
          # the first is mm/dd/yy hh:mm:ss (am/pm), with a 12-hour hour field
          # the second is mm/dd/yy hh:mm:ss, with a 24-hour hour field
          # the two format strings in orders should take care of that
          tz="HST"
          # since POSIXct values are represented internally in UTC, 
          # we make sure the parser knows these datetimes are in Hawai‘i local time
          # by setting tz="HST"
        )
      ) %>%
      select(logger,datetime:light) # only get the fields we care about
  )
})

# now we cast the concatenated loggers to wide format
# you'll get temp and light for each logger, and they'll be merged
# by the datetime field and sorted chronologically
loggers_wide <- loggers %>%
  pivot_wider(names_from="logger",values_from=c("temp","light")) %>%
  arrange(datetime)

# save the wide format data as a csv
# the datetime is saved in ISO8601 format with UTC timezone
# so you'll have to deal with that when you load the file again
# here's a whole huge article on ISO8601: https://en.wikipedia.org/wiki/ISO_8601
write_csv(loggers_wide,here::here("hobo loggers","output","logger_data.csv"))

# here's how I would read it. There might be a simpler way but this is what I'd do:
# logger_data <- read_csv(here::here("hobo loggers","output","logger_data.csv")) %>%
#   mutate(datetime = with_tz(datetime,"HST"))

# oh, it looks like you can also do it like this:
# logger_data <- read_csv(here::here("hobo loggers","output","logger_data.csv"),locale=locale(tz="HST")) 

# both methods seem to do the same thing.
# try it without setting the timezone and you'll see how the displayed date changes
# it's probably ultimately safest to keep it in UTC though once it's there
# timezones are a nightmare and computers are very bad at them
