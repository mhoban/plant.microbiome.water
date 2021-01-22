library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)
###################
###################
# plot the data
logger_data <- read_csv(here::here("hobo loggers","output","logger_data.csv"), locale=locale(tz="HST"))

# I did all the same renaming you did so your plotting code would work
logger_data <- logger_data %>% 
  mutate(Date = date(datetime)) %>%
  # all of this is commented out because the data should already be named
  # what you want it to be named. 
  # rename(
  #   timestamp = datetime,
  #   bot1.Temp = temp_logger_20940476,
  #   bot1.Light = light_logger_20940476,
  #   top2.Temp = temp_logger_20940477,
  #   top2.Light = light_logger_20940477,
  #   bot3.Temp = temp_logger_20940478,
  #   bot3.Light = light_logger_20940478,
  #   bot2.Temp = temp_logger_20940479,
  #   bot2.Light = light_logger_20940479,
  #   top1.Temp = temp_logger_20940480,
  #   top1.Light = light_logger_20940480,
  #   top3.Temp = temp_logger_20940481,
  #   top3.Light = light_logger_20940481
  # ) %>%
  # we still do this though:
  select(Date,timestamp=datetime,top1.Temp,bot1.Temp,top2.Temp,,bot2.Temp,top3.Temp,bot3.Temp)
  # rstudio shows a little yellow warning triangle in the gutter over to the left
  # because Date is a function in lubridate and it thinks you're trying to do something
  # with that rather than using it as the name of a field in a dataframe


# ok I went a head and ggplotified this. It's all standard ggplot code
# except for the last bit (p1 + p2 + p3). That uses patchwork, which allows
# for very clever and simple plot layout

# plot it

# chamber 1
p1 <- ggplot(logger_data) + 
  geom_line(aes(x = Date, y = top1.Temp),col="dodgerblue") +
  geom_line(aes(x = Date, y = bot1.Temp), col = "mediumseagreen") +
  xlab("Date") + 
  ylab("Temperature (ªC)") +
  ggtitle("Chamber 1")

####chamber 2
p2 <- ggplot(logger_data) + 
  geom_line(aes(x = Date, y = top2.Temp),col="dodgerblue") +
  geom_line(aes(x = Date, y = bot2.Temp), col = "mediumseagreen") +
  xlab("Date") + 
  ylab("Temperature (ªC)") +
  ggtitle("Chamber 2")

####chamber 3
p3 <- ggplot(logger_data) + 
  geom_line(aes(x = Date, y = top3.Temp),col="dodgerblue") +
  geom_line(aes(x = Date, y = bot3.Temp), col = "mediumseagreen") +
  xlab("Date") + 
  ylab("Temperature (ªC)") +
  ggtitle("Chamber 3")

# put 'em all together
# this uses the magic of patchwork to layout the plots
# this will put them all side by side, like you had it
p1 + p2 + p3

# just to show off how great and easy patchwork is, this will
# put chambers 1 & 2 on top, with chamber 3 on the bottom
(p1 + p2) / p3

# here are some other things you can do with it:
(p1 / p2) | p3
p1 | (p2 / p3)
p1 / p2 / p3
p1 / (p2 + p3)

# magic!