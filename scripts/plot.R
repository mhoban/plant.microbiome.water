library(tidyverse)
library(lubridate)
###################
###################
# plot the data
logger_data <- read_csv(here::here("hobo loggers","output","logger_data.csv"), locale=locale(tz="HST"))

# I did all the same renaming you did so your plotting code would work
logger_data <- logger_data %>% 
  mutate(Date = date(datetime)) %>%
  rename(
    timestamp = datetime,
    bot1.Temp = temp_logger_20940476,
    bot1.Light = light_logger_20940476,
    top2.Temp = temp_logger_20940477,
    top2.Light = light_logger_20940477,
    bot3.Temp = temp_logger_20940478,
    bot3.Light = light_logger_20940478,
    bot2.Temp = temp_logger_20940479,
    bot2.Light = light_logger_20940479,
    top1.Temp = temp_logger_20940480,
    top1.Light = light_logger_20940480,
    top3.Temp = temp_logger_20940481,
    top3.Light = light_logger_20940481
  ) %>%
  select(`Date`,`timestamp`,top1.Temp,bot1.Temp,top2.Temp,,bot2.Temp,top3.Temp,bot3.Temp)


# plot it
par(mfrow=c(1,3))

####chamber 1
plot(top1.Temp~ Date, logger_data, type="n", ylab="Temperature (°C)", ylim=c(15, 35), yaxt="n", xaxt="n", 
     xlab="Date", cex.lab=0.7, cex.axis=0.7, main="chamber 1")
#top-chamber1
lines(logger_data$top1.Temp~logger_data$Date, lwd=.6, lty=1, col="dodgerblue")
#bot-logger_data
lines(logger_data$bot1.Temp~logger_data$Date, lwd=.6, lty=1, col="mediumseagreen")
axis(side=2, at=c(0, 10, 20, 30), cex.lab=0.7, cex.axis=0.7)
legend("topright", legend=c("top", "bottom"),
       col=c("dodgerblue", "mediumseagreen"), cex=0.6, lty =1, box.lty=0, lwd=2)
axis.Date(1, at=seq(min(logger_data$Date), max(logger_data$Date), by="2 month"), format="%b '%y", cex.lab=0.7, cex.axis=0.7)

####chamber 2
plot(top2.Temp~ Date, logger_data, type="n", ylab="", ylim=c(15, 35), yaxt="n", xaxt="n", 
     xlab="Date", cex.lab=0.7, cex.axis=0.7, main="chamber 2")
#top-chamber2
lines(logger_data$top2.Temp~logger_data$Date, lwd=.6, lty=1, col="dodgerblue")
#bot-chamber2
lines(logger_data$bot2.Temp~logger_data$Date, lwd=.6, lty=1, col="mediumseagreen")
axis(side=2, at=c(0, 10, 20, 30), cex.lab=0.7, cex.axis=0.7)
axis.Date(1, at=seq(min(logger_data$Date), max(logger_data$Date), by="2 month"), format="%b '%y", cex.lab=0.7, cex.axis=0.7)

####chamber 3
plot(top3.Temp~ Date, logger_data, type="n", ylab="", ylim=c(15, 35), yaxt="n", xaxt="n", 
     xlab="Date", cex.lab=0.7, cex.axis=0.7, main="chamber 3")
#top-chamber3
lines(logger_data$top3.Temp~logger_data$Date, lwd=.6, lty=1, col="dodgerblue")
#bot-chamber3
lines(logger_data$bot3.Temp~logger_data$Date, lwd=.6, lty=1, col="mediumseagreen")
axis(side=2, at=c(0, 10, 20, 30), cex.lab=0.7, cex.axis=0.7)
axis.Date(1, at=seq(min(logger_data$Date), max(logger_data$Date), by="2 month"), format="%b '%y", cex.lab=0.7, cex.axis=0.7)

dev.copy(pdf, "hobo loggers/output/tempoutput.pdf", height=4, width=8)
dev.off()
