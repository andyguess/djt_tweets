library(ggplot2)

setwd("")
trump <- read.csv("djt_tweets.csv", stringsAsFactors = FALSE)

# Already run: #####
trump$android <- ifelse(grepl("Android", trump$source, fixed = TRUE), 1, 0)
trump$iphone <- ifelse(grepl("iPhone", trump$source, fixed = TRUE), 1, 0)
trump$web <- ifelse(grepl("Web", trump$source, fixed = TRUE), 1, 0)
trump$time <- strftime(trump$datetime, format="%H:%M")
####################

head(trump)

agg <- with(trump, aggregate(android, by=list(time=time), FUN=sum))
agg2 <- with(trump, aggregate(iphone, by=list(time=time), FUN=sum))
agg3 <- with(trump, aggregate(web, by=list(time=time), FUN=sum))
agg$Device <- "Android"
agg2$Device <- "iPhone"
agg3$Device <- "Web"
agg <- rbind(agg, agg2, agg3)
agg$time <- as.POSIXct(agg$time, format = "%H:%M")

head(agg)

p <- ggplot(agg, aes(x=time, y=x, color=Device))
pq <- p + geom_point(alpha = 0.5) + geom_smooth(se = FALSE) + theme_bw() + 
  scale_x_datetime(name = "Tweets from Nov. 11, 2015 to Aug. 7, 2016", 
                   breaks = date_breaks("2 hour"), labels = date_format("%H:%M", tz="America/New_York")) +
  scale_y_continuous("Average # of tweets each minute") + 
  ggtitle("@realDonaldTrump: iPhone vs. Android") 

pqnew <- pq + theme(panel.grid.major = element_line(colour = "gray92"), 
                    panel.grid.minor = element_line(colour = "gray92"), 
                    panel.background = element_rect(fill = NA), 
                    plot.background = element_rect(colour = NA), 
                    plot.title = element_text(hjust = .5),
                    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
                    legend.key = element_rect(colour = NA),
                    legend.background = element_rect(fill = NA), 
                    legend.direction = "horizontal", legend.position = "bottom")
pqnew


# ggsave(pqnew, file="trump_twitter_phones.png", height=7, width=8)

