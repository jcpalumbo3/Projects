library(ggplot2)
library(scales)

data = read.csv("AAPL_mid_price.csv")

timeString = strptime(data$Time, "%Y/%m/%d %H:%M:%S")

p = ggplot(data[seq(1, nrow(data), by=500),], aes(timeString[seq(1, nrow(data), by=500)], 
                                                  data[seq(1, nrow(data), by=500),2])) + 
  geom_line(color = "coral") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Time of Day") + ylab("Price")+ ggtitle("Mid Price Movements from 9:30am to 12:30pm")
p

ggsave("./Mid_Price.pdf", plot = p)

# p = ggplot(data[seq(1, nrow(data), by=500),], aes(timeString[seq(1, nrow(data), by=500)], 
#                                                  data[seq(1, nrow(data), by=500),2])) + 
#   geom_line(color = "black") +
#   geom_rect(data = NULL, aes(xmin = strptime("2012/05/22 09:30:00.000", "%Y/%m/%d %H:%M:%S"), 
#                              xmax = strptime("2012/05/22 10:30:00.000", "%Y/%m/%d %H:%M:%S"),
#                              ymin = -Inf, ymax = Inf, fill = "training"), alpha = 0.1) +
#   geom_rect(data = NULL, aes(xmin = strptime("2012/05/22 10:30:00.000", "%Y/%m/%d %H:%M:%S"), 
#                              xmax = strptime("2012/05/22 11:00:00.000", "%Y/%m/%d %H:%M:%S"),
#                              ymin = -Inf, ymax = Inf, fill = "validation"), alpha = 0.5) +
#   geom_rect(data = NULL, aes(xmin = strptime("2012/05/22 11:00:00.000", "%Y/%m/%d %H:%M:%S"), 
#                              xmax = strptime("2012/05/22 12:00:00.000", "%Y/%m/%d %H:%M:%S"),
#                              ymin = -Inf, ymax = Inf, fill = "test"), alpha = 0.1) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   xlab("Time of Day") + ylab("Price")+ ggtitle("Mid Price Movements from 9:30am to 12:30pm")
# p

