# example of missing dat ahandling
dfr <- data.frame(
  x = 1:10,
  y = runif(10)
)
dfr[c(3, 6, 7), "y"] <- NA
library(pracma)
dfr$z <- with(dfr, interp1(x, y, x, "linear"))

ggplot(dfr, aes(x, y)) + geom_line()
ggplot(dfr, aes(x, z)) + geom_line()
ggplot(dfr, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_line(aes(y = y)) +
  geom_line(aes(y = z), linetype = "dotted")
