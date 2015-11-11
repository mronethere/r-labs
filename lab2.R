library(ggplot2)
stats <- read.csv("./data/lab2.csv")

vecIn <- as.numeric(stats$one_in)
vecOut <- as.numeric(stats$one_out)

cat("in: ", sum(vecIn))
cat("out: ", sum(vecOut))

step <- function(vec) {
  return(round(seq(min(vec), max(vec), by = 1),1))
}

p1 <- ggplot(stats, aes(x=id, y=one_in)) + ggtitle("На вхід") + 
  scale_x_continuous(breaks = step(stats$id)) + 
  scale_y_continuous(breaks = step(stats$one_in)) + 
  geom_bar(stat="identity") + ylab("Кількість") + xlab("Номер зупинки")
p2 <- ggplot(stats, aes(x=id, y=one_out)) + ggtitle("На вихід") +
  scale_x_continuous(breaks = step(stats$id)) + 
  scale_y_continuous(breaks = step(stats$one_out)) + 
  geom_bar(stat="identity") + ylab("Кількість") + xlab("Номер зупинки")

multiplot(p1, p2, cols=1)