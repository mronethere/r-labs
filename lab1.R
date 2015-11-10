library(ggplot2)
stats <- read.csv("./data/lab1.csv")
counts <- as.numeric(stats$count)
temp <- table(as.vector(counts))

avrg <- mean(counts)
mode <- names(temp)[temp == max(temp)]
mdn <- median(counts)
dsp <- var(counts)

cat(" Середнє значення:", avrg, "\n",
    "Мода:", mode, "\n",
    "Медіана:", mdn, "\n",
    "Дисперсія:", dsp)

acz <- acf(counts, plot=F)
ggplot(data.frame(lag=acz$lag, acf=acz$acf), aes(lag, acf)) + 
  geom_line() + geom_point() + theme_bw() + geom_hline(aes(yintercept=0))

ggplot(data=stats, aes(x=year, y=count)) + geom_line() + geom_point() +
  ylab("Кількість") + xlab("Рік")
