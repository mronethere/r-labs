# Підключення бібліотеки для побудови графіків
library(ggplot2)

# Підготовка даних для подальшого опрацювання
stats <- read.csv("./data/lab1.csv")
counts <- as.numeric(stats$count)
temp <- table(as.vector(counts))

# Обчислення статистичних даних
avrg <- mean(counts)
mode <- names(temp)[temp == max(temp)]
mdn <- median(counts)
dsp <- var(counts)
cat(" Середнє значення:", avrg, "\n",
    "Мода:", mode, "\n",
    "Медіана:", mdn, "\n",
    "Дисперсія:", dsp)

# Побудова автокореляційної функції
acz <- acf(counts, plot=F)
ggplot(data.frame(lag=acz$lag, acf=acz$acf), aes(lag, acf)) + 
  geom_line() + geom_point() + theme_bw() + geom_hline(aes(yintercept=0))

# Допоміжня функція яка генерує крок для заданого вектора
step <- function(vec, by0) { return(round(seq(min(vec), max(vec), by = by0),1))}

# Побудова графіку к-сть навч. закладів станом на заданий проміжок часу
ggplot(data=stats, aes(x=year, y=count)) + geom_line() + geom_point() +
  scale_x_continuous(breaks = step(stats$year, 2)) + 
  scale_y_continuous(breaks = step(stats$count, 1)) + 
  ylab("Кількість") + xlab("Рік")
