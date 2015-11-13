# Підключення бібліотеки для побудови графіків
library(ggplot2) 

# На сонові даних про вхід/вихід людей з маршрутки рахує поточну к-сть людей
countPeople <- function(ins, outs) {
 arr <- seq(length(ins))
 counter <- 0
 for (i in 1:length(ins)) {
   counter = counter + ins[i] - outs[i]
   arr[i] = abs(counter)
 }
 return(arr)
}

# Перетворює список з відстаней між зупинками до списку з наростаючими відстаннями від початку до кожної зупинки
sumPath <- function(arr) {
	acc <- arr
	for (i in 2:length(acc)) { 
		acc[i] = acc[i] + acc[i - 1]
	}
	return(acc);
}

# Допоміжня функція яка генерує крок для заданого вектора
step <- function(vec) { return(round(seq(min(vec), max(vec), by = 1),1))}

# Функція для побудови графіків відношення к-сті входу/виходу людей з маршрутки до порядкового номера зупинки
plotInOut <- function(stats) {
	p1 <- ggplot(stats, aes(x=id, y=ins)) + ggtitle("Кількість людей які заходять в маршрутку") + 
	  scale_x_continuous(breaks = step(stats$id)) + 
	  scale_y_continuous(breaks = step(stats$ins)) + 
	  geom_bar(stat="identity", fill="#FF9999", colour="black") + ylab("Кількість") + xlab("Номер зупинки")
	p2 <- ggplot(stats, aes(x=id, y=outs)) + ggtitle("Кількість людей які виходять з маршрутки") + 
	  scale_x_continuous(breaks = step(stats$id)) + 
	  scale_y_continuous(breaks = step(stats$outs)) + 
	  geom_bar(stat="identity", fill="#FF9999", colour="black") + ylab("Кількість") + xlab("Номер зупинки")
	multiplot(p1, p2, cols = 1)
}

# Функція для побудови графіку відношення поточної к-сті людей в маршрутці до порядкового номера зупинки
plotStats <- function(stats) {
	newStats <- data.frame(id=stats$id, counted=countPeople(stats$ins, stats$outs))
	ggplot(newStats, aes(x=id, y=counted)) + ggtitle("Кількість людей які пепебувають в маршрутці") + 
	  scale_x_continuous(breaks = step(newStats$id)) + 
	  scale_y_continuous(breaks = step(newStats$counted)) + 
	  geom_bar(stat="identity",fill="#FF9999", colour="black") + ylab("Кількість") + xlab("Номер зупинки")
}

# Функція для побудови графіку відношення відстаней між зупинками до її порядкового номера
plotPath <- function(stats) {
	newStats <- data.frame(id=stats$id, summed=sumPath(stats$dist))
	ggplot(newStats, aes(x=id, y=summed)) + ggtitle("Графік відстаней між зупинками") + 
	  scale_x_continuous(breaks = step(newStats$id)) + 
	  scale_y_continuous(breaks = step(newStats$summed)) + 
	  geom_line() + geom_point() + ylab("Відстань (км)") + xlab("Номер зупинки")
}

# Функція для обчислення сер. значення, моди, медіани та дисперсій дял заданого розподілу
calculateStats <- function(stats, text) {
	temp <- table(as.vector(stats))
	cat(text)
	cat(" Середнє значення:", mean(stats), "\n",
	    "Мода:", names(temp)[temp == max(temp)], "\n",
	    "Медіана:", median(stats), "\n",
	    "Дисперсія:", var(stats))
}