# Підключення бібліотеки для побудови графіків
library(ggplot2)
# Підготовка даних для подальшого опрацювання
stats <- read.csv("./data/lab2.csv")

# Обчилення статистичних даних та побудова графіків для маршруту першого напрямку
statsFirst <- data.frame(id=stats$id, ins=stats$one_in, outs=stats$one_out)
calculateStats(countPeople(statsFirst$ins, statsFirst$outs), 
  "Статистичні дані для розподілу к-сті людей для маршруту першого напрямку:\n")
plotInOut(statsFirst)
plotStats(statsFirst)

# Обчилення статистичних даних та побудова графіків для маршруту другого напрямку
statsSecond <- data.frame(id=stats$id, ins=stats$two_in, outs=stats$two_out)
calculateStats(countPeople(statsSecond$ins, statsSecond$outs),
  "Статистичні дані для розподілу к-сті людей для маршруту другого напрямку:\n")
plotInOut(statsSecond)
plotStats(statsSecond)

# Графік функції відношення відстаней між зупинками до її порядкового номера
plotPath(stats)
