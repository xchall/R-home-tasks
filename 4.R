#install.packages("rvest")
#install.packages("dplyr") 
library(dplyr) 
library(rvest)

#получим таблицу за определенный год переданный как параметр year
get_data_year <- function(year) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  html_data <- read_html(url)
  table <- html_table(html_nodes(html_data, 'table'))[[2]] %>% as.data.frame()
  table[1:nrow(table), 1] <- rownames(table)  # Обновляем номера строк
  table$Year <- year  # Добавляем столбец с годом
  table$'Climate Index' <- as.double(table$'Climate Index') #меняем на числовой тип данных
  return(table)
}
years <- 2014:2021
combined_data <- lapply(years, get_data_year) %>% bind_rows()

#Куба, Аргентина, Болгария, Венгрия, Латвия
selected_countries <- c("Argentina", "Bulgaria", "Hungary", "Latvia") #Убрал Кубу, потому что нет данных по ней
result <- combined_data %>% filter(Country %in% selected_countries) %>%arrange(Country, Year)
#arrange сортирует данные сначала по стране, затем по году
result

#Для каждой страны делаем свой датафрейм
#df_Cuba <- result %>% filter(Country == "Cuba") #этот df пустой, на сайте нет никакой информации по Кубе
df_Argentina <- result %>% filter(Country == "Argentina")
df_Bulgaria <- result %>% filter(Country == "Bulgaria")
df_Hungary <- result %>% filter(Country == "Hungary") #Венгрия
df_Latvia <- result %>% filter(Country == "Latvia")

#График 1
colors <- c("red", "green", "blue", "yellow")
metrics <- setdiff(names(result), c("Rank", "Country", "Year"))
names(colors) <- selected_countries #colors теперь именнованный вектор именами стран

#Создаем разметку для графиков и легенды
layout_matrix <- matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE)
layout_matrix <- cbind(layout_matrix, rep(13, 3)) # Добавляем колонку для легенды

#Устанавливаем параметры расположения
layout(layout_matrix, widths = c(rep(1, 4), 0.5)) # Легенда занимает 0.5 от ширины графика
par(mar = c(4, 4, 3, 1), oma = c(0, 0, 0, 2))

#Создаем графики
for(i in seq_along(metrics)) {
  metric <- metrics[i]
  y_lim <- range(result[[metric]], na.rm = TRUE)
  
  plot(NA, xlim = range(years), ylim = y_lim,main = metric, xlab = "", ylab = "")
  
  for(country in selected_countries) {
    country_data <- result[result$Country == country, ]
    lines(country_data$Year, country_data[[metric]], 
          col = colors[country], 
          lwd = 2)
    points(country_data$Year, country_data[[metric]], 
           col = colors[country], 
           pch = 16)
  }
  title(xlab = "Годы", ylab = "Значения")
  
}

#Рисуем общую легенду
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center", legend = selected_countries, col = colors, lty = 1, lwd = 2, pch = 16,cex = 1.2, bty = "n", title = "Страны")

#Восстанавливаем стандартные настройки
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)


#График 2


#Получаем данные о рангах
rank_data <- result %>% select(Country, Year, Rank) %>% arrange(Country, Year) %>% mutate(Rank = as.numeric(Rank))

# Увеличиваем правый отступ для легенды
par(mar = c(5, 4, 4, 8), xpd = FALSE)  # Правое поле 8 вместо стандартных 4

#Создаем основной график
plot.new()
plot.window(xlim = range(years), ylim = c(max(rank_data$Rank) + 10, 1))  # Ось Y перевернута
title(main = "Динамика рейтингов стран (2014-2021)", xlab = "Год", ylab = "Место в рейтинге")
axis(1, at = years)
axis(2, at = seq(1, max(rank_data$Rank) + 5, by = 5))

#Добавляем сетку, которая не выходит за пределы графика
#Задаем цвет с прозрачностью для построения сетки
transparent_gray <- rgb(0.8, 0.8, 0.8, alpha = 0.5) # 0.5 - 50% прозрачности

abline(v = years, col = transparent_gray, lty = "solid") # вертикальные линии
abline(h = seq(1, max(rank_data$Rank) + 5, by = 5), col = transparent_gray, lty = "solid") # горизонтальные линии

# Рисуем линии для каждой страны
colors <- c("red", "blue", "green", "yellow")
names(colors) <- selected_countries

for(country in selected_countries) {
  country_ranks <- rank_data[rank_data$Country == country, ]
  lines(country_ranks$Year, country_ranks$Rank, 
        col = colors[country], lwd = 2)
  points(country_ranks$Year, country_ranks$Rank, 
         col = colors[country], pch = 16)
  
  # Подписи значений
  text(years, 
       country_ranks$Rank,
       labels = country_ranks$Rank,
       pos = 1, col = colors[country], cex = 0.6, xpd = TRUE)
}

#Добавляем легенду в увеличенное правое поле
legend(x = max(years) + 0.5,  # Сдвигаем вправо от последнего года
       y = par("usr")[4],  #  Сверху по вертикали
       legend = selected_countries, 
       col = colors, 
       lty = 1, lwd = 2, pch = 16,
       bty = "n",  # Без рамки
       x.intersp = 0.8, xpd = TRUE)  # Уменьшаем расстояние между элементами

#Восстанавливаем стандартные параметры
par(mar = c(5, 4, 4, 2) + 0.1, xpd = FALSE)



#мусор
url <- "https://tonkosti.ru/Музеи_Санкт-Петербурга"
encoded_url <- URLencode(url)

# Заголовки, имитирующие Chrome
headers <- add_headers(
  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
)

response <- GET(encoded_url, timeout(30), headers) 
page <- read_html(response)




#Вариант 8
#Музеи Ростовской области
#install.packages("chromote")
library(rvest)
library(dplyr)
library(chromote) #нужно было для read_html_live


url <- "https://ru.wikipedia.org/wiki/Список_музеев_Ростовской_области"
page <- read_html(url)


#полный селектор к самому первому названию
#mw-content-text > div.mw-content-ltr.mw-parser-output > table > tbody > tr:nth-child(1) > td:nth-child(2) > a
# trim=TRUE удаляет лишние пробелы
selector_names <- "table.wikitable > tbody > tr > td:nth-child(2) > a" 
nodes_names <- html_nodes(page, selector_names)
names <- html_text(nodes_names, trim=TRUE);
names <- names[-47] 

names
selector_addresses <- "table.wikitable > tbody > tr > td:nth-child(3)" 
nodes_addresses <- html_nodes(page, selector_addresses)
addresses <- html_text(nodes_addresses, trim=TRUE)
addresses

selector_descriptions <- "table.wikitable > tbody > tr > td:nth-child(5)"
nodes_descriptions <- html_nodes(page, selector_descriptions)
descriptions <- html_text(nodes_descriptions, trim=TRUE)
descriptions

selector_references <- "table.wikitable > tbody > tr > td:nth-child(2) > a"
part_references <- html_nodes(page, selector_references) %>% html_attr("href");
part_references <- part_references[-47] 

first_part_url <- "https://ru.wikipedia.org"
references <- paste0(first_part_url, part_references)
references

df <- data.frame(names, addresses, descriptions, references)
View(df)
