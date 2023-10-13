library(tidyverse)

# variables temporales
years <-  seq(2020, 2023)
months <- seq(1, 12, 1)
this_year <-  year(today())
this_month <-  month(today())


year_months <- expand_grid(years, months) %>%
  arrange(years, months) %>%
  filter(!(years == this_year & months >= this_month)) %>%
  mutate(date = paste0(years, "_", months))
tail(year_months)





Map(f = function(i, b){
  a <- i+b
  return(a)
}, i = 1:10, b = 1:20)


i <- year_months$date[1]
f <-  sprintf("https://transparenciachc.blob.core.windows.net/oc-da/%s.zip", i)
f <-  sprintf("https://transparenciachc.blob.core.windows.net/oc-da/%s.zip", i)
download.file(f, destfile = basename(f))


# hipoteunsa de los catetos
# a secuencia 1  al 3
# b secuecia 5 al 8
# evaluar todos con todos

i
hip <-  function(a, b){
  print(i)
  h <-  sqrt(a^2 + b^2)
  return(h)
}
a = 1:10
b = 1:10
pos <- expand_grid(a, b)


#native
hi <- Map(f = hip, pos$a, pos$b)

pos <- pos %>%
  mutate(hipotenusa = Reduce(f = c, x = hi))
plot(pos$hipotenusa, type = "l")
ff <- c("c", "list")
do.call(mean, hi)
do.call("mean", hi)

df_VV <- ee_get_date_ic(VV)%>%
  arrange(time_start)
df_VV



vectors <- list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
combined_matrix <- do.call(rbind, vectors)
combined_matrix

data_frames <- list(
  data.frame(a = 1:3),
  data.frame(a = 4:6),
  data.frame(a = 7:9)
)
mean_results <- do.call(
  rbind,
  lapply(data_frames, function(df) mean(df$a))
)
mean_results

vectors <- list(c(1, 2, 3),
                c(4, 5, 6))
result <- do.call("+", vectors)




# use of vector assignment
vector.assn <- function(n, x) {
  result <- double(n)
  result[] <- x
  result
}
n <- 100
x <- 7
bad.for <- function(n,x) {
  result <- NULL
  for (i in 1:n) {
    result[i] <- x
  }
  result
}
bad.result <- bad.for(n, x)

