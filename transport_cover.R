library(dplyr)

cov_dat <- read.csv("data/BAZA-MPK.csv")

select(cov_dat, Kierunek) %>% 
  filter(!duplicated(.)) %>% 
  mutate(Kierunek = as.character(Kierunek)) %>% 
  arrange(Kierunek) %>% 
  mutate(kierunek_lc = tolower(Kierunek)) %>% 
  write.csv("direction_names.csv")

select(cov_dat, Nazwa.przystanku, linia, Kierunek) %>% 
  filter(!duplicated(.)) %>% 
  arrange(linia)

filter(cov_dat, Nazwa.przystanku == "RYNEK (KAZIMIERZA WIELKIEGO/KRUPNICZA)")

unique(cov_dat[["Nazwa.przystanku"]])
lapply(as.character(unique(cov_dat[["Kierunek"]])), function(i) {
  both_parts <- strsplit(i, "(", fixed = TRUE)[[1]]
  part1 <- both_parts[1]
  part2 <- strsplit(both_parts[2], ")", fixed = TRUE)[[1]][1]
  c(name = part1, id = part2)
})
