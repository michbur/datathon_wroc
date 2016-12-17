library(dplyr)

cov_dat <- read.csv("data/BAZA-MPK.csv")
stops <- read.table("data/stops.txt", sep = ",", header = TRUE)

select(cov_dat, Kierunek) %>%
  filter(!duplicated(.)) %>%
  mutate(Kierunek = as.character(Kierunek)) %>%
  arrange(Kierunek) %>%
  mutate(kierunek_lc = tolower(Kierunek)) %>%
  write.csv2("./results/direction_names.csv", row.names = FALSE)

# repletion - hour data
reph_dat <- select(cov_dat, Nazwa.przystanku, Kierunek, Godzinarzeczywista, Napełnienie) %>% 
  mutate(hour = substr(Godzinarzeczywista, 0, 2)) %>% 
  select(-Godzinarzeczywista) %>% 
  group_by(Nazwa.przystanku, Kierunek, hour) %>% 
  summarise(nap_sum = mean(Napełnienie)) %>% 
  ungroup


library(ggplot2)
filter(reph_dat, Nazwa.przystanku == "PLAC BEMA") %>% 
  ggplot(aes(x = hour, y = nap_sum)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Kierunek)

select(cov_dat, linia, Nazwa.przystanku) %>% 
  filter(!duplicated(.)) 

lapply(as.character((cov_dat[["Kierunek"]])), function(i) {
  splitted <- strsplit(i, "\\([0-9]")[[1]]
  c(name = splitted[1], id = strsplit(splitted[2], ")")[[1]][1])
  }) %>% 
  do.call(rbind, .) %>% 
  edit

cov_dat[["Nazwa.przystanku"]]
