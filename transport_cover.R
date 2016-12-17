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
  do.call(rbind, .) 


# easy stop names
easy_sn <- inner_join(stops[, c("stop_name", "stop_code")] %>% mutate(sname = tolower(stop_name)),
           cov_dat["Nazwa.przystanku"] %>% mutate(sname = tolower(Nazwa.przystanku))
) %>% 
  filter(!duplicated(.)) %>% 
  select(-sname) %>% 
  write.csv2("./results/easy_stop_names.csv", row.names = FALSE)

cov_dat["Nazwa.przystanku"] %>% 
  filter(!duplicated(.)) %>% 
  filter(!(Nazwa.przystanku %in% easy_sn[["Nazwa.przystanku"]])) %>% 
  write.csv2("./results/assign_stop_names.csv", row.names = FALSE)

cover_sum <- select(cov_dat, Nazwa.przystanku, Godzinarzeczywista, Napełnienie) %>% 
  mutate(hour = substr(Godzinarzeczywista, 0, 2)) %>% 
  select(-Godzinarzeczywista) %>% 
  group_by(Nazwa.przystanku, hour) %>% 
  summarise(nap_sum = mean(Napełnienie)) %>% 
  ungroup %>% 
  mutate(Nazwa.przystanku = as.character(Nazwa.przystanku))

stop_id <- rbind(read.csv("results/assign_stop_names2.csv") %>% rename(stop_code = code),
      read.csv2("results/easy_stop_names.csv") %>% select(Nazwa.przystanku, stop_code)) %>% 
  na.omit %>% 
  mutate(Nazwa.przystanku = as.character(Nazwa.przystanku))

inner_join(
  data.frame(stop_code = stops[["stop_code"]],
                      reg = apply(gContains(shp1, p, byid = TRUE), 1, function(i) {
                        reg <- unname(which(i))
                        ifelse(length(reg) != 0, reg, NA)
                      })
),
inner_join(cover_sum, stop_id) %>% 
  group_by(Nazwa.przystanku, hour) %>% 
  filter(stop_code == min(stop_code)) %>% 
  ungroup()
) %>% 
  write.csv2("./results/stop_covers.csv", row.names = FALSE)

