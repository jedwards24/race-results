
library(tidyverse)
#library(lubridate)
library(edwards)

dt <- readRDS("data_processed/xc_results_all.RDS") %>% 
  mutate(year = str_sub(race_id, -2, -1))
count_nas(dt)
count(dt, race_id)
count(dt, non_count) # something wrong here

dt <- select(dt, -non_count)
#Race winners ----------------
dt %>% group_by(race_id) %>% summarise(fastest = min(time, na.rm = T)) %>% print(n = Inf)

#These would work with character time I think
winners <- dt %>% group_by(race_id) %>% top_n(1, desc(seconds))
cat_winners <- dt %>% group_by(race_id, cat) %>% top_n(1, desc(seconds))

#Or using placings
winners <- filter(dt, place == 1)
cat_winners <- filter(dt, cat_place == 1)

filter(cat_winners, cat == "V40")

#Single runner info --------------------
runner = "James Edwards"
jed <- dt %>% filter(name == runner) %>% select(-club)
arrange(jed, cat)
ggplot(jed, aes(x = race_id, y = pc_win)) + geom_col()
plot(Jed$pc_winner)
dt %>% filter(name == runner)

# Compare a pair of runners --------------------
# It'd be useful to have this work with vectors of names. Output will be a list.
run2 <- "James Edwards"
run1 <- "Phil Mather"
run1 <- "Lee Parrington"
dt2 <- dt %>% rename(raceID = race_id)
  
(compare <- compare_runners(dt2, run1, run2) %>% select(-c(seconds1, seconds2)))

ggplot(compare, aes(x = factor(raceID, levels = raceID), y = multiple - 1)) +
  geom_col() +
  coord_flip(ylim = c(min(c(compare$multiple - 1, 0)), max(compare$multiple - 1)))

# Analyse rivals --------------------
runner <- "James Edwards"

# Creates df of people/race combinations who have raced with "runner".
all_raced_with <- dt2 %>%
  filter(year == "20") %>% 
  raced_with(runner)

# Creates df summary of all runners who have raced with "runner"
rivals_full <- all_raced_with %>% group_by(name) %>% 
  summarise(races=n(), races_faster=sum(multiple < 1), 
            prop_faster=mean(multiple < 1), avg_multiple=mean(multiple), 
            oldest_cat=max(cat)) %>%
  mutate(vet=check_vet(oldest_cat)) %>%
  arrange(desc(races)) 
rivals_full

# Various filters of rivals full
filter_rivals(rivals_full, max_multiple=1.08, min_races=2, desc=T)
filter_rivals(rivals_full, min_multiple=0.97, max_multiple=1.03, min_races=2) %>% arrange(desc(races), desc(avg_multiple))
filter_rivals(rivals_full, min_prop_faster = 1, min_races=3) %>% arrange(desc(races)) #never beaten

filter_rivals(rivals_full, min_multiple=0.9, max_multiple=1.04, min_races=2, vet_filter="Y")

