# Please run draft.R first

Ri <- get_Ri(data)
head(Ri)

library(tidyverse)
Ri <- Ri %>% as_tibble() %>% select(id, group, date_infection, A, B, Ri)
Ri

Ri %>% 
  group_by(group, date_infection) %>% 
  summarise(Ri = mean(Ri)) %>% 
  ggplot(aes(x = date_infection,
             y = Ri,
             col = group))+
  geom_line()+
  geom_point()+
  gg_col +
  gg_scale


Ri_long <- Ri %>% 
  pivot_longer(cols = c("A", "B"),
               names_to = "target") %>% 
  select(id, group, target, value, date_infection) %>% 
  mutate(label = paste0(group, "-->", target)) 
head(Ri_long)

Ri_long %>% 
  group_by(group, target, date_infection) %>% 
  summarise(Ri = mean(value)) %>%
  ggplot(aes(x = date_infection,
             y = Ri,
             col = group))+
  geom_line()+
  geom_point()+
  facet_wrap(group~target)+
  gg_col +
  gg_scale

