
# count offspring by source
Ri_counts <- aggregate(id ~ source + source_group + group,
                       data = data,
                       FUN = length)
names(Ri_counts)[length(names(Ri_counts))] <- c("Ri")

# IDs without offsprings
no_offspring <-
  data.frame(id = setdiff(unique(data$id), unique(data$source)))
no_offspring <-
  merge(no_offspring, data[c("id", "group")], by = "id", all.x = TRUE)
names(no_offspring) <- c("source", "source_group")
no_offspring$group <- NA
no_offspring$Ri <- 0

Ri_counts <- rbind(Ri_counts, no_offspring)


Ri_data <-
  merge(Ri_counts,
        data[c("id", "date_infection")],
        by.x = "source",
        by.y = "id",
        all.x = TRUE)


Ri_data %>% 
  mutate(group = ifelse(is.na(group), "A", group)) %>% 
  group_by(source_group, group, date_infection) %>% 
  summarise(Ri = mean(Ri)) %>% 
  ggplot() +
  aes(x = date_infection,
      y = Ri ,
      col = source_group) +
  facet_wrap(source_group ~ group)+
  geom_point() +
  geom_line() +
  geom_hline(aes(yintercept = 1), lty = "dashed") 


#offspring dist
Ri_counts %>% 
  group_by(source_group, Ri) %>% 
  summarise(n = n()) %>% 
  ggplot()+
  aes(x = Ri,
      y = n ,
      fill = source_group) +
  geom_col(position = "dodge", col = "black")



#offspring dist
Ri_data %>% 
  group_by(source_group, date_infection) %>% 
  summarise(Ri = mean(Ri)) %>% 
  ggplot()+
  aes(x = date_infection,
      y = Ri ,
      col = source_group) +
  geom_line()+
  geom_point()


# OLD ---------------------------------------------------------------------
library(tidyverse)
plot_Ri2 <- function(data, bylabel = TRUE) {

  # Count the number of times a source appears in each group
  source_counts <- data %>%
    drop_na(source) %>% #drop introductions
    group_by(group) %>%
    count(source)

  #Retrieve basic source info
  info <- data %>%
    select(id, group, date_infection) %>%
    rename(source = id,
           source_group = group)

  indivRi <- left_join(source_counts, info, by = "source") %>%
    mutate(label = paste0(source_group, "-->", group))

  #By label (A->A, A->B etc...)
  if (isTRUE(bylabel)) {

    Ri <- indivRi %>%
      group_by(label, date_infection) %>%
      summarise(Ri = mean(n)) %>%
      ungroup() %>%
      mutate(source_group = sub("-->.*", "", label))

    #Factoring for Facets
    source_levels <- unique(Ri$source_group) #sort()
    label_levels <-
      paste0(rep(source_levels, each = length(source_levels)), "-->", source_levels)
    Ri$label <- factor(Ri$label, levels = label_levels)

    p <- Ri %>%
      ggplot(aes(x = date_infection, y = Ri)) +
      geom_point(aes(col = source_group)) +
      geom_line(aes(col = source_group)) +
      geom_hline(aes(yintercept = 1), col = "grey", lty = "dotted") +
      facet_wrap( ~ label) +
      theme_bw() +
      theme(legend.position = "bottom") +
      labs(title = "Case Reproduction Number (Ri)",
           x = "Date of infection",
           y = "Ri")

  } else{

    Ri <- indivRi %>%
      group_by(source, source_group, date_infection) %>%
      summarise(Ri = sum(n)) %>%
      ungroup() %>%
      group_by(source_group, date_infection) %>%
      summarise(Ri = mean(Ri)) %>%
      ungroup()

    p <- Ri %>%
      ggplot(aes(x = date_infection, y = Ri)) +
      geom_point(aes(col = source_group)) +
      geom_line(aes(col = source_group)) +
      geom_hline(aes(yintercept = 1), col = "grey", lty = "dotted") +
      theme_bw() +
      theme(legend.position = "bottom") +
      labs(title = "Case Reproduction Number (Ri)",
           x = "Date of infection",
           y = "Ri")
  }
  #02vQdb
  return(list(indivRi = indivRi,
              Ri = Ri,
              p = p))

}
plot_Ri2(data, bylabel = F)

