listings <- listings %>% 
  mutate(price = parse_number(price))

listings %>% 
  filter(price<1000) %>% 
  ggplot(aes(x=log(price)))+
  geom_density()

listings %>% 
  select(price, listing_url) %>% 
  arrange(desc(price)) %>% 
  head(10)


# Propery types
listings %>% 
  count(property_type, sort=TRUE)

listings <- listings %>%
  mutate(prop_type_simplified = case_when(
    property_type %in% c("______","______", "______","______") ~ property_type, 
    TRUE ~ "Other"
  ))


# Neighbourhood
listings %>% 
  count(neighbourhood, sort=TRUE)


listings %>% 
  count(neighbourhood_cleansed, sort=TRUE)


listings %>% 
  count(neighbourhood_group_cleansed, sort=TRUE)


listings %>% 
  count(host_is_superhost, sort=TRUE)

listings %>% 
  count(instant_bookable, sort=TRUE)
