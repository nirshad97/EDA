# Load the libraries
library(tidyverse)

# Load in the datasets
billboard

# Tidyr functions
billboard %>% 
  pivot_longer(wk1:wk76, names_to="week", values_to='rank') %>% 
  drop_na() %>% 
  # Convert back to the normal data set
  pivot_wider(names_from = week, values_from = rank, values_fill= as.double(-999) )


# One hot encoding
billboard %>% 
  select(artist, track) %>% 
  mutate(n = 1) %>% 
  pivot_wider(names_from = artist, values_from = n, values_fill = 0)


#Drops na
billboard %>% 
  drop_na()

# Dplyr

#Distinct values
iris %>% 
  distinct(Species)


#Mutate with pivot_wider
iris %>% 
  mutate(sqrt_sepal = sqrt(Sepal.Length),
         larger_than_five = if_else(Sepal.Length >5, "Greater than 5", "Not Greater than 5")) %>% 
  count(Species, larger_than_five) %>% 
  pivot_wider(names_from = larger_than_five, values_from = n)


#Mutate with ifelse
iris %>% 
  mutate(sqrt_sepal = sqrt(Sepal.Length),
         larger_than_five = if_else(Sepal.Length >5, "Greater than 5", "Not Greater than 5")) %>% 
  count(Species, larger_than_five, sort = TRUE)


# Filter
iris %>% 
  filter(Species %in% c("setosa", "versicolor")) 


# Groupby
iris %>% 
  group_by(Species) %>% 
  summarise(avg_sepal_length = mean(Sepal.Length))


iris %>% 
  group_by(Species) %>% 
  # When we group it actually computes operations for group dataframes
  mutate(avg_sepal_length = mean(Sepal.Length)) %>%
  # Ungroup
  ungroup() %>% 
  # Calculate the mean sepal length
  mutate(avg_length = mean(Sepal.Length))


# Groupby and the count
iris %>% 
  group_by(Species) %>% 
  summarise(avg_length = mean(Sepal.Length),
            no_of_obs = n())


# Case when in dplyr
iris %>% 
  mutate(test = case_when(Sepal.Length > 5 & Sepal.Width > 3 ~ "Large",
                          Sepal.Length < 4 & Sepal.Width < 2 ~ "Small",
                          T ~ "Medium"))


#Lag
economics %>% 
  select(date, unemploy) %>% 
  mutate(prev_unemploy = lag(unemploy, n=1, order_by = date)) %>% 
  ggplot(aes(x=date)) + 
  geom_line(aes(y=unemploy)) +
  geom_point(aes(y = prev_unemploy))



#Ranking
iris %>% 
  arrange(desc(Sepal.Length)) %>% 
  mutate(s_length_rank = row_number())


# Slice function
iris %>% 
  slice_max(Sepal.Length)

iris %>% 
  arrange(desc(Sepal.Length)) %>% 
  slice(1:15)

band_members %>% 
  left_join(band_instruments)
