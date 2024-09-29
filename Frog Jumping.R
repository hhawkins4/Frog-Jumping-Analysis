
library(tidyverse)
library(ggplot2)

library(readxl)
frogs <- read_excel("~/Downloads/CalaverasData.xls")
View(frogs)

#Chunk #2
frogs <-  rename(frogs,
                 day = Day,
                 frog_type = `rent/ind/pro`,
                 distance = `jump distance`,
                 jump_n = `jump #`,
                 distance_rel = `Relative to jump #1`,
                 distance_3 = `3-jump dist`,
                 distance_3_off = `measured 3-jump`,
                 duration = `jump duration`,
                 angle_00 = unknown1,
                 angle_01 = angle,
                 angle_10 = unknown2,
                 velocity_00 = unknown3,
                 velocity_01 = Vel,
                 velocity_10 = unknown4
) %>%
  mutate(
    row = seq_len(nrow(.))
  ) %>%
  select(row, everything())

#Chunk #3
## dbl --> int
frogs <- frogs %>%
  mutate(jump_n = as.integer(jump_n))

#Chunk #4
## frog_type
frogs <- frogs %>%
  mutate(
    frog_type2 = case_when(
      frog_type == 1 ~ "rental",
      frog_type == 2 ~ "individual",
      frog_type == 3 ~ "pro",
      frog_type < 0 ~ "unknown"
    )
  )

mean(frogs$distance, na.rm = TRUE)
frogs %>% 
  filter(frog_type == 1 | frog_type == 3) %>% 
  group_by(frog_type) %>% 
  summarise(average_distance = mean(distance, na.rm = TRUE))
max(frogs$distance, na.rm = TRUE)
frogs %>% filter(frog_type == 1) %>% summarise(average_distance_rental = mean(distance, na.rm = TRUE))
frogs %>% filter(frog_type == 3) %>% summarise(average_distance_pro = mean(distance, na.rm = TRUE))
frogs %>% filter(frog_type == 1 | frog_type == 3) %>% count(frog_type)

frogs<- frogs[frogs$distance != 0, ]


ggplot(frogs, aes(x = distance)) +
  geom_histogram(binwidth =5, fill = "green", color = "black") +
  labs(title = "Histogram of All Observed Jumps",
       x = "Jump Distance (m)",
       y = "Frequency") +
  theme_minimal()

ggplot(frogs[frogs$frog_type == 1,], aes(x = distance)) +
  geom_histogram(binwidth = 5, fill = "grey", color = "black") +
  labs(title = "Histogram of Jump Distances for Rental Frogs",
       x = "Jump Distance (m)",
       y = "Frequency") +
  theme_minimal()

ggplot(frogs[frogs$frog_type == 3,], aes(x = distance)) +
  geom_histogram(binwidth = 5, fill = "darkgreen", color = "black") +
  labs(title = "Histogram of Jump Distances for Professional Frogs",
       x = "Jump Distance (m)",
       y = "Frequency") +
  theme_minimal()

rent_frogs <- frogs[frogs$frog_type == 1, ]
pro_frogs <- frogs[frogs$frog_type == 3, ]

set.seed(123)  # Ensuring replicability

# Simulation to calculate the probability of observing at least a 1.6 m jump in 50 rental frog jumps
results_rent_50 <- replicate(5000, max(sample(rent_frogs$distance, 50, replace = FALSE)) >= 1.6)
prob_rent_50 <- mean(results_rent_50)

# Output the probability
print(prob_rent_50)

set.seed(123)  # Ensuring replicability

# Simulation for professional frogs
results_pro_50 <- replicate(5000, max(sample(pro_frogs$distance, 50, replace = FALSE)) >= 1.6)
prob_pro_50 <- mean(results_pro_50)

# Output the probability
print(prob_pro_50)
# Simulation for rental frogs with sample size of 10
results_rent_10 <- replicate(5000, max(sample(rent_frogs$distance, 10, replace = FALSE)) >= 1.6)
prob_rent_10 <- mean(results_rent_10)

# Output the probability
print(prob_rent_10)


# Creating a histogram of the results
max_jumps_rent_10 <- replicate(5000, max(sample(rent_frogs$distance, 10, replace = FALSE)))

# Convert results into a dataframe for ggplot
jump_data <- data.frame(Max_Jump = max_jumps_rent_10)

# Create the histogram using ggplot2
ggplot(jump_data, aes(x = Max_Jump, fill = Max_Jump >= 1.6)) +
  geom_histogram(binwidth = 3, color = "black") +
  labs(title = "Histogram of Maximum Jump Distances for 10 Rental Frog Jumps",
       x = "Max Jump Distance (m)", 
       y = "Frequency") +
  scale_color_brewer(palette='Dark2')








