
data <- read_csv("data/NSWFRD_2014/raw/data.csv", guess_max = 3086)

transcribed_time_to_flowering <- read_csv("data/NSWFRD_2014/raw/substitutions_for_time_from_fire_to_flowering.csv")

data %>% 
  left_join(transcribed_time_to_flowering) -> data

data %>% write_csv("data/NSWFRD_2014/data.csv")
