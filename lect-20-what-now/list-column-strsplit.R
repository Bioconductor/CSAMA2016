library(purrr)
library(tibble)
library(dplyr)

x <- c("Robert Downey Jr",
       "Prince",
       "Taylor Swift",
       "Charles Philip Arthur George Mountbatten-Windsor")
(df <- enframe(x, "i"))

df %>%
  mutate(words = strsplit(x, " "),
         n_words = lengths(words),
         first_name = map(words, first)) %>%
  View()

