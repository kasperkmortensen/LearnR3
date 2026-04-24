library(here)

# The dataset can be downloaded manually from
# <https://drive.proton.me/urls/ZHZ6TCAN2W#e5D5U4IajdYp>.
# Make sure to save this file to the `data-raw/` folder with the name
# `nurses-stress.tar`.

untar(
  here("data-raw/nurses-stress.tar"),
  exdir = here("data-raw/nurses-stress/")
)
