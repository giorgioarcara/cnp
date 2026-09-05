# Data licensed under CC BY-NC 4.0 (Attribution-NonCommercial); see Mondini
# et al. (2022), doi:10.1002/brb3.2710.
GEMS <- read.csv("data-raw/GEMS_Dataset.csv", stringsAsFactors = FALSE)
usethis::use_data(GEMS, overwrite = TRUE)
