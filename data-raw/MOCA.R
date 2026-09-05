# Data licensed under CC BY-NC 4.0 (Attribution-NonCommercial); see Montemurro
# et al. (2023), doi:10.1007/s12144-022-03062-6.
MOCA <- read.csv("data-raw/MOCA_Dataset.csv", stringsAsFactors = FALSE)
usethis::use_data(MOCA, overwrite = TRUE)
