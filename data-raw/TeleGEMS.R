# Data licensed under CC BY-NC 4.0 (Attribution-NonCommercial); see Montemurro
# et al. (2023), doi:10.1007/s10072-023-06862-1.
TeleGEMS <- read.csv("data-raw/TeleGEMS_Dataset.csv", stringsAsFactors = FALSE)
TeleGEMS$ID <- seq_len(nrow(TeleGEMS))
usethis::use_data(TeleGEMS, overwrite = TRUE)
