## code to prepare `sn_available_lau` dataset goes here

library("streetnamer")
library("latlon2map")
ll_set_folder(path = fs::path(fs::path_home_r(), "R", "ll_data"))
sn_set_data_folder(fs::path(fs::path_home_r(), "R", "sn_data"))
options(timeout = 12000)

temp <- ll_get_lau_eu()


usethis::use_data(sn_available_lau, overwrite = TRUE)
