library("dplyr", warn.conflicts = FALSE)
library("shiny")
library("streetnamer")
library("latlon2map")

user_base <- tibble::tibble(
  user = c("secretexporter", "secretimporter"),
  password = c("secretpassword", "secretpassword"),
  permissions = c("export", "import"),
  name = c("Download", "Upload")
)

options(timeout = 60000) # big timeout, as big downloads needed 

ll_set_folder(path = "/streetnamer_data/ll_data")

library(dplyr, warn.conflicts = FALSE)
library("tidywikidatar")

tw_set_language(language = "en")

tw_enable_cache(SQLite = FALSE)


connection <- tw_set_cache_db(driver = "MySQL ODBC 8.0 Driver",
                              host = "streetnamer_db",
                              server = "streetnamer_db",
                              port = 3306,
                              database = "secretdbname",
                              user = "secretusername",
                              pwd = "secretpassword")



sn_set_data_folder("/streetnamer_data/sn_data")

#sn_create_data_folder()


sn_run_app(user_base = user_base,
           country_name = "Belgium",
           connection = connection)
