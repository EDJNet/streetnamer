## code to prepare `sn_street_name_to_remove_df` dataset goes here
library("dplyr")

sn_street_name_to_remove_df <- dplyr::bind_rows(
  tibble::tibble(
    country = "Spain",
    string = c(	
      "Calle de las ",
      "Calle de la ",
      "Calle de los ",
      "Calle del ",
      "Calle el ",
      "Calle de ",
      "Calle ",
      "Calleja ",
      "Callejón de ",
      "Callejón ",
      "Camino del ",
      "Camino de las ",
      "Camino de la ",
      "Camino de los ",
      "Camino de ",
      "Camino ",
      "Carretera de ",
      "Carretera ",
      "Cortafuego de las ",
      "Cortafuego ",
      "Plaza de la ",
      "Plaza de los ",
      "Plaza de ",
      "Plaza del ",
      "Plaza ",
      "Puente de ",
      "Puente del ",
      "Ronda del ",
      "Vereda de la ",
      "Vereda de los ",
      "Avenida de la ",
      "Avenida de los ",
      "Avenida de las ",
      "Avenida del ",  
      "Avenida de ",
      "Avenida ",
      "Autovía del ",
      "Autovía de ",
      "Bulevar de ", 
      "Bulevar ",
      "Travesía de las ",
      "Travesía de los ",
      "Travesía del ",
      "Travesía de ",
      "Travesía ",
      "Via de ",
      "Via ",
      "Viaducto de ",
      "Glorieta de ",
      "Glorieta del ",
      "Glorieta ",
      "Cuesta de ",
      "Costanilla de los",
      "Costanilla de ",
      "Carril bici "
    ) %>% 
      stringr::str_c("^", .)),
  
  tibble::tibble(
    country = "Germany",
    string = c("strasse",
               "Strasse",
               "Straße",
               "straße",
               "weg",
               "Weg",
               "tunnel",
               "brücke",
               "promenade",
               "Platz",
               "platz",
               "allee",
               "steig",
               "Zeile",
               "hof")  %>% 
      stringr::str_c(., "$") %>% 
      c(. ,
        c("Allee der ",
          "Allée du ",
          "Allée ",
          "Am ") %>%  
        stringr::str_c("^", .))
      
  )
) 
  
sn_street_name_to_remove_df %>% tail()
usethis::use_data(sn_street_name_to_remove_df,
                  overwrite = TRUE)