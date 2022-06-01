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
      "Calle las ",
      "Calle la ",
      "Calle los ",
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
      "Carretera del ",
      "Carretera de ",
      "Carretera ",
      "Cortafuego de las ",
      "Cortafuego ",
      "Naves ",
      "Pasaje del ",
      "Pasaje de", 
      "Pasaje ",
      "Paseo de los ",
      "Paseo de las ",
      "Paseo del ",
      "Paseo de ",
      "Paseo ",
      "Patio de ",
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
      "Carril bici ",
      "Avinguda d'",
      "Autovia de l'",
      "Avinguda de ",
      "Avinguda del ",
      "Avinguda dels ",
      "Avinguda ",
      "Baixada de la ",
      "Baixada del ",
      "Baixada de ",
      "Camí de la ",
      "Camí de l'",
      "Camí de ",
      "Callejón del ",
      "Callejón de los ",
      "Callejón ",
      "Carrer de les ",
      "Carrer de la ",
      "Carrer del ",
      "Carrer d'",
      "Carrer de ",
      "Carrer ",
      "Carreró de ",
      "Gran Via de les ",
      "Escales de la ",
      "Escales del ",
      "Escales d'",
      "Jardins de ",
      "Jardins del ",
      "Jardins dels ",
      "Moll de la ",
      "Moll de ",
      "Passatge d'",
      "Passatge de la ",
      "Passatge de les ",
      "Passatge del ",
      "Passatge dels ",
      "Passatge de ",
      "Passatge ",
      "Passeig de la ",
      "Passeig del ",
      "Passeig de ",
      "Passeig ",
      "Patio de ",
      "Patio ",
      "Plaça d'",
      "Plaça de las ",
      "Plaça de la ",
      "Plaça de ",
      "Plaça del ",
      "Plaça dels ",
      "Plaça ",
      "Placeta de ",
      "Placeta ",
      "Pont del ",
      "Pont de ",
      "Rambla de ",
      "Rambla del ",
      "Rambla dels ",
      "Ronda de la ",
      "Ronda de ",
      "Ronda ",
      "Rotonda de l'",
      "Rotonda de ",
      "Travessia de ",
      "Torrent de la ",
      "Torrent ",
      "Túnel de la ",
      "Túnel de  ",
      "Volta dels ",
      "Via de ",
      
      "Viaducte de "
    ) %>%
      stringr::str_c("^", .)
  ),
  tibble::tibble(
    country = "Germany",
    string = c(
      "kamp ",
      
      "strasse",
      "straße",
      "weg",
      "tunnel",
      "brücke",
      "promenade",
      "platz",
      "allee",
      "steig",
      "Zeile",
      "hof",
      "gasse",
      "ring"
    ) %>%
      stringr::str_c(., "$") %>%
      c(
        .,
        c(
          "Allee der ",
          "Allée du ",
          "Allée ",
          "Am ",
          "An den ",
          "An der "
        ) %>%
          stringr::str_c("^", .)
      )
  ),
  tibble::tibble(
    country = "Netherlands",
    string = c(
      "straat",
      "laan",
      "boulevard",
      "weg",
      "passage",
      "hof",
      "kade",
      "pad",
      "brug",
      "circuit",
      "plein",
      "pleintje",
      "steeg"
    ) %>%
      stringr::str_c(., "$") %>%
      c(
        .,
        c(
          "Allee der "
        ) %>%
          stringr::str_c("^", .)
      )
  ),
  tibble::tibble(
    country = "Hungary",
    string = c(
      " utca",
      " köz",
      " út",
      " tér",
      " utcája",
      " dűlő",
      " rakpart",
      " körút",
      " hid",
      " sétány",
      " felüljáró",
      " lejtő",
      " köz",
      " sugárút",
      " zug",
      " útfél",
      " liget"
    ) %>%
      stringr::str_c(., "$")
  ),
  tibble::tibble(
    country = "Poland",
    string = c(
      "rondo ",
      "aleja ",
      "Alejka ",
      "(Alejka) ",
      "Bulwar ",
      "Most ",
      "Mostek ",
      "Pasaż ",
      "Pętla ",
      "rondo imienia ",
      "Rondo ",
      "Doktora ",
      "Harcmistrza ",
      "Księdza ",
      "Błogosławionego ",
      "Błogosławionej ",
      "Generała ",
      "Kapitana ",
      "Kardynała ",
      "Profesora ",
      "Pułkownika ",
      "Arcybiskupa ",
      "Majora ",
      "Marszałka ",
      "Plac ",
      "Skwer ",
      "Strona ",
      "Wiadukt ",
      "Wybrzeże "
    ) %>%
      stringr::str_c("^", .)
  ),
  tibble::tibble(
    country = "Romania",
    string = c(
      "Bulevardul ",
      "Strada ",
      "Calea ",
      "Șoseaua ",
      "Piața ",
      "Splaiul ",
      "Drumul ",
      "Intrarea ",
      "Aleea ",
      "Podul ",
      "Prelungirea ",
      "Fundătura ",
      "Ulița ",
      "Piațeta ",
      "Pietonalul "
    ) %>%
      stringr::str_c("^", .)
  ),
  tibble::tibble(
    country = "Italy",
    string = c(
      purrr::map(
        .x = c(
          "Via ",
          "Viale ",
          "Piazza ",
          "Piazzetta ",
          "Piazzale ",
          "Corso ",
          "Lungomare ",
          "Ponte ",
          "Ciclabile ",
          "Circonvallazione ",
          "Largo ",
          "Percorso Ciclopedonale ",
          "Belvedere ",
          "Rampa ",
          "Rotatoria ",
          "Salita ",
          "Discesa ",
          "Strada ",
          "Archivolto ",
          "Calata ",
          "Galleria ",
          "Scalinata ",
          "Sottopassaggio ",
          "Sottopasso ",
          "Sentiero ",
          "Vicolo ",
          "Vicoletto ",
          "Vico ",
          "Viottolo ",
          "Cupa ",
          "Fondaco ",
          "Gradini ",
          "Parco ",
          "Supportico ",
          "Traversa ",
          "Cortile ",
          "Fondo ",
          "Passaggio "
        ),
        .f = function(x) {
          stringr::str_c(
            x,
            c("del ",
              "dei ",
              "di ",
              "d'",
              "al ",
              "ai ",
              "alla ",
              "",
              collapse = " "
            )
          )
        }
      ) %>%
        unlist() %>%
        unique()
    ) %>%
      stringr::str_c("^", .)
  ),
  tibble::tibble(
    country = "France",
    string = c(
      purrr::map(
        .x = c(
          "Allée ",
          "Avenue ",
          "Boulevard ",
          "Camin ",
          "Champ ",
          "Chaussée ",
          "Chemin ",
          "Clos ",
          "Corniche ",
          "Cours ",
          "Descente ",
          "Domaine ",
          "Drève ",
          "Escaliers ",
          "Escalier ",
          "Espace ",
          "Esplanade ",
          "Galerie ",
          "Grande rue ",
          "Impasse ",
          "Montée",
          "Parc ", 
          "Parvis ",
          "Passerelle ",
          "Passage ",  
          "Petite Avenue ",
          "Petite rue ",
          "Placette ",
          "Place ",
          "Pont ",  
          "Promenade ",
          "quai ",
          "Raccourci ",
          "rampe ",  
          "route ",
          "Rue ",
          "Sentier ",
          "Square ",
          "Terrasse ",
          "Traverse ",
          "Tunnel ",
          "Vieux Chemin ",
          "Voie "
        ),
        .f = function(x) {
          stringr::str_c(
            x,
            c("des ",
              "de l'",
              "de la ",
              "de ",
              "du ",
              "au ",
              "d'",
              "",
              collapse = " "
            )
          )
        }
      ) %>%
        unlist() %>%
        unique()
    ) %>%
      stringr::str_c("^", .)
  ),
  tibble::tibble(
    country = "Portugal",
    string = c(
      purrr::map(
        .x = c(
          "Alameda ",
          "Azinhaga ",
          "Avenida ",
          "Bairro ",
          "Beco ",
          "Calçada ",
          "Caminho ",
          "Canto ",
          "Escadaria ",
          "Escadas ",
          "Escadinhas ",
          "Estrada ",
          "Ladeira ",
          "Largo ",
          "Praça ",
          "Praceta ",
          "Rua ",
          "Travessa ",
          "Trilho ",
          "Viaduto ",
          "Viela "
        ),
        .f = function(x) {
          stringr::str_c(
            x,
            c("das ", 
              "dos ",
              "da ",
              "do ",
              "de ",
              "",
              collapse = " "
            )
          )
        }
      ) %>%
        unlist() %>%
        unique()
    ) %>%
      stringr::str_c("^", .)
  ),
  tibble::tibble(
    country = c("Ireland"),
    string = c(
      " Avenue Lower",
      " Avenue Upper",
      " avenue",
      " bridge",
      " boulevard",
      " crescent",
      " close",
      " court",
      " drive",
      " gardens",
      " glen",
      " green",
      " grove",
      " heath",
      " lane",
      " lawn",
      " manor",
      " meadows",
      " mews",
      " motorway",
      " park",
      " place",
      " plaza",
      " quay",
      " rise",
      " road",
      " row",
      " roundabout",
      " Square West",
      " Square North",
      " Square East",
      " Square South",
      " square",
      " street",
      " terrace",
      " walk",
      " way",
      " view"
    ) %>%
      stringr::str_c(., "$")
  ),
  tibble::tibble(
    country = c("Greece"),
    string = c(" ") %>%
      stringr::str_c(., "$")),
  tibble::tibble(
    country = c("Czech Republic"),
    string = c(" ") %>%
      stringr::str_c(., "$"))
)

sn_street_name_to_remove_df <- sn_street_name_to_remove_df %>%
  bind_rows(sn_street_name_to_remove_df %>%
              dplyr::filter(country == "France" | country == "Netherlands") %>%
              mutate(country = "Belgium") %>%
              distinct())

sn_street_name_to_remove_df %>% tail()
usethis::use_data(sn_street_name_to_remove_df,
                  overwrite = TRUE
)
