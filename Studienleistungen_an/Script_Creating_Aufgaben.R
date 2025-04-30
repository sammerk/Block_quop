library(tidyverse)
data <- tibble::tribble(
          ~Nachname,                 ~Vorname,
         "Albrecht",                "Melanie",
         "Alliegro",                 "Alissa",
            "Appel",              "Katharina",
             "Arno",                 "Nadine",
             "Bach",           "Stefanie Lia",
           "Barsch",             "Lena-Marie",
          "Benthin",            "Emma Sophie",
         "Bickmann",                   "Luis",
           "Bleier",              "Lea-Marie",
             "Boos",              "Stephanie",
            "Brand",           "Hannah Naemi",
            "Braun",          "Jasmin Manita",
            "Braun",          "Chiara Emilia",
           "Brecht",                  "Julia",
             "Buch",                "Corinna",
           "Crisan",                  "Nelia",
            "Damar",                    "Eda",
          "Diebold",          "Larissa Sinja",
             "Dorn",                  "Colin",
          "Fischer",          "Julie Kristin",
  "Fischer-Rasokat",                "Antonia",
     "Fortenbacher",           "Lara Estelle",
            "Frank",        "Sarah Katharina",
          "Fritsch",            "Judith Anna",
            "Fritz",                  "Hanna",
             "Funk",         "Marie-Dorothée",
         "Gehrlein",             "Nele Marie",
           "Gelies",            "Luisa Maria",
          "Gläsner",              "Nele Gwen",
          "Graeber",                 "Daniel",
         "Grubisic",               "Casandra",
              "Gül",                 "Dilara",
             "Haas",                  "Jonas",
         "Hartmann",                 "Isabel",
          "Hegedüs",                  "Liana",
             "Heid",         "Lorena Despina",
           "Herzog",  "Felizitas Viva Andrea",
            "Hesse",                 "Tamara",
           "Hilger",                   "Leon",
             "Hill", "Vera Claudia Christina",
           "Hladik",       "Aaron Constantin",
             "Höll",            "Laura Maria",
          "Hüttler",           "Marie Sophie",
        "Juraschek",                    "Tom",
         "Juretzko",           "Lina Kristin",
            "Kalac",               "Anamaria",
          "Karacay",                   "Liva",
          "Karaman",                 "Shirin",
           "Keitel",                 "Melina",
           "Keller",                  "Silas",
             "Kerl",                    "Lea",
          "Kleiner",      "Christian Andreas",
            "Kloos",           "Monja Sophie",
          "Kneifel",            "Anna-Sophie",
          "Knütter",                 "Martha",
           "Kobets",                   "Jana",
            "Korte",                 "Aileen",
            "Koser",               "Josefine",
           "Koster",        "Selina Fabienne",
             "Kowa",              "Maya Nina",
           "Kremer",                  "Karla",
             "Krey",                 "Jolien",
           "Krüger",                  "Laura",
             "Kunz",             "Tabea Lina",
         "Lojowski",                    "Max",
            "Mesch",                  "Alina",
          "Miketta",                    "Lea",
             "Mörk",         "Liliane Coline",
           "Müller",            "Teresa Lara",
           "Müller",           "Laurine Anna",
            "Nagel",             "Jona Kevin",
          "Nehring",            "Rose Elewat",
          "Neubert",                 "Janina",
            "Neutz",                  "Laura",
           "Öztürk",                  "Beyza",
           "Öztürk",             "Elif Rabia",
         "Pehlivan",                 "Jasmin",
         "Pertschy",           "Elisa Sophie",
           "Peters",                   "Nele",
         "Peuthert",     "Cara-Cae Hannelore",
        "Pierpaoli",            "Sofia Elena",
           "Piluso",          "Chiara Morena",
         "Pospiech",            "Noel Johann",
          "Püschel",         "Stella Malaika",
          "Riedlin",                   "Jana",
             "Roth",                 "Selina",
    "Rothenhäusler",                  "Ruben",
        "Rothermel",       "Katharina Mirjam",
            "Sahin",                  "Filiz",
          "Schäfer",            "Judith Anna",
          "Schäfer",                  "Alina",
           "Scheck",  "Luzie Elisabeth Heidi",
        "Schilling",              "Anna-Lena",
           "Schler",           "Mina Kristin",
        "Schneider",                 "Hannah",
        "Schneider",            "Anna Louise",
          "Schwarz",           "Paula Marlen",
          "Schwing",                   "Jana",
       "Shevchenko",                 "Alyona",
         "Simmerer",                 "Fabian",
           "Sohmer",                   "Yara",
          "Stremel",                  "Alina",
            "Trapp",      "Vanessa Magdalena",
            "Turan",                "Nilüfer",
     "van den Berg",                   "Sara",
      "von Klaeden",           "Leonie Julia",
        "Waldhauer",          "Prisca Ramona",
    "Walterspacher",                  "Laura",
           "Wander",        "Sophie-Beatrice",
             "Weiß",                "Jessica",
     "Wintermantel",           "Julia Leonie",
          "Wöllner",                   "Lina",
           "Zoller",                  "Hanna"
  )



data <- data %>% 
  mutate(studi = paste(Vorname, Nachname, sep = " "))

data$perzentil <-  sample(49:70, nrow(data), replace = TRUE)
data$verlauf  <-  sample(1:19,  nrow(data), replace = TRUE)

data <- data |> 
  dplyr::mutate(
    output_format = "docx",       # Output format (html, word, etc.)
    output_file = paste(          # Output file name
      "Studienleistung ",
      studi, 
      ".docx",
      sep = "")
    )


for (i in 1:nrow(df)) {
    
  data_i <- data[i,]
    
  quarto::quarto_render(
    input = "Studienleistungen/Studienleistung_template.qmd",
    output_file = paste0("Studienleistung_", data_i$studi, ".docx"),
    execute_params = list(
      studi = data_i$studi,
      perzentil = data_i$perzentil,
      verlauf = data_i$verlauf
    )
  )
}
