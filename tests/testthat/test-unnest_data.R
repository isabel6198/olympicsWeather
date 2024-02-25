# jeu de données crée pour tester la fonction, avec la meme structure que celle recu par perfom_request

test_data <- tibble(
  latitude = rep(47.2, 5),
  longitude = rep(-1.55, 5),
  generationtime_ms = rep(0.226, 5),
  utc_offset_seconds = rep(0, 5),
  timezone = rep("GMT", 5),
  timezone_abbreviation = rep("GMT", 5),
  elevation = rep(10, 5),
  hourly_units = list(list(temperature_2m = "°C")),
  hourly = I(list(
    time = list(seq(from = as.POSIXct("2022-01-01 00:00:00", tz = "UTC"),
                    to = as.POSIXct("2022-01-07 23:00:00", tz = "UTC"), by = "hour")),
    temperature_2m = list(runif(168, min = -5, max = 15)),
    apparent_temperature = list(runif(168, min = -10, max = 15)),
    precipitation_probability = list(runif(168, min = 0, max = 100)),
    precipitation = list(runif(168, min = 0, max = 20))
  ))
)


# ---------------------------------------------------------

test_that("unnest_data transforme et structure correctement les données d'entrée", {
  # Utilisez `test_data` comme entrée pour `unnest_data`
  result <- unnest_data(test_data)

  # Bloc 1: Vérification des noms de colonnes
  test_that("Les noms de colonnes sont corrects", {
    expected_columns <- c("date_heure", "temperature_celsius", "temperature_ressentie", "precipitation_proba", "precipitation_mm")
    expect_equal(names(result), expected_columns, info = "Les noms des colonnes devraient correspondre exactement à ceux attendus.")
  })

  # Bloc 2: Vérification du nombre de lignes pour une prévision de 7 jours
  test_that("Le nombre de lignes correspond à une prévision de 7 jours", {
    # Supposez que chaque "heure" dans les données mockées représente une heure pendant 7 jours
    expect_equal(nrow(result), 168, info = "Le résultat devrait contenir 168 lignes pour une prévision météorologique de 7 jours.")
  })

  # Bloc 3: Vérification du type de données pour la colonne date_heure
  test_that("Le type de données pour date_heure est POSIXct", {
    expect_true(inherits(result$date_heure, "POSIXct"), info = "La colonne date_heure devrait être de type POSIXct pour représenter correctement les dates et heures.")
  })

})


