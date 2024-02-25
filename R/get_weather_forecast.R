# Déclaration des variables globales pour éviter les avertissements lors de la vérification du package
utils::globalVariables(c("date_heure", "temperature_celsius", "temperature_ressentie"))

#' Effectuer une Requête à l'API Météo
#'
#' Cette fonction envoie une requête à l'API Open-Meteo en utilisant des coordonnées GPS spécifiques
#' et retourne des données météorologiques horaires sous forme de tibble.
#'
#' @param latitude La latitude de l'emplacement pour lequel obtenir les prévisions.
#' @param longitude La longitude de l'emplacement pour lequel obtenir les prévisions.
#'
#' @importFrom httr2 request req_url_query req_perform resp_body_json
#' @importFrom tibble as_tibble
#'
#' @return Un tibble contenant les données météorologiques horaires pour l'emplacement spécifié.
#' @export
#'
#' @examples
#'
#' perform_request(latitude = 48.85, longitude = 2.35)
perform_request <- function(latitude, longitude) {
  # API Open-Meteo
  url <- "https://api.open-meteo.com/v1/forecast"
  requete <- request(url) |>
    req_url_query(
      latitude = latitude,
       longitude = longitude,
      hourly = c("temperature_2m", "apparent_temperature", "precipitation_probability", "precipitation"),
      .multi = "comma") |>
    req_perform() |>
    resp_body_json()
    meteo_data <- as_tibble(requete) # Conversion en tibble
    return(meteo_data)
}

#' Transformer les Données Météo en Format Tibble
#'
#' Cette fonction prend un ensemble de données météorologiques horaires et les transforme
#' en un tibble plus lisible, en extrayant des informations spécifiques comme la date, l'heure,
#' la température, et la probabilité de précipitation.
#'
#' @param meteo_data Un tibble contenant les données brutes de l'API météorologique.
#' @importFrom tibble tibble
#'
#' @return Un tibble structuré avec les colonnes pour la date, l'heure, la température,
#'         la température ressentie, la probabilité de précipitation et les précipitations.
#' @export
#'
#' @examples
#' perform_request(lat = 47.21725, long = -1.55336) |> unnest_data()
unnest_data <- function(meteo_data) {
  # Extraction des données horaires et conversion en tibble
  meteo_tibble <- tibble(
    date_heure = as.POSIXct(unlist(meteo_data$hourly$time), format = "%Y-%m-%dT%H:%M", tz = "UTC"),  #POSIXct avec le fuseau horaire UTC pour assurer que les heures sont correctement interprétées
    temperature_celsius = unlist(meteo_data$hourly$temperature_2m),
    temperature_ressentie = unlist(meteo_data$hourly$apparent_temperature),
    precipitation_proba = unlist(meteo_data$hourly$precipitation_probability),
    precipitation_mm = unlist(meteo_data$hourly$precipitation)
  )
  return(meteo_tibble)
}


#' Obtenir des Prévisions Météorologiques
#'
#' Cette fonction générique renvoie des prévisions météorologiques basées sur la localisation fournie.
#' La localisation peut être fournie sous forme de coordonnées GPS (vecteur numérique) ou d'une adresse (chaîne de caractères).
#'
#' @usage get_forecast(location)
#' @param location Soit un vecteur numérique de taille 2 contenant les coordonnées GPS (latitude, longitude),
#'                 soit une chaîne de caractères représentant une adresse.
#' @return Un tibble contenant les prévisions météorologiques pour la localisation spécifiée.
#' @export
#' @examples
#' get_forecast(c(48.85, 2.35)) # Coordonnées GPS de Paris
#' get_forecast("Eiffel Tower, Paris, France") # Adresse textuelle
get_forecast <- function(location) {
  UseMethod("get_forecast")
}

#' Convertir une Adresse en Coordonnées GPS
#'
#' Cette fonction convertit une adresse textuelle en coordonnées GPS à l'aide du service de géocodage OpenStreetMap.
#'
#' @param location L'adresse à convertir en coordonnées GPS.
#' @importFrom tidygeocoder geocode
#' @return Un vecteur numérique de taille 2 contenant la latitude et la longitude de l'adresse.
#' @export
#' @examples
#' address_to_gps("Eiffel Tower, Paris, France")
address_to_gps <- function(location) {
  # dataframe temporaire pour utilisation de geocode
  df_temp <- data.frame(address = location, stringsAsFactors = FALSE)

  # Appel à geocode avec le dataframe temporaire
  result <- geocode(
    .tbl = df_temp,
    address = "address",
    method = 'osm',
    limit = 1
  )
  # Vérifie si des coordonnées ont été trouvées
    if(nrow(result) > 0 && !is.na(result$lat[1]) && !is.na(result$long[1])) {
    return(c(result$lat[1], result$long[1]))
  } else {
    # Retourne une erreur si aucune coordonnée n'a été trouvée
    stop("Aucune coordonnée GPS trouvée pour l'adresse fournie.")
  }
}


#' Obtenir les Coordonnées GPS pour une Adresse
#'
#' Fonction enveloppe appelant `address_to_gps` pour obtenir les coordonnées GPS d'une adresse donnée.
#'
#' @param location L'adresse pour laquelle les coordonnées GPS sont demandées.
#' @return Un vecteur numérique contenant la latitude et la longitude de l'adresse.
#' @export
#' @examples
#' get_gps_coordinate("Eiffel Tower, Paris, France")
get_gps_coordinate <- function(location) {
  return(address_to_gps(location))
}


#' Obtenir des Prévisions Météorologiques pour des Coordonnées GPS
#'
#' Cette fonction renvoie des prévisions météorologiques pour un ensemble de coordonnées GPS spécifié.
#'
#' @param location Un vecteur numérique de taille 2 contenant les coordonnées GPS (latitude, longitude).
#' @return Un tibble contenant les prévisions météorologiques pour les coordonnées fournies.
#' @export
get_forecast.numeric <- function(location) {
  stopifnot(length(location) == 2, is.numeric(location)) # verifie que le vecteur est bien de taille 2
  latitude <-location [1]
  longitude <- location [2]
  meteo_forecast <- perform_request(latitude, longitude)
  weather_forecast <- unnest_data(meteo_forecast)
  visualiser_temperatures(weather_forecast)
  return(weather_forecast)
}


#' Obtenir des Prévisions Météorologiques pour une Adresse
#'
#' Cette fonction renvoie des prévisions météorologiques pour une adresse textuelle spécifiée.
#'
#' @param location Une chaîne de caractères représentant une adresse.
#' @return Un tibble contenant les prévisions météorologiques pour l'adresse fournie.
#' @export
get_forecast.character <- function(location) {
  gps_coords <- address_to_gps(location)
  weather_forecast <- get_forecast.numeric(gps_coords)
  visualiser_temperatures(weather_forecast)
  return(weather_forecast)
}



#' Visualiser les Températures avec Interactivité Plotly
#'
#' Cette fonction génère un graphique interactif affichant la température et la température ressentie
#' au fil du temps. Elle utilise `ggplot2` pour créer le graphique initial puis le convertit
#' en un graphique interactif avec `plotly`.
#'
#' @param weather_forecast Tibble contenant les données météorologiques à visualiser.
#'     Le Tibble ou data frame doit contenir au moins les colonnes `date_heure` pour les dates et heures des prévisions,
#'     `temperature_celsius` pour les températures en degrés Celsius, et `temperature_ressentie`
#'     pour les températures ressenties, également en degrés Celsius.
#'
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual theme_minimal labs
#' @importFrom plotly ggplotly
#'
#' @return Un graphique interactif `plotly` est affiché.
#'
#' @examples
#' # Supposer que weather_data est un data frame contenant
#' # les colonnes date_heure, temperature_celsius et temperature_ressentie
#' # weather_data <- data.frame(
#' #   date_heure = seq(as.POSIXct("2022-01-01"), by = "hour", length.out = 24),
#' #   temperature_celsius = rnorm(24, mean = 15, sd = 5),
#' #   temperature_ressentie = rnorm(24, mean = 15, sd = 5)
#' # )
#' # visualiser_temperatures(weather_data)
#'
#' @export
visualiser_temperatures <- function(weather_forecast) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Le package plotly est nécessaire pour cette fonction.")
  }

  gg <- ggplot2::ggplot(weather_forecast, ggplot2::aes(x = date_heure)) +
    ggplot2::geom_line(ggplot2::aes(y = temperature_celsius, color = "Température"), size = 0.6) +
    ggplot2::geom_line(ggplot2::aes(y = temperature_ressentie, color = "Température ressentie"), size = 0.6) +
    ggplot2::scale_color_manual(values = c("Température" = "blue", "Température ressentie" = "red")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Température et Température ressentie au fil du temps",
      x = "Heure",
      y = "Température (°C)",
      color = "Légende"
    )

  print(plotly::ggplotly(gg))
}


get_forecast(c(47.21, 1.55))

