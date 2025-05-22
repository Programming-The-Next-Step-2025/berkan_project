# Additional Packages -----------------------------------------------------------
#' @importFrom dplyr bind_rows filter select
#' @importFrom rgbif occ_search
#' @importFrom leaflet leaflet addTiles addMarkers

# ------------------------------------------------------------------------------
# FUNCTION 1: get_gbif_occurrences_by_month
# ------------------------------------------------------------------------------

#' @title Get GBIF Occurrences for a Plant Filtered by Month
#'
#' @description
#' Downloads occurrence data for a given plant species from GBIF (Global Biodiversity Information Facility),
#' filtered by observation month(s) and country (Netherlands).
#'
#' @param scientific_name Scientific name of the plant (e.g., "Allium ursinum").
#' @param months A character vector of month names (e.g., c("March", "April")).
#' @param limit Maximum number of GBIF records to retrieve (default is 1000).
#'
#' @details
#' Only occurrences with valid coordinates and observation dates are returned.
#' The function extracts the observation month from the event date, then filters accordingly.
#'
#' @return A data frame of occurrences with columns: scientificName, month, decimalLatitude, decimalLongitude.
#'
#' @examples
#' # Get wild garlic locations for March and April
#' wild_garlic_locations <- get_gbif_occurrences_by_month("Allium ursinum", c("March", "April"))
#'
#' # Visualize on a Leaflet map
#' if (nrow(wild_garlic_locations) > 0) {
#'   leaflet::leaflet(wild_garlic_locations) %>%
#'     leaflet::addTiles() %>%
#'     leaflet::addMarkers(~decimalLongitude, ~decimalLatitude)
#' }
#'
#' @export
get_gbif_occurrences_by_month <- function(scientific_name, months, limit = 1000) {
  results <- rgbif::occ_search(
    scientificName = scientific_name,
    country = "NL",
    limit = limit
  )
  df <- results$data

  # Remove incomplete records
  df <- dplyr::filter(df, !is.na(decimalLatitude), !is.na(decimalLongitude), !is.na(eventDate))

  # Parse month
  df$eventDate <- as.Date(df$eventDate)
  df$month <- format(df$eventDate, "%B")

  # Filter by month
  df_filtered <- dplyr::filter(df, month %in% months)

  if (nrow(df_filtered) == 0) {
    message("No matching GBIF records found for those months.")
    return(data.frame(
      scientificName = character(),
      month = character(),
      decimalLatitude = numeric(),
      decimalLongitude = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  dplyr::select(df_filtered, scientificName, month, decimalLatitude, decimalLongitude)
}

# ------------------------------------------------------------------------------
# FUNCTION 2: add_foraging_location
# ------------------------------------------------------------------------------

#' @title Add a New Foraging Location
#'
#' @description
#' Adds a new plant observation to an existing foraging dataset. Useful for user-contributed entries in the Urban Foraging Map app.
#'
#' @param data A data frame of existing foraging locations.
#' @param plant Name of the plant found (character).
#' @param lat Latitude of the location (numeric).
#' @param lon Longitude of the location (numeric).
#' @param month The month when the plant is harvestable (e.g., "April").
#' @param notes Optional notes about the location or plant (character).
#'
#' @details
#' The new entry is appended as a row to the input data frame using `dplyr::bind_rows()`.
#'
#' @return A data frame with the new location added.
#'
#' @examples
#' df <- data.frame(
#'   plant = character(),
#'   lat = numeric(),
#'   lon = numeric(),
#'   month = character(),
#'   notes = character(),
#'   stringsAsFactors = FALSE
#' )
#' df <- add_foraging_location(df, "Wild Garlic", 52.37, 4.89, "March", "Under the bridge")
#'
#' @export
add_foraging_location <- function(data, plant, lat, lon, month, notes = "") {
  new_entry <- data.frame(
    plant = plant,
    lat = lat,
    lon = lon,
    month = month,
    notes = notes,
    stringsAsFactors = FALSE
  )
  dplyr::bind_rows(data, new_entry)
}

# remove.packages("urbanforaging")
#
# devtools::document()  # will now recreate NAMESPACE and man/ files
# devtools::install()




