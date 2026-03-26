#' Maps of media records
#'
#' `map_locations` creates maps to visualize the geographic spread of
#' media records.
#' @inheritParams template_params
#' @param cluster Logical to control if icons are clustered by locality.
#' Default is `FALSE`.
#' @param marker_color Character vector  with the color(s) to be used for the
#' markers (when \code{type = "markers"}. Possible values are "red", "darkred", "lightred", "orange",
#' "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue",
#' "lightblue", "purple", "darkpurple", "pink", "cadetblue", "white", "gray",
#' "lightgray", "black". By default it "orange". Can be used to indicate the
#' levels of a character or factor column with the argument "by". In such
#' a case users must supplied as many colors as levels in the column.
#' @param by Character string indicating the name of the column used to group
#' observations for coloring. Default is `"species"`. For `type = "circles"`,
#' this determines the color mapping and legend. For `type = "markers"`,
#' this determines how marker colors are assigned.
#' @param type Character string indicating how observations are displayed.
#' Options are `"circles"` (default) or `"markers"`. Circles use a color
#' palette and include a legend, while markers use colored icons and can be
#' clustered.
#' @param palette Function used to generate colors for circle markers when
#' `type = "circles"`. The function must take a single integer (`n`) and
#' return `n` colors. By default it uses
#' `function(n) grDevices::hcl.colors(n, "mako")`. Ignored when
#' `type = "markers"`.
#' @return An interacrive map with the locations of the observations.
#' @export
#' @name map_locations
#' @details The function uses the `leaflet` package to create interactive
#' maps for visualizing the geographic spread of observations. Note that only
#' observations with geographic coordinates are displayed. For each
#' observation the function displays a marker in the map with a popup that
#' shows the species name, country, locality, user name, and repository.
#' The popup also includes an audio player for sound recordings, an image
#' for photos and a video player for videos. Users can zoom in and out of the
#' map and click on the markers to see the popups. If `cluster = TRUE`,
#' markers that are close together will be clustered into a single marker
#' that shows the number of observations in that cluster. Users can click on
#' the cluster marker to zoom in and see the individual markers. This function
#' is useful for exploring the geographic distribution of media records and
#' identifying patterns or gaps in the data.
#' @examples \dontrun{
#' # search in xeno-canto
#' e_hochs <- query_gbif(species = "Entoloma hochstetteri", format = "image")
#'
#' # run if query didnt fail
#'  if (!is.null(e_hochs)) {
#'
#' # create map
#' map_locations(e_hochs)
#' }
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})

map_locations <- function(
    metadata,
    cluster = FALSE,
    marker_color = NULL,
    by = "species",
    type = "circles",
    palette = function(n) grDevices::hcl.colors(n, "mako")
) {

  # argument checking
  check_results <- .check_arguments(
    fun = "map_locations",
    args = list(
      metadata = metadata,
      cluster = cluster,
      marker_color = marker_color,
      by = by,
      type = type,
      palette = palette
    )
  )

  .report_assertions(check_results)

  # validate type
  if (!type %in% c("circles", "markers")) {
    stop("'type' must be either 'circles' or 'markers'")
  }

  # validate palette
  if (!is.function(palette)) {
    stop("'palette' must be a function")
  }

  # remove observations with no lat lon data
  inx_with_coors <- !is.na(metadata$latitude) |
    !is.na(metadata$longitude)

  if (all(!inx_with_coors)) {
    .message(
      "Not a single observation (row) has geographic coordinates",
      as = "failure"
    )
    return(invisible(NULL))
  }

  metadata <- metadata[inx_with_coors, , drop = FALSE]

  # labels for coloring
  metadata$labels <- metadata[, by, drop = TRUE]
  levs <- unique(metadata$labels)

  # palette handling
  if (length(levs) == 1) {

    cols <- palette(3)
    col <- cols[2]

    pal <- function(x) rep(col, length(x))

  } else {

    pal_colors <- palette(length(levs))

    pal <- leaflet::colorFactor(
      palette = pal_colors,
      levels = levs
    )
  }

  # media html
  media_html <- .make_media_html(metadata)

  # popup content
  content <- paste0(
    "<br/> <b>Repository:</b> ", metadata$repository,
    "<br/> <b>Observation:</b> <a href='", metadata$observation_url,
    "' target='_blank' rel='noopener noreferrer'>", metadata$key, "</a>",
    "<br/> <b>Species:</b> <i>", metadata$species, "</i>",
    "<br/> <b>Country:</b> ", metadata$country,
    "<br/> <b>Locality:</b> ", metadata$locality,
    "<br/> <b>User:</b> ", metadata$user_name,
    "<br/><br/>", media_html
  )

  # base map
  leaf_map <- leaflet::leaflet(metadata) |>
    leaflet::addTiles()

  # circles
  if (type == "circles") {

    leaf_map <- leaflet::addCircleMarkers(
      map = leaf_map,
      lng = ~longitude,
      lat = ~latitude,
      color = ~pal(labels),
      fillColor = ~pal(labels),
      radius = 6,
      weight = 3,
      stroke = TRUE,
      fillOpacity = 0.3,
      label = ~labels,
      popup = content,
      data = metadata
    )

  } else if (type == "markers") {

    default_marker_colors <- c(
      "red", "darkred", "lightred", "orange", "beige",
      "green", "darkgreen", "lightgreen", "blue", "darkblue",
      "lightblue", "purple", "darkpurple", "pink",
      "cadetblue", "white", "gray", "lightgray", "black"
    )

    if (is.null(marker_color)) {
      cols <- default_marker_colors
    } else {
      cols <- marker_color
    }

    cols <- rep(cols, length.out = length(levs))

    marker_cols <- cols[as.numeric(as.factor(metadata$labels))]

    icons <- leaflet::awesomeIcons(
      icon = "ios-close",
      iconColor = "black",
      library = "ion",
      markerColor = marker_cols
    )

    if (cluster) {

      leaf_map <- leaflet::addAwesomeMarkers(
        map = leaf_map,
        lng = ~longitude,
        lat = ~latitude,
        icon = icons,
        label = ~labels,
        popup = content,
        data = metadata,
        clusterOptions = leaflet::markerClusterOptions(),
        clusterId = "rec.cluster"
      )

    } else {

      leaf_map <- leaflet::addAwesomeMarkers(
        map = leaf_map,
        lng = ~longitude,
        lat = ~latitude,
        icon = icons,
        label = ~labels,
        popup = content,
        data = metadata
      )
    }
  }

  # legend only for circles and >1 level
  if (type == "circles" && !is.null(by) && length(levs) > 1) {
    leaf_map <- leaflet::addLegend(
      map = leaf_map,
      position = "bottomright",
      pal = pal,
      values = metadata$labels,
      title = by,
      opacity = 1
    )
  }

  # minimap
  leaf_map <- leaflet::addMiniMap(leaf_map)

  # zoom button
  leaf_map <- leaflet::addEasyButton(
    leaf_map,
    leaflet::easyButton(
      icon = "fa-globe",
      title = "Zoom to full view",
      onClick = leaflet::JS("function(btn, map){ map.setZoom(1); }")
    )
  )

  # warning for removed rows
  if (any(!inx_with_coors)) {
    .message(
      paste(
        "{n} observation{?s} d{?oes/o} not have geographic",
        "coordinates and w{?as/ere} ignored"
      ),
      n = sum(!inx_with_coors),
      as = "warning"
    )
  }

  return(leaf_map)
}
