#' Maps of media records
#'
#' `map_locations` creates maps to visualize the geographic spread of
#' media records.
#' @inheritParams template_params
#' @param cluster Logical to control if icons are clustered by locality.
#' Default is `FALSE`.
#' @param marker_color Character vector  with the color(s) to be used for the
#' markers. Possible values are "red", "darkred", "lightred", "orange",
#' "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue",
#' "lightblue", "purple", "darkpurple", "pink", "cadetblue", "white", "gray",
#' "lightgray", "black". By default it "orange". Can be used to indicate the
#' levels of a character or factor column with the argument "by". In such
#' a case users must supplied as many colors as levels in the column.
#' @param by Name of column to be used for coloring markers. Default is
#' "species".
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
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}) and Grace
#' Smith Vidaurre

map_locations <- function(
  metadata,
  cluster = FALSE,
  marker_color = "orange",
  by = "species"
) {

  ##  argument checking
  check_results <- .check_arguments(
    fun = "map_locations",
    args = list(
     metadata = metadata,
     cluster = cluster,
     marker_color = marker_color,
     by = by
    )
  )

  .report_assertions(check_results)


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

  # make map

  # if only one species use subspecies for color marker
  # labels for hovering
    metadata$labels <- metadata[, by, drop = TRUE]

  # if only 1 color supplied
  if (length(marker_color) == 1){
    marker_color <- rep(marker_color, length(unique(metadata$labels)))
  }

  # if less colors than levels recycle
  if (length(marker_color) < length(unique(metadata$labels))){
    marker_color <- rep(marker_color, length(unique(metadata$labels)))
  }

  # color for marker
  marker_cols <- marker_color[as.numeric(as.factor(metadata$labels))]

  # use ios icons with marker colors
  icons <- leaflet::awesomeIcons(
    icon = "ios-close",
    iconColor = "black",
    library = "ion",
    markerColor = marker_cols
  )

  # set listen link for Xeno-Canto
  metadata$file_url <- ifelse(
    metadata$repository == "Xeno-Canto",
    paste0(
      "https://xeno-canto.org/",
      metadata$key,
      "/embed?simple=1"
    ),
    metadata$file_url
  )


  # make content for popup
  media_html <- .make_media_html(metadata)

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

  # make base map
  leaf_map <- leaflet::leaflet(metadata)

  # add tiles
  leaf_map <- leaflet::addTiles(leaf_map)

  # add markers
  if (cluster) {
    leaf_map <- leaflet::addAwesomeMarkers(
      map = leaf_map,
      ~longitude,
      ~latitude,
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
      ~longitude,
      ~latitude,
      icon = icons,
      label = ~labels,
      popup = content,
      data = metadata
    )
  }

  # add minimap view at bottom right
  leaf_map <- leaflet::addMiniMap(leaf_map)

  # add zoom-out button
  leaf_map <- leaflet::addEasyButton(
    leaf_map,
    leaflet::easyButton(
      icon = "fa-globe",
      title = "Zoom to full view",
      onClick = leaflet::JS("function(btn, map){ map.setZoom(1); }")
    )
  )

  if (cluster) {
    leaf_map <- leaflet::addEasyButton(
      leaf_map,
      leaflet::easyButton(
        states = list(
          leaflet::easyButtonState(
            stateName = "unfrozen-markers",
            icon = "ion-toggle",
            title = "Freeze Clusters",
            onClick = leaflet::JS(
              "
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'rec.cluster');
            clusterManager.freezeAtZoom();
            btn.state('frozen-markers');
          }"
            )
          ),
          leaflet::easyButtonState(
            stateName = "frozen-markers",
            icon = "ion-toggle-filled",
            title = "UnFreeze Clusters",
            onClick = leaflet::JS(
              "
          function(btn, map) {
            var clusterManager =
              map.layerManager.getLayer('cluster', 'rec.cluster');
            clusterManager.unfreeze();
            btn.state('unfrozen-markers');
          }"
            )
          )
        )
      )
    )
  }

  # let users know that some observations were not
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

  # plot map
  return(leaf_map)
}

