#' Maps of media records
#'
#' `map_locations` creates maps to visualize the geographic spread of
#' media records.
#' @param metadata Data frame with the metadata of the media records to be
#' mapped. Typically the output of one of the query functions in this package
#' (e.g. [query_gbif()], [query_inaturalist()], etc.) or metadata formatting
#' functions (e.g. [merge_metadata()], [remove_duplicates()], etc.). Note that
#' only observations with geographic coordinates (i.e., non-missing values in
#' the `latitude` and `longitude` columns) are displayed in the map.
#' @param cluster Logical to control if icons are clustered by locality.
#' Default is `FALSE`. Only applies when `type = "markers"`. When
#' `cluster = TRUE`, markers that are close together will be clustered into a
#' single marker that shows the number of observations in that cluster. Users
#' can click on the cluster marker to zoom in and see the individual markers.
#' @param marker_color Character vector  with the color(s) to be used for the
#' markers (when \code{type = "markers"}. Possible values are "red", "darkred",
#' "lightred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue",
#' "darkblue","lightblue", "purple", "darkpurple", "pink", "cadetblue",
#' "white", "gray", "lightgray", "black". Can be used to indicate the
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
#' @param tags Character vector with the names of the columns in
#' \code{metadata} to be shown in the popup. Default is
#' \code{c("repository", "key", "species", "date", "country", "locality",
#' "user_name")}.
#' @param show_media Logical indicating whether to display media files (audio,
#' images, videos) in the popup windows. Default is `TRUE`.
#' @param popup_size Numeric value that controls the size of the popups.
#' Default is `1`. Values greater than `1` will increase the size of the
#' popups, while values between `0` and `1` will decrease it.
#' @return An  a `leaflet` interactive map object with the locations of
#' the observations.
#' @export
#' @name map_locations
#' @details The function uses the `leaflet` package to create interactive
#' maps for visualizing the geographic spread of observations. Note that only
#' observations with geographic coordinates are displayed. For each
#' observation the function displays a marker in the map with a popup that
#' shows the species name, country, locality, user name, and repository.
#' The popup also includes an audio player for sound recordings, an image
#' for photos and a video player for videos.
#'
#' When multiple media files are associated with the same observation (i.e.,
#' identical values in the \code{key} column), they are grouped into a single
#' popup. The first media item is displayed by default, and users can navigate
#' through additional media using arrow buttons within the popup.
#'
#' Users can zoom in and out of the map and click on the markers to see the
#' popups. If `cluster = TRUE`, markers that are close together will be
#' clustered into a single marker that shows the number of observations in
#' that cluster. Users can click on the cluster marker to zoom in and see
#' the individual markers. This function is useful for exploring the geographic
#' distribution of media records and identifying patterns or gaps in the data.
#'
#' Check the
#' [leaflet package documentation](https://rstudio.github.io/leaflet/index.html)
#'  and the
#'  [leaflet.extras package](https://CRAN.R-project.org/package=leaflet.extras)
#'  for more information on how to customize maps.
#' @examples
#' if(interactive()){
#' # search in xeno-canto
#' e_hochs <- query_gbif(species = "Entoloma hochstetteri", format = "image")
#'
#' # run if query didnt fail
#' if (!is.null(e_hochs)) {
#'
#'   # create map
#'   map_locations(e_hochs)
#'
#'   # create map without media
#'   map_locations(e_hochs, show_media = FALSE)
#' }
#' }
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
map_locations <- function(
  metadata,
  cluster = FALSE,
  marker_color = NULL,
  by = "species",
  type = c("circles", "markers"),
  palette = function(n) grDevices::hcl.colors(n, "mako"),
  tags = c(
    "repository",
    "key",
    "species",
    "date",
    "country",
    "locality",
    "user_name"
  ),
  show_media = TRUE,
  popup_size = 1
) {
  check_results <- .check_arguments(
    fun = "map_locations",
    args = list(
      metadata = metadata,
      cluster = cluster,
      marker_color = marker_color,
      by = by,
      type = type,
      palette = palette,
      tags = tags,
      show_media = show_media
    )
  )

  .report_assertions(check_results)

  # assign a value to type
  type <- rlang::arg_match(type, c("circles", "markers"))

  metadata <- metadata[
    !is.na(metadata$latitude) & !is.na(metadata$longitude),
    ,
    drop = FALSE
  ]

  .if_na_empty <- function(x) ifelse(is.na(x), "", x)

  metadata_list <- split(metadata, metadata$key)

  metadata$labels <- metadata[[by]]
  levs <- unique(metadata$labels)

  if (length(levs) == 1) {
    cols <- palette(3)[2]
    pal <- function(x) rep(cols, length(x))
  } else {
    pal_colors <- palette(length(levs))
    pal <- leaflet::colorFactor(pal_colors, levs)
  }

  content <- lapply(metadata_list, function(df) {
    if (show_media) {
      media_html <- unname(as.character(.make_media_html(df)))

      slides <- paste0(
        mapply(
          function(x, i) {
            paste0(
              "<div class='slide' style='display:",
              ifelse(i == 1, "block", "none"),
              ";'>",
              x,
              "</div>"
            )
          },
          media_html,
          seq_along(media_html),
          SIMPLIFY = TRUE
        ),
        collapse = ""
      )

      if (length(media_html) == 1) {
        gallery <- paste0(
          "<div class='popup-gallery' style='text-align:center;'>",
          slides,
          "</div>"
        )
      } else {
        gallery <- paste0(
          "<div class='popup-gallery' style='text-align:center;'>",
          slides,
          paste0(
            "<div style='margin-top:",
            8 * popup_size,
            "px;'>",

            "<button onclick='prevSlide(this)' style='",
            "margin-right:",
            10 * popup_size,
            "px;",
            "font-size:",
            14 * popup_size,
            "px;",
            "padding:",
            4 * popup_size,
            "px ",
            8 * popup_size,
            "px;",
            "border:none; background:none; cursor:pointer;'>&#9664;</button>",

            "<button onclick='nextSlide(this)' style='",
            "font-size:",
            14 * popup_size,
            "px;",
            "padding:",
            4 * popup_size,
            "px ",
            8 * popup_size,
            "px;",
            "border:none; background:none; cursor:pointer;'>&#9654;</button>",

            "</div>"
          ),
          "</div>"
        )
      }
    } else {
      gallery <- NULL
    }

    row <- df[1, ]

    special_tags <- list(
      repository = paste0("<br/><b>Repository:</b> ", row$repository),
      key = paste0(
        "<br/><b>Observation:</b> <a href='",
        row$observation_url,
        "' target='_blank'>",
        row$key,
        "</a>"
      ),
      species = paste0("<br/><b>Species:</b> <i>", row$species, "</i>"),
      date = paste0("<br/><b>Date:</b> ", .if_na_empty(row$date)),
      country = paste0("<br/><b>Country:</b> ", .if_na_empty(row$country)),
      locality = paste0(
        "<br/><b>Locality:</b> <span",
        " style='overflow-wrap:break-word;'>",
        .if_na_empty(row$locality),
        "</span>"
      ),
      user_name = paste0("<br/><b>User:</b> ", .if_na_empty(row$user_name))
    )

    popup_rows <- lapply(tags, function(tag) {
      if (tag %in% names(special_tags)) {
        special_tags[[tag]]
      } else {
        label <- paste0(toupper(substring(tag, 1, 1)), substring(tag, 2))
        paste0("<br/><b>", label, ":</b> ", .if_na_empty(row[[tag]]))
      }
    })

    inner_content <- if (length(tags) == 0) {
      if (show_media) gallery else ""
    } else {
      if (show_media) {
        paste0(do.call(paste0, popup_rows), "<br/><br/>", gallery)
      } else {
        paste0(do.call(paste0, popup_rows))
      }
    }

    paste0(
      "<div style='font-size:",
      100 * popup_size,
      "%;",
      "width:",
      250 * popup_size,
      "px;",
      "overflow-wrap:break-word;'>",
      inner_content,
      "</div>"
    )
  })

  content <- unname(unlist(content))

  metadata_unique <- do.call(rbind, lapply(metadata_list, function(df) df[1, ]))

  popup_data <- data.frame(
    latitude = metadata_unique$latitude,
    longitude = metadata_unique$longitude,
    popup = content
  )

  # make map
  leaf_map <- leaflet::leaflet(metadata_unique)

  # add tiles, minimap and scale bar
  leaf_map <- leaflet::addTiles(leaf_map)
  leaf_map <- leaflet::addMiniMap(leaf_map, toggleDisplay = TRUE)
  leaf_map <- leaflet::addScaleBar(leaf_map, position = "bottomleft")

  if (type == "circles") {
    # outer black ring
    leaf_map <- leaflet::addCircleMarkers(
      leaf_map,
      ~longitude,
      ~latitude,
      color = "black",
      fill = FALSE,
      radius = 7,
      weight = 1,
      fillOpacity = 0.8,
      popup = NULL
    )

    # # inner colored circle
    # leaf_map <- leaflet::addCircleMarkers(
    #   leaf_map,
    #   ~longitude,
    #   ~latitude,
    #   color = NA,
    #   fillColor = ~ pal(metadata_unique[[by]]),
    #   radius = 5, # smaller than outer
    #   weight = 0,
    #   fillOpacity = 0.7,
    #   popup = content
    # )

    leaf_map <- leaflet::addCircleMarkers(
      leaf_map,
      ~longitude,
      ~latitude,
      color = ~ pal(metadata_unique[[by]]),
      fillColor = ~ pal(metadata_unique[[by]]),
      radius = 6,
      weight = 3,
      fillOpacity = 0.3,
      popup = content
    )
  } else {
    default_colors <- c(
      "red",
      "blue",
      "green",
      "orange",
      "purple",
      "darkred",
      "cadetblue"
    )
    cols <- if (is.null(marker_color)) default_colors else marker_color
    cols <- rep(cols, length.out = length(levs))

    marker_cols <- cols[as.numeric(as.factor(metadata_unique[[by]]))]

    icons <- leaflet::awesomeIcons(
      icon = "ios-close",
      library = "ion",
      markerColor = marker_cols
    )

    leaf_map <- leaflet::addAwesomeMarkers(
      leaf_map,
      ~longitude,
      ~latitude,
      icon = icons,
      popup = content,
      clusterOptions = if (cluster) leaflet::markerClusterOptions() else NULL
    )
  }

  if (type == "circles" && length(levs) > 1) {
    leaf_map <- leaflet::addLegend(
      leaf_map,
      "bottomright",
      pal = pal,
      values = metadata_unique[[by]],
      title = by
    )
  }

  leaf_map <- htmlwidgets::onRender(
    leaf_map,
    htmlwidgets::JS(
      paste0(
        "
function(el, x) {

  var map = this;
  (function() {
    if (typeof L.Control.Fullscreen === 'undefined') {
      var cssUrl = 'https://api.mapbox.com/mapbox.js/plugins/' +
        'leaflet-fullscreen/v1.0.1/leaflet.fullscreen.css';
      var jsUrl = 'https://api.mapbox.com/mapbox.js/plugins/' +
        'leaflet-fullscreen/v1.0.1/Leaflet.fullscreen.min.js';
      var link = document.createElement('link');
      link.rel = 'stylesheet';
      link.href = cssUrl;
      document.head.appendChild(link);
      var script = document.createElement('script');
      script.src = jsUrl;
      script.onload = function() {
        map.addControl(new L.Control.Fullscreen({ position: 'topleft' }));
      };
      document.head.appendChild(script);
    } else {
      map.addControl(new L.Control.Fullscreen({ position: 'topleft' }));
    }
  })();

  var popupsVisible = false;
  var uid = 'map-' + Math.random().toString(36).substr(2, 9);

  var control = L.control({position: 'topright'});

  control.onAdd = function(map) {

    var div = L.DomUtil.create(
      'div',
      'leaflet-bar leaflet-control'
    );

    div.innerHTML =
      '<div id=\"popup-toggle-' + uid +
      '\" style=\"display:flex;align-items:center;' +
      'gap:6px;background:white;padding:6px 10px;' +
      'border-radius:6px;box-shadow:0 1px 5px ' +
      'rgba(0,0,0,0.3);font-size:12px;cursor:pointer;\">' +
      '<span>Popups</span>' +
      '<div id=\"toggle-switch-' + uid +
      '\" style=\"width:34px;height:18px;' +
      'background:#ccc;border-radius:9px;' +
      'position:relative;\">' +
      '<div id=\"toggle-knob-' + uid +
      '\" style=\"width:14px;height:14px;' +
      'background:white;border-radius:50%;' +
      'position:absolute;top:2px;left:2px;\">' +
      '</div></div></div>';

    return div;
  };

  control.addTo(map);

  function setUI(on) {
    var knob = el.querySelector('#toggle-knob-' + uid);
    var sw = el.querySelector('#toggle-switch-' + uid);
    if (!knob || !sw) return;
    knob.style.transition = '0.3s';
    if (on) {
      knob.style.left = '18px';
      sw.style.background = '#4CAF50';
    } else {
      knob.style.left = '2px';
      sw.style.background = '#ccc';
    }
  }

  function showPopups() {
    map._allPopups = [];
    var sorted = popupData.slice().sort(function(a, b) {
      return b.latitude - a.latitude;
    });
    sorted.forEach(function(d) {
      if (!isNaN(d.longitude) && !isNaN(d.latitude)) {
        var p = L.popup({autoClose:false, closeOnClick:false})
          .setLatLng([d.latitude, d.longitude])
          .setContent(d.popup)
          .addTo(map);
        map._allPopups.push(p);
      }
    });
  }

  function hidePopups() {
    if (map._allPopups) {
      map._allPopups.forEach(function(p) {
        map.removeLayer(p);
      });
      map._allPopups = [];
    }
  }

  var popupData = ",
        jsonlite::toJSON(popup_data, auto_unbox = TRUE),
        ";

  setUI(false);

  var toggle = el.querySelector('#popup-toggle-' + uid);

  if (toggle) {
    toggle.onclick = function() {
      if (!popupsVisible) {
        showPopups();
        popupsVisible = true;
        setUI(true);
      } else {
        hidePopups();
        popupsVisible = false;
        setUI(false);
      }
    };
  }

  window.nextSlide = function(btn) {
    var gallery = btn.parentElement.parentElement;
    var slides = gallery.getElementsByClassName('slide');
    for (var i = 0; i < slides.length; i++) {
      if (slides[i].style.display === 'block') {
        slides[i].style.display = 'none';
        slides[(i + 1) % slides.length].style.display = 'block';
        return;
      }
    }
    slides[0].style.display = 'block';
  };

  window.prevSlide = function(btn) {
    var gallery = btn.parentElement.parentElement;
    var slides = gallery.getElementsByClassName('slide');
    for (var i = 0; i < slides.length; i++) {
      if (slides[i].style.display === 'block') {
        slides[i].style.display = 'none';
        slides[(i - 1 + slides.length) % slides.length]
          .style.display = 'block';
        return;
      }
    }
    slides[0].style.display = 'block';
  };

}
"
      )
    )
  )

  leaf_map
}
