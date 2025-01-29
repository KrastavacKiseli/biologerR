#' @import httr
#' @import jsonlite
#' @import dplyr
#' @importFrom readr write_csv locale
NULL

#' URL Parameter Enforcement
#' @keywords internal
enforce_per_page <- function(url, per_page = 1000) {
  if (is.null(url)) return(NULL)
  parsed <- httr::parse_url(url)
  parsed$query$per_page <- per_page
  httr::build_url(parsed)
}

#' Get Biologer API Access Token
#' @keywords internal
get_biologer_token <- function() {
  username <- Sys.getenv("BIOLOGER_USER", "gojak2009@hotmail.com")
  password <- Sys.getenv("BIOLOGER_PASS", "Liverpool12345")

  response <- httr::POST(
    "https://biologer.hr/oauth/token",
    body = list(
      grant_type = "password",
      client_id = "4",
      client_secret = "XWc2QlMh7WV4BqbNtpAXyTTOhU7GbtixLmupKOxz",
      scope = "*",
      username = username,
      password = password
    ),
    encode = "form",
    httr::timeout(30)
  )

  if (httr::status_code(response) != 200) {
    stop("Authentication failed: ", httr::content(response, "text"))
  }

  httr::content(response)$access_token
}

#' Fetch Paginated Observations
#' @keywords internal
fetch_observations <- function(access_token) {
  all_obs <- list()
  url <- enforce_per_page("https://biologer.hr/api/public-field-observations")
  total_records <- NULL
  cumulative <- 0

  while (!is.null(url)) {
    for (i in 1:3) {
      response <- tryCatch({
        httr::GET(
          url,
          httr::add_headers(
            Authorization = paste("Bearer", access_token),
            Accept = "application/json"
          ),
          httr::timeout(30)
        )
      }, error = function(e) NULL)

      if (!is.null(response)) break
      Sys.sleep(10)
    }

    if (is.null(response) || httr::status_code(response) != 200) {
      warning("Failed to fetch page after retries")
      break
    }

    if (httr::status_code(response) == 429) {
      Sys.sleep(15)
      next
    }

    # Suppress JSON encoding message
    data <- suppressMessages(
      jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)
    )

    # Get total records from first response
    if (is.null(total_records)) {
      total_records <- tryCatch(
        as.integer(data$meta$total),
        error = function(e) NULL
      )
      if (!is.null(total_records)) {
        message("Total records to fetch: ", format(total_records, big.mark = ","))
      }
    }

    # Update progress
    current_count <- nrow(data$data)
    cumulative <- cumulative + current_count
    if (!is.null(total_records)) {
      progress <- paste0(
        format(cumulative, big.mark = ","), "/",
        format(total_records, big.mark = ","),
        " (", round(cumulative / total_records * 100, 1), "%)"
      )
    } else {
      progress <- format(cumulative, big.mark = ",")
    }

    message("Fetched page with ", current_count, " records (Total: ", progress, ")")

    all_obs <- c(all_obs, list(data$data))
    url <- enforce_per_page(data$links$`next`)
    Sys.sleep(0.5)
  }

  dplyr::bind_rows(all_obs)
}

#' Main Export Function
#' @export
get_biologer_data <- function(save_path = "biologer_data.csv") {
  token <- get_biologer_token()
  df <- fetch_observations(token)

  if (!is.null(df) && nrow(df) > 0) {
    readr::write_excel_csv(df, save_path)
    message("\nSaved ", format(nrow(df), big.mark = ","),
            " observations to ", save_path)
    invisible(df)
  } else {
    message("No data retrieved")
    invisible(NULL)
  }
}


#' Open Biologer Data
#'
#' Opens the Biologer data CSV file as a dataframe.
#'
#' @param path A character string specifying the path to the CSV file.
#'             If NULL, defaults to the path used in `get_biologer_data()`.
#' @param verbose Logical, whether to display messages about data loading. Defaults to TRUE.
#' @return A dataframe containing the Biologer data.
#' @export
open.data <- function(path = "biologer_data.csv", verbose = TRUE) {
  # Check if the file exists
  if (!file.exists(path)) {
    stop("The file does not exist at the specified path: ", path)
  }

  # Read the CSV into a dataframe
  df <- tryCatch({
    readr::read_csv(
      path,
      locale = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE # Suppress column spec messages
    )
  }, error = function(e) {
    stop("Failed to read the file: ", e$message)
  })

  # Log success message if verbose is TRUE
  if (verbose) {
    message("Data successfully loaded from: ", path)
    message("Rows: ", nrow(df), ", Columns: ", ncol(df))
  }

  return(df)
}



#' Subset Biologer Data
#'
#' Subset a Biologer dataframe based on specified column-value conditions.
#'
#' @param df A dataframe to subset. Defaults to the result of `open.data()` if NULL.
#' @param ... Named arguments specifying column names and the values to subset by.
#'            For example: `sex = "value"`, `taxon.name = "value"`, or
#'            `taxon.id = c("value1", "value2")`.
#' @return A subsetted dataframe.
#' @export



subset.biologer <- function(df = NULL, ...) {
  # Load default dataset if df is not supplied
  if (is.null(df)) {
    df <- open.data(verbose = FALSE) # Suppress messages when auto-loading
  }

  # Convert variable conditions into a list
  conditions <- list(...)

  # Ensure all provided column names exist in the dataframe
  invalid_cols <- setdiff(names(conditions), names(df))
  if (length(invalid_cols) > 0) {
    stop("Invalid column names: ", paste(invalid_cols, collapse = ", "))
  }

  # Dynamically build filtering logic
  subset_df <- df
  for (col in names(conditions)) {
    values <- conditions[[col]]
    if (length(values) > 1) {
      # Use `%in%` for multiple values
      subset_df <- subset_df[subset_df[[col]] %in% values, ]
    } else {
      # Use `==` for single value
      subset_df <- subset_df[subset_df[[col]] == values, ]
    }
  }

  # Return the subsetted dataframe
  return(subset_df)
}



#############################################################

#' Plot RH_2018 Shapefile
#'
#' Loads and plots the RH_2018 shapefile included in the package.
#'
#' @return A ggplot object showing the RH_2018 map.
#' @export
plot.hr <- function() {
  library(sf)
  library(ggplot2)

  # Locate and load the shapefile
  shapefile_path <- system.file("extdata", "RH_2018.shp", package = "biologerR")
  if (shapefile_path == "") stop("Shapefile not found in the package.")

  rh_data <- sf::st_read(shapefile_path, quiet = TRUE)

  # Create the plot
  ggplot(data = rh_data) +
    geom_sf(fill = "lightblue", color = "black") +
    theme_minimal()
}


#' Plot Zupanije JSON
#'
#' Loads and plots the Zupanije.json file included in the package.
#'
#' @return A ggplot object showing the Zupanije map.
#' @export
plot.zupanija <- function() {
  library(sf)
  library(ggplot2)

  # Locate the GeoJSON file
  json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
  if (json_path == "") stop("GeoJSON file not found in the package.")

  # Read the GeoJSON
  zupanije_sf <- sf::st_read(json_path, quiet = TRUE)

  # Add region centroids for labeling
  zupanije_sf$centroid <- sf::st_centroid(zupanije_sf$geometry)
  centroids <- sf::st_coordinates(zupanije_sf$centroid)
  zupanije_sf$lon <- centroids[, 1]
  zupanije_sf$lat <- centroids[, 2]

  # Plot with labels
  ggplot(data = zupanije_sf) +
    geom_sf(fill = "lightgreen", color = "darkgreen") +
    geom_text(aes(x = lon, y = lat, label = name), size = 2, color = "blue") +
    theme_minimal()
}


#' Plot Biologer Observations on a Map
#'
#' Plots latitude and longitude of observations from the Biologer dataframe (`df`) on either the RH_2018 or Zupanije map.
#'
#' @param fill Column name to use for coloring points. Default is "taxon.name".
#' @param shape Column name to use for point shapes. Default is NULL (no shapes).
#' @param layer Map layer to use: "rh" for RH_2018.shp or "zupanije" for Zupanije.json.
#' @param df A dataframe containing Biologer data with latitude and longitude columns.
#' @param title Plot title. Default is NULL (no title).
#' @param x_axis_label X-axis label. Default is NULL (no label).
#' @param y_axis_label Y-axis label. Default is NULL (no label).
#' @param legend_fill_title Title for the fill legend. Default is NULL (no title).
#' @param legend_shape_title Title for the shape legend. Default is NULL (no title).
#' @param dot_size Size of the points. Default is 2.
#' @param layer_fill_color Fill color for the map layer. Default is "lightblue".
#' @param layer_border_color Border color for the map layer. Default is "black".
#' @param layer_border_width Border width for the map layer. Default is 0.5.
#' @param palette Color palette for points. Can be a predefined name or a custom named vector.
#'
#' @return A ggplot object showing observations on the selected map layer.
#' @export
plot.biologer <- function(fill = "taxon.name", shape = NULL, layer = "rh", df,
                          title = NULL, x_axis_label = NULL, y_axis_label = NULL,
                          legend_fill_title = NULL, legend_shape_title = NULL,
                          dot_size = 2,
                          layer_fill_color = "lightblue",
                          layer_border_color = "black",
                          layer_border_width = 0.5,
                          palette = "viridis") {
  library(sf)
  library(ggplot2)
  library(scales)

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # Load the selected map layer
  map_layer <- NULL
  if (layer == "rh") {
    shapefile_path <- system.file("extdata", "RH_2018.shp", package = "biologerR")
    if (shapefile_path == "") stop("RH_2018 shapefile not found in the package.")
    map_layer <- sf::st_read(shapefile_path, quiet = TRUE)
  } else if (layer == "zupanije") {
    json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
    if (json_path == "") stop("Zupanije GeoJSON file not found in the package.")
    map_layer <- sf::st_read(json_path, quiet = TRUE)
  } else {
    stop("Invalid layer specified. Use 'rh' for RH_2018 or 'zupanije'.")
  }

  # Convert the Biologer data to an sf object
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Transform Biologer data to match the CRS of the map layer
  df_sf <- sf::st_transform(df_sf, crs = sf::st_crs(map_layer))

  # Generate a dynamic palette if needed
  unique_fill_values <- unique(df_sf[[fill]])
  if (is.character(palette) && !is.null(names(palette))) {
    # Custom palette provided
    if (!all(unique_fill_values %in% names(palette))) {
      missing_values <- setdiff(unique_fill_values, names(palette))
      stop("The custom palette is missing colors for: ", paste(missing_values, collapse = ", "))
    }
    colors <- palette
  } else if (is.character(palette) && length(palette) == 1) {
    # Predefined palette
    color_count <- length(unique_fill_values)
    if (palette == "viridis") {
      colors <- viridis::viridis(color_count)
    } else if (palette == "plasma") {
      colors <- viridis::plasma(color_count)
    } else if (palette == "rainbow") {
      colors <- rainbow(color_count)
    } else {
      colors <- scales::hue_pal()(color_count)
    }
    names(colors) <- unique_fill_values
  } else {
    stop("Invalid palette. Use a predefined palette name or a named list of colors.")
  }

  # Build the ggplot
  p <- ggplot() +
    geom_sf(data = map_layer, fill = layer_fill_color, color = layer_border_color, size = layer_border_width) +
    geom_sf(
      data = df_sf,
      aes_string(color = fill, shape = shape),
      size = dot_size
    ) +
    scale_color_manual(values = colors) +
    theme_minimal()

  # Add custom title and axis labels
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  if (!is.null(x_axis_label)) {
    p <- p + xlab(x_axis_label)
  }
  if (!is.null(y_axis_label)) {
    p <- p + ylab(y_axis_label)
  }

  # Customize the legends if titles are provided
  if (!is.null(legend_fill_title)) {
    p <- p + scale_color_manual(name = legend_fill_title, values = colors)
  }
  if (!is.null(legend_shape_title)) {
    p <- p + scale_shape_discrete(name = legend_shape_title)
  }

  return(p)
}


#' Count Observations Per Zupanija
#'
#' Counts the number of observations per Zupanija based on the coordinates of rows in the dataframe.
#'
#' @param fill Column name to group and count rows by. Default is "taxon.name".
#' @param df A dataframe containing latitude and longitude columns.
#' @param selected_zupanije Optional vector of županija names to filter by. Default is NULL (all županije).
#'
#' @return A dataframe with counts for each Zupanija and a TOTAL row.
#' @export
zupanije.biologer <- function(fill = "taxon.name", df, selected_zupanije = NULL) {
  library(sf)
  library(dplyr)

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # Load Zupanije map layer
  json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
  if (json_path == "") stop("Zupanije GeoJSON file not found in the package.")
  zupanije_sf <- sf::st_read(json_path, quiet = TRUE)

  # Filter županije if specified
  if (!is.null(selected_zupanije)) {
    if (!all(selected_zupanije %in% zupanije_sf$name)) {
      invalid_zupanije <- selected_zupanije[!selected_zupanije %in% zupanije_sf$name]
      stop("Invalid županija names: ", paste(invalid_zupanije, collapse = ", "))
    }
    zupanije_sf <- zupanije_sf[zupanije_sf$name %in% selected_zupanije, ]
  }

  # Convert the Biologer data to an sf object
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Spatial join to find which Zupanija each point falls into
  joined_data <- sf::st_join(df_sf, zupanije_sf, join = sf::st_within)

  # Count rows per Zupanija and grouping variable
  if (fill == "none") {
    counts <- joined_data %>%
      group_by(name) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(!!fill := "ALL")  # Add a default "ALL" column when no grouping
  } else {
    counts <- joined_data %>%
      group_by(!!sym(fill), name) %>%
      summarise(count = n(), .groups = "drop")
  }

  # Add TOTAL row for each group and for the entire dataset
  total_per_group <- counts %>%
    group_by(!!sym(fill)) %>%
    summarise(name = "TOTAL", count = sum(count), .groups = "drop")

  total_overall <- counts %>%
    summarise(!!sym(fill) := "TOTAL", name = "TOTAL", count = sum(count), .groups = "drop")

  # Combine everything into a single dataframe
  final_counts <- bind_rows(counts, total_per_group, total_overall)

  # Arrange for better readability
  final_counts <- final_counts %>%
    arrange(!!sym(fill), name)

  # Print the result
  print(final_counts)
  return(final_counts)
}


#' Plot Biologer Observations for Specific Zupanije
#'
#' Plots observations within one or more Zupanije based on coordinates and user specifications.
#'
#' @param fill Column name to use for coloring points. Default is "taxon.name".
#' @param shape Column name to use for point shapes. Default is NULL (no shapes).
#' @param zupanija Vector of Zupanija names to filter and plot.
#' @param df A dataframe containing latitude and longitude columns.
#' @param title Plot title. Default is NULL (no title).
#' @param x_axis_label X-axis label. Default is NULL (no label).
#' @param y_axis_label Y-axis label. Default is NULL (no label).
#' @param legend_fill_title Title for the fill legend. Default is NULL (no title).
#' @param legend_shape_title Title for the shape legend. Default is NULL (no title).
#' @param dot_size Size of the points. Default is 2.
#' @param layer_fill_color Fill color for the map layer. Default is "lightblue".
#' @param layer_border_color Border color for the map layer. Default is "black".
#' @param layer_border_width Border width for the map layer. Default is 0.5.
#' @param palette Color palette for points. Can be a predefined name or a custom named vector.
#'
#' @return A ggplot object showing observations for the specified Zupanije.
#' @export
plot.zupanije <- function(fill = "taxon.name", shape = NULL, zupanija, df,
                          title = NULL, x_axis_label = NULL, y_axis_label = NULL,
                          legend_fill_title = NULL, legend_shape_title = NULL,
                          dot_size = 2,
                          layer_fill_color = "lightblue",
                          layer_border_color = "black",
                          layer_border_width = 0.5,
                          palette = "viridis") {
  library(sf)
  library(ggplot2)
  library(scales)

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # Load Zupanije map layer
  json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
  if (json_path == "") stop("Zupanije GeoJSON file not found in the package.")
  zupanije_sf <- sf::st_read(json_path, quiet = TRUE)

  # Filter for the selected Zupanije
  zupanije_filtered <- zupanije_sf[zupanije_sf$name %in% zupanija, ]
  if (nrow(zupanije_filtered) == 0) stop("None of the specified Zupanije were found.")

  # Convert the Biologer data to an sf object
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Transform Biologer data to match the CRS of the Zupanija map layer
  df_sf <- sf::st_transform(df_sf, crs = sf::st_crs(zupanije_sf))

  # Spatial join to filter points within the selected Zupanije
  points_in_zupanije <- sf::st_join(df_sf, zupanije_filtered, join = sf::st_within, left = FALSE)

  # Generate a dynamic palette if needed
  unique_fill_values <- unique(points_in_zupanije[[fill]])
  if (is.character(palette) && !is.null(names(palette))) {
    # Custom palette provided
    if (!all(unique_fill_values %in% names(palette))) {
      missing_values <- setdiff(unique_fill_values, names(palette))
      stop("The custom palette is missing colors for: ", paste(missing_values, collapse = ", "))
    }
    colors <- palette
  } else if (is.character(palette) && length(palette) == 1) {
    # Predefined palette
    color_count <- length(unique_fill_values)
    if (palette == "viridis") {
      colors <- viridis::viridis(color_count)
    } else if (palette == "plasma") {
      colors <- viridis::plasma(color_count)
    } else if (palette == "rainbow") {
      colors <- rainbow(color_count)
    } else {
      colors <- scales::hue_pal()(color_count)
    }
    names(colors) <- unique_fill_values
  } else {
    stop("Invalid palette. Use a predefined palette name or a named list of colors.")
  }

  # Build the ggplot
  p <- ggplot() +
    geom_sf(data = zupanije_filtered, fill = layer_fill_color, color = layer_border_color, size = layer_border_width) +
    geom_sf(
      data = points_in_zupanije,
      aes_string(color = fill, shape = shape),
      size = dot_size
    ) +
    scale_color_manual(values = colors) +
    theme_minimal()

  # Add custom title and axis labels
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  if (!is.null(x_axis_label)) {
    p <- p + xlab(x_axis_label)
  }
  if (!is.null(y_axis_label)) {
    p <- p + ylab(y_axis_label)
  }

  # Customize the legends if titles are provided
  if (!is.null(legend_fill_title)) {
    p <- p + scale_color_manual(name = legend_fill_title, values = colors)
  }
  if (!is.null(legend_shape_title)) {
    p <- p + scale_shape_discrete(name = legend_shape_title)
  }

  return(p)
}

####################################3
#' Plot Observations Within a Polygon
#'
#' This function plots observations within a user-defined polygon, providing both a detailed plot with observations and a location overview.
#'
#' @param fill Column name for coloring points. Default is "taxon.name".
#' @param shape Column name for point shapes. Default is NULL (no shapes).
#' @param polygon_coords A data frame or list of data frames defining polygon(s) with longitude and latitude columns.
#' @param df A dataframe containing latitude and longitude columns, as well as any additional data for plotting.
#' @param layer Map layer to use for the background: "rh" or "zupanije". Default is "zupanije".
#' @param padding Padding in meters around the polygon to adjust the map extent. Default is 0.
#' @param title Title for the plot with observations. Default is NULL.
#' @param x_axis_label Label for the X-axis. Default is NULL.
#' @param y_axis_label Label for the Y-axis. Default is NULL.
#' @param legend_fill_title Title for the fill legend. Default is NULL.
#' @param legend_shape_title Title for the shape legend. Default is NULL.
#' @param dot_size Size of the observation points. Default is 2.
#' @param layer_fill_color Fill color for the map layer. Default is "lightblue".
#' @param layer_border_color Border color for the map layer. Default is "black".
#' @param layer_border_width Border width for the map layer. Default is 0.5.
#' @param polygon_fill_color Fill color for the polygon. Default is "orange".
#' @param polygon_border_color Border color for the polygon. Default is "red".
#' @param polygon_border_width Border width for the polygon. Default is 1.
#' @param palette Color palette for points. Can be a predefined palette (e.g., "viridis") or a custom named vector. Default is "viridis".
#'
#' @return A list containing two ggplot objects:
#'   - `plot_with_points`: Plot showing the observations within the polygon.
#'   - `polygon_location`: Plot showing only the polygon over the selected map layer.
#' @export
plot.polygon <- function(fill = "taxon.name", shape = NULL, polygon_coords, df, layer = "zupanije",
                         padding = 0, title = NULL, x_axis_label = NULL, y_axis_label = NULL,
                         legend_fill_title = NULL, legend_shape_title = NULL,
                         dot_size = 2, layer_fill_color = "lightblue",
                         layer_border_color = "black", layer_border_width = 0.5,
                         polygon_fill_color = "orange", polygon_border_color = "red", polygon_border_width = 1,
                         palette = "viridis") {
  library(sf)
  library(ggplot2)
  library(scales)

  # Handle single polygon or list of polygons
  if (inherits(polygon_coords, "data.frame")) {
    polygon_coords_list <- list(polygon_coords)
  } else if (is.list(polygon_coords) && all(sapply(polygon_coords, inherits, "data.frame"))) {
    polygon_coords_list <- polygon_coords
  } else {
    stop("polygon_coords must be a data frame or a list of data frames.")
  }

  # Create sf polygons with closing points
  create_polygon <- function(coords) {
    if (!all(coords[1, ] == coords[nrow(coords), ])) {
      coords <- rbind(coords, coords[1, ])
    }
    sf::st_polygon(list(as.matrix(coords)))
  }

  polygons_sfg <- lapply(polygon_coords_list, create_polygon)
  polygons_sfc <- sf::st_sfc(polygons_sfg, crs = 4326)
  polygons_sf <- sf::st_sf(geometry = polygons_sfc)

  # Calculate bounding box with padding
  if (padding > 0) {
    combined_poly <- sf::st_union(polygons_sf)
    centroid <- sf::st_centroid(combined_poly)
    coords <- sf::st_coordinates(centroid)
    lon <- coords[1, "X"]
    lat <- coords[1, "Y"]
    zone <- floor((lon + 180) / 6) + 1
    hemisphere <- ifelse(lat >= 0, "north", "south")
    crs_utm <- sprintf("+proj=utm +zone=%d +%s +datum=WGS84", zone, hemisphere)

    combined_poly_utm <- sf::st_transform(combined_poly, crs = crs_utm)
    buffered_utm <- sf::st_buffer(combined_poly_utm, dist = padding)
    buffered_poly <- sf::st_transform(buffered_utm, crs = 4326)
    bbox <- sf::st_bbox(buffered_poly)
  } else {
    bbox <- sf::st_bbox(polygons_sf)
  }
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])

  # Clean data and convert to sf
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing NA coordinates.")
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Load map layer
  if (layer == "rh") {
    shapefile_path <- system.file("extdata", "RH_2018.shp", package = "biologerR")
    if (shapefile_path == "") stop("RH_2018 shapefile not found.")
    map_layer <- sf::st_read(shapefile_path, quiet = TRUE)
  } else if (layer == "zupanije") {
    json_path <- system.file("extdata", "zupanije.json", package = "biologerR")
    if (json_path == "") stop("Zupanije GeoJSON not found.")
    map_layer <- sf::st_read(json_path, quiet = TRUE)
  } else {
    stop("Invalid layer. Use 'rh' or 'zupanije'.")
  }

  # Ensure map layer has correct CRS
  map_layer <- sf::st_transform(map_layer, crs = 4326)

  # Filter points within polygons
  points_in_poly <- sf::st_join(df_sf, polygons_sf, join = sf::st_within, left = FALSE)

  # Prepare color palette
  unique_fill <- unique(points_in_poly[[fill]])
  if (is.character(palette) && !is.null(names(palette))) {
    if (!all(unique_fill %in% names(palette))) {
      missing <- setdiff(unique_fill, names(palette))
      stop("Palette missing colors for: ", paste(missing, collapse = ", "))
    }
    colors <- palette
  } else if (is.character(palette) && length(palette) == 1) {
    n <- length(unique_fill)
    colors <- switch(palette,
                     "viridis" = viridis::viridis(n),
                     "plasma" = viridis::plasma(n),
                     "rainbow" = rainbow(n),
                     scales::hue_pal()(n))
    names(colors) <- unique_fill
  } else {
    stop("Invalid palette. Use a named vector or predefined palette.")
  }

  # Generate plots
  plot_main <- ggplot() +
    geom_sf(data = map_layer, fill = layer_fill_color, color = layer_border_color, linewidth = layer_border_width) +
    geom_sf(data = polygons_sf, fill = polygon_fill_color, color = polygon_border_color, linewidth = polygon_border_width) +
    geom_sf(data = points_in_poly, aes_string(color = fill, shape = shape), size = dot_size) +
    scale_color_manual(values = colors) +
    coord_sf(xlim = xlim, ylim = ylim) +
    labs(title = title, x = x_axis_label, y = y_axis_label, color = legend_fill_title, shape = legend_shape_title) +
    theme_minimal()

  plot_location <- ggplot() +
    geom_sf(data = map_layer, fill = layer_fill_color, color = layer_border_color, linewidth = layer_border_width) +
    geom_sf(data = polygons_sf, fill = polygon_fill_color, color = polygon_border_color, linewidth = polygon_border_width) +
    coord_sf() +
    labs(title = "Polygon Location Overview") +
    theme_minimal()

  return(list(plot_with_points = plot_main, polygon_location = plot_location))
}


#' Count Observations Per Polygon
#'
#' Counts the number of observations that fall within each provided polygon.
#'
#' @param fill Column name to group and count rows by. Default is "taxon.name".
#' @param polygon_coords A data frame or list of data frames defining polygon(s) with longitude and latitude columns.
#' @param polygon_names Optional vector of names for the polygons. If not provided, polygons will be numbered.
#' @param df A dataframe containing latitude and longitude columns, as well as the column specified in fill.
#'
#' @return A dataframe with counts for each polygon and a TOTAL row.
#' @export
polygon.counts <- function(fill = "taxon.name", polygon_coords, polygon_names = NULL, df) {
  library(sf)
  library(dplyr)

  # Remove rows with missing coordinates
  df <- df[!is.na(df$latitude) & !is.na(df$longitude), ]
  message(nrow(df), " rows remain after removing rows with missing latitude or longitude.")

  # Handle single polygon or list of polygons
  if (inherits(polygon_coords, "data.frame")) {
    polygon_coords_list <- list(polygon_coords)
  } else if (is.list(polygon_coords) && all(sapply(polygon_coords, inherits, "data.frame"))) {
    polygon_coords_list <- polygon_coords
  } else {
    stop("polygon_coords must be a data frame or a list of data frames.")
  }

  # Create polygon names if not provided
  if (is.null(polygon_names)) {
    polygon_names <- paste("Polygon", seq_along(polygon_coords_list))
  }
  if (length(polygon_names) != length(polygon_coords_list)) {
    stop("Length of polygon_names must match the number of polygons.")
  }

  # Create sf polygons with closing points
  create_polygon <- function(coords) {
    if (!all(coords[1, ] == coords[nrow(coords), ])) {
      coords <- rbind(coords, coords[1, ])
    }
    sf::st_polygon(list(as.matrix(coords)))
  }

  polygons_sfg <- lapply(polygon_coords_list, create_polygon)
  polygons_sfc <- sf::st_sfc(polygons_sfg, crs = 4326)
  polygons_sf <- sf::st_sf(
    name = polygon_names,
    geometry = polygons_sfc
  )

  # Convert the data to an sf object
  df_sf <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Spatial join to find which polygon each point falls into
  joined_data <- sf::st_join(df_sf, polygons_sf, join = sf::st_within)

  # Count rows per polygon and grouping variable
  if (fill == "none") {
    counts <- joined_data %>%
      group_by(name) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(!!fill := "ALL")  # Add a default "ALL" column when no grouping
  } else {
    counts <- joined_data %>%
      group_by(!!sym(fill), name) %>%
      summarise(count = n(), .groups = "drop")
  }

  # Add TOTAL row for each group and for the entire dataset
  total_per_group <- counts %>%
    group_by(!!sym(fill)) %>%
    summarise(name = "TOTAL", count = sum(count), .groups = "drop")

  total_overall <- counts %>%
    summarise(!!sym(fill) := "TOTAL", name = "TOTAL", count = sum(count), .groups = "drop")

  # Combine everything into a single dataframe
  final_counts <- bind_rows(counts, total_per_group, total_overall)

  # Arrange for better readability
  final_counts <- final_counts %>%
    arrange(!!sym(fill), name)

  # Print the result
  print(final_counts)
  return(final_counts)
}
