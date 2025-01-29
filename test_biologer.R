library(biologerR)
packageVersion("biologerR")  # Should show 0.1.1

# Run the main function
get_biologer_data()


# Test with default path
df <- open.data()

head(df)

df_subset <- subset.biologer(taxon.name = c("Vipera berus", "Aglais io"))

head(df_subset)

counts <- zupanije.biologer(fill = "taxon.name", df = df_subset)
counts_df <- as.data.frame(counts)
print(counts_df)

######################################################

plot.hr()

plot.zupanija()

# Use a predefined palette
plot.zupanije(
  zupanija = "Primorsko-Goranska",
  df = df_subset,
  fill = "taxon.name",
  palette = "plasma",
  dot_size = 4,
  layer_fill_color = "lightgreen",
  layer_border_color = "darkgreen",
  layer_border_width = 1.5,
  title = "Observations in Primorsko-Goranska",
  legend_fill_title = "Taxon Name"
)

# Use a custom palette
custom_palette <- c("Vipera berus" = "red", "Aglais io" = "blue")
plot.zupanije(
  zupanija = c("Primorsko-Goranska", "Grad Zagreb"),
  df = df_subset,
  fill = "taxon.name",
  palette = custom_palette,
  dot_size = 4,
  layer_fill_color = "lightgreen",
  layer_border_color = "darkgreen",
  layer_border_width = 1.5,
  title = "Observations in Primorsko-Goranska",
  legend_fill_title = "Taxon Name"
)


# Use a predefined palette
plot.biologer(
  df = df_subset,
  layer = "zupanije",       # Specify the layer (either "rh" or "zupanije")
  fill = "taxon.name",      # Column to use for coloring points
  palette = "viridis",      # Use the predefined "viridis" palette
  dot_size = 1,             # Set the dot size
  layer_fill_color = "lightgray", # Map layer fill color
  layer_border_color = "blue",    # Map layer border color
  layer_border_width = 0.1,         # Map layer border width
  title = "Biologer Observations",
  legend_fill_title = "Taxon Name" # Custom legend title
)

# Use a custom palette
custom_palette <- c("Vipera berus" = "red", "Aglais io" = "blue")
plot.biologer(
  df = df_subset,
  layer = "zupanije",       # Specify the layer (either "rh" or "zupanije")
  fill = "taxon.name",      # Column to use for coloring points
  palette = custom_palette, # Provide the custom palette
  dot_size = 4,             # Set the dot size
  layer_fill_color = "lightgray", # Map layer fill color
  layer_border_color = "blue",    # Map layer border color
  layer_border_width = 1,         # Map layer border width
  title = "Biologer Observations",
  legend_fill_title = "Taxon Name" # Custom legend title
)



########################

# Define a polygon
example_polygon_coords <- data.frame(
  longitude = c(18.34, 18.37, 18.37, 18.34),
  latitude = c(42.52, 42.52, 42.54, 42.54)
)

# Plot the observations with polygon auto-closing
plots <- plot.polygon(
  fill = "taxon.name",
  shape = NULL, # No shape column in this example
  polygon_coords = example_polygon_coords,
  df = df_subset,
  layer = "zupanije", # or "rh" for RH_2018
  padding = 30000, # Small buffer
  title = "Observations in Polygon",
  x_axis_label = "Longitude",
  y_axis_label = "Latitude",
  legend_fill_title = "Taxon",
  dot_size = 3,
  layer_fill_color = "lightblue",
  layer_border_color = "darkblue",
  layer_border_width = 0.7,
  polygon_fill_color = "yellow",
  polygon_border_color = "darkred",
  polygon_border_width = 1.5,
  palette = "rainbow"
)

# View the plots
print(plots)
print(plots$plot_with_points)  # Plot with points
print(plots$polygon_location)  # Plot with only the polygon

result <- polygon.counts(
  fill = "taxon.name",
  polygon_coords = example_polygon_coords,
  polygon_names = "My Study Area",
  df = df_subset
)

# Define multiple polygons
polygon_list2<- list(
  data.frame(longitude = c(18.34, 18.37, 18.37, 18.34), latitude = c(42.52, 42.52, 42.54, 42.54)),
  data.frame(longitude = c(18.40, 18.42, 18.42, 18.40), latitude = c(42.56, 42.56, 42.58, 42.58))
)


plots <- plot.polygon(
  polygon_coords = polygon_list2,  # Correct parameter name
  df = df_subset,
  layer = "zupanije",
  title = "Observations in Multiple Polygons",
  x_axis_label = "Longitude",
  y_axis_label = "Latitude",
  legend_fill_title = "Taxon",
  dot_size = 3,
  layer_fill_color = "lightblue",
  layer_border_color = "darkblue",
  layer_border_width = 0.7,
  polygon_fill_color = "yellow",
  polygon_border_color = "darkred",
  polygon_border_width = 1.5,
  palette = "rainbow"
)

# View the plots
print(plots$plot_with_points)  # Plot with points
print(plots$polygon_location)  # Plot with only the polygons

result <- polygon.counts(
  fill = "taxon.name",
  polygon_coords = polygon_list2,
  polygon_names = c("Area 1", "Area 2"),
  df = df_subset
)
