# ------------------------------------------------------------------------------
#  Data and functions to create and visualize firm statistics for Berlin at the 
#  PLZ (postal code) level.
#
#  Requires `make all` to be run in repo before sourcing.
#
#  See LICENSE file for license
# ------------------------------------------------------------------------------

suppressWarnings(suppressPackageStartupMessages({
	library(tidyverse)
	library(logger)
	library(sf)
	library(leaflet)
	library(htmltools)
}))

berlin_sf <- st_read(
	"https://tsb-opendata.s3.eu-central-1.amazonaws.com/plz/plz.geojson",
	quiet = TRUE
)

berlin_by_plz <- readRDS("data/generated/orbis_panel_berlin.rds") %>% 
	filter(year == 2021) %>% 
	filter(toas > 0, !is.na(postcode)) %>%
	group_by(postcode) %>%
	summarise(
		n_firms = n(),
		tot_assets = sum(toas)/1e6,
		tot_equity = sum(shfd)/1e6,
		agg_equity_ratio = tot_equity/tot_assets
	) %>%
	rename(plz = postcode) %>%
	arrange(-n_firms)

berlin_plz_choropleth <- function(
	df, var, var_label = waiver(), var_breaks = waiver(), 
	title = "", trans = "identity"
) {
	df <- berlin_sf %>%
		left_join(df, by = "plz")
	ggplot(df) +
		geom_sf(aes(fill = {{ var }}), color = "white") +
		scale_fill_viridis_c(
			option = "plasma", trans = trans, na.value = "grey90",
			breaks = var_breaks,
			labels = function(x) format(x, big.mark = ",")
		) +
		labs(
			fill = var_label, 
			title = title
		) + 
		theme_void() +
		theme(legend.position = "bottom")
}

berlin_plz_leaflet <- function(
		df, var, var_label = "Variable", trans = "identity"
) {
	df <- berlin_sf %>%
		left_join(df, by = "plz") %>%
		mutate(
			var = {{ var }},
			dispvar = case_when(
				trans == "log10" ~ log10({{ var }}),
				TRUE ~ {{ var }}
			)
		) %>%
		rowwise() %>%
		mutate(
			hover_label = HTML(paste(
				"PLZ:", plz, "<br>", 
				format(var, big.mark = ","), var_label
			))
		) %>%
		ungroup()
	
	pal <- colorNumeric(palette = "plasma", domain = df$dispvar)
	leaflet(df) %>%
		addProviderTiles("CartoDB.Positron") %>%
		addPolygons(
			fillColor = ~pal(dispvar),
			weight = 1,
			color = "#555555",
			fillOpacity = 0.7,
			label = ~hover_label
		)
}


# --- Some Usage Examples (not run by default) ---------------------------------

if (FALSE) {
	berlin_plz_choropleth(
		berlin_by_plz, n_firms, trans = "log10",
		var_label = "# companies", 
		title = "Where do Berlin's companies live?"
	)
	berlin_plz_choropleth(
		berlin_by_plz, tot_assets, trans = "log10",
		var_label = "Cumlative Total Assets [Mio. €]", 
		var_breaks = c(10, 500, 10000),
		title = "Where do Berlin's companies' assets live?"
	)
	berlin_plz_choropleth(
		berlin_by_plz, tot_equity, trans = "log10",
		var_label = "Cumlative Total Equity [Mio. €]", 
		var_breaks = c(10, 500, 10000),
		title = "Where do Berlin's companies' equity live?"
	)
	
	berlin_plz_choropleth(berlin_by_plz, agg_equity_ratio)
	
	berlin_plz_leaflet(
		berlin_by_plz, n_firms, "companies", trans = "log10"
	)
}


