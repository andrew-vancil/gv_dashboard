library(shiny)
library(bslib)
suppressMessages(library(readr))
suppressMessages(library(dplyr))
library(shinyWidgets)
library(shinyjs)


{
  d_cchmc <- read_csv("../data/cchmc_data/mental_health_admissions_tract.csv") |> 
    mutate(census_tract_id = as.character(census_tract_id)) |> 
    left_join(read_rds("../data/tract_to_neighborhood.rds"), by = c("census_tract_id" = "fips_tract_id")) |> 
    summarize(n_adm_neighborhood = sum(admissions),
              n_ed_neigbborhood = sum(ed_visits), .by = c(month_year, neighborhood))
  
  cpd_files <- list.files(path = "../data/cpd_files_sna/", pattern = "\\.csv", full.names = T) |> 
    purrr::map(read_csv) 
  
  names(cpd_files) <- c("harrass", "violent", "shot_spotter", "suicide", "threats")
  
  list2env(cpd_files, envir = .GlobalEnv)
  
  d_housing <- read_csv("../data/housing_data/sna_level_frac_vacant_housing.csv")
  
  neigh <- cincy::neigh_sna |> 
    sf::st_as_sf()
  
}

ui <- page_sidebar(
  title = "Youth Gun Violence Prevention",
  fillable = FALSE,
  theme = bs_theme(version = 5,
                   bg = "#FFFFFF",
                   fg = "#396175",
                   primary = "#C28273",
                   "grid-gutter-width" = "0.0rem",
                   "border-radius" = "0.5rem",
                   "btn-border-radius" = "0.25rem"),
  
  tags$head(
    tags$style(type="text/css", "text {font-family: sans-serif}")),
  tags$style(HTML(
    ".irs-handle {background: #C28273 !important;};
       .irs-handle:active {background: #C28273 !important;}"
  )),
  
  sidebar = sidebar(
    
    selectizeInput("neighborhood",
                   "Select your neighborhood:",
                    choices = cincy::neigh_sna$neighborhood,
                   selected = "Over-the-Rhine",
                   multiple = TRUE),
    
  ),
      
      layout_column_wrap(
        width = 1/1,
        fill = FALSE,
        heights_equal = "row",
        
        layout_column_wrap(
          width = 1/3,
          max_height = 150L,
          fill = FALSE,
          
          value_box("Firearm Crime YTD",
                    theme = "primary",
                    showcase = icon("bell"),
                    value = violent |> 
                      filter(FirearmUsed_YN == "Yes") |> 
                      mutate(date = as.Date(DateFrom, format = "%m/%d/%y")) |> 
                      filter(lubridate::year(date) > 2023) |> 
                      summarise(n = n(), .by = geoid) |> 
                      filter(geoid == "Over-the-Rhine") |> 
                      pull(n)
          ),
          value_box("Shot Spotter (confirmed)",
                    theme = "primary",
                    showcase = icon("bullseye"),
                    value = shot_spotter |> 
                      mutate(date = as.Date(Response_Date, format = "%Y-%m-%d")) |> 
                      filter(lubridate::year(date) > 2023) |> 
                      filter(Call_Disposition == "INV: Inv") |> 
                      summarise(n = n(), .by = geoid) |> 
                      filter(geoid == "Over-the-Rhine") |> 
                      pull(n)
  
          ),
          value_box("Mental Health Admissions",
                    theme = "primary",
                    showcase = icon("hospital-user"),
                    value = d_cchmc |> 
                      filter(neighborhood == "OTR-Pendleton") |> 
                      filter(stringr::str_sub(month_year, -4, -1) == "2024") |> 
                      summarise(n_ed = sum(n_ed_neigbborhood)) |> 
                      pull(n_ed)
          )
        ),
        
        card(
          card_header("plot"),
          plotOutput("plot_output")
        ),
        
        layout_column_wrap(
          width = 1/2,
          fill = TRUE,
          
          card(
            full_screen = TRUE,
            card_header("table"),
            gt::gt_output("table_output")
          ),
          
          card(
            full_screen = TRUE,
            card_header("map"),
            
            layout_sidebar(
              
              leafletOutput("map_output"),
              
              sidebar = sidebar(
                open = FALSE,
                
                selectizeInput("metric",
                               multiple = TRUE,
                               "Select metric:",
                               choices = c(
                                 "mental health admissions",
                                 "housing infractions",
                                 "violent crime",
                                 "shot_spotter",
                                 "harrassment",
                                 "suicide",
                                 "threats"
                               ),
                               selected = "threats"
                )
              )
            )
          )
        )
  )
  
)

server <- function(input, output, session){
  
  output$plot_output <- 
    renderPlot({
      d_gv <- violent |> 
        filter(FirearmUsed_YN == "Yes") |> 
        filter(geoid = "Over-the-Rhine") |> 
        mutate(date = as.Date(DateFrom, format = "%m/%d/%y")) |> 
        filter(lubridate::year(date) > 2021) 
      
      d_gv <- d_gv |> 
        arrange(desc(date)) |> 
        mutate(days_since_last_shooting = date - lead(date))
      
      d_gv$y_m <- as.Date(paste(d_gv$IncYear, d_gv$IncMonth,
                                1, sep = "-"))
      
      d_gv_by_month <- d_gv |> 
        filter(days_since_last_shooting != 0) |> 
        mutate(avg_days_month = round(mean(days_since_last_shooting), 1), .by = y_m) |> 
        distinct(y_m, .keep_all = T)
      
      days_bw_plot <- 
        ggplot(d_gv_by_month) +
        geom_line(aes(x = y_m, y = avg_days_month, group = 1)) +
        geom_point(aes(x = y_m, y = avg_days_month, group = 1)) +
        geom_hline(yintercept = mean(d_gv_by_month$avg_days_month),
                   col = CB::cchmc_color(1)[[1]]) +
        scale_y_continuous() + 
        scale_x_date(date_breaks = "1 months", date_labels = "%b %Y") +
        theme_light() +
        labs(x = "Date", y = "Days", title = "Monthly Average Days Between Shootings (all victims)",
             subtitle = "Note: calculation excludes multiple shootings that occur on the same day") +
        ggeasy::easy_rotate_labels("x", 290)
      
      days_bw_plot
    })
  
  output$table_output <- 
    render_gt({
      
      violent |> 
        filter(FirearmUsed_YN == "Yes") |> 
        mutate(date = as.Date(DateFrom, format = "%m/%d/%y")) |> 
        filter(lubridate::year(date) > 2023) |> 
        summarise(n_firearm_incidents = n(), .by = geoid) |> 
        gt::gt() |> 
        opt_interactive(
          use_search = TRUE
        )
      
    })
  
  output$map_output <- 
    renderLeaflet({
      d_map <- threats |> 
        mutate(date = as.Date(Response_Date, format = "%Y-%m-%d")) |> 
        filter(lubridate::year(date) > 2023) |> 
        summarise(n_neighborhood = n(), .by = geoid) |> 
        left_join(cincy::neigh_sna, by = c("geoid" = "neighborhood")) |> 
        tidyr::drop_na(SHAPE) |> 
        sf::st_as_sf()
      
      tmap_map <- 
        tm_basemap("CartoDB.Positron") +
        tm_shape(d_map) +
        tm_polygons(fill = "n_neighborhood",
                    fill_alpha = .7,
                    fill.legend = tm_legend("2024 Threats"))
      
      leaflet_map <- tmap_leaflet(tmap_map) |> 
        setView(zoom = 11, lng = -84.5, lat = 39.14)
      
      leaflet_map
      
    })
  
}

shinyApp(ui, server)

