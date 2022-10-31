library(targets)
tar_option_set(packages = c("tidyverse", "lubridate", "dataRetrieval", "dotenv"))

get_flow <- function (site_id, por) {
  dataRetrieval::readNWISdv(siteNumbers = site_id, parameterCd = "00060", startDate = por[1], endDate = por[2]) |> 
    dataRetrieval::renameNWISColumns() |> 
    as_tibble() |> 
    select(date = Date, flow = Flow)
}

# db_connect <- function() {
#   # dotenv::load_dot_env()
#   DBI::dbConnect(
#     RPostgres::Postgres(),
#     host = Sys.getenv("DB_HOST"),
#     port = Sys.getenv("DB_PORT"),
#     dbname = Sys.getenv("DB_DBNAME"),
#     user = Sys.getenv("DB_USER"),
#     password = Sys.getenv("DB_PASSWORD")
#   )
# }

get_rainfall <- function (por) {
  # con <- db_connect()
  # df <- DBI::dbGetQuery(con, "SELECT * FROM ...")
  # DBI::dbDisconnect(con)
  # return(df)
  # 
  load_dot_env()
  if (Sys.getenv("DB_SECRET") == "mypassword") {
    df <- tibble(
      date = seq.Date(as_date(por[1]), as_date(por[2]), by = "1 day")
    ) |>
      mutate(
        rain = runif(n(), 1, 100)
      )
    return(df)
  } else {
    stop("Invalid database secret!")
  }
}

list(
  tar_target(por, c("2020-01-01", as.character(lubridate::today() - lubridate::days(1)))),
  tar_target(site_id, "01059000"),
  
  tar_target(flow_data, get_flow(site_id, por)),
  tar_target(flow_plot, {
    flow_data |> 
      ggplot(aes(date, flow)) +
      geom_line()
  }),
  
  tar_target(rainfall_data, get_rainfall(por)),
  
  tar_target(dataset, {
    flow_data |> 
      left_join(rainfall_data, by = "date")
  }),
  tar_target(dataset_plot, {
    dataset |> 
      pivot_longer(-date) |> 
      ggplot(aes(date, value)) +
      geom_line() +
      facet_wrap(vars(name), scales = "free_y", ncol = 1)
  }),
  tar_target(dataset_plot_png, {
    filename <- "export/flow-rainfall.png"
    dir.create("export", showWarnings = FALSE, recursive = TRUE)
    ggsave(filename, dataset_plot, width = 6, height = 4)
    filename
  }, format = "file")
)
