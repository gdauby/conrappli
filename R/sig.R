
#' Load the database
#'
#' Load the SIG database and ask for password
#'
#' @param pass string
#' @param user string
#'
#' @return The database is loaded
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
conn_mydb_shp <- function(pass = NULL, user = NULL) {
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "shapefiles",
    host = "dg474899-001.dbaas.ovh.net",
    port = 35699,
    # or any other port specified by your DBA
    user = user,
    password = pass
  )
}


#' Evaluate intersection
#'
#' Evaluate the intersection between coordinates and bounding boxes of the database
#'
#' @param XY data.frame with two columns with geographical coordinates (latitude, longitude)
#' @param col_x string column name indicating longitude
#' @param col_y string column name indicating latitude
#'
#' @return A list with two elements
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @importFrom dplyr tbl collect mutate
#' @importFrom sf st_bbox st_crs st_crs<- st_intersects st_set_crs st_as_sf st_as_sfc
#' @importFrom DBI dbDisconnect
#'
#' @export
extract_overlap_shp <- function(XY, col_x = "ddlon", col_y = "ddlat") {

  mydb_shp <- conn_mydb_shp(pass = "Anyuser2022", user = "common")
  on.exit(DBI::dbDisconnect(mydb_shp))

  shp_tables <-
    try_open_postgres_table(table = "meta_data", con = mydb_shp) %>%
    dplyr::collect() %>%
    dplyr::mutate(bbox = strsplit(bbox, ", ")) %>%
    dplyr::mutate(bbox_sf = lapply(
      X = bbox,
      FUN = function(x) {
        st_as_sfc(st_bbox(
          c(
            xmin = as.numeric(x[1]),
            xmax = as.numeric(x[3]),
            ymax = as.numeric(x[4]),
            ymin = as.numeric(x[2])
          )
        ))
      }
    )) %>%
    dplyr::mutate(bbox_sf = lapply(bbox_sf, function(x)
      st_set_crs(x, 4326)))

  XY_sf <- st_as_sf(XY, coords = c(col_x, col_y))
  st_crs(XY_sf) <- 4326

  res <- lapply(
    X = shp_tables$bbox_sf,
    FUN = function(x) {
      length(st_intersects(x, XY_sf)[[1]]) > 0
    }
  )
  names(res) <- shp_tables$table_name
  return(list(shp_tables = shp_tables %>% dplyr::mutate(overlap = unlist(res)), XY_sf = XY_sf))

}

#' Extract a shapefile
#'
#' Extract a shapefile from the database
#'
#' @param names string name of the table
#' @param XY_sf sf object of dataset
#'
#'
#' @return A sf object
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @importFrom sf st_read st_as_sfc st_as_text
#'
#' @export
collect_shp <- function(table_names, XY_sf = NULL) {

  mydb_shp <- conn_mydb_shp(pass = "Anyuser2022", user = "common")
  on.exit(DBI::dbDisconnect(mydb_shp))

  if (!is.null(XY_sf)) sss_txt <- sf::st_as_text(sf::st_as_sfc(st_bbox(XY_sf)))

  extracts <- vector("list", nrow(table_names))
  for (i in seq_len(nrow(table_names))) {

    if (table_names$type[i] == "polygon") {

      cat(" ", i)

      extracts[[i]] <- st_read(mydb_shp, table_names$table_name[i], geometry_column ='geometry')

      # }
    }

    if (table_names$type[i] == "raster") {

      cat(" ", i)

      extracts[[i]] <- collect_rast(names =  table_names$table_name[i], XY_sf = XY_sf)

    }
  }

  names(extracts) <- table_names$table_name

  return(extracts)
}


try_open_postgres_table <- function(table, con) {

  rep <- TRUE
  rep_try <- 1
  while(rep) {

    res_q <- try({table_postgre <- dplyr::tbl(con, table)}, silent = T)

    if (any(grepl("Lost connection to database", res_q[1])))
      stop("Lost connection to database")

    if (any(grepl("Failed to prepare query", res_q[1]))) {
      rep <- TRUE
      cli::cli_alert_warning("failed to query, trying again")
      rep_try <- rep_try + 1
    } else {
      rep <- FALSE
    }

    if (rep_try == 10)
      stop("Failed to connect to database")
  }

  return(table_postgre)
}

#' @importFrom sf st_read
func_try_st_read <- function(con, sql) {
  rep <- TRUE
  rep_try <- 1
  while(rep) {

    res_q <- try({
      st_read(con,
              query = sql,
              geometry_column = "geometry")
    }, silent = T)

    if (any(grepl("Lost connection to database", res_q[1])))
      stop("Lost connection to database")

    if (any(grepl("Failed to prepare query", res_q[1]))) {
      rep <- TRUE
      cat(rep_try, "failed to query, trying again")
      rep_try <- rep_try + 1
    } else {
      rep <- FALSE
    }

    if (rep_try == 10)
      stop("Failed to connect to database")
  }
  res_q <- res_q


  return(res_q)
}

#' Extract a raster
#'
#' Extract a raster from the database
#'
#' @param names string name of the table
#' @param XY_sf sf object of dataset
#'
#'
#' @return A sf object
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @importFrom sf st_read
#' @importFrom glue glue
#' @importFrom DBI dbGetQuery
#' @importFrom terra rast
#'
#' @export
collect_rast <- function(names, XY_sf = NULL) {

  mydb_shp <- conn_mydb_shp(pass = "Anyuser2022", user = "common")
  on.exit(DBI::dbDisconnect(mydb_shp))

  if (!is.null(XY_sf)) {

    sss_txt <- sf::st_as_text(st_as_sfc(st_bbox(XY_sf)))

    sql <- glue::glue("SELECT st_xmax(st_envelope(rast)) as xmx, st_xmin(st_envelope(rast)) as xmn, st_ymax(st_envelope(rast)) as ymx, st_ymin(st_envelope(rast)) as ymn, st_width(rast) as cols, st_height(rast) as rows
                      FROM (SELECT st_union(rast,1) rast
                      FROM public.{names}
                      WHERE ST_Intersects(rast, ST_SetSRID(ST_GeomFromText('{`sss_txt`}'),4326))) as a;")

    info <- DBI::dbGetQuery(mydb_shp, sql)

  } else {

    sql <- glue::glue("SELECT st_xmax(st_envelope(rast)) as xmx, st_xmin(st_envelope(rast)) as xmn, st_ymax(st_envelope(rast)) as ymx, st_ymin(st_envelope(rast)) as ymn, st_width(rast) as cols, st_height(rast) as rows
                      FROM (SELECT st_union(rast,1) rast
                      FROM public.cities) as a;")
    info <- DBI::dbGetQuery(mydb_shp, sql)
  }

  if (is.na(info$cols) & is.na(info$rows)) {
    cli::cli_alert_info("No overlapping data for this raster")
  } else {

    if (!is.null(XY_sf)) sql <- glue::glue("SELECT unnest(st_dumpvalues(rast, 1)) as vals
                    FROM (SELECT st_union(rast,1) rast FROM public.{names}
                    WHERE ST_Intersects(rast, ST_SetSRID(ST_GeomFromText('{`sss_txt`}'),4326))) as a;")

    if (is.null(XY_sf)) sql <- glue::glue("SELECT unnest(st_dumpvalues(rast, 1)) as vals
                    FROM (SELECT st_union(rast,1) rast FROM public.{names}) as a;")

    vals <- DBI::dbGetQuery(mydb_shp, sql)

    rast_obt <- terra::rast(
      nrows = info$rows, ncols = info$cols,
      xmin = info$xmn,
      xmax = info$xmx,
      ymin = info$ymn,
      ymax = info$ymx,
      crs = "EPSG:4326",
      vals = vals$vals
    )

    return(rast_obt)
  }
}
