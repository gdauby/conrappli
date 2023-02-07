
#' Extract the Rainbio database from a species list
#'
#' Extract from the Rainbio database of all records from a species list
#'
#' @param species Names of species to search for.
#' @param idtax idtax.
#' @param only_checked_georef logical, whether filtering out occurrences with non validated georeferencing.
#'
#' @return A list with the sf of the Rainbio database extracted, the polygon used to extract, a tibble with idtax
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @importFrom sf st_as_text st_read st_drop_geometry
#' @importFrom glue glue
#' @importFrom dplyr as_tibble distinct
#'
#' @examples
#' \dontrun{
#' query_rb_taxa(species = c("diospyros iturensis", "anthonotha macrophylla"))
#' }
#' @export
query_rb_taxa <- function(species = NULL, idtax = NULL, only_checked_georef = TRUE) {

  mydb_rb <- conn_mydb_rb(pass = "Anyuser2022", user = "common")
  on.exit(DBI::dbDisconnect(mydb_rb))

  print(idtax)

  quer_sp <-
    query_taxa(
      species = species,
      ids = idtax,
      check_syn = TRUE,
      class = NULL
    )

  idtax <- unique(quer_sp$idtax_n)

  tbl <- "table_rec"
  sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE idtax_f IN ({vals*})",
                       vals = idtax, .con = mydb_rb)
  rs <- DBI::dbSendQuery(mydb_rb, sql)
  res <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  res <- dplyr::as_tibble(res)

  if (only_checked_georef) {

    res <-
      res %>%
      dplyr::filter(georef_final == 1)

  }

  return(list(
    extract_all_tax = res,
    idtax = tibble(idtax_f = idtax)
  ))
}


#' Extract the rainbio database from a spatial query
#'
#' Extract all records of all taxa identified to species level into a dranw polygon from the rainbio database
#'
#' @param poly An `sf` object, typically a polygon obtained with `mapedit::drawFeatures()`.
#' @param only_checked_georef logical, whether filtering out occurrences with non validated georeferencing.
#'
#' @return A list with the sf of the rainbio database extracted, the polygon used to extract, a tibble with idtax
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @importFrom sf st_as_text st_read st_drop_geometry st_combine st_cast
#' @importFrom glue glue
#' @importFrom dplyr as_tibble distinct
#' @importFrom cli cli_alert_success
#'
#'
#' @export
#' @examples
#' \dontrun{
#' poly <- mapedit::drawFeatures()
#' query_rb_poly(poly)
#' }
query_rb_poly <- function(poly, only_checked_georef = TRUE) {

  mydb_rb <- conn_mydb_rb(pass = "Anyuser2022", user = "common")
  on.exit(DBI::dbDisconnect(mydb_rb))
  
  poly <- sf::st_transform(poly, 4326)

  if (inherits(poly, "sf")) {
    p_geo <- sf::st_cast(sf::st_combine(poly$geometry), to = "MULTIPOLYGON")
    sss_txt <- sf::st_as_text(p_geo)
  } else if (inherits(poly, "sfc")) {
    sss_txt <- sf::st_as_text(poly)
  } else {
    stop("'poly' must be an 'sf' or 'sfc' object.")
  }

  tbl <- "table_rec_sf"
  query_g <-
    glue::glue(
      "SELECT * FROM {`tbl`} WHERE St_intersects(geometry::geography,
        ST_MultiPolygonFromText('{`sss_txt`}', 4326)::geography);"
    )

  extract <- func_try_st_read(con = mydb_rb, sql = query_g)

  cli::cli_alert_success("Extract from polygon done")

  idtax_sel <- extract %>%
    sf::st_drop_geometry() %>%
    dplyr::as_tibble() %>%
    dplyr::filter(!is.na(tax_sp_level)) %>%
    dplyr::distinct(idtax_f)

  tbl <- "table_rec"
  sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE idtax_f IN ({vals*})",
                       vals = idtax_sel$idtax_f, .con = mydb_rb)
  res <- func_try_fetch(con = mydb_rb, sql = sql)
  
  if (only_checked_georef) {
    
    res <-
      res %>%
      dplyr::filter(georef_final == 1)
    
  }

  cli::cli_alert_success("Extract from rainbio record done")

  return(list(
    extract_sf = extract,
    extract_all_tax = res,
    poly = poly,
    idtax = idtax_sel
  ))
}



#' Load the rainbio database
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
conn_mydb_rb <- function(pass = NULL, user = NULL) {
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = 'rainbio',
    host = 'dg474899-001.dbaas.ovh.net',
    port = 35699,
    user = user,
    password = pass
  )
}



#' List, extract taxa
#'
#' Provide list of selected taxa
#'
#' @return A tibble of all taxa
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param class character string of class
#' @param family string
#' @param genus string
#' @param order string
#' @param species string genus followed by species name separated by one space
#' @param only_genus logical
#' @param only_family logical
#' @param only_class logical
#' @param ids integer id of searched taxa
#' @param verbose logical
#' @param exact_match logical
#' @param check_syn logical
#' @param extract_known_syn logical
#'
#' @return A tibble of plots or individuals if extract_individuals is TRUE
#' @export
query_taxa <- function(class = c("Magnoliopsida", "Pinopsida", "Lycopsida", "Pteropsida"),
                       family = NULL,
                       genus = NULL,
                       order = NULL,
                       species = NULL,
                       only_genus = FALSE,
                       only_family = FALSE,
                       only_class = FALSE,
                       ids = NULL,
                       verbose = TRUE,
                       exact_match = FALSE,
                       check_syn = TRUE,
                       extract_known_syn = FALSE) {

  mydb_rb <- conn_mydb_rb(pass = "Anyuser2022", user = "common")
  on.exit(DBI::dbDisconnect(mydb_rb))

  if(!is.null(class)) {

    res_q <- query_exact_match(
      tbl = "table_tax_famclass",
      field = "tax_famclass",
      values_q = class,
      con = mydb_rb
    )

    res_class <-
      tbl(mydb_rb, "table_taxa") %>%
      dplyr::filter(id_tax_famclass %in% !!res_q$res_q$id_tax_famclass) %>%
      dplyr::select(idtax_n, idtax_good_n) %>%
      dplyr::collect()

  }

  if(is.null(ids)) {

    extract_list <- vector('list', 4)

    if(!is.null(order)) {

      q_res <- query_exact_match(
        tbl = "table_taxa",
        field = "tax_order",
        values_q = order,
        con = mydb_rb
      )

      if (!exact_match & any(is.na(q_res$query_tb$id))) {

        if (verbose) cli::cli_alert_info("Fuzzy search for order for {sum(is.na(q_res$query_tb$id))} name(s)")

        query_tb_miss <-
          q_res$query_tb %>%
          filter(is.na(id))

        fuz_list <- vector('list', nrow(query_tb_miss))
        for (i in 1:nrow(query_tb_miss)) {

          fuz_list[[i]] <- query_fuzzy_match(
            tbl = "table_taxa",
            field = "tax_order",
            values_q = query_tb_miss$tax_order[i],
            con = mydb_rb
          )

        }

        fuz_res <- dplyr::bind_rows(fuz_list) %>% dplyr::distinct()

      }

      if (!exact_match & any(is.na(q_res$query_tb$id))) {
        res_order <- bind_rows(fuz_res, q_res$res_q)
      } else {
        res_order <- q_res$res_q
      }

      extract_list[[1]] <- res_order

    }

    if(!is.null(family)) {

      q_res <- query_exact_match(
        tbl = "table_taxa",
        field = "tax_fam",
        values_q = family,
        con = mydb_rb
      )

      if (!exact_match & any(is.na(q_res$query_tb$id))) {

        if (verbose) cli::cli_alert_info("Fuzzy search for family for {sum(is.na(q_res$query_tb$id))} name(s)")

        query_tb_miss <-
          q_res$query_tb %>%
          filter(is.na(id))

        fuz_list <- vector('list', nrow(query_tb_miss))
        for (i in 1:nrow(query_tb_miss)) {

          fuz_list[[i]] <- query_fuzzy_match(
            tbl = "table_taxa",
            field = "tax_fam",
            values_q = query_tb_miss$tax_fam[i],
            con = mydb_rb
          )

        }

        fuz_res <- dplyr::bind_rows(fuz_list) %>% dplyr::distinct()

      }

      if (!exact_match & any(is.na(q_res$query_tb$id))) {
        res_family <- bind_rows(fuz_res, q_res$res_q)
      } else {
        res_family <- q_res$res_q
      }

      extract_list[[2]] <- res_family

    }

    if(!is.null(genus)) {

      q_res <- query_exact_match(
        tbl = "table_taxa",
        field = "tax_gen",
        values_q = genus,
        con = mydb_rb
      )

      if (!exact_match & any(is.na(q_res$query_tb$id))) {

        if (verbose) cli::cli_alert_info("Fuzzy search for genus for {sum(is.na(q_res$query_tb$id))} name(s)")

        query_tb_miss <-
          q_res$query_tb %>%
          filter(is.na(id))

        fuz_list <- vector('list', nrow(query_tb_miss))
        for (i in 1:nrow(query_tb_miss)) {

          fuz_list[[i]] <- query_fuzzy_match(
            tbl = "table_taxa",
            field = "tax_gen",
            values_q = query_tb_miss$tax_gen[i],
            con = mydb_rb
          )

        }

        fuz_res <- dplyr::bind_rows(fuz_list) %>% dplyr::distinct()

      }

      if (!exact_match & any(is.na(q_res$query_tb$id))) {
        res_genus <- bind_rows(fuz_res, q_res$res_q)
      } else {
        res_genus <- q_res$res_q
      }

      extract_list[[3]] <- res_genus

    }

    if(!is.null(species)) {

      q_res <- query_exact_match(
        tbl = "table_taxa",
        field = c("tax_gen", "tax_esp"),
        values_q = species,
        con = mydb_rb
      )

      if (!exact_match & any(is.na(q_res$query_tb$id))) {

        if (verbose) cli::cli_alert_info("Fuzzy search for species for {sum(is.na(q_res$query_tb$id))} name(s)")

        query_tb_miss <-
          q_res$query_tb %>%
          filter(is.na(id))

        fuz_list <- vector('list', nrow(query_tb_miss))
        for (i in 1:nrow(query_tb_miss)) {

          fuz_list[[i]] <- query_fuzzy_match(
            tbl = "table_taxa",
            field = c("tax_gen", "tax_esp"),
            values_q = query_tb_miss$species[i],
            con = mydb_rb
          )

        }

        fuz_res <- dplyr::bind_rows(fuz_list) %>% dplyr::distinct()

      }

      if (!exact_match & any(is.na(q_res$query_tb$id))) {
        res_species <- bind_rows(fuz_res, q_res$res_q)
      } else {
        res_species <- q_res$res_q
      }

      extract_list[[4]] <- res_species
    }

    res <- dplyr::bind_rows(extract_list[unlist(lapply(extract_list, function(x) !is.null(x)))]) %>%
      dplyr::distinct()

    if (!is.null(class))
      res <- res %>% filter(idtax_n %in% res_class$idtax_n)

    no_match <- FALSE

    if(nrow(res) == 0) {
      res <- NULL
      cli::cli_alert_danger("no matching names")
      no_match <- TRUE
    }

  } else {

    if(!is.null(class)) {

      ids <-
        ids[ids %in% res_class$idtax_n]

      if (length(ids) == 0) {

        stop("id provided not found in the class queried")

      }
    }

    tbl <- "table_taxa"
    sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE idtax_n IN ({vals*})",
                         vals = ids, .con = mydb_rb)

    res <- func_try_fetch(con = mydb_rb, sql = sql)

    no_match <- FALSE

    if(nrow(res) == 0) {
      res <- NULL
      cli::cli_alert_danger("no matching names")
      no_match <- TRUE
    }

  }

  if (only_genus)
    res <-
    res %>%
    dplyr::filter(is.na(tax_esp))

  if (only_family)
    res <-
    res %>%
    dplyr::filter(is.na(tax_esp),
                  is.na(tax_gen))

  if (only_class)
    res <-
    res %>%
    dplyr::filter(is.na(tax_esp),
                  is.na(tax_gen),
                  is.na(tax_order),
                  is.na(tax_fam))

  if (!no_match)
    res <-
    res %>% dplyr::mutate(tax_submitted = paste(tax_gen, tax_esp))

  ## checking synonymies
  if (!no_match & check_syn) {

    ## if selected taxa are synonyms
    if(any(!is.na(res$idtax_good_n))) {

      if (any(res$idtax_good_n > 1)) {

        if (verbose) {

          cli::cli_alert_info("{sum(res$idtax_good_n > 1, na.rm = TRUE)} submitted taxa is/are synonym(s)")
          cli::cli_alert_info("{nrow(res)} taxa selected before checking synonymies")

        }

        ## retrieving good idtax_n if selected ones are considered synonyms
        idtax_accepted <-
          res %>%
          dplyr::select(idtax_n, idtax_good_n) %>%
          dplyr::mutate(idtax_f = ifelse(!is.na(idtax_good_n),
                                         idtax_good_n, idtax_n)) %>%
          dplyr::distinct(idtax_f) %>%
          dplyr::rename(idtax_n = idtax_f)

        idtax_already_extracted <-
          res %>%
          dplyr::filter(idtax_n %in% idtax_accepted$idtax_n)

        idtax_syn <-
          res %>%
          dplyr::filter(!idtax_n %in% idtax_accepted$idtax_n) %>%
          dplyr::select(idtax_good_n, tax_submitted)

        idtax_missing <- idtax_accepted %>%
          dplyr::filter(!idtax_n %in% idtax_already_extracted$idtax_n)

        res_syn <-
          dplyr::tbl(mydb_rb, "table_taxa") %>%
          dplyr::filter(idtax_n %in% !!idtax_missing$idtax_n) %>%
          dplyr::collect() %>%
          dplyr::left_join(idtax_syn, by = c("idtax_n" = "idtax_good_n"))

        res <- res_syn %>% dplyr::bind_rows(idtax_already_extracted) %>% dplyr::distinct()

      }
    }
  }

  if (extract_known_syn & !no_match) {
    ## retrieving all synonyms from selected taxa
    id_synonyms <-
      tbl(mydb_rb, "table_taxa") %>%
      dplyr::filter(idtax_good_n %in% !!res$idtax_n) %>% ## all taxa synonyms of selected taxa
      # filter(idtax_n %in% !!res$idtax_n) %>% ## excluding taxa already in extract
      dplyr::select(idtax_n, idtax_good_n) %>%
      dplyr::collect()

    if (nrow(id_synonyms) > 0) {
      if (verbose) {
        cli::cli_alert_info("{sum(id_synonyms$idtax_good_n > 0, na.rm = TRUE)} taxa selected have synonym(s)")
        cli::cli_alert_info("{nrow(res)} taxa selected before checking synonymies")
      }

      synonyms <-
        dplyr::tbl(mydb_rb, "table_taxa") %>%
        dplyr::filter(idtax_n %in% !!id_synonyms$idtax_n) %>%
        dplyr::collect()

      res <-
        res %>%
        bind_rows(synonyms)

    }

  }

  if(!is.null(res)) {
    res <-
      res %>%
      mutate(tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), NA)) %>%
      mutate(tax_infra_level = ifelse(!is.na(tax_esp),
                                      paste0(tax_gen,
                                             " ",
                                             tax_esp,
                                             ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                             ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                             ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                             ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")),
                                      NA)) %>%
      mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
                                           paste0(tax_gen,
                                                  " ",
                                                  tax_esp,
                                                  ifelse(!is.na(author1), paste0(" ", author1), ""),
                                                  ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                                  ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                                  ifelse(!is.na(author2), paste0(" ", author2), ""),
                                                  ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                                  ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
                                                  ifelse(!is.na(author3), paste0(" ", author3), "")),
                                           NA)) %>%
      dplyr::mutate(introduced_status = trimws(introduced_status)) %>%
      dplyr::mutate(tax_sp_level = as.character(tax_sp_level),
                    tax_infra_level = as.character(tax_infra_level),
                    tax_infra_level_auth = as.character(tax_infra_level_auth)) %>%
      dplyr::select(-tax_famclass) %>%
      dplyr::select(-fktax) %>%
      dplyr::select(-tax_tax) %>%
      dplyr::left_join(dplyr::tbl(mydb_rb, "table_tax_famclass") %>%
                         dplyr::collect(),
                       by = c("id_tax_famclass" = "id_tax_famclass")) %>%
      dplyr::relocate(tax_famclass, .after = tax_order) %>%
      dplyr::relocate(year_description, .after = citation) %>%
      dplyr::relocate(data_modif_d, .after = morpho_species) %>%
      dplyr::relocate(data_modif_m, .after = morpho_species) %>%
      dplyr::relocate(data_modif_y, .after = morpho_species) %>%
      dplyr::relocate(tax_sp_level, .before = idtax_n) %>%
      dplyr::relocate(id_tax_famclass, .after = morpho_species) %>%
      dplyr::relocate(tax_submitted, .before = tax_sp_level)



  }

  if(!is.null(res)) return(res)
}





#' Query exact match
#'
#' Extract from a sql database an exact match on a given field
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param tbl tibble with one field listing names to be searched
#' @param field string column name to be search
#' @param values_q string names to be searched
#' @param con PqConnection connection to RPostgres database
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbSendQuery dbFetch dbClearResult
#' @importFrom dplyr sym tibble mutate distinct left_join
#'
#' @return A list of two elements, one with the extract if any, two with the names with id not NA when matched
#' @export
query_exact_match <- function(tbl, field, values_q, con) {

  if (length(field) == 1) {

    field_col <- dplyr::sym(field)

    query_tb <- tibble(!!field_col := tolower(values_q))

  } else {

    query_tb <- tibble(species := tolower(values_q))

  }

  if (length(field) == 1) sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE lower({`field`}) IN ({vals*})",
                                               vals = tolower(values_q), .con = con)
  if (length(field) > 1) sql <-glue::glue_sql("SELECT * FROM {`tbl`} WHERE lower(concat({`field[1]`},' ',{`field[2]`})) IN ({vals*})",
                                              vals = tolower(values_q), .con = con)


  rs <- DBI::dbSendQuery(con, sql)
  res_q <-DBI::dbFetch(rs) %>% as_tibble
  DBI::dbClearResult(rs)

  if (length(field) == 1) {
    query_tb <- query_tb %>%
      dplyr::left_join(
        res_q %>%
          dplyr::select(!!field_col) %>%
          dplyr::mutate(!!field_col := tolower(!!field_col)) %>%
          dplyr::distinct() %>%
          dplyr::mutate(id = seq_len(nrow(.)))
      )
  }

  if (length(field) > 1) {
    query_tb <- query_tb %>%
      dplyr::left_join(
        res_q %>% 
          dplyr::select(dplyr::all_of(field)) %>%
          dplyr::mutate(species = paste(!!dplyr::sym(field[1]), !!dplyr::sym(field[2]), sep = " ")) %>%
          dplyr::mutate(species = tolower(species)) %>%
          dplyr::distinct() %>%
          dplyr::mutate(id = seq_len(nrow(.)))
      )
  }

  return(list(res_q = res_q,
              query_tb = query_tb))

}

#' Query fuzzy match
#'
#' Extract from a sql database an exact match on a given field
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param tbl tibble with one field listing names to be searched
#' @param field string column name to be search
#' @param values_q string names to be searched
#' @param con PqConnection connection to RPostgres database
#'
#' @importFrom glue glue_sql
#' @importFrom DBI dbSendQuery dbFetch dbClearResult
#' @importFrom cli cli_alert_warning
#'
#' @return A list of two elements, one with the extract if any, two with the names with id not NA when matched
#' @export
query_fuzzy_match <- function(tbl, field, values_q, con) {

  # if (length(field) == 1) sql <-glue::glue_sql("SELECT * FROM {`tbl`} ORDER BY SIMILARITY (lower({`field`}), {values_q}) DESC LIMIT 1;",
  #                                              .con = con)

  if (length(field) == 1) {
    sql <- glue::glue_sql(
      "SELECT * FROM {`tbl`} WHERE SIMILARITY (lower({`field`}), {values_q}) > 0.4;",
      .con = con
    )
  }

  if (length(field) > 1) {
    sql <- glue::glue_sql(
      "SELECT * FROM {`tbl`} ORDER BY SIMILARITY (lower(concat({`field[1]`},' ',{`field[2]`})), {values_q}) DESC LIMIT 2;",
      .con = con
    )
  }

  res_q <- func_try_fetch(con = con, sql = sql)

  if (nrow(res_q) == 0) {

    cli::cli_alert_warning("Failed fuzzy match for {values_q[i]} in {field} field in {tbl}")

  }

  return(res_q)
}



func_try_fetch <- function(con, sql) {
  rep <- TRUE
  rep_try <- 1
  while(rep) {

    res_q <- try({rs <- DBI::dbSendQuery(con, sql);
    DBI::dbFetch(rs)}, silent = T)

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
  res_q <- res_q %>% as_tibble
  DBI::dbClearResult(rs)

  return(res_q)
}


func_try_st_read <- function(con, sql) {
  rep <- TRUE
  rep_try <- 1
  while(rep) {

    res_q <-try({st_read(
      con,
      query = sql,
      geometry_column = "geometry"
    )}, silent = FALSE)

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

