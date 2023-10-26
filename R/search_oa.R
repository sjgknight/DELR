#if periodic issues occur e.g. a deparse, try remotes::install_github("ropensci/openalexR")
#I don't know why glob, or regex, or pattern dont work for fs::list_files (all work outside function call), base works fine though
# uses pipes and glue for assigning using variables
# assign(glue("{outputname}_raw"), "hello world") #or pipe
# "hello world" %>% assign(glue("{outputname}_raw"), .)

#' search_oa
#'
#' @param db defaults to openalex, can be modified
#' @param outputname used to (a) save an RDS output and (b) check if an RDS output already exists
#' @param keyterm used in openalex for a single-term search (because it doesn't handle Boolean searches over TAK)
#' @param searchquery full query, constructed with functions from db_search
#'
#' @return
#' @export
#'
#' @examples
search_oa <-
  function(db = c("openalex", "scopus"),
           outputname = "",
           keyterm = "",
           searchquery = "") {

    db <- match.arg(db, choices = c("openalex", "scopus"))
    ############################################################
    ############################################################
    ############### Check if an output already exists, if so, you can update it
    ############################################################
    ############################################################

    data_file <-
      list.files(
        here::here("data/"),
        pattern = glue::glue("search_output_{db}.*{outputname}\\.rds"),
        ignore.case = T,
        full.names = T
      )
    data_file <- data_file[which.max(file.mtime(data_file))]
    data_date <- if (length(data_file) > 0) {
      as.Date(stringr::str_extract(data_file, "\\d{4}-\\d{2}-\\d{2}"))
    } else {
      as.Date("1900-01-01")
    }

    # if a file already exists, it will only search for any updates since then
    if (any(length(data_file) == 0, #does data exist, and
            Sys.Date() < data_date)) {

      ############################################################
      ############################################################
      ############### Search openalex
      ############################################################
      ############################################################

      if(db == "openalex"){

      # 'search' function takes simple input
      openalexR::oa_fetch(
        entity = 'works',
        from_publication_date = data_date,
        search = paste0("\"", keyterm, "\"")
      ) %>%
        mutate(glue("{outputname}_term_core_in_raw") = TRUE) %>%
        assign(glue("{outputname}_raw"), ., envir = .GlobalEnv)

      # 'fulltext.search' searches full text, can be Boolean
      openalexR::oa_fetch(
        entity = 'works',
        from_publication_date = data_date,
        fulltext.search = searchquery,
        verbose = T
      ) %>%
        mutate(glue("{outputname}_terms_in_ft") = TRUE) %>%
        assign(glue("{outputname}_ft"), ., envir = .GlobalEnv)

      # 'abstract.search'
      openalexR::oa_fetch(
        entity = 'works',
        from_publication_date = data_date,
        abstract.search = searchquery
      ) %>%
        mutate(glue("{outputname}_terms_in_ab") = TRUE) %>%
        assign(glue("{outputname}_ab"), ., envir = .GlobalEnv)

      # 'title.search'
      openalexR::oa_fetch(
        entity = 'works',
        from_publication_date = data_date,
        title.search = searchquery
      ) %>%
        mutate(glue("{outputname}_terms_in_ti") = TRUE) %>%
        assign(glue("{outputname}_ti"), ., envir = .GlobalEnv)

      bind_rows(
        get(glue("{outputname}_ab")),
        get(glue("{outputname}_ft")),
        get(glue("{outputname}_raw")),
        get(glue("{outputname}_ti"))
      ) %>%
        assign(glue("{outputname}"), ., envir = .GlobalEnv)

      # Group and summarise
      # There are 9741 results pre
      # And 9733 results post. I think this is because epcog_ft_ captures most things
      get(outputname) %>%
#        group_by(as.factor(id)) %>%
        group_by(across(
          -c(relevance_score, terms_in_ab, term_core_in_raw, terms_in_ft, terms_in_ti)
        )) %>%
        summarize(
          terms_in_ab = any(terms_in_ab),
          term_core_in_raw = any(term_core_in_raw),
          terms_in_ft = any(terms_in_ft),
          terms_in_ti = any(terms_in_ti)
        ) %>% ungroup() %>%
        assign(glue("{outputname}"), ., envir = .GlobalEnv)

      }
      ############################################################
      ############################################################
      ############### end openalex, start scopus
      ############################################################
      ############################################################

      if(db == "scopus"){

        results <- rscopus::scopus_search(query = paste0("TITLE-ABS-KEY(", searchquery, ")"),
                                               api_key = rscopus::get_api_key(),
                                               verbose = T
        )

        gen_entries_to_df(results$entries) %>%
          assign(glue("{outputname"), ., envir = .GlobalEnv)

        get(outputname)$df %>%
          assign(glue("{outputname}"), ., envir = .GlobalEnv)

      }
      ############################################################
      ############################################################
      ############### end scopus, start eric
      ############################################################
      ############################################################

      if(db == "eric"){

        cat("whoops, this hasn't been written yet")

        #ericr::search_eric()

        # results <- rscopus::scopus_search(query = paste0("TITLE-ABS-KEY(", searchquery, ")"),
        #                                   api_key = rscopus::get_api_key(),
        #                                   verbose = T
        # )
        #
        # gen_entries_to_df(results$entries) %>%
        #   assign(glue("{outputname"), ., envir = .GlobalEnv)
        #
        # get(outputname)$df %>%
        #   assign(glue("{outputname}"), ., envir = .GlobalEnv)

      }
      ############################################################
      ############################################################
      ############### Main function scope
      ############################################################
      ############################################################

      last_saved <- Sys.Date()

      readr::write_rds(
        get(outputname), glue(
        "data/search_output_{db}_{last_saved}_{outputname}.rds")
        )


    } else {
      readr::read_rds(data_file) %>%
        assign(glue("{db}_{outputname}"), .)
    }
  }
