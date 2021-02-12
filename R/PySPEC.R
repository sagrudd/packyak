
PySPEC = R6::R6Class(
  "PySPEC",

  inherit = SPECtacular,

  public = list(

    initialize = function(
      package_name, python_version = "3.8",
      package_spec="python-${package_name}",
      package_rpm="python${rpm_addval}_bio_${package_name}",
      rpm_addval = "3",
      forcerebuild=FALSE) {

      private$valid_package <- FALSE
      private$package_name <- package_name
      private$license <- "undefined"
      private$update_specfile <- FALSE

      private$python_version <- python_version

      cli::cli_h1(stringr::str_interp(
        "Trying to evaluate Python package [${private$package_name}]"
      ))

      if (self$resolve_pipy_name()) {
        private$parse_metadata()

        # does a SPEC file already exist?
        self$spec_file <- stringr::str_interp(package_spec)
        if (self$spec_file_exists()) {
          cli::cli_alert("SPEC file found - checking versions")
          # get SPEC package version ...
          specversion <- self$get_specced_version()

          # is version here newer than version in SPECfile
          deltaversion = 0
          if (specversion != private$package_version) {
            deltaversion <- utils::compareVersion(
              specversion, private$package_version)
          }

          # has the python build version been updated???
          deltapyversion <- utils::compareVersion(
            self$get_spec_py_version(), python_version)


          if (deltaversion == -1 || deltapyversion == -1  || forcerebuild==TRUE) {

            if (!self$is_specfile_locked()) {
              cli::cli_alert("this package will be updated")
              private$update_specfile <- TRUE
            } else {
              cli::cli_alert("associated SPECfile is locked ... not following")
            }
          } else {
            cli::cli_alert("SPEC version is current and force rebuild not requested.")
          }



        } else {
          cli::cli_alert("SPEC file does not already exist")
          private$update_specfile <- TRUE
        }
      }

    },


    resolve_pipy_name = function() {

      private$url <- stringr::str_interp(
        "https://pypi.org/project/${private$package_name}/")
      cli::cli_alert(stringr::str_interp("checking PyPi [${private$url}]"))

      private$lookup_page <- httr::GET(private$url)
      if (private$lookup_page $status_code == 200) {
        cli::cli_alert_success("PyPi HTML::200 response")
        return(TRUE)
      } else {
        cli::cli_alert_warning(stringr::str_interp(
          "package [${private$package_name}] not found at PyPi"))
        return(FALSE)
      }
    },


    get_download_link = function() {
      cli::cli_alert_success("python download_link called ...")
      mytable <- private$pick_table()

      links = mytable %>% rvest::html_nodes("a") %>% rvest::html_attr("href")
      print(links)

      link = "unresolvable link ..."

      if (any(stringr::str_detect(links, "^https.*tar.gz"))) {
        httplinks <- stringr::str_which(links, "^https.*tar.gz")
        links <- links[httplinks]
        link <- links[length(links)]
      } else if (any(stringr::str_detect(links, "^https.*zip"))) {
        httplinks <- stringr::str_which(links, "^https.*zip")
        links <- links[httplinks]
        link <- links[length(links)]
      } else {
        cli::cli_alert_danger(stringr::str_interp("PyPi without expected suffix ... [${private$pkgname}]"))
      }
      return(link)

    },

    trawl_dependencies = function() {
      cli::cli_alert_info("trawling PyPi dependencies using johnnydep ...")

      command = stringr::str_interp("~/.local/bin/johnnydep --output-format pinned ${private$package_name}")
      johnnydata <- system(command, intern=TRUE) %>%
        stringr::str_extract("^.*(?=\\=\\=)")
      return(johnnydata)

    }


  ),

  private = list (

    lookup_page = NULL,


    parse_metadata = function() {
      cli::cli_h2("parsing HTML page metadata")

      page_data <- httr::content(private$lookup_page, "text")


      # we're looking for something like this ... <h1 class="package-header__name">
      title_string <- xml2::read_html(page_data) %>%
        rvest::html_nodes(".package-header__name") %>%
        rvest::html_text() %>%
        stringr::str_replace_all("\n", "") %>%
        stringr::str_trim() %>%
        stringr::str_split("\\s", n=2)
      title_string <- unlist(title_string)
      private$package_version <- title_string[2]
      cli::cli_alert_success(stringr::str_interp(
        "version parsed as [${private$package_version}]"))

      # extract license
      license <- page_data %>%
        stringr::str_c(collapse="", sep="") %>%
        stringr::str_extract("(?<=<p><strong>License:</strong>)[^<]+") %>%
        stringr::str_trim()
      if (!is.na(license)) {
        private$license <- license
      }
      cli::cli_alert_info(stringr::str_interp("license     : ${self$get_license()}"))

      private$valid_package <- TRUE

    },


    pick_table = function() {

      page_tables <- rvest::html_nodes(
        httr::content(private$lookup_page, encoding="UTF-8"), "table")

      for (i in seq(10)) {
        mytable <- page_tables[[i]]
        attrs <- rvest::html_attrs(mytable)
        if ("table table--downloads" %in% attrs) {
          cli::cli_alert(stringr::str_interp("picked tagged table [[${i}]]"))
          return(mytable)
        }
      }
      silent_stop("missing table ???")
    }

  )

)
