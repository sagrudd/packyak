
PySPEC = R6::R6Class(
  "PySPEC",

  inherit = SPECtacular,

  public = list(

    initialize = function(package_name) {

      private$package_name <- package_name
      private$valid_package <- FALSE

      cli::cli_h1(stringr::str_interp(
        "Trying to evaluate Python package [${private$package_name}]"
      ))

      if (self$resolve_pipy_name()) {
        private$parse_metadata()
      }

    },

    is_valid_package = function() {
      return(private$valid_package)
    },

    resolve_pipy_name = function() {

      pypi <- stringr::str_interp(
        "https://pypi.org/project/${private$package_name}/")
      cli::cli_alert(stringr::str_interp("checking PyPi [{pypi}]"))

      private$lookup_page <- httr::GET(pypi)
      if (private$lookup_page $status_code == 200) {
        cli::cli_alert_success("PyPi HTML::200 response")
        return(TRUE)
      } else {
        cli::cli_alert_warning(stringr::str_interp(
          "package [${private$package_name}] not found at PyPi"))
        return(FALSE)
      }
    },

    get_package_version = function() {

    }

  ),

  private = list (

    valid_package = NULL,
    package_name = NULL,
    package_version = NULL,
    lookup_page = NULL,


    parse_metadata = function() {
      cli::cli_h2("parsing HTML page metadata")

      page_data <- content(private$lookup_page, "text")


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
      private$license <- page_data %>%
        stringr::str_c(collapse="", sep="") %>%
        stringr::str_extract("(?<=<p><strong>License:</strong>)[^<]+") %>%
        stringr::str_trim()
      cli::cli_alert_info(stringr::str_interp("license     : ${self$get_license()}"))

    }

  )

)
