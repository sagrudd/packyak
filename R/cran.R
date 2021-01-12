# lookup = httr::GET("https://cran.r-project.org/web/packages/RCurl/index.html")



Cran = R6::R6Class(
  "Cran",
  inherit = RPackage,
  public = list(

    initialize = function(pkgname, htmlpage) {
      cli::cli_alert(
        stringr::str_interp(
          "investigating CRAN package [{pkgname}]"))
      private$htmlpage <- htmlpage
      private$page_tables <- rvest::html_nodes(httr::content(lookup), "table")
      private$extract_links()
      private$system_requirements()
    }
  ),

  private = list(
    htmlpage = NA,
    page_tables = NA,

    extract_links = function() {
      fields = c("Depends:", "Imports:", "Suggests:")
      info <- private$page_tables[[1]] %>% rvest::html_table(fill = TRUE)
      for (field in fields) {
        cli::cli_alert(
          stringr::str_interp(
            "extracting package dependencies from [${field}]"))
        if (field %in% info[,1]) {
          print(info[which(info[,1]==field),2])
        }

      }
    },

    system_requirements = function() {
      cli::cli_alert("looking for system requirements")
    }

  )
)

