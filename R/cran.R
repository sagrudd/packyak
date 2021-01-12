# lookup = httr::GET("https://cran.r-project.org/web/packages/RCurl/index.html")


#' @import R6
#' @importFrom magrittr %>%
#' @importFrom rvest html_nodes
#' @importFrom httr content
#' @importFrom rvest html_table
#' @importFrom stringr str_interp
Cran = R6::R6Class(
  "Cran",
  public = list(

    initialize = function(pkgname, htmlpage, strategy, url) {
      cli::cli_alert(
        stringr::str_interp(
          "investigating CRAN package [{pkgname}]"))
      private$pkgname <- pkgname
      private$htmlpage <- htmlpage
      private$url <- url
      private$strategy <- strategy
      private$page_tables <- rvest::html_nodes(httr::content(htmlpage), "table")
      private$extract_links()
      if (private$is_installable()) {
        private$install_package()
      }
    },


    get_pkg_name = function() {
      return(private$pkgname)
    },

    get_repo = function() {
      return("cran")
    },


    get_license = function() {
      info <- private$page_tables[[1]] %>% rvest::html_table(fill = TRUE)
      vstr <- info[which(info[,1]=="License:"),2]
    },

    get_url = function() {
      return(private$url)
    },

    get_version_str = function() {
      info <- private$page_tables[[1]] %>% rvest::html_table(fill = TRUE)
      vstr <- info[which(info[,1]=="Version:"),2]
      return(vstr)
    },

    get_version = function() {
      return(unlist(strsplit(self$get_version_str(), "-"))[1])
    },

    get_release = function() {
      unp <- unlist(strsplit(self$get_version_str(), "-"))
      if (length(unp)==1) {
        return(1)
      } else {
        return(unp[2])
      }
    },

    get_source = function() {
      info <- private$page_tables[[2]] %>% rvest::html_table(fill = TRUE)
      pointer <- which(grepl("Package",info[,1]))
      filename <- info[pointer, 2]
      links <- private$page_tables[[2]] %>%
        rvest::html_nodes("tr") %>%
        rvest::html_nodes("a") %>% rvest::html_attr("href")
      path <- links[grepl(filename, links)]
      return(paste0(dirname(private$url), "/", path))

    },

    get_sys_reqs = function() {
      return(private$sys_reqs)
    },

    get_depends = function() {
      return(private$depends)
    },


    get_package_id = function(pkg) {
      return(private$strategy$get_feature(pkg))
    }


  ),

  private = list(
    pkgname = NA,
    htmlpage = NA,
    page_tables = NA,
    strategy = NA,
    suggestions = NULL,
    url = NA,
    sys_reqs = NULL,
    novels = NULL,
    depends = NULL,

    extract_links = function() {
      info <- private$page_tables[[1]] %>% rvest::html_table(fill = TRUE)
      #print(info)

      package_filter <- function(key) {
        if (key %in% info[,1]) {
          return(
            stringr::str_trim(unlist(
              strsplit(
                info[which(info[,1]==key),2], ",")))
          )
        }
        return(NULL)
      }


      clean_up <- function(fields, data) {
        data <- gsub("\\s\\(.*","",data)
        for (field in fields) {
          if (any(grepl(field, data))) {
            data <- data[-which(grepl(field, data))]
          }
        }
        return(data)
      }

      clean_up_filters <- c("^methods$", "^R$", "^utils$", "^datasets$", "^grid$", "^stats$", "^tools$", "^grDevices$", "^parallel$", "^graphics$", "^tcltk$")

      private$depends <- package_filter("Imports:")
      private$depends <- append(private$depends, package_filter("Depends:"))
      private$depends <- clean_up(clean_up_filters, private$depends)

      private$strategy$add_imports(private$depends)

      private$suggestions <- package_filter("Suggests:")
      private$novels <- private$strategy$add_imports(private$suggestions)
      private$suggestions <- clean_up(clean_up_filters, private$suggestions)

      # if ("Suggests:" %in% info[,1]) {
      #    private$suggestions <- stringr::str_trim(
      #      unlist(
      #        strsplit(
      #          info[which(info[,1]=="Suggests:"),2], ",")))
      #    private$novels <- private$strategy$add_imports(private$suggestions)
      #    print(private$novels)
      #  }
      #
      #
      # if ("SystemRequirements:" %in% info[,1]) {
      #   private$sys_reqs <- stringr::str_trim(
      #     unlist(
      #       strsplit(
      #         info[which(info[,1]=="SystemRequirements:"),2], ",")))
      #   private$strategy$add_sys_reqs(private$sys_reqs)

    },


    is_installable = function() {
      cli::cli_alert(
        stringr::str_interp("testing if package [${private$pkgname}] can be installed"))

      for (inst in private$depends) {
        if (private$strategy$is_installed(inst)) {
          cli::cli_alert_info(stringr::str_interp("[${inst}] required by [${private$pkgname}] has already been installed"))
        } else {
          cli::cli_alert_info(stringr::str_interp("[${inst}] requirement [${private$pkgname}] is still pending"))
          # recurse into the problem ...
          child <- PackYak$new(inst, strategy=private$strategy)
        }
      }

      return(TRUE)
    },


    install_package = function() {
      if (private$strategy$is_installed(private$pkgname)) {
        cli::cli_alert_info(stringr::str_interp("package [${private$pkgname}] has already been installed"))
      } else {
        cli::cli_alert_info(stringr::str_interp("installing package [${private$pkgname}]"))
        so <- SpecOps$new(self)
        private$strategy$register(private$pkgname, so$get_rpm())


        if (!is.null(private$suggestions)) {
          cli::cli_alert_info("reviewing suggested packages")
          # print(private$suggestions)
          for (pack in private$suggestions) {
            cli::cli_alert_info(stringr::str_interp("considering installation of package [${pack}]"))
            if (!private$strategy$is_installed(pack)) {
              cli::cli_alert_info(stringr::str_interp("[${pack}] remains uninstalled"))
              # child <- PackYak$new(pack, strategy=private$strategy)
            }
          }
        }
      }




    }

  )
)

