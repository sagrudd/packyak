
#' @export
PackYak = R6::R6Class(
  "PackYak",
  public = list(

    initialize = function(
      pkgname="rpm_targets.yaml",
      strategy=NULL,
      build_rpm=FALSE,
      overwrite=FALSE,
      follow_suggests=FALSE,
      fedora=NULL,
      context=NULL) {

      #system_installed = c()

      #if (pkgname %in% system_installed) return()
      if (pkgname == "rpm_targets.yaml" && !file.exists("rpm_targets")) {
        pkgname <- system.file("extdata/rpm_targets.yaml", package="packyak")
      }


      cli::cli_alert(
        stringr::str_interp(
          "creating RPM file from package [{pkgname}]"))

      # if not a fedora system - quit
      if (is.null(fedora))
        fedora <- Fedora$new()
      if (!fedora$is_fedora())
        silent_stop("This workflow is only intended for Fedora systems")

      private$package_name <- pkgname
      if (is.null(strategy)) {
        private$strategy <- PackageStrategy$new()
      } else {
        private$strategy <- strategy
        private$strategy$print()
      }

      if (is.character(pkgname) && length(pkgname) > 1) {
        cli::cli_alert_info("processing a vector of entries ...")
        for (p in pkgname) {
          child <- PackYak$new(
            p, strategy=private$strategy, build_rpm=build_rpm,
            overwrite=overwrite, follow_suggests=follow_suggests,
            fedora=fedora)
        }
      } else {
        # single entity ... is it a file?
        if (file.exists(pkgname)) {
          cli::cli_alert_info("This looks like a file - trying to extract YAML")
          yaml_content <- yaml::yaml.load_file(pkgname)
          if ("PackYak" %in% names(yaml_content)) {
            root <- yaml_content[["PackYak"]]
            if (fedora$get_architecture() %in% names(root)) {
              targets <- root[[fedora$get_architecture()]]
              for (target in targets) {
                child <- PackYak$new(
                  target, strategy=private$strategy, build_rpm=build_rpm,
                  overwrite=overwrite, follow_suggests=follow_suggests,
                  fedora=fedora)

              }
            } else {
              silent_stop(
                stringr::str_interp(
                  "YAML does not contain [${fedora$get_architecture()}] arch element"))
            }
          } else {
            silent_stop("YAML does not contain PackYak root element")
          }

          silent_stop("END")
        } else {



            if (self$is_bioconductor_resource()) {
              cli::cli_alert_success("successfully loaded a Bioconductor page")
            } else if (self$is_cran_resource()) {
              cli::cli_alert_success("successfully loaded a CRAN page")
            } else if (self$is_bioconductor_annotation()) {
              cli::cli_alert_success("successfully loaded a Bioconductor annotation page")
            } else if (self$is_bioconductor_experiment()) {
              cli::cli_alert_success("successfully loaded a Bioconductor experiment page")
            } else {
              silent_stop("This package cannot be found at BioC or CRAN")
            }



          if (!is.null(private$package_page)) {
            cli::cli_alert_success("Proto-package object created")
            private$package_page$install_package(
              build_rpm=build_rpm, overwrite=overwrite, fedora=fedora,
              follow_suggests=follow_suggests)
            if (follow_suggests) {
              private$package_page$install_suggestions(
                build_rpm=build_rpm, overwrite=overwrite, fedora=fedora,
                follow_suggests=follow_suggests)
            }
          }
        }
      }

    },

    is_cran_resource = function() {
      cran <- stringr::str_interp(
        "https://cran.r-project.org/web/packages/${private$package_name}/index.html")
      cli::cli_alert(stringr::str_interp("checking cran [{cran}]"))
      lookup <- httr::GET(cran)
      if (lookup$status_code == 200) {
        private$package_page <- Cran$new(
          pkgname=private$package_name, htmlpage=lookup,
          strategy=private$strategy, url=cran)
        return(TRUE)
      }
      return(FALSE)
    },

    is_bioconductor_resource = function() {
      bioc <- stringr::str_interp(
        "https://www.bioconductor.org/packages/release/bioc/html/${private$package_name}.html")
      cli::cli_alert(stringr::str_interp("checking bioc [{bioc}]"))
      lookup <- httr::GET(bioc)
      if (lookup$status_code == 200) {
        private$package_page <- Bioconductor$new(
          pkgname=private$package_name, htmlpage=lookup,
          strategy=private$strategy, url=bioc)
        return(TRUE)
      }
      return(FALSE)
    },


    is_bioconductor_annotation = function() {
      bioc <- stringr::str_interp(
        "https://bioconductor.org/packages/release/data/annotation/html/${private$package_name}.html")
      cli::cli_alert(stringr::str_interp("checking bioc - annotation - [{bioc}]"))
      lookup <- httr::GET(bioc)
      if (lookup$status_code == 200) {
        private$package_page <- Bioconductor$new(
          pkgname=private$package_name, htmlpage=lookup,
          strategy=private$strategy, url=bioc)
        return(TRUE)
      }
      return(FALSE)
    },

    is_bioconductor_experiment = function() {
      bioc <- stringr::str_interp(
        "https://bioconductor.org/packages/release/data/experiment/html/${private$package_name}.html"
      )
      cli::cli_alert(stringr::str_interp("checking bioc - experiment - [{bioc}]"))
      lookup <- httr::GET(bioc)
      if (lookup$status_code == 200) {
        private$package_page <- Bioconductor$new(
          pkgname=private$package_name, htmlpage=lookup,
          strategy=private$strategy, url=bioc)
        return(TRUE)
      }
      return(FALSE)
    }



  ),

  private = list(
    strategy = NA,
    package_name = NA,
    package_page = NULL
  )
)


rversion <- function() {
  paste(sessionInfo()$R.version$major, sessionInfo()$R.version$minor,sep=".")
}



silent_stop <- function(message=NULL) {
  if (!is.null(message))
    cli::cli_alert_danger(message)
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop("\r ", call.=FALSE)
}


