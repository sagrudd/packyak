
#' @export
Fedora = R6::R6Class(
  "Fedora",
  public = list(

    initialize = function() {
      cli::cli_alert("Fedora validation")
      private$sanity_check()
    },

    is_fedora = function() {
      return(private$validated_fedora)
    },

    get_architecture = function() {
      return(private$uname_m)
    },

    get_fedora_version = function() {
      gsub(
        "^.*\\.", "", gsub(paste0(".", private$uname_m), "", private$uname_r))
    },

    is_rpmbuild_available = function() {
      return(private$rpm_build)
    },

    get_rpmbuild_dir = function() {
      return(private$rpm_build_dir)
    }



  ),

  private = list(
      validated_fedora = FALSE,
      uname_m = NULL,
      uname_r = NULL,
      rpm_build = FALSE,
      rpm_build_dir = NULL,

      sanity_check = function() {

        tryCatch(
          private$is_uname_available(),
          error = function(e) {
            cli::cli_alert_danger(
              stringr::str_interp(
                "unable to call `uname` - is this even Linux?"))
          }
        )
        if (is.null(private$uname_m) || is.null(private$uname_m)) {
          return()
        }

        cli::cli_alert_info(
          stringr::str_interp("architecture: [${self$get_architecture()}]"))
        cli::cli_alert_info(
          stringr::str_interp("release:      [${self$get_fedora_version()}]"))

        if (file.exists("/etc/fedora-release") &&
            grepl("^fc", self$get_fedora_version())) {
          cli::cli_alert_success("This is a fedora system ....")
          private$validated_fedora <- TRUE
        } else if (file.exists("/etc/redhat-release") &&
                   grepl("^el", self$get_fedora_version())) {
          cli::cli_alert_info_success("This is a redhat system ....")
          return()
        } else {
          silent_stop(
            "Not sure what architecture this system is - please review")
        }

        private$check_rpm_build()
        cli::cli_alert_info(
          stringr::str_interp(
            "rpmbuild:     [${self$is_rpmbuild_available()}]"))
        cli::cli_alert_info(
          stringr::str_interp("rpmbuild_dir: [${self$get_rpmbuild_dir()}]"))

      },

      is_uname_available = function() {
          private$uname_m <- system2(
            "uname", c("-m"), stdout=TRUE, stderr=FALSE)
          private$uname_r <- system2(
            "uname", c("-r"), stdout=TRUE, stderr=FALSE)
      },


      check_rpm_build = function() {
        xx <- NULL
        tryCatch(
          xx <- system2("rpmbuild", c("--version"), stdout=TRUE, stderr=FALSE),
          error = function(e) {
            cli::cli_alert_danger(
              stringr::str_interp("syscmd [rpmbuild] not found"))
          }
        )
        if (!is.null(xx)) {
          private$rpm_build <- TRUE
          private$parse_rpm_topdir()
        }
      },

      parse_rpm_topdir = function() {
        if (!file.exists("~/.rpmmacros")) {
          private$rpm_build_dir = "~/rpmbuild"
        } else {
          cli::cli_alert_info("parsing [~/.rpmmacros]")
          lines <- readLines("~/.rpmmacros")
          topdirline <- lines[grepl("^%_topdir", lines)]
          # blunt regex here ... need complaints/criticisms...
          private$rpm_build_dir <-
            gsub("%_topdir %(echo $HOME)/", "~/", topdirline)
        }
      }

  )
)


