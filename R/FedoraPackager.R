
FedoraPackager = R6::R6Class(
  "FedoraPackager",
  public = list(

    initialize = function() {

    },

    is_fedora = function() {
      return(private$validated_fedora)
    },

    spec_dir = function() {
      return(
        path.expand(
          file.path(self$get_rpmbuild_dir(), "SPECS")))
    },

    get_rpmsource_dir = function() {
      return(file.path(self$get_rpmbuild_dir(), "SOURCES"))
    },

    get_rpmbuild_dir = function() {
      if (!file.exists("~/.rpmmacros")) {
        return("~/rpmbuild")
      } else {
        cli::cli_alert_info("parsing [~/.rpmmacros]")
        lines <- readLines("~/.rpmmacros")
        topdirline <- lines[grepl("^%_topdir", lines)]
        # blunt regex here ... need complaints/criticisms...
        return(
          gsub("%_topdir %\\(echo \\$HOME\\)/", "~/", topdirline))
      }
    }

  )

)
