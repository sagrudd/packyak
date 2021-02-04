
#' @export
Fedora = R6::R6Class(
  "Fedora",
  public = list(

    initialize = function() {
      cli::cli_h1("Fedora validation")
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
    },

    get_rpmsource_dir = function() {
      return(file.path(self$get_rpmbuild_dir(), "SOURCES"))
    },

    spec2rpm = function(package, specfile, architecture=NULL) {
      cli::cli_h2(stringr::str_interp("building RPM package from [${specfile}]"))

      if (is.null(architecture)) {
        architecture <- self$get_architecture()
      }
      fname <- tools::file_path_sans_ext(basename(specfile))
      version <- package$get_version()
      release <- package$get_release()
      fedora_release <- self$get_fedora_version()

      RPM <- file.path(
        self$get_rpmbuild_dir(),
        "RPMS",
        architecture,
        paste0(fname, "-", version, "-", release, ".", fedora_release, ".", architecture, ".rpm"))
      cli::cli_alert(stringr::str_interp("target = [${RPM}]"))



      if (!file.exists(RPM)) {
        command1 <- stringr::str_interp("spectool -g -R ${specfile}")
        status1 <- system(command1)
        if (!status1 == 0) {
          silent_stop(stringr::str_interp("command ${command1} exited with fail code [${status1}]"))
        }

        command2 <- stringr::str_interp("rpmbuild -ba ${specfile}")
        status2 <- system(command2)
        if (!status2 == 0) {
          silent_stop(stringr::str_interp("command ${command2} exited with fail code [${status2}]"))
        }
      }

      if (file.exists(RPM)) {
        pack_id <- paste0(fname, ".", architecture)
        cli::cli_alert(stringr::str_interp("Trying to install package [${pack_id}]"))
        pack_id_row <- which(grepl(pack_id, private$installed_packages$V1))

        if (length(pack_id_row) > 0) {
          install_row <- private$installed_packages[(which(grepl(pack_id, private$installed_packages$V1))),]
          print(install_row)
          vstr <- paste0(version, "-", release, ".", fedora_release)
          if (vstr %in% install_row[,2]) {
            cli::cli_alert_success(stringr::str_interp("package [${pack_id}] version [${vstr}] already installed"))
            return()
          } else {
            cli::cli_alert_success(stringr::str_interp("package [${pack_id}] already installed but with [${install_row[,2]}] now[${vstr}]"))
          }
        }

        command3 <- stringr::str_interp("sudo yum install -y ${RPM}")
        status3 <- system(command3)
        if (!status3 == 0) {
          cli::cli_alert_warning(stringr::str_interp("command ${command3} exited with fail code [${status3}]"))
          stop()
        }
      } else {

      }

      #silent_stop("development stop")
    },


    check_prior_art = function(pkgname, skip=TRUE) {

      if (skip) {
        return(NULL)
      }

      cli::cli_alert_info(stringr::str_interp("looking for package [${pkgname}]"))

      lookup_key <- paste0("(?i)^", pkgname, ".(noarch|", self$get_architecture(),")")

      keys <- which(stringr::str_detect(private$prior_art$V1, lookup_key))
      if (length(keys)==0) {
        return(NULL)
      } else {
        response <- private$prior_art[keys,]
        print(response)
        return(response)
      }

    },


    package_sync = function(external_package) {
      system(stringr::str_interp("sudo yum -y install ${external_package}"))
      system(stringr::str_interp("sudo yum -y update ${external_package}"))
    }



  ),

  private = list(
      validated_fedora = FALSE,
      uname_m = NULL,
      uname_r = NULL,
      rpm_build = FALSE,
      rpm_build_dir = NULL,
      installed_packages = NULL,
      prior_art = NULL,

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

        private$installed_packages <-
          tibble::as_tibble(
            matrix(
              unlist(
                unlist(
                  lapply(
                    stringr::str_trim(
                      system("yum list installed", intern=TRUE)[-1]),
                    strsplit, "\\s+"))),
              ncol=3,
              byrow=TRUE, dimnames=list(NULL, c("V1", "V2", "V3"))))

        print(private$installed_packages)

        lines <- system("dnf list all", intern=TRUE)[-c(1)]
        lines <- lines[!lines %in% c("Available Packages", "Installed Packages")]
        items <- lines %>%
          stringr::str_split("\\s+", n=3) %>%
          purrr::flatten() %>%
          stringr::str_trim()
        private$prior_art <-
          tibble::as_tibble(matrix(items, ncol=3, byrow=TRUE, dimnames=list(NULL, c("V1", "V2", "V3"))))
        # and strip out the locally installed resources - they complicate ...
        private$prior_art <-
          private$prior_art[
            which(!stringr::str_detect(private$prior_art$V3, "commandline")),]


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
            gsub("%_topdir %\\(echo \\$HOME\\)/", "~/", topdirline)
        }
      }

  )
)


