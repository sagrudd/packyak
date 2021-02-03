
PackageHandler = R6::R6Class(
  "PackageHandler",
  public = list(

    get_pkg_name = function() {
      return(private$pkgname)
    },

    get_repo = function() {
      return(private$repo_name)
    },

    get_license = function() {
      return(private$license)
    },

    get_url = function() {
      return(private$url)
    },

    get_version_str = function() {
      return(private$version_str)
    },

    get_version = function() {
      return(unlist(strsplit(private$version_str, "-"))[1])
    },

    get_release = function() {
      if (!is.null(private$release)) {
        return(private$release)
      }

      unp <- unlist(strsplit(private$version_str, "-"))
      if (length(unp)==1) {
        return(1)
      } else {
        return(unp[2])
      }
    },

    get_sys_reqs = function() {
      return(private$sys_reqs)
    },

    get_depends = function() {
      return(private$depends)
    },


    get_package_id = function(pkg) {
      return(private$strategy$get_feature(pkg))
    },

    get_source = function() {
      return(private$source_code)
    },

    install_package = function(build_rpm, overwrite, fedora, follow_suggests) {
      if (self$prior_art(fedora)) { # publicly available ...
        private$strategy$register(private$pkgname, self$canonical_name)
        # and ensure that the package is actually installed
        fedora$package_sync(self$canonical_name)
      }

      private$assess_dependencies(
        overwrite=overwrite, build_rpm=build_rpm,
        fedora=fedora, follow_suggests=follow_suggests)

      spec_o <- private$install_me(overwrite, fedora=fedora)

      print(spec_o)

      if (build_rpm && !is.null(spec_o)) {
        if (self$is_noarch()) {
          fedora$spec2rpm(self, spec_o$get_spec_file(), "noarch")
        } else {
          fedora$spec2rpm(self, spec_o$get_spec_file())
        }
      }
    },

    install_suggestions = function(build_rpm, overwrite, fedora, follow_suggests) {
      private$assess_packages(
        private$novels, overwrite=overwrite, build_rpm=build_rpm,
        fedora=fedora, follow_suggests=follow_suggests)
    },

    prior_art = function(fedora, force_update=FALSE) {
      # first step - check for prior art ...
      cli::cli_h1(stringr::str_interp("Checking for prior art [${private$pkgname}]..."))

      prior_art <- fedora$check_prior_art(self$canonical_name)
      if (!is.null(prior_art)) {
        if (nrow(prior_art) > 1) {
          silent_stop("prior art ambiguity - please resolve code")
        }

        build_arch <- as.character(prior_art[1,1]) %>% stringr::str_extract("(?<=\\.).*")
        if (build_arch == "noarch") {
          private$noarch <- TRUE
        }
        version <- as.character(prior_art[1,2]) %>% stringr::str_extract("^[^-]*")
        version <- stringr::str_replace(version, "1:", "") # may be a fix for e.g. numpy ...

        patch <- as.character(prior_art[1,2]) %>% stringr::str_extract("(?<=-)[^\\.]*")

        cli::cli_alert_info(stringr::str_interp("buildarch : [${build_arch}]"))
        cli::cli_alert_info(stringr::str_interp("version   : [${version}]"))
        cli::cli_alert_info(stringr::str_interp("patch     : [${patch}]"))

        cli::cli_alert_info(stringr::str_interp("defined - package == ${self$get_version()}"))

        deltaversion <- utils::compareVersion(version, self$get_version())
        if (deltaversion >= 0 || force_update == FALSE) { # -1 when b is later than a
          private$release <- as.integer(patch) + 1
          cli::cli_alert_success("Prior art *has* been established")
          return(TRUE)
        }
      }
      cli::cli_alert_success("Prior art has not been established")
      return(FALSE)
    },

    is_noarch = function() {
      return(private$noarch)
    }

  ),

  active = list(
    language = function(set=NULL) {
      if (!is.null(set)) {
        private$packlanguage <- set
      }
      return(private$packlanguage)
    },

    canonical_name = function(set) {
      xname <- private$pkgname
      if (xname == "python-dateutil")
        xname <- "dateutil"
      else if (xname == "PyYAML")
        xname <- "pyyaml"

      if (self$language=="Python") {
        return(paste0("python3-", xname))
      } else if (self$language=="R") {
        return(paste0("r_", xname))
      } else {
        silent_stop("Not an expected language")
      }
    }
  ),

  private = list(
    packlanguage = "R",
    pkgname = NA,
    repo_name = NA,
    htmlpage = NA,
    url = NA,
    strategy = NA,
    page_tables = NA,
    license = NA,
    version_str = NA,
    suggestions = NULL,
    sys_reqs = NULL,
    novels = NULL,
    depends = NULL,
    source_code = NA,
    noarch = FALSE,
    release = NULL,
    clean_up_filters = c(
      "methods", "R", "Matrix", "mgcv", "nlme", "MASS", "utils",
      "splines", "lattice", "datasets", "grid", "stats", "tools",
      "grDevices", "parallel", "graphics", "tcltk", "stats4", "rpart",
      "codetools", "nnet"),


    get_value_from_table = function(key, table) {
      if (any(grepl(key, table[,1]))) {
        rows <- which(grepl(key, table[,1]))
        return(table[rows,2])
      } else {
        return(NULL)
      }
    },

    get_download_location = function(lookup_key, lookup_table) {
      download_table <- lookup_table %>% rvest::html_table(fill = TRUE)
      row_pointer <- which(grepl(lookup_key, download_table[,1]))
      download_filename <- download_table[row_pointer, 2]
      download_paths <- lookup_table %>% rvest::html_nodes("a") %>% rvest::html_attr("href")

      if (length(download_filename) > 1) {
        print(download_filename)
        silent_stop("FUBAR")
      }

      selected_path <- download_paths[grepl(download_filename, download_paths)]
      return(paste0(dirname(private$url), "/", selected_path))
    },

    referenced_package_filter = function(key, table) {

      clean_up <- function(fields, data) { # fields is the query
        data <- stringr::str_trim(gsub("\\(.*","",data))
        if (any(fields %in% data)) {
          data <- data[-which(data %in% fields)]
        }
        return(stringr::str_trim(data))
      }

      if (key %in% table[,1]) {
        return(
          clean_up(
            private$clean_up_filters,
            stringr::str_trim(
              unlist(
                strsplit(
                  table[which(table[,1]==key),2], ","))))
        )
      }
      return(NULL)
    },

    assess_dependencies = function(overwrite, build_rpm, fedora, follow_suggests) {
      cli::cli_alert(
        stringr::str_interp("testing if package [${private$pkgname}] can be installed"))

      private$assess_packages(
        private$depends, overwrite=overwrite, build_rpm=build_rpm,
        fedora=fedora, follow_suggests=follow_suggests)
    },

    assess_packages = function(packages_to_assess, overwrite, build_rpm, fedora, follow_suggests) {
      for (inst in packages_to_assess) {
        if (private$strategy$is_installed(inst)) {
          cli::cli_alert_info(stringr::str_interp("[${inst}] required by [${private$pkgname}] has already been installed"))
        } else {
          cli::cli_alert_info(stringr::str_interp("[${inst}] requirement [${private$pkgname}] is still pending"))
          # recurse into the problem ...

          child <- PackYak$new(
            inst, strategy=private$strategy, overwrite=overwrite,
            build_rpm=build_rpm, fedora=fedora, follow_suggests=follow_suggests,
            context=self$language)
        }
      }
    },


    install_me = function(overwrite, fedora) {

      if (private$strategy$is_installed(private$pkgname)) {
        cli::cli_alert_info(stringr::str_interp("package [${private$pkgname}] has already been installed"))
        return(NULL)
      } else {
        cli::cli_alert_info(stringr::str_interp("installing package [${private$pkgname}]"))
        so <- SpecOps$new(self, fedora=fedora, overwrite=overwrite)
        private$strategy$register(private$pkgname, so$get_rpm())
        return(so)
      }
    }
  )
)
