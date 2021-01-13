
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

    install_package = function(build_rpm, overwrite, fedora_release=NULL) {
      private$assess_dependencies(overwrite=overwrite, build_rpm=build_rpm)

      spec_o <- private$install_me(overwrite)


      if (build_rpm) {
        cli::cli_alert_info(stringr::str_interp("Trying to build RPM package with [${spec_o$get_spec_file()}]"))

        # where will the RPM file be found?
        architecture <- system("uname -m", intern = TRUE)
        fname <- tools::file_path_sans_ext(basename(spec_o$get_spec_file()))
        version <- self$get_version()
        release <- self$get_release()
        RPM <- file.path(
          "RPMS",
          architecture,
          paste0(fname, "-", version, "-", release, ".", fedora_release, ".", architecture, ".rpm"))

        if (spec_o$is_updated() || !file.exists(RPM)) {

          command1 <- stringr::str_interp("spectool -g -R ${spec_o$get_spec_file()}")
          status1 <- system(command1)
          if (!status1 == 0) {
            cli::cli_alert_warning(stringr::str_interp("command ${command1} exited with fail code [${status1}]"))
            stop()
          }

          command2 <- stringr::str_interp("rpmbuild -ba ${spec_o$get_spec_file()}")
          status2 <- system(command2)
          if (!status2 == 0) {
            cli::cli_alert_warning(stringr::str_interp("command ${command2} exited with fail code [${status2}]"))
            stop()
          }

          if (is.null(fedora_release)) {
            fedora_release <- gsub("^.*\\.", "", gsub(paste0(".", architecture), "", system("uname -r", intern=TRUE)))
          }

          cli::cli_alert_info(stringr::str_interp("checking for RPM at [${RPM}]"))
          if (!file.exists(RPM)) {
            cli::cli_alert_danger("RPM file not found")
            stop()
          } else {
            command3 <- stringr::str_interp("sudo yum install -y ${RPM}")
            status3 <- system(command3)
            if (!status3 == 0) {
              cli::cli_alert_warning(stringr::str_interp("command ${command3} exited with fail code [${status3}]"))
              stop()
            }
          }
        }

        # command1 <- stringr::str_interp("spectool -g -R ${private$spec_file}")
        # print(command1)
        # system(command1)
        # system(stringr::str_interp("rpmbuild -ba ${private$spec_file}"))
        # file <- tools::file_path_sans_ext(basename(private$spec_file))
        # print(file)
        # path <- Sys.glob(paste0("RPMS/aarch64/",file,"*"))
        # print(path)
        # system(stringr::str_interp("sudo yum install -y ${path}"))
        # stop()
      }
    }

  ),

  private = list(

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
    clean_up_filters = c(
      "methods", "R", "Matrix", "mgcv", "nlme", "MASS", "utils",
      "splines", "lattice", "datasets", "grid", "stats", "tools",
      "grDevices", "parallel", "graphics", "tcltk", "S4Vectors", "stats4"),


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

    assess_dependencies = function(overwrite, build_rpm) {
      cli::cli_alert(
        stringr::str_interp("testing if package [${private$pkgname}] can be installed"))

      for (inst in private$depends) {
        if (private$strategy$is_installed(inst)) {
          cli::cli_alert_info(stringr::str_interp("[${inst}] required by [${private$pkgname}] has already been installed"))
        } else {
          cli::cli_alert_info(stringr::str_interp("[${inst}] requirement [${private$pkgname}] is still pending"))
          # recurse into the problem ...

          child <- PackYak$new(inst, strategy=private$strategy, overwrite=overwrite, build_rpm=build_rpm)
        }
      }
    },


    install_me = function(overwrite) {
      if (private$strategy$is_installed(private$pkgname)) {
        cli::cli_alert_info(stringr::str_interp("package [${private$pkgname}] has already been installed"))
      } else {
        cli::cli_alert_info(stringr::str_interp("installing package [${private$pkgname}]"))
        so <- SpecOps$new(self, overwrite=overwrite)
        private$strategy$register(private$pkgname, so$get_rpm())
        return(so)
      }
    }
  )
)
