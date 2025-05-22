# @export
get_config_yaml_path_local <- function(package,
                                       fn_local = NULL,
                                       fn_tmpl = NULL,
                                       tmpl2local_comp = NULL,
                                       case_format = "snake_case") {
  if (is.null(fn_local)) {
    fn_local <- paste0(package, "_config_local.yml")
  }

  # Get paths to the template and local config files
  config_file_local <- file.path(
    system.file(package = package),
    fn_local
  )

  # Create local config file if it doesn't exist
  if (!file.exists(config_file_local)) {
    if (is.null(fn_tmpl)) {
      fn_tmpl <- paste0(package, "_config_template.yml")
    }

    # Read template and extract the relevant section
    config_file_template <- system.file(fn_tmpl, package = package)
    template_config <- yaml::read_yaml(config_file_template)
    if (!is.null(tmpl2local_comp)) {
      template_config <- template_config[[tmpl2local_comp]]
    }

    # Only keep the default section for the local config
    local_config <- template_config

    # Write just the default section to the local config file
    yaml::write_yaml(local_config, config_file_local)
  }

  return(config_file_local)
}
