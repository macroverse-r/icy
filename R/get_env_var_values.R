
get_env_var_values <- function(package,
                               var_names,
                               fn_local = NULL,
                               fn_tmpl = NULL,
                               tmpl2local_comp = NULL,
                               case_format = "snake_case") {

  fn_local_path <- get_config_yaml_path(package = package,
                                        fn_local = fn_local,
                                        fn_tmpl = fn_tmpl,
                                        tmpl2local_comp = tmpl2local_comp,
                                        case_format = case_format)


  local_config <- yaml::read_yaml(fn_local_path)

  env_var_values <- unlist(local_config$default[var_names])

  return(env_var_values)

}

