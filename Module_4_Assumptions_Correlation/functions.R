stat.desc.clean <- function(dataset, variable, ...){
  variable <- enquo(variable)
  groups <- enquos(...)
  
  if(is.null(groups)){
    dataset %>% 
      group_by(!!!groups) %>% 
      pull(!!variable) %>% 
      stat.desc(basic = FALSE, desc = FALSE, norm = TRUE) %>% 
      enframe() %>% 
      pivot_wider(names_from = name, values_from = value)
  } else{
    dataset %>% 
      group_by(!!!groups) %>% 
      group_modify(~ enframe(stat.desc(.x %>% pull(!!variable), basic = FALSE, desc = FALSE, norm = TRUE))) %>% 
      pivot_wider(names_from = name, values_from = value)
  }
}