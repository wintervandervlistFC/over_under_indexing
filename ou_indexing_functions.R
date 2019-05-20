get_perc_of_univ <- function(dataf, grouping_vars){
  
  dataf <- dataf %>%
    
    select(prospect_id, grouping_vars, funding_circle_identifier) %>% 
    
    unique() %>% 
    
    group_by_(.dots = setNames(as.list(grouping_vars), grouping_vars)) %>% 
    
    summarize(n_prospects = n_distinct(prospect_id),
              n_signups = sum(ifelse(!is.na(funding_circle_identifier),1,0), 
                              na.rm = T)) %>% 
    
    ungroup() %>% 
    
    mutate(perc_of_universe = n_signups/n_prospects) %>% 
    
    collect()
  
  return(dataf)
}

get_match_city <- function(inp_city, ref_cities){
  
  dist_vec <- stringdist::stringdist(tolower(inp_city) , 
                                     tolower(ref_cities))
  
  if(min(dist_vec) < 1){
    output_city <- city_data$city[which.min(dist_vec)]
  }else{
    output_city <- "no match"
  }
  return(output_city)
}