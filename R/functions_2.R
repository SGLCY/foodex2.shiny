
# New functions for Occurrence in FoodEx2 ----

#' Retreives the selected food codes from the treelist
#' Creates a table of selected food items with `termCode` and `level`
#' @param occur_list The list of the selected food items.Derived from \code{data.tree::get_selected(tree)}
#' @details Needs the `mtx_levels` dataframe
#' @noRd
get_selected_termCodes <- function(occur_list){
  
  # Helper to get the food, chain and parent from each list item
  get_food_item <- function(key) {
    
    food <- key[1]
    
    chain <- attr(key,"ancestry")
    
    chain_conc <- paste(chain, collapse = "/")
    parent <-  tail(chain, 1)
    
    return(
      c(
        food = food,
        chain = chain_conc,
        parent = parent
      )
    )
  }
  
  result <- purrr::map_df(occur_list, get_food_item) 
  
  
  final_table <- 
    result %>% 
    mutate(
      food_item = if_else(food == "Overall", parent, food)
    ) %>% 
    distinct(food_item) %>% 
    left_join(mtx_levels, by= c("food_item" = "termExtendedName")) %>% 
    rename(level = depth)
  
  
  final_table
}


#' Creates the tree like df for occurrence and the node counts
#' @param occurrence The occurrence dataframe
#' @details Needs `fdx2_chain_name`, `fdx2_chain_code`
get_occurrence_nodes <- function(occurrence){
  
  occurrence <- 
    occurrence %>% 
    mutate(
      termCode = str_extract(foodex2, "^.{5}")
    ) %>% 
    # left_join(mtx_levels, by = "termCode") %>% 
    filter(!is.na(termCode))
  
  occurrence_tree_name <- 
    occurrence %>% 
    distinct(termCode) %>% 
    left_join(fdx2_chain_name,by = "termCode")
  
  occurrence_tree_code <- 
    occurrence %>% 
    distinct(termCode) %>% 
    left_join(fdx2_chain_code, by = "termCode")
  
  
  fdx_levels <- paste0("level", 1:7)
  
  cat("Calculting node counts\n")
  node_counts_list <- 
    map(fdx_levels , ~ {
      
      level = .x
      
      occurrence %>% 
        count(termCode) %>% 
        left_join(fdx2_chain_name) %>% 
        group_by(.data[[level]]) %>% 
        summarise(n = sum(n)) %>% 
        ungroup() %>% 
        rename(termExtendedName = {{level}}) %>% 
        left_join(mtx_levels[c("termCode", "termExtendedName")])
      
      
    }) %>% 
    set_names(c(1:7))
  
  node_counts <- 
    node_counts_list %>% 
    bind_rows(.id = "level") %>% 
    mutate(level = as.numeric(level))
  
  
  cat("Out the get_occurece_nodes")
  out <- lst(
    occurrence_tree_code,
    occurrence_tree_name,
    node_counts
  )
  
  out
}



#' Suppreses the NA's in paste
#' Used to create the path for data.tree
#' @details See https://stackoverflow.com/questions/43803949
paste5 <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))
      
      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}



#' Aggregates to a specific level
#' @param occurrence_df The raw occurrence data
#' @param occurrence_tree_code The df format tree structure of the occurrence. The codes
#' @param level String. What level shuld the aggregation be done. level1, level2, ..etc.
#' @details Needs the 
#' a) `occurrence_summary`. Te list tht contains the aggregation functions/stats.
#' b) mtx_levels
#' b) `occurrence_df` needs to have the `res_num` column
aggregate_by_level <- function(occurrence_df, occurrence_tree_code, level){
  
  occurrence_df %>% 
    select(termCode, termExtendedName, res_num, lod) %>% 
    # Substitution of 0 values
    mutate(
      LB = res_num,
      MB = if_else(res_num == 0 , lod/2, res_num),
      UB = if_else(res_num == 0 , lod, res_num),
    ) %>% 
    left_join(occurrence_tree_code) %>% 
    #filter(depth == 2) %>% 
    group_by(.data[[level]]) %>% 
    summarise(
      N = n(),
      across(c(LB, MB, UB), occurrence_summary)
    ) %>% 
    ungroup() %>% 
    rename(termCode = {{level}})
}


#' Createss the occurrence tree in a list format for viewing with \code{shinyTree}
#' @param occurrence_tree_name The df format occurence treewith the names of the food items
create_occurrence_TreeList <- function(occurrence_tree_name) {
  
  #check that the occurrence_tree_name has the names
  #level1,...to , level7
  temp <- 
    occurrence_tree_name %>% 
    select(starts_with("level")) 
  
  temp$pathString <- paste5("p", 
                            temp$level1, 
                            temp$level2,
                            temp$level3,
                            temp$level4,
                            temp$level5,
                            temp$level6,
                            temp$level7,
                            sep = "//",
                            na.rm = TRUE)       
  
  # in FoodEx2 there are some items denoted with `/`, eg.A02JK - Canned/jarred smoked fish in oil-deprecated
  #Thats why the `//` delimiter
  htree <- as.Node(temp %>% select(pathString), pathDelimiter = "//", na.rm=TRUE)
  
  occurrence_TreeList <- data.tree::ToListSimple(htree, nameName = "Overall")
  
  occurrence_TreeList$Overall <- NULL
  
  occurrence_TreeList
  
}



summarise_weighted <- function(data, ref_value = NULL){
  
  # Note the ref_value  
  if(is.null(ref_value)){
    ref_value <- NA
  }
  
  data %>% 
    dplyr::summarise(
      #N           = dplyr::n(),
      Min         = min(exposure, na.rm = TRUE),
      Max         = max(exposure, na.rm = TRUE),
      Mean        = Hmisc::wtd.mean(exposure, wcoeff),
      SD          = sqrt(Hmisc::wtd.var(exposure, wcoeff)),
      #  For SE see the discussion 
      #R https://stats.stackexchange.com/questions/25895/computing-standard-error-in-weighted-mean-estimation
      
      #SE         = wtd.std*sqrt(sum(wcoeff))/sum(wcoeff),
      P25         = Hmisc::wtd.quantile(exposure, wcoeff, probs = 0.25),
      Median      = Hmisc::wtd.quantile(exposure, wcoeff, probs = 0.50),
      P75         = Hmisc::wtd.quantile(exposure, wcoeff, probs = 0.75),
      P95         = Hmisc::wtd.quantile(exposure, wcoeff, probs = 0.95),
      # % above reference value
      pctOver     = sum(wcoeff[exposure>ref_value])/sum(wcoeff),
      MOE         = ref_value / Mean
    )
  
}


#'Turn a vector of numerics into percentages
#'@param ...  Arguments passed to scales::percent()
#'@param x A numeric vector
#'@noRd
#'@return A vector of percenages same elngth as x
percent <- function(x, ...){
  scales::label_percent(...)(x)
  
}


#'Label the reference value
#'@details if the user does not provide ref value then we have NA
#'@param ref_value Numeric. The reference value
#'@noRd
label_ref_value <- function(ref_value, ...){
  
  if(not_null(ref_value)){
    label <- glue::glue("Reference value: {ref_value} μg/Kw b.w.")
    
    stringr::str_wrap(label, ...)
  } else NULL
  
}


#' Finds t
#' @param consumption The consumption data.
#' @param facet String. The facet
#' @param description A tibble with termCode, termExtendedName for the facet. Can my mtx_levels
#' for the ingredients or a kew table for the processs facets with the same column names
create_facet_table <- function(consumption, facet, description){
  #browser()
  #facet = "F04"
  
  # TODO Check
  # facet is valid
  # neededc colums in the dataset
  
  facet_description <- NULL
  
  if(facet == "F04"){
    facet_description <- select(description,termCode, facet_name = termExtendedName)
  }
  
  if(facet == "F28"){
    facet_description <- select(description,termCode, facet_name = termExtendedName)
  }
  
  if(!is.null(facet_description)){
    
    consumption %>% 
      # select(ORSUBCODE,RECORDIDENTIFIER, 
      #        DAY,
      #        FOODEXCODE, ORFOODCODE,
      #        AMOUNTFRAW,
      #        AMOUNTRECIPE,ENRECIPEDESC,
      #        ORFOODNAME, fdx2_name
      # ) %>% 
      mutate(
        N = str_count(FOODEXCODE, facet)
      ) %>% 
      filter(N>0) %>% 
      mutate( 
        facet =  str_extract_all(FOODEXCODE,paste0("(?<=", facet,"\\.)[a-zA-Z0-9]{5}")),
        #f04 = str_extract_all(FOODEXCODE,"(?<=F04\\.)[a-zA-Z0-9]{5}")
      ) %>% 
      unnest_longer(facet) %>% 
      left_join(
        facet_description,
        by = c("facet"= "termCode")
      ) %>% 
      filter(!is.na(facet_name))
    
    
  } else {
    cat("No valid facet\n")
    
  }
  
}
# Example
# cadmium_tree <- readRDS(here("SampleData/cadmium_tree.rds"))
# 
# temp <- 
#   cadmium_tree %>% 
#   select(starts_with("level")) %>% 
#   #rename_with(~paste0("level", .), -level) %>% 
#   select(-level7)
# 
# 
# 
# 
# temp$pathString <- paste5("p", 
#                           temp$level1, 
#                           temp$level2,
#                           temp$level3,
#                           temp$level4,
#                           temp$level5,
#                           temp$level6,
#                           temp$level7,
#                           sep = "/",
#                           na.rm = TRUE)       
# 
# htree <- as.Node(temp %>% select(pathString), na.rm=TRUE)
# 
# htree$attributesAll
# 
# print(htree, "level")
# 
# class(htree)
# 
# cadmium_tree_list <- ToListSimple(htree, nameName = "Overall")
# 
# #cadmium_tree_list <- ToListExplicit(htree,unname = TRUE)
# 
# saveRDS(cadmium_tree_list, here::here("SampleData/cadmium_tree_list.rds"))
# 
# 
# data.tree::FromDataFrameTable(temp)



