

#'The table of subjecs
#'@param consumption The uploaded consumption file
#'@param ... Unused yet
#'@noRd
create_tbl_subjects_fdx2 <- function(consumption, ...){
  
  #dt_names <- names(consumption)
  
  #  food occasion related columns
  #food_rel_names <- c("serial","day", "amountfood", "foodex_l4_code","foodname")
  
 # subject_vars <- setdiff(dt_names, food_rel_names)
  
  subject_vars <- c("subjectid" ,"age","weight", "area", "pop_class", "wcoeff")
  
  consumption_days <- 
    consumption %>% 
    dplyr::group_by(subjectid) %>% 
    dplyr::summarise(cons_days  = dplyr::n_distinct(day)) %>% 
    dplyr::ungroup()
  
  
  tbl_subjects <- 
    consumption %>% 
    dplyr::select(
      dplyr::all_of(subject_vars)
    ) %>% 
    distinct(subjectid, .keep_all = TRUE) %>% 
    dplyr::left_join(consumption_days, by = "subjectid")
  
  
  tbl_subjects
}

# Merge Consumption and Occurrence files

#'@param consumption A tibble of the consumption 
#'@param occurrence A tibble of the aggregated occurrence
#'@param ... Unused yet
#'@details case_when rocks! First checks for level 7, then level 6, etc..
#' Always the lowest level is preferred in case we have occurrenc from the parent of an item
#'@noRd
create_tbl_merged_fdx2 <- function(consumption, occurrence, fdx2_chain_hierararchy,mtx_levels,...){
  
  occur <- occurrence
  consumption <- 
    consumption %>% 
    mutate(
      termCode =  str_extract(FOODEXCODE, "^.{5}"),
      .before =  1
    ) %>% 
    left_join(
      fdx2_chain_hierararchy
      , by = "termCode"
    ) %>% 
    rename_all(tolower)
  
  # Add foodex levels
  #' Indicate the `level` and `termCode` for each food occassion.
  #' Extract these from what is indicted in the `occurrence` dataset 
  with_foodex <- 
    consumption %>% 
    mutate(
      
      termCode_used  = case_when(
        
        level7 %in% unique(filter(occur,level =="level7")[["termCode"]]) ~ level7,
        level6 %in% unique(filter(occur,level =="level6")[["termCode"]]) ~ level6,
        level5 %in% unique(filter(occur,level =="level5")[["termCode"]]) ~ level5,
        level4 %in% unique(filter(occur,level =="level4")[["termCode"]]) ~ level4,
        level3 %in% unique(filter(occur,level =="level3")[["termCode"]]) ~ level3,
        level2 %in% unique(filter(occur,level =="level2")[["termCode"]]) ~ level2,
        level1 %in% unique(filter(occur,level =="level1")[["termCode"]]) ~ level1,
        TRUE ~ NA_character_
        
      )
    ) %>% 
    # Level Used- use the termCode_used
    left_join(
      mtx_levels %>% 
        transmute(termCode_used = termCode, level_used = paste0("level",depth ))
      , by= "termCode_used"
    ) 
  
  # Add occurrence ####
  with_occurrence <- 
    with_foodex %>% 
    left_join(
      occur %>% 
        rename(termExtenedName_used = termExtendedName)
      , by = c("level_used" = "level", "termCode_used" = "termCode")
    ) 
  
  
  # Merged ####
  
  merged <- 
    with_occurrence %>% 
    
    # Exposure L2 at each food consumptio occassion
    mutate(
      meal_exp_mean_LB = amountfood * LB_mean / weight,
      meal_exp_mean_MB = amountfood * MB_mean / weight,
      meal_exp_mean_UB = amountfood * UB_mean / weight
    ) %>% 
    
    # #Exposure L3 at each food consumption occassion
    # 
    # mutate(
    #   l3_meal_exp_mean_LB	= amountfood * LB_mean_l3 / weight,
    #   l3_meal_exp_mean_MB	= amountfood * MB_mean_l3 / weight,
    #   l3_meal_exp_mean_UB = amountfood * UB_mean_l3 / weight
    # ) %>% 
    # 
    # # Refined exposure. If we have info from level 3 then that else from l2
    # mutate(
    #   refined_exp_lb = if_else(is.na(LB_mean_l3), meal_exp_mean_LB, l3_meal_exp_mean_LB),
    #   refined_exp_mb = if_else(is.na(MB_mean_l3), meal_exp_mean_MB, l3_meal_exp_mean_MB),
    #   refined_exp_ub = if_else(is.na(UB_mean_l3), meal_exp_mean_UB, l3_meal_exp_mean_UB)
    # ) %>% 
    mutate(
      refined_exp_lb = meal_exp_mean_LB,
      refined_exp_mb = meal_exp_mean_MB,
      refined_exp_ub = meal_exp_mean_UB
    ) %>% 
    
    # Adjust by wcoeff
    mutate(
      wcoeff_adjusted_refined_exposure_lb  = refined_exp_lb  * wcoeff,
      wcoeff_adjusted_refined_exposure_mb	 = refined_exp_mb  * wcoeff,
      wcoeff_adjusted_refined_exposure_ub  = refined_exp_ub  * wcoeff,
    ) 
    
    # #Other Level 3
    # # Need at least 1 value at any scenario in L3
    # mutate(
    #   foodex_l3_desc_aggr = if_else(rowSums(is.na(across(LB_mean_l3:UB_mean_l3)))==3,
    #                                 paste0("-Other-", foodex_l2_desc), 
    #                                 foodex_l3_desc)
    # ) 
  
  #return
  merged
  
}


#'Create the Individual Exposure table
#'@param merged_data The merged data derived from create_tbl_merged()
#'@param tbl_subjecs The `tbl_subjecs` derived from `create_tbl_subjects()`
#'@param exposure_factor Integer. 1 (daily), 7 (weekly)
#'@details The `exposure_factor` is derived from the the `occurrence_l2` table
#'@param ... Unused yet
#'@noRd
create_tbl_exposure_fdx2 <- function(merged_data,tbl_subjects, exposure_factor,...){
  
  
  merged_data %>% 
    dplyr::group_by(
      subjectid
    ) %>% 
    dplyr::summarise(
      nday_lb = sum(wcoeff_adjusted_refined_exposure_lb, na.rm = TRUE),
      nday_mb = sum(wcoeff_adjusted_refined_exposure_mb, na.rm = TRUE),
      nday_ub = sum(wcoeff_adjusted_refined_exposure_ub, na.rm = TRUE)
    ) %>% 
    dplyr::left_join(tbl_subjects,by = "subjectid") %>% 
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with(("nday_")), ~ ./wcoeff
      )
    ) %>% 
    dplyr::mutate(
      subExp_LB = (nday_lb/cons_days) * exposure_factor,
      subExp_MB = (nday_mb/cons_days) * exposure_factor,
      subExp_UB = (nday_ub/cons_days) * exposure_factor
    ) %>% 
    dplyr::ungroup()
  
}