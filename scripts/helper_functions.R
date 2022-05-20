# Workshop Helper Functions


naturalize_labelled_df <- function(labelled_df, levels = "default", 
                                   native_numerical_suffix = "", 
                                   fctr_suffix = "_fct", 
                                   char_suffix = ""){
  # all labelled variables as factors
  native_df <- labelled_df %>% 
  rename_with(~ paste0(.x, native_numerical_suffix), .cols = where(is.labelled)) %>%
    mutate(
      across(where(is.labelled), ~.x %>% 
               zap_labels() %>% 
               zap_formats() %>% 
               zap_widths() %>% 
               zap_missing())
    ) 
  
  # all labelled variables transformed to native R
  # user defined missings turned to NA
  # Most variables will become numeric, except for string variables
  
  fctr_df <- labelled_df %>% 
    select(where(is.labelled)) %>% 
    rename_with(~ paste0(.x, fctr_suffix)) %>% 
    mutate(
      across(where(is.labelled), ~ as_factor(.x, levels = "both"))
    )
  
  #lastly, add a suffix for spss character vectors (containing only text)
  
  native_df %>% add_column(fctr_df) %>% 
    rename_with(~ paste0(.x, char_suffix), .cols=where(is.character))
}

register_labels <- function(labelled_df){
  # Collects all variable labels in a data frame
  
  var_labels <- labelled_df %>% 
    map_chr(~ attr(.x, "label")) %>% 
    as_tibble(rownames = NA) %>% 
    rownames_to_column("var_name") %>% 
    rename(variable_label = value)
  
  value_labels <- labelled_df %>% 
    map_chr(~ attr(.x, "labels") %>% paste0("[", ., "] ", names(.), collapse = "; ")) %>% 
    as_tibble(rownames = NA) %>% 
    rownames_to_column("var_name") %>% 
    rename(value_labels = value)
  
  full_join(var_labels, value_labels, by = "var_name")
  
}

fetch_var_lab <- function(var_name_to_look_up, label_table){
  # fetches the correct label from the df, when given a var_name
  # This also works with the suffix var names from naturalize_labelld_df() e.g., v_01_fctr
  label_table %>% 
    filter(var_name_to_look_up == var_name) %>% 
    pull(variable_label) %>% 
    pluck(1)
}

fetch_val_lab <- function(var_name_to_look_up, label_table){
  # fetches the correct label from the df, when given a var_name
  # This also works with the suffix var names from naturalize_labelld_df() e.g., v_01_fctr
  label_table %>% 
    filter(var_name_to_look_up == var_name) %>% 
    pull(value_labels) %>% 
    pluck(1)
}

# Generates an intercorrelation matrix for a dataframe; 
# non-numeric vars are discarded

form_intercorr_matrix <- function(df, discard_non_numeric = TRUE){
  if(discard_non_numeric) {
    df <- df %>% select(where(is.numeric))
  } 
  
  cor_matrix <- tibble(
    variables = names(df),
    variable_b = list(variables)
  ) %>% 
    unnest(cols = c(variable_b)) %>% 
    mutate(correlation = map2_dbl(variables, variable_b, ~ cor(df[[.x]], df[[.y]]))) %>% 
    pivot_wider(values_from = correlation, names_from = variable_b) %>% 
    select(variables, sort(colnames(.))) %>% 
    arrange(variables)
  
  cor_matrix_variables <- cor_matrix[1]
  cor_matrix <- cor_matrix[-1]
  cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
  cor_matrix %>% add_column(cor_matrix_variables, .before = 1) %>% 
    mutate(across(where(is.numeric), round, 3)) 
}

## reverse codes items
flip_item <- function(x, min_score = min(x, na.rm = TRUE), max_score = max(x, na.rm = TRUE)){
  ((x-min_score)*-1)+max_score
}


## Scale builder
# Syntax vom Clemens Lechner


scale_builder <- function(data, item_list, score_function) {
  if (!is.list(item_list) | is.null(names(item_list))) {
    stop("Supply a named list in the item_list argument")
  }
  
  scale_names <- names(item_list)
  
  for (i in 1:length(item_list)) {
    
    #new_name <- str_c(scale_names[i], "_", score_fun_name)
    
    new_name <- scale_names[i]
    
    if (score_function == "mean") {
      
      data[new_name] <- data[item_list[[i]]] %>% 
        rowMeans(na.rm = FALSE)
      
      message(
        "Computing ", new_name, " as the mean score across ",
        length(item_list[[i]]), " items (", 
        str_flatten(item_list[[i]], collapse = ", "), ").")
    } else if (score_function == "sum") {
      
      data[new_name] <- data[item_list[[i]]] %>% 
        rowSums(na.rm = FALSE)
      
      message(
        "Computing ", new_name, " as the sum score across ",
        length(item_list[[i]]), " items (", 
        str_flatten(item_list[[i]], collapse = ", "), ")."
      )  
    } else stop("Supply a valid scoring function (sum or mean)")
  }
  data
}

### round up function --- 
round_df <- function(df, digits=2) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(df, mode) == 'numeric'
  df[numeric_columns] <-  round(df[numeric_columns], digits)
  df
} #closes func


## pca centered ---


pca_centered <- function(data, items, nfactors = 2, rotation =  "varimax") {
  
  data %>% 
    select(all_of(items)) %>% rowwise() %>% 
    mutate(ipsmean = mean(c_across(items), na.rm = FALSE)) %>% 
    ungroup() %>% 
    mutate(across(all_of(items), ~ .x - ipsmean)) %>% 
    select(-ipsmean) %>% 
    psych::principal(nfactors = nfactors, rotate = rotation) 
  
}
