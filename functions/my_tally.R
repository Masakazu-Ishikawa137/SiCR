
my_tally <- function(
    
    data,
    group_by,
    x,
    drop_na=TRUE,
    arrange="desc",
    count=TRUE,
    percent=TRUE

) {
  
  # drop_na
  if (isTRUE(drop_na)) {
    data <- drop_na(data, any_of(c(group_by, x)))
  }
  
  # tally
  res <- data %>%
    select(all_of(c(group_by, x))) %>%
    group_by(across(all_of(c(group_by, x)))) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    complete(!!sym(group_by), !!sym(x), fill = list(count = 0))
  
  # arrange
  if (arrange == "desc") {
    res <- arrange(res, !!sym(group_by), desc(count))
  } else if (arrange == "asc") {
    res <- arrange(res, !!sym(group_by), count)
  }
  
  # get percentage
  if (isTRUE(percent)) {
    res <- res %>%
      group_by(across(all_of(group_by))) %>%
      mutate(percent = count / sum(count) * 100) %>%
      ungroup()
  }
  
  # drop count
  if (!isTRUE(count)) {
    res <- select(res, -count)
  }
  
  return(res)
  
}