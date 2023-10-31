extend_data <- function(
    df0, ### Data frame to extend
    from0 = 2090, ### Year to extend from
    to0   = 2300, ### Year to extend to
    by0   = 1     ### 
    ){
  ### complete(year = seq(min(year), 2300, 1)) |> fill(-c("year"))
  #### Years
  years0 <- tibble(year = seq(from0 + 1, to0, by=by0))
  #### Data to extend
  df1    <- df0 |> filter(year==from0) |> select(-c("year"))
  df1    <- df1 |> cross_join(years0)
  ### Bind back in
  df0    <- df0 |> filter(year< from0)
  df0    <- df0 |> rbind(df1)
  ### Return
  return(df0)
}
