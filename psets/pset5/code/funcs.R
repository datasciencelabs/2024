get_cdc_data <- function(url,limit= 1000000000){
  require(httr2)
  ret <- request(url) |> 
    req_url_query("$limit" = limit) |>
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
  return(ret)
}


