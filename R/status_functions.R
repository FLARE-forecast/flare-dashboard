
## function for health check table

## resources
#https://datascienceplus.com/accessing-web-data-json-in-r-using-httr/
#https://healthchecks.io/docs/api/

health_check_table <- function(){

  health_check <- GET(
    "https://healthchecks.io/api/v1/checks/",
    accept_json(),
    add_headers('X-Api-Key' = '7jlyRG1Kt5Vr2Jkb1cZ8qY3rjcfpIjaJ')
  )

  check_parsed <- content(health_check,as='parsed')$checks

  hc_names <- c()
  hc_status <- c()
  hc_latest <- c()

  for (i in seq.int(1,length(check_parsed))){
    hc_names[i] <- check_parsed[[i]][[1]] # json objects 1,7,9 are name, status, and last ping
    hc_status[i] <- check_parsed[[i]][[7]]
    hc_latest[i] <- check_parsed[[i]][[9]]
  }

  check_df <- data.frame(hc_names,hc_status,hc_latest)
  check_df$hc_latest <- as.character(lubridate::as_datetime(check_df$hc_latest))


    hc_table <- reactable(check_df,
                          defaultColDef = colDef(
                            align = "center"),
                          columns = list(hc_names = colDef(name='Process Name'),
                                         hc_status = colDef(name='Status'),
                                         hc_latest = colDef(name = 'Updated (UTC)')),
                          defaultPageSize = 10,
                          filterable = TRUE,
                          highlight = TRUE)

    return(hc_table)

}
