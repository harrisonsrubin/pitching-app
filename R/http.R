# Polite HTTP GET shared by every fetcher: generic User-Agent, retries with
# exponential backoff on 429 / 5xx / timeouts, and at least 1 second between calls.

.pv_http <- new.env(parent = emptyenv())
.pv_http$last_call <- 0

http_get <- function(url, query = list(), tries = 4, min_gap = 1, timeout = 120) {
  wait <- min_gap - (as.numeric(Sys.time()) - .pv_http$last_call)
  if (wait > 0) Sys.sleep(wait)
  on.exit(.pv_http$last_call <- as.numeric(Sys.time()), add = TRUE)

  req <- httr2::request(url) %>%
    httr2::req_url_query(!!!query) %>%
    httr2::req_user_agent(cfg()$user_agent) %>%
    httr2::req_timeout(timeout) %>%
    httr2::req_retry(
      max_tries = tries,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504),
      retry_on_failure = TRUE,
      backoff = function(attempt) 2^attempt
    )
  httr2::req_perform(req)
}
