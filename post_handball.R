# post_handball.R  (Weg 2: eigener Google-Apps-Script-Proxy)
library(atrrr)
library(httr2)
library(xml2)

# >>> HIER deine Google-Apps-Script Web-App-URL eintragen (.../exec) <<<
proxy_url    <- "https://script.google.com/macros/s/XXXXXXXX/exec"

original_url <- "https://www.handball-world.news/feed.xml"
mirror_file  <- "feed/handball.xml"
state_file   <- "state/posted_ids.txt"
max_per_run  <- 20

BROWSER_UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36"

looks_like_feed <- function(txt) {
  !is.null(txt) && length(txt) == 1 && nzchar(txt) &&
    grepl("<rss|<feed|<item|<entry", txt, ignore.case = TRUE)
}

try_one <- function(name, u) {
  tryCatch({
    resp <- httr2::request(u) |>
      httr2::req_headers(`User-Agent` = BROWSER_UA,
                         Accept = "application/rss+xml, application/xml;q=0.9, */*;q=0.8") |>
      httr2::req_timeout(45) |>
      httr2::req_retry(max_tries = 2) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    st <- httr2::resp_status(resp)
    if (st != 200) { message(name, ": HTTP ", st); NULL }
    else {
      body <- httr2::resp_body_string(resp)
      if (!looks_like_feed(body)) { message(name, ": Antwort ist kein Feed"); NULL } else body
    }
  }, error = function(e) { message(name, ": ", conditionMessage(e)); NULL })
}

download_feed_text <- function(url) {
  enc <- utils::URLencode(url, reserved = TRUE)
  strategies <- list(
    c(name = "gscript",    u = proxy_url),                                       # eigener Google-Proxy (primär)
    c(name = "jina",       u = paste0("https://r.jina.ai/", url)),
    c(name = "allorigins", u = paste0("https://api.allorigins.win/raw?url=", enc)),
    c(name = "codetabs",   u = paste0("https://api.codetabs.com/v1/proxy/?quest=", url)),
    c(name = "direct",     u = url)
  )
  for (s in strategies) {
    if (s[["name"]] == "gscript" && grepl("XXXXXXXX", s[["u"]])) next  # Proxy-URL noch nicht gesetzt
    body <- try_one(s[["name"]], s[["u"]])
    if (!is.null(body)) { message("Feed geladen ueber: ", s[["name"]]); return(body) }
  }
  NULL
}

parse_feed_text <- function(txt) {
  txt <- gsub('xmlns(:[a-zA-Z0-9]+)?="[^"]*"', "", txt)
  doc <- read_xml(txt)
  nodes <- xml_find_all(doc, "//item | //entry")
  if (length(nodes) == 0) return(NULL)
  g1 <- function(n, xp) { x <- xml_find_first(n, xp); if (inherits(x, "xml_missing")) NA_character_ else xml_text(x) }
  glink <- function(n) {
    l <- xml_find_first(n, ".//link")
    if (inherits(l, "xml_missing")) return(NA_character_)
    h <- xml_attr(l, "href"); if (!is.na(h) && nzchar(h)) h else xml_text(l)
  }
  data.frame(
    title = vapply(nodes, g1, "", xp = ".//title"),
    link  = vapply(nodes, glink, ""),
    guid  = vapply(nodes, function(n) { g <- g1(n, ".//guid"); if (is.na(g)) g <- g1(n, ".//id"); g }, ""),
    stringsAsFactors = FALSE
  )
}

# ---- Feed beschaffen: frisch (+Kopie) oder gespeicherte Kopie ----
fresh <- download_feed_text(original_url)
if (!is.null(fresh)) {
  dir.create(dirname(mirror_file), showWarnings = FALSE, recursive = TRUE)
  writeLines(fresh, mirror_file, useBytes = TRUE)
  feed_txt <- fresh
} else if (file.exists(mirror_file)) {
  message("Kein frischer Abruf moeglich -> nutze gespeicherte Kopie.")
  feed_txt <- paste(readLines(mirror_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
} else {
  stop("Feed weder frisch noch aus Kopie verfuegbar - Abbruch.")
}

feed <- parse_feed_text(feed_txt)
if (is.null(feed) || nrow(feed) == 0) stop("Feed enthaelt keine Eintraege.")
feed$uid <- ifelse(is.na(feed$guid) | feed$guid == "", feed$link, feed$guid)

# ---- Dedup ----
posted    <- if (file.exists(state_file)) readLines(state_file, warn = FALSE) else character(0)
first_run <- !file.exists(state_file)
if (first_run) {
  dir.create(dirname(state_file), showWarnings = FALSE, recursive = TRUE)
  writeLines(unique(feed$uid), state_file)
  message("Erststart: ", nrow(feed), " Eintraege als gesehen markiert, kein Post.")
  quit(save = "no")
}

new_items <- feed[!feed$uid %in% posted, , drop = FALSE]
new_items <- new_items[rev(seq_len(nrow(new_items))), , drop = FALSE]
if (nrow(new_items) == 0) { message("Keine neuen Beitraege."); quit(save = "no") }
if (nrow(new_items) > max_per_run) new_items <- tail(new_items, max_per_run)

# ---- Anmelden ----
auth(user = Sys.getenv("BSKY_USER"), password = Sys.getenv("BSKY_APP_PASSWORD"), overwrite = TRUE)

# ---- Posten (#handball, max. 300 Zeichen) ----
hashtag <- "#handball"; sep <- "\n"
posted_now <- character(0)
for (i in seq_len(nrow(new_items))) {
  title <- new_items$title[i]; link <- new_items$link[i]
  if (is.na(title) || !nzchar(title) || is.na(link)) next
  fixed  <- nchar(link) + nchar(hashtag) + 2 * nchar(sep)
  budget <- 300 - fixed
  if (nchar(title) > budget) title <- if (budget > 3) paste0(substr(title, 1, budget - 3), "...") else ""
  txt <- if (nzchar(title)) paste0(title, sep, link, sep, hashtag) else paste0(link, sep, hashtag)
  ok <- tryCatch({
    post_skeet(text = txt, langs = "de", preview_card = TRUE); TRUE
  }, error = function(e) tryCatch({
    post_skeet(text = txt, langs = "de", preview_card = FALSE); TRUE
  }, error = function(e2) { message("Fehler beim Posten: ", conditionMessage(e2)); FALSE }))
  if (ok) posted_now <- c(posted_now, new_items$uid[i])
  Sys.sleep(2)
}
if (length(posted_now) > 0) {
  writeLines(unique(c(posted, posted_now)), state_file)
  message(length(posted_now), " Beitrag/Beitraege gepostet.")
}
