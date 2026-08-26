# post_handball.R
library(atrrr)
library(httr2)
library(xml2)

feed_url    <- "https://www.handball-world.news/feed.xml"
state_file  <- "state/posted_ids.txt"
max_per_run <- 5

# ---- Feed robust laden: direkt -> Proxy-Fallback ----
parse_feed_xml <- function(txt) {
  txt <- gsub('xmlns(:[a-zA-Z0-9]+)?="[^"]*"', "", txt)   # Namespaces raus (RSS + Atom)
  doc <- read_xml(txt)
  nodes <- xml_find_all(doc, "//item | //entry")
  if (length(nodes) == 0) return(NULL)

  get1 <- function(node, xpath) {
    x <- xml_find_first(node, xpath)
    if (inherits(x, "xml_missing")) NA_character_ else xml_text(x)
  }
  get_link <- function(node) {
    l <- xml_find_first(node, ".//link")
    if (inherits(l, "xml_missing")) return(NA_character_)
    href <- xml_attr(l, "href")
    if (!is.na(href) && nzchar(href)) href else xml_text(l)
  }
  data.frame(
    title = vapply(nodes, get1, "", xpath = ".//title"),
    link  = vapply(nodes, get_link, ""),
    guid  = vapply(nodes, function(n){ g <- get1(n,".//guid"); if(is.na(g)) g <- get1(n,".//id"); g }, ""),
    stringsAsFactors = FALSE
  )
}

fetch_feed <- function(url) {
  hdrs <- list(
    `User-Agent`      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36",
    Accept            = "application/rss+xml, application/atom+xml, application/xml;q=0.9, */*;q=0.8",
    `Accept-Language` = "de-DE,de;q=0.9,en;q=0.8",
    Referer           = "https://www.handball-world.news/"
  )

  # Strategien der Reihe nach probieren, erste mit Ergebnis gewinnt
  strategies <- list(
    direct = url,
    allorigins = paste0("https://api.allorigins.win/raw?url=", utils::URLencode(url, reserved = TRUE))
  )

  for (name in names(strategies)) {
    res <- tryCatch({
      resp <- httr2::request(strategies[[name]]) |>
        httr2::req_headers(!!!hdrs) |>
        httr2::req_timeout(30) |>
        httr2::req_retry(max_tries = 2) |>
        httr2::req_error(is_error = function(resp) FALSE) |>   # 403 nicht als R-Fehler werfen
        httr2::req_perform()

      if (httr2::resp_status(resp) != 200) {
        message(name, ": HTTP ", httr2::resp_status(resp)); NULL
      } else {
        parse_feed_xml(httr2::resp_body_string(resp))
      }
    }, error = function(e) { message(name, ": ", conditionMessage(e)); NULL })

    if (!is.null(res) && nrow(res) > 0) {
      message("Feed geladen ueber: ", name, " (", nrow(res), " Eintraege)")
      return(res)
    }
  }
  stop("Feed konnte weder direkt noch ueber Proxy geladen werden.")
}

feed <- fetch_feed(feed_url)
if (nrow(feed) == 0) { message("Keine Eintraege im Feed gefunden."); quit(save = "no") }

feed$uid  <- ifelse(is.na(feed$guid) | feed$guid == "", feed$link, feed$guid)
posted    <- if (file.exists(state_file)) readLines(state_file, warn = FALSE) else character(0)
first_run <- !file.exists(state_file)

# Erststart: nur merken, nicht posten
if (first_run) {
  dir.create(dirname(state_file), showWarnings = FALSE, recursive = TRUE)
  writeLines(unique(feed$uid), state_file)
  message("Erststart: ", nrow(feed), " Eintraege als gesehen markiert, kein Post.")
  quit(save = "no")
}

new_items <- feed[!feed$uid %in% posted, , drop = FALSE]
new_items <- new_items[rev(seq_len(nrow(new_items))), , drop = FALSE]  # Feed ist meist neueste-zuerst -> umdrehen
if (nrow(new_items) == 0) { message("Keine neuen Beitraege."); quit(save = "no") }
if (nrow(new_items) > max_per_run) new_items <- tail(new_items, max_per_run)

auth(user = Sys.getenv("BSKY_USER"), password = Sys.getenv("BSKY_APP_PASSWORD"), overwrite = TRUE)

# robustes Posten: erst mit Vorschau-Karte, bei Fehler ohne
do_post <- function(txt) {
  tryCatch(
    { post_skeet(text = txt, langs = "de", preview_card = TRUE); TRUE },
    error = function(e) tryCatch(
      { post_skeet(text = txt, langs = "de", preview_card = FALSE); TRUE },
      error = function(e2) { message("Fehler: ", conditionMessage(e2)); FALSE })
  )
}

posted_now <- character(0)
for (i in seq_len(nrow(new_items))) {
  title <- new_items$title[i]; link <- new_items$link[i]
  if (is.na(title) || !nzchar(title) || is.na(link)) next

  budget <- 300 - nchar(link) - 1                          # Bluesky-Limit: 300 Zeichen
  if (nchar(title) > budget) title <- paste0(substr(title, 1, budget - 3), "...")
  txt <- paste0(title, "\n", link)                          # Link im Text -> immer anklickbar

  if (do_post(txt)) posted_now <- c(posted_now, new_items$uid[i])
  Sys.sleep(2)
}

if (length(posted_now) > 0) {
  writeLines(unique(c(posted, posted_now)), state_file)
  message(length(posted_now), " Beitrag/Beitraege gepostet.")
}
