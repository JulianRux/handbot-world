# post_handball.R
library(atrrr)
library(tidyRSS)

feed_url    <- "https://rss.app/feeds/oe9wCvNL8vqUiZ8U.xml"
state_file  <- "state/posted_ids.txt"
max_per_run <- 10   # Sicherheitslimit gegen Fluten

# 1) Bereits gepostete IDs laden
posted <- if (file.exists(state_file)) readLines(state_file, warn = FALSE) else character(0)

# 2) Feed einlesen
feed <- tidyfeed(feed_url)

# Eindeutige ID je Beitrag (guid, sonst der Link)
if (!"item_guid" %in% names(feed)) feed$item_guid <- feed$item_link
feed$uid <- ifelse(is.na(feed$item_guid) | feed$item_guid == "",
                   feed$item_link, feed$item_guid)

first_run <- !file.exists(state_file)

# Erststart: alles nur als "gesehen" markieren, NICHT posten (kein Spam alter Meldungen)
if (first_run) {
  dir.create(dirname(state_file), showWarnings = FALSE, recursive = TRUE)
  writeLines(unique(feed$uid), state_file)
  message("Erststart: ", nrow(feed), " Eintraege als gesehen markiert, kein Post.")
  quit(save = "no")
}

# 3) Nur neue Eintraege
new_items <- feed[!feed$uid %in% posted, , drop = FALSE]

# aelteste zuerst posten
if ("item_pub_date" %in% names(new_items)) {
  new_items <- new_items[order(new_items$item_pub_date), , drop = FALSE]
}

if (nrow(new_items) == 0) {
  message("Keine neuen Beitraege.")
  quit(save = "no")
}

# auf max_per_run begrenzen (die neuesten behalten)
if (nrow(new_items) > max_per_run) new_items <- tail(new_items, max_per_run)

# 4) Anmelden (Zugangsdaten kommen aus Umgebungsvariablen, nicht aus dem Code)
auth(user      = Sys.getenv("BSKY_USER"),
     password  = Sys.getenv("BSKY_APP_PASSWORD"),
     overwrite = TRUE)

# 5) Posten
hashtag <- "#handball"
sep     <- "\n"

posted_now <- character(0)
for (i in seq_len(nrow(new_items))) {
  title <- new_items$item_title[i]
  link  <- new_items$item_link[i]

  if (is.na(title) || !nzchar(title) || is.na(link)) next

  # --- Text bauen, immer unter 300 Zeichen (Bluesky-Limit) ---
  # Fixkosten: Link + Hashtag + zwei Trenner
  fixed  <- nchar(link) + nchar(hashtag) + 2 * nchar(sep)
  budget <- 300 - fixed   # so viel Platz bleibt fuer den Titel

  if (nchar(title) > budget) {
    if (budget > 3) {
      title <- paste0(substr(title, 1, budget - 3), "...")
    } else {
      title <- ""   # extrem langer Link -> Titel weglassen, Link + Hashtag bleiben
    }
  }

  txt <- if (nzchar(title)) {
    paste0(title, sep, link, sep, hashtag)
  } else {
    paste0(link, sep, hashtag)
  }

  # atrrr erkennt URL und #hashtag im Text automatisch und erzeugt die Vorschau-Karte
  ok <- tryCatch({
    post_skeet(text = txt, langs = "de", preview_card = TRUE)
    TRUE
  }, error = function(e) {
    # Fallback ohne Vorschau-Karte, falls das Erzeugen der Karte scheitert
    tryCatch({
      post_skeet(text = txt, langs = "de", preview_card = FALSE)
      TRUE
    }, error = function(e2) { message("Fehler beim Posten: ", conditionMessage(e2)); FALSE })
  })

  if (ok) posted_now <- c(posted_now, new_items$uid[i])
  Sys.sleep(2)
}

# 6) Status speichern
if (length(posted_now) > 0) {
  writeLines(unique(c(posted, posted_now)), state_file)
  message(length(posted_now), " Beitrag/Beitraege gepostet.")
}
