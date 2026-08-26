# post_handball.R
library(atrrr)
library(tidyRSS)

feed_url    <- "https://www.handball-world.news/feed.xml"
state_file  <- "state/posted_ids.txt"
max_per_run <- 5   # Sicherheitslimit gegen Fluten

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
posted_now <- character(0)
for (i in seq_len(nrow(new_items))) {
  title <- new_items$item_title[i]
  link  <- new_items$item_link[i]

  txt <- title
  if (nchar(txt) > 280) txt <- paste0(substr(txt, 1, 277), "...")  # Bluesky-Limit 300 Zeichen

  ok <- tryCatch({
    # link + preview_card erzeugt automatisch eine anklickbare Vorschau-Karte
    post_skeet(text = txt, link = link, langs = "de", preview_card = TRUE)
    TRUE
  }, error = function(e) { message("Fehler beim Posten: ", conditionMessage(e)); FALSE })

  if (ok) posted_now <- c(posted_now, new_items$uid[i])
  Sys.sleep(2)
}

# 6) Status speichern
if (length(posted_now) > 0) {
  writeLines(unique(c(posted, posted_now)), state_file)
  message(length(posted_now), " Beitrag/Beitraege gepostet.")
}
