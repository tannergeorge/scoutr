library(nflverse)
library(tidyverse)
library(gt)
library(gtExtras)


draft_picks <- nflreadr::load_draft_picks(2012:2024)
combine_data <- nflreadr::load_combine(2012:2024)
rosters <- nflreadr::load_rosters(2012:2024)
teams <- nflreadr::load_teams()
contracts <- nflreadr::load_contracts()

#Functions
convert_height <- Vectorize(function(height) {
  split = strsplit(height, split = "-")[[1]]
  feet = as.integer(split[1]) * 12
  inches = as.integer(split[2])
  
  feet + inches
})

get_similarity <- Vectorize(function(ht, pht, wt, pwt, 
                                     forty, pforty,
                                     vert, pvert) {
  ht_diff <- abs(1 - convert_height(ht)/pht)
  wt_diff <- abs(1 - wt/pwt)
  forty_diff <- abs(1 - forty/pforty)
  vert_diff <- abs(1 - vert/pvert)
  
  diffs <- c(ht_diff, wt_diff, forty_diff, vert_diff)
  
  mean(diffs)
})

get_value <- Vectorize(function(name) {
  player_match <- draft_picks %>%
    filter(pfr_player_name == name)
  
  player_match$dr_av[1]
})

get_pb <- Vectorize(function(name) {
  player_match <- draft_picks %>%
    filter(pfr_player_name == name)
  
  player_match$w_av[1]/ player_match$games[1]
})

get_contract_value <- Vectorize(function(name) {
  match <- contracts %>%
    filter(player == name)
  
  sum(match$inflated_value[1], na.rm = TRUE) / sum(match$years[1], 
                                                   na.rm = TRUE)
})

get_headshot <- Vectorize(function(id) {
  player_match <- rosters %>%
    filter(pfr_id == id)
  
  player_match$headshot_url[1]
})

get_team_logo <- Vectorize(function(name) {
  if (name == "Washington Redskins" || name == "Washington Football Team") {
    name = "Washington Commanders"
  } else if (name == "St. Louis Rams") {
    name = "Los Angeles Rams"
  } else if (name == "San Diego Chargers") {
    name = "Los Angeles Chargers"
  } else if (name == "Oakland Raiders") {
    name = "Las Vegas Raiders"
  }
  
  team <- teams %>%
    filter(team_name == name)
  team$team_logo_espn[1]
})

get_college_logo <- Vectorize(function(school_name) {
  # Remove any leading/trailing spaces before comparison
  school_name <- trimws(school_name)
  
  if (school_name == "Miami" || school_name == "Miami (FL)") {
    school_name <- "miami-fl"
  } else if (school_name == "USC") {
    school_name <- "southern-california"
  } else if (school_name == "TCU") {
    school_name <- "texas-christian"
  } else if (school_name == "LSU") {
    school_name <- "louisiana-state"
  } else if (school_name == "Southern Miss") {
    school_name <- "southern-mississippi"
  } else if (school_name == "Texas A&M") {
    school_name <- "texas-am"
  }
  
  # Replace "St." with "State" and "Col." with "College" in the school name
  school_name <- gsub("\\bSt\\.\\b", "State", school_name, ignore.case = TRUE)
  school_name <- gsub("\\bCol\\.\\b", "College", school_name, ignore.case = TRUE)
  
  # Clean the school name by converting to lowercase and replacing spaces with hyphens
  clean_name <- gsub(" ", "-", trimws(tolower(school_name)))
  
  # Construct the Sports Reference NCAA logo URL based on the pattern
  logo_url <- paste0("https://cdn.ssref.net/req/202412261/tlogo/ncaa/", clean_name, "-2024.png")
  
  return(logo_url)
})


run <- function() {
  player <- readline(prompt = "Player name: ")
  
  this_player <- combine_data %>%
    filter(player_name == player)
  
  pn <- this_player$player_name
  player_school <- this_player$school
  player_pos <- this_player$pos
  player_height <- convert_height(this_player$ht)
  player_weight <- this_player$wt
  player_forty <- this_player$forty
  player_vertical <- this_player$vertical
  player_shuttle <- this_player$shuttle
  player_bench <- this_player$bench
  
  result <- tryCatch({
    similar <- combine_data %>%
      filter(!is.na(wt), !is.na(forty), !is.na(ht), !is.na(draft_ovr),
             !is.na(vertical)) %>%
      filter(!is.na(draft_year)) %>%
      filter(player_name != pn) %>%
      filter(pos == player_pos) %>%
      filter(abs(convert_height(ht) - player_height) <= 5) %>%
      filter(abs(wt - player_weight) <= 10) %>%
      filter(abs(forty - player_forty) <= 0.5) %>%
      filter(abs(vertical - player_vertical) <= 5) %>%
      
      mutate(similarity = (1 - get_similarity(ht, player_height,
                                              wt, player_weight,
                                              forty, player_forty,
                                              vertical, 
                                              player_vertical)) * 100) %>%
      
      mutate(value_per_game = get_value(player_name)) %>%
      mutate(contract = get_contract_value(player_name)) %>%
      mutate(headshot = get_headshot(pfr_id)) %>%
      mutate(team = get_team_logo(draft_team)) %>%
      mutate(school_logo = get_college_logo(school)) %>%
      arrange(-similarity) %>%
      select(draft_year, school_logo, headshot, player_name, draft_ovr, similarity,
             contract) %>%
      slice(1:10) %>%
      gt() %>%
      gt_img_rows(columns = headshot, img_source = "web", height = 50) %>%
      gt_img_rows(columns = school_logo, img_source = "web", height = 40) %>%
      #gt_img_rows(columns = team, img_source = "web", height = 50) %>%
      gt_theme_espn() %>%
      fmt_number(columns = c(similarity), decimals = 2) %>%
      fmt(
        columns = c(contract),
        fns = function(x) paste0("$", sprintf("%.2f", x), "M")
      ) %>%
      
      data_color(columns = c(similarity), colors = "Blues") %>%
      data_color(columns = c(draft_ovr), colors = "Reds", reverse = TRUE) %>%
      data_color(columns = c(contract), colors = "Greens") %>%
      cols_label(
        school_logo = "School",
        headshot = "Player",
        player_name = "",
        draft_year = "Year",
        similarity = "Similarity",
        draft_ovr = "Pick",
        contract = "Adj. APY"
      ) %>%
      cols_align(
        columns = c(draft_year),
        align = "left"
      ) %>%
      opt_table_font(font = list(google_font(name = "Roboto Condensed"), default_fonts())) %>%
      tab_header(title = paste("Scoutr Similarity: ", pn), 
                 subtitle = paste(player_pos, "/ ", player_school)) %>%
      opt_table_outline() %>%
      tab_footnote(footnote = "Data: nflreadr, Table: Tanner George")
    
    print(similar, n = 10)
    
    return(similar)
  }, error = function(e) {
    message("Combine data not available for this player")
    return(NULL)
  })
  
}

run()