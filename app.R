library(shiny)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(ggrepel)
library(campfin)
library(scales)
library(fs)
library(ggplot2)

ui <- fluidPage(
    titlePanel("Spotify Viz"),
    helpText(
        "An implementation of ",
        tags$a(href = "https://old.reddit.com/r/dataisbeautiful/comments/rfkh6r/for_fans_of_spotify_wrapped_you_can_download_your/", "this Spotify data viz "),
        "by ",
        tags$a(href = "https://github.com/kiernann", "Kiernan Nicholls "),
        "Adapted for R Shiny."
    ),
    helpText(
        "You can request your Spotofy data from ",
        tags$a(href = "https://www.spotify.com/us/account/privacy/", "here "),
        "and upload StreamingHistory0.json to get your own insights. ",
        tags$a(href = "https://raw.githubusercontent.com/cosmoduende/r-spotify-history-analysis/main/StreamingHistory0.json", "Here's "),
        "a publicly available dataset used for testing,"
    ),
    helpText(
        "Check out the code for this project ",
        tags$a(href = "https://github.com/johnathanfernandes/SpotifyRedditViz", "here!")
    ),
    
    mainPanel(
        fileInput(
            "historyfile",
            "Choose CSV File",
            multiple = FALSE,
            accept = c(".json")
        ),
        numericInput("Year", label = "Year:", value = 2020),
        
        
        plotOutput("timePolar"),
        plotOutput("calendar"),
        plotOutput("MostListened"),
        plotOutput(outputId = "monthBar"),
        plotOutput("cumulativeTime"),
    )
)



server <- function(input, output) {
    download.file(
        "https://raw.githubusercontent.com/cosmoduende/r-spotify-history-analysis/main/StreamingHistory0.json",
        "history.json"
    )
    
    clean_data <- function(x) {
        as_tibble(map_df(x, fromJSON)) %>%
            mutate(across(endTime, ymd_hm), minPlayed = msPlayed / 6e4) %>%
            filter(year(endTime) == input$Year ,
                   artistName != "Unknown Artist")
    }
    
    history <- reactive({
        if (is.null(input$historyfile)) {
            clean_data("history.json")
        }
        else{
            clean_data(input$historyfile$datapath)
        }
    })
    
    output$monthBar <- renderPlot({
        history() %>%
            group_by(month = month(endTime)) %>%
            
            filter(artistName %in% most_common(artistName, 3)) %>%
            
            group_by(month, artistName) %>%
            summarise(totalMin = sum(minPlayed),
                      .groups = "drop_last") %>%
            mutate(
                lbl_y = cumsum(totalMin),
                lbl_abb = if_else(totalMin < 100, "", abbreviate(artistName)),
                lbl_abb = if_else(lbl_abb == "TPOTUSOA",
                                  "PUSA",
                                  lbl_abb)
            ) %>%
            ggplot(mapping = aes(x = month, y = totalMin)) +
            geom_col(
                mapping = aes(fill = str_trunc(artistName, width = 25)),
                color = "black",
                position = position_stack(reverse = TRUE)
            ) +
            geom_text(
                mapping = aes(y = lbl_y, label = lbl_abb),
                vjust = 1.5,
                color = "black"
            ) +
            scale_x_continuous(breaks = 1:12,
                               labels = month.abb) +
            scale_y_continuous(labels = comma) +
            labs(
                title = "Most listened to artists each month",
                fill = "Artist",
                x = "Month",
                y = "Minutes"
            ) +
            theme_classic()
    })
    
    output$timePolar <- renderPlot({
        history() %>%
            mutate(hour = hour(with_tz(endTime, "America/New_York")), ) %>%
            group_by(hour) %>%
            summarise(totalMin = sum(minPlayed)) %>%
            
            ggplot(aes(x = hour, y = totalMin)) +
            geom_col(mapping = aes(fill = totalMin),
                     color = "black") +
            scale_fill_viridis_c(end = 0.95,
                                 option = "B",
                                 guide = "none") +
            scale_x_continuous(
                breaks = 0:23,
                minor_breaks = NULL,
                labels = function(x) {
                    format(as.POSIXct(as.character(x), format = "%H"), format = "%I %p")
                }
            ) +
            scale_y_continuous(labels = comma,
                               n.breaks = 10) +
            coord_polar(start = 0) +
            labs(title = "Listening time by hour of the day",
                 x = "Hour",
                 y = "Minutes") +
            theme_classic() +
            theme(panel.grid.major = element_line())
    })
    
    output$cumulativeTime <- renderPlot({
        y <- history() %>%
            filter(artistName %in% most_common(artistName, 5)) %>%
            group_by(artistName, wk = week(endTime)) %>%
            summarise(wkMins = sum(minPlayed)) %>%
            mutate(cumMin = cumsum(wkMins),
                   lbl_abb = abbreviate(artistName))
        
        y %>%
            ggplot(mapping = aes(x = wk, y = cumMin)) +
            geom_step(mapping = aes(color = artistName)) +
            geom_text_repel(
                data = filter(y, cumMin == max(cumMin)),
                x = max(y$wk),
                mapping = aes(
                    label = artistName,
                    color = artistName,
                    x = wk
                ),
                direction = "y"
            ) +
            scale_y_continuous(labels = comma) +
            scale_color_discrete(guide = "none") +
            labs(
                title = "Cumulative listening time over year",
                color = "Artist",
                x = "Week",
                y = "Minutes"
            ) +
            theme_classic()
    })
    
    output$calendar <- renderPlot({
        history() %>%
            group_by(date = as_date(endTime)) %>%
            summarise(minTotal = sum(minPlayed)) %>%
            mutate(
                year = year(date),
                month = month(date, label = TRUE, abbr = TRUE),
                wday = fct_relevel(
                    .f = wday(date, label = TRUE, abbr = TRUE),
                    c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
                ),
                day = day(date),
                wk = format(date, "%W")
            ) %>%
            ggplot(mapping = aes(x = wk, y = wday)) +
            geom_tile(
                mapping = aes(fill = minTotal),
                color = "black",
                size = 0.5
            ) +
            coord_equal() +
            scale_fill_viridis_c(end = 0.95,
                                 option = "B",
                                 na.value = "grey") +
            labs(
                title = "Listened time each day of the year",
                x = "Week",
                y = "Weekday",
                fill = "Minutes"
            ) +
            scale_x_discrete(labels = if_else(is_even(0:48), "", as.character(0:48))) +
            theme_classic()
    })
    
    output$MostListened <- renderPlot({
        history() %>%
            group_by(artistName) %>%
            summarise(minPlayed = sum(minPlayed)) %>%
            arrange(desc(minPlayed)) %>%
            head(20) %>%
            ggplot(aes(x = reorder(
                str_trunc(artistName, 25), minPlayed
            ),
            y = minPlayed)) +
            geom_col(aes(fill = minPlayed)) +
            scale_fill_viridis_c(end = 0.9,
                                 option = "B",
                                 guide = "none") +
            coord_flip()  +
            labs(
                title = "Most listened to artist",
                x = "Artist",
                y = "Minutes",
                fill = "Minutes"
            ) +
            theme_classic()
    })
}

shinyApp(ui = ui, server = server)
