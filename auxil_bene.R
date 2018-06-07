parallel_blocks <- function(columns = c("relevancies_1")) {
  relevancies_df <- read.csv("relevancies.csv", sep = ";")
  
  df <- relevancies_df
  padding = 0.02
  # Height of column header box
  box_height = 0.2
  for(relevancy in c("relevancies_3", "relevancies_2", "relevancies_1")) {
    df <- df[order(df[relevancy]),] #, df$label
    for(i in c(2:11)) {
      df[relevancy][i:11,1] <- df[relevancy][i:11,1] + padding
    }
    df[relevancy] <- cumsum(df[relevancy])
    df[paste(relevancy, "_lower", sep = "")] <- 0
    df[paste(relevancy, "_lower", sep = "")][2:11,1] <- df[relevancy][1:10,1] + padding
  }
  
  cols <- c('rgba(166,206,227,0.6)', 'rgba(31,120,180,0.6)', 'rgba(178,223,138,0.6)', 'rgba(51,160,44,0.6)',
            'rgba(251,154,153,0.6)', 'rgba(227,26,28,0.6)', 'rgba(253,191,111,0.6)', 'rgba(255,127,0,0.6)',
            'rgba(202,178,214,0.6)', 'rgba(106,61,154,0.6)', 'rgba(255, 255, 91,0.6)') #150,210,150
  
  axs <- rep(columns, each=2)
  headers = c("Current", rep(c("Adjusted"), times = length(axs)-1))
  #axs <- c("relevancies_1", "relevancies_2", "relevancies_3")
  p <- plot_ly(x = c(1:length(axs)))
  i = 1
  for(name in df$label) {
    relevs <- df[df$label==name,]
    relevs_lower <- c(relevs[[paste(axs[1], "_lower", sep = "")]])
    relevs_upper <- c(relevs[[axs[1]]])
    for(j in 2:length(axs)) {
      relevs_lower = c(relevs_lower, relevs[[paste(axs[j], "_lower", sep = "")]])
      relevs_upper <- c(relevs_upper, relevs[[axs[j]]])
    }
    
    p <- p %>% 
      # Lower lines of the contours
      add_trace(y = relevs_upper,
                type = 'scatter', mode = 'lines', line = list(color = cols[i]),
                showlegend = FALSE, hoverinfo = "none") %>%
      # Upper lines of the contours
      add_trace(y = relevs_lower, type = 'scatter', sort = FALSE, mode = 'lines',
                fill = 'tonexty', fillcolor=cols[i], hoverinfo = "none", line = list(color = cols[i]),
                name = " ", showlegend = FALSE # TBD: Entfernen für Bericht
                )
    # # Black lines on the axis
    # for(j in 1:length(axs)) {
    #   p <- p %>% add_trace(x = c(j,j), y = c(relevs[[paste(axs[j], "_lower", sep = "")]],relevs[[axs[j]]]), 
    #                        type = 'scatter', mode = 'lines', line = list(color = 'rgba(0,0,0,1)'),
    #                        showlegend = FALSE, name = name)
    # }
    
    # Draw column borders and heading for all columns
    stack_height <- max(df$relevancies_1)
    for(j in seq(1,length(axs),2)) {
      p <- p %>% 
        # Add left column border
        add_trace(x = c(j,j), y = c(0,stack_height+padding+box_height), 
                  type = 'scatter', mode = 'lines', line = list(color = 'rgba(0,0,0,1)'),
                  showlegend = FALSE, name = name, hoverinfo = "none") %>% 
        # Add right column border
        add_trace(x = c(j+1,j+1), y = c(0,stack_height+padding+box_height), 
                  type = 'scatter', mode = 'lines', line = list(color = 'rgba(0,0,0,1)'),
                  showlegend = FALSE, name = name, hoverinfo = "none") %>%
        # Add column header box
        add_trace(
          x = c(j,j,j+1,j+1),
          y = c(stack_height+padding,stack_height+padding+0.2,stack_height+padding+0.2,stack_height+padding),
          mode = "none",
          type = 'scatter',
          fill = 'toself',
          fillcolor = '#000000', showlegend = FALSE, hoverinfo = "none") %>%
        # Add column header text
        add_trace(
          x = c(j+0.5),
          y = c(stack_height+padding+box_height/2),
          mode = "text",
          text = headers[j], #text = axs[j],
          textfont = list(color = '#FFFFFF', size = 14),
          type = 'scatter', showlegend = FALSE, hoverinfo = "none")
    }
    i = i+1
  }
  
  p <- p %>%
          layout(
            autosize = F, width = 250+(length(columns)-1)*300, height = 750, # TBD: 300 am Anfang für Bericht
            xaxis = list(
              title = "",
              zeroline = FALSE,
              showline = FALSE,
              showticklabels = FALSE,
              showgrid = FALSE,
              range = c(0.9, length(axs)+0.1)),
            yaxis = list(
              title = "",
              zeroline = FALSE,
              showline = FALSE,
              showticklabels = FALSE,
              showgrid = FALSE
            )) %>% config(displayModeBar = F)
  
  return(p)
}

prob_bars <- function(col_new, col_old = NULL) {
  
  probs_df <- read.csv("probs.csv", sep = ",")

  if(!is.null(col_old)) {
    # Some calculations to come up with the correct height of each bar stack
    tmp_neg <- tmp_pos <- probs_df[[col_new]] - probs_df[[col_old]]
    tmp_neg[tmp_neg > 0] <- 0
    tmp_pos[tmp_pos < 0] <- 0
    probs_df$Grey <- probs_df[[col_old]] + tmp_neg
    probs_df$Red <- abs(tmp_neg)
    probs_df$Green <- tmp_pos
  } else {
    # No Red or Green Bars should be added, thus 0
    probs_df$Grey <- probs_df[[col_new]]
    probs_df$Red <- 0
    probs_df$Green <- 0
  }
  
  t <- list(
    family = "arial",
    size = 40,
    color = 'rgba(103, 169, 207, 1)')
  percentages <- paste(as.character(probs_df$Grey * 100), '%', sep = '')
  
  p <- plot_ly(data = probs_df, x = ~therapy, y = ~Grey, name = "Old", hoverinfo = "none", type = "bar", 
               marker = list(color = 'rgba(103, 169, 207, 1)'), showlegend = FALSE)%>%
               #,text = percentages, textfont = t, textposition = 'outside') %>%
    add_trace(y=~Red, name = 'Less', marker = list(color = 'rgba(103, 169, 207, .5)'), showlegend = FALSE) %>%
    add_trace(y=~Green, name = 'More', marker = list(color = 'rgb(13, 216, 125)'), showlegend = FALSE) %>%
    layout(yaxis = list(title = 'Count', range = c(0, 0.8)), barmode = 'stack')
  
  p <- p %>%
    layout(
      autosize = T, height = 600,
      xaxis = list(
        title = ""),
      yaxis = list(
        title = "",
        zeroline = T,
        showline = F,
        showticklabels = FALSE,
        showgrid = FALSE
      )) %>%
    add_annotations(text = percentages,
                    x = probs_df$therapy,
                    y = probs_df$Grey + 0.04,
                    #xref = "x",
                    #yref = "y",
                    font = list(family = 'Arial',
                                size = 35,
                                color = 'rgba(103, 169, 207, 1)'),
                    showarrow = FALSE) %>% 
    config(displayModeBar = F)

  return(p)
}
prob_changes <- function(x, y_old, y_new) {
  y_low <- c(0,0,0)
  y_low[which(y_new>y_old)] <- y_old[which(y_new>y_old)]
  y_mid <- c(0,0,0)
  y_mid[which(y_new>=y_old)] <- y_new[which(y_new>=y_old)] - y_old[which(y_new>=y_old)]
  y_high <- c(0,0,0)
  y_high[which(y_new<=y_old)] <- y_new[which(y_new<=y_old)]
  y_highest <- c(0,0,0)
  y_highest[which(y_new<y_old)] <- y_old[which(y_new<y_old)] - y_new[which(y_new<y_old)]
  
  t <- list(
    family = "arial",
    size = 40,
    color = 'rgba(103, 169, 207, 1)')
  percentages <- paste(as.character(y_new * 100), '%', sep = '')
  
  p <- plot_ly(x = x, y = y_low, type = "bar",  height = 600,
          marker = list(color = 'rgba(103, 169, 207, 1)',
                        line = list(color = 'rgb(255,255,255)', width = 1)),
          name = "Current", hoverinfo = "none", showlegend = FALSE) %>%
    add_trace(y = y_mid, marker = list(color = 'rgba(103, 169, 207, 1)',
                                       line = list(color = 'rgb(255,255,255)', width = 1)),
              name = "Adjusted", hoverinfo = "none", showlegend = FALSE) %>%
    add_trace(y = y_high, marker = list(color = 'rgba(103, 169, 207, 1)',
                                        line = list(color = 'rgb(255,255,255)', width = 1)),
              name = "Adjusted", hoverinfo = "none", showlegend = FALSE) %>%
    add_trace(y = y_highest, marker = list(color = 'rgba(103, 169, 207, .3)',
                                           line = list(color = 'rgb(255,255,255)', width = 1)),
              name = "Current", hoverinfo = "none", showlegend = FALSE) %>%
    layout(autosize = T,
           xaxis = list(title = ""),
           yaxis = list(title = '', range = c(0, 0.8), zeroline = T,showline = F, 
                        showticklabels = F, showgrid = F),
           barmode = 'stack') %>%
    add_annotations(text = percentages,
                    x = x,
                    y = y_new + 0.04,
                    #xref = "x",
                    #yref = "y",
                    font = list(family = 'Arial',
                                size = 35,
                                color = 'rgba(103, 169, 207, 1)'),
                    showarrow = FALSE) %>% 
    config(displayModeBar = F)
  
  return(p)
}