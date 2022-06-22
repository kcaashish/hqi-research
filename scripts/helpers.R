library(plotly)

# calculate required summary statistics of variables ----
get_summary <- function(var) {
  return(
    list(
      "Min." = min(var),
      "1st Qu." = quantile(var, 0.25),
      "Median" = median(var),
      "Mean" = mean(var),
      "3rd Qu." = quantile(var, 0.75),
      "Max." = max(var),
      "Sd." = sd(var)
    )
  )
}

# get the frequency count of the variables ----
get_freq <- function(df, varri) {
  a <- count(df, varri) %>%
    rename("count" = "freq") %>% 
    tibble::add_column(score = which(levels(df[[varri]]) == .[[varri]]), .after = 1) %>% 
    bind_cols(., total = nrow(df)) %>% 
    mutate(per = round(count / total * 100, 2)) %>% 
    arrange(desc(score))
  
  b <- table(df[[varri]], df[["region"]]) %>% 
    as_tibble(.name_repair = ~c(varri, "region", "freq")) %>%
    pivot_wider(names_from = region, values_from = freq) %>% 
    mutate(tot_reg = rowSums(.[2:4])) %>% 
    mutate(
      himalaya_per = round(Himalaya / tot_reg * 100, 2),
      hill_per = round(Hill / tot_reg * 100, 2),
      terai_per = round(Terai / tot_reg * 100, 2)
    )
  
  ab <- left_join(a,
            b %>% select(varri, himalaya_per, hill_per, terai_per),
            by = c(varri))
  return(ab)
}


# function to create Pie charts ----
make_pie_chart <- function(variable, name, filename) {
  pie_df <- as.data.frame(table(variable))
  figure <-
    plot_ly(
      pie_df,
      labels = ~ variable,
      values = ~ Freq,
      type = "pie"
    ) %>%
    layout(
      title = list(
        text = paste0('<b>', name, '</b>'),
        x = 0.5,
        y = 0.98
      ),
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      legend = list(title = list(text = paste0(
        '<b>', name, '</b>'
      )))
    )
  return(figure)
}