library(plotly)

# read the processed data ----
hqi <- readRDS("./data/processed/dependent.RDS")

# function to create Pie charts ----
make_pie_chart <- function(variable, name, filename) {
  pie_df <- as.data.frame(table(variable))
  figure <-
    plot_ly(pie_df,
            labels = ~ variable,
            values = ~ Freq,
            type = "pie") %>%
    layout(
      title = list(text = paste0('<b>', name, '</b>'),
                   x = 0.5, y = 0.98),
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
      legend = list(title = list(text = paste0('<b>', name, '</b>')))
    )
  return(figure)
}

# create pie charts ----
fig1 <- make_pie_chart(hqi$house_own, "Types of house ownership")
fig1
fig2 <- make_pie_chart(hqi$mat_foundation, "Types of foundation")
fig2
fig3 <- make_pie_chart(hqi$mat_outerwall, "Types of wall")
fig3
fig4 <- make_pie_chart(hqi$mat_roof, "Types of roof")
fig4
fig5 <- make_pie_chart(hqi$source_water, "Types of water source")
fig5
fig6 <- make_pie_chart(hqi$source_fuel, "Types of fuel source")
fig6
fig7 <- make_pie_chart(hqi$source_light, "Types of light source")
fig7
fig8 <- make_pie_chart(hqi$type_toilet, "Types of toilet")
fig8
