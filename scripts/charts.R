# source required scripts ----
source("./scripts/helpers.R")

# read the processed data ----
hqi <- readRDS("./data/processed/dependent_vars.RDS")

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
