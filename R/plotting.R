# =============================================================================
# > Plot a concentration, reaction rate, grid property, ... profile - echarts4r
# =============================================================================
plot_std_profile_echart <- function(df, entity, xlab = "", ylab = "", main = "") {

    df |>
    subset(name == entity) |>
    dplyr::group_by(tag) |>
    echarts4r::e_charts(depth) |>
    echarts4r::e_line(value, smooth = FALSE, symbol = "none", lineStyle = list(width = 3)) |>
    echarts4r::e_x_axis(
        name = ylab,
        nameGap = 30,
        nameLocation = "center",
        nameTextStyle = list(fontWeight = 'bold', fontSize = 15),
        inverse = TRUE
    ) |>
    echarts4r::e_y_axis(
        name = xlab,
        nameGap = 30,
        nameLocation = "center",
        nameTextStyle = list(fontWeight = 'bold', fontSize = 15)
    ) |>
    echarts4r::e_title(main) |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_legend(top = "bottom", type = "scroll") |>
    echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross"), showContent = FALSE)

}
