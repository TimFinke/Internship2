library("tidyverse")
library("ganttrify")
library("lubridate")

# tibble dates
gantt_chart <- tibble(
  "wp" = c(rep("Project 1", 7), rep("Project 2", 5), rep("Knowledge Exchange", 3)),
  "activity" = c("Literature study", "Data Request", "Data Cleaning", "Statistical Analyses", 
                 "Results", "Discussion", " Final Revision",
                 "Literature study ", "Statistical Analyses ", 
                 "Results ", "Discussion ", " Final Revision ",
                 "Symposium 1", "Symposium 2", "Dissertation "),
  "start_date" = c("2021-02-01", "2021-02-21", "2021-03-01", "2021-03-07",
                   "2021-03-18", "2021-04-07", "2021-06-01",
                   "2021-08-01", "2021-09-01", 
                   "2021-09-18", "2021-10-07", "2021-12-01",
                   "2021-06-19", "2021-12-18", "2021-12-18"),
  "end_date" = c("2021-03-14", "2021-03-04", "2021-03-09", "2021-03-31",
                 "2021-04-30", "2021-05-31", "2021-06-18",
                 "2021-09-30", "2021-09-30", 
                 "2021-10-31", "2021-11-30", "2021-12-18",
                 "2021-07-31", "2022-01-10", "2022-01-20")
  )
# dates for spots
gantt_spots <- tibble(
  "activity" = c(rep("Project 1", 6), rep("Project 2", 6), rep("Knowledge Exchange", 5)),
  "spot_type" = c(rep("M", 12), rep("S", 2), "D", rep("PR", 2)),
  "spot_date" = c("2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01", "2021-06-18",
                  "2021-08-01", "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01", "2021-12-18",
                  "2021-07-31", "2022-01-10", "2022-01-20", "2021-06-19", "2021-12-18")
)

# create gantt chart
chart_gantt <- ganttrify(project = gantt_chart,
                         spots = gantt_spots,
                         by_date = TRUE,
                         exact_date = TRUE,
                         size_text_relative = 1,
                         month_number_label = FALSE,
                         font_family = "sans",
                         mark_quarters = TRUE,
                         mark_years = TRUE,
                         size_wp = 6,
                         hide_wp = FALSE,
                         size_activity = 3,
                         colour_stripe = "Grey75",
                         x_axis_position = "top")
chart_gantt
# save image
ts = as.numeric(Sys.time())
out_fname = sprintf("./plots/gantt_chart_%.0f", ts)
png(str_c(out_fname, ".png"), width = 9, height = 9, units = "in", res = 300)
chart_gantt
dev.off()