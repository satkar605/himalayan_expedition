# scripts/screenshot.R

# Install required packages if not present
if (!requireNamespace("shinytest2", quietly = TRUE)) install.packages("shinytest2")
if (!requireNamespace("webshot2", quietly = TRUE)) install.packages("webshot2")
if (!requireNamespace("fs", quietly = TRUE)) install.packages("fs")

library(shinytest2)
library(webshot2)
library(fs)

# Create 'viz' directory if it doesn't exist
viz_dir <- file.path(dirname(getwd()), "viz")
if (!dir_exists(viz_dir)) dir_create(viz_dir)

# Path to your app
app_path <- file.path("scripts", "app.R")

# Start the app for testing
app <- AppDriver$new(app_path, load_timeout = 10000)

# Helper to take screenshot of a tab
screenshot_tab <- function(tab_label, file_name) {
  app$set_inputs(`.panel` = tab_label)
  Sys.sleep(2) # Wait for UI to update
  app$expect_screenshot(file.path(viz_dir, paste0(file_name, ".png")))
}

# Take screenshots of each tab
screenshot_tab("Map View", "map_view")
screenshot_tab("Peak Insights", "peak_insights")
screenshot_tab("Expedition Details", "expedition_details")

# If you have an Overview tab, uncomment the next line:
# screenshot_tab("Overview", "overview")

# Stop the app
app$stop()

cat("Screenshots saved in", viz_dir, "\n")
