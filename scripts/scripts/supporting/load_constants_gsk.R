# Define constants for project

# Set directories
input_dir <- "input"
output_dir <- "output"

# # set default color choices for background and font
body_bg_col <- "white"
text_col <- "black"

# We may allow for template to be user specified, so putting the file name here along with 
# other template specific arguments
template_filename <-
  "gsk_template.pptx"

# Set master value name
master_value <- "GSK"

# Set layouts
title_content_layout <- "Title and Content"
header_layout <- "Title Slide Blank"
section_layout <- "Section Header WHITE with ORANGE Text"

# Set types
header_type <- "ctrTitle"
title_type <- "title"

# Set placeholder
section_ph <- "Text Placeholder 3"
section_ph_2 <- "Text Placeholder 5"

# set default color choices for background and font
body_bg_col <- "white"
text_col <- "black"

# Define measurements for slide positioning and plotting
slide_width <- 10
slide_height <- 5.625

# set plot sizing information
plot_height <- 4
plot_width <- 8.5
top_margin <- 1.3

# define color choices for charting
arrow_col <- "#FF4D00"

# set default font choices
# font_name <- "Roboto"
font_name <- "Arial"
font_size <- 12

# set default number of decimals

num_decimals <- 1

gsk_red <- rgb(255, 25, 13, maxColorValue = 255)
gsk_blue <- rgb(0, 176, 255, maxColorValue = 255)
gsk_green <- rgb(89, 255, 0, maxColorValue = 255)
gsk_grey <- rgb(166, 162, 158, maxColorValue = 255)
gsk_purple <- rgb(192, 0, 235, maxColorValue = 255)
gsk_teal <- rgb(45, 235, 181, maxColorValue = 255)

gsk_orange <- "#F36633"

gsk_yellow <- "#FFD900"
gsk_green2 <- "#00D900"

gsk_grad_low <- "#0000FF"
gsk_grad_med <- "#00FFFF"
gsk_grad_high <- "#FF19FF"

gsk_pink <- gsk_grad_high

gsk_pal <- c(gsk_red, gsk_green, gsk_blue, gsk_purple, gsk_teal, gsk_grey, gsk_yellow, gsk_green2, gsk_orange) %>% 
  set_names(c("red", "green", "blue", "purple", "teal", "grey", "yellow", "other green", "orange"))

cluster_pal <- c(gsk_grad_low, gsk_green2, gsk_purple)

#cluster_cols %>% show_col()

#gsk_pal %>% show_col()
