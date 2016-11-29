library(rmarkdown)

render("../4-Visualization.R", output_dir=".")
render("../5-hypTest.R", output_dir=".")


render("../4-Visualization.boost.R", output_dir=".")
render("../5-hypTest.R", output_dir=".", output_file = "5-hypTest.boost.pdf")
