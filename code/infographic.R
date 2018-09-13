### Infographic
library(waffle)
library(extrafont)

font_import()

# check that Font Awesome is imported
fonts()[grep("Awesome", fonts())]
# [1] "FontAwesome"
# this should be fine for Mac OSX
loadfonts()
# use this if things look odd in RStudio under Windows
loadfonts(device = "win")

waffle(c(50, 30, 15, 5), rows = 5, title = "Your basic waffle chart")

waffle(c(50, 30, 15, 5), rows = 5, use_glyph = "child", glyph_size = 6, 
       title = "Look I made an infographic using R!")

values0 <- c("U" = 92, "LS" = 8, "LR" = 0)
pdf("~/Dropbox/MDR/output/info0.pdf")
waffle(values0, rows = 5)
dev.off()

values1 <- c("U" = 81, "LS" = 18, "LR" = 1)
pdf("~/Dropbox/MDR/output/info1.pdf")
waffle(values1, rows = 5)
dev.off()

values2 <- c("U" = 77, "LS" = 21, "LR" = 2)
pdf("~/Dropbox/MDR/output/info2.pdf")
waffle(values2, rows = 5)
dev.off()

### With age

values0 <- c("U" = 92, "LS" = 8, "LR" = 0)

iron(
waffle(c(50, 30, 15, 5), rows = 3),
waffle(c(50, 30, 15, 5), rows = 3)
)

       

