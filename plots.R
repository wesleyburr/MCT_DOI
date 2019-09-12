# load object 
# note: ensure file to be loaded is in the same folder / directory as this file 
load("MCT_for_DOI.RDa") 

# function that extracts a certain variable (specified by var) for each of the tests, 
# returns result as a vector (if 1-dimensional) or a list (if >1D)
extract <- function(individual_objects, var) {
  n <- length(individual_objects)
  results <- list()
  for (i in 1:n) {
    expression <- paste("individual_objects[[i]]$", var, sep = "")
    results[[i]] <- eval(parse(text = expression))  
  }
  if (length(results[[1]]) == 1) {
    results <- unlist(results)
  }
  return(results)
}

# plot 1
# histogram of num items and num students, side by side 
plot1 <- function() {
  num_items <- extract(individual_objects, "num_items")
  num_students <- extract(individual_objects, "num_students")
  par(mfrow = c(1, 2), xaxs = "i", yaxs = "i", mar = c(5, 5, 1, 0))
  hist(num_students, 
       main = NULL, 
       col = "dimgray", 
       xlab = "Number of students",
       ylim = c(0, 55), 
       ylab = "Number of tests", 
       breaks = 20, 
       font.lab = 2, 
       las = 1,
       cex.lab = 1.25)
  legend(x = "topright", legend = "A", bty = 'n')
  box("plot", lwd = 1)
  par(mar = c(5, 1, 1, 4))
  x <- hist(num_items, 
            main = NULL, 
            col = "dimgray", 
            xlab = "Number of MC items",
            ylim = c(0, 55), 
            axes = FALSE, 
            ylab = NA, 
            breaks = 20, 
            font.lab = 2, 
            las = 1,
            cex.lab = 1.25)
  axis(side = 1, at = x$breaks[seq(from = 1, to = length(x$breaks), by = 4)])
  legend(x = "topright", legend = "B", bty = 'n')
  box("plot", lwd = 1)
}
png(filename = "hist_num_items_num_students.png", width = 7.5, height = 3.25, 
    units = "in",
    res = 72)
plot1()
dev.off()
pdf(file = "hist_num_items_num_students.pdf", width = 7.5, height = 3.25)
plot1()
dev.off()

# plot 2 
# histogram of item difficulties
plot2 <- function() {
  difficulties <- unlist(extract(individual_objects, "difficulty_indices"))
  xlabel <- substitute(paste(bold("Item difficulty, "), bolditalic("p")))
  par(xaxs = "i", yaxs = "i", mar = c(5, 5, 1, 1))
  hist(difficulties, 
       main = NULL, 
       col = "dimgray", 
       xlab = xlabel,
       ylab = "Number of MC items", 
       breaks = 20, 
       font.lab = 2, 
       las = 1,
       ylim = c(0, 1100),
       cex.lab = 1.25)
  box("plot", lwd = 1)
}
png(filename = "hist_item_difficulty.png", 
    height = 3.25, 
    width = 7.5, 
    units = "in",
    res = 72)
plot2()
dev.off()
pdf(file = "hist_item_difficulty.pdf",
    height = 3.25,
    width = 7.5)
plot2()
dev.off()

# plot 3 
# relative position of item on test 
difficulties <- list()
positions <- list()
for (i in 1:length(individual_objects)) {
  difficulties[[i]] <- individual_objects[[i]]$difficulty_indices
  num_items <- individual_objects[[i]]$num_items
  positions[[i]] <- c(0, 0, (3:num_items) / num_items)
}
difficulties <- unlist(difficulties)
positions <- unlist(positions)

ranges <- seq(0, 1.0, by = 0.10)
list_ranges <- vector(length = length(ranges), mode = "list")
for (i in 1:length(difficulties)) {
  j <- 1
  if (positions[i] == 0) {
    list_ranges[[j]] <- c(list_ranges[[j]], difficulties[i])
  } else {
    for (j in 2:(length(ranges)-1)) {
      if (positions[i] >= ranges[j-1] & positions[i] < ranges[j]) {
        list_ranges[[j]] <- c(list_ranges[[j]], difficulties[i])
      }
    }
    j <- length(ranges)
    if (positions[i] >= ranges[j-1] & positions[i] <= ranges[j]) {
      list_ranges[[j]] <- c(list_ranges[[j]], difficulties[i])
    } 
  }
}
avg_diff_by_range <- unlist(lapply(list_ranges, FUN = "mean"))
sd_diff_by_range <- unlist(lapply(list_ranges, FUN = "sd"))

plot3 <- function() {
  index <- c(0.025, seq(0.05, 0.95, by = 0.10))
  axis_labels <- seq(0, 1.0, by = 0.10)
  ylabel <- substitute(paste(bold("Item difficulty, "), bolditalic("p")))
  par(mar = c(5, 5, 1, 1))
  plot(index, avg_diff_by_range, 
       ylab = ylabel,
       xlab = "Relative position of item on test",
       xaxt = "n",
       pch = 19,
       cex.lab = 1.25, 
       #ylim = c(0.3, 0.9),
       font.lab = 2)
  axis(at = axis_labels, side = 1, labels = axis_labels)
  # arrows(index, 
  #        avg_diff_by_range - sd_diff_by_range,
  #        index,
  #        avg_diff_by_range + sd_diff_by_range, 
  #        angle = 90,
  #        code = 3,
  #        length = 0.05)
}
png(filename = "difficulty_eleven.png",
    height = 3.25, 
    width = 7.5, 
    units = "in", 
    res = 72)
plot3()
dev.off()
pdf(file = "difficulty_eleven.pdf",
    height = 3.25,
    width = 7.5)
plot3()
dev.off()

# plot 4 
# average test scores
plot4 <- function() {
  avg_test_scores <- extract(individual_objects, "mean_student_scores")
  par(mar = c(5, 5, 1, 1), xaxs = "i", yaxs = "i")
  hist(avg_test_scores, 
       xlab = "Test scores", 
       ylab = "Number of tests", 
       col = "dimgray",
       main = NULL,
       font.lab = 2,
       las = 1,
       breaks = seq(0.4, 0.9, by = 0.025),
       cex.lab = 1.25,
       ylim = c(0, 35))
  box("plot", lwd = 1)
}
png(filename = "hist_test_scores.png",
    height = 3.25, 
    width = 7.5, 
    units = "in", 
    res = 72)
plot4()
dev.off()
pdf(file = "hist_test_scores.pdf",
    height = 3.25,
    width = 7.5)
plot4()
dev.off()

# plot 5 
# discrimination histograms on top of each other 
r_values <- unlist(extract(individual_objects, "r_values"))
r_prime_values <- unlist(extract(individual_objects, "r_prime_values"))

plot5 <- function() {
  colours <- c(rep("black", 13), 
               rep("#808080", 3),
               rep("#C0C0C0", 4),
               rep("white", 9))
  par(mfrow = c(2, 1), xaxs = "i", yaxs = "i")
  par(mar = c(1, 5, 3, 1))
  hist(r_values, 
       xlim = c(-0.5, 0.8), 
       col = "dimgray",
       xlab = "r", 
       main = NULL,
       xaxt = "n",
       ylab = "",
       font.lab = 2,
       las = 1,
       ylim = c(0, 1600),
       breaks = 20)
  legend(x = "topleft", inset = 0.05, legend = substitute(paste(bold("Item-included, "), bolditalic(r[pb]))),
         bty = 'n')
  mtext(text = "Total Number of Items",
        side = 2, 
        at = 750, 
        cex = 1,
        line = 4,
        font = 2)
  box("plot", lwd = 1)
  par(mar = c(4, 5, 0, 1))
  hist(r_prime_values, 
       xlim = c(-0.5, 0.8), 
       col = colours,
       xlab = "", 
       main = NULL,
       ylab = "",
       font.lab = 2,
       las = 1, 
       ylim = c(0, 1600),
       breaks = 20)
  legend(x = "topleft", inset = 0.05, legend = substitute(paste(bold("Item-excluded, "), bolditalic("r'"))),
         bty = 'n')
  box("plot", lwd = 1)
  mtext(text = "Total Number of Items",
        side = 2, 
        at = 750, 
        cex = 1,
        line = 4,
        font = 2)
   mtext(text = "Individual Item Discrimination",
        side = 1, 
        at = 0.1, 
        cex = 1,
        line = 3,
        font = 2)
}
png(filename = "hist_r_r_prime.png",
    height = 5, 
    width = 7.5, 
    units = "in", 
    res = 72)
plot5()
dev.off()
pdf(file = "hist_r_r_prime.pdf",
    height = 5,
    width = 7.5)
plot5()
dev.off()

# plot 6
# mean discrimination (histograms on top of each other)
mean_r_values <- extract(individual_objects, "mean_r_values")
mean_r_prime_values <- extract(individual_objects, "mean_r_prime_values")
plot6 <- function() {
  par(mfrow = c(2, 1), xaxs = "i", yaxs = "i")
  par(mar = c(1, 5, 3, 1))
  hist(mean_r_values, 
       xlim = c(0, 0.5), 
       col = "dimgrey",
       xlab = NULL,
       main = NULL,
       font.lab = 2,
       las = 1,
       xaxt = "n",
       ylab = NULL,
       ylim = c(0, 35),
       breaks = seq(0, 0.5, by = 0.025))
  legend(x = "topleft", inset = 0.05, legend = substitute(paste(bold("Item-included, "), bolditalic(bar(r)[pb]))),
         bty = 'n')
  mtext(text = "Total Number of Tests",
        side = 2, 
        at = 20, 
        cex = 1,
        line = 4,
        font = 2)
  box("plot", lwd = 1)
  par(mar = c(4, 5, 0, 1))
  hist(mean_r_prime_values, 
       xlim = c(0, 0.5), 
       col = "lightgrey",
       xlab = "Mean Item Discrimination (by test)", 
       main = NULL,
       font.lab = 2,
       las = 1,
       ylab = NULL, 
       ylim = c(0, 35),
       breaks = seq(0, 0.5, by = 0.025))
  legend(x = "topleft", inset = 0.05, legend = substitute(paste(bold("Item-excluded, "), bolditalic(bar("r'")))),
         bty = 'n')
  mtext(text = "Total Number of Tests",
        side = 2, 
        at = 18, 
        cex = 1,
        line = 4,
        font = 2)
  box("plot", lwd = 1)
}
png(filename = "hist_mean_r_r_prime.png",
    height = 5, 
    width = 7.5, 
    units = "in", 
    res = 72)
plot6()
dev.off()
pdf(file = "hist_mean_r_r_prime.pdf",
    height = 5,
    width = 7.5)
plot6()
dev.off()

# plot 6 but with density plots stacked instead
plot6_dens_stacked <- function() {
  dens_mean_r <- density(mean_r_values)
  dens_mean_r_prime <- density(mean_r_prime_values)
  par(mfrow = c(2, 1), xaxs = "i", yaxs = "i")
  par(mar = c(1, 5, 1, 1))
  plot(dens_mean_r$x, dens_mean_r$y,
       type = "l",
       xlab = "",
       main = "",
       las = 1,
       xaxt = "n",
       ylab = "",
       ylim = c(0, 6))
  box("plot", lwd = 1)
  par(mar = c(2, 5, 0, 1))
  plot(dens_mean_r_prime$x, dens_mean_r_prime$y, 
       type = "l",
       xlab = "", 
       main = "",
       font.lab = 2,
       las = 1,
       ylab = "", 
       ylim = c(0, 6))
  box("plot", lwd = 1)
}
png(filename = "dens_mean_r_r_prime.png",
    height = 5, 
    width = 7.5, 
    units = "in", 
    res = 72)
plot6_dens_stacked()
dev.off()
pdf(file = "dens_mean_r_r_prime.pdf",
    height = 5,
    width = 7.5)
plot6_dens_stacked()
dev.off()

# plot 6 but overlayed
plot6_dens_overlay <- function() {
  dens_mean_r <- density(mean_r_values)
  dens_mean_r_prime <- density(mean_r_prime_values)
  par(xaxs = "i", yaxs = "i")
  par(mar = c(4, 5, 1, 1))
  plot(dens_mean_r$x, dens_mean_r$y,
       type = "l",
       xlab = "Mean item discrimination by test",
       main = "",
       las = 1,
       ylab = "",
       ylim = c(0, 6),
       xlim = c(-0.05, 0.55),
       font.lab = 2,
       cex.lab = 1.25)
  box("plot", lwd = 1)
  lines(dens_mean_r_prime$x, dens_mean_r_prime$y, 
       type = "l",
       col = "grey")
  abline(h = 0, col = "black")
}
png(filename = "overlay_dens_mean_r_r_prime.png",
    height = 3.25, 
    width = 7.5, 
    units = "in", 
    res = 72)
plot6_dens_overlay()
dev.off()
pdf(file = "overlay_dens_mean_r_r_prime.pdf",
    height = 3.25,
    width = 7.5)
plot6_dens_overlay()
dev.off()

# plot 7 
# side by side boxplots of discrimination by year 
years <- extract(individual_objects, "year_level")
mean_r_prime_values <- extract(individual_objects, "mean_r_prime_values")
first_year <- NULL 
second_year <- NULL
third_year <- NULL 
fourth_year <- NULL 
for (i in 1:length(mean_r_prime_values)) {
  if (years[i] == "1") {
    first_year <- c(first_year, mean_r_prime_values[i])
  } else {
    if (years[i] == "2") {
      second_year <- c(second_year, mean_r_prime_values[i])
    } else {
      if (years[i] == "3") {
        third_year <- c(third_year, mean_r_prime_values[i])
      } else {
        fourth_year <- c(fourth_year, mean_r_prime_values[i])
      }
    }
  }
}
plot7 <- function() {
  par(xaxs = "i", yaxs = "i", mar = c(5, 5, 1, 1), font.lab = 2)
  boxplot(first_year, second_year, third_year, fourth_year,
          xlab = "Instructional level (year)",
          ylab = "",
          ylim = c(0, 0.45),
          names = c("1", "2", "3", "4"),
          cex.lab = 1.25,
          las = 1)
  mtext(text = "Mean item discrimination",
        side = 2, 
        at = 0.21, 
        cex = 1,
        line = 4,
        font = 2)
  mtext(text = "by test", 
        side = 2, 
        at = 0.21, 
        cex = 1,
        line = 3,
        font = 2)
}
png(filename = "instructional_level_boxplot.png",
    height = 3.25, 
    width = 7.5, 
    units = "in", 
    res = 72)
plot7()
dev.off()
pdf(file = "instructional_level_boxplot.pdf",
    height = 3.25,
    width = 7.5)
plot7()
dev.off()

# plot 8
# discrimination vs difficulty 
r_prime_values <- unlist(extract(individual_objects, "r_prime_values"))
difficulties <- unlist(extract(individual_objects, "difficulty_indices"))
ranges <- seq(0.1, 0.9, by = 0.05)
axis_labels <- vector(length = length(ranges) + 1)
axis_labels[1] <- paste0("<", ranges[1])
axis_labels[length(ranges) + 1] <- paste0(">", ranges[length(ranges)])
for(i in 2:(length(ranges))) {
  axis_labels[i] <- paste0(ranges[i - 1], "-", ranges[i])
}
diff_by_range <- vector(length = length(axis_labels), mode = "list")
for (i in 1:length(r_prime_values)) {
  for (j in 1:length(axis_labels)) {
    if (j == 1) {
      if (difficulties[i] < ranges[j]) {
        diff_by_range[[j]] <- c(diff_by_range[[j]], r_prime_values[i])
      } 
    } else {
      if (j %in% 2:length(ranges)) {
        if (difficulties[i] >= ranges[j-1] & difficulties[i] < ranges[j]) {
          diff_by_range[[j]] <- c(diff_by_range[[j]], r_prime_values[i])
        } 
      } else {
        if (j == length(axis_labels)) {
          if (difficulties[i] > ranges[j-1]) {
            diff_by_range[[j]] <- c(diff_by_range[[j]], r_prime_values[i])
          }
        }
      }
    }
  }
}
sd_diff_by_range <- unlist(lapply(diff_by_range, FUN = "sd", na.rm = TRUE))
diff_by_range <- unlist(lapply(diff_by_range, FUN = "mean", na.rm = TRUE))
index <- 1:length(axis_labels)
xlabel <- substitute(paste(bold("Item difficulty, "), bolditalic("p")))
ylabel <- substitute(paste(bold("Item-excluded discrimination,  "), bolditalic("r'")))
plot8 <- function() {
  par(mar = c(7, 5, 1, 1), xaxs = "i", yaxs = "i")
  plot(NULL, NULL,
       xaxt = "n",
       ylab = "",
       xlab = "",
       pch = 19, 
       main = NULL, 
       ylim = c(-0.2, 0.5),
       xlim = c(1, index[length(index)]),
       font.lab = 2, 
       las = 1)
  polygon(c(index, rev(index)), 
          c(diff_by_range - sd_diff_by_range, rev(diff_by_range + sd_diff_by_range)), 
          col = "lightgrey",
          border = TRUE)
  points(index, diff_by_range, pch = 19, cex = 1.25)
  lines(index, diff_by_range, pch = 19, cex = 1.25)
  axis(side = 1, at = index, labels = axis_labels, las = 2)
  mtext(text = xlabel, side = 1, line = 5, at = 10)
  mtext(text = ylabel, side = 2, line = 3, at = 0.1, cex = 1)
}
png(filename = "r_prime_vs_p.png",
    height = 5.00, 
    width = 7.5, 
    units = "in", 
    res = 72)
plot8()
dev.off()
pdf(file = "r_prime_vs_p.pdf",
    height = 5.00,
    width = 7.5)
plot8()
dev.off()

# plot 9
# cronbachs alpha 
cron_alpha <- extract(individual_objects, "cronbach_alpha")
adj_cron_alpha <- extract(individual_objects, "adjusted_cronbach_alpha")
plot9 <- function() {
  ylabel <- substitute(paste(bold("Cronbach's alpha, "), bold(alpha)))
  xlabel <- substitute(paste(bold("Item-total Point-biserial correlation, "), bolditalic(bar(r)[pb])))
  ylabel2 <- substitute(paste(bold("Adjusted Cronbach's alpha, "), bolditalic(alpha["50"])))
  xlabel2 <- substitute(paste(bold("Item-excluded correlation, "), bolditalic(bar("r'"))))
  par(mfrow = c(1, 2), xaxs = "i", yaxs = "i")
  par(mar = c(5, 5, 1, 0.5))
  plot(mean_r_values, cron_alpha,
       xlim = c(0, 0.5),
       ylim = c(0.2, 1.0),
       main = NULL,
       xlab = "",
       ylab = "",
       cex.lab = 0.75,
       pch = 19,
       font.lab = 2,
       las = 1, 
       xaxt = "n")
  axis(side = 1, 
       at = c(0, 0.1, 0.2, 0.3, 0.4),
       labels = c(0, 0.1, 0.2, 0.3, 0.4))
  mtext(text = xlabel, 
        side = 1, 
        line = 3, 
        font = 2, 
        at = 0.2)
  mtext(text = ylabel, 
        side = 2, 
        line = 3, 
        font = 2, 
        cex = 1)
  par(mar = c(5, 0.5, 1, 5))
  plot(mean_r_prime_values, adj_cron_alpha,
       xlim = c(0, 0.5),
       ylim = c(0.2, 1.0),
       main = NULL,
       xlab = "",
       yaxt = "n",
       cex.lab = 0.75,
       pch = 19,
       font.lab = 2,
       las = 1,
       xaxt = "n")
  axis(side = 1,
       at = c(0, 0.1, 0.2, 0.3, 0.4),
       labels = c(0, 0.1, 0.2, 0.3, 0.4))
  axis(side = 4, 
       at = seq(0.2, 1.0, by = 0.2), 
       labels = c("0.2", "0.4", "0.6", "0.8", "1.0"),
       las = 1)
  mtext(text = ylabel2, 
        side = 4, 
        line = 3,
        at = 0.5,
        cex = 1,
        font = 2)
  mtext(text = xlabel2, 
        side = 1, 
        line = 3, 
        font = 2, 
        at = 0.25)
}
png(filename = "cronbach_scatterplot.png",
    height = 3.25, 
    width = 7.5, 
    units = "in", 
    res = 72)
plot9()
dev.off()
pdf(file = "cronbach_scatterplot.pdf",
    height = 3.25,
    width = 7.5)
plot9()
dev.off()

