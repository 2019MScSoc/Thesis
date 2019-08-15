library(SpeedReader)
library(quanteda)
library(viridis)
library(ggplot2)

setwd('~/Box Sync/*OxfordMSc/Thesis')

load(file = './ProcessedData/fightin_words.RData')

# fightin' words plots created using code from Denny's fightin_words_plot()
# function in SpeedReader

max_terms_to_display <- 100000
display_top_words <- 20

################################################################################
############### FW Plot 1: /r/liberalgunowners vs. /r/progun ###################
################################################################################

z_scores <- fw1_trimmed[[3]]$z_scores
zeta <- fw1_trimmed[[3]]$scores
y.tot <- fw1_trimmed[[3]]$total_count
words <- fw1_trimmed[[3]]$terms

tot <- length(zeta)
max_y.tot <- max(y.tot)

max.zeta.one <- which(z_scores > 1.96)[1:display_top_words]
max.zeta.two <- which(z_scores < -1.96)
max.zeta.two <- max.zeta.two[(length(max.zeta.two) - 
                                display_top_words + 1):length(max.zeta.two)]
max.zeta.two <- rev(max.zeta.two)

display_limits <- 1.2 * max(abs(zeta))
ylims <- c(-display_limits, display_limits)
xlims <- c(1, 2 * max_y.tot)

sig.z <- abs(z_scores) > 1.96
psize <- 2 * abs(zeta)/max(abs(zeta))

png('./Figures/FW1.png', width = 6, height = 4, units = 'in', 
    res = 300)

options(scipen = 999)
par(mar = c(3, 3, 1, 4), family = 'sans')

plot(xlims, ylims, type = "n", log = "x", pch = 19, col = "black", 
     cex = psize, xlab = '', ylab = '')
mtext(text = 'term count', side = 1, line = 1.8)
mtext(text = expression(italic(z) - score), side = 2, line = 1.8)
points(y.tot, zeta, pch = 19, col = "gray", cex = psize)
points(y.tot[sig.z], zeta[sig.z], pch = 19, col = "black", 
       cex = psize[sig.z])
points(y.tot[max.zeta.one], zeta[max.zeta.one], pch = 19, 
       col = viridis(3)[1], cex = psize[max.zeta.one])
points(y.tot[max.zeta.two], zeta[max.zeta.two], pch = 19, 
       col = viridis(3)[2], cex = psize[max.zeta.two])
mtext(text = words[max.zeta.one], side = 4, col = viridis(3)[1],
      las = 1, line = 1, at = seq(display_limits, 0.1*display_limits,
                                  length.out = display_top_words),
      cex = 0.65)
mtext(text = words[max.zeta.two], side = 4, col = viridis(3)[2],
      las = 1, line = 1, at = seq(-display_limits, -0.05*display_limits,
                                  length.out = display_top_words),
      cex = 0.65)
text(1, 0.9 * display_limits, '/r/liberalgunowners', col = viridis(3)[1], 
     pos = 4, cex = 1.5)
text(1, -0.9 * display_limits, '/r/progun', col = viridis(3)[2], 
     pos = 4, cex = 1.5)

dev.off()

################################################################################
############### FW Plot 2: /r/liberalgunowners vs. /r/guns #####################
################################################################################

z_scores <- fw2_trimmed[[3]]$z_scores
zeta <- fw2_trimmed[[3]]$scores
y.tot <- fw2_trimmed[[3]]$total_count
words <- fw2_trimmed[[3]]$terms

tot <- length(zeta)
max_y.tot <- max(y.tot)

max.zeta.one <- which(z_scores > 1.96)[1:display_top_words]
max.zeta.two <- which(z_scores < -1.96)
max.zeta.two <- max.zeta.two[(length(max.zeta.two) - 
                                display_top_words + 1):length(max.zeta.two)]
max.zeta.two <- rev(max.zeta.two)

display_limits <- 1.2 * max(abs(zeta))
ylims <- c(-display_limits, display_limits)
xlims <- c(1, 2 * max_y.tot)

sig.z <- abs(z_scores) > 1.96
psize <- 2 * abs(zeta)/max(abs(zeta))

png('./Figures/FW2.png', width = 6, height = 4, units = 'in', 
    res = 300)

options(scipen = 999)
par(mar = c(3, 3, 1, 4), family = 'sans')

plot(xlims, ylims, type = "n", log = "x", pch = 19, col = "black", 
     cex = psize, xlab = '', ylab = '')
mtext(text = 'term count', side = 1, line = 1.8)
mtext(text = expression(italic(z) - score), side = 2, line = 1.8)
points(y.tot, zeta, pch = 19, col = "gray", cex = psize)
points(y.tot[sig.z], zeta[sig.z], pch = 19, col = "black", 
       cex = psize[sig.z])
points(y.tot[max.zeta.one], zeta[max.zeta.one], pch = 19, 
       col = viridis(3)[1], cex = psize[max.zeta.one])
points(y.tot[max.zeta.two], zeta[max.zeta.two], pch = 19, 
       col = inferno(4)[3], cex = psize[max.zeta.two])
mtext(text = words[max.zeta.one], side = 4, col = viridis(3)[1],
      las = 1, line = 1, at = seq(display_limits, 0.1*display_limits,
                                  length.out = display_top_words),
      cex = 0.65)
mtext(text = words[max.zeta.two], side = 4, col = inferno(4)[3],
      las = 1, line = 1, at = seq(-display_limits, -0.05*display_limits,
                                  length.out = display_top_words),
      cex = 0.65)
text(1, 0.9 * display_limits, '/r/liberalgunowners', col = viridis(3)[1], 
     pos = 4, cex = 1.5)
text(1, -0.9 * display_limits, '/r/guns', col = inferno(4)[3], 
     pos = 4, cex = 1.5)

dev.off()

################################################################################
############### FW Plot 3: /r/progun vs. /r/guns ###############################
################################################################################

z_scores <- fw3_trimmed[[3]]$z_scores
zeta <- fw3_trimmed[[3]]$scores
y.tot <- fw3_trimmed[[3]]$total_count
words <- fw3_trimmed[[3]]$terms

tot <- length(zeta)
max_y.tot <- max(y.tot)

max.zeta.one <- which(z_scores > 1.96)[1:display_top_words]
max.zeta.two <- which(z_scores < -1.96)
max.zeta.two <- max.zeta.two[(length(max.zeta.two) - 
                                display_top_words + 1):length(max.zeta.two)]
max.zeta.two <- rev(max.zeta.two)

display_limits <- 1.2 * max(abs(zeta))
print(display_limits)
ylims <- c(-display_limits, display_limits)
xlims <- c(1, 2 * max_y.tot)

sig.z <- abs(z_scores) > 1.96
psize <- 2 * abs(zeta)/max(abs(zeta))

png('./Figures/FW3.png', width = 6, height = 4, units = 'in', 
    res = 300)

options(scipen = 999)
par(mar = c(3, 3, 1, 4), family = 'sans')

plot(xlims, ylims, type = "n", log = "x", pch = 19, col = "black", 
     cex = psize, xlab = '', ylab = '')
mtext(text = 'term count', side = 1, line = 1.8)
mtext(text = expression(italic(z) - score), side = 2, line = 1.8)
points(y.tot, zeta, pch = 19, col = "gray", cex = psize)
points(y.tot[sig.z], zeta[sig.z], pch = 19, col = "black", 
       cex = psize[sig.z])
points(y.tot[max.zeta.one], zeta[max.zeta.one], pch = 19, 
       col = viridis(3)[2], cex = psize[max.zeta.one])
points(y.tot[max.zeta.two], zeta[max.zeta.two], pch = 19, 
       col = inferno(4)[3], cex = psize[max.zeta.two])
mtext(text = words[max.zeta.one], side = 4, col = viridis(3)[2],
      las = 1, line = 1, at = seq(display_limits, 0.1*display_limits,
                                  length.out = display_top_words),
      cex = 0.65)
mtext(text = words[max.zeta.two], side = 4, col = inferno(4)[3],
      las = 1, line = 1, at = seq(-display_limits, -0.05*display_limits,
                                  length.out = display_top_words),
      cex = 0.65)
text(1, 0.9 * display_limits, '/r/progun', col = viridis(3)[2], 
     pos = 4, cex = 1.5)
text(1, -0.9 * display_limits, '/r/guns', col = inferno(4)[3], 
     pos = 4, cex = 1.5)

dev.off()

################################################################################
####################### Document Frequency Plot ################################
################################################################################

load('./ProcessedData/RDataAug12_trimmed.RData')

doc_freq_liberal <- docfreq(liberal_dfm_trimmed)
doc_freq_progun <- docfreq(progun_dfm_trimmed)
doc_freq_guns <- docfreq(guns_dfm_trimmed)

bar_df <- data.frame(sub = character(15), feature = character(15), 
                     freq = numeric(15), stringsAsFactors = FALSE)

bar_df$sub <- rep(c('lib', 'progun', 'guns'), times = 5)
bar_df$feature[1:3] <- 'liber'
bar_df$feature[4:6] <- 'post'
bar_df$feature[7:9] <- 'govern'
bar_df$feature[10:12] <- 'crime'
bar_df$feature[13:15] <- 'nra'

bar_df$freq[1] <- doc_freq_liberal['liber']/ndoc(liberal_dfm_trimmed)
bar_df$freq[2] <- doc_freq_progun['liber']/ndoc(progun_dfm_trimmed)
bar_df$freq[3] <- doc_freq_guns['liber']/ndoc(guns_dfm_trimmed)
bar_df$freq[4] <- doc_freq_liberal['post']/ndoc(liberal_dfm_trimmed)
bar_df$freq[5] <- doc_freq_progun['post']/ndoc(progun_dfm_trimmed)
bar_df$freq[6] <- doc_freq_guns['post']/ndoc(guns_dfm_trimmed)
bar_df$freq[7] <- doc_freq_liberal['govern']/ndoc(liberal_dfm_trimmed)
bar_df$freq[8] <- doc_freq_progun['govern']/ndoc(progun_dfm_trimmed)
bar_df$freq[9] <- doc_freq_guns['govern']/ndoc(guns_dfm_trimmed)
bar_df$freq[10] <- doc_freq_liberal['crime']/ndoc(liberal_dfm_trimmed)
bar_df$freq[11] <- doc_freq_progun['crime']/ndoc(progun_dfm_trimmed)
bar_df$freq[12] <- doc_freq_guns['crime']/ndoc(guns_dfm_trimmed)
bar_df$freq[13] <- doc_freq_liberal['nra']/ndoc(liberal_dfm_trimmed)
bar_df$freq[14] <- doc_freq_progun['nra']/ndoc(progun_dfm_trimmed)
bar_df$freq[15] <- doc_freq_guns['nra']/ndoc(guns_dfm_trimmed)

png('./Figures/DocFreqs.png', width = 6, height = 4, units = 'in', 
    res = 300)
ggplot(data = bar_df, aes(x = feature, y = freq, fill = sub)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  scale_fill_manual(values = c(inferno(4)[3], viridis(3)[2], viridis(3)[1]),
                    name = 'Subreddit', 
                    labels = c('/r/guns', '/r/liberalgunowners', '/r/progun')) + 
  labs(x = 'Feature', y = 'Document Frequency') 
dev.off()







