library(readr)
library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(readxl)
library(gridExtra)
library(stringr)
greinke <- read_csv("~/GitHub/greinke/greinke2015.csv")

# Print the first 6 rows of the data
head(greinke)

# Print the number of rows in the data frame
nrow(greinke)

str_replace()

# Summarize the start_speed variable
summary(greinke$start_speed)

# Get rid of data without start_speed

greinke <- greinke %>% filter(!is.na(start_speed))

# Print the number of rows in the data frame
nrow(greinke)

# Print the structure of greinke
glimpse(greinke)

# Check if dates are formatted as dates
class(greinke$game_date)

# Change them to dates
greinke$game_date <- mdy(greinke$game_date)

# Check that the variable is now formatted as a date
class(greinke$game_date)

# Separate game_date into "year", "month", and "day"
greinke <- separate(data = greinke, col = game_date,
                    into = c("year", "month", "day"),
                    sep = "-", remove = FALSE)

# Convert month to numeric
greinke$month <- as.numeric(greinke$month)

# Create the july variable
greinke <- greinke %>% mutate(july = ifelse(greinke$month == 7, "july", "other"))

# View the head() of greinke
head(greinke)

# Make a histogram of Greinke's start speed
ggplot(greinke, aes(start_speed)) + geom_histogram()

#Plot start_speed histogram from july and other
ggplot(greinke, aes(start_speed)) + geom_histogram() + facet_wrap(~ july)

#Plot histogram of fastball speeds for july and other month
greinke %>% filter(pitch_type == "FF") %>% ggplot(aes(start_speed)) + geom_histogram() + facet_wrap(~ july, scales = "free_y")

#Plot overlapping histogram of fastball speeds for july and other month
greinke %>% filter(pitch_type == "FF") %>% 
  ggplot(aes(start_speed, fill = july)) + geom_histogram(aes(y = ..density..), alpha = 0.3) + 
  geom_vline(aes(xintercept = mean(start_speed[july == "july"])), color = "red") + 
  geom_vline(aes(xintercept = mean(start_speed[july == "other"])), color = "blue") + 
  xlab("Velocity (mph)") + ggtitle("Greinke 4-Seam Fastball Velocity")

# Summarize velocity in July and other months
greinke %>% group_by(july) %>% summarise(mean(start_speed))

# Calculate mean fastball velocities: ff_velo_month 
ff_velo_month <- greinke %>% filter(pitch_type == "FF") %>% group_by(july) %>% summarise(mean(start_speed))

# Print ff_velo_month
ff_velo_month

# Create ff_dt
ff_dt <- greinke %>% filter(pitch_type == "FF") %>% group_by(game_date) %>% 
  summarise(mean(start_speed))

# Plot game-by-game 4-seam fastballs
ggplot(ff_dt, aes(x = game_date, y = start_speed)) + geom_line(lwd = 2) + ylim(c(88, 95)) + 
  xlab("Date") + ylab("Velocity (mph)") + ggtitle("Greinke 4-Seam Fastball Velocity") + 
  geom_point(data = greinke, position = "jitter", pch = 16, col = "#99004450")


# Subset the data to remove pitch types "IN" and "EP"
greinke <- greinke %>% filter(pitch_type != c("IN")) %>% filter(pitch_type != c("EP"))

# Create type_tab
type_tab <- table(greinke$pitch_type, greinke$july)

# Print type_tab
type_tab

# Create type_prop table
type_prop <- round(prop.table(type_tab, margin = 2), 3)

# Print type_prop
type_prop

# Create ff_prop
ff_prop <- type_prop[3, 1:2]

# Print ff_prop
ff_prop

# Print ff_velo_month
ff_velo_month

#convert type prop from table to dataframe
type_prop <- as.data.frame(type_prop) %>% spread(Var2, Freq)
names(type_prop) <- c("Pitch", "july", "other")

# Create the Difference column
type_prop$Difference <- (type_prop$july - type_prop$other)/type_prop$other

# Print type_prop
type_prop

# Plot a barplot
ggplot(type_prop, aes(Pitch, Difference)) + geom_bar(stat = "identity") + 
  ggtitle("Pitch Usage in July vs. Other Months") + 
  ylab("Percentage Change in July") + 
  ylim(c(-0.4, 0.4))
 
# Create bs_table
bs_table <- table(greinke$balls, greinke$strikes)

# Create bs_prop_table
bs_prop_table <- round(prop.table(bs_table), 3)

# Print bs_prop_table
bs_prop_table

# Print row sums
rowSums(bs_prop_table)

# Print column sums
colSums(bs_prop_table)

# Create bs_count
greinke <- greinke %>% mutate(bs_count = paste(balls, strikes, sep = "-"))

# Print the first 6 rows of greinke
head(greinke)

# Create bs_count_tab
bs_count_tab <- table(greinke$bs_count, greinke$july)

# Create bs_month
bs_month <- round(prop.table(bs_count_tab, 2), 3)

# Print bs_month
bs_month

# Create diff_bs
diff_bs <- round(((bs_month[, 1] - bs_month[, 2])/bs_month[, 2]), 3)

# Print diff_bs
diff_bs

# Create a bar plot of the changes
barplot(diff_bs, main = "Ball-Strike Count Rate in July vs. Other Months", 
        ylab = "Percentage Change in July", ylim = c(-0.15, 0.15), las = 2)

# Create type_bs
type_bs <- table(greinke$pitch_type, greinke$bs_count)

# Print type_bs
type_bs

# Create type_bs_prop
type_bs_prop <- round(prop.table(type_bs, 2), 3)

# Print type_bs_prop
type_bs_prop

# Create the late_in_game column as factor
greinke <- greinke %>% mutate(late_in_game = factor(ifelse(greinke$inning > 5, 1, 0)))

# Create type_late
type_late <- table(greinke$pitch_type, greinke$late_in_game)

# Create type_late_prop
type_late_prop <- round(prop.table(type_late, 2), 3)

# Print type_late_prop
type_late_prop

# Converting type_late to dataframe
type_late_df <- as.data.frame(type_late) %>% 
  mutate(Var2 = recode(Var2, "0" = "Early", "1" = "Late"))

# Changing names 
names(type_late_df) <- c("Pitch", "Occ", "Freq")

# Make barplot using type_late_df
ggplot(type_late_df, aes(Pitch, Freq, fill = Occ)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Early vs. Late In Game Pitch Selection") + 
  ylab("Pitch Selection Proportion")

# Calculate average pitch height in inches in July vs. other months
greinke %>% group_by(july) %>% summarise(mean(pz) * 12)
  
# Create greinke_lhb
greinke_lhb <- greinke %>% filter(batter_stand == "L")

# Create greinke_rhb
greinke_rhb <- greinke %>% filter(batter_stand == "R")

# Compute average px location for LHB
greinke_lhb %>% group_by(july) %>% summarise(mean(px) * 12)

# Compute average px location for RHB
greinke_rhb %>% group_by(july) %>% summarise(mean(px) * 12)

# Plot location of all pitches
ggplot(greinke, aes(px, pz, color = factor(july))) + geom_point(alpha = 0.5)

#Plot the pitch loctions for July and other months separately
ggplot(greinke, aes(px, pz, color = factor(july))) + geom_point() + facet_wrap(~ july)

# Create greinke_sub
greinke_sub <- greinke %>% filter(px > -2 & px < 2 & pz > 0 & pz < 5)

# Create vector posx
posx <- rep(seq(from = -1.5, to = 1.5, by = 1), times = 5)

# Create vector posz
posz <- rep(seq(from = 4.5, to = 0.5, by = -1), each = 4)

# Create vector of zone numbers
zone_1 <- seq(from = 1, to = 20)

# Create locgrid
locgrid <- data.frame(zone_1, posx, posz)

# Print locgrid
locgrid

#plot locgrid
plot_base_grid <- ggplot(locgrid, aes(posx, posz))

#read zone into R
zone <- read_excel("~/GitHub/greinke/zone.xlsx")

# add zone to greinke_sub
greinke_sub <- cbind(greinke_sub, zone)

# Create greinke_table
greinke_table <- table(greinke_sub$zone)

# Create zone_prop
zone_prop <- round(prop.table(greinke_table), 3)

# plotting locational grid proportions
plot_base_grid + annotate("text", x = locgrid$posx, y = locgrid$posz,
                          label = zone_prop, size = 5) + xlim(-2,2) + ylim(0, 5) + 
  geom_vline(xintercept = seq(from = -2, to = 2, by = 1)) + 
               geom_hline(yintercept = seq(from = 0, to = 5, by = 1)) + 
  ggtitle("Greinke Locational Zone Proportions") + 
  labs(x = "Horizontal Location (ft.; Catcher's View)", 
       y = ("Vertical Location (ft.)"))


# Calculating Binned locational differences
zone_prop_byjuly <- greinke_sub %>% group_by(july, zone) %>% summarise(n()) %>% 
  ungroup() %>% spread(july, `n()`)

# calculating proportion  
zone_prop_byjuly$july <- round(zone_prop_byjuly$july/sum(zone_prop_byjuly$july, na.rm = TRUE), 3)
zone_prop_byjuly$other <- round(zone_prop_byjuly$other/sum(zone_prop_byjuly$other, na.rm = TRUE), 3)

# assign NA to 0
zone_prop_byjuly$july[4] <- 0.00
zone_prop_byjuly <- zone_prop_byjuly %>% mutate(differ = july - other)

#print zone_prop_byjuly
zone_prop_byjuly

#combining locgrid and zone_prop_byjuly
locgrid <- cbind(locgrid, zone_prop_byjuly[, -1])

#Plotting zone proportion differences
plot_base_grid + annotate("text", x = locgrid$posx, y = locgrid$posz,
                          label = locgrid$differ, size = 5) + xlim(-2,2) + ylim(0, 5) + 
  geom_vline(xintercept = seq(from = -2, to = 2, by = 1)) + 
  geom_hline(yintercept = seq(from = 0, to = 5, by = 1)) + 
  ggtitle("Greinke Locational Zone Proportions") + 
  labs(x = "Horizontal Location (ft.; Catcher's View)", 
       y = ("Vertical Location (ft.)"))

# Create greinke_zone_tab
greinke_zone_tab <- table(greinke_sub$zone, greinke_sub$bs_count)

# Create zone_count_prop
zone_count_prop <- round(prop.table(greinke_zone_tab, 2), 3)

# Print zone_count_prop
zone_count_prop

#0-2 vs. 3-0 locational changes
# Create zone_count_diff
zone_count_diff <- round(zone_count_prop[, 3] - zone_count_prop[, 10], 3)

# Print the table
zone_count_diff

#Plotting count-based locational differences
plot_base_grid + annotate("text", x = locgrid$posx, y = locgrid$posz,
                          label = zone_count_diff, size = 5) + xlim(-2,2) + ylim(0, 5) + 
  geom_vline(xintercept = seq(from = -2, to = 2, by = 1)) + 
  geom_hline(yintercept = seq(from = 0, to = 5, by = 1)) + 
  ggtitle("Greinke Locational Zone (0-2 vs. 3-0 Counts)") + 
  labs(x = "Horizontal Location (ft.; Catcher's View)", 
       y = ("Vertical Location (ft.)"))

#Velocity impact on contact rate
# create greinke_ff
greinke_ff <- greinke %>% filter(pitch_type == "FF")

# Create batter_swing
no_swing <- c("Ball", "Called Strike", "Ball in Dirt", "Hit By Pitch")
greinke_ff <- greinke_ff %>% 
  mutate(batter_swing = ifelse(pitch_result %in% no_swing, 0, 1))

# Create swing_ff
swing_ff <- greinke_ff %>% filter(batter_swing == 1)

# Create the contact variable
no_contact <- c("Swinging Strike", "Missed Bunt")
swing_ff <- swing_ff %>% mutate(contact = ifelse(pitch_result %in% no_contact, 0, 1))

#Create velo_bin
swing_ff$velo_bin <- ifelse(swing_ff$start_speed < 90.5, "Slow", NA)

swing_ff$velo_bin <- ifelse(swing_ff$start_speed >= 90.5 & swing_ff$start_speed < 92.5, 
                            "Medium", swing_ff$velo_bin)

swing_ff$velo_bin <- ifelse(swing_ff$start_speed >= 92.5, 
                            "Fast", swing_ff$velo_bin)

# Aggregate contact rate by velocity bin
swing_ff %>% group_by(velo_bin) %>% summarise(mean(contact))

#Pitch type impact on contact rate
#create swings data set with batter_swing = 1 and contact variable
swings <- greinke_sub %>% 
  mutate(batter_swing = ifelse(pitch_result %in% no_swing, 0, 1)) %>% 
  filter(batter_swing == 1) %>% 
  mutate(contact = ifelse(pitch_result %in% no_contact, 0, 1))

# Create the bin_pitch_speed function
bin_pitch_speed <- function(start_speed){
  FQ <- quantile(start_speed, 0.15)
  TQ <- quantile(start_speed, 0.65)

velo_bin <- ifelse(start_speed < FQ, "slow", NA)
velo_bin <- ifelse(start_speed >= FQ & start_speed < TQ, "medium", velo_bin)
velo_bin <- ifelse(start_speed >= TQ, "fast", velo_bin)

return(velo_bin)
}

# Create the subsets for each pitch type
swing_ff <- swings %>% filter(pitch_type == "FF")
swing_ch <- swings %>% filter(pitch_type == "CH")
swing_cu <- swings %>% filter(pitch_type == "CU")
swing_ft <- swings %>% filter(pitch_type == "FT")
swing_sl <- swings %>% filter(pitch_type == "SL")

# Make velo_bin_pitch variable for each subset
swing_ff$velo_bin <- bin_pitch_speed(swing_ff$start_speed)
swing_ch$velo_bin <- bin_pitch_speed(swing_ch$start_speed)
swing_cu$velo_bin <- bin_pitch_speed(swing_cu$start_speed)
swing_ft$velo_bin <- bin_pitch_speed(swing_ft$start_speed)
swing_sl$velo_bin <- bin_pitch_speed(swing_sl$start_speed)

# Print quantile levels for each pitch
thirds <- c(0, 1/3, 2/3, 1)
quantile(swing_ff$start_speed, probs = thirds)
quantile(swing_ch$start_speed, probs = thirds)
quantile(swing_cu$start_speed, probs = thirds)
quantile(swing_ft$start_speed, probs = thirds)
quantile(swing_sl$start_speed, probs = thirds)

#Velocity impact on contact by pitch type
# Calculate contact rate by velocity for swing_ff
swing_ff %>% group_by(velo_bin) %>% summarise(mean(contact))

# Calculate contact rate by velocity for swing_ft
swing_ft %>% group_by(velo_bin) %>% summarise(mean(contact))

# Calculate contact rate by velocity for swing_ch
swing_ch %>% group_by(velo_bin) %>% summarise(mean(contact))

# Calculate contact rate by velocity for swing_cu
swing_cu %>% group_by(velo_bin) %>% summarise(mean(contact))

# Calculate contact rate by velocity for swing_sl
swing_sl %>% group_by(velo_bin) %>% summarise(mean(contact))


#Greinke's out pitch?
# Create swings_str2
swings_str2 <- swings %>% filter(strikes == 2)

# Print number of observations
nrow(swings_str2)

# Print a table of pitch use
table(swings_str2$pitch_type)

# Calculate contact rate by pitch type
swings_str2 %>% group_by(pitch_type) %>% summarise(round(mean(contact),3))

#Impact of pitch location on contact rate

#Create zone_contact_r
zone_contact_r <- swings %>% filter(batter_stand == "R") %>% 
  group_by(zone) %>% summarise(round(mean(contact), 3))

#Create zone_contact_l
zone_contact_l <- swings %>% filter(batter_stand == "L") %>% 
  group_by(zone) %>% summarise(round(mean(contact), 3))

zone_contact_l <- data.frame(zone = 1:20, contat_avg = unlist(c(0, zone_contact_l[1, 2], 0, 0, zone_contact_l[2:16, 2], 0)))
zone_contact_r <- data.frame(zone = 1:20, contat_avg = unlist(c(zone_contact_r[1, 2], 0, 
                                                                zone_contact_r[2, 2], 0, 
                                                                zone_contact_r[3:14, 2], 0, zone_contact_r[15:17, 2])))

#Plotting count-based locational differences
#for RHB
plot_base_grid + annotate("text", x = locgrid$posx, y = locgrid$posz,
                          label = zone_contact_r$contat_avg, size = 5) + xlim(-2,2) + ylim(0, 5) + 
  geom_vline(xintercept = seq(from = -2, to = 2, by = 1)) + 
  geom_hline(yintercept = seq(from = 0, to = 5, by = 1)) + 
  ggtitle("Greinke Locational Zone (0-2 vs. 3-0 Counts)") + 
  labs(x = "Horizontal Location (ft.; Catcher's View)", 
       y = ("Vertical Location (ft.)"))

#for LHB
plot_base_grid + annotate("text", x = locgrid$posx, y = locgrid$posz,
                          label = zone_contact_l$contat_avg, size = 5) + xlim(-2,2) + ylim(0, 5) + 
  geom_vline(xintercept = seq(from = -2, to = 2, by = 1)) + 
  geom_hline(yintercept = seq(from = 0, to = 5, by = 1)) + 
  ggtitle("Greinke Locational Zone (0-2 vs. 3-0 Counts)") + 
  labs(x = "Horizontal Location (ft.; Catcher's View)", 
       y = ("Vertical Location (ft.)"))

# Merge locgrid with zone_contact_r and zone_contact_l
locgrid <- cbind(locgrid, zone_contact_r$contat_avg, zone_contact_l$contat_avg)
names(locgrid) <- c("zone_1", "posx", "posz", "zone_prop", "july", "other", "differ", 
                    "contact_rate_r", "contact_rate_l")


# Plot base grid
plot_base_grid <- ggplot(locgrid, aes(posx, posz))

#Plot RHB plot 
plot_titles_rhb <- plot_base_grid +
  ggtitle("RHB Contact Rates") + 
  labs(x = "Horizontal Location(ft.; Catcher's View)", 
       y = "Vertical Location (ft.)") + 
  theme(plot.title = element_text(size = 15))

plot_colors_rhb <- plot_titles_rhb + 
  geom_tile(aes(fill = contact_rate_r)) + 
  scale_fill_gradientn(name = "Contact Rate", 
                       limits = c(0.5, 1), 
                       breaks = seq(from = 0.5, to = 1, by = 0.1), 
                       colors = c(brewer.pal(n = 7, name = "Reds")))

plot_contact_rhb <- plot_colors_rhb + 
  annotate("text", x = locgrid$posx, y = locgrid$posz, 
           label = locgrid$contact_rate_r, size = 5)

# LHB plot
plot_titles_lhb <- plot_base_grid +
  ggtitle("LHB Contact Rates") + 
  labs(x = "Horizontal Location(ft.; Catcher's View)", 
       y = "Vertical Location (ft.)") + 
  theme(plot.title = element_text(size = 15))

plot_colors_lhb <- plot_titles_lhb + 
  geom_tile(aes(fill = contact_rate_l)) + 
  scale_fill_gradientn(name = "Contact Rate", 
                       limits = c(0.5, 1), 
                       breaks = seq(from = 0.5, to = 1, by = 0.1), 
                       colors = c(brewer.pal(n = 7, name = "Reds")))

plot_contact_lhb <- plot_colors_lhb + 
  annotate("text", x = locgrid$posx, y = locgrid$posz, 
           label = locgrid$contact_rate_l, size = 5)

# Plot them side-by-side
grid.arrange(plot_contact_rhb, plot_contact_lhb, ncol = 2)

# Contact and exit speed
pcontact <- swings %>% filter(!is.na(batted_ball_velocity)) %>% filter(contact == 1)

# Create pcontact_r
pcontact_r <- pcontact %>% filter(batter_stand == "R")

# Create pcontact_l
pcontact_l <- pcontact %>% filter(batter_stand == "L")

#Location and exit speed

# Create exit_speed_r
exit_speed_r <- pcontact_r %>% group_by(zone) %>% 
  summarise(round(mean(batted_ball_velocity), 1))
colnames(exit_speed_r) <- c("zone_1", "exit_speed_rhb")

# Create exit_speed_l
exit_speed_l <- pcontact_l %>% group_by(zone) %>% 
  summarise(round(mean(batted_ball_velocity), 1))
colnames(exit_speed_l) <- c("zone_1", "exit_speed_lhb")

# Merge with locgrid
locgrid <- left_join(locgrid, exit_speed_r, by = "zone_1")
locgrid <- left_join(locgrid, exit_speed_l, by = "zone_1")

# Plotting exit speed as a heat map

plot_exit_rhb <- plot_base_grid + 
  geom_tile(data = locgrid, aes(fill = exit_speed_rhb)) + 
  scale_fill_gradientn(name = "Exit Speed (mph)", 
                       limits = c(60, 95), 
                       breaks = seq(from = 60, to = 95, by = 5), 
                       colors = c(brewer.pal(n = 7, name = "Reds"))) + 
  annotate("text", x = locgrid$px, y = locgrid$pz, 
           label = locgrid$exit_speed_rhb, size = 5) + 
  ggtitle("RHB Exit Velocity (mph)") + 
  labs(x = "Horizontal Location(ft.; Catcher's View)", 
       y = "Vertical Location (ft.)") + 
  theme(plot.title = element_text(size = 15))

# Create LHB exit speed plotting object
plot_exit_lhb <- plot_base_grid + 
  geom_tile(data = locgrid, aes(fill = exit_speed_lhb)) + 
  scale_fill_gradientn(name = "Exit Speed (mph)", 
                       limits = c(60, 95), 
                       breaks = seq(from = 60, to = 95, by = 5), 
                       colors = c(brewer.pal(n = 7, name = "Reds"))) + 
  annotate("text", x = locgrid$px, y = locgrid$pz, 
           label = locgrid$exit_speed_rhb, size = 5) + 
  ggtitle("LHB Exit Velocity (mph)") + 
  labs(x = "Horizontal Location(ft.; Catcher's View)", 
       y = "Vertical Location (ft.)") + 
  theme(plot.title = element_text(size = 15))

# Plot each side-by-side
grid.arrange(plot_exit_rhb, plot_exit_lhb, ncol = 2)

exit <- locgrid[, c(1:3, 10:11)]
names(exit) <- c("zone", "posx", "posz", "RHB", "LHB")

exit_tidy <- gather(exit, batter_stand, exit_speed, -c(zone_1, posx, posz))
exit_tidy$batter_stand <- c(rep("RHB", 20), rep("LHB", 20))


# Create plot_exit
plot_exit <- plot_base_grid + 
  geom_tile(data = exit_tidy, aes(fill = exit_speed)) + 
  scale_fill_gradientn(name = "Exit Speed (mph)", 
                       colors = c(brewer.pal(n = 7, name = "Reds"))) + 
  ggtitle("Exit Speed (mph)") + 
  labs(x = "Horizontal Location(ft.; Catcher's View)", 
       y = "Vertical Location (ft.)") + 
  theme(plot.title = element_text(size = 15)) +
  facet_grid(. ~ batter_stand)

# Display plot_exit
plot_exit





























