# ============================================================
#  Marketing Analytics Portfolio - Part A
#  Student ID : 40478325
#  Seed       : 25  (last two digits of student ID)
#  Name       : Kshitiz Rawat
#  Topic      : Market Segmentation & Targeting
#  Retailer   : UK-based online gift store (Jan-Aug 2024)
# ============================================================

# ------------------------------------------------------------
#  0. PACKAGES  (uncomment install line on first run only)
# ------------------------------------------------------------
# install.packages(c("readxl","dplyr","tidyr","lubridate","ggplot2",
#                    "scales","cluster","factoextra","MASS","gridExtra",
#                    "RColorBrewer","reshape2"))

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(cluster)
library(factoextra)
library(MASS)
library(gridExtra)
library(RColorBrewer)
library(reshape2)

set.seed(25)

# ------------------------------------------------------------
#  FILE PATHS
# ------------------------------------------------------------
input_current   <- "C:/Users/Kshitiz/Desktop/Current Customers.xlsx"
input_prospects <- "C:/Users/Kshitiz/Desktop/Prospect Customers.xlsx"
output_dir      <- "C:/Users/Kshitiz/Desktop/Marketing Analytics Assignment/"

# ------------------------------------------------------------
#  1. LOAD DATA
# ------------------------------------------------------------
sales_raw     <- read_excel(input_current)
prospects_raw <- read_excel(input_prospects)

cat("Current customers :", nrow(sales_raw),    "rows,", ncol(sales_raw),    "cols\n")
cat("Prospects         :", nrow(prospects_raw), "rows,", ncol(prospects_raw), "cols\n")

# ============================================================
#  STEP 1 - EXPLORATORY DATA ANALYSIS
# ============================================================

names(sales_raw)     <- gsub(" ", "_", tolower(trimws(names(sales_raw))))
names(prospects_raw) <- gsub(" ", "_", tolower(trimws(names(prospects_raw))))

date_col  <- ifelse("invoicedate" %in% names(sales_raw), "invoicedate", "invoice_date")
qty_col   <- ifelse("quantity"    %in% names(sales_raw), "quantity",    "qty")
price_col <- ifelse("unitprice"   %in% names(sales_raw), "unitprice",   "unit_price")
cid_col   <- ifelse("customerid"  %in% names(sales_raw), "customerid",  "customer_id")
inv_col   <- ifelse("invoiceno"   %in% names(sales_raw), "invoiceno",   "invoice_no")

cat("Columns - Date:", date_col, "| Qty:", qty_col, "| Price:", price_col,
    "| CustID:", cid_col, "| Invoice:", inv_col, "\n")

sales_raw[[date_col]] <- as.Date(sales_raw[[date_col]])

n0        <- nrow(sales_raw)
sales_raw <- sales_raw[!is.na(sales_raw[[cid_col]]), ]
sales_raw <- sales_raw[sales_raw[[qty_col]]   > 0,   ]
sales_raw <- sales_raw[sales_raw[[price_col]] > 0,   ]
cat("Rows removed:", n0 - nrow(sales_raw), "| Clean rows:", nrow(sales_raw), "\n")

sales_raw$line_revenue <- sales_raw[[qty_col]] * sales_raw[[price_col]]
sales_raw$ym           <- floor_date(sales_raw[[date_col]], "month")
sales_raw$month_label  <- factor(
  format(sales_raw[[date_col]], "%b %Y"),
  levels = format(sort(unique(sales_raw$ym)), "%b %Y")
)

cat("\n--- Dataset summary ---\n")
overall <- sales_raw %>%
  dplyr::summarise(
    rows             = dplyr::n(),
    unique_customers = dplyr::n_distinct(.data[[cid_col]]),
    revenue_total    = sum(line_revenue),
    avg_line         = mean(line_revenue),
    first_date       = min(.data[[date_col]]),
    last_date        = max(.data[[date_col]])
  )
print(t(overall))

my_theme <- theme_minimal(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(colour = "grey50"))

# ============================================================
#  PLOTS 1-7 : EDA
# ============================================================

p1 <- ggplot(sales_raw, aes(x = line_revenue)) +
  geom_histogram(bins = 60, fill = "#4361EE", colour = "white", alpha = 0.85) +
  scale_x_log10(labels = label_dollar(prefix = "\u00a3")) +
  labs(title = "Plot 1 - Distribution of Line-Item Revenue",
       subtitle = "Log-10 scale", x = "Line Revenue (log scale)", y = "Count") +
  my_theme
print(p1)

monthly <- sales_raw %>%
  dplyr::group_by(ym) %>%
  dplyr::summarise(revenue = sum(line_revenue), .groups = "drop")

p2 <- ggplot(monthly, aes(x = ym, y = revenue)) +
  geom_area(fill = "#4CC9F0", alpha = 0.35) +
  geom_line(colour = "#3A0CA3", linewidth = 1.2) +
  geom_point(colour = "#3A0CA3", size = 3) +
  scale_y_continuous(labels = label_dollar(prefix = "\u00a3", big.mark = ",")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Plot 2 - Monthly Revenue Trend (Jan-Aug 2024)", x = NULL, y = "Revenue") +
  my_theme + theme(axis.text.x = element_text(angle = 35, hjust = 1))
print(p2)

monthly_orders <- sales_raw %>%
  dplyr::group_by(ym) %>%
  dplyr::summarise(orders = dplyr::n_distinct(.data[[inv_col]]), .groups = "drop")

p3 <- ggplot(monthly_orders, aes(x = ym, y = orders)) +
  geom_col(fill = "#F72585", alpha = 0.85) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  labs(title = "Plot 3 - Monthly Number of Orders", x = NULL, y = "Unique Invoices") +
  my_theme + theme(axis.text.x = element_text(angle = 35, hjust = 1))
print(p3)

p4 <- ggplot(sales_raw, aes(x = month_label, y = .data[[price_col]])) +
  geom_boxplot(fill = "#7209B7", alpha = 0.6, outlier.size = 0.5) +
  scale_y_log10(labels = label_dollar(prefix = "\u00a3")) +
  labs(title = "Plot 4 - Unit Price Distribution by Month", subtitle = "Log scale",
       x = NULL, y = "Unit Price (log scale)") +
  my_theme + theme(axis.text.x = element_text(angle = 35, hjust = 1))
print(p4)

if ("description" %in% names(sales_raw)) {
  top_prod <- sales_raw %>%
    dplyr::group_by(description) %>%
    dplyr::summarise(rev = sum(line_revenue), .groups = "drop") %>%
    dplyr::slice_max(rev, n = 15)
  p5 <- ggplot(top_prod, aes(x = reorder(description, rev), y = rev)) +
    geom_col(fill = "#FB8500", alpha = 0.9) +
    scale_y_continuous(labels = label_dollar(prefix = "\u00a3", big.mark = ",")) +
    coord_flip() +
    labs(title = "Plot 5 - Top 15 Products by Revenue", x = NULL, y = "Total Revenue") +
    my_theme + theme(axis.text.y = element_text(size = 7))
  print(p5)
}

p6 <- ggplot(sales_raw, aes(x = .data[[qty_col]])) +
  geom_density(fill = "#06D6A0", colour = "#023E8A", alpha = 0.7, linewidth = 1) +
  scale_x_log10() +
  labs(title = "Plot 6 - Density of Quantity per Transaction", subtitle = "Log scale",
       x = "Quantity (log scale)", y = "Density") +
  my_theme
print(p6)

cust_spend <- sales_raw %>%
  dplyr::group_by(.data[[cid_col]]) %>%
  dplyr::summarise(total = sum(line_revenue), .groups = "drop")

p7 <- ggplot(cust_spend, aes(x = total)) +
  geom_histogram(bins = 50, fill = "#480CA8", colour = "white", alpha = 0.85) +
  scale_x_log10(labels = label_dollar(prefix = "\u00a3", big.mark = ",")) +
  labs(title = "Plot 7 - Total Spend per Customer", subtitle = "Log scale",
       x = "Total Spend (log scale)", y = "Number of Customers") +
  my_theme
print(p7)

# ============================================================
#  STEP 2 - RFM ANALYSIS
# ============================================================
snapshot_date <- max(sales_raw[[date_col]], na.rm = TRUE) + 1
cat("\nSnapshot date:", as.character(snapshot_date), "\n")

rfm <- sales_raw %>%
  dplyr::group_by(.data[[cid_col]]) %>%
  dplyr::summarise(
    recency   = as.numeric(snapshot_date - max(.data[[date_col]])),
    frequency = dplyr::n_distinct(.data[[inv_col]]),
    monetary  = sum(line_revenue),
    .groups   = "drop"
  )
names(rfm)[1] <- "cust_id"

cat("Customers in RFM table:", nrow(rfm), "\n")
print(summary(rfm[, c("recency", "frequency", "monetary")]))

rfm$r_score <- ntile(-rfm$recency,   5)
rfm$f_score <- ntile( rfm$frequency, 5)
rfm$m_score <- ntile( rfm$monetary,  5)
rfm$rfm_sum <- rfm$r_score + rfm$f_score + rfm$m_score

rfm$r_seq <- ntile(-rfm$recency, 5)
rfm <- rfm %>%
  dplyr::group_by(r_seq) %>%
  dplyr::mutate(f_seq = ntile(frequency, 5)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(r_seq, f_seq) %>%
  dplyr::mutate(m_seq = ntile(monetary, 5)) %>%
  dplyr::ungroup()
rfm$rfm_sum_seq <- rfm$r_seq + rfm$f_seq + rfm$m_seq

rfm <- rfm %>%
  dplyr::mutate(segment = dplyr::case_when(
    rfm_sum_seq >= 13                             ~ "Champions",
    rfm_sum_seq >= 10 & r_seq >= 4               ~ "Loyal Customers",
    rfm_sum_seq >= 10 & r_seq <  4               ~ "Potential Loyalists",
    r_seq >= 4 & rfm_sum_seq < 10                ~ "New / Recent",
    f_seq >= 4 & r_seq <  3                      ~ "At Risk",
    r_seq <= 2 & f_seq <= 2 & m_seq >= 4         ~ "Cannot Lose Them",
    r_seq <= 2 & rfm_sum_seq < 8                 ~ "Lost",
    TRUE                                          ~ "Needs Attention"
  ))

cat("\nSegment distribution:\n")
print(table(rfm$segment))

# ============================================================
#  PLOTS 8-13 : RFM
# ============================================================
seg_colours <- c(
  "Champions"           = "#2DC653", "Loyal Customers"     = "#118AB2",
  "Potential Loyalists" = "#06D6A0", "New / Recent"        = "#FFD166",
  "Needs Attention"     = "#F4A261", "At Risk"             = "#EF476F",
  "Cannot Lose Them"    = "#9B2226", "Lost"                = "#6C757D"
)

seg_count <- rfm %>% dplyr::count(segment)

p8 <- ggplot(seg_count, aes(x = reorder(segment, n), y = n, fill = segment)) +
  geom_col(show.legend = FALSE, alpha = 0.9) +
  scale_fill_manual(values = seg_colours) + coord_flip() +
  labs(title = "Plot 8 - Customer Count per RFM Segment", x = NULL, y = "Customers") +
  my_theme
print(p8)

p9 <- ggplot(seg_count, aes(x = 2, y = n, fill = segment)) +
  geom_col(width = 1, colour = "white") +
  coord_polar(theta = "y") + xlim(0.5, 2.5) +
  scale_fill_manual(values = seg_colours) +
  labs(title = "Plot 9 - RFM Segment Share (Donut Chart)", fill = "Segment") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 13), legend.position = "right")
print(p9)

seg_money <- rfm %>%
  dplyr::group_by(segment) %>%
  dplyr::summarise(avg_spend = mean(monetary), .groups = "drop")

p10 <- ggplot(seg_money, aes(x = reorder(segment, avg_spend), y = avg_spend, fill = segment)) +
  geom_col(show.legend = FALSE, alpha = 0.9) +
  scale_fill_manual(values = seg_colours) +
  scale_y_continuous(labels = label_dollar(prefix = "\u00a3", big.mark = ",")) +
  coord_flip() +
  labs(title = "Plot 10 - Average Total Spend by Segment", x = NULL, y = "Avg Spend") +
  my_theme
print(p10)

p11 <- ggplot(rfm, aes(x = recency, y = frequency, colour = segment)) +
  geom_point(alpha = 0.55, size = 1.6) +
  scale_colour_manual(values = seg_colours) + scale_x_log10() +
  labs(title = "Plot 11 - Recency vs Frequency by Segment", subtitle = "Log x-axis",
       x = "Days Since Last Purchase (log)", y = "Orders", colour = "Segment") +
  my_theme + theme(legend.position = "bottom", legend.text = element_text(size = 8))
print(p11)

rfm_heat <- rfm %>%
  dplyr::group_by(r_score, f_score) %>%
  dplyr::summarise(avg_m = mean(m_score), .groups = "drop")

p12 <- ggplot(rfm_heat, aes(x = factor(f_score), y = factor(r_score), fill = avg_m)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  scale_fill_distiller(palette = "RdYlGn", direction = 1) +
  labs(title = "Plot 12 - RFM Heatmap (R vs F, fill = avg M)",
       x = "Frequency Score", y = "Recency Score", fill = "Avg M") + my_theme
print(p12)

p13 <- ggplot(rfm, aes(x = rfm_sum)) +
  geom_histogram(binwidth = 1, fill = "#560BAD", colour = "white", alpha = 0.85) +
  scale_x_continuous(breaks = 3:15) +
  labs(title = "Plot 13 - Distribution of Total RFM Score",
       subtitle = "R + F + M (each 1-5); max = 15",
       x = "Total RFM Score", y = "Number of Customers") + my_theme
print(p13)

# ============================================================
#  STEP 3 - K-MEANS CLUSTERING
# ============================================================
rfm_scaled <- scale(rfm[, c("recency", "frequency", "monetary")])

wss <- sapply(1:10, function(k) {
  kmeans(rfm_scaled, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})

p14 <- ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) +
  geom_line(colour = "#F72585", linewidth = 1.2) +
  geom_point(colour = "#F72585", size = 3.5) +
  geom_vline(xintercept = 4, linetype = "dashed", colour = "grey40") +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Plot 14 - Elbow Method for Optimal k", subtitle = "Dashed line at k = 4",
       x = "k", y = "Total Within-Cluster SS") + my_theme
print(p14)

km4         <- kmeans(rfm_scaled, centers = 4, nstart = 50, iter.max = 200)
rfm$cluster <- factor(km4$cluster)
cat("\nCluster sizes:\n")
print(table(rfm$cluster))

p15 <- ggplot(rfm, aes(x = recency, y = monetary, colour = cluster)) +
  geom_point(alpha = 0.55, size = 1.6) +
  scale_colour_brewer(palette = "Set1") +
  scale_y_log10(labels = label_dollar(prefix = "\u00a3", big.mark = ",")) +
  scale_x_log10() +
  labs(title = "Plot 15 - K-Means Clusters: Recency vs Monetary",
       subtitle = "Both axes log-scaled",
       x = "Recency (days, log)", y = "Total Spend (log)", colour = "Cluster") + my_theme
print(p15)

cluster_profile <- rfm %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(Recency = mean(recency), Frequency = mean(frequency),
                   Monetary = mean(monetary), .groups = "drop") %>%
  dplyr::mutate(dplyr::across(Recency:Monetary, scale)) %>%
  tidyr::pivot_longer(-cluster, names_to = "metric", values_to = "z_score")

p16 <- ggplot(cluster_profile, aes(x = metric, y = z_score, fill = cluster)) +
  geom_col(position = "dodge", alpha = 0.88) +
  scale_fill_brewer(palette = "Set1") +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.5) +
  labs(title = "Plot 16 - Cluster Profiles (Standardised R, F, M)",
       subtitle = "Bars above 0 = above average", x = NULL, y = "Z-score", fill = "Cluster") +
  my_theme
print(p16)

sil <- silhouette(km4$cluster, dist(rfm_scaled))
p17 <- fviz_silhouette(sil, palette = "Set1", print.summary = FALSE) +
  labs(title = "Plot 17 - Silhouette Plot for k = 4") + my_theme
print(p17)

# ============================================================
#  STEP 4 - LDA CLASSIFICATION ON PROSPECTS
# ============================================================
demo_cols <- c("married", "household_size", "income", "age", "work", "education")

# One row per customer demographics from sales data
sales_demo <- sales_raw %>%
  dplyr::group_by(.data[[cid_col]]) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

names(sales_demo)[names(sales_demo) == cid_col] <- "cust_id"
sales_demo$cust_id <- as.character(sales_demo$cust_id)
sales_demo         <- sales_demo[, c("cust_id", intersect(demo_cols, names(sales_demo)))]
names(sales_demo)  <- tolower(names(sales_demo))

# Use base merge to avoid MASS masking dplyr::select
rfm$cust_id <- as.character(rfm$cust_id)
train_df    <- merge(rfm[, c("cust_id", "cluster")], sales_demo, by = "cust_id", all.x = FALSE)
train_df    <- train_df[complete.cases(train_df), ]

cat("\nTraining set:", nrow(train_df), "rows x", ncol(train_df), "cols\n")

available_demo <- intersect(demo_cols, names(train_df))
lda_formula    <- as.formula(paste("cluster ~", paste(available_demo, collapse = " + ")))
lda_model      <- lda(lda_formula, data = train_df)

cat("\nLDA prior probabilities:\n")
print(lda_model$prior)

train_pred <- predict(lda_model, train_df)
conf_mat   <- table(Predicted = train_pred$class, Actual = train_df$cluster)
cat("\nConfusion matrix (training):\n")
print(conf_mat)
cat("Training accuracy:", round(sum(diag(conf_mat)) / sum(conf_mat) * 100, 2), "%\n")

lda_scores <- data.frame(LD1 = train_pred$x[, 1], LD2 = train_pred$x[, 2],
                          cluster = train_df$cluster)

p18 <- ggplot(lda_scores, aes(x = LD1, y = LD2, colour = cluster)) +
  geom_point(alpha = 0.5, size = 1.5) + stat_ellipse(linewidth = 0.9) +
  scale_colour_brewer(palette = "Set1") +
  labs(title = "Plot 18 - LDA Discriminant Space (LD1 vs LD2)",
       subtitle = "Ellipses = 95% confidence region", colour = "Cluster") + my_theme
print(p18)

names(prospects_raw)  <- tolower(names(prospects_raw))
prospect_demo         <- prospects_raw[, intersect(available_demo, names(prospects_raw)), drop = FALSE]
prospect_pred         <- predict(lda_model, prospect_demo)
prospects_raw$cluster <- prospect_pred$class

cat("\nProspect cluster distribution:\n")
print(table(prospects_raw$cluster))

prospect_count        <- as.data.frame(table(Cluster = prospects_raw$cluster))
names(prospect_count) <- c("cluster", "n")

p19 <- ggplot(prospect_count, aes(x = cluster, y = n, fill = cluster)) +
  geom_col(show.legend = FALSE, alpha = 0.9) +
  scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label = n), vjust = -0.4, fontface = "bold") +
  labs(title = "Plot 19 - Predicted Cluster Membership for Prospects",
       subtitle = "LDA applied to prospective customers", x = "Cluster", y = "Prospects") +
  my_theme
print(p19)

curr_avg <- data.frame(Age = mean(sales_raw$age, na.rm = TRUE),
                        Income = mean(sales_raw$income, na.rm = TRUE),
                        Household_Size = mean(sales_raw$household_size, na.rm = TRUE),
                        Group = "Current Customers")
pros_avg <- data.frame(Age = mean(prospects_raw$age, na.rm = TRUE),
                        Income = mean(prospects_raw$income, na.rm = TRUE),
                        Household_Size = mean(prospects_raw$household_size, na.rm = TRUE),
                        Group = "Prospects")

demo_compare <- tidyr::pivot_longer(rbind(curr_avg, pros_avg),
                                     cols = -Group, names_to = "metric", values_to = "value")

p20 <- ggplot(demo_compare, aes(x = metric, y = value, fill = Group)) +
  geom_col(position = "dodge", alpha = 0.88) +
  scale_fill_manual(values = c("Current Customers" = "#4361EE", "Prospects" = "#F72585")) +
  labs(title = "Plot 20 - Demographic Comparison: Current vs Prospects",
       subtitle = "Average Age, Income and Household Size",
       x = NULL, y = "Average Value", fill = NULL) + my_theme
print(p20)

# ============================================================
#  SAVE ALL 20 PLOTS
# ============================================================
plot_list  <- list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20)
plot_names <- paste0("Plot_", 1:20, ".png")

for (i in seq_along(plot_list)) {
  tryCatch({
    ggsave(paste0(output_dir, plot_names[i]), plot = plot_list[[i]],
           width = 9, height = 6, dpi = 150)
    cat("Saved:", plot_names[i], "\n")
  }, error = function(e) cat("Could not save", plot_names[i], ":", e$message, "\n"))
}

cat("\n============================================================\n")
cat("  Part A complete - 20 plots generated & saved\n")
cat("  Student : Kshitiz Rawat  |  ID : 40478325  |  Seed : 25\n")
cat("  Output  :", output_dir, "\n")
cat("============================================================\n")