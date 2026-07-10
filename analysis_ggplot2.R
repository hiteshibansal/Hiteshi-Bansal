

library(ggplot2)

# ---- Setup: reuse the same working directory as analysis.R ----
setwd("C:/Users/HITESHI BANSAL/Desktop/Nestle_India_Project")
dir.create("plots_ggplot", showWarnings = FALSE)

pnl <- read.csv("data/pnl.csv", stringsAsFactors = FALSE)
bs  <- read.csv("data/balance_sheet.csv", stringsAsFactors = FALSE)
cf  <- read.csv("data/cash_flow.csv", stringsAsFactors = FALSE)
rg  <- read.csv("data/ratios_given.csv", stringsAsFactors = FALSE)

pnl$Year_clean <- factor(gsub("_15M", " (15M)", pnl$Year), levels = gsub("_15M", " (15M)", pnl$Year))
rg$Year_clean  <- pnl$Year_clean
bs$Year_clean  <- pnl$Year_clean

# A consistent look-and-feel across all 4 charts
nestle_theme <- theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 15, color = "#2F5233"),
    plot.subtitle = element_text(size = 10, color = "#666666"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 10, color = "#333333"),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  )

green <- "#2F5233"; red <- "#B22222"; blue <- "#1F6FB2"; orange <- "#E58429"

## ---------------------------------------------------------------------
## 1. Revenue trend + linear forecast (clean 12-month years only)
## ---------------------------------------------------------------------
clean_years <- pnl[pnl$Year != "FY2024_15M", ]
t_clean <- 1:nrow(clean_years)
model_sales <- lm(clean_years$Sales ~ t_clean)
next_t <- (nrow(clean_years) + 1):(nrow(clean_years) + 2)
forecast_sales <- predict(model_sales, newdata = data.frame(t_clean = next_t))

plot_df <- data.frame(
  x = c(t_clean, next_t),
  label = c(as.character(clean_years$Year_clean), "FY25_fc", "FY26_fc"),
  sales = c(clean_years$Sales, forecast_sales),
  type = c(rep("Actual", length(t_clean)), rep("Forecast", 2))
)
trend_line <- data.frame(x = c(t_clean, next_t),
                          y = predict(model_sales, newdata = data.frame(t_clean = c(t_clean, next_t))))

p1 <- ggplot() +
  geom_line(data = trend_line, aes(x, y), color = orange, linetype = "dashed", linewidth = 0.8) +
  geom_point(data = plot_df, aes(x, sales, color = type, shape = type), size = 3) +
  geom_line(data = subset(plot_df, type == "Actual"), aes(x, sales), color = green, linewidth = 0.9) +
  scale_x_continuous(breaks = plot_df$x, labels = plot_df$label) +
  scale_color_manual(values = c("Actual" = green, "Forecast" = red)) +
  scale_shape_manual(values = c("Actual" = 16, "Forecast" = 17)) +
  labs(title = "Nestlé India: Sales Trend & Linear Forecast",
       subtitle = "Clean 12-month fiscal years only (FY2024 15-month stub excluded)",
       x = "Fiscal Year", y = "₹ Crores") +
  nestle_theme
ggsave("plots_ggplot/01_revenue_trend.png", p1, width = 8, height = 5.2, dpi = 150)

## ---------------------------------------------------------------------
## 2. Operating margin vs net profit margin
## ---------------------------------------------------------------------
margin_df <- data.frame(
  Year = rep(pnl$Year_clean, 2),
  Value = c(pnl$OPM_pct, pnl$Net_Profit / pnl$Sales * 100),
  Metric = rep(c("Operating Margin %", "Net Profit Margin %"), each = nrow(pnl))
)

p2 <- ggplot(margin_df, aes(Year, Value, color = Metric, group = Metric)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Operating Margin %" = green, "Net Profit Margin %" = red)) +
  labs(title = "Nestlé India: Operating Margin vs Net Profit Margin",
       subtitle = "FY2015-FY2026, standalone",
       x = "Fiscal Year", y = "%") +
  nestle_theme
ggsave("plots_ggplot/02_margins.png", p2, width = 8, height = 5.2, dpi = 150)

## ---------------------------------------------------------------------
## 3. ROCE vs Capex (dual axis)
## ---------------------------------------------------------------------
capex <- bs$Fixed_Assets + bs$CWIP
scale_factor <- max(rg$ROCE_pct) / max(capex)

p3 <- ggplot(data.frame(Year = pnl$Year_clean, ROCE = rg$ROCE_pct, Capex = capex), aes(x = Year)) +
  geom_line(aes(y = ROCE, group = 1), color = green, linewidth = 0.9) +
  geom_point(aes(y = ROCE), color = green, size = 2.5) +
  geom_line(aes(y = Capex * scale_factor, group = 1), color = blue, linewidth = 0.9, linetype = "dashed") +
  geom_point(aes(y = Capex * scale_factor), color = blue, size = 2.5, shape = 17) +
  scale_y_continuous(
    name = "ROCE %",
    sec.axis = sec_axis(~ . / scale_factor, name = "Capex (₹ Cr)")
  ) +
  labs(title = "Nestlé India: ROCE vs Capex (Fixed Assets + CWIP)",
       subtitle = "Weak correlation full-period (r=0.14) vs strong negative FY23-FY26 (r=-0.80)",
       x = "Fiscal Year") +
  nestle_theme +
  theme(axis.title.y.right = element_text(color = blue), axis.title.y.left = element_text(color = green))
ggsave("plots_ggplot/03_roce_vs_capex.png", p3, width = 8, height = 5.2, dpi = 150)

## ---------------------------------------------------------------------
## 4. Cash Conversion Cycle
## ---------------------------------------------------------------------
ccc_df <- data.frame(Year = pnl$Year_clean, CCC = rg$Cash_Conversion_Cycle)
ccc_df$Positive <- ccc_df$CCC >= 0

p4 <- ggplot(ccc_df, aes(Year, CCC, fill = Positive)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = c("TRUE" = red, "FALSE" = green), guide = "none") +
  labs(title = "Nestlé India: Cash Conversion Cycle",
       subtitle = "Negative = supplier-funded working capital (favourable)",
       x = "Fiscal Year", y = "Days") +
  nestle_theme
ggsave("plots_ggplot/04_cash_conversion_cycle.png", p4, width = 8, height = 5.2, dpi = 150)


