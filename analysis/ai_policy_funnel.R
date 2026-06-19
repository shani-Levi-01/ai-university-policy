# =============================================================
#  AI Policy in Universities — השוואת כמויות בין שלבי המדיניות
#  ממחיש: דרישת גילוי -> אכיפה -> היתר שימוש כללי -> פרסום לציבור בקורסי ליבה
# =============================================================

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# --- 1. קריאת הנתונים ---
csv_path <- "data/universities_ai_policy.csv"
df <- read.csv(csv_path, stringsAsFactors = FALSE, encoding = "UTF-8", fill = TRUE)

# --- 2. ניקוי ---
df <- df[-1, ]                                                   # שורת התיאור
df <- df[!is.na(df$university) & trimws(df$university) != "", ]  # שורות ריקות
df <- df[!grepl("unknown", df$university, ignore.case = TRUE), ] # שורת האגדה

clean <- function(x) trimws(tolower(gsub("unknowen", "unknown", x)))
cols <- c("requires_ai_disclosure", "enforce_penalties",
          "genai_allowed_general_use", "cs_ai_policy_specified",
          "ds_ai_policy_specifies")
df[cols] <- lapply(df[cols], clean)

n <- nrow(df)   # = 39

# --- 3. חישוב הכמויות ---
counts <- c(
  disclosure = sum(df$requires_ai_disclosure == "yes", na.rm = TRUE),
  enforce    = sum(df$enforce_penalties == "yes", na.rm = TRUE),
  permit     = sum(df$genai_allowed_general_use == "yes", na.rm = TRUE),
  published  = sum(df$cs_ai_policy_specified == "yes" |
                   df$ds_ai_policy_specifies == "yes", na.rm = TRUE)
)

plot_df <- data.frame(
  stage = factor(
    c("דרישת גילוי AI", "אכיפת ענישה",
      "מתירות שימוש כללי", "מפורסם לציבור בקורסי ליבה"),
    levels = c("דרישת גילוי AI", "אכיפת ענישה",
               "מתירות שימוש כללי", "מפורסם לציבור בקורסי ליבה")
  ),
  count = as.integer(counts),
  pct   = round(100 * counts / n)
)
print(plot_df)

# --- 4. גרף ---
p <- ggplot(plot_df, aes(x = stage, y = count, fill = stage)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(count, "/", n, "  (", pct, "%)")),
            hjust = -0.1, size = 4.2) +
  scale_y_continuous(limits = c(0, n + 6), expand = c(0, 0)) +
  scale_fill_manual(values = c("#185FA5", "#993C1D", "#378ADD", "#0F6E56")) +
  coord_flip() +
  labs(title = "מדיניות AI: פער בין הצהרה מוסדית לפרסום בקורסי הליבה",
       subtitle = paste0("מתוך ", n, " אוניברסיטאות מובילות"),
       x = NULL, y = "מספר אוניברסיטאות") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 15),
        plot.subtitle = element_text(color = "grey40"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 12))

print(p)

# --- 5. שמירה ---
dir.create("output", showWarnings = FALSE)
ggsave("output/ai_policy_funnel.png", p, width = 9, height = 5, dpi = 150, bg = "white")
cat("\nנשמר: output/ai_policy_funnel.png\n")
