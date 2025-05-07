# ==============================================================================  
# Cleaned R Parser (with formula chaining, parentheses checking, robust errors)  
# ==============================================================================  

# Clear workspace  
rm(list = ls())  

# Install and load required packages  
required_packages <- c("lubridate", "stringr", "data.table", "zoo", "dplyr", "readxl", "rlang")  
installed_packages  <- rownames(installed.packages())  
for (pkg in required_packages) {  
  if (!(pkg %in% installed_packages)) {  
    install.packages(pkg)  
  }  
}  
library(lubridate)  
library(stringr)  
library(data.table)  
library(zoo)  
library(dplyr)  
library(readxl)  
library(rlang)  
options(stringsAsFactors = FALSE)  

# ==============================================================================  
# 1) Read Excel File for Formulas  
# ==============================================================================  
path.dir        <- dirname(rstudioapi::getSourceEditorContext()$path)  
file_formulas   <- file.path(path.dir, "Formulas.xlsx")  
formulas_dt     <- as.data.table(read_excel(file_formulas, sheet = "Formulas"))  
setnames(formulas_dt, old = "Construction", new = "Formula")  

# ==============================================================================  
# 2) Read real data from Excel files (skip first row header; skip missing tickers)  
# ==============================================================================  
data_dir        <- file.path(path.dir, "data")  
tickers         <- c("MAY MK Equity", "PBK MK Equity", "TNB MK Equity", "CIMB MK Equity", "GAM MK Equity")  
missing_tickers <- character(0)  
data_list       <- list()  
for (ticker_full in tickers) {  
  ticker_code <- sub(" .*", "", ticker_full)  
  acc_file     <- file.path(data_dir, paste0("Data_Accounting_", ticker_code, ".xlsx"))  
  mkt_file     <- file.path(data_dir, paste0("Data_Market_", ticker_code, ".xlsx"))  
  
  # Skip if files are missing  
  if (!file.exists(acc_file) || !file.exists(mkt_file)) {  
    warning(paste("Data files not found for ticker", ticker_full, "- skipping."))  
    missing_tickers <- c(missing_tickers, ticker_full)  
    next  
  }  
  
  # Read accounting and market sheets, skipping first metadata row  
  dt_acc <- as.data.table(read_excel(acc_file, skip = 1))  
  dt_mkt <- as.data.table(read_excel(mkt_file, skip = 1))  
  
  # Standardize Date column  
  setnames(dt_acc, names(dt_acc)[1], "Date")  
  setnames(dt_mkt, names(dt_mkt)[1], "Date")  
  
  # Clean non-numeric values in all other columns  
  num_cols_acc <- setdiff(names(dt_acc), c("Date", "Company Name"))
  dt_acc[, (num_cols_acc) := lapply(.SD, function(x) as.numeric(gsub("[^0-9.-]", "", x))), .SDcols = num_cols_acc]  
  num_cols_mkt <- setdiff(names(dt_mkt), c("Date", "Company Name"))  
  dt_mkt[, (num_cols_mkt) := lapply(.SD, function(x) as.numeric(gsub("[^0-9.-]", "", x))), .SDcols = num_cols_mkt]  
  
  # Lag accounting variables by 4 months for public availability  
  dt_acc[, Date := Date %m+% months(4)]  
  
  # Ensure single entry per Date to avoid cartesian joins  
  dt_acc <- unique(dt_acc, by = "Date")  
  dt_mkt <- unique(dt_mkt, by = "Date")  
  
  # Merge and tag ticker (allow cartesian if truly needed)  
  dt_merge <- merge(dt_acc, dt_mkt, by = "Date", all = TRUE, allow.cartesian = TRUE)  
  dt_merge[, Ticker := ticker_full]  
  data_list[[ticker_full]] <- dt_merge  
}  
# Report skipped tickers  
if (length(missing_tickers) > 0) {  
  cat("Tickers skipped due to missing data:", paste(missing_tickers, collapse = ", "), "\n")  
}  
# Combine and reshape to long format  
dt <- rbindlist(data_list, fill = TRUE)  
dt <- melt(dt, id.vars = c("Ticker", "Date"), variable.name = "Bloomberg.Field", value.name = "Value")  

# ==============================================================================  
# 3) Custom Functions  
# ==============================================================================  
LAG    <- function(x, k) { ks <- as.character(k); n <- as.numeric(str_extract(ks, "^[0-9]+")); s <- str_extract(ks, "[MDY]$"); m <- switch(s, D = 1L, M = 21L, Y = 252L, 1L); dplyr::lag(x, n*m) }  
LEAD   <- function(x, k) { ks <- as.character(k); n <- as.numeric(str_extract(ks, "^[0-9]+")); s <- str_extract(ks, "[MDY]$"); m <- switch(s, D = 1L, M = 21L, Y = 252L, 1L); dplyr::lead(x, n*m) }  
STD    <- function(x, window, na.rm = TRUE) { ws <- as.character(window); if (grepl("^[0-9]+[MDY]$", ws)) { n <- as.integer(str_extract(ws, "^[0-9]+")); sfx <- str_extract(ws, "[MDY]$"); mult <- switch(sfx, D=1L, M=21L, Y=252L); w <- n*mult } else w <- as.integer(window); zoo::rollapply(x, width = w, FUN = function(z) stats::sd(z, na.rm = na.rm), align = "right", fill = NA) }  
CHG_TO_EXP <- function(x) { l12 <- dplyr::lag(x,12*21); l24 <- dplyr::lag(x,24*21); x/((l12+l24)/2) }  
SUR    <- function(x, window = 12*21) { x3 <- dplyr::lag(x,3*21); x15 <- dplyr::lag(x,15*21); num <- x - (x3+(x3-x15)/4); den <- STD(x3-x15, window); num/den }  
MAX    <- function(x,y) pmax(x,y,na.rm=TRUE)  
MIN    <- function(x,y) pmin(x,y,na.rm=TRUE)  
CUMPROD<- function(x) cumprod(x)  

# ==============================================================================  
# 4) Helper Functions  
# ==============================================================================  
check_parentheses_balance <- function(text) { str_count(text, fixed("(")) == str_count(text, fixed(")")) }  
# Global storage for computed variables  
computed_variables <- list()  

# ==============================================================================  
# 5) Evaluate a Formula  
# ==============================================================================  
evaluate_formula <- function(formula, data) {  
  if (is.na(formula) || nchar(trimws(formula)) == 0) return(NULL)  
  if (!check_parentheses_balance(formula)) { warning("Unbalanced parentheses in: ", formula); return(NULL) }  
  cat("DEBUG: Formula original:", formula, "\n")  
  fc <- formula %>% str_replace_all("(LAG|LEAD|STD)\\(([^,]+?),\\s*([0-9]+[MDY])\\)", "\\1(\\2, \"\\3\")") %>% str_replace_all("−","-")  
  cat("DEBUG: Formula clean   :", fc, "\n")  
  expr   <- rlang::parse_expr(fc)  
  fields <- setdiff(unique(str_extract_all(fc, "\\b[A-Z_][A-Z0-9_]*\\b")[[1]]), c("LAG","LEAD","STD","CHG_TO_EXP","SUR","MAX","MIN","CUMPROD"))  
  res    <- data.table()  
  for (tic in unique(data$Ticker)) {  
    parts <- lapply(fields, function(f) { tmp <- data[Ticker==tic & Bloomberg.Field==f, .(Date,Value)]; setnames(tmp,"Value",f); tmp })  
    if (any(sapply(parts, nrow)==0)) next  
    # Merge field parts ensuring cartesian allowed if needed  
    dtf   <- Reduce(function(a,b) merge(a,b, by="Date", all=TRUE, allow.cartesian=TRUE), parts)  
    setorder(dtf, Date)  
    env   <- list2env(c(setNames(dtf[,..fields], fields), list(LAG=LAG,LEAD=LEAD,STD=STD,CHG_TO_EXP=CHG_TO_EXP,SUR=SUR,MAX=MAX,MIN=MIN,CUMPROD=CUMPROD)))  
    comp  <- try(eval(expr, env), silent=TRUE)  
    if (inherits(comp, "try-error")) next  
    dtf   <- dtf[, Computed := comp][, Ticker := tic]  
    res   <- rbind(res, dtf[, .(Ticker, Date, Computed)])  
  }  
  res  
}  

# ==============================================================================  
# 6) Main compute_formulas()  
# ==============================================================================  
compute_formulas <- function(formulas_dt, dt) {  
  res_list <- list()  
  for (i in seq_len(nrow(formulas_dt))) {  
    nm  <- formulas_dt[i, Name]  
    frm <- formulas_dt[i, Formula]  
    cat("Computing:", nm, "\n")  
    out <- evaluate_formula(frm, dt)  
    if (is.null(out) || nrow(out) == 0 || !"Computed" %in% names(out)) {  
      warning(paste("No computed data for formula", nm, "- skipping."))  
      next  
    }  
    out[, Name := nm]  
    setnames(out, "Computed", "Value")  
    res_list[[nm]] <- out[, .(Ticker, Date, Value, Name)]  
    computed_variables[[nm]] <- out[, .(Ticker, Date, Value)]  
  }  
  if (length(res_list)) rbindlist(res_list, use.names = TRUE, fill = TRUE) else NULL  
}  

# ==============================================================================  
# Main Execution  
# ==============================================================================  
fwrite(dt, file = "cleaned_data.csv")  
computed_dt <- compute_formulas(formulas_dt, dt)  
if (!is.null(computed_dt)) {  
  fwrite(computed_dt, file = "computed_factors.csv")  
  cat("Done. computed_factors.csv written.\n")  
} else {  
  cat("No formulas computed.\n")  
}

