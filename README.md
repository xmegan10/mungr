# MungrCleaner: An R Data Cleaning Framework

**MungrCleaner** is an R package designed to streamline data cleaning using the **S3 object system**. It provides a structured, auditable approach to common data preparation steps. Best for users who want to quickly clean data without worrying about long pipelines.

-----

## Key Features

The package centers around the `MungrCleaner` S3 object, which holds the raw data, applies modifications through generic methods, and maintains a detailed log of every operation.

| Function | Type | Description |
| :--- | :--- | :--- |
| `quick_clean()` | Generic | Executes a predefined, sequential pipeline of cleaning steps: text cleaning, type standardization, and imputation. |
| `clean_text()` | Generic | **Standardizes text**: Trims whitespace, converts empty strings (`""`) to `NA`, and optionally standardizes case (`lower`, `upper`, `title`). |
| `impute_missing()` | Generic | **Fills `NA` values** using various strategies (`mean`, `median`, `mode`) for numeric columns, and a custom value (default: `"Unknown"`) for character and factor columns. |
| `standardize_strcols()` | Generic | **Refines string columns**: Attempts to coerce character columns to **Date**, **Numeric**, or **Factor** based on content and predefined thresholds. |
| `standardize_numcols()` | Generic | **Refines numeric columns**: Converts numeric columns to optimal types, such as **Logical** (for 0/1 data), **Factor** (for low-cardinality data), or **Integer** (for memory optimization). |
