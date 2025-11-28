Mungr Demo & Example Cases
================
Author: Megan Xiao

Date: 2025-11-20

## Introduction

<br>This document intends to teach users how to use the “mungr” package.
The mungr package is designed to streamline and automate common data
cleaning and preparation tasks (data munging). It is most helpful for
users who want to quickly clean data without worrying about long
pipelines. The package provides users with generic advice on how to
clean messy data frames, column type standardization, text
standardization, missing value management, and the ability to automate
the entire cleaning sequence.<br> First, let’s load the library:<br>

``` r
library(mungr)
library(tibble)
```

<br>Let’s create an intentionally messy data frame, which will help us
test the effectiveness of our functions. For this demonstration, we
create a function called “generate_messy_df.” This function creates a
data frame by sampling from predefined vectors containing NA values,
inconsistent casing, erroneous data types, and empty values. Each column
samples from these vectors 100 times and returns the final data frame.
For reproducibility, we set the seed to 21:<br>

``` r
set.seed(21)

generate_messy_df <- function(){
  Names <- c("alice ", "BOB", "Charlie", "david", "EVE   ", NA, "frank", "", "grace", "Heidi", "unknown","JOE","john","Bill"," GRACE ","jerry","arnold")
Categories <- c(
    "ACTIVE", "inactive", "PENDING", "Active", "inactive", NA, "active", "", 
    "pending", "Active", "Unknown"
  )
Prices <- c("$100.50", "200.00", "300.75", "450", "Error", NA, "$10", "150.25", 
    "350", "225","300"," $2.50 ")
Is_Client <- sample(c(0, 1, 1.0, 0.0, NA), 100, replace = TRUE, prob = c(0.4, 0.4, 0.1, 0.05, 0.05))
Satisfaction_Rating <- c(sample(c("1","2","3",4.0,5), 100, replace = TRUE, prob = c(0.3, 0.2, 0.2, 0.1, 0.1)))
Dates <- c(
    "2023-01-01", "02/03/2023", "20230405", NA, "2023-10-20", "", 
    "2023-11-25", "2023-12-01", "2024-01-01", "01/15/2024","2024-01-05","unknown","2023-01-02","2023-01-03"
  )
ID <- 1:100


messy_df <- tibble(
  ID = factor(ID),
  Name = sample(Names, 100, replace = TRUE),
  Status = sample(Categories, 100, replace = TRUE),
  Sale_Price = sample(Prices, 100, replace = TRUE),
  Satisfaction_Rating = Satisfaction_Rating,
  Is_Active = Is_Client,
  Transaction_Date = sample(Dates, 100, replace = TRUE),
  Notes_this_is_an_empty_column = "  "
)

return(messy_df)
}

messy_df <- generate_messy_df()
```

<br>Let’s inspect the structure of messy_df:<br>

``` r
str(messy_df)
```

    ## tibble [100 × 8] (S3: tbl_df/tbl/data.frame)
    ##  $ ID                           : Factor w/ 100 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Name                         : chr [1:100] " GRACE " "unknown" "frank" "Heidi" ...
    ##  $ Status                       : chr [1:100] "pending" "inactive" "Active" "" ...
    ##  $ Sale_Price                   : chr [1:100] "450" NA "150.25" "200.00" ...
    ##  $ Satisfaction_Rating          : chr [1:100] "1" "2" "4" "5" ...
    ##  $ Is_Active                    : num [1:100] 1 0 1 0 0 NA 0 0 0 1 ...
    ##  $ Transaction_Date             : chr [1:100] "2023-01-01" "01/15/2024" "01/15/2024" "2023-12-01" ...
    ##  $ Notes_this_is_an_empty_column: chr [1:100] "  " "  " "  " "  " ...

<br>If we wanted to get the status of “cleanliness” of our data frame,
we could use the “get_advice” function from the MungrCleaner class. To
do this, we first create an object and pass messy_df into MungrCleaner.
Let’s print this object:<br>

``` r
cleaner <- MungrCleaner(messy_df)
print(cleaner)
```

    ## MungrCleaner History
    ## Status: No changes yet

<br>“cleaner” has 2 attributes: data and log. Data allows us to see our
data frame as a tibble within the object. We can pass this object into
other functions and, when we reassign the output, we can see the updated
tibble in the data attribute. The log attributes show us changes made to
our tibble from MungrClass methods.<br> We can access data and log by
using the \$ symbol:<br>

``` r
cleaner$data
```

    ## # A tibble: 100 × 8
    ##    ID    Name   Status Sale_Price Satisfaction_Rating Is_Active Transaction_Date
    ##    <fct> <chr>  <chr>  <chr>      <chr>                   <dbl> <chr>           
    ##  1 1     " GRA… "pend… 450        1                           1 2023-01-01      
    ##  2 2     "unkn… "inac… <NA>       2                           0 01/15/2024      
    ##  3 3     "fran… "Acti… 150.25     4                           1 01/15/2024      
    ##  4 4     "Heid… ""     200.00     5                           0 2023-12-01      
    ##  5 5      <NA>   <NA>  150.25     3                           0 2023-01-03      
    ##  6 6     "Heid… "Acti… 150.25     3                          NA 01/15/2024      
    ##  7 7     "BOB"  "Unkn… <NA>       3                           0 20230405        
    ##  8 8     " GRA… "Acti… 350        1                           0 <NA>            
    ##  9 9     " GRA… "Acti… <NA>       2                           0 2023-01-02      
    ## 10 10    "arno… "Acti… Error      5                           1 20230405        
    ## # ℹ 90 more rows
    ## # ℹ 1 more variable: Notes_this_is_an_empty_column <chr>

``` r
cleaner$log
```

    ## character(0)

## Example: get_advice

<br>The method “get_advice” analyzes a MungrCleaner object data frame,
identifies common data quality issues (missing values, empty strings,
case inconsistency, and inappropriate cardinality), and prints suggested
cleaning actions to the console. It invisibly returns the input object
with no modifications to the data or log.<br> Pass the object into the
method to get immediate results:<br>

``` r
cleaner <- get_advice(cleaner)
```

    ## Column [ID]:
    ##   -> Datatype: There's a lot of unique categories; 70% of your values are unique. This looks like an ID or free text.
    ## 
    ## Column [Name]:
    ##   -> Warning: 4% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ##   -> Consistency Issue: Mixed empty strings and NAs. If empty fields aren't intentional, replace empty strings with this code: df[df == ''] <- NA
    ## 
    ## Column [Status]:
    ##   -> Warning: 13% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ##   -> Consistency Issue: Mixed empty strings and NAs. If empty fields aren't intentional, replace empty strings with this code: df[df == ''] <- NA
    ##   -> Formatting issue: Case inconsistency detected (3 duplicates). You have the same values counted as unique from each other due to case sensitivity. If this is not intentional, run: toupper() or tolower()
    ##   -> Datatype: This column has low cardinality; 6.0% of your column has variance. You may want to change this column to a factor-type.
    ## 
    ## Column [Sale_Price]:
    ##   -> Warning: 14% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ## 
    ## Column [Satisfaction_Rating]:
    ##   -> Datatype: This column has low cardinality; 5.0% of your column has variance. You may want to change this column to a factor-type.
    ## 
    ## Column [Is_Active]:
    ##   -> Warning: 11% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ## 
    ## Column [Transaction_Date]:
    ##   -> Warning: 7% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ##   -> Consistency Issue: Mixed empty strings and NAs. If empty fields aren't intentional, replace empty strings with this code: df[df == ''] <- NA
    ## 
    ## Column [Notes_this_is_an_empty_column]:
    ##   -> Redundancy: Column has zero variance (same value for entire column). You might want to drop it.

<br>Additionally, users can adjust the following parameters:
na_threshold, factor_card_perc, and string_card_perc. Details on these
parameters can be found in the documentation.<br> For example, changing
string_card_perc from 0.1 (default) to 0.01 will remove the “low
cardinality” warning from Status and Satisfaction_Rating since they no
longer meet the threshold:<br>

``` r
cleaner <- get_advice(cleaner, string_card_perc = 0.01)
```

    ## Column [ID]:
    ##   -> Datatype: There's a lot of unique categories; 70% of your values are unique. This looks like an ID or free text.
    ## 
    ## Column [Name]:
    ##   -> Warning: 4% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ##   -> Consistency Issue: Mixed empty strings and NAs. If empty fields aren't intentional, replace empty strings with this code: df[df == ''] <- NA
    ## 
    ## Column [Status]:
    ##   -> Warning: 13% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ##   -> Consistency Issue: Mixed empty strings and NAs. If empty fields aren't intentional, replace empty strings with this code: df[df == ''] <- NA
    ##   -> Formatting issue: Case inconsistency detected (3 duplicates). You have the same values counted as unique from each other due to case sensitivity. If this is not intentional, run: toupper() or tolower()
    ## 
    ## Column [Sale_Price]:
    ##   -> Warning: 14% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ## 
    ## Column [Is_Active]:
    ##   -> Warning: 11% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ## 
    ## Column [Transaction_Date]:
    ##   -> Warning: 7% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ##   -> Consistency Issue: Mixed empty strings and NAs. If empty fields aren't intentional, replace empty strings with this code: df[df == ''] <- NA
    ## 
    ## Column [Notes_this_is_an_empty_column]:
    ##   -> Redundancy: Column has zero variance (same value for entire column). You might want to drop it.

## Example: clean_text

<br>The method “clean_text” standardizes text in character columns by
trimming whitespace, converting empty strings to NA, and standardizing
cases.<br>

``` r
cleaner <- clean_text(cleaner)
```

<br>Passing the object into the method changes the data attribute/data
frame. For example, our messy data frame from before contained trailing
white spaces in the name “EVE”:<br>

``` r
messy_df$Name
```

    ##   [1] " GRACE " "unknown" "frank"   "Heidi"   NA        "Heidi"   "BOB"    
    ##   [8] " GRACE " " GRACE " "arnold"  "Bill"    "BOB"     ""        "david"  
    ##  [15] "grace"   "EVE   "  "unknown" "unknown" "unknown" "unknown" "Bill"   
    ##  [22] "Heidi"   "david"   "Bill"    " GRACE " "david"   "JOE"     "JOE"    
    ##  [29] NA        "Heidi"   "grace"   "alice "  "JOE"     "alice "  "alice " 
    ##  [36] "david"   "frank"   "JOE"     "Charlie" "Bill"    ""        "BOB"    
    ##  [43] "Heidi"   "EVE   "  "grace"   "alice "  "grace"   "arnold"  "unknown"
    ##  [50] "Bill"    NA        "Heidi"   "JOE"     " GRACE " ""        "unknown"
    ##  [57] "david"   "jerry"   "Heidi"   "Heidi"   "grace"   "jerry"   "arnold" 
    ##  [64] ""        "alice "  "david"   "jerry"   "grace"   "Heidi"   "Charlie"
    ##  [71] "arnold"  "alice "  " GRACE " "Bill"    "frank"   " GRACE " "Heidi"  
    ##  [78] "Charlie" "arnold"  "Charlie" "Charlie" "unknown" "Bill"    NA       
    ##  [85] "Heidi"   "grace"   "john"    "arnold"  "JOE"     "unknown" "arnold" 
    ##  [92] "Charlie" "Bill"    "frank"   "grace"   "BOB"     " GRACE " "EVE   " 
    ##  [99] "david"   "Charlie"

<br>In our cleaner object, the trailing white space has now been
removed:<br>

``` r
print(cleaner$data$Name)
```

    ##   [1] "GRACE"   "unknown" "frank"   "Heidi"   NA        "Heidi"   "BOB"    
    ##   [8] "GRACE"   "GRACE"   "arnold"  "Bill"    "BOB"     NA        "david"  
    ##  [15] "grace"   "EVE"     "unknown" "unknown" "unknown" "unknown" "Bill"   
    ##  [22] "Heidi"   "david"   "Bill"    "GRACE"   "david"   "JOE"     "JOE"    
    ##  [29] NA        "Heidi"   "grace"   "alice"   "JOE"     "alice"   "alice"  
    ##  [36] "david"   "frank"   "JOE"     "Charlie" "Bill"    NA        "BOB"    
    ##  [43] "Heidi"   "EVE"     "grace"   "alice"   "grace"   "arnold"  "unknown"
    ##  [50] "Bill"    NA        "Heidi"   "JOE"     "GRACE"   NA        "unknown"
    ##  [57] "david"   "jerry"   "Heidi"   "Heidi"   "grace"   "jerry"   "arnold" 
    ##  [64] NA        "alice"   "david"   "jerry"   "grace"   "Heidi"   "Charlie"
    ##  [71] "arnold"  "alice"   "GRACE"   "Bill"    "frank"   "GRACE"   "Heidi"  
    ##  [78] "Charlie" "arnold"  "Charlie" "Charlie" "unknown" "Bill"    NA       
    ##  [85] "Heidi"   "grace"   "john"    "arnold"  "JOE"     "unknown" "arnold" 
    ##  [92] "Charlie" "Bill"    "frank"   "grace"   "BOB"     "GRACE"   "EVE"    
    ##  [99] "david"   "Charlie"

<br>Additionally, users can adjust the following parameters: case and
trime. Details on these parameters can be found in the documentation.
For example, changing case to “title” will capitalize the first letter
of every name:<br>

``` r
cleaner <- clean_text(cleaner, case = "title")
cleaner$data$Name
```

    ##   [1] "Grace"   "Unknown" "Frank"   "Heidi"   NA        "Heidi"   "Bob"    
    ##   [8] "Grace"   "Grace"   "Arnold"  "Bill"    "Bob"     NA        "David"  
    ##  [15] "Grace"   "Eve"     "Unknown" "Unknown" "Unknown" "Unknown" "Bill"   
    ##  [22] "Heidi"   "David"   "Bill"    "Grace"   "David"   "Joe"     "Joe"    
    ##  [29] NA        "Heidi"   "Grace"   "Alice"   "Joe"     "Alice"   "Alice"  
    ##  [36] "David"   "Frank"   "Joe"     "Charlie" "Bill"    NA        "Bob"    
    ##  [43] "Heidi"   "Eve"     "Grace"   "Alice"   "Grace"   "Arnold"  "Unknown"
    ##  [50] "Bill"    NA        "Heidi"   "Joe"     "Grace"   NA        "Unknown"
    ##  [57] "David"   "Jerry"   "Heidi"   "Heidi"   "Grace"   "Jerry"   "Arnold" 
    ##  [64] NA        "Alice"   "David"   "Jerry"   "Grace"   "Heidi"   "Charlie"
    ##  [71] "Arnold"  "Alice"   "Grace"   "Bill"    "Frank"   "Grace"   "Heidi"  
    ##  [78] "Charlie" "Arnold"  "Charlie" "Charlie" "Unknown" "Bill"    NA       
    ##  [85] "Heidi"   "Grace"   "John"    "Arnold"  "Joe"     "Unknown" "Arnold" 
    ##  [92] "Charlie" "Bill"    "Frank"   "Grace"   "Bob"     "Grace"   "Eve"    
    ##  [99] "David"   "Charlie"

## Example: standardize_strcols

<br>The method “standardize_strcols” refines the data type of string
columns within a MungrCleaner object, converting them to a Boolean type,
numerical type, date type, or factor (Categorical) type based on their
content.<br>

``` r
cleaner <- standardize_strcols(cleaner)
```

<br>Passing the object into the method changes the column types of
string columns in the data attribute/data frame. For example, the
structure of messy_df has incorrect column types and mixed data types in
the fields:<br>

``` r
str(messy_df)
```

    ## tibble [100 × 8] (S3: tbl_df/tbl/data.frame)
    ##  $ ID                           : Factor w/ 100 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Name                         : chr [1:100] " GRACE " "unknown" "frank" "Heidi" ...
    ##  $ Status                       : chr [1:100] "pending" "inactive" "Active" "" ...
    ##  $ Sale_Price                   : chr [1:100] "450" NA "150.25" "200.00" ...
    ##  $ Satisfaction_Rating          : chr [1:100] "1" "2" "4" "5" ...
    ##  $ Is_Active                    : num [1:100] 1 0 1 0 0 NA 0 0 0 1 ...
    ##  $ Transaction_Date             : chr [1:100] "2023-01-01" "01/15/2024" "01/15/2024" "2023-12-01" ...
    ##  $ Notes_this_is_an_empty_column: chr [1:100] "  " "  " "  " "  " ...

<br>After running “standardize_strcols,” the string columns of our
cleaner objects have been converted to the correct data types:<br>

``` r
str(cleaner$data)
```

    ## tibble [100 × 8] (S3: tbl_df/tbl/data.frame)
    ##  $ ID                           : num [1:100] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Name                         : chr [1:100] "Grace" "Unknown" "Frank" "Heidi" ...
    ##  $ Status                       : Factor w/ 4 levels "Active","Inactive",..: 3 2 1 NA NA 1 4 1 1 1 ...
    ##  $ Sale_Price                   : num [1:100] 450 NA 150 200 150 ...
    ##  $ Satisfaction_Rating          : num [1:100] 1 2 4 5 3 3 3 1 2 5 ...
    ##  $ Is_Active                    : num [1:100] 1 0 1 0 0 NA 0 0 0 1 ...
    ##  $ Transaction_Date             : Date[1:100], format: "2023-01-01" NA ...
    ##  $ Notes_this_is_an_empty_column: chr [1:100] NA NA NA NA ...

<br>Additionally, users can adjust the following parameters: col_name,
num_threshold, date_threshold, and cat_threshold. Details on these
parameters can be found in the documentation.<br> <br>For example,
changing the cat_threshold from 0.1 (default) to 0.4 will turn the
column type of Name into a factor since unique values do not appear more
in 40% of the total values for that column.<br>

``` r
cleaner <- standardize_strcols(cleaner, cat_threshold = 0.4)
str(cleaner$data)
```

    ## tibble [100 × 8] (S3: tbl_df/tbl/data.frame)
    ##  $ ID                           : num [1:100] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Name                         : Factor w/ 14 levels "Alice","Arnold",..: 9 14 8 10 NA 10 4 9 9 2 ...
    ##  $ Status                       : Factor w/ 4 levels "Active","Inactive",..: 3 2 1 NA NA 1 4 1 1 1 ...
    ##  $ Sale_Price                   : num [1:100] 450 NA 150 200 150 ...
    ##  $ Satisfaction_Rating          : num [1:100] 1 2 4 5 3 3 3 1 2 5 ...
    ##  $ Is_Active                    : num [1:100] 1 0 1 0 0 NA 0 0 0 1 ...
    ##  $ Transaction_Date             : Date[1:100], format: "2023-01-01" NA ...
    ##  $ Notes_this_is_an_empty_column: chr [1:100] NA NA NA NA ...

<br>Let’s return the Name column back to a character type.<br>

``` r
cleaner$data$Name <- as.character(cleaner$data$Name)
str(cleaner)
```

    ## List of 2
    ##  $ data: tibble [100 × 8] (S3: tbl_df/tbl/data.frame)
    ##   ..$ ID                           : num [1:100] 1 2 3 4 5 6 7 8 9 10 ...
    ##   ..$ Name                         : chr [1:100] "Grace" "Unknown" "Frank" "Heidi" ...
    ##   ..$ Status                       : Factor w/ 4 levels "Active","Inactive",..: 3 2 1 NA NA 1 4 1 1 1 ...
    ##   ..$ Sale_Price                   : num [1:100] 450 NA 150 200 150 ...
    ##   ..$ Satisfaction_Rating          : num [1:100] 1 2 4 5 3 3 3 1 2 5 ...
    ##   ..$ Is_Active                    : num [1:100] 1 0 1 0 0 NA 0 0 0 1 ...
    ##   ..$ Transaction_Date             : Date[1:100], format: "2023-01-01" NA ...
    ##   ..$ Notes_this_is_an_empty_column: chr [1:100] NA NA NA NA ...
    ##  $ log : chr [1:4] "Text cleaned (Empty -> NA), Trimmed)" "Text cleaned (Empty -> NA), Trimmed, Case: title)" "Standardize Str Col Types: Converted 5 columns: [ID (-> Num), Status (-> Factor), Sale_Price (-> Num), Satisfac"| __truncated__ "Standardize Str Col Types: Converted 2 columns: [Name (-> Factor), Status (-> Factor)]"
    ##  - attr(*, "class")= chr "MungrCleaner"

## Example: standardize_numcols

<br>The method “standardize_numcols” refines the data type of numeric
columns within a MungrCleaner object, converting them into a Boolean
type, factor (Categorical) type, or integer (Memory Optimization) type
based on their content. For the purpose of this demonstration, we will
create a new cleaner object called. “cleaner_num” and change the column
“Satisfaction_Rating” to a numeric:<br>

``` r
cleaner_num <- MungrCleaner(messy_df)
cleaner_num$data$Satisfaction_Rating <- as.numeric(cleaner_num$data$Satisfaction_Rating)

cleaner_num <- standardize_numcols(cleaner_num)
```

<br>Passing the object into the method changes the column types of
numeric columns in the data attribute/data frame. For this example, we
are focusing on “Satisfaction_Rating” and Is_Active.”<br>

``` r
str(messy_df)
```

    ## tibble [100 × 8] (S3: tbl_df/tbl/data.frame)
    ##  $ ID                           : Factor w/ 100 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Name                         : chr [1:100] " GRACE " "unknown" "frank" "Heidi" ...
    ##  $ Status                       : chr [1:100] "pending" "inactive" "Active" "" ...
    ##  $ Sale_Price                   : chr [1:100] "450" NA "150.25" "200.00" ...
    ##  $ Satisfaction_Rating          : chr [1:100] "1" "2" "4" "5" ...
    ##  $ Is_Active                    : num [1:100] 1 0 1 0 0 NA 0 0 0 1 ...
    ##  $ Transaction_Date             : chr [1:100] "2023-01-01" "01/15/2024" "01/15/2024" "2023-12-01" ...
    ##  $ Notes_this_is_an_empty_column: chr [1:100] "  " "  " "  " "  " ...

<br>After running “standardize_numcols,” the numeric columns of our
cleaner_num object have been converted to the correct data types.
“Satisfaction_Rating” and “Is_Active” are now the correct column
types:<br>

``` r
str(cleaner_num$data)
```

    ## tibble [100 × 8] (S3: tbl_df/tbl/data.frame)
    ##  $ ID                           : Factor w/ 100 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Name                         : chr [1:100] " GRACE " "unknown" "frank" "Heidi" ...
    ##  $ Status                       : chr [1:100] "pending" "inactive" "Active" "" ...
    ##  $ Sale_Price                   : chr [1:100] "450" NA "150.25" "200.00" ...
    ##  $ Satisfaction_Rating          : Factor w/ 5 levels "1","2","3","4",..: 1 2 4 5 3 3 3 1 2 5 ...
    ##  $ Is_Active                    : logi [1:100] TRUE FALSE TRUE FALSE FALSE NA ...
    ##  $ Transaction_Date             : chr [1:100] "2023-01-01" "01/15/2024" "01/15/2024" "2023-12-01" ...
    ##  $ Notes_this_is_an_empty_column: chr [1:100] "  " "  " "  " "  " ...

<br>As a note, a majority of data cleaning can be done with the method
“standardized_strcols.” However, if a user wants to target only numeric
columns, this is a way to do so.<br> <br>Moreover, users can adjust the
following parameters: col_name, convert_int, cat_threshold. Details on
these parameters can be found in the documentation.<br>

## Example: impute_missing

<br>The method “impute missing” fills missing (NA) values in data
columns using various strategies (e.g., mean, median, mode, or a custom
value). Before implementing the method, let’s investigate the numeric
column “Sale_Price”:<br>

``` r
cleaner$data$Sale_Price
```

    ##   [1] 450.00     NA 150.25 200.00 150.25 150.25     NA 350.00     NA     NA
    ##  [11]   2.50 150.25     NA 150.25     NA 150.25 450.00 350.00 150.25  10.00
    ##  [21] 150.25 200.00   2.50     NA 300.75     NA 150.25 200.00   2.50 150.25
    ##  [31] 100.50     NA   2.50 150.25 300.00 200.00     NA  10.00 450.00 300.00
    ##  [41]     NA 100.50 450.00 200.00 200.00 225.00     NA 300.75  10.00 450.00
    ##  [51] 150.25 200.00 300.00     NA 350.00 450.00 200.00 300.00     NA 100.50
    ##  [61]  10.00 450.00 450.00     NA 100.50 200.00 300.75     NA 300.00 100.50
    ##  [71] 450.00 225.00     NA 350.00     NA     NA 200.00 225.00   2.50 100.50
    ##  [81] 100.50   2.50 100.50 350.00 450.00   2.50 450.00 300.00   2.50 450.00
    ##  [91] 100.50 300.00 200.00 300.75   2.50     NA 350.00 300.75 350.00 150.25

<br>The total number of NAs in this column is 20:<br>

``` r
sum(is.na(cleaner$data$Sale_Price))
```

    ## [1] 20

<br>Let’s pass our object into this method. By default, the method will
impute missing values for numeric columns with the column mean:<br>

``` r
cleaner <- impute_missing(cleaner)
```

<br>Now, the total number of NAs is 0:<br>

``` r
sum(is.na(cleaner$data$Sale_Price))
```

    ## [1] 0

<br>We can see that the method replaced the NA values with 215.6125:<br>

``` r
cleaner$data$Sale_Price
```

    ##   [1] 450.0000 215.6125 150.2500 200.0000 150.2500 150.2500 215.6125 350.0000
    ##   [9] 215.6125 215.6125   2.5000 150.2500 215.6125 150.2500 215.6125 150.2500
    ##  [17] 450.0000 350.0000 150.2500  10.0000 150.2500 200.0000   2.5000 215.6125
    ##  [25] 300.7500 215.6125 150.2500 200.0000   2.5000 150.2500 100.5000 215.6125
    ##  [33]   2.5000 150.2500 300.0000 200.0000 215.6125  10.0000 450.0000 300.0000
    ##  [41] 215.6125 100.5000 450.0000 200.0000 200.0000 225.0000 215.6125 300.7500
    ##  [49]  10.0000 450.0000 150.2500 200.0000 300.0000 215.6125 350.0000 450.0000
    ##  [57] 200.0000 300.0000 215.6125 100.5000  10.0000 450.0000 450.0000 215.6125
    ##  [65] 100.5000 200.0000 300.7500 215.6125 300.0000 100.5000 450.0000 225.0000
    ##  [73] 215.6125 350.0000 215.6125 215.6125 200.0000 225.0000   2.5000 100.5000
    ##  [81] 100.5000   2.5000 100.5000 350.0000 450.0000   2.5000 450.0000 300.0000
    ##  [89]   2.5000 450.0000 100.5000 300.0000 200.0000 300.7500   2.5000 215.6125
    ##  [97] 350.0000 300.7500 350.0000 150.2500

<br>Additionally, users can adjust the following parameters: col_name,
imp_type, num_val, char_val. Details on these parameters can be found in
the documentation.<br> <br>For example, changing the “imp_type” to
“custom_char” will fill all character type columns with “Unknown.” As
seen by the warning, factor columns will be converted to strings during
imputation, so best practice includes filling missing fields before
standardizing column types:<br>

``` r
cleaner <- impute_missing(cleaner, imp_type = "custom_char")
```

    ## Warning in impute_missing.MungrCleaner(cleaner, imp_type = "custom_char"):
    ## Column 'Status' was converted to 'character' to safely impute with 'Unknown'.

<br>Now, none of our columns (except “Transaction_Date” due to it being
a date type) have NA values:<br>

``` r
colSums(is.na(cleaner$data))
```

    ##                            ID                          Name 
    ##                             0                             0 
    ##                        Status                    Sale_Price 
    ##                             0                             0 
    ##           Satisfaction_Rating                     Is_Active 
    ##                             0                             0 
    ##              Transaction_Date Notes_this_is_an_empty_column 
    ##                            41                             0

## Example: quick_clean

<br>The method “quick_clean” applies a standardized, multi-step sequence
of data cleaning and type standardization functions to a MungrCleaner
object. In summary, it runs through every MungrCleaner method, allowing
users to quickly clean a data frame without worrying about long
pipelines. For the purpose of this demonstration, let’s create a new
MungrCleaner object called “cleaner_quick”:<br>

``` r
cleaner_quick <- MungrCleaner(messy_df)
```

<br>Let’s run “get_advice” to see the current errors with each
column:<br>

``` r
get_advice(cleaner_quick)
```

    ## Column [ID]:
    ##   -> Datatype: There's a lot of unique categories; 70% of your values are unique. This looks like an ID or free text.
    ## 
    ## Column [Name]:
    ##   -> Warning: 4% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ##   -> Consistency Issue: Mixed empty strings and NAs. If empty fields aren't intentional, replace empty strings with this code: df[df == ''] <- NA
    ## 
    ## Column [Status]:
    ##   -> Warning: 13% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ##   -> Consistency Issue: Mixed empty strings and NAs. If empty fields aren't intentional, replace empty strings with this code: df[df == ''] <- NA
    ##   -> Formatting issue: Case inconsistency detected (3 duplicates). You have the same values counted as unique from each other due to case sensitivity. If this is not intentional, run: toupper() or tolower()
    ##   -> Datatype: This column has low cardinality; 6.0% of your column has variance. You may want to change this column to a factor-type.
    ## 
    ## Column [Sale_Price]:
    ##   -> Warning: 14% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ## 
    ## Column [Satisfaction_Rating]:
    ##   -> Datatype: This column has low cardinality; 5.0% of your column has variance. You may want to change this column to a factor-type.
    ## 
    ## Column [Is_Active]:
    ##   -> Warning: 11% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ## 
    ## Column [Transaction_Date]:
    ##   -> Warning: 7% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ##   -> Consistency Issue: Mixed empty strings and NAs. If empty fields aren't intentional, replace empty strings with this code: df[df == ''] <- NA
    ## 
    ## Column [Notes_this_is_an_empty_column]:
    ##   -> Redundancy: Column has zero variance (same value for entire column). You might want to drop it.

    ## MungrCleaner History
    ## Status: No changes yet

<br>Now, let’s pass “cleaner_quick” into this method. Now when we run
“get_advice,” we get less error messages:<br>

``` r
cleaner_quick <- quick_clean(cleaner_quick)
get_advice(cleaner_quick)
```

    ## Column [Name]:
    ##   -> Warning: 8% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ## 
    ## Column [Status]:
    ##   -> Warning: 27% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ##   -> Datatype: This column has low cardinality; 5.0% of your column has variance. You may want to change this column to a factor-type.
    ## 
    ## Column [Satisfaction_Rating]:
    ##   -> Datatype: This column has low cardinality; 5.0% of your column has variance. You may want to change this column to a factor-type.
    ## 
    ## Column [Is_Active]:
    ##   -> Warning: 11% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ## 
    ## Column [Transaction_Date]:
    ##   -> Warning: 41% missing. Consider imputing missing values with a custom value. If this is a numeric-type column, consider imputing values with mean, median, or mode
    ## 
    ## Column [Notes_this_is_an_empty_column]:
    ##   -> CRITICAL: 100% missing. Consider dropping this column.

    ## MungrCleaner History
    ## Status: Cleaned 6 steps.

<br>We can view our history of method usage with the log attribute:<br>

``` r
cleaner_quick$log
```

    ## [1] "===Quick Clean==="                                                                                                                                               
    ## [2] "Text cleaned (Empty -> NA), Trimmed, Case: title)"                                                                                                               
    ## [3] "Standardize Str Col Types: Converted 5 columns: [ID (-> Num), Status (-> Factor), Sale_Price (-> Num), Satisfaction_Rating (-> Num), Transaction_Date (-> Date)]"
    ## [4] "Standardized Num Column Types: Converted 3 columns: [ID(-> Int), Satisfaction_Rating (-> Factor), Is_Active (-> Bool)]"                                          
    ## [5] "Imputed Missing: 2 columns changed: [ID (Num Imputed), Sale_Price (Num Imputed)]"                                                                                
    ## [6] "===End Quick Clean==="

<br>As of now, “quick_clean” has not been optimized to handle every
existing case for messy data. Therefore, this function has certain
limitations.<br>
