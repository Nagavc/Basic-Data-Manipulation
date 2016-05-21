#### DATA WRANGLING EXERCISE 1: BASIC DATA MANIPULATION ####


data_path = 'C:/Users/Naga Viraja/Desktop/Foundations of Data Science/refine_original.csv'
refine <- read.csv(data_path, header = TRUE, stringsAsFactors = FALSE)

## 1. CLEAN UP BRAND NAMES
# values in company column
refine$company

clean_company <- function(s){
  s <- tolower(s)
  n <- nchar(s)
  first_2 <- substr(s,1,2)
  last_2 <- substr(s,n-1,n)
  
  if(last_2 == 'ps'){
    return('phillips')
  }else if(first_2 == 'ak'){
    return('akzo')
  }else if(first_2 == 'va'){
    return('van houten')
  }else{
    return('unilever')
  }
}

#testing output for 1.
clean_company(refine$company[1])
clean_company(refine$company[20])
clean_company(refine$company[24])

refine$company <- sapply(refine$company, FUN = clean_company)
refine$company <- factor(refine$company)

## 2. SEPARATE PRODUCT CODE AND NUMBER
productcodes <- strsplit(refine$Product.code...number, split = '-')

refine$product_code <- sapply(productcodes, FUN = function(x) x[1])
refine$product_number <- sapply(productcodes, FUN = function(x) x[2])
refine$product_number <- as.integer(refine$product_number)

#testing output for 2.
refine$product_code
refine$product_number


## 3. ADD COLUMN PRODUCT CATEGORY
look_up_table <- c('p' = 'Smartphone', 'v' = 'TV', 'x' = 'Laptop', 'q' = 'Tablet')
refine$product_category <- factor(look_up_table[refine$product_code])
refine$product_category

## 4. ADD FULL ADDRESS FOR GEOCODING
refine$full_address <- paste(refine$address, refine$city, refine$country, sep = ', ')
names(refine)

## 5. CREATE DUMMY VARIABLES FOR COMPANY AND PRODUCT CATEGORY

create_dummy <- function(vec, value){
  vec <- as.character(vec)
  return(as.integer(vec == value))
}

company_names <- as.character(unique(refine$company))

for (company in company_names) {
  new_var_name <- paste0('company_', company)
  refine[[new_var_name]] <- create_dummy(refine$company, company)
}

product_names <- as.character(unique(refine$product_category))

for (product in product_names) {
  new_var_name <- paste0('product_', product)
  refine[[new_var_name]] <- create_dummy(refine$product_category, product)
}

names(refine)

## 6. SUBMIT THE PROJECT ON GITHUB
write.csv(x = refine, file = 'refine_clean.csv')
