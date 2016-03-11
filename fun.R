# These functions help convert ISTAT comuni administrative codes from year to year based on 

variation_file <- "http://www.istat.it/it/files/2011/01/Variazioni-amministrative-e-territoriali-dal-1991.zip"

## Step 1 
## Get variations details

prepareComVariationDF <- function (com_variations) {
  require(plyr)
  # 
  com_variations <- plyr::rename(com_variations, 
                                 c("Anno" = "year",
                                   "Tipo.variazione" = "type",
                                   "Codice.Regione" = "cod_istat_regione",
                                   "Codice.Istat.del.Comune" = "cod_istat_comune",
                                   "Denominazione.Comune" = "name_comune",
                                   "Codice.Istat.del.Comune.associato.alla.variazione.o.nuovo.codice.Istat.del.Comune" =
                                     "new_cod_istat_comune",
                                   "Denominazione.Comune.associata.alla.variazione.o.nuova.denominazione" =
                                     "new_name_comune",
                                   "Provvedimento.e.Documento" = "legislative_measure",
                                   "Contenuto.del.provvedimento" = "legislative_measure_desc",
                                   "Data.decorrenza.validità.amministrativa" = "date"))
  
  
  extractDateFromString <- function(string) {
    require(stringr)
    it_months <- c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio",
                   "agosto","settembre","ottobre","novembre","dicembre")
    names(it_months) <- sprintf("%02d", 1:12)
    grep_it_date <- paste0("\\d{1,2}(°?)( ?)(", paste(it_months, collapse="|") ,")( ?)\\d{2,4}")
    dates_from_string <- str_extract_all(tolower(string), grep_it_date, simplify = FALSE)[[1]]
    last_date_from_string <- gsub("°", "", dates_from_string[length(dates_from_string)])
    
    day <- str_extract(last_date_from_string, "^\\d{1,2}")
    month <- str_extract(last_date_from_string, paste0("(", paste(it_months, collapse="|"),")"))
    month <- names(which(it_months == month))
    year <- str_extract(last_date_from_string, "\\d{2,4}$")
    year <- ifelse(nchar(year)==2, paste0('19',year), year) 
    
    return(as.Date(paste(day, month, year, collapse = " "), format = "%e %m %Y"))
  }
  
  com_variations$date <- as.Date(com_variations$date, format = '%e/%m/%Y')
  com_variations$date <- as.Date(ifelse(is.na(com_variations$date), 
                                        sapply(com_variations$legislative_measure, 
                                               extractDateFromString),
                                        com_variations$date), origin = "1970-01-01")
  return(com_variations)
}


# SERVICE
# Download new ISTAT file
# tmp_dir <- tempdir()
# download.file(variation_file, paste0(tmp_dir,"/istat.zip"))
# path <- unzip(paste0(tmp_dir,"/istat.zip"), exdir=tmp_dir)
# com_variations <-
#   read.csv(path[grepl(".csv$", path)], stringsAsFactors = FALSE, fileEncoding="latin3", sep=";")
# clean_com_variations <- prepareComVariationDF(com_variations)
# unlink(tmp_dir)
# write.csv(clean_com_variations, "istat_com_variations.csv")


matchingCode <- function(code_a, name_a, vec_code_b, vec_name_b, tollerance = 2) {
  
  if (name_a == "") return(code_a)
  
  name_a <- gsub("Ã", "à", name_a)
  name_a <- gsub("Ã¡","á", name_a)
  name_a <- gsub("Ã¨","è", name_a)
  name_a <- gsub("Ã©","é", name_a)
  name_a <- gsub("Ã¬","ì", name_a)
  name_a <- gsub("Ã²","ò", name_a)
  name_a <- gsub("Ã³","ó", name_a)
  name_a <- gsub("Ã¹","ù", name_a)
  name_a <- gsub("Ãº","ú", name_a)
  
  name_a <- gsub("Roma Capitale", "Roma", name_a)
  
  require(stringdist)

  matching_code_i <- match(code_a, vec_code_b, nomatch = 0)
  
  if (matching_code_i == 0) {
    return(NA)
  } else {
    mean_nchar_name <- mean(c(nchar(name_a), nchar(vec_name_b[matching_code_i])))
    dist_name <- stringdist(name_a, vec_name_b[matching_code_i])
    if ((dist_name / log(mean_nchar_name)) <= tollerance) {
      return(vec_code_b[matching_code_i])
    } else {
      name_a_splt <- strsplit(name_a, split = " ")[[1]][1]
      name_b_splt <- strsplit(vec_name_b[matching_code_i], split = " ")[[1]][1]
      mean_nchar_name <- mean(c(nchar(name_a_splt), nchar(name_b_splt)))
      dist_name <- stringdist(name_a_splt, name_b_splt)
      if ((dist_name / log(mean_nchar_name)) <= tollerance) {
        return(vec_code_b[matching_code_i]) }
      else {
        return(NA) 
      }
    }
  }
}

actualiseIstatComCode <- function (vec_code_a,
                                   vec_name_a,
                                   vec_code_b, 
                                   vec_name_b,
                                   clean_com_variations) {
  
  prepareName <- function (x) {
    x <- gsub("^\\s+|\\s+$", "", x)
    return(gsub("/.*$", "", x))
  }
  
  actual_istat_code <- 
    mapply(matchingCode, vec_code_a, prepareName(vec_name_a),
           MoreArgs = list(vec_code_b = vec_code_b,
                           vec_name_b = prepareName(vec_name_b)))
  
  for (i in which(is.na(actual_istat_code))) {
    com_chg_df <- subset(clean_com_variations, 
                         cod_istat_comune == vec_code_a[i] &
                           type == 'ES')
    com_chg_df <- com_chg_df[order(com_chg_df$date, decreasing = T),]
    actual_istat_code[i] <- com_chg_df$new_cod_istat_comune[1]
  }
  return(actual_istat_code)
}