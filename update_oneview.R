update_oneview <- function(db="sp_cons_an",
         uid,pwd,
         r_table_name,
         netezza_table_name,
         odbc_connection=oneview,
         distribution_column,
         varchar_length=50,
         verbose=T,
         overwrite_temp_file=T,
         delete_file=T) {
  
  now <- Sys.time()
  
  # Checks ------------------------------------------------------------------
  if (!RODBC:::odbcValidChannel(odbc_connection))
    stop("Stage 0: first argument is not an open RODBC channel")
  
  ## Check if the temporary file folder exists
  folder_exists <- file_test("-d", file.path('/hadoop/used_by_hadoop/hadoop-kn-p2',uid))
  if(!folder_exists) {stop(paste0("Stage 0 : user folder doesn't exist in /hadoop/used_by_hadoop/hadoop-kn-p2. Run system('mkdir /hadoop/used_by_hadoop/hadoop-kn-p2/",uid,"/')"))}
  
  # Changes -----------------------------------------------------------------
  netezza_table_name <- toupper(netezza_table_name)
  
  # Do stuff ----------------------------------------------------------------
  
  # Create temporary file
  if(overwrite_temp_file) {
    cat(paste0("Stage 1: writing table to text file in /hadoop/used_by_hadoop/hadoop-kn-p2/",uid,"/\n"))
    fwrite(r_table_name,paste0("/hadoop/used_by_hadoop/hadoop-kn-p2/",uid,"/temp.csv"),quote=F,sep="|",row.names = F,col.names=F,na="")
  }
  
  # Create table in Netezza -------------------------------------------------
  # This is a copied version of sqlSave
  
  if (!RODBC:::odbcValidChannel(odbc_connection))
    stop("first argument is not an open RODBC channel")
  if (missing(r_table_name))
    stop("missing parameter")
  if (!is.data.frame(r_table_name))
    stop("should be a data frame")
  if (is.null(netezza_table_name))
    netezza_table_name <- if (length(substitute(r_table_name)) == 1)
      as.character(substitute(r_table_name))
  else as.character(substitute(r_table_name)[[2L]])
  if (length(netezza_table_name) != 1L)
    stop(sQuote(netezza_table_name), " should be a name")
  keys <- -1
  if (is.logical(rownames) && rownames)
    rownames <- "rownames"
  if (is.character(rownames)) {
    r_table_name <- cbind(row.names(r_table_name), r_table_name)
    names(r_table_name)[1L] <- rownames
    if (addPK) {
      keys <- vector("list", 4L)
      keys[[4L]] <- rownames
    }
  }
  if (is.logical(colnames) && colnames) {
    r_table_name <- as.data.frame(rbind(colnames(r_table_name), as.matrix(r_table_name)))
  }
  dbname <- RODBC:::odbcTableExists(odbc_connection, netezza_table_name, abort = FALSE)
  types <- sapply(r_table_name, class)
  isnum <- (types %in% c("integer","numeric"))
  islogi <- (types == "logical")
  isdate <- (types == "Date")
  colspecs <- rep(paste0("varchar(",varchar_length,")"), length(r_table_name))
  
  colspecs[isnum] <- "double"
  colspecs[islogi] <- "varchar(5)"
  colspecs[isdate] <- "date"
  
  names(colspecs) <- names(r_table_name)
  
  
  
  
  # NOT REQUIRED FOR TABLE UPDATE
  
  # Generate query ----------------------------------------------------------
  # query <- paste("INSERT INTO", RODBC:::quoteTabNames(odbc_connection, netezza_table_name)," (")
  # colnames <- RODBC:::quoteColNames(odbc_connection, RODBC:::mangleColNames(names(colspecs)))
  # entries <- paste(colnames)
  # query <- paste(query, paste(entries, collapse = ", "),sep = "")
  # query <- paste(query, ")", sep = "")
  # query <- toupper(query)
  # 
  # cat("Stage 3: Insert table\n")
  # 
  # if (verbose)
  #   cat("Query: ", query, "\n", sep = "")
  # res <- RODBC:::sqlQuery(odbc_connection, query, errors = FALSE)
  # if (is.numeric(res) && res == -1)
  #   stop(paste(RODBC:::odbcGetErrMsg(odbc_connection), collapse = "\n"))
  # invisible(1L)
  
  
  # Call nzload (Unix command)
  cat("Stage 3: loading text file to Netezza table\n")
  system(paste0('/usr/local/nz/bin/nzload -u ',
                uid,' -pw ',
                pwd,' -host nz-mak-kn-a1 -db ',
                db,' -t ',
                netezza_table_name,
                ' -delim "|" -df /hadoop/used_by_hadoop/hadoop-kn-p2/',uid,'/temp.csv')
         )
  
  
  if(delete_file) {file.remove(paste0("/hadoop/used_by_hadoop/hadoop-kn-p2/",uid,"/temp.csv"))}
  
  RODBC::sqlQuery(odbc_connection,paste0("generate statistics on ",db,"..",toupper(netezza_table_name),";"))
  
  dim_r_object <- dim(r_table_name)
  # dim_sql_table <- c(RODBC::sqlQuery(odbc_connection,paste0("select count(*) as COUNT from ",db,"..",netezza_table_name,";"))$COUNT,
  #                    RODBC::sqlQuery(odbc_connection,paste0("select count(*) as COUNT from _v_odbc_columns1 where table_schem = '",toupper(uid),"' and table_name = '",toupper(netezza_table_name),"'"))$COUNT)
  # 
  # cat(paste0("Dimensions of R object: ",dim_r_object[1]," rows, ",dim_r_object[2]," columns\n"))
  # cat(paste0("Dimensions of Netezza table: ",dim_sql_table[1]," rows, ",dim_sql_table[2]," columns\n"))
  
  difftime(Sys.time(),now)
}