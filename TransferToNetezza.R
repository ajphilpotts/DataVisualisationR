# Wrapper function to transfer local R files back into NZ table

TransferToNetezza <- function(file,targetDB,targetTable,distColumn,uid,pwd,delim='|')
{
  
  require("virgin.media.functions",lib.loc = "/R/home/b7800340/R/x86_64-pc-linux-gnu-library/3.3")
  require(readr)
  
  loadDB <- connect_to_netezza(toupper(targetDB),uid,pwd)
  
  #sqlbase <- 'SELECT * FROM ACQ_MODEL_MIGRATE_'
  
  sourceFile <- read_delim(file,delim=delim)
  
  load_to_netezza(db=targetDB
                  ,uid=uid
                  ,pwd=pwd
                  ,r_table_name = sourceFile
                  ,netezza_table_name = toupper(targetTable)
                  ,odbc_connection = loadDB
                  ,distribution_column = distColumn
                  ,varchar_length=50
                  ,verbose=T
                  ,overwrite_temp_file=T
                  ,delete_file=T
                  ,drop_table=T)
  
}
