############################################################################
############### functions/remove.variables.in.folder.R #####################
#
# Funções auxiliares utilizadas na importação dos dados.
# Elas removem do dataframe variáveis cujos nomes encontram-se em múltiplos 
# arquivos de texto dentro de uma determinada pasta passada como parâmetro.
#
############################################################################

remove_variables_in_file <- function(file.path, df)
{
  nomes <- read.table(file.path);
  indices <- which(names(df) %in% nomes$V1);
  if (length(indices) > 0)
  {
    df <- df[,-indices];
  }
  return(df);
}

remove_variables_in_folder <- function(folder.path, df)
{
  files <- list.files(path=folder.path, pattern="*.txt", full.names=T, recursive=FALSE);  
  for(file.path in files)
  {
    df <- remove_variables_in_file(file.path,df);
  }  
  return(df);
}
