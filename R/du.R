du =
function(dir)
{
  dir = gsub(" ", '\\\\ ', dir)
  txt = system(sprintf('du -hs %s/*', dir), intern = TRUE)
  du = read.table(con <- textConnection(txt), header = FALSE)
  close(con)
  names(du) = c("size", "file")
  du$isdir = file.info(as.character(du$file))$isdir

  du$factor = ordered(gsub("[0-9.]+", "", as.character(du[,1])), levels = c("K", "M", "G"))
  du$num = as.numeric(gsub("[KMG]+", "", as.character(du[,1])))
  du$size = 1000^as.integer(du$factor) * du$num
  du[order(du$size, decreasing = TRUE), ]
}
