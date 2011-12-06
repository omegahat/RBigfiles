bigfiles =
  #
  # a = bigfiles("~/Projects/org/omegahat/R", maxDepth = 3, excludePattern = "(tar.gz|tu)$")
  #
  # This version does each directory separately in R.
  # file.info() does it in C.
  #
function(dir = ".", recursive = TRUE, num = 50, maxDepth = 10,
         excludePattern = character(),
          files = list.files(dir, full.names = TRUE))
{
    # remove any trailing / or \\ from the dir.
  dir = gsub(sprintf("%s+$", .Platform$file.sep), "", path.expand(dir))
  
  ans = file.info(files)

  warn = character()  # for collecting the names of the directories we don't process
                      # because of depth restrictions.

  applyExclude =
   function(info) {
      if(length(excludePattern)) {
         j = grep(excludePattern, rownames(info))
         if(length(j))
           return(info[-j,])
      }

      info
   }

  
  ans = applyExclude(ans)
  dirs = rownames(ans)[ans$isdir]  # the directories in the top-level collection of files.
  ans = ans[!ans$isdir, ]
  
  processDir =
      function(d, depth = 0) {

         i = file.info(list.files(d, full.names = TRUE))
         i = applyExclude(i)
         
         tmp = rbind(ans, i[!i$isdir, ])

            # if we don't have to sort to drop some entries, don't.
         if(nrow(tmp) > num)
           tmp = tmp[order(tmp$size, decreasing = TRUE)[1:num], ]

         ans <<- tmp

         if(depth < maxDepth) {
           lapply(rownames(i)[i$isdir], processDir, depth + 1)
         } else if(sum(i$isdir))
           warn <<- c(warn, d)
      }

  if(recursive) {
    lapply(dirs, processDir, 0)
  }

  if(length(warn))
    warning("due to depth restrictions, didn't process ", length(warn), " directories: ", paste(warn, collapse = ", "))

  ans = ans[order(ans$size, decreasing = TRUE)[seq(length = min(num, nrow(ans)))], ]

  rownames(ans) = gsub(sprintf("^%s%s", dir, .Platform$file.sep), "", rownames(ans))
  ans
}

abigfiles =
function(dir = ".", recursive = TRUE, num = 50, maxDepth = 10,
         excludePattern = character(),
         files = list.files(dir, full.names = TRUE))
{
# if(!missing(files)) {
#    stop("not implemented yet")
# } else 
    ans = file.info(files)

  if(length(excludePattern)) {
    i = grep(excludePattern, rownames(ans))
    if(length(i))
      ans = ans[-i]
  }

  if(is.finite(maxDepth)) {
     len = sapply(strsplit(rownames(ans), .Platform$file.sep), length)
     ans = ans[ len <= maxDepth, ]
  }

  ans = ans[seq(length = min(num, nrow(ans))), ]
  ans[  order(ans$size, decreasing = TRUE), ]
}


########################################################################################

getExtension =
  #
  # Extract the extension of the file name or an NA if there is no extension.
  #
  # ans = bigfiles(R.home())
  # sort(table(getExtension(rownames(ans))), decreasing = TRUE)
  #
function(files)
{
  hasNoExt = grep("\\.", files, invert = TRUE)
  ans = gsub(".*\\.([^.]+)$", "\\1", files)

  if(length(hasNoExt))
     ans[hasNoExt] = NA
  
  ans
}
