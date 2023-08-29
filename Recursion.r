mDecRecursive <- function(s, depth = 0) {
  indentation <- strrep(" ", depth * 2)
  
  if (nchar(s) == 1) {
    cat(paste(indentation, "Mdec(<", s, ">) =", s, "\n"))
    as.integer(s)
  } else {
    prefix <- substr(s, 1, nchar(s) - 1)
    lastDigit <- substr(s, nchar(s), nchar(s))

    cat(paste(indentation, "Mdec(<", prefix, ">'", lastDigit, "') = 10 * Mdec(<", prefix, ">) +", lastDigit, "\n"))
    
    subResult <- mDecRecursive(prefix, depth + 1)
    result <- 10 * subResult + as.integer(lastDigit)
    
    cat(paste(indentation, "Mdec(<", prefix, ">'", lastDigit, "') =", result, "\n"))
    result
  }
}

number <- readline(prompt="Digite um número: ")
result <- mDecRecursive(number)
cat(paste("Final result:", result, "\n"))
