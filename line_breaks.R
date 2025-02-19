
# https://en.wikipedia.org/wiki/Moby-Dick
my_string = "When Ahab finally appears on the quarterdeck, he announces he seeks revenge on the white whale that took his leg from the knee down, leaving him with a prosthesis fashioned from a whale's jawbone. Ahab will give the first man to sight Moby Dick a doubloon, which he nails to the mast. Starbuck objects that he has not come for vengeance but for profit, but Ahab's purpose exercises a mysterious spell on Ishmael: Ahab's quenchless feud seemed mine."
max_length = 85 #--> set max length for line breaks

for (i in 1:2){
  max_length = round(max_length/i)
  all_length = nchar(my_string)
  big_word = max(nchar(strsplit(my_string, " ")[[1]]))
  x_test = big_word
  x_start = 1
  space_last = 0
  result_string = c()
  
  if (all_length > max_length & max_length > big_word){
    repeat{
      x_start = x_start + space_last
      x_end = x_start + max_length
      teilstring = substr(my_string, x_start, x_end)
      if(x_end > all_length) {Teilsatz = teilstring; result_string = c(result_string, Teilsatz); break} #--> stopp when end of my_string
      space_last = max(which(strsplit(teilstring, "")[[1]]==" ")) #--> last blank in $teilstring
      Teilsatz = substr(teilstring, 1, space_last - 1) #--> last blank out
      result_string = c(result_string, Teilsatz)
      if (x_test > all_length){break}; x_test = x_test + 1 #--> security system
    }
    result_string = paste(result_string, collapse = "\n")
  }else result_string = paste0("- value to small, nothing changed -\n", my_string)

  cat(paste0("\nmax_length: ", max_length, "\n-----------\n", result_string, "\n\n"))
}
