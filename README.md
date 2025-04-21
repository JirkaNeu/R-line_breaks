In this script two methods are being applied for breaking up long sentences at a variable max length:

a) using base::r - sentences will be simply line broken at the nearest blank before the max length, without splitting words

b) using "sylly" library (eng and ger) - each word exceeding the max length of a line is being checked for possible hyphenation and will be split if the max length is not exceeded
