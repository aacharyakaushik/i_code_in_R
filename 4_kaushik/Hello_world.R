
# code to accept arguments from PBS Script
args = commandArgs(trailingOnly = TRUE)

if (length(args) == 0)
{
   print("At least one argument is required")
}

# using sprintf to print mtext and variable in a line
sprintf("Hello World!!! SCript in Aeolus executed successfully!! HURRAY!!! %s",args[1])

# another method
hello <- "Hello World!!! SCript in Aeolus executed successfully!! HURRAY!!!"
paste(hello,args[1])

# Print system time - %X is to print only time
format(Sys.time(),"%X")

# system sleep
Sys.sleep(10)

print("We are 10s late!!!")

