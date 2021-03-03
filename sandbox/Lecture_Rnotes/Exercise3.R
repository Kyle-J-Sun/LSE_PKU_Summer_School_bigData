fileName = "communityCrimeData.csv"
defaultDataDir = "/Users/kyle/Documents/LSE-PKU/tutorial_class/data"
fileLocation = file.path(defaultDataDir, fileName)
CC = read.csv(file = fileLocation, header = F)
