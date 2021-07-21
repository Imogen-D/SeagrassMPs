#Creatinglongframe
files <- list.files(path = "./Datasheets/", pattern = "*.csv", full.names = T)
samples <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")
samples <- samples[-1]
#changing katsadia to D
samples$Site <- str_replace(samples$Site, "Kat", "D")
samples$Type[which(samples$Type == "Fibres")] <- "Fibre"
samples$Type[which(samples$Type == "Fragments")] <- "Fragment"
loc <- str_sub(samples$Site, start = 1, end = 1)
samples$loc <- loc
samples$Size <- samples$Size/100

#remove FOR NOW JUST ALL ORANGE FRAGMENTS
samples$ColTyp <- paste(samples$Colour,samples$Type)
samples <- samples[samples$ColTyp != "Orange Fragment",]
samples <- samples[,-6]

#Change all other colours to other
others <- c("Brown", "Grey", "Orange", "Pink", "Purple", "Yellow", "Multicoloured")
samples$Colour[which(samples$Colour %in% others)] <- "Other"
samples$Colour[which(samples$Colour == "Clear")] <- "Transparent"

samples$x <- paste("SG", samples$Site)
samples$p <- paste("SG", samples$loc)


write.csv(samples, file = "samples.csv")


sedimentsamples <- read.delim("sediment.csv", sep = ";", col.names = c("loc", "Coll", "Type", "Size", "Colour"))
sedimentsamples <- sedimentsamples[30:203,] #remove blank
sedimentsamples$Size <- as.numeric(str_replace(sedimentsamples$Size, ",", "."))

sedimentsamples$loc <- str_replace(sedimentsamples$loc, "Katsadia", "D")
sedimentsamples$loc <- str_replace(sedimentsamples$loc, "Vroulia", "V")
sedimentsamples$loc <- str_replace(sedimentsamples$loc, "Kampos", "K")
sedimentsamples$loc <- str_replace(sedimentsamples$loc, "Platis", "P")
sedimentsamples$loc <- str_replace(sedimentsamples$loc, "Xerokampos", "X")
sedimentsamples$loc <- str_remove(sedimentsamples$loc, " ")
sedimentsamples$Site <- paste(sedimentsamples$loc, sedimentsamples$Coll, sep = "")


#need to change names of types
sedimentsamples$Type <- str_replace(sedimentsamples$Type, "fiber ", "fiber")
sedimentsamples$Type <- str_replace(sedimentsamples$Type, "fragment ", "fragment")
sedimentsamples$Type <- str_replace(sedimentsamples$Type, "fiber", "Fibre")
sedimentsamples$Type <- str_replace(sedimentsamples$Type, "fragment", "Fragment")

#need to change names of Colours -> move this into actual table to clean up code
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour, "transparent ", "transparent")
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour, "transparent", "Transparent")
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour, "blue ", "blue")
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour, "blue", "Blue")
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour, "black ", "black")
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour, "black", "Black")
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour, "red ", "red")
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour, "red", "Red")
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour,"white ", "white")
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour, "white", "White")
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour, "green ", "green")
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour, "green", "Green")
sedimentsamples$Colour <- str_replace(sedimentsamples$Colour,"orange", "Other")

sedimentsamples$x <- paste("Sediment", sedimentsamples$Site)
sedimentsamples$p <- paste("Sediment", sedimentsamples$loc)


#order samples so all same now 1, 2, A, B
sedimentsamples <- sedimentsamples[order(sedimentsamples$Site),]


write.csv(sedimentsamples, file = "sedimentsamples.csv")

