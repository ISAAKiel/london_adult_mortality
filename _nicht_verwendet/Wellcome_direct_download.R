path <- "https://www.museumoflondon.org.uk/download_file/view/3226/735"
chelsea <- read.table(path, header =TRUE, sep = "|")
colnames(chelsea) <- colnames(chelsea)[-7] # there is one "SITECODE" too much
chelsea <- chelsea[which(chelsea$GROUP == "Overall age code"),-14]
chelsea %>% group_by(AGE) %>% summarize(n = n())


path <- "https://www.museumoflondon.org.uk/download_file/view/4419/735"
st_benet <- read.table(path, header =TRUE, sep = "|")
colnames(st_benet) <- colnames(st_benet)[-7] # there is one "SITECODE" too much
st_benet <- st_benet[which(st_benet$GROUP == "Overall age code"),-14]
st_benet %>% group_by(AGE) %>% summarize(n = n())


path <- "https://www.museumoflondon.org.uk/download_file/view/3255/735"
st_bride_lower <- read.table(path, header =TRUE, sep = "|")
colnames(st_bride_lower) <- colnames(st_bride_lower)[-7] # there is one "SITECODE" too much
st_bride_lower <- st_bride_lower[which(st_bride_lower$GROUP == "Overall age code"),-14]
st_bride_lower %>% group_by(AGE) %>% summarize(n = n())


# unsuitable because mainly females
path <- "https://www.museumoflondon.org.uk/download_file/view/3296/735"
cross_bones <- read.table(path, header =TRUE, sep = "|")
colnames(cross_bones) <- colnames(cross_bones)[-7] # there is one "SITECODE" too much
cross_bones <- cross_bones[which(cross_bones$GROUP == "Overall age code"),-14]
cross_bones %>% group_by(SEX, AGE) %>% summarize(n = n()) %>% as.data.frame(.)


########
# Medieval
# too small
path <- "https://www.museumoflondon.org.uk/download_file/view/3039/720"
guildhall <- read.table(path, header =TRUE, sep = "|")
colnames(guildhall) <- colnames(guildhall)[-7] # there is one "SITECODE" too much
guildhall <- guildhall[which(guildhall$GROUP == "Overall age code"),-14]
guildhall %>% group_by(AGE) %>% summarize(n = n()) %>% as.data.frame(.)

path <- "https://www.museumoflondon.org.uk/download_file/view/3072/720"
east_smithfield <- read.table(path, header =TRUE, sep = "|")
colnames(east_smithfield) <- colnames(east_smithfield)[-7] # there is one "SITECODE" too much
east_smithfield <- east_smithfield[which(east_smithfield$GROUP == "Overall age code"),-14]
east_smithfield %>% group_by(AGE) %>% summarize(n = n()) %>% as.data.frame(.)


path <- "https://www.museumoflondon.org.uk/download_file/view/3124/720"
st_mary_graces <- read.table(path, header =TRUE, sep = "|")
colnames(st_mary_graces) <- colnames(st_mary_graces)[-7] # there is one "SITECODE" too much
st_mary_graces <- st_mary_graces[which(st_mary_graces$GROUP == "Overall age code"),-14]
st_mary_graces %>% group_by(AGE) %>% summarize(n = n()) %>% as.data.frame(.)


path <- "https://www.museumoflondon.org.uk/download_file/view/3142/720"
merton_priory <- read.table(path, header =TRUE, sep = "|")
colnames(merton_priory) <- colnames(merton_priory)[-7] # there is one "SITECODE" too much
merton_priory <- merton_priory[which(merton_priory$GROUP == "Overall age code"),-14]
merton_priory %>% group_by(PERIOD, AGE) %>% summarize(n = n()) %>% as.data.frame(.)

# very unlikely death distribution
path <- "https://www.museumoflondon.org.uk/download_file/view/3159/720"
spital_square <- read.table(path, header =TRUE, sep = "|")
colnames(spital_square) <- colnames(spital_square)[-7] # there is one "SITECODE" too much
spital_square <- spital_square[which(spital_square$GROUP == "Overall age code"),-14]
spital_square %>% group_by(AGE) %>% summarize(n = n()) %>% as.data.frame(.)
