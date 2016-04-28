data <- read.facereader("/Users/dalbohn/Documents/School Work/facereadeR/data/", 6)

#data$analysis <- make.analysis.id(data)
#data$condition <- make.condition.id(data)
#data$subject_id <- make.sub.id(data)

data <- clean.data(data, include = "Basic")

data <- rename.aus(data)
data <- score.aus(data)
