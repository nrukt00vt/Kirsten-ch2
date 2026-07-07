library(sf)
library(tidyr)
shapefile = counties
shapefile = subset(shapefile, NAME_0 == "United States")

shapefile_all = shapefile[0,]
shapefile_state = subset(shapefile,NAME_1 == "Arkansas")

data_file = read.csv("Poultry State CSV/Arkansas.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:77)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Hot.Spring")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Hot.Spring")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Little.River")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Little.River")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "St..Francis")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "St..Francis")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "Van.Buren")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "Van.Buren")]
shapefile_all = rbind(shapefile_all,shapefile_state)
shapefile_state_alabama = shapefile_state
shapefile_state2 = subset(shapefile,NAME_1 == "Arizona")

data_file2 = read.csv("Poultry State CSV/Arizona.csv")
data_file2 = data_file2[-1,]
data_file2 = data_file2[1,]

data_file_long2 = pivot_longer(data_file2, cols = 2:17)

is.element(data_file_long2$name, shapefile_state2$NAME_2)

print(data_file_long2[!is.element(data_file_long2$name, shapefile_state2$NAME_2),],n= 70)


shapefile_state2 = merge(shapefile_state2,data_file_long2, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state2[which(is.na(shapefile_state2$value)),]
missing_names2 = shapefile_state2$NAME_2[which(is.na(shapefile_state2$value))]
print(missing_names2)

missing_rows2 = which(is.na(shapefile_state2$value))

shapefile_state2$Item[missing_rows2[1]] = data_file_long2$Item[which(data_file_long2$name == "La..Paz")]
shapefile_state2$value[missing_rows2[1]] = data_file_long2$value[which(data_file_long2$name == "La..Paz")]

shapefile_state2$Item[missing_rows2[2]] = data_file_long2$Item[which(data_file_long2$name == "Santa..Cruz")]
shapefile_state2$value[missing_rows2[2]] = data_file_long2$value[which(data_file_long2$name == "Santa..Cruz")]

shapefile_all = rbind(shapefile_all,shapefile_state2); shapefile_state_arizona = shapefile_state2

shapefile_state = subset(shapefile,NAME_1 == "California")

data_file = read.csv("Poultry State CSV/California.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:60)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Contra.Costa")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Contra.Costa")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Del.Norte")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Del.Norte")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "El.Dorado")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "El.Dorado")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "Los.Angeles")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "Los.Angeles")]

shapefile_state$Item[missing_rows[5]] = data_file_long$Item[which(data_file_long$name == "San.Benito")]
shapefile_state$value[missing_rows[5]] = data_file_long$value[which(data_file_long$name == "San.Benito")]

shapefile_state$Item[missing_rows[6]] = data_file_long$Item[which(data_file_long$name == "San.Bernardino")]
shapefile_state$value[missing_rows[6]] = data_file_long$value[which(data_file_long$name == "San.Bernardino")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "San.Diego")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "San.Diego")]

shapefile_state$Item[missing_rows[8]] = data_file_long$Item[which(data_file_long$name == "San.Francisco")]
shapefile_state$value[missing_rows[8]] = data_file_long$value[which(data_file_long$name == "San.Francisco")]

shapefile_state$Item[missing_rows[9]] = data_file_long$Item[which(data_file_long$name == "San.Joaquin")]
shapefile_state$value[missing_rows[9]] = data_file_long$value[which(data_file_long$name == "San.Joaquin")]

shapefile_state$Item[missing_rows[10]] = data_file_long$Item[which(data_file_long$name == "San.Luis.Obispo")]
shapefile_state$value[missing_rows[10]] = data_file_long$value[which(data_file_long$name == "San.Luis.Obispo")]

shapefile_state$Item[missing_rows[11]] = data_file_long$Item[which(data_file_long$name == "San.Mateo")]
shapefile_state$value[missing_rows[11]] = data_file_long$value[which(data_file_long$name == "San.Mateo")]

shapefile_state$Item[missing_rows[12]] = data_file_long$Item[which(data_file_long$name == "Santa.Barbara")]
shapefile_state$value[missing_rows[12]] = data_file_long$value[which(data_file_long$name == "Santa.Barbara")]

shapefile_state$Item[missing_rows[13]] = data_file_long$Item[which(data_file_long$name == "Santa.Clara")]
shapefile_state$value[missing_rows[13]] = data_file_long$value[which(data_file_long$name == "Santa.Clara")]

shapefile_state$Item[missing_rows[14]] = data_file_long$Item[which(data_file_long$name == "Santa.Cruz")]
shapefile_state$value[missing_rows[14]] = data_file_long$value[which(data_file_long$name == "Santa.Cruz")]


shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_california = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Colorado")

data_file = read.csv("Poultry State CSV/Colorado.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:66)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Clear.Creek")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Clear.Creek")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "El.Paso")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "El.Paso")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "Kit.Carson")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "Kit.Carson")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "La.Plata")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "La.Plata")]

shapefile_state$Item[missing_rows[5]] = data_file_long$Item[which(data_file_long$name == "Las.Animas")]
shapefile_state$value[missing_rows[5]] = data_file_long$value[which(data_file_long$name == "Las.Animas")]

shapefile_state$Item[missing_rows[6]] = data_file_long$Item[which(data_file_long$name == "Rio.Blanco")]
shapefile_state$value[missing_rows[6]] = data_file_long$value[which(data_file_long$name == "Rio.Blanco")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "Rio.Grande")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "Rio.Grande")]

shapefile_state$Item[missing_rows[8]] = data_file_long$Item[which(data_file_long$name == "San.Juan")]
shapefile_state$value[missing_rows[8]] = data_file_long$value[which(data_file_long$name == "San.Juan")]

shapefile_state$Item[missing_rows[9]] = data_file_long$Item[which(data_file_long$name == "San.Miguel")]
shapefile_state$value[missing_rows[9]] = data_file_long$value[which(data_file_long$name == "San.Miguel")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_colorado = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Connecticut")

data_file = read.csv("Poultry State CSV/Connecticut.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:10)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "New.Haven")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "New.Haven")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "New.London")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "New.London")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_connecticut = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Delaware")

data_file = read.csv("Poultry State CSV/Delaware.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:5)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "New.Castle")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "New.Castle")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_delaware = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Florida")

data_file = read.csv("Poultry State CSV/Florida.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:69)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "DeSoto")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "DeSoto")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Indian.River")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Indian.River")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "Miami.Dade")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "Miami.Dade")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "Palm.Beach")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "Palm.Beach")]

shapefile_state$Item[missing_rows[5]] = data_file_long$Item[which(data_file_long$name == "St..Johns")]
shapefile_state$value[missing_rows[5]] = data_file_long$value[which(data_file_long$name == "St..Johns")]

shapefile_state$Item[missing_rows[6]] = data_file_long$Item[which(data_file_long$name == "St..Lucie")]
shapefile_state$value[missing_rows[6]] = data_file_long$value[which(data_file_long$name == "St..Lucie")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "Santa.Rosa")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "Santa.Rosa")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_florida = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Georgia")

data_file = read.csv("Poultry State CSV/Georgia.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:161)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Ben.Hill")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Ben.Hill")]

shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Jeff.Davis")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Jeff.Davis")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_georgia = shapefile_state

#Missing Kalawao
shapefile_state = subset(shapefile,NAME_1 == "Hawaii")

data_file = read.csv("Poultry State CSV/Hawaii.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:6)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state = subset(shapefile,NAME_1 == "Idaho")

data_file = read.csv("Poultry State CSV/Idaho.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:46)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))

shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Bear.Lake")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Bear.Lake")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Nez.Perce")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Nez.Perce")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "Twin.Falls")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "Twin.Falls")]

shapefile_state = subset(shapefile,NAME_1 == "Illinois")

data_file = read.csv("Poultry State CSV/Illinois.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:104)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "De.Kalb")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "De.Kalb")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "De.Witt")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "De.Witt")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "Du.Page")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "Du.Page")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "Jo.Daviess")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "Jo.Daviess")]

shapefile_state$Item[missing_rows[5]] = data_file_long$Item[which(data_file_long$name == "La.Salle")]
shapefile_state$value[missing_rows[5]] = data_file_long$value[which(data_file_long$name == "La.Salle")]

shapefile_state$Item[missing_rows[6]] = data_file_long$Item[which(data_file_long$name == "Rock.Island")]
shapefile_state$value[missing_rows[6]] = data_file_long$value[which(data_file_long$name == "Rock.Island")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "St..Clair")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "St..Clair")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_illinois = shapefile_state

#Missing Lake Michigan
shapefile_state = subset(shapefile,NAME_1 == "Indiana")

data_file = read.csv("Poultry State CSV/Indiana.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:94)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "DeKalb")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "DeKalb")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "St..Joseph")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "St..Joseph")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_indiana = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Iowa")

data_file = read.csv("Poultry State CSV/Iowa.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:101)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Black.Hawk")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Black.Hawk")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Buena.Vista")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Buena.Vista")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "Cerro.Gordo")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "Cerro.Gordo")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "Des.Moines")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "Des.Moines")]

shapefile_state$Item[missing_rows[5]] = data_file_long$Item[which(data_file_long$name == "O.Brien")]
shapefile_state$value[missing_rows[5]] = data_file_long$value[which(data_file_long$name == "O.Brien")]

shapefile_state$Item[missing_rows[6]] = data_file_long$Item[which(data_file_long$name == "Palo.Alto")]
shapefile_state$value[missing_rows[6]] = data_file_long$value[which(data_file_long$name == "Palo.Alto")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "Van.Buren")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "Van.Buren")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_iowa = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Louisiana")

data_file = read.csv("Poultry State CSV/Louisiana.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:66)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "De.Soto")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "De.Soto")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "East.Baton.Rouge")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "East.Baton.Rouge")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "East.Carroll")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "East.Carroll")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "East.Feliciana")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "East.Feliciana")]

shapefile_state$Item[missing_rows[5]] = data_file_long$Item[which(data_file_long$name == "Jefferson.Davis")]
shapefile_state$value[missing_rows[5]] = data_file_long$value[which(data_file_long$name == "Jefferson.Davis")]

shapefile_state$Item[missing_rows[6]] = data_file_long$Item[which(data_file_long$name == "LaSalle")]
shapefile_state$value[missing_rows[6]] = data_file_long$value[which(data_file_long$name == "LaSalle")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "Pointe.Coupee")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "Pointe.Coupee")]

shapefile_state$Item[missing_rows[8]] = data_file_long$Item[which(data_file_long$name == "Red.River")]
shapefile_state$value[missing_rows[8]] = data_file_long$value[which(data_file_long$name == "Red.River")]

shapefile_state$Item[missing_rows[9]] = data_file_long$Item[which(data_file_long$name == "St..Bernard")]
shapefile_state$value[missing_rows[9]] = data_file_long$value[which(data_file_long$name == "St..Bernard")]

shapefile_state$Item[missing_rows[10]] = data_file_long$Item[which(data_file_long$name == "St..Charles")]
shapefile_state$value[missing_rows[10]] = data_file_long$value[which(data_file_long$name == "St..Charles")]

shapefile_state$Item[missing_rows[11]] = data_file_long$Item[which(data_file_long$name == "St..Helena")]
shapefile_state$value[missing_rows[11]] = data_file_long$value[which(data_file_long$name == "St..Helena")]

shapefile_state$Item[missing_rows[12]] = data_file_long$Item[which(data_file_long$name == "St..James")]
shapefile_state$value[missing_rows[12]] = data_file_long$value[which(data_file_long$name == "St..James")]

shapefile_state$Item[missing_rows[13]] = data_file_long$Item[which(data_file_long$name == "St..John.the.Baptist")]
shapefile_state$value[missing_rows[13]] = data_file_long$value[which(data_file_long$name == "St..John.the.Baptist")]

shapefile_state$Item[missing_rows[14]] = data_file_long$Item[which(data_file_long$name == "St..Landry")]
shapefile_state$value[missing_rows[14]] = data_file_long$value[which(data_file_long$name == "St..Landry")]

shapefile_state$Item[missing_rows[15]] = data_file_long$Item[which(data_file_long$name == "St..Martin")]
shapefile_state$value[missing_rows[15]] = data_file_long$value[which(data_file_long$name == "St..Martin")]

shapefile_state$Item[missing_rows[16]] = data_file_long$Item[which(data_file_long$name == "St..Mary")]
shapefile_state$value[missing_rows[16]] = data_file_long$value[which(data_file_long$name == "St..Mary")]

shapefile_state$Item[missing_rows[17]] = data_file_long$Item[which(data_file_long$name == "St..Tammany")]
shapefile_state$value[missing_rows[17]] = data_file_long$value[which(data_file_long$name == "St..Tammany")]

shapefile_state$Item[missing_rows[18]] = data_file_long$Item[which(data_file_long$name == "West.Baton.Rouge")]
shapefile_state$value[missing_rows[18]] = data_file_long$value[which(data_file_long$name == "West.Baton.Rouge")]

shapefile_state$Item[missing_rows[19]] = data_file_long$Item[which(data_file_long$name == "West.Carroll")]
shapefile_state$value[missing_rows[19]] = data_file_long$value[which(data_file_long$name == "West.Carroll")]

shapefile_state$Item[missing_rows[20]] = data_file_long$Item[which(data_file_long$name == "West.Feliciana")]
shapefile_state$value[missing_rows[21]] = data_file_long$value[which(data_file_long$name == "West.Feliciana")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_Louisiana = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Maryland")

data_file = read.csv("Poultry State CSV/Maryland.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:25)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Anne.Arundel")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Anne.Arundel")]

shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Prince.George.s")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Prince.George.s")]

shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Queen.Anne.s")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Queen.Anne.s")]

shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "St..Mary.s")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "St..Mary.s")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_maryland = shapefile_state

#Missing Lake Hurron, Lake Michigan, Lake St. Clair, Lake Superior
shapefile_state = subset(shapefile,NAME_1 == "Michigan")

data_file = read.csv("Poultry State CSV/Michigan.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:85)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Grand.Traverse")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Grand.Traverse")]

shapefile_state$Item[missing_rows[6]] = data_file_long$Item[which(data_file_long$name == "Presque.Isle")]
shapefile_state$value[missing_rows[6]] = data_file_long$value[which(data_file_long$name == "Presque.Isle")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "St..Clair")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "St..Clair")]

shapefile_state$Item[missing_rows[8]] = data_file_long$Item[which(data_file_long$name == "St..Joseph")]
shapefile_state$value[missing_rows[8]] = data_file_long$value[which(data_file_long$name == "St..Joseph")]

shapefile_state$Item[missing_rows[9]] = data_file_long$Item[which(data_file_long$name == "Van.Buren")]
shapefile_state$value[missing_rows[9]] = data_file_long$value[which(data_file_long$name == "Van.Buren")]

#Missing Lake Superior
shapefile_state = subset(shapefile,NAME_1 == "Minnesota")

data_file = read.csv("Poultry State CSV/Minnesota.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:89)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Big.Stone")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Big.Stone")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Blue.Earth")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Blue.Earth")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "Crow.Wing")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "Crow.Wing")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "Lac.qui.Parle")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "Lac.qui.Parle")]

shapefile_state$Item[missing_rows[5]] = data_file_long$Item[which(data_file_long$name == "Lake.of.the.Woods")]
shapefile_state$value[missing_rows[5]] = data_file_long$value[which(data_file_long$name == "Lake.of.the.Woods")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "Le.Sueur")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "Le.Sueur")]

shapefile_state$Item[missing_rows[8]] = data_file_long$Item[which(data_file_long$name == "Mille.Lacs")]
shapefile_state$value[missing_rows[8]] = data_file_long$value[which(data_file_long$name == "Mille.Lacs")]

shapefile_state$Item[missing_rows[9]] = data_file_long$Item[which(data_file_long$name == "Otter.Tail")]
shapefile_state$value[missing_rows[9]] = data_file_long$value[which(data_file_long$name == "Otter.Tail")]

shapefile_state$Item[missing_rows[10]] = data_file_long$Item[which(data_file_long$name == "Red.Lake")]
shapefile_state$value[missing_rows[10]] = data_file_long$value[which(data_file_long$name == "Red.Lake")]

shapefile_state$Item[missing_rows[11]] = data_file_long$Item[which(data_file_long$name == "St..Louis")]
shapefile_state$value[missing_rows[11]] = data_file_long$value[which(data_file_long$name == "St..Louis")]

shapefile_state$Item[missing_rows[12]] = data_file_long$Item[which(data_file_long$name == "Yellow.Medicine")]
shapefile_state$value[missing_rows[12]] = data_file_long$value[which(data_file_long$name == "Yellow.Medicine")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_minnesota = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Mississippi")

data_file = read.csv("Poultry State CSV/Mississippi.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:84)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "DeSoto")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "DeSoto")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Jefferson.Davis")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Jefferson.Davis")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "Pearl.River")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "Pearl.River")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_mississippi = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Missouri")

data_file = read.csv("Poultry State CSV/Missouri.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:116)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Cape.Girardeau")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Cape.Girardeau")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "DeKalb")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "DeKalb")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "New.Madrid")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "New.Madrid")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "St..Charles")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "St..Charles")]

shapefile_state$Item[missing_rows[5]] = data_file_long$Item[which(data_file_long$name == "St..Clair")]
shapefile_state$value[missing_rows[5]] = data_file_long$value[which(data_file_long$name == "St..Clair")]

shapefile_state$Item[missing_rows[6]] = data_file_long$Item[which(data_file_long$name == "Ste..Genevieve")]
shapefile_state$value[missing_rows[6]] = data_file_long$value[which(data_file_long$name == "Ste..Genevieve")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "St..Francois")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "St..Francois")]

shapefile_state$Item[missing_rows[8]] = data_file_long$Item[which(data_file_long$name == "St..Louis")]
shapefile_state$value[missing_rows[8]] = data_file_long$value[which(data_file_long$name == "St..Louis")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_missouri = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Montana")

data_file = read.csv("Poultry State CSV/Montana.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:58)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Big.Horn")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Big.Horn")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Deer.Lodge")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Deer.Lodge")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "Golden.Valley")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "Golden.Valley")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "Judith.Basin")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "Judith.Basin")]

shapefile_state$Item[missing_rows[5]] = data_file_long$Item[which(data_file_long$name == "Lewis.and.Clark")]
shapefile_state$value[missing_rows[5]] = data_file_long$value[which(data_file_long$name == "Lewis.and.Clark")]

shapefile_state$Item[missing_rows[6]] = data_file_long$Item[which(data_file_long$name == "Powder.River")]
shapefile_state$value[missing_rows[6]] = data_file_long$value[which(data_file_long$name == "Powder.River")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "Silver.Bow")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "Silver.Bow")]

shapefile_state$Item[missing_rows[8]] = data_file_long$Item[which(data_file_long$name == "Sweet.Grass")]
shapefile_state$value[missing_rows[8]] = data_file_long$value[which(data_file_long$name == "Sweet.Grass")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_montana = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Nebraska")

data_file = read.csv("Poultry State CSV/Nebraska.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:95)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Box.Butte")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Box.Butte")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Keya.Paha")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Keya.Paha")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "Red.Willow")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "Red.Willow")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "Scotts.Bluff")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "Scotts.Bluff")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_nebraska = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Nevada")

data_file = read.csv("Poultry State CSV/Nevada.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:19)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "White.Pine")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "White.Pine")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Carson.City")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Carson.City")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_nevada = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "New Jersey")

data_file = read.csv("Poultry State CSV/New Jersey.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:23)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Cape.May")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Cape.May")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_new_jersey = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "New Mexico")

data_file = read.csv("Poultry State CSV/New Mexico.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:35)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "De.Baca")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "De.Baca")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Dona.Ana")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Dona.Ana")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "Los.Alamos")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "Los.Alamos")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "Rio.Arriba")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "Rio.Arriba")]

shapefile_state$Item[missing_rows[5]] = data_file_long$Item[which(data_file_long$name == "San.Juan")]
shapefile_state$value[missing_rows[5]] = data_file_long$value[which(data_file_long$name == "San.Juan")]

shapefile_state$Item[missing_rows[6]] = data_file_long$Item[which(data_file_long$name == "San.Miguel")]
shapefile_state$value[missing_rows[6]] = data_file_long$value[which(data_file_long$name == "San.Miguel")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "Santa.Fe")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "Santa.Fe")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_new_mexico = shapefile_state

#Missing Lake Ontario
shapefile_state = subset(shapefile,NAME_1 == "New York")

data_file = read.csv("Poultry State CSV/New York.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:64)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "New.York.1")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "New.York.1")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "St..Lawrence")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "St..Lawrence")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_new_york = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "North Carolina")

data_file = read.csv("Poultry State CSV/North Carolina.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:102)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "New.Hanover")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "New.Hanover")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_north_carolina = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "North Dakota")

data_file = read.csv("Poultry State CSV/North Dakota.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:55)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Golden.Valley")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Golden.Valley")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Grand.Forks")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Grand.Forks")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "LaMoure")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "LaMoure")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_north_dakota = shapefile_state

#Missing Lake Erie
shapefile_state = subset(shapefile,NAME_1 == "Ohio")

data_file = read.csv("Poultry State CSV/Ohio.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:90)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Van.Wert")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Van.Wert")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_ohio = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Oklahoma")

data_file = read.csv("Poultry State CSV/Oklahoma.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:79)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Le.Flore")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Le.Flore")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Roger.Mills")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Roger.Mills")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_oklahoma = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Oregon")

data_file = read.csv("Poultry State CSV/Oregon.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:39)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Hood.River")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Hood.River")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_oregon = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Pennsylvania")

data_file = read.csv("Poultry State CSV/Pennsylvania.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:69)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "McKean")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "McKean")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_pennsylvania = shapefile_state

#Missing Shannon
shapefile_state = subset(shapefile,NAME_1 == "South Dakota")

data_file = read.csv("Poultry State CSV/South Dakota.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:68)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Bon.Homme")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Bon.Homme")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Charles.Mix")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Charles.Mix")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "Fall.River")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "Fall.River")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_south_dakota = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Tennessee")

data_file = read.csv("Poultry State CSV/Tennessee.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:97)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Van.Buren")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Van.Buren")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_tennessee = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Texas")

data_file = read.csv("Poultry State CSV/Texas.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:257)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Deaf.Smith")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Deaf.Smith")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "DeWitt")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "DeWitt")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "El.Paso")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "El.Paso")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "Fort.Bend")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "Fort.Bend")]

shapefile_state$Item[missing_rows[5]] = data_file_long$Item[which(data_file_long$name == "Jeff.Davis")]
shapefile_state$value[missing_rows[5]] = data_file_long$value[which(data_file_long$name == "Jeff.Davis")]

shapefile_state$Item[missing_rows[6]] = data_file_long$Item[which(data_file_long$name == "Jim.Hogg")]
shapefile_state$value[missing_rows[6]] = data_file_long$value[which(data_file_long$name == "Jim.Hogg")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "Jim.Wells")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "Jim.Wells")]

shapefile_state$Item[missing_rows[8]] = data_file_long$Item[which(data_file_long$name == "La.Salle")]
shapefile_state$value[missing_rows[8]] = data_file_long$value[which(data_file_long$name == "La.Salle")]

shapefile_state$Item[missing_rows[9]] = data_file_long$Item[which(data_file_long$name == "Live.Oak")]
shapefile_state$value[missing_rows[9]] = data_file_long$value[which(data_file_long$name == "Live.Oak")]

shapefile_state$Item[missing_rows[10]] = data_file_long$Item[which(data_file_long$name == "Palo.Pinto")]
shapefile_state$value[missing_rows[10]] = data_file_long$value[which(data_file_long$name == "Palo.Pinto")]

shapefile_state$Item[missing_rows[11]] = data_file_long$Item[which(data_file_long$name == "Red.River")]
shapefile_state$value[missing_rows[11]] = data_file_long$value[which(data_file_long$name == "Red.River")]

shapefile_state$Item[missing_rows[12]] = data_file_long$Item[which(data_file_long$name == "San.Augustine")]
shapefile_state$value[missing_rows[12]] = data_file_long$value[which(data_file_long$name == "San.Augustine")]

shapefile_state$Item[missing_rows[13]] = data_file_long$Item[which(data_file_long$name == "San.Jacinto")]
shapefile_state$value[missing_rows[13]] = data_file_long$value[which(data_file_long$name == "San.Jacinto")]

shapefile_state$Item[missing_rows[14]] = data_file_long$Item[which(data_file_long$name == "San.Patricio")]
shapefile_state$value[missing_rows[14]] = data_file_long$value[which(data_file_long$name == "San.Patricio")]

shapefile_state$Item[missing_rows[15]] = data_file_long$Item[which(data_file_long$name == "San.Saba")]
shapefile_state$value[missing_rows[15]] = data_file_long$value[which(data_file_long$name == "San.Saba")]

shapefile_state$Item[missing_rows[16]] = data_file_long$Item[which(data_file_long$name == "Tom.Green")]
shapefile_state$value[missing_rows[16]] = data_file_long$value[which(data_file_long$name == "Tom.Green")]

shapefile_state$Item[missing_rows[17]] = data_file_long$Item[which(data_file_long$name == "Val.Verde")]
shapefile_state$value[missing_rows[17]] = data_file_long$value[which(data_file_long$name == "Val.Verde")]

shapefile_state$Item[missing_rows[18]] = data_file_long$Item[which(data_file_long$name == "Van.Zandt")]
shapefile_state$value[missing_rows[18]] = data_file_long$value[which(data_file_long$name == "Van.Zandt")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_texas = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Utah")

data_file = read.csv("Poultry State CSV/Utah.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:31)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Box.Elder")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Box.Elder")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Salt.Lake")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Salt.Lake")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "San.Juan")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "San.Juan")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_utah = shapefile_state

shapefile_state = subset(shapefile,NAME_1 == "Washington")

data_file = read.csv("Poultry State CSV/Washington.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:41)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Grays.Harbor")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Grays.Harbor")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Pend.Oreille")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Pend.Oreille")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "San.Juan")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "San.Juan")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "Walla.Walla")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "Walla.Walla")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_washington = shapefile_state

#Missing Lake Michigan and Lake Superior
shapefile_state = subset(shapefile,NAME_1 == "Wisconsin")

data_file = read.csv("Poultry State CSV/Wisconsin.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:74)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Eau.Claire")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Eau.Claire")]

shapefile_state$Item[missing_rows[2]] = data_file_long$Item[which(data_file_long$name == "Fond.du.Lac")]
shapefile_state$value[missing_rows[2]] = data_file_long$value[which(data_file_long$name == "Fond.du.Lac")]

shapefile_state$Item[missing_rows[3]] = data_file_long$Item[which(data_file_long$name == "Green.Lake")]
shapefile_state$value[missing_rows[3]] = data_file_long$value[which(data_file_long$name == "Green.Lake")]

shapefile_state$Item[missing_rows[4]] = data_file_long$Item[which(data_file_long$name == "La.Crosse")]
shapefile_state$value[missing_rows[4]] = data_file_long$value[which(data_file_long$name == "La.Crosse")]

shapefile_state$Item[missing_rows[7]] = data_file_long$Item[which(data_file_long$name == "St..Croix")]
shapefile_state$value[missing_rows[7]] = data_file_long$value[which(data_file_long$name == "St..Croix")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_wisconsin = shatefile_state

shapefile_state = subset(shapefile,NAME_1 == "Wyoming")

data_file = read.csv("Poultry State CSV/Wyoming.csv")
data_file = data_file[-1,]
data_file = data_file[1,]

data_file_long = pivot_longer(data_file, cols = 2:25)

is.element(data_file_long$name, shapefile_state$NAME_2)

print(data_file_long[!is.element(data_file_long$name, shapefile_state$NAME_2),],n= 70)


shapefile_state = merge(shapefile_state,data_file_long, by.x = "NAME_2", by.y= "name", all.x=T)

shapefile_state[which(is.na(shapefile_state$value)),]
missing_names = shapefile_state$NAME_2[which(is.na(shapefile_state$value))]
print(missing_names)

missing_rows = which(is.na(shapefile_state$value))


shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Big.Horn")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Big.Horn")]

shapefile_state$Item[missing_rows[1]] = data_file_long$Item[which(data_file_long$name == "Hot.Springs")]
shapefile_state$value[missing_rows[1]] = data_file_long$value[which(data_file_long$name == "Hot.Springs")]

shapefile_all = rbind(shapefile_all,shapefile_state); shapefile_state_wyoming = shapefile_state

