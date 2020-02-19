
BC <- BC_avhrr_only_v2_Document_Document
names(BC) = c("lon", "lat", "temp", "date")
save(BC, file = "data/BC.RData")


CalC <- CalC_avhrr_only_v2_Document_Document
names(CalC) = c("lon","lat","temp","date")
save(CalC, file = "data/CalC.RData")


CC <- CC_avhrr_only_v2_Document_Document
names(CC) = c("lon","lat","temp","date")
save(CC, file = "data/CC.RData")


HC <- HC_avhrr_only_v2_Document_Document
names(HC) = c("lon","lat","temp","date")
save(HC, file = "data/HC.RData")