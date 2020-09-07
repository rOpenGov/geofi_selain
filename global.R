# # Tarvittavat kirjastot
# library(ows4R)
# library(dplyr)
# 
# if (FALSE){
# # Data apien endpointeista ja versioista
# apis <- tibble(
#   api_url = c("http://geo.stat.fi/geoserver/wfs",
#               "http://avaa.tdata.fi/geoserver/paituli/wfs",
#               "http://geoserver.ymparisto.fi/geoserver/wfs?",
#               "https://kartta.hsy.fi/geoserver/wfs",
#               "https://kartta.hel.fi/ws/geoserver/avoindata/wfs",
#               "http://geoserver.hel.fi/geoserver/ows",
#               "https://kartat.espoo.fi/teklaogcweb/wfs.ashx",
#               "http://gis.vantaa.fi/geoserver/wfs",
#               "http://geodata.tampere.fi/geoserver/ows",
#               "https://opaskartta.turku.fi/TeklaOGCWeb/WFS.ashx",
#               "https://e-kartta.ouka.fi/TeklaOGCWeb/WFS.ashx",
#               "http://kartta.kuopio.fi/TeklaOgcWeb/WFS.ashx",
#               "https://kartta.rovaniemi.fi/teklaogcweb/WFS.ashx",
#               "https://kartta.jkl.fi/TeklaOgcWeb/WFS.ashx",
#               "http://lipas.cc.jyu.fi/geoserver/lipas/ows",
#               "http://geoserver.lounaistieto.fi/geoserver/varsinais-suomi_aluesuunnittelu/ows",
#               "http://geoserver.lounaistieto.fi/geoserver/koostetietovaranto/ows",
#               # Uudet
#               "http://kartta.hyvinkaa.fi/wfs_1/ows.ashx",
#               # "http://kartta.kokkola.fi/TeklaOGCWeb/wfs.ashx",
#               # "https://kartta.kouvola.fi/TeklaOGCWeb/wfs.ashx",
#               "https://extranet.liikennevirasto.fi/inspirepalvelu/avoin/wfs",
#               "https://extranet.liikennevirasto.fi/inspirepalvelu/rajoitettu/wfs",
#               "https://extranet.liikennevirasto.fi/inspirepalvelu/TransportNetworks/wfs",
#               "http://gis2.luke.fi:8080/geoserver/kala/wfs",
#               "http://gis2.luke.fi:8080/geoserver/riista/wfs",
#               # "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/bu_mtk_point",
#               # "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/bu_mtk_polygon",
#               # "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/gn",
#               # "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/hy",
#               # "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/ad",
#               # "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/bu_vtj",
#               # "https://kartta.salo.fi/TeklaOGCWeb/wfs.ashx",
#               # "http://geoserver.ymparisto.fi/geoserver/wfs",
#               "http://maps.luomus.fi/geoserver/wfs"
#               ),
#   ver = c("1.0.0",
#           "2.0.0",
#           "2.0.0",
#           "2.0.0",
#           "1.0.0",
#           "2.0.0",
#           "1.0.0",
#           "2.0.0",
#           "2.0.0",
#           "1.0.0",
#           "1.0.0",
#           "1.0.0",
#           "1.0.0",
#           "1.0.0",
#           "1.0.0",
#           "1.0.0",
#           "1.0.0",
#           # uudet
#           "1.0.0",
#           # "1.0.0",
#           # "1.0.0",
#           "1.0.0",
#           "1.0.0",
#           "1.0.0",
#           "1.0.0",
#           "1.0.0",
#           # "2.0.0",
#           # "2.0.0",
#           # "2.0.0",
#           # "2.0.0",
#           # "2.0.0",
#           # "2.0.0",
#           # "1.0.0",
#           # "1.0.0",
#           "1.0.0"),
#   provider = c("Tilastokeskus",
#                "Paituli",
#                "Syke",
#                "HSY",
#                "Helsinki",
#                "Helsinki geoserver",
#                "Espoo",
#                "Vantaa",
#                "Tampere",
#                "Turku",
#                "Oulu",
#                "Kuopio",
#                "Rovaniemi",
#                "Jyväskylä",
#                "LIPAS",
#                "Lounaistieto: varsinais-suomi_aluesuunnittelu",
#                "Lounaistieto: koostetietovaranto",
#                # uudet
#                "Hyvinkää",
#                # "Kokkola",
#                # "Kouvola",
#                "Liikennevirasto avoin",
#                "Liikennevirasto rajoitettu",
#                "Liikennevirasto Transport network",
#                # "uusi_api 7",
#                # "uusi_api 8",
#                # "uusi_api 9",
#                # "uusi_api 10",
#                # "uusi_api 11",
#                # "uusi_api 12",
#                "LUKE kala",
#                "LUKE riista",
#                # "Salo",
#                # "uusi_api 16",
#                "Luomus")
# )
# 
# if (!file.exists("data")) {
#   dir.create("data")
# }
# 
# saveRDS(apis, "./data/apis.RDS")
# # käydään apit läpi ja kootaan saatavilla olevat datat
# api_content_lst <- list()
# for (ii in 1:nrow(apis)){
# 
#   wfs <- WFSClient$new(apis$api_url[ii],
#                        serviceVersion = apis$ver[ii],
#                        logger = "INFO")
#   caps <- wfs$getCapabilities()
#   ft <- caps$findFeatureTypeByName("", exact = FALSE)
#   data_lst <- list()
#   if (length(ft) == 0) next()
#   for (i in 1:length(ft)){
#     data_lst[[i]] <- tibble(name = ft[[i]]$getName(),
#                             title = ft[[i]]$getTitle())
#   }
#   api_content_lst[[ii]] <- do.call(bind_rows, data_lst) %>%
#     mutate(provider = apis$provider[ii],
#            api_url = apis$api_url[ii],
#            api_ver = apis$ver[ii])
# }
# 
# api_content <- do.call(bind_rows, api_content_lst) %>%
#   mutate(provider = factor(provider)) %>%
#   select(provider,title,name,api_url,api_ver) %>% 
#   mutate(layer_url = glue::glue("{api_url}?service=WFS&version={api_ver}&request=getFeature&typename={name}"))
# 
# saveRDS(api_content, "./data/api_content.RDS")
# write.csv(api_content, "./www/api_content.csv")
# 
# # Aineistojen määrä per organisaatio
# # api_content %>%
# #   count(provider,api_url,api_ver)  %>%
# #   arrange(desc(n)) %>%
# #   rename(aineistoja = n)
# }
# # api_content <- readRDS("./data/api_content.RDS")
# # apis <- readRDS("./data/apis.RDS")
# 
# 
