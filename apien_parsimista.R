library(dplyr)
library(rvest)
read_html("https://www.paikkatietohakemisto.fi/inspire_seuranta/inspire_monitoring_2016_FI.html") %>% 
  html_table(fill = TRUE) -> res
dat <- res[[3]]

wfslist <- list()
for (i in 1:ncol(dat)){
  wfslist[[i]] <- dat %>% pull(i) %>% 
    grep("wfs", ., value = TRUE)
}
wfss <- do.call(combine, wfslist)
sub("^.+http", "http", wfss) %>% 
  unique(.) %>% 
  cat(sep = '",\n"')

c(
  "http://kartta.hyvinkaa.fi/wfs_1/ows.ashx?service=WFS&request=GetCapabilities",
  # "http://data.fmi.fi/fmi-apikey/insert-your-apikey-here/wfs?request=GetCapabilities",
  "http://kartta.kokkola.fi/TeklaOGCWeb/wfs.ashx?service=WFS&request=GetCapabilities",
  "https://kartta.kouvola.fi/TeklaOGCWeb/wfs.ashx?service=WFS&request=GetCapabilities",
  "https://extranet.liikennevirasto.fi/inspirepalvelu/avoin/wfs?request=getcapabilities",
  # "https://extranet.liikennevirasto.fi/inspirepalvelu/beta/wfs?request=getcapabilities",
  "https://extranet.liikennevirasto.fi/inspirepalvelu/rajoitettu/wfs?request=getcapabilities",
  "https://extranet.liikennevirasto.fi/inspirepalvelu/TransportNetworks/wfs?request=getcapabilities",
  "http://gis2.luke.fi:8080/geoserver/kala/wfs?request=getcapabilities",
  "http://gis2.luke.fi:8080/geoserver/riista/wfs?request=getcapabilities",
  # "https://ws.nls.fi/ktjkii/wfs/wfs?service=WFS&request=GetCapabilities&version=1.1.0",
  # "https://ws.nls.fi/nimisto/wfs?service=WFS&request=GetCapabilities&version=1.1.0",
  # "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/au?service=WFS&request=GetCapabilities",
  "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/bu_mtk_point?service=WFS&request=GetCapabilities",
  "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/bu_mtk_polygon?service=WFS&request=GetCapabilities",
  "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/gn?service=wfs&request=GetCapabilities",
  "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/hy?service=WFS&request=GetCapabilities",
  "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/ad?service=wfs&request=GetCapabilities",
  "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/bu_vtj?service=wfs&request=GetCapabilities",
  "https://kartta.salo.fi/TeklaOGCWeb/wfs.ashx?service=WFS&request=GetCapabilities",
  "http://geoserver.ymparisto.fi/geoserver/wfs?service=wfs&version2.0&request=GetCapabilities",
  # "http://tampere.navici.com/tampere_wfs_geoserver/ows?service=wfs&version=1.0.0&request=GetCapabilities",
  "http://geo.stat.fi/geoserver/ows?service=wfs&version=1.0.0&request=GetCapabilities",
  "https://opaskartta.turku.fi/TeklaOgcWeb/wfs.ashx?service=WFS&request=GetCapabilities",
  # "https://ws.nls.fi/rahu/wfs?service=WFS&request=GetCapabilities&version=1.1.0",
  "http://maps.luomus.fi/geoserver/wfs?service=WFS&request=GetCapabilities"
) -> wfsat

datx <- tibble()
for (i in 1:length(wfsat)){
  httr::GET(wfsat[i]) -> res
  if (res$status_code == 200){
    content <- xml2::read_xml(res$content)
    xml2::write_xml(content, "tmp.xml")
    tmp <- readLines("tmp.xml")
    pituus <- length(tmp)
  } else {
    pituus = NA
  }
  datx <- bind_rows(datx, tibble(url = wfsat[i], 
                                 pituus = pituus))
}
datx
toimii <- datx$url

toimii %>% cat(sep = '",\n"')

c("http://kartta.hyvinkaa.fi/wfs_1/ows.ashx?service=WFS&request=GetCapabilities",
  "http://kartta.kokkola.fi/TeklaOGCWeb/wfs.ashx?service=WFS&request=GetCapabilities",
  "https://kartta.kouvola.fi/TeklaOGCWeb/wfs.ashx?service=WFS&request=GetCapabilities",
  "https://extranet.liikennevirasto.fi/inspirepalvelu/avoin/wfs?request=getcapabilities",
  "https://extranet.liikennevirasto.fi/inspirepalvelu/rajoitettu/wfs?request=getcapabilities",
  "https://extranet.liikennevirasto.fi/inspirepalvelu/TransportNetworks/wfs?request=getcapabilities",
  "http://gis2.luke.fi:8080/geoserver/kala/wfs?request=getcapabilities",
  "http://gis2.luke.fi:8080/geoserver/riista/wfs?request=getcapabilities",
  "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/bu_mtk_point?service=WFS&request=GetCapabilities",
  "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/bu_mtk_polygon?service=WFS&request=GetCapabilities",
  "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/gn?service=wfs&request=GetCapabilities",
  "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/hy?service=WFS&request=GetCapabilities",
  "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/ad?service=wfs&request=GetCapabilities",
  "https://inspire-wfs.maanmittauslaitos.fi/inspire-wfs/bu_vtj?service=wfs&request=GetCapabilities",
  "https://kartta.salo.fi/TeklaOGCWeb/wfs.ashx?service=WFS&request=GetCapabilities",
  "http://geoserver.ymparisto.fi/geoserver/wfs?service=wfs&version2.0&request=GetCapabilities",
  "http://geo.stat.fi/geoserver/ows?service=wfs&version=1.0.0&request=GetCapabilities",
  "https://opaskartta.turku.fi/TeklaOgcWeb/wfs.ashx?service=WFS&request=GetCapabilities",
  "http://maps.luomus.fi/geoserver/wfs?service=WFS&request=GetCapabilities")
