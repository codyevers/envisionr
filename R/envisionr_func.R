pacman::p_load(XML, raster, RColorBrewer, shiny, plyr, animation, stringr, sf)

prime_data <- function(delta.bundle.directory){
  scn.run.df <<- getDeltaBundleNames(dir=delta.bundle.directory)
  idu.grid.ref <<- extend(raster(read.asciigrid('data/misc/idu2raster_cs_90m.asc')), 10)
  load('data/misc/REnv_load.Rdata')
  assign("idu.ref", idu.ref, .GlobalEnv)
  ugb <- st_read('data/misc/ugb.geojson')
  assign("ugb", ugb, .GlobalEnv)
  
  scn.loaded <<- getDeltaBundleToLoad(scn.run.df$file[1])
  load(scn.run.df$fn[1]); db.base <<- db; da <- db$state.array
  slice <<- da[,'vegclass',1]
}

pullSlice <- function(state.array, year, filter){
  if(grepl(",",filter)){
    # if ____
    ptm <- proc.time()
    a <- strsplit(filter, split=",")[[1]][1]
    b <- strsplit(filter, split=",")[[1]][2]
    txt <- paste("slice <- apply(state.array[,,",b,"], c(3), function(x){with(as.data.frame(x),{",a,"})})")
    eval(parse(text=txt))
    slice <- apply(slice, 1, sum)
  } else {
    yr <- as.numeric(gsub("\\D","",year))
    txt <- paste("slice <- with(as.data.frame(cbind(state.array[,,yr], idu.ref)), {",filter,"})")
    eval(parse(text=txt))
  }
  print(txt)
  return(slice)
}

getDeltaBundleNames <- function(dir){
  db.list <- list.files(dir,full.names=T, pattern='.*DeltaBundle.*Rdata')
  x <- ldply(strsplit(db.list,split='/'))
  fn <- x[,ncol(x)]
  y <- ldply(strsplit(fn,'_'))
  scn <- y[,1]
  run <- gsub('\\D','',y[,3])
  scn.run.df <- data.frame(scn=scn, run=run, file=fn, fn=db.list, stringsAsFactors=F)
  print(scn.run.df)
  return(scn.run.df)
}

getDeltaBundleToLoad <- function(scn_txt){
  row <- which(scn.run.df$file == scn_txt)
  scn.run.fn <<- scn.run.df$fn[row]
}

mapSlice <- function(data.slice, filter, use.color.scheme=FALSE, extent=NULL, custom.pal=NULL, plot.options, ...){

  idu.val <- getValues(idu.grid.ref)
  ras <- setValues(idu.grid.ref, data.slice[idu.val])
  ras <- ratify(ras)

  # crop raster (i.e. zoom) if extent is provided
  if(!is.null(extent)){
    txt <- paste("e <- extent(c(",paste(extent, collapse=", "),"))",sep="")
    eval(parse(text=txt))
    ras <- crop(ras, e)
  }
  
  axis_toggle = ifelse(any(plot.options == 'Axis'), 1, 0)
  legend_toggle = ifelse(any(plot.options == 'Legend'), 1, 0)
  
  par(mar=rep(2,4), bg = NA, bty = ifelse(any(plot.options == 'Box'),'o','n'))
  
  if(any(plot.options == 'Custom palette') == FALSE){
    if(use.color.scheme == TRUE & sum(displayPals() == filter) > 0){
      
      unique.vals <- unique(getValues(ras))
      unique.vals <- sort(unique.vals)
      pal.df <<- pullColorScheme(filter)
      
      if(length(grep("maxvalue",names(pal.df)))>0){
        cat("Mapping field w/ preset range values\n")
        pal <- pal.df$hex
        if(any(plot.options == 'Transparent white')) pal[pal == '#FFFFFF'] <- NA
        brks <- as.numeric(pal.df$maxvalue)
        sp::plot(ras, breaks=c((min(brks)-1),brks), col=pal, axes=axis_toggle, legend=legend_toggle, ...)
        if(any(plot.options == 'UGB')) plot(ugb %>% st_geometry(), add=T, lwd=2)
      } else {
        cat("Mapping field w/ preset discrete values\n")
        pal <- pal.df$hex[match(unique.vals,pal.df$value)]
        if(any(plot.options == 'Transparent white')) pal[pal == '#FFFFFF'] <- NA
        rat <- levels(ras)[[1]]
        rat$desc <- substr(pal.df$label[match(rat$ID, pal.df$value)],1,20)
        levels(ras) <- rat
        sp::plot(ras, breaks=c((min(unique.vals)-1),unique.vals), col=pal, axes=axis_toggle, legend=legend_toggle, ...)
        # rasterVis::levelplot(ras, col.regions=pal, scales = list(draw=FALSE))
        if(any(plot.options == 'UGB')) plot(ugb %>% st_geometry(), add=T, lwd=2)
      }
    } else {
        cat("No color scheme found,`` using blank template\n")
        pal <- brewer.pal(n=9, name='YlGnBu')
        if(any(plot.options == 'Transparent white')) pal[pal == '#FFFFFF'] <- NA
        sp::plot(ras, col=pal, axes=axis_toggle, legend=legend_toggle, ...)
        if(any(plot.options == 'UGB')) plot(ugb %>% st_geometry(), add=T, lwd=2)
    }
  } else {
      if(custom.pal == 'null'){
        cat("Custom palette not specified\n")
        cr = colorRampPalette(c('grey90','red'))
      } else {
        cat("Mapping field custom palette\n")
        cr = colorRampPalette(stringr::str_trim(unlist(stringr::str_split(custom.pal, ','))))
      }
    pal <- cr(10)
    if(any(plot.options == 'Transparent white')) pal[pal == '#FFFFFF'] <- NA
    sp::plot(ras, col=pal, axes=axis_toggle, legend=legend_toggle, ...)
    if(any(plot.options == 'UGB')) plot(ugb %>% st_geometry(), add=T, lwd=2) 
  }
}

displayPals <- function(){
  doc <- xmlTreeParse("data/misc/idu.xml", getDTD=F)
  r <- xmlRoot(doc)
  f1 <- xmlChildren(r)
  xml.headers <- tolower(ldply(f1, xmlGetAttr, name='col')$V1)
  return(xml.headers)
}

pullColorScheme <- function(index){
  
  doc <- xmlTreeParse("data/misc/idu.xml", getDTD=F)
  r <- xmlRoot(doc)
  
  if(class(index)=='character'){
    index <- tolower(index)
    f1 <- xmlChildren(r)
    xml.headers <- tolower(ldply(f1, xmlGetAttr, name='col')$V1)
    index <- which(xml.headers == index)
  }
  
  f1 <- xmlChildren(r)[[index]]
  xmlGetAttr(f1, name='col')
  
  a <- xmlChildren(f1)$attributes
  a.df <- data.frame(
    t(
      xmlSApply(a, FUN=function(x){
        label = xmlGetAttr(x, name='label')
        color = xmlGetAttr(x, name='color')
        txt <- paste("rgb",color,sep="")
        txt <- gsub(")",", maxColorValue=255)", txt)
        hex <- eval(parse(text=txt))
        value = xmlGetAttr(x, name='value')
        minvalue = xmlGetAttr(x, name='minVal')
        maxvalue = xmlGetAttr(x, name='maxVal')
        return(c(value=value, minvalue=minvalue, maxvalue=maxvalue, color=color, hex=hex, label=label))})
    ), row.names=NULL, stringsAsFactors=FALSE)
  
  return(a.df)
}

