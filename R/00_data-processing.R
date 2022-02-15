######################################################################
#
#  Wilderness Stewardship Performance: data processing
#
#    Rob Smith, 17 Aug 2020
#
##      GNU General Public License, Version 3.0    ###################


# This project was supported in part by an appointment to the Research
# Participation Program at the United States Forest Service, United
# States Department of Agriculture, administered by the Oak Ridge
# Institute for Science and Education through an interagency agreement
# between the U.S. Department of Energy and USFS.


### preamble
rm(list=ls())
# install.packages('crs')                       # from CRAN
# remotes::github_install('phytomosaic/ecole')  # from github
require(crs)
require(ecole)

### assumes all RAW data are in './data_contributed/'

### create dir to hold clean data
if (!dir.exists('./data/')) {
        dir.create('./data/')
}

### load and clean species occurrence data
d <- read.csv('./data_contributed/MegaDbPLOT_2020.05.09.csv',
              stringsAsFactors=F,
              fileEncoding = 'latin1')
names(d) <- clean_text(names(d), lower=T)
names(d)[names(d) == 'wilderns'] <- 'wa'
d[,c('latusedd','longusedd')] <- NULL
names(d)[names(d) %in% c('latusenad83','longusenad83')] <-
        c('lat','lon')
d$wa[d$wa == ' '] <- ''  # fix a single case
d$wa[d$wa == '' ] <- NA
d$wa <- gsub(' Study Area', '', d$wa)           # some adjuncts
d$wa <- gsub(' \\(draft boundary\\)', '', d$wa) # some adjuncts

### HACK! remove years before 2019 because ARM did not yet query CMAQ values!
d <- d[d$year < 2019,]

### ten-year bins
d$tenyr_calendar <- cut(d$year, breaks=seq(1970,2030,by=10),
                        labels=paste0(seq(1970,2020,by=10),'-',
                                      seq(1980,2030,by=10)))
# sampling rounds begin in initial year the wilderness was sampled
x <- d[,c('match_to','year','wa')]
x$wa_init      <- ave(x$year, x$wa, FUN=min)
x$wa_round     <- ((x$year - x$wa_init) %/% 10)
x$wa_rnd_begin <- x$wa_init + (x$wa_round * 10)
x$tenyr        <- paste0(x$wa_rnd_begin, '-', (x$wa_rnd_begin + 9))
d$tenyr        <- factor(x$tenyr)
d$tenyr[is.na(x$wa)] <- NA


###   detrending climate  #########################################
x <- d                              # temporary data.frame
x <- x[x$lat < 49.01,]              # remove alaska ! ! ! ! ! ! !
x <- x[!is.na(x$n_airscore),]       # remove if lacking airscore
x <- x[!is.na(x$meanmaxaugt5y_c),]  # remove if lacking climate data
x <- x[!is.na(x$meanppt5y_cm),]     # remove if lacking climate data
x <- x[!is.na(x$cmaq_n_3yroll ),]   # remove if lacking CMAQ data
set.seed(88) ; x$meanmaxaugt5y_c <- jitter(x$meanmaxaugt5y_c,fac=2.5)
### bin 2 climate variables into quantile groups (E vs W separately)
k <- 5 # number of climate groups (5 = quintiles)
`bin` <- function(x) {
        x2  <- as.numeric(ecole::cut_quantile(x$meanppt5y_cm, n=k))
        x3  <- as.numeric(ecole::cut_quantile(x$meanmaxaugt5y_c, n=k))
        return(interaction(x2, x3, sep='_'))
}
iswest <- which(x$us_loc == 'west')
iseast <- which(x$us_loc == 'east')
x$grp  <- factor(
        NA, levels=interaction(rep(1:k,ea=k),rep(1:k,time=k),sep='_'))
x$grp[iseast] <- bin(x[iseast,])
x$grp[iswest] <- bin(x[iswest,])
table(x$grp[iseast])
table(x$grp[iswest])
### CRS model, using smooth for each discrete climate 'grp'
`f` <- function(x, yvar='n_airscore', xvar='cmaq_n_3yroll', ...) {
        # x <- x[iseast,]
        x$y   <- x[,yvar]
        x$x1  <- x[,xvar]
        m <- crs::crs(y ~ x1 + grp, degree=4, segments=1, lambda=0.1,
                      cv='none', kernel=TRUE, basis='tensor', data=x)
        f <- fitted(m) # fitted raw values
        # get offset coefficients for groups
        o <- plot(m, mean=T, ci=T, plot.behavior='data')[[2]]
        # offset by group mean (plus 'intercept')
        a <- f - o$mean[match(x$grp, o$grp)] + mean(o$mean)
        return(list(m = m, fit = f, adj = a))
}
ne <- f(x[iseast,], 'n_airscore', 'cmaq_n_3yroll')
nw <- f(x[iswest,], 'n_airscore', 'cmaq_n_3yroll')
se <- f(x[iseast,], 's_airscore', 'cmaq_s_3yroll')
sw <- f(x[iswest,], 's_airscore', 'cmaq_s_3yroll')
x$nfit <- x$nadj <- x$sfit <- x$sadj <- NA
x$nfit[iswest]  <- nw$fit
x$nadj[iswest]  <- nw$adj
x$nfit[iseast]  <- ne$fit
x$nadj[iseast]  <- ne$adj
x$sfit[iswest]  <- sw$fit
x$sadj[iswest]  <- sw$adj
x$sfit[iseast]  <- se$fit
x$sadj[iseast]  <- se$adj
### match to original full dataframe
d$nadj <- x$nadj[match(d$megadbid, x$megadbid)]
d$sadj <- x$sadj[match(d$megadbid, x$megadbid)]
d$nfit <- x$nfit[match(d$megadbid, x$megadbid)]
d$sfit <- x$sfit[match(d$megadbid, x$megadbid)]
rm(x,iswest,iseast,nw,ne,sw,se,f)
###   END detrending climate  #####################################

###   calc EXCEEDANCES   ##########################################
# lookup table CLs from Diversity paper (are invariant to geography)
tab <- data.frame(metric = rep(c('Species richness',
                                 'Sensitive species richness',
                                 'Forage lichen abundance',
                                 'Cyanolichen abundance',
                                 'Comm composition shifts'),len=10),
                  element = rep(c('N','S'),ea=5),
                  deposition = c(3.5, 3.1, 1.9, 1.3, 1.5,
                                 6.0, 2.5, 2.6, 2.3, 2.7))
lab <- c('ex_sr','ex_sr_sens','ex_sr_forag','ex_sr_cyano','ex_compn')
### based on CMAQ
n_exceed <- sweep(x = d[,rep('cmaq_n_3yroll',ea=5)],
                  MARGIN = 2, STATS  = tab$deposition[1:5])
s_exceed <- sweep(x = d[,rep('cmaq_s_3yroll',ea=5)],
                  MARGIN = 2, STATS  = tab$deposition[6:10])
names(n_exceed) <- paste0('n_cmaq_', lab)
names(s_exceed) <- paste0('s_cmaq_', lab)
d <- cbind(d, n_exceed, s_exceed)  ; rm(n_exceed, s_exceed)
### based on raw airscores
n_exceed <- sweep(x = d[,rep('n_airscore',ea=5)],
                  MARGIN = 2, STATS  = tab$deposition[1:5])
s_exceed <- sweep(x = d[,rep('s_airscore',ea=5)],
                  MARGIN = 2, STATS  = tab$deposition[6:10])
names(n_exceed) <- paste0('n_raw_', lab)
names(s_exceed) <- paste0('s_raw_', lab)
d <- cbind(d, n_exceed, s_exceed)  ; rm(n_exceed, s_exceed)
### based on adjusted airscores
n_exceed <- sweep(x = d[,rep('nadj',ea=5)],
                  MARGIN = 2, STATS  = tab$deposition[1:5])
s_exceed <- sweep(x = d[,rep('sadj',ea=5)],
                  MARGIN = 2, STATS  = tab$deposition[6:10])
names(n_exceed) <- paste0('n_adj_', lab)
names(s_exceed) <- paste0('s_adj_', lab)
d <- cbind(d, n_exceed, s_exceed)  ; rm(n_exceed, s_exceed)
###   END exceedances   ###########################################

###   nationwide PERCENTILES of SITE SCORES   #####################
`p` <- function(x) ecdf(x)(x) * 100
d$p_n_airscore <- p(d$n_airscore)
d$p_n_adj      <- p(d$nadj)
d$p_n_cmaq     <- p(d$cmaq_n_3yroll)
d$p_s_airscore <- p(d$s_airscore)
d$p_s_adj      <- p(d$sadj)
d$p_s_cmaq     <- p(d$cmaq_s_3yroll)
rm(p)
###   end percentiles   ###########################################


### keep only wilderness-adjacent plots (within 1 mile)
i <- (!is.na(d$wa)) |
        (d$inwilderness == 'Y') |
        (d$withinhalfmile == 'Y') |
        (d$within1mile == 'Y')
sum(i) / dim(d)[[1]]  # propn of plots that are wilderness-adjacent
d <- d[i,]
rm(i)

### manually fix spelling errors
d$wa[which(d$wa == 'Delerium Wilderness')] <- 'Delirium Wilderness'
d$wa[which(d$wa == 'Rock River Wilderness')] <-
        'Rock River Canyon Wilderness'
d$wa[which(d$wa == 'Sawthtooth Wilderness')] <- 'Sawtooth Wilderness'
d$wa[which(d$wa == 'Whisker Wilderness')] <- 'Whisker Lake Wilderness'
d$wa[which(d$wa == 'Olympic Wilderness')] <- 'Daniel J. Evans Wilderness'
d$wa[which(d$wa == 'Nordehouse Dunes Wilderness')] <-
        'Nordhouse Dunes Wilderness'
d$wa[which(d$wa == 'Near wilderness')] <- 'Glacier Peak Wilderness'
d$wa[which(d$wa == 'Marble Mountain Wilderness ')] <-
        'Marble Mountain Wilderness'
d$wa[which(d$wa == 'Mt. Thielsen Wilderness')] <-
        'Mount Thielsen Wilderness'
d$wa[which(d$wa == 'Mount Moriah Wilderness')] <-
        'Mt. Moriah Wilderness'
d$wa[which(d$wa == 'Mount Irish Wilderness')] <-
        'Mt. Irish Wilderness'
d$wa[which(d$wa == 'Maurelle Islands Wilderness')] <-
        'Maurille Islands Wilderness'
d$wa[which(d$wa == 'College Fiord-Nellie Juan Wilderness Study Area')] <-
        'Nellie Juan-College Fiord Wilderness'
# "Capitol Reef National Park"  # is only a Wilderness Study Area

### manually fix one erroneous latitude coordinate
i <- which(d$wa == 'Horseshoe Bay Wilderness' & d$lat > 49.000)
d$lat[i] <- 45.99755 ; rm(i)

### manually fix one erroneous sampling round number
d$roundno[d$roundno == '2010-2018'] <- '2010-2019'

### sort by wilderness, then year (for run-length encoding)
d <- d[order(d$wa, d$year),]

### make sign of ratios consistent with sign of airscores
d$olig_olig_eut       <- 1 - d$olig_olig_eut
d$s_sens_s_sens_s_tol <- 1 - d$s_sens_s_sens_s_tol

### aggregate MEAN per 10-YEAR interval per WA
j <- c(which(names(d) %in% c('cmaq_n_3yroll','n_airscore',
                             'cmaq_s_3yroll','s_airscore')),
       which(names(d) == 'nadj'):NCOL(d))
a <- aggregate(list(d[,j]),
               by=list(yr=d$tenyr, wa=d$wa),
               function (x, na.rm=TRUE, digits=2, ...) {
                       if (!is.numeric(x)) { return(NULL) }
                       m   <- mean(x, na.rm=na.rm)
                       s   <- sd(x, na.rm=na.rm)
                       n   <- length(x) - sum(is.na(x))
                       out <- round(c(mean=m,sd=s,n=n),digits=digits)
                       mode(out) <- 'numeric'
                       return(out)},
               drop=F)
a <- do.call(cbind.data.frame, a) # simplify structure
a <- data.frame(warea=a[,2], tenyr=a[,1], a[,c(3:NCOL(a))])
a <- data.frame(a,
                lat=aggregate(d$lat, by=list(yr=d$tenyr, wa=d$wa),
                              mean, na.rm=T, drop=F)[[3]],
                lon=aggregate(d$lon, by=list(yr=d$tenyr, wa=d$wa),
                              mean, na.rm=T, drop=F)[[3]])
a <- a[rowSums(a[,grep('\\.n',x=names(a),value=T)],na.rm=T) > 0,]
a <- a[rowSums(is.na(a)) != ncol(a),]
is.na(a) <- is.na(a) # convert all NaN,Inf,or NA to same NA
n_plots <- aggregate(d[,'megadbid'], by=list(wa=d$wa), length, drop=F)
a$n_plots <- n_plots$x[match(a$warea, n_plots$wa)]  ;  rm(n_plots)


### load and clean LICHEN SPECIES occurrence data
s <- read.csv('./data_contributed/MegaDbLICHEN_2020.05.09.csv', stringsAsFactors=F)
names(s) <- clean_text(names(s), lower=T)
s <- s[s$megadbid %in% unique(d$megadbid),] # keep only wilderness
s <- s[s$growthform == 'Epiphytic macrolichen',] # keep only macros
s <- s[,c('megadbid','sci_22chklst','fxlgp','n_class','n_critload',
          's_class','s_critload','consrv_stat')]
s$fxlgp[grep('For', s$fxlgp)] <- 'forage' # relabel functional groups
s$fxlgp[grep('Cya', s$fxlgp)] <- 'cyano'  # relabel functional groups
s$fxlgp[grep('Mtx', s$fxlgp)] <- ''       # relabel functional groups
### list of species lists per wilderness
wname <- sort(unique(d$wa))
splist <- lapply(wname, function(pick){
        i    <- d$megadbid[which(d$wa == pick)]
        spp  <- s[s$megadbid %in% i,]
        spp  <- spp[!duplicated(spp$sci_22chklst),]
        spp  <- spp[order(spp$sci_22chklst),]
        spp  <- spp[,c('sci_22chklst','fxlgp','n_class','n_critload',
                       's_class','s_critload','consrv_stat')]
        return(spp)})
names(splist) <- wname
s <- splist # rename it



### save   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###
save(d, file='./data/d.rda') # data per SITE (each survey)
save(a, file='./data/a.rda') # aggregate MEAN per 10-YEAR interval per WA
save(s, file='./data/s.rda') # list of species lists per WA
### copy to /R dir for shiny app
file.copy(from='./data/d.rda', to='./R/d.rda', overwrite=TRUE)
file.copy(from='./data/a.rda', to='./R/a.rda', overwrite=TRUE)
file.copy(from='./data/s.rda', to='./R/s.rda', overwrite=TRUE)
###   ###   ###   ###   ###   ###   ###   ###   ######   ###   ###   ###




######################################################################
#
#  ELEMENTAL data processing -- all below
#
#    Rob Smith, 21 Jun 2021
#
##      GNU General Public License, Version 3.0    ###################

# This project was supported in part by an appointment to the Research
# Participation Program at the United States Forest Service, United
# States Department of Agriculture, administered by the Oak Ridge
# Institute for Science and Education through an interagency agreement
# between the U.S. Department of Energy and USFS.

### TODO:
#         1 - use detection limit when below
#         2 - handle when Wilderness has ELEMENTAL but not LICHEN surveys
#         3 - calibrations/offsets per species?
#         4 - assign sampling rounds to years
#         5 - what is the acceptable THRESHOLD per element?

### preamble
rm(list=ls())  # dont pollute
require(ecole)

### load and clean data
e <- read.csv('./data_contributed/MegaDbELEMENTAL_20210530.csv',
              stringsAsFactors=F,
              na.strings = c('NA','',' ','n.d.', 'n.d. ', 'n.d', 'nd'),
              fileEncoding = 'latin1'
)
names(e) <- clean_text(names(e), lower=T)
names(e)[names(e) == 'ars_ppb'] <- 'as_ppb'   # fix col name
names(e)[names(e) == 'wilderns'] <- 'wa'      # fix col name
e <- e[,1:which(colnames(e) == 'no3n_ppm')]
e <- e[!is.na(e$wa),]          # keep only if assigned a wilderness name
# e <- e[e$megadbid != 20000,] # looks like a placeholder for missing megadbid?
# table(e$scicode)             # big hot mess on their data entry

### TODO: get detection limits
e[,grep('x_', names(e))]   # index detection limit columns (what is the DL?)

### fix the concentration columns
j <- grep('_ppm|_ppb|_pct_adj', names(e))     # index concentration columns
e[,j] <- sapply(e[,j], as.character) # set character
e[,j] <- sapply(e[,j], function(x) gsub('<','',x)) # use detection limit when below
e[,j] <- sapply(e[,j], as.numeric)   # set numeric
e[,j] <- sapply(e[,j], function(x) { x[ x < 0 ] <- NA ; x } ) # remove neg values ! ! ! ! how TF did these get in ? ? ?
e$hg_ppm <- e$hg_ppb * 0.001 # convert mercury from ppb to ppm

### ensure all names have *valid* Wilderness names
load('./R/d.rda') # throws encoding error, ok for now
head(d)
wname <- sort(unique(d$wa))
# manually fix spelling errors (5 have ELEMENTAL but not LICHEN surveys)
`rename` <- function(from, to, ...) { e$wa[which(e$wa == from)] <<- to }
rename('Ansel Adams', 'Ansel Adams Wilderness')     # not in good list??
rename('Currant Mountain Wilderness', 'Currant Mountain Wilderness')  # not in good list??
rename('Granite Chief', 'Granite Chief Wilderness') # not in good list??
rename('Kaiser', 'Kaiser Wilderness')               # not in good list??
rename('Neota Wilderness', 'Neota Wilderness')      # not in good list??
rename('Basin Creek Reasearch Natural Area', NA) # not a wilderness
rename('Bull Run Watershed', NA)                 # not a wilderness
rename('Elkhorn Wildlife management Unit', NA)   # not a wilderness
rename('Sawthooth Wilderness', 'Sawtooth Wilderness')
rename('Admiralty Island Wilderness', 'Kootznoowoo Wilderness')
rename('Agua Tibia', 'Agua Tibia Wilderness')
rename('Anaconda Pintler Wilderness (vicinity)', 'Anaconda Pintler Wilderness')
rename('Anaconda-Pintler Wilderness', 'Anaconda Pintler Wilderness')
rename('Bucks Lake', 'Bucks Lake Wilderness')
rename('Bull Canyon Wilderness Study Area', 'Bull Canyon Wilderness')
rename('College Fiord-Nellie Juan Wilderness Study Area',
       'College Fiord-Nellie Juan Wilderness')
rename('Desolation', 'Desolation Wilderness')
rename('Dinkey Lakes', 'Dinkey Lakes Wilderness')
rename('Emigrant', 'Emigrant Wilderness')
rename('Endicott River  Wilderness', 'Endicott River Wilderness')
rename('Gates of the Mountain Wilderness', 'Gates of the Mountains Wilderness')
rename('Gospel Hump Wilderness', 'Gospel-Hump Wilderness')
rename('Jedidiah Smith Wilderness', 'Jedediah Smith Wilderness')
rename('John Muir', 'John Muir Wilderness')
rename('Joyce Kilmer Wilderness', 'Joyce Kilmer-Slickrock Wilderness')
rename('La Madre Mountains Wilderness Study Area', 'La Madre Mountain Wilderness')
rename('Linnville Gorge Wilderness', 'Linville Gorge Wilderness')
rename('Maurelle Islands Wilderness', 'Maurille Islands Wilderness')
rename('Misty Fjords Wilderness', 'Misty Fiords National Monument Wilderness')
rename('Mokelumne', 'Mokelumne Wilderness')
rename('Mount Charleston Wilderness', 'Mt. Charleston Wilderness')
rename('Mount Moriah Wilderness', 'Mt. Moriah Wilderness')
rename('Mount Stirling Wilderness Study Area', 'Mount Stirling Wilderness')
rename('Mountain Sky Wilderness', 'Sky Lakes Wilderness') # probably
rename('Mt Jefferson', 'Mount Jefferson Wilderness')
rename('Mt Thielsen', 'Mount Thielsen Wilderness')
rename('Olympic Wilderness', 'Daniel J. Evans Wilderness')
rename('Pemigewasset', 'Pemigewasset Wilderness')
rename('Pleasant-Lemesurier-Inian Islands Wilderness',
       'Pleasant/Lemusurier/Inian Islands Wilderness')
rename('Sarius Creek Wilderness', 'Sarvis Creek Wilderness')
rename('Selway Bitteroot Wilderness', 'Selway-Bitterroot Wilderness')
rename('South Kuiu Wilderness', 'Kuiu Wilderness')
rename('Strawberry', 'Strawberry Mountain Wilderness')
rename('Tebenkof Wilderness', 'Tebenkof Bay Wilderness')
rename('Wenaha Tucannon Wilderness', 'Wenaha-Tucannon Wilderness')
rename('Wonder Mountain (just outside boundary)', 'Wonder Mountain Wilderness')
e <- e[!is.na(e$wa),]  # keep only if assigned a wilderness name
cat(paste0(sort(unique(e$wa[which(e$wa %notin% wname)])), sep='\n')) # need fixed
rm(d, wname, rename, j)

### aggregate certain species --- CAUTION ! ! ! very subjective, need confirm
table(e$scicode, useNA='always')
`rename` <- function(from, to, ...) {
        e$scicode[which(e$scicode == from)] <<- to
}
rename('Hyap', 'Hypapi')
rename('Alectoria', 'Alesar')
rename('Alesar ', 'Alesar')
rename('Blank', NA)
rename('Brfre', 'Bryfre')
rename('Brypse/Bryfre', 'Bryfre')
rename('Brycap', 'Bryoria')
rename('Brygla', 'Bryoria')
rename('Bryfus', 'Bryoria')
rename('Brypsef', 'Bryfre')
rename('Usnsubf', 'Usnsub')
rename('Hypocc/Hypims', 'Hypims')
rename('Flavopunctelia', 'Flacap')
rename('Parcap', 'Flacap')
rename('Xancum', '*Xanthoparmelia')
rename('Xanwyo', '*Xanthoparmelia')
rename('Xancol', '*Xanthoparmelia')
rename('Xanthoparmelia', '*Xanthoparmelia')
rename('Rhichr', '*Rhizoplaca')
rename('Rhimel', '*Rhizoplaca')
rename('Rhipel', '*Rhizoplaca')
rename('Rhychr', '*Rhizoplaca')
rename('Rhizoplaca', '*Rhizoplaca')
rename('Usnsubl', 'Usnsub')
rename('Laspap', '*Umbilicaria')
rename('Umbame', '*Umbilicaria')
rename('Umbhir', '*Umbilicaria')
rename('Umbhyp', '*Umbilicaria')
rename('Umbpha', '*Umbilicaria')
rename('Umbpol', '*Umbilicaria')
rename('Umbkra', '*Umbilicaria')
rename('Umbvir', '*Umbilicaria')
rename('Umbpolp', '*Umbilicaria')
rename('Umbpus', '*Umbilicaria')
rename('Umbvel', '*Umbilicaria')
rename('Letvul/Letcol', 'Letvul')
rename('letcol', 'Letcol')
rename('Usnlon', 'Usnea')
rename('Aleims', 'Alesar')
rename('Cetniv', '*Flaniv')
rename('Flaniv', '*Flaniv')
rename('Claran', '*Cladina')
rename('Claste', '*Cladina')
rename('Clasty', '*Cladina')
rename('Clasub', '*Cladina')
rename('Dermin', '*Dermatocarpon')
rename('Derret', '*Dermatocarpon')
rename('Sphtuc', '*Sphglo')
rename('Sphglo', '*Sphglo')
rename('Pseint', '*Pseudevernia')
rename('Pseinte', '*Pseudevernia')
rename('Plawhe', 'Plagla')
rename('Isomyo', '*moss')
rename('Torrur', '*moss')
rename('Necdou', '*moss')
rename('Hylspl', '*moss')
rename('Parsax', '*Parsax')
rename('Parstu', 'Pmostu')
rename('Evepru', '*Evepru')
e <- e[!is.na(e$scicode),] # cant use if no species given
table(e$scicode, useNA='always')

### TODO: aggregate to 10-y bins, not 1-y

### aggregate geometric mean per year/species/wilderness
j   <- c('wa','year','scicode',
         'cd_ppm','cr_ppm','cu_ppm','hg_ppm','ni_ppm','pb_ppm','zn_ppm',
         'n_pct_adj','s_pct_adj')
n   <- aggregate(. ~ year+scicode+wa, e[,j], length, na.action=na.pass)
n   <- n$s_pct_adj
e   <- cbind(aggregate(. ~ year+scicode+wa, e[,j], ecole::geom_mean,
                       na.rm=TRUE, na.action=na.pass), n)
e[sapply(e, is.nan)] <- NA
j <- c('cd_ppm','cr_ppm','cu_ppm','hg_ppm','ni_ppm','pb_ppm','zn_ppm',
       'n_pct_adj','s_pct_adj')
e[,j] <- sapply(e[,j], round, digits=2)
dim(e)  # 1116 *final* sample units (year/species/wilderness)


### save   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###   ###
save(e, file='./data/e.rda')
file.copy(from='./data/e.rda', to='./R/e.rda', overwrite=T)
###   ###   ###   ###   ###   ###   ###   ###   ######   ###   ###   ###



####    END    ####