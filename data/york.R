library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(hrbrthemes)

data <- read.csv('data-science-long-project-data-nyc-airbnb.csv')

# remove $0 listings
data <- data %>% filter(price > 0)
data$neighbourhood <- factor(data$neighbourhood)
data$neighbourhood_group <- factor(data$neighbourhood_group)
neighborhoods <- levels(data$neighbourhood)
boroughs <- levels(data$neighbourhood_group)

neighbourhood_counts <- data %>%
  group_by(neighbourhood) %>%
  summarize(count = n())

price_counts <- data %>%
  group_by(price) %>%
  summarize(count = n())

host_counts <- data %>%
  group_by(host_id) %>%
  summarize(count = n())
host_counts %>%
  ggplot(aes(x = count)) +
  geom_histogram()

# Neighbourhood counts histogram
data %>%
  group_by(neighbourhood) %>% mutate(count = n()) %>%
  ggplot(aes(x = reorder(neighbourhood, count))) +
  geom_bar() +
  theme(axis.text.x = element_text(
    angle = 80,
    hjust = 1,
    vjust = 0.5
  ),
  axis.title.x = element_blank())
data %>%
  filter(price > 0) %>%
  group_by(neighbourhood) %>% mutate(count = n()) %>%
  filter(count <= 8) %>%
  ggplot(aes(x = reorder(neighbourhood, count))) +
  geom_bar() +
  theme(axis.text.x = element_text(
    angle = 80,
    hjust = 1,
    vjust = 0.5
  ),
  axis.title.x = element_blank())
data %>%
  group_by(neighbourhood) %>% mutate(count = n()) %>%
  filter(count > 150) %>%
  ggplot(aes(x = reorder(neighbourhood, count))) +
  geom_bar() +
  theme(axis.text.x = element_text(
    angle = 80,
    hjust = 1,
    vjust = 0.5
  ),
  axis.title.x = element_blank())

# Price vs Boroughs
data %>%
  rename(Boroughs = neighbourhood_group, Price = price) %>%
  ggplot(aes(x = Boroughs, y = Price, fill = Boroughs)) +
  geom_boxplot(show.legend = F) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6)

# log(Price) vs Boroughs with jitter
data %>%
  rename(Boroughs = neighbourhood_group, Price = price) %>%
  ggplot(aes(x = Boroughs, y = log(Price), fill = Boroughs)) +
  geom_boxplot(show.legend = F) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(
    color = "black",
    size = 0.3,
    alpha = 0.05,
    show.legend = F
  )

# log(Price) vs Boroughs
data %>%
  rename(Boroughs = neighbourhood_group, Price = price) %>%
  ggplot(aes(x = Boroughs, y = log(Price), fill = Boroughs)) +
  geom_boxplot(show.legend = F) +
  scale_fill_viridis(discrete = TRUE, alpha = 0.6)

# Price histogram & density for all Boroughs
data %>%
  rename(Boroughs = neighbourhood_group) %>%
  ggplot(aes(
    x = price,
    y = after_stat(density),
    fill = Boroughs
  )) +
  geom_histogram(binwidth = 2) +
  geom_density(alpha = .2) +
  theme_ipsum() +
  facet_wrap( ~ Boroughs, ncol = 1)

# log(Price) histogram & density for all Boroughs
data %>%
  rename(Boroughs = neighbourhood_group) %>%
  ggplot(aes(
    x = log(price),
    y = after_stat(density),
    fill = Boroughs
  )) +
  geom_histogram(binwidth = 0.05) +
  geom_density(alpha = .2) +
  theme_ipsum() +
  facet_wrap( ~ Boroughs, ncol = 1)

# new data
data2 <- read_csv(
  'listings-out.csv',
  col_select = c(
    beds,
    baths,
    bedrooms,
    neighborhood = neighbourhood,
    borough = neighbourhood_group,
    price,
    room_type,
    zip_code
  ),
  col_types = cols(
    beds = col_double(),
    baths = col_double(),
    bedrooms = col_double(),
    neighbourhood = col_factor(),
    neighbourhood_group = col_factor(),
    price = col_double(),
    room_type = col_factor(),
    zip_code = col_factor()
  )
) %>%
  filter(price > 0 &
           !is.na(baths) &
           !is.na(bedrooms) &
           !is.na(beds) & !is.na(zip_code)) %>% droplevels()

# correlation matrix
panel.cor <- function(x,
                      y,
                      digits = 2,
                      prefix = "",
                      cex.cor,
                      ...) {
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if (missing(cex.cor))
    cex.cor <- 0.8 / strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.25)#cex.cor * r)
}
pairs(mutate(data2, logprice = log(price))[c(1, 2, 3, 6, 9)],
      lower.panel = panel.cor)

# maxes
max(data2$baths)
max(data2$bedrooms)
max(data2$beds)
max(data2$price)

# model
source("DBDA2E-utilities.R")

# metric predictors
bedBathBeyond <- data2[, c('beds', 'baths', 'bedrooms')]

# nominal predictors
neighborhoodZipRoom <-
  data2[, c('neighborhood', 'zip_code', 'room_type')]

# hierarchical predictor
borough <- data2$borough

# predicted
price <- data2$price

mode_factor <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# temporary for testing
y <- price
metric <- bedBathBeyond
nominal <- neighborhoodZipRoom
hierarchy <- borough

genMCMC = function(y,
                   metric,
                   nominal,
                   hierarchy,
                   # data=myData , zName="Hits", NName="AtBats", sName="Player", cName="PriPos",
                   # data, yName, NName , xName ,
                   numSavedSteps = 50000,
                   thinSteps = 1,
                   saveName = NULL,
                   runjagsMethod = runjagsMethodDefault,
                   nChains = nChainsDefault) {
  #------------------------------------------------------------------------------
  # THE DATA.
  # Convert data file columns to generic x,y variable names for model:
  # y = as.numeric(datFrm[,yName])
  # x = as.numeric(as.factor(datFrm[,xName]))
  # N = as.numeric(datFrm[,NName])
  
  # nominal1Levels = levels(nominal[,1])
  # nominal2Levels = levels(nominal[,2])
  # nominal3Levels = levels(nominal[,3])
  # hierarchyLevels = levels(hierarchy)
  
  # xlevels = levels(as.factor(datFrm[,xName]))
  # Ntotal = length(y)
  # NxLvl = length(unique(x))
  linearModel <-
    lm(
      y ~ pull(nominal, 1) + pull(nominal, 2) + pull(nominal, 3) + pull(metric, 1) + pull(metric, 2) + pull(metric, 3)
    )
  residSD <- sqrt(mean(linearModel$residuals ^ 2))
  ySD <- sd(y)
  agammaShRa <-
    unlist(gammaShRaFromModeSD(mode = ySD / 2, sd = 2 * ySD))
  
  nominal <- sapply(nominal, as.numeric)
  metric <- as.matrix(metric)
  hierarchy <- as.numeric(hierarchy)
  nominal1Parent <-
    reframe(
      tibble(one = nominal[, 1], parent = hierarchy),
      parent = mode_factor(parent),
      .by = one
    ) %>%
    arrange(one)
  nominal2Parent <-
    reframe(
      tibble(one = nominal[, 2], parent = hierarchy),
      parent = mode_factor(parent),
      .by = one
    ) %>%
    arrange(one)
  
  # Specify the data in a list for sending to JAGS:
  dataList <-
    list(
      y = y,
      yMean = mean(y),
      ySD = ySD,
      n = length(y),
      metric = metric,
      metrics = ncol(metric),
      metricMean = apply(metric, mean),
      metricSD = apply(metric, sd),
      nominal = nominal,
      nominals = ncol(nominal),
      nominalLevels = apply(nominal, 2, max),
      nominal1Parent = nominal1Parent$parent,
      nominal2Parent = nominal2Parent$parent,
      hierarchy = hierarchy,
      hierarchyLevels = max(hierarchy),
      residSD = residSD,
      agammaShRa = agammaShRa
      # N = N ,
      # x = x ,
      # Ntotal = Ntotal ,
      # NxLvl = NxLvl
    )
  
  #------------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  initsList = NA
  
  #------------------------------------------------------------------------------
  # RUN THE CHAINS
  
  require(runjags)
  parameters = c(
    "mu",
    "hMu",
    "b0",
    "b1",
    "b2",
    "b3",
    "a4",
    "a4",
    "a6",
    "a1Sigma",
    "a2Sigma",
    "a3Sigma",
    "hSigma",
    "ySigma",
    "nu"
  )
  adaptSteps = 500
  burnInSteps = 1000
  
  runJagsOut <- run.jags(
    method = runjagsMethod ,
    model = "model.jags" ,
    monitor = parameters ,
    data = dataList ,
    inits = initsList ,
    n.chains = nChains ,
    adapt = adaptSteps ,
    burnin = burnInSteps ,
    sample = ceiling(numSavedSteps / nChains) ,
    thin = thinSteps ,
    summarise = FALSE ,
    plots = FALSE
  )
  codaSamples = as.mcmc.list(runJagsOut)
  # resulting codaSamples object has these indices:
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if (!is.null(saveName)) {
    save(codaSamples, file = paste(saveName, "Mcmc.Rdata", sep = ""))
  }
  return(codaSamples)
}

#===============================================================================

smryMCMC = function(codaSamples ,
                    datFrm = NULL ,
                    xName = NULL ,
                    contrasts = NULL ,
                    saveName = NULL) {
  # All single parameters:
  parameterNames = varnames(codaSamples)
  if (!is.null(datFrm) & !is.null(xName)) {
    xlevels = levels(as.factor(datFrm[, xName]))
  }
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples, chains = TRUE)
  for (parName in parameterNames) {
    summaryInfo = rbind(summaryInfo , summarizePost(mcmcMat[, parName]))
    thisRowName = parName
    if (!is.null(datFrm) & !is.null(xName)) {
      # For row name, extract numeric digits from parameter name. E.g., if
      # parameter name is "beta[12,34]" then pull out 12 and 34:
      levelVal = as.numeric(grep("^[1-9]" , # grep only substrings that begin with digits.
                                 # Return sll substrings split by "[" or "," or "]":
                                 unlist(strsplit(
                                   parName , "\\[|,|\\]"
                                 )) ,
                                 value = TRUE))
      if (length(levelVal) > 0) {
        # Assumes there is only a single factor, i.e., levelVal has only entry:
        thisRowName = paste(thisRowName, xlevels[levelVal])
      }
    }
    rownames(summaryInfo)[NROW(summaryInfo)] = thisRowName
  }
  # All contrasts:
  if (!is.null(contrasts)) {
    if (is.null(datFrm) | is.null(xName)) {
      show(" *** YOU MUST SPECIFY THE DATA FILE AND FACTOR NAMES TO DO CONTRASTS. ***\n")
    } else {
      # contrasts:
      if (!is.null(contrasts)) {
        for (cIdx in 1:length(contrasts)) {
          thisContrast = contrasts[[cIdx]]
          left = right = rep(FALSE, length(xlevels))
          for (nIdx in 1:length(thisContrast[[1]])) {
            left = left | xlevels == thisContrast[[1]][nIdx]
          }
          left = normalize(left)
          for (nIdx in 1:length(thisContrast[[2]])) {
            right = right | xlevels == thisContrast[[2]][nIdx]
          }
          right = normalize(right)
          contrastCoef = matrix(left - right , ncol = 1)
          # contrast on b[j]:
          postContrast = (mcmcMat[, paste("b[", 1:length(xlevels), "]", sep =
                                            "")]
                          %*% contrastCoef)
          summaryInfo = rbind(
            summaryInfo ,
            summarizePost(
              postContrast ,
              compVal = thisContrast$compVal ,
              ROPE = thisContrast$ROPE
            )
          )
          rownames(summaryInfo)[NROW(summaryInfo)] = (paste(
            "b:",
            paste(thisContrast[[1]], collapse = ""),
            ".v.",
            paste(thisContrast[[2]], collapse = ""),
            sep = ""
          ))
          # contrast on omega[j]:
          postContrast = (mcmcMat[, paste("omega[", 1:length(xlevels), "]", sep =
                                            "")]
                          %*% contrastCoef)
          summaryInfo = rbind(
            summaryInfo ,
            summarizePost(
              postContrast ,
              compVal = thisContrast$compVal ,
              ROPE = thisContrast$ROPE
            )
          )
          rownames(summaryInfo)[NROW(summaryInfo)] = (paste(
            "omega:",
            paste(thisContrast[[1]], collapse = ""),
            ".v.",
            paste(thisContrast[[2]], collapse = ""),
            sep = ""
          ))
          
          
        }
      }
    }
  }
  # Save results:
  if (!is.null(saveName)) {
    write.csv(summaryInfo , file = paste(saveName, "SummaryInfo.csv", sep =
                                           ""))
  }
  return(summaryInfo)
}

#===============================================================================


plotMCMC = function(codaSamples ,
                    datFrm ,
                    yName ,
                    xName ,
                    NName,
                    contrasts = NULL ,
                    saveName = NULL ,
                    saveType = "png") {
  mcmcMat = as.matrix(codaSamples, chains = TRUE)
  chainLength = NROW(mcmcMat)
  y = datFrm[, yName]
  x = as.numeric(as.factor(datFrm[, xName]))
  xlevels = levels(as.factor(datFrm[, xName]))
  N = datFrm[, NName]
  
  # Display data with posterior predictive distributions
  openGraph(width = min(10, 1.25 * length(xlevels)), height = 5)
  par(mar = c(3, 3, 2, 0.5)) # number of margin lines: bottom,left,top,right
  par(mgp = c(1.75, 0.5, 0)) # which margin lines to use for labels
  par(las = 3) # labels vertical
  par(mai = c(1.25, 0.75, 0.75, 0.25))
  plot(
    -1,
    0,
    xlim = c(0.1, length(xlevels) + 0.1) ,
    xlab = xName ,
    xaxt = "n" ,
    ylab = paste(yName, "/", NName) ,
    ylim = c(0, 1) ,
    main = "Data with Posterior Predictive Distrib."
  )
  axis(1 ,
       at = 1:length(xlevels) ,
       tick = FALSE ,
       lab = xlevels)
  for (xidx in 1:length(xlevels)) {
    xPlotVal = xidx
    yVals = y[x == xidx]
    nVals = N[x == xidx]
    points(
      rep(xPlotVal, length(yVals)) + runif(length(yVals),-0.05, 0.05) ,
      yVals / nVals ,
      pch = 1 ,
      cex = (0.25 + 1.75 * nVals / max(nVals)) ,
      col = "red"
    )
    chainSub = round(seq(1, chainLength, length = 20))
    for (chnIdx in chainSub) {
      o = mcmcMat[chnIdx, paste("omega[", xidx, "]", sep = "")]
      k = mcmcMat[chnIdx, paste("kappa", sep = "")]
      a = o * (k - 2) + 1
      b = (1 - o) * (k - 2) + 1
      blim = qbeta(c(0.025, 0.975) , a , b)
      yl = blim[1]
      yh = blim[2]
      ycomb = seq(yl, yh, length = 201)
      yb = dbeta(ycomb , a, b)
      yb = 0.67 * yb / max(yb)
      lines(xPlotVal - yb , ycomb , col = "skyblue")
    }
  }
  if (!is.null(saveName)) {
    saveGraph(file = paste(saveName, "PostPred", sep = ""),
              type = saveType)
  }
  
  if (!is.null(contrasts)) {
    if (is.null(datFrm) | is.null(xName)) {
      show(" *** YOU MUST SPECIFY THE DATA FILE AND FACTOR NAMES TO DO CONTRASTS. ***\n")
    } else {
      for (cIdx in 1:length(contrasts)) {
        thisContrast = contrasts[[cIdx]]
        left = right = rep(FALSE, length(xlevels))
        for (nIdx in 1:length(thisContrast[[1]])) {
          left = left | xlevels == thisContrast[[1]][nIdx]
        }
        left = normalize(left)
        for (nIdx in 1:length(thisContrast[[2]])) {
          right = right | xlevels == thisContrast[[2]][nIdx]
        }
        right = normalize(right)
        contrastCoef = matrix(left - right , ncol = 1)
        openGraph(height = 8, width = 4)
        layout(matrix(1:2, ncol = 1))
        # b contrast:
        postContrast = (mcmcMat[, paste("b[", 1:length(xlevels), "]", sep =
                                          "")]
                        %*% contrastCoef)
        plotPost(
          postContrast ,
          xlab = "Difference (in b)" ,
          main = paste0(
            "b: " ,
            paste(thisContrast[[1]], collapse = "."),
            "\nvs\n",
            paste(thisContrast[[2]], collapse = ".")
          ) ,
          compVal = thisContrast$compVal ,
          ROPE = thisContrast$ROPE
        )
        # omega contrast:
        postContrast = (mcmcMat[, paste("omega[", 1:length(xlevels), "]", sep =
                                          "")]
                        %*% contrastCoef)
        plotPost(
          postContrast ,
          xlab = "Difference (in omega)" ,
          main = paste0(
            "omega: " ,
            paste(thisContrast[[1]], collapse = "."),
            "\nvs\n",
            paste(thisContrast[[2]], collapse = ".")
          ) ,
          compVal = thisContrast$compVal ,
          ROPE = thisContrast$ROPE
        )
        if (!is.null(saveName)) {
          saveGraph(file = paste0(saveName, paste0(
            paste(thisContrast[[1]], collapse = ""),
            ".v.",
            paste(thisContrast[[2]], collapse = "")
          )),
          type = saveType)
        }
      }
    }
  } # end if ( !is.null(contrasts) )
}

contrasts = list(list(
  c("Manhattan"),
  c("Brooklyn"),
  compVal = 0.0,
  ROPE = NULL
),
list(
  c("Manhattan"),
  c("Queens"),
  compVal = 0.0,
  ROPE = NULL
))
fileNameRoot = "plot-"
graphFileType = "png"
#-------------------------------------------------------------------------------
# Generate the MCMC chain:
#startTime = proc.time()
mcmcCoda = genMCMC(
  price,
  bedBathBeyond,
  neighborhoodZipRoom,
  borough,
  numSavedSteps = 15000,
  thinSteps = 10,
  saveName = fileNameRoot
)
#stopTime = proc.time()
#elapsedTime = stopTime - startTime
#show(elapsedTime)
#-------------------------------------------------------------------------------
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names for reference
for (parName in c("b0", "b[1]", "omega[1]", "kappa")) {
  diagMCMC(
    codaObject = mcmcCoda ,
    parName = parName ,
    saveName = fileNameRoot ,
    saveType = graphFileType
  )
}
#-------------------------------------------------------------------------------
# Get summary statistics of chain:
summaryInfo = smryMCMC(
  mcmcCoda ,
  contrasts = contrasts ,
  datFrm = myData,
  xName = "PriPos",
  #yName="Hits", NName="AtBats",
  saveName = fileNameRoot
)
show(summaryInfo)
# Display posterior information:
plotMCMC(
  mcmcCoda ,
  contrasts = contrasts ,
  datFrm = myData,
  xName = "PriPos",
  yName = "Hits",
  NName = "AtBats",
  saveName = fileNameRoot ,
  saveType = graphFileType
)
#-------------------------------------------------------------------------------
