library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(hrbrthemes)

rm(list = ls())

myDataFrame <- read.csv('listings-out.csv') %>%
  select(
    beds,
    baths,
    bedrooms,
    neighborhood = neighbourhood,
    borough = neighbourhood_group,
    price,
    room_type,
    zip_code
  ) %>%
  filter(price > 0 &
           # !is.na(baths) &
           # !is.na(beds) &
           # !is.na(zip_code) &
           !is.na(bedrooms)) %>%
  mutate("log(price)" = log(price), borough = factor(borough))
# 'listings-out.csv',
#   col_select = c(
#     beds,
#     baths,
#     bedrooms,
#     neighborhood = neighbourhood,
#     borough = neighbourhood_group,
#     price,
#     room_type,
#     zip_code
#   ),
#   col_types = cols(
#     beds = col_double(),
#     baths = col_double(),
#     bedrooms = col_double(),
#     neighbourhood = col_factor(),
#     neighbourhood_group = col_factor(),
#     price = col_double(),
#     room_type = col_factor(),
#     zip_code = col_factor()
#   )
# ) %>%
#   filter(price > 0 &
#            !is.na(baths) &
#            !is.na(bedrooms) &
#            !is.na(beds) & !is.na(zip_code)) %>% droplevels()

# model
source("DBDA2E-utilities.R")

genMCMC = function(datFrm ,
                   yName = "y" ,
                   xNomName = "xNom" ,
                   xMetName = "xMet" ,
                   xParentName = "xParent",
                   numSavedSteps = 50000 ,
                   thinSteps = 1 ,
                   saveName = NULL ,
                   runjagsMethod = runjagsMethodDefault ,
                   nChains = nChainsDefault) {
  #------------------------------------------------------------------------------
  # THE DATA.
  # Convert data file columns to generic xNom,y variable names for model:
  y = as.numeric(datFrm[, yName])
  xNom = as.numeric(as.factor(datFrm[, xNomName]))
  xNomlevels = levels(as.factor(datFrm[, xNomName]))
  xMet = as.numeric(datFrm[, xMetName])
  # xParent = as.numeric(factor(datFrm[, xParentName]))
  xParent <- myDataFrame[c(xNomName, xParentName)] %>%
    group_by(neighborhood) %>%
    summarise(borough = names(which.max(table(borough))))
  xParent = as.numeric(factor(xParent$borough))
  xParentLevels = max(xParent)
  Ntotal = length(y)
  NxNomLvl = length(unique(xNom))
  # Compute scale properties of data, for passing into prior to make the prior
  # vague on the scale of the data.
  lmInfo = lm(datFrm[, yName] ~ datFrm[, xMetName] + datFrm[, xNomName])
  residSD = sqrt(mean(lmInfo$residuals ^ 2)) # residual root mean squared deviation
  # For hyper-prior on deflections:
  ySD <- sd(y)
  aGammaShRa = unlist(gammaShRaFromModeSD(mode = ySD / 2 , sd = 2 * ySD))
  # Specify the data in a list for sending to JAGS:
  dataList = list(
    y = y ,
    xNom = xNom ,
    xMet = xMet ,
    xMetMean = mean(xMet) ,
    borough = xParent,
    boroughs = xParentLevels,
    boroughMuPrior = c(-0.22223117, 0.08921023, 0.22495904, -0.06456669, -0.02737141),
    Ntotal = Ntotal ,
    NxNomLvl = NxNomLvl ,
    # data properties for scaling the prior:
    xMetSD = sd(xMet) ,
    yMean = mean(y) ,
    ySD = ySD,
    # residSD = residSD ,
    aGammaShRa = aGammaShRa
  )
  #------------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  initsList = list(
    a0 = mean(y) - lmInfo$coefficients["datFrm[, xMetName]"] * mean(datFrm[, xMetName]) ,
    a = unname(
      c(
        lmInfo$coef["(Intercept)"]
        + mean(datFrm[, xMetName]) * lmInfo$coef["datFrm[, xMetName]"] ,
        lmInfo$coef["(Intercept)"]
        + mean(datFrm[, xMetName]) * lmInfo$coef["datFrm[, xMetName]"]
        + lmInfo$coef[grep("xNomName" , names(lmInfo$coef))]
      )
    ) - mean(y) ,
    aSigma = sd(
      c(
        lmInfo$coef["(Intercept)"]
        + mean(datFrm[, xMetName]) * lmInfo$coef["datFrm[, xMetName]"] ,
        lmInfo$coef["(Intercept)"]
        + mean(datFrm[, xMetName]) * lmInfo$coef["datFrm[, xMetName]"]
        + lmInfo$coef[grep("xNomName" , names(lmInfo$coef))]
      )
    ) ,
    # ySigma = residSD ,
    ySigma = rep(residSD, NxNomLvl) ,
    aMet = unname(lmInfo$coefficients["datFrm[, xMetName]"])
    # Let JAGS do other parameters automatically...
  )
  #show( initsList )
  #------------------------------------------------------------------------------
  # RUN THE CHAINS
  
  parameters = c("boroughMu", "b0" ,  "b" , "aMet" , "aSigma" , "ySigma")
  adaptSteps = 500
  burnInSteps = 1000
  runJagsOut <- run.jags(
    method = runjagsMethod ,
    model = "model_boroughs_t.jags" ,
    # model = "model_boroughs_t_informed.jags" ,
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
  
  #   nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )
  #   # Create, initialize, and adapt the model:
  #   jagsModel = jags.model( "TEMPmodel.txt" , data=dataList , inits=initsList ,
  #                           n.chains=nChains , n.adapt=adaptSteps )
  #   # Burn-in:
  #   cat( "Burning in the MCMC chain...\n" )
  #   update( jagsModel , n.iter=burnInSteps )
  #   # The saved MCMC chain:
  #   cat( "Sampling final MCMC chain...\n" )
  #   codaSamples = coda.samples( jagsModel , variable.names=parameters ,
  #                               n.iter=nIter , thin=thinSteps )
  
  # resulting codaSamples object has these indices:
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if (!is.null(saveName)) {
    save(codaSamples , file = paste(saveName, "Mcmc.Rdata", sep = ""))
  }
  return(codaSamples)
}

#===============================================================================

smryMCMC = function(codaSamples ,
                    datFrm = NULL ,
                    xNomName = NULL ,
                    xMetName = NULL ,
                    xParentName = NULL ,
                    contrasts = NULL ,
                    saveName = NULL) {
  # All single parameters:
  parameterNames = varnames(codaSamples)
  if (!is.null(datFrm) & !is.null(xNomName)) {
    xNomlevels = levels(as.factor(datFrm[, xNomName]))
  }
  if (!is.null(datFrm) & !is.null(xParentName)) {
    xParentlevels = levels(as.factor(datFrm[, xParentName]))
  }
  summaryInfo = NULL
  mcmcMat = as.matrix(codaSamples, chains = TRUE)
  for (parName in parameterNames) {
    summaryInfo = rbind(summaryInfo , summarizePost(mcmcMat[, parName]))
    thisRowName = parName
    if (!is.null(datFrm) & !is.null(xNomName)) {
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
        thisRowName = paste(thisRowName, xNomlevels[levelVal])
      }
    }
    rownames(summaryInfo)[NROW(summaryInfo)] = thisRowName
  }
  # All contrasts:
  if (!is.null(contrasts)) {
    if (is.null(datFrm) | is.null(xNomName)) {
      show(" *** YOU MUST SPECIFY THE DATA FILE AND FACTOR NAMES TO DO CONTRASTS. ***\n")
    } else {
      # contrasts:
      if (!is.null(contrasts)) {
        for (cIdx in 1:length(contrasts)) {
          thisContrast = contrasts[[cIdx]]
          left = right = rep(FALSE, length(xNomlevels))
          for (nIdx in 1:length(thisContrast[[1]])) {
            left = left | xNomlevels == thisContrast[[1]][nIdx]
          }
          left = normalize(left)
          for (nIdx in 1:length(thisContrast[[2]])) {
            right = right | xNomlevels == thisContrast[[2]][nIdx]
          }
          right = normalize(right)
          contrastCoef = matrix(left - right , ncol = 1)
          postContrast = (mcmcMat[, paste("b[", 1:length(xNomlevels), "]", sep =
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
                    xNomName ,
                    xMetName ,
                    xParentName,
                    contrasts = NULL ,
                    saveName = NULL ,
                    saveType = "jpg") {
  mcmcMat = as.matrix(codaSamples, chains = TRUE)
  chainLength = NROW(mcmcMat)
  y = as.numeric(datFrm[, yName])
  xNom = as.numeric(as.factor(datFrm[, xNomName]))
  xNomlevels = levels(as.factor(datFrm[, xNomName]))
  xMet = as.numeric(datFrm[, xMetName])
  xParent = factor(datFrm[, xParentName])
  xParentLevels = levels(xParent)
  xParent = as.numeric(xParent)
  NxParentLevels = max(xParent)
  Ntotal = length(y)
  NxNomLvl = length(unique(xNom))
  
  # Display data with posterior predictive distributions:
  for (xNomLvlIdx in 1:NxNomLvl) {
    # Open blank graph with appropriate limits:
    xLim = c(min(xMet) - 0.2 * (max(xMet) - min(xMet)) ,
             max(xMet) + 0.2 * (max(xMet) - min(xMet)))
    yLim = c(min(y) - 0.2 * (max(y) - min(y)) ,
             max(y) + 0.2 * (max(y) - min(y)))
    openGraph(width = 4, height = 5)
    par(mar = c(3, 3, 3, 0.5)) # number of margin lines: bottom,left,top,right
    par(mgp = c(1.75, 0.5, 0)) # which margin lines to use for labels
    plot(
      2 * max(xLim),
      2 * max(yLim),
      # point out of range not seen
      xlab = xMetName ,
      xlim = xLim ,
      ylab = yName ,
      ylim = yLim ,
      main = paste(xNomlevels[xNomLvlIdx], "Data\nwith Post. Pred. Distrib.")
    )
    # plot credible regression lines and noise profiles:
    nSlice = 3
    curveXpos = seq(min(xMet), max(xMet), length = nSlice)
    curveWidth = (max(xMet) - min(xMet)) / (nSlice + 2)
    nPredCurves = 30
    for (i in floor(seq(
      from = 1,
      to = nrow(mcmcMat),
      length = nPredCurves
    ))) {
      intercept = mcmcMat[i, "b0"] + mcmcMat[i, paste0("b[", xNomLvlIdx, "]")]
      slope = mcmcMat[i, "aMet"]
      noise = mcmcMat[i, paste0("ySigma[", xNomLvlIdx, "]")]
      # noise = mcmcMat[i, "ySigma"]
      abline(a = intercept ,
             b = slope ,
             col = "skyblue")
      for (j in 1:nSlice) {
        hdiLo = intercept + slope * curveXpos[j] - 1.96 * noise
        hdiHi = intercept + slope * curveXpos[j] + 1.96 * noise
        yComb = seq(hdiLo , hdiHi , length = 75)
        xVals = dnorm(yComb ,
                      mean = intercept + slope * curveXpos[j] ,
                      sd = noise)
        xVals = curveWidth * xVals / max(xVals)
        lines(curveXpos[j] - xVals , yComb , col = "skyblue")
        lines(curveXpos[j] - 0 * xVals ,
              yComb ,
              col = "skyblue" ,
              lwd = 2)
      }
    }
    # plot data points:
    includeVec = (xNom == xNomLvlIdx)
    xVals = xMet[includeVec]
    yVals = y[includeVec]
    points(xVals ,
           yVals ,
           pch = 1 ,
           cex = 1.5 ,
           col = "red")
    
    
    if (!is.null(saveName)) {
      saveGraph(file = paste0(saveName, "PostPred-", xNomlevels[xNomLvlIdx]),
                type = saveType)
    }
  }
  
  # Display contrast posterior distributions:
  if (!is.null(contrasts)) {
    if (is.null(datFrm) | is.null(xNomName)) {
      show(" *** YOU MUST SPECIFY THE DATA FILE AND FACTOR NAMES TO DO CONTRASTS. ***\n")
    } else {
      for (cIdx in 1:length(contrasts)) {
        thisContrast = contrasts[[cIdx]]
        left = right = rep(FALSE, length(xNomlevels))
        for (nIdx in 1:length(thisContrast[[1]])) {
          left = left | xNomlevels == thisContrast[[1]][nIdx]
        }
        left = normalize(left)
        for (nIdx in 1:length(thisContrast[[2]])) {
          right = right | xNomlevels == thisContrast[[2]][nIdx]
        }
        right = normalize(right)
        contrastCoef = matrix(left - right , ncol = 1)
        postContrast = (mcmcMat[, paste("b[", 1:length(xNomlevels), "]", sep =
                                          "")]
                        %*% contrastCoef)
        openGraph(height = 8, width = 4)
        layout(matrix(1:2, ncol = 1))
        plotPost(
          postContrast ,
          xlab = "Difference" ,
          main = paste0(
            paste(thisContrast[[1]], collapse = "."),
            "\nvs\n",
            paste(thisContrast[[2]], collapse = ".")
          ) ,
          compVal = thisContrast$compVal ,
          ROPE = thisContrast$ROPE
        )
        plotPost(
          # postContrast / mcmcMat[, "ySigma"] ,
          postContrast / mcmcMat[, paste0("ySigma[", 1:length(xNomlevels), "]")] ,
          xlab = "Effect Size" ,
          main = paste0(
            paste(thisContrast[[1]], collapse = "."),
            "\nvs\n",
            paste(thisContrast[[2]], collapse = ".")
          ) ,
          compVal = 0.0 ,
          ROPE = c(-0.1, 0.1)
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

plotMCMC2 = function(codaSamples ,
                     data ,
                     sName = "s" ,
                     yName = "y" ,
                     compVal = 0.5 ,
                     rope = NULL ,
                     diffIdVec = NULL ,
                     compValDiff = 0.0 ,
                     ropeDiff = NULL ,
                     saveName = NULL ,
                     saveType = "jpg") {
  #-----------------------------------------------------------------------------
  # N.B.: This function expects the data to be a data frame,
  # with one component named y being a vector of integer 0,1 values,
  # and one component named s being a factor of subject identifiers.
  y = data[, yName]
  s = as.numeric(as.factor(data[, sName])) # ensures consecutive integer levels
  # Now plot the posterior:
  mcmcMat = as.matrix(codaSamples, chains = TRUE)
  chainLength = NROW(mcmcMat)
  # Plot b0, aMet:
  openGraph(width = 3.5 * 2, height = 3.0)
  par(mfrow = c(1, 2))
  par(mar = c(3.5, 3, 1, 1) , mgp = c(2.0, 0.7, 0))
  postInfo = plotPost(
    mcmcMat[, "b0"] ,
    compVal = NULL ,
    ROPE = NULL ,
    xlab = expression(beta[0]) ,
    main = "" ,
    xlim = c(min(mcmcMat[, "b0"]),
             quantile(mcmcMat[, "b0"], probs = c(0.990)))
  )
  postInfo = plotPost(
    mcmcMat[, "aMet"] ,
    compVal = compVal ,
    ROPE = rope ,
    xlab = expression(beta[bedrooms]) ,
    #main="Group Mode" ,
    xlim = quantile(mcmcMat[, "aMet"], probs = c(0.005, 0.995))
  )
  if (!is.null(saveName)) {
    saveGraph(file = paste(saveName, "PostaMet", sep = ""),
              type = saveType)
  }
  # Plot individual theta's and differences:
  if (!is.null(diffIdVec)) {
    Nidx = length(diffIdVec)
    openGraph(width = 2.5 * Nidx, height = 2.0 * Nidx)
    par(mfrow = c(Nidx, Nidx))
    for (t1Idx in 1:Nidx) {
      for (t2Idx in 1:Nidx) {
        parName1 = paste0("b[", diffIdVec[t1Idx], "]")
        parName2 = paste0("b[", diffIdVec[t2Idx], "]")
        if (t1Idx > t2Idx) {
          # plot.new() # empty plot, advance to next
          par(
            mar = c(3.5, 3.5, 1, 1) ,
            mgp = c(2.0, 0.7, 0) ,
            pty = "s"
          )
          nToPlot = 700
          ptIdx = round(seq(1, chainLength, length = nToPlot))
          plot (
            mcmcMat[ptIdx, parName2] ,
            mcmcMat[ptIdx, parName1] ,
            cex.lab = 1.75 ,
            xlab = parName2 ,
            ylab = parName1 ,
            col = "skyblue"
          )
          abline(0, 1, lty = "dotted")
        } else if (t1Idx == t2Idx) {
          par(
            mar = c(3.5, 1, 1, 1) ,
            mgp = c(2.0, 0.7, 0) ,
            pty = "m"
          )
          postInfo = plotPost(
            mcmcMat[, parName1] ,
            cex.lab = 1.75 ,
            compVal = compVal ,
            ROPE = rope ,
            cex.main = 1.5 ,
            xlab = parName1 ,
            main = ""
          )
          includeRows = (s == diffIdVec[t1Idx]) # rows of this subject in data
          dataPropor = sum(y[includeRows]) / sum(includeRows)
          # points( dataPropor , 0 , pch="+" , col="red" , cex=3 )
        } else if (t1Idx < t2Idx) {
          par(
            mar = c(3.5, 1, 1, 1) ,
            mgp = c(2.0, 0.7, 0) ,
            pty = "m"
          )
          postInfo = plotPost(
            mcmcMat[, parName1] - mcmcMat[, parName2] ,
            compVal = compValDiff ,
            ROPE = ropeDiff ,
            cex.main = 1.5 ,
            xlab = paste0(parName1, "-", parName2) ,
            main = "" ,
            cex.lab = 1.75
          )
          includeRows1 = (s == diffIdVec[t1Idx]) # rows of this subject in data
          dataPropor1 = sum(y[includeRows1]) / sum(includeRows1)
          includeRows2 = (s == diffIdVec[t2Idx]) # rows of this subject in data
          dataPropor2 = sum(y[includeRows2]) / sum(includeRows2)
          # points( dataPropor1-dataPropor2 , 0 , pch="+" , col="red" , cex=3 )
        }
      }
    }
  }
  if (!is.null(saveName)) {
    saveGraph(file = paste(saveName, "PostB", sep = ""),
              type = saveType)
  }
}

graphics.off() # This closes all of R's graphics windows.
#-------------------------------------------------------------------------------
# Load The data file

# Specify the column names in the data file relevant to the analysis:
yName = "log(price)"
xNomName = "neighborhood"
xMetName = "bedrooms"
xParentName = "borough"
# Specify desired contrasts.
# Each main-effect contrast is a list of 2 vectors of level names,
# a comparison value (typically 0.0), and a ROPE (which could be NULL):
contrasts = list(
  list(
    c("Harlem") ,
    c("Midtown") ,
    compVal = 0.0 ,
    ROPE = c(-0.05, 0.05)
  ))
  # , list(
  #   c("Upper East Side") ,
  #   c("Harlem") ,
  #   compVal = 0.0 ,
  #   ROPE = c(-0.05, 0.05)
  # )
  # ,list(
  #   c("Manhattan") ,
  #   c("Brooklyn") ,
  #   compVal = 0.0 ,
  #   ROPE = c(-1.5, 1.5)
  # ),list(
  #   c("Manhattan") ,
  #   c("Queens") ,
  #   compVal = 0.0 ,
  #   ROPE = c(-1.5, 1.5)
  # ))
  # Specify filename root and graphical format for saving output.
  # Otherwise specify as NULL or leave saveName and saveType arguments
  # out of function calls.
  fileNameRoot = "kenparables_boroughs-t-priors-"
  graphFileType = "png"
  
  mcmcCoda = genMCMC(
    datFrm = myDataFrame ,
    yName = yName ,
    xNomName = xNomName ,
    xMetName = xMetName ,
    xParentName = xParentName,
    numSavedSteps = 1000 ,
    thinSteps = 10 ,
    saveName = fileNameRoot,
    nChains = 10
  )
  #-------------------------------------------------------------------------------
  # Display diagnostics of chain, for specified parameters:
  parameterNames = varnames(mcmcCoda)
  show(parameterNames) # show all parameter names, for reference
  # for (parName in parameterNames) {
  for (parName in c(
    'b[16]',
    'b[75]',
    'b[149]',
    'ySigma[16]',
    'ySigma[75]',
    'ySigma[149]',
    'aMet',
    'b0',
    'boroughMu[1]',
    'boroughMu[2]',
    'boroughMu[3]',
    'boroughMu[4]',
    'boroughMu[5]'
  )) {
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
    datFrm = myDataFrame ,
    xNomName = xNomName ,
    xMetName = xMetName ,
    xParentName = xParentName,
    # contrasts = contrasts ,
    saveName = fileNameRoot
  )
  show(summaryInfo)
  # Display posterior information:
  plotMCMC(
    mcmcCoda ,
    datFrm = myDataFrame ,
    yName = yName ,
    xNomName = xNomName ,
    xMetName = xMetName ,
    xParentName = xParentName,
    # contrasts = contrasts ,
    saveName = fileNameRoot ,
    saveType = graphFileType
  )
  #-------------------------------------------------------------------------------
  # plot Bellarose, Fort Greene, and Prospect Heights
  plotMCMC2(
    mcmcCoda ,
    data = myDataFrame ,
    yName = yName ,
    sName = 'neighborhood' ,
    compVal = 0.0,
    diffIdVec = c(16, 75, 149),
    compValDiff = 0.0,
    saveName = fileNameRoot ,
    saveType = graphFileType
  )
  
  plotPosterior <- function(codaSamples ,
                            # data ,
                            compVal = 0.0 ,
                            rope = NULL ,
                            diffIdVec = NULL ,
                            compValDiff = 0.0 ,
                            ropeDiff = NULL ,
                            saveName = NULL ,
                            saveType = "png",
                            parName = "boroughMu[1]") {
    # y = data[,yName]
    # s = as.numeric(as.factor(data[,sName])) # ensures consecutive integer levels
    # Now plot the posterior:
    mcmcMat = as.matrix(codaSamples, chains = TRUE)
    openGraph(width = 5, height = 4)
    par(mar = c(3.5, 1, 1, 1) ,
        mgp = c(2.0, 0.7, 0) ,
        pty = "m")
    postInfo = plotPost(
      mcmcMat[, parName] ,
      cex.lab = 1.75 ,
      compVal = compVal ,
      ROPE = rope ,
      cex.main = 1.5 ,
      xlab = parName ,
      main = ""
    )
    if (!is.null(saveName)) {
      saveGraph(file = paste(saveName, "Posterior-", parName, sep = ""),
                type = saveType)
    }
  }
  for (parName in c('boroughMu[1]',
                    'boroughMu[2]',
                    'boroughMu[3]',
                    'boroughMu[4]',
                    'boroughMu[5]')) {
    plotPosterior(
      mcmcCoda ,
      compVal = 0.0,
      compValDiff = 0.0,
      saveName = fileNameRoot ,
      saveType = graphFileType,
      parName = parName
    )
  }
  