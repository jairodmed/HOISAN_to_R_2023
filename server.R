#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(gridGraphics)
library(ggrepel)
library(svglite)
library(grid)
library(foreign)
library(shinyjs)
library(shinydashboard)
library(Rttf2pt1)
library(readxl)
library(extrafont)
library(extrafontdb)
library(purrr)
library(vcd)
library("graphics")
library(igraph)
library(qgraph)
library(DiagrammeR)
library(magicfor)
library(igraphdata, pos=4)
library(tcltk, pos=4)
library(rgl, pos=4)
library(ape, pos=4)
library(vcd)
library(irr)


sdis_sequential <- function(data, labels = NULL, lag = 1, adjacent = TRUE,
                            onezero = NULL, tailed = 2, permtest = FALSE, nperms = 10) {
    
    cat('\n\nLag Sequential Analysis\n')
    
    if (!adjacent) {
        adjacent <- 0
    } else if (adjacent && is.null(onezero)) {
        adjacent <- 1
    } else if (adjacent && !is.null(onezero)) {
        adjacent <- 2
    }
    
    if (!is.matrix(data)) data <- matrix(data, ncol = 1)
    
    
    # if data is a frequency transition matrix
    if (nrow(data) == ncol(data)) {
        datais <- 2
        ncodes <- ncol(data)
        freqs <- data
        if (is.null(labels)) {
            labels <- 1:ncodes
            for (lupe in 1:ncodes) labels[lupe] <- paste("Code", lupe)
        }
    }
    
    
    # if dataset is NOT a frequency transition matrix
    if (!(nrow(data) == ncol(data))) {
        datais <- 1
        
        data <- matrix(data, ncol = 1)
        
        # are all data values numeric? any problems?
        if ((all(sapply(data, is.numeric))) == TRUE) {
            codesmin <- min(data)
            codesmax <- max(data)
            codefreqs <- table(data)
            cat("\n\nThe code frequencies:\n\n")
            print(codefreqs)
            if (codesmin != 1 || (codesmax > length(codefreqs)) ||
                (min(codefreqs) == 0)) {
                warning("The entered data is numeric but there is a problem:",
                        "\n\t-- the minimum code value should be 1,",
                        "\n\t-- the set of possible code values should be consecutive",
                        "integers",
                        "\n\t-- all code frequencies should be 1",
                        "\nAt least one of these conditions has not been met",
                        " which will cause problems.")
            }
            
            ncodes <- max(data)
            
            if (is.null(labels)) {
                labels <- 1:ncodes
                for (lupe in 1:ncodes) labels[lupe] <- paste(" Code", lupe)
            }
        }
        
        # if any data values are characters, treat them all as strings & provide numeric values for the analyses
        if (any(sapply(data, is.character))) {
            labels <- unique(data)
            for (lupe in 1:length(data)) data[lupe, 1] <- which(labels == data[
                lupe,
                1
            ], arr.ind = F)
            data <- as.matrix(as.numeric(data))
            ncodes <- max(data)
        }
        
        # transitional frequency matrix.
        freqs <- matrix(0, ncodes, ncodes)
        for (c in 1:nrow(data)) {
            if (c + lag <= nrow(data)) {
                freqs[data[c], data[c + lag]] <- freqs[data[c], data[c + lag]] + 1
            }
        }
        if (lag < -1){
            for (i in lag) {
                freqs[data[i],data[i-lag]] <- freqs[data[i],data[i-lag]]
                for (i in 0:-(lag +1)) {
                    freqs[data[i],] <- freqs[data[i],] - 1
                }}}
    }
    
    # Warning message for specification error.
    if ((adjacent == 0) & (any(diag(freqs) == 0))) {
        warning("\n\nYou have indicated that adjacent codes can never repeat", 
                "\n(ajdacent = 0), yet repeating values have been found in the data.",
                "\nSee the main diagonal frequency matrix.",
                "\nThis will result in faulty computations for LRX2,",
                "\nz values, and adjusted residuals.\n\n")
    }
    
    # initializing.
    lrx2t <- matrix(0)
    rowtots <- matrix(rowSums(freqs), ncol = 1)
    coltots <- matrix(colSums(freqs), nrow = 1)
    ntrans <- sum(rowtots)
    prows <- rowtots / ntrans
    pcols <- coltots / ntrans
    tprob <- matrix(-9999, ncodes, ncodes)
    et <- matrix(-9999, ncodes, ncodes)
    expfreq <- matrix(-9999, ncodes, ncodes)
    zadjres <- matrix(-9999, ncodes, ncodes)
    pzadjres <- matrix(1, ncodes, ncodes)
    yulesq <- matrix(-9999, ncodes, ncodes)
    var <- matrix(-9999, ncodes, ncodes)
    min <- matrix(-9999, ncodes, ncodes)
    kappa <- matrix(-9999, ncodes, ncodes)
    zkappa <- matrix(-9999, ncodes, ncodes)
    pzkappa <- matrix(1, ncodes, ncodes)
    signs <- matrix(0, ncodes, ncodes)
    n <- ntrans + 1
    nr <- rowtots
    
    if (datais == 1) {
        nr[data[nrow(data), 1]] <- nr[data[nrow(data), 1]] + 1
    }
    
    
    for (i in 1:ncodes) {
        for (j in 1:ncodes) {
            
            # Note: more refined computations for when adjacent codes cannot repeat appear below,
            # after the above 2 loops are completed.
            if ((lag == 1 || lag == -1) && adjacent == 0 & (ntrans - rowtots[i]) > 0) {
                pcols[j] <- coltots[j] / ntrans 
            }
            if ((lag == 1 || lag == -1) && adjacent == 0 & (ntrans - rowtots[j]) > 0) {
                expfreq[i, j] <- (rowtots[i] * coltots[j]) / (ntrans - rowtots[j])
            }
            if ((lag == 1 || lag == -1) && adjacent == 0 & (n - nr[i]) > 0) {
                et[i, j] <- (nr[i] * nr[j]) / (n - nr[i])
            }
            if ((lag > 1 || lag < -1) && adjacent == 0 & (n - nr[i]) > 0) {
                et[i, j] <- (nr[i] * nr[j]) / n
                expfreq[i, j] <- (rowtots[i] * coltots[j]) / ntrans
            }
            
            if (adjacent == 1) {
                et[i, j] <- (nr[i] * nr[j]) / n
                expfreq[i, j] <- (rowtots[i] * coltots[j]) / ntrans
            }
            
            # transitional probabilities.
            if (rowtots[i] > 0) {
                tprob[i, j] <- freqs[i, j] / rowtots[i]
            }
            
            # tablewise LRX2
            if (freqs[i, j] > 0 & expfreq[i, j] > 0) {
                lrx2t <- lrx2t + 2 * (freqs[i, j] * log(freqs[i, j] / expfreq[i, j]))
            }
            
            # adjusted residuals (z values) & sig levels
            if ((expfreq[i, j] * (1 - pcols[j]) * (1 - prows[i])) > 0) {
                zadjres[i, j] <- (freqs[i, j] - expfreq[i, j]) / sqrt(expfreq[i, j] * (1 - pcols[j]) * (1 - prows[i]))
                pzadjres[i, j] <- (1 - pnorm(abs(zadjres[i, j]))) * tailed
            }
            
            # Yule's Q.
            a <- freqs[i, j]
            b <- rowtots[i] - freqs[i, j]
            c <- coltots[j] - freqs[i, j]
            d <- ntrans - rowtots[i] - coltots[j] + freqs[i, j]
            if ((a * d + b * c) > 0) {
                yulesq[i, j] <- (a * d - b * c) / (a * d + b * c)
            }
            
            # kappas, z values & sig levels.
            var[i, j] <- (nr[i] * nr[j] * (n - nr[j]) * (n - nr[i])) / (n^2 * (n - 1))
            if (var[i, j] > 0) {
                zkappa[i, j] <- (freqs[i, j] - et[i, j]) / sqrt(var[i, j])
                if (nr[i] <= nr[j]) {
                    min[i, j] <- nr[i]
                } else {
                    min[i, j] <- nr[j]
                }
                if (min[i, j] - et[i, j] != 0) {
                    kappa[i, j] <- (freqs[i, j] - et[i, j]) / (min[i, j] - et[i, j])
                    if (kappa[i, j] < 0) {
                        kappa[i, j] <- (freqs[i, j] - et[i, j]) / et[i, j]
                    }
                    pzkappa[i, j] <- (1 - pnorm(abs(zkappa[i, j]))) * tailed
                }
            }
            
            # signs
            if (freqs[i, j] > expfreq[i, j]) {
                signs[i, j] <- 1
            } else if (freqs[i, j] < expfreq[i, j]) {
                signs[i, j] <- (-1)
            }
        }
    }
    
    if ((lag == 1 || lag == -1) && (adjacent == 0) || (adjacent == 2)) {
        
        # maximum likelihood estimation of the expected cell frequencies using iterative proportional fitting (Wickens, 1989, pp. 107-112).
        
        rsumsf <- rowSums(freqs)
        csumsf <- colSums(freqs)
        
        if (is.null(onezero)) {
            onezero <- matrix(1, ncodes, ncodes)
            diag(onezero) <- 0
        }
        
        expfreq <- onezero
        
        for (ipfloop in 1:100) {
            
            # adjusting by row.
            xr <- matrix(0, ncodes, 1)
            rsumse <- rowSums(expfreq)
            for (r in 1:ncodes) {
                if (rsumse[r] > 0) {
                    xr[r] <- rsumsf[r] / rsumse[r]
                }
            }
            for (i in 1:ncodes) {
                for (j in 1:ncodes) {
                    if (onezero[i, j] == 1) {
                        expfreq[i, j] <- expfreq[i, j] * xr[i]
                    }
                }
            }
            
            # adjusting by column.
            xc <- matrix(0, 1, ncodes)
            csumse <- colSums(expfreq)
            for (c in 1:ncodes) {
                if (csumse[c] > 0) {
                    xc[c] <- csumsf[c] / csumse[c]
                }
            }
            for (i in 1:ncodes) {
                for (j in 1:ncodes) {
                    if (onezero[i, j] == 1) {
                        expfreq[i, j] <- expfreq[i, j] * xc[j]
                    }
                }
            }
            
            rdiffs <- rsumsf - rowSums(expfreq)
            cdiffs <- csumsf - colSums(expfreq)
            if ((max(rdiffs) < 1e-04) & (max(cdiffs) < 1e-04)) {
                break
            }
        }
        
        cat("\nMaximum likelihood estimation of the expected cell frequencies using iterative proportional fitting ")
        if ((max(rdiffs) < 1e-04) & (max(cdiffs) < 1e-04)) {
            cat(
                "converged after the following number of iterations:", ipfloop,
                "\n\n"
            )
        } else {
            cat(
                "did NOT converge after the following number of iterations:", ipfloop,
                "\n\n"
            )
        }
        
        # tablewise LRX2
        lrx2t <- matrix(0)
        for (i in 1:ncodes) {
            for (j in 1:ncodes) {
                if ((freqs[i, j] > 0) & (expfreq[i, j] > 0)) {
                    lrx2t <- lrx2t + 2 * (freqs[i, j] * log(freqs[i, j] / expfreq[i, j]))
                }
            }
        }
        
        # adjusted residuals for matrices with structural zeros (Christensen, 1997, p. 357).
        
        # constructing the design matrix.
        x <- matrix(1, ncodes^2, 1)
        y <- matrix(0, ncodes^2, ncodes - 1)
        z <- matrix(0, ncodes^2, ncodes - 1)
        for (i in 1:(ncodes - 1)) {
            for (j in 1:ncodes) {
                y[i * ncodes + j, i] <- 1
                z[(((j - 1) * ncodes) + (i + 1)), i] <- 1
            }
        }
        des1 <- cbind(x, y, z)
        
        # pruning values corresponding to cells with structural zeros.
        onezero2 <- matrix(t(onezero), ncodes^2, 1)
        dm1 <- matrix(t(expfreq), ncodes^2, 1)
        dm2 <- matrix(-9999, 1, 1)
        des2 <- matrix(-9999, 1, ncol(des1))
        for (pp in 1:(ncodes^2)) {
            if (onezero2[pp] == 1) {
                dm2 <- rbind(dm2, dm1[pp])
                des2 <- rbind(des2, des1[pp, ])
            }
        }
        dm2 <- dm2[2:nrow(dm2), 1]
        des2 <- des2[2:nrow(des2), ]
        
        dm2 <- diag(dm2)
        if (det(t(des2) %*% dm2 %*% des2) != 0) {
            zadjres <- matrix(0, ncodes, ncodes)
            a <- des2 %*% (solve(t(des2) %*% dm2 %*% des2)) %*% t(des2) %*% dm2
            acounter <- 1
            for (i in 1:ncodes) {
                for (j in 1:ncodes) {
                    if (onezero[i, j] != 0) {
                        zadjres[i, j] <- (freqs[i, j] - expfreq[i, j]) / sqrt(expfreq[i, j] * (1 - pcols[j]) * (1 - prows[i]))
                        acounter <- acounter + 1
                    }
                }
            }
        } else {
            warning("\n\nA nonsingular matrix has been identified, which means that proper",
                    "\nadjusted residuals cannot be computed for this data, probably",
                    "\nbecause there are no values for one or more codes. Try recoding",
                    "\nusing sequential integers, and redo the analyses. The adjusted",
                    "\nresiduals that are printed below are based on equation 5 from",
                    "\nBakemand & Quera (1995, p. 274), and are close approximations",
                    "\nto the proper values. The procedures recommended by Bakemen &",
                    "\nQuera (1995, p. 276), Haberman (1979), and Christensen (1997)",
                    "\ncannot be conducted with nonsingular matrices.\n\n")
        }
        
        for (i in 1:ncodes) {
            for (j in 1:ncodes) {
                if (onezero[i, j] == 0) {
                    zadjres[i, j] <- 0
                    yulesq[i, j] <- 0
                    kappa[i, j] <- 0
                    zkappa[i, j] <- 0
                    pzadjres[i, j] <- 1
                    pzkappa[i, j] <- 1
                }
            }
        }
    }
    
    b <- labels[1:ncodes]
    bb <- c(b, "Totals")
    cat("\n\nRequested 'tail' (1 or 2) for Significance Tests =", tailed, "\n")
    
    cfreq <- rbind(cbind(freqs, rowtots), cbind(coltots, ntrans))
    rownames(cfreq) <- bb
    colnames(cfreq) <- bb
    cat("\n\nCell Frequencies, Row & Column Totals, & N\n\n")
    cfreq <- cfreq[,order(colnames(cfreq))]
    cfreq <- cfreq[order(rownames(cfreq)),]
    print(cfreq)
    
    
    if (lag == 1 && adjacent == 0 || adjacent == 2) {
        cat("\nThe processed ONEZERO matrix appears below. In the ONEZERO matrix,",
            "\na 0 indicates a structural zero, and a 1 indicates that an expected cell",
            "\nfrequency will be estimated.",
            "\n\nONEZERO matrix:\n")
        rownames(onezero) <- b
        colnames(onezero) <- b
        print(onezero)
    }
    
    cat("\n\nExpected Values/Frequencies\n\n")
    rownames(expfreq) <- b
    colnames(expfreq) <- b
    expfreq <- expfreq[,order(colnames(expfreq))]
    expfreq <- expfreq[order(rownames(expfreq)),]
    print(round(expfreq, 3))
    
    cat("\n\nTransitional Probabilities\n\n")
    rownames(tprob) <- b
    colnames(tprob) <- b
    tprob <- tprob[,order(colnames(tprob))]
    tprob <- tprob[order(rownames(tprob)),]
    print(round(tprob, 2))
    
    if (adjacent == 1) {
        df <- (ncodes - 1)^2
    } else {
        df <- (ncodes - 1)^2 - (ncodes^2 - sum(onezero))
    }
    
    plrx2t <- 1 - pchisq(abs(lrx2t), df)
    tlr <- cbind(lrx2t, df, plrx2t)
    cat("\n\nTablewise Likelihood Ratio (Chi-Square) test = ",round(lrx2t,2),
        ",  df = ",df,",  p = ",round(plrx2t,5),"\n",sep='')
    
    cat("\n\nAdjusted Residuals\n\n")
    rownames(zadjres) <- b
    colnames(zadjres) <- b
    zadjres <- zadjres[,order(colnames(zadjres))]
    zadjres <- zadjres[order(rownames(zadjres)),]
    print(round(zadjres, 2))
    
    cat("\n\nSignificance Levels for the Adjusted Residuals\n\n")
    rownames(pzadjres) <- b
    colnames(pzadjres) <- b
    pzadjres <- pzadjres[,order(colnames(pzadjres))]
    pzadjres <- pzadjres[order(rownames(pzadjres)),]
    print(round(pzadjres, 4))
    
    cat("\n\nYule's Q Values\n\n")
    rownames(yulesq) <- b
    colnames(yulesq) <- b
    print(round(yulesq, 2))
    
    cat("\n\nUnidirectional Kappas\n\n")
    rownames(kappa) <- b
    colnames(kappa) <- b
    print(round(kappa, 2))
    
    cat("\n\nz values for the Unidirectional Kappas\n\n")
    rownames(zkappa) <- b
    colnames(zkappa) <- b
    print(round(zkappa, 2))
    
    cat("\n\nSignificance Levels for the Unidirectional Kappas\n\n")
    rownames(pzkappa) <- b
    colnames(pzkappa) <- b
    print(round(pzkappa, 4))
    
    rownames(freqs) <- b
    colnames(freqs) <- b
    freqs <- freqs[,order(colnames(freqs))]
    freqs <- freqs[order(rownames(freqs)),]
    
    # Permutation tests of significance
    
    if (permtest && datais == 2) {
        warning("\n\nYou have requested permutation tests of significance be computed",
                "\n(permtest = TRUE) but the data is a frequency transition matrix.",
                "\nPermutation tests can only be computed when data is a sequence",
                "\nof codes. Permutation tests will not be performed.\n\n")
    }
    
    if (permtest && datais == 1) {
        obs2 <- matrix(t(freqs), 1, nrow(freqs) * ncol(freqs))
        signs2 <- matrix(t(signs), 1, nrow(freqs) * ncol(freqs))
        sigs <- matrix(1, 1, nrow(freqs) * ncol(freqs))
        
        results <- matrix(-9999, nperms, nrow(freqs) * ncol(freqs))
        
        for (perm in 1:nperms) {
            
            # permuting the sequences; algorithm from Castellan 1992.
            
            # when adjacent codes may be the same.
            datap <- data
            if (adjacent == 1) {
                for (i in 1:(nrow(datap) - 1)) {
                    kay <- as.integer((nrow(datap) - i + 1) * runif(1) + 1) + i - 1
                    d <- datap[i]
                    datap[i] <- datap[kay]
                    datap[kay] <- d
                }
                datap <- matrix(sample(data), nrow(data), 1)
            }
            
            # when adjacent codes may NOT be the same.
            if (adjacent == 0 && (lag == 1 || lag == -1)) {
                datap <- rbind(0, data, 0)
                for (i in 2:(nrow(datap) - 2)) {
                    limit <- 10000
                    for (j in 1:limit) {
                        kay <- as.integer(((length(datap) - 1) - i + 1) * runif(1) + 1) + i - 1
                        if ((datap[i - 1] != datap[kay]) & (datap[i + 1] != datap[kay]) &
                            (datap[kay - 1] != datap[i]) & (datap[kay + 1] != datap[i])) {
                            break
                        }
                    }
                    d <- datap[i]
                    datap[i] <- datap[kay]
                    datap[kay] <- d
                }
                datap <- matrix(datap[2:(nrow(datap) - 1), ], ncol = 1)
            }
            
            # transitional frequency matrix for permuted data.
            freqsp <- matrix(0, ncodes, ncodes)
            for (c in 1:nrow(datap)) {
                if (c + lag <= nrow(datap)) {
                    freqsp[datap[c], datap[c + lag]] <- freqsp[datap[c], datap[c + lag]] + 1
                }
            }
            results[perm, ] <- matrix(t(freqsp), 1, nrow(freqs) * ncol(freqs))
        }
        
        
        # one-tailed
        for (j in 1:ncol(results)) {
            counter <- 0
            for (i in 1:nrow(results)) {
                if ((results[i, j] >= obs2[j]) & (signs2[j] > 0)) { 
                    counter <- counter + 1
                } else if ((results[i, j] <= obs2[j]) & (signs2[j] < 0)) {
                    counter <- counter + 1
                }
            }
            if (signs2[j] != 0) sigs[1, j] <- counter / nperms
        }
        
        
        cat("\n\nData Permutation Significance Levels (number of permutations = ", nperms, ")\n\n",sep='')
        sigs <- t(matrix(sigs, ncodes, ncodes))
        rownames(sigs) <- b
        colnames(sigs) <- b    
        print(sigs)
        
    }
    
    seq_output <- list(
        cfreq = cfreq, freqs = freqs, expfreqs = expfreq, probs = tprob, chi = tlr,
        adjres = zadjres, p = pzadjres, YulesQ = yulesq, kappas = kappa, z = zkappa,
        pk = pzkappa
    )
    
    return(seq_output)
}

# Define server logic 

server <- (function(input, output, session) {

#######   Register tab #######    

  #####   Date and time #####   
    output$currentTime <- renderText({
                          invalidateLater(as.integer(input$interval), session)
                          format(Sys.time())
                          })
    
#####   Dimensions   #####  
    
    output$dimension   <- renderText({ input$caption_1 })
    output$dimension_2 <- renderText({ input$caption_2 })
    output$dimension_3 <- renderText({ input$caption_3 })

    
    starttime <- NULL
    endtime   <- NULL
    action_1  <- NULL
    action_2  <- NULL

    observeEvent(input$start, {
      starttime <<- Sys.time()
    })
    
    observeEvent(input$end, {
      endtime <<- Sys.time()
    })
    
    observeEvent(input$categoria_1, {
      action_1 <<- Sys.time()
    })
    
    observeEvent(input$categoria_2, {
      action_2 <<- Sys.time()
    })

##       
    
    output$downloadData <- downloadHandler(
      filename = function() {
        "download.csv"
      },
      content = function(file) {
        start <- starttime
        end   <- endtime
        for (i in 1:input$categories){
          categoria_[i] <- action_[i]
        }
        data <- t(data.frame(start, 
                             categoria_1 = action_1, 
                             categoria_2 = action_2, 
                             end))
        row.names(data) <- c("start",input$categoria_1,input$categoria_2,"end")
        write.csv(data, file, row.names = T)
      }
    )
    
    #####   Categories   #####
    
    ## Definition
    
    output$categories <- renderUI({
      req(input$categories)
      lapply(1:(input$categories), function(i) {
        textInput(inputId = paste0 ("categoria_",i), 
                  label   = paste0 ("Category ", i, ":"),
                  value   = paste0 ("Indique el nombre de la Categoría ", i))
      })
    })
    
    ## Register
    
    output$my_button <- renderUI({
      req(input$categories)
      lapply(1:(input$categories), function(i) {
        actionButton(inputId = paste0("category_",i),
                     label   = input[[paste0("categoria_", i)]])
                     })
    })

##################################        
#######   End register tab #######
##################################
    
    rv <- reactiveValues(data = NULL)
    
    p <- reactive({
        Dataset = input$Dataset
        req(input$Dataset)
        req(!rv$clear)
        if (is.null(Dataset)) {
            return(NULL)
        } else if (input$upload_type== "xls") {
            sheet <- excel_sheets(Dataset$datapath)
            Dataset = as.data.frame(read_xlsx(Dataset$datapath,sheet= sheet,na="NeuN"))
            cat <- Dataset$Categoria
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
            
            updateSliderInput(session, "Scale_01", min = 0, max=round(max(escala_0)+0.5), step=0.01)
            
        } else if (input$upload_type== "csv") {
            Dataset = read.csv2(Dataset$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
            
            cat <- Dataset[,1]
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_0 <- gsub("[(*)]","",Dataset$Radio)
            escala_0 <- as.numeric(gsub(",",".", escala_0))
            escala_0 <- as.numeric(gsub("[(*)]","",Dataset$Longitud))
            
            updateSliderInput(session, "Scale_01", min = 0, max=round(max(escala_0)+0.5), step=0.01)
            
        } 
        
        updateSliderInput(session, "Scale_01", min = 0, max=round(max(escala_0)+0.5), step=0.01)
        
        polar <- structure(list(degree = as.numeric(gsub(",",".",Dataset$Angulo)), value = escala_0,cat=as.factor(Dataset$Categoria)), .Names = c("degree","value", "Categoría"), class = "data.frame", row.names = attr(Dataset, "row.names"))
        rbPal <- colorRampPalette(c('blue',"purple",'red'))
        ybreaks = pretty(x = polar$value, n = 3)
        rbPal <- colorRampPalette(c('blue',"purple",'red'))
        polar$Col <- rbPal(3)[cut(escala_0, breaks = c(0,1.96,2.58,Inf),right = FALSE)]
        data_sub=as.data.frame(subset(polar, value!=0))
        
        theme_polar <- function(yvals, xgeo = 133, ygeo = 0, 
                                color = "grey50", size = 1, 
                                ylab = "y",
                                textsize = 3,
                                ticks = 10,
                                ylimit = max(abs(yvals))
        ){
            #Add ticks programatically
            ticks_y <- ybreaks
            ticks_y_sig <- c(1.96,2.58)
            
            
            #Add axis
            theme.list <- 
                list(
                    ylim(0, ylimit),
                    annotate("text", x = 150, y = ticks_y_sig, color=c("purple","red"),size = textsize,label = ticks_y_sig)
                )
            
            #Add ticks of x axis
            nlist <- length(theme.list)
            
            #Add labels to the y-ticks
            theme.list[[nlist+2]] <- annotate("text", size = textsize,x = xgeo,y = ticks_y,color=color,label = paste(ticks_y))
            
            
            return(theme.list)
        }	
        
        
        ggplot(data_sub, aes(x=degree, y=value,color = data_sub$Col))+
            scale_x_continuous(breaks = seq(45, 360, 45), limits = c(0, 360),expand = c(0, 0))+
            coord_polar(theta = "x", start = 1.5 * pi, direction = -1) +
            geom_text(x = 45, y = input$Scale_01[2]*1.45, label = "I", color="black",size = 6) +
            geom_text(x = -45, y = input$Scale_01[2]*1.45, label = "IV", color="black",size = 6)+
            geom_text(x = -45, y = input$Scale_01[2]*-1.45, label = "II", color="black",size = 6)+
            geom_text(x = 45, y = input$Scale_01[2]*-1.45, label = "III", color="black",size = 6) +
            scale_color_manual(data_sub$Col,labels = c('p < .005', 'p < .001'))+
            geom_hline(aes(yintercept=1.96), colour="purple",size=0.1,linetype = 2)+
            geom_hline(aes(yintercept=2.58), colour="red",size=0.1,linetype = 2)+	
            geom_hline(yintercept = as.integer(ybreaks), colour = "black", size = 0.1,linetype = 2) +
            geom_hline(yintercept = input$Scale_01[2], colour = "black", size = 0.2,linetype = 1) +
            geom_vline(xintercept = seq(0, 360, by = 45), colour = "black", size = 0.1) +
            theme_bw()+
            theme_polar(yvals=input$Scale_01)+
            theme(panel.grid.major = element_blank())+
            geom_segment(aes(y=0, xend=degree, yend=value), arrow=arrow(length=unit(0.5,"cm")),colour=data_sub$Col,size=input$vector.width)+
            geom_text_repel(label = data_sub$Categoría,vjust = ifelse(data_sub$degree >= 180, 0.25, -0.25), hjust = ifelse(data_sub$degree >= 180, 0.25, -0.50), family=input$font.family,point.padding = NA,size = input$font.size,colour=data_sub$Col) +
            theme(panel.border = element_blank(),
                  axis.title = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text.x = element_text(size=11,color = "grey50"),
                  panel.grid  = element_blank())
    })
    
    
    p01 <- reactive({
        
        Dataset = input$Dataset
        req(input$Dataset)
        req(!rv$clear)
        if (is.null(Dataset)) {
            return(NULL)
        } else if (input$upload_type== "xls") {
            sheet <- excel_sheets(Dataset$datapath)
            Dataset = as.data.frame(read_xlsx(Dataset$datapath,sheet= sheet,na="NeuN"))
            cat <- Dataset$Categoria
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
            
        } else if (input$upload_type== "csv") {
            Dataset = read.csv2(Dataset$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
            
            cat <- Dataset[,1]
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_0 <- gsub("[(*)]","",Dataset$Radio)
            escala_0 <- as.numeric(gsub(",",".", escala_0))
            escala_0 <- as.numeric(gsub("[(*)]","",Dataset$Longitud))
            
        } 
        
        Dataset_02 = input$Dataset_02
        req(input$Dataset_02)
        req(!rv$clear)
        if (is.null(Dataset_02)) {
            return(NULL)
        } else if (input$upload_type_02== "xls") {
            sheet <- excel_sheets(Dataset_02$datapath)
            
            Dataset_02 = as.data.frame(read_xlsx(Dataset_02$datapath,sheet= sheet,na="NeuN"))
            
            cat <- Dataset_02$Categoria
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_01 <- as.numeric(gsub(",",".",Dataset_02[,6]))
            
        } else if (input$upload_type_02== "csv") {
            Dataset_02 = read.csv2(Dataset_02$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
            
            cat <- Dataset_02[,1]
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_01 <- gsub("[(*)]","",Dataset_02$Radio)
            escala_01 <- as.numeric(gsub(",",".", escala_01))
            escala_01 <- as.numeric(gsub("[(*)]","",Dataset_02$Longitud))
            
        } 
        
        updateSliderInput(session, "Scale_02", min = 0, max=round(max(c(escala_01,escala_0))+0.5))
        
        if (input$upload_type_02== "xls"){
            updateSliderInput(session, "Scale_01", min = 0, max=round(max(c(escala_01,escala_0))+0.5))}
        
        if (input$upload_type_02== "csv"){
            updateSliderInput(session, "Scale_01", min = 0, max=round(max(c(escala_01,escala_0))+0.5))}
        
        polar_01 <- structure(list(degree = as.numeric(gsub(",",".",Dataset_02$Angulo)), value = escala_01, cat=as.factor(Dataset_02$Categoria)), .Names = c("degree","value", "Categoría"), class = "data.frame", row.names = attr(Dataset_02, "row.names"))
        rbPal <- colorRampPalette(c('blue',"purple",'red'))
        ybreaks = pretty(x = polar_01$value, n = 3)
        rbPal <- colorRampPalette(c('blue',"purple",'red'))
        polar_01$Col <- rbPal(3)[cut(escala_01, breaks = c(0,1.96,2.58,Inf),right = FALSE)]
        data_sub_01=as.data.frame(subset(polar_01, value!=0))
        
        theme_polar <- function(yvals, xgeo = 133, ygeo = 0, 
                                color = "grey50", size = 1, 
                                ylab = "y",
                                textsize = 3,
                                ticks = 10,
                                ylimit = max(abs(yvals))
        ){
            
            theme(panel.border = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),axis.text.x = element_text(size=11),panel.grid  = element_blank())
            #Add ticks programatically
            ticks_y <- ybreaks
            ticks_y_sig <- c(1.96,2.58)
            
            
            #Add axis
            theme.list <- 
                list(
                    ylim(0, ylimit),
                    annotate("text", x = 150, y = ticks_y_sig, color=c("purple","red"),size = textsize,label = ticks_y_sig)
                )
            
            #Add ticks of x axis
            nlist <- length(theme.list)
            
            #Add labels to the y-ticks
            theme.list[[nlist+2]] <- annotate("text", size = textsize,x = xgeo,y = ticks_y,color=color,label = paste(ticks_y))
            
            
            return(theme.list)
        }	
        ggplot(data_sub_01, aes(x=degree, y=value,color = data_sub_01$Col))+
            scale_x_continuous(breaks = seq(45, 360, 45), limits = c(0, 360),expand = c(0, 0))+
            coord_polar(theta = "x", start = 1.5 * pi, direction = -1) +
            geom_segment(aes(y=0, xend=degree, yend=value), arrow=arrow(length=unit(0.5,"cm")),colour=data_sub_01$Col,size=input$vector.width)+
            geom_text_repel(label = data_sub_01$Categoría,vjust = ifelse(data_sub_01$degree >= 180, 0.25, -0.25), hjust = ifelse(data_sub_01$degree >= 180, 0.25, -0.50), family=input$font.family,point.padding = NA,size = input$font.size,colour=data_sub_01$Col) +
            geom_text(x = 45,  y = input$Scale_02[2]* 1.45, label = "I",   color="black",size = 6) +
            geom_text(x = -45, y = input$Scale_02[2]* 1.45, label = "IV",  color="black",size = 6)+
            geom_text(x = -45, y = input$Scale_02[2]*-1.45, label = "II",  color="black",size = 6)+
            geom_text(x = 45,  y = input$Scale_02[2]*-1.45, label = "III", color="black",size = 6) +
            scale_color_manual(data_sub_01$Col,labels = c('p < .005', 'p < .001'))+
            geom_hline(aes(yintercept=1.96), colour="purple",size=0.1,linetype = 2)+
            geom_hline(aes(yintercept=2.58), colour="red",size=0.1,linetype = 2)+	
            geom_hline(yintercept = as.integer(ybreaks), colour = "black", size = 0.1,linetype = 2) +
            geom_hline(yintercept = input$Scale_02[2], colour = "black", size = 0.2,linetype = 1) +
            geom_vline(xintercept = seq(0, 360, by = 45), colour = "black", size = 0.1) +
            theme_bw(base_size = 8)+
            theme(panel.grid.major = element_blank())+
            theme(panel.border = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),axis.text.x = element_text(size=11),panel.grid  = element_blank())+
            theme_polar(yvals=input$Scale_02)
        
    })
    
    
    
    p02 <- reactive({
        Dataset = input$Dataset
        req(input$Dataset)
        req(!rv$clear)
        if (is.null(Dataset)) {
            return(NULL)
        } else if (input$upload_type== "xls") {
            sheet <- excel_sheets(Dataset$datapath)
            Dataset = as.data.frame(read_xlsx(Dataset$datapath,sheet= sheet,na="NeuN"))
            cat <- Dataset$Categoria
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
            
            
        } else if (input$upload_type== "csv") {
            Dataset = read.csv2(Dataset$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
            
            cat <- Dataset[,1]
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_0 <- gsub("[(*)]","",Dataset$Radio)
            escala_0 <- as.numeric(gsub(",",".", escala_0))
            escala_0 <- as.numeric(gsub("[(*)]","",Dataset$Longitud))
            
        } 
        
        Dataset_02 = input$Dataset_02
        req(input$Dataset_02)
        req(!rv$clear)
        if (is.null(Dataset_02)) {
            return(NULL)
        } else if (input$upload_type_02== "xls") {
            sheet <- excel_sheets(Dataset_02$datapath)
            
            Dataset_02 = as.data.frame(read_xlsx(Dataset_02$datapath,sheet= sheet,na="NeuN"))
            
            cat <- Dataset_02$Categoria
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_01 <- as.numeric(gsub(",",".",Dataset_02[,6]))
            
            
        } else if (input$upload_type_02== "csv") {
            Dataset_02 = read.csv2(Dataset_02$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
            
            cat <- Dataset_02[,1]
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_01 <- gsub("[(*)]","",Dataset_02$Radio)
            escala_01 <- as.numeric(gsub(",",".", escala_01))
            escala_01 <- as.numeric(gsub("[(*)]","",Dataset_02$Longitud))
            
        } 
        
        Dataset_03 = input$Dataset_03
        req(input$Dataset_03)
        req(!rv$clear)
        if (is.null(Dataset_03)) {
            return(NULL)
        } else if (input$upload_type_03== "xls") {
            sheet <- excel_sheets(Dataset_03$datapath)
            Dataset_03 = as.data.frame(read_xlsx(Dataset_03$datapath,sheet=sheet,na="NeuN"))
            escala_01 <- as.numeric(gsub(",",".",Dataset_02[,6]))
            escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
            escala_03 <- as.numeric(gsub(",",".",Dataset_03[,6]))
            
        } else if (input$upload_type_03== "csv") {
            Dataset_03 = read.csv2(Dataset_03$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
            
            cat <- Dataset_03[,1]
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala <- gsub("[(*)]","",Dataset_03$Radio)
            escala <- as.numeric(gsub(",",".", escala))
            escala <- as.numeric(gsub("[(*)]","",Dataset_03$Longitud))
            
        } 
        
        updateSliderInput(session, "Scale_03", min = 0, max=round(max(c(escala_03,escala_01,escala_0))+0.5))
        
        
        polar_02 <- structure(list(degree = as.numeric(gsub(",",".",Dataset_03$Angulo)), value = escala_03,cat=as.factor(Dataset_03$Categoria)), .Names = c("degree","value", "Categoría"), class = "data.frame", row.names = attr(Dataset_03, "row.names"))
        rbPal <- colorRampPalette(c('blue',"purple",'red'))
        ybreaks = pretty(x = polar_02$value, n = 3)
        rbPal <- colorRampPalette(c('blue',"purple",'red'))
        polar_02$Col <- rbPal(3)[cut(escala_03, breaks = c(0,1.96,2.58,Inf),right = FALSE)]
        data_sub_02=as.data.frame(subset(polar_02, value!=0))
        
        theme_polar <- function(yvals, xgeo = 133, ygeo = 0, 
                                color = "grey50", size = 1, 
                                ylab = "y",
                                textsize = 3,
                                ticks = 10,
                                ylimit = max(abs(yvals))
        ){
            
            theme(panel.border = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),axis.text.x = element_text(size=11),panel.grid  = element_blank())
            #Add ticks programatically
            ticks_y <- ybreaks
            ticks_y_sig <- c(1.96,2.58)
            
            
            #Add axis
            theme.list <- 
                list(
                    ylim(0, ylimit),
                    annotate("text", x = 150, y = ticks_y_sig, color=c("purple","red"),size = textsize,label = ticks_y_sig)
                )
            
            #Add ticks of x axis
            nlist <- length(theme.list)
            
            #Add labels to the y-ticks
            theme.list[[nlist+2]] <- annotate("text", size = textsize,x = xgeo,y = ticks_y,color=color,label = paste(ticks_y))
            
            
            return(theme.list)
        }	
        
        
        ggplot(data_sub_02, aes(x=degree, y=value,color = data_sub_02$Col))+
            scale_x_continuous(breaks = seq(45, 360, 45), limits = c(0, 360),expand = c(0, 0))+
            coord_polar(theta = "x", start = 1.5 * pi, direction = -1) +
            geom_segment(aes(y=0, xend=degree, yend=value), arrow=arrow(length=unit(0.5,"cm")),colour=data_sub_02$Col,size=input$vector.width)+
            geom_text_repel(label = data_sub_02$Categoría,vjust = ifelse(data_sub_02$degree >= 180, 0.25, -0.25), hjust = ifelse(data_sub_02$degree >= 180, 0.25, -0.50), family=input$font.family,point.padding = NA,size = input$font.size,colour=data_sub_02$Col) +
            geom_text(x = 45, y = input$Scale_03[2]*1.45, label = "I", color="black",size = 6) +
            geom_text(x = -45, y = input$Scale_03[2]*1.45, label = "IV", color="black",size = 6)+
            geom_text(x = -45, y = input$Scale_03[2]*-1.45, label = "II", color="black",size = 6)+
            geom_text(x = 45, y = input$Scale_03[2]*-1.45, label = "III", color="black",size = 6) +
            scale_color_manual(data_sub_02$Col,labels = c('p < .005', 'p < .001'))+
            geom_hline(aes(yintercept=1.96), colour="purple",size=0.1,linetype = 2)+
            geom_hline(aes(yintercept=2.58), colour="red",size=0.1,linetype = 2)+	
            geom_hline(yintercept = as.integer(ybreaks), colour = "black", size = 0.1,linetype = 2) +
            geom_hline(yintercept = input$Scale_03[2], colour = "black", size = 0.2,linetype = 1) +
            geom_vline(xintercept = seq(0, 360, by = 45), colour = "black", size = 0.1) +
            theme_bw(base_size = 8)+
            theme(panel.grid.major = element_blank())+
            theme_polar(yvals=input$Scale_03)+
            theme(panel.border = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),axis.text.x = element_text(size=11),panel.grid  = element_blank())
        
    })
    
    
    p03 <- reactive({
        
        Dataset = input$Dataset
        req(input$Dataset)
        req(!rv$clear)
        if (is.null(Dataset)) {
            return(NULL)
        } else if (input$upload_type== "xls") {
            sheet <- excel_sheets(Dataset$datapath)
            Dataset = as.data.frame(read_xlsx(Dataset$datapath,sheet= sheet,na="NeuN"))
            cat <- Dataset$Categoria
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
            
        } else if (input$upload_type== "csv") {
            Dataset = read.csv2(Dataset$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
            
            cat <- Dataset[,1]
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_0 <- gsub("[(*)]","",Dataset$Radio)
            escala_0 <- as.numeric(gsub(",",".", escala_0))
            escala_0 <- as.numeric(gsub("[(*)]","",Dataset$Longitud))
            
        } 
        
        
        Dataset_02 = input$Dataset_02
        req(input$Dataset_02)
        req(!rv$clear)
        if (is.null(Dataset_02)) {
            return(NULL)
        } else if (input$upload_type_02== "xls") {
            sheet <- excel_sheets(Dataset_02$datapath)
            
            Dataset_02 = as.data.frame(read_xlsx(Dataset_02$datapath,sheet= sheet,na="NeuN"))
            
            cat <- Dataset_02$Categoria
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_01 <- as.numeric(gsub(",",".",Dataset_02[,6]))
            
            
        } else if (input$upload_type_02== "csv") {
            Dataset_02 = read.csv2(Dataset_02$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
            
            cat <- Dataset_02[,1]
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_01 <- gsub("[(*)]","",Dataset_02$Radio)
            escala_01 <- as.numeric(gsub(",",".", escala_01))
            escala_01 <- as.numeric(gsub("[(*)]","",Dataset_02$Longitud))
            
        } 
        
        
        Dataset_03 = input$Dataset_03
        req(input$Dataset_03)
        req(!rv$clear)
        if (is.null(Dataset_03)) {
            return(NULL)
        } else if (input$upload_type_03== "xls") {
            sheet <- excel_sheets(Dataset_03$datapath)
            Dataset_03 = as.data.frame(read_xlsx(Dataset_03$datapath,sheet=sheet,na="NeuN"))
            escala_01 <- as.numeric(gsub(",",".",Dataset_02[,6]))
            escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
            escala_03 <- as.numeric(gsub(",",".",Dataset_03[,6]))
            
        } else if (input$upload_type_03== "csv") {
            Dataset_03 = read.csv2(Dataset_03$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
            
            cat <- Dataset_03[,1]
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala <- gsub("[(*)]","",Dataset_03$Radio)
            escala <- as.numeric(gsub(",",".", escala))
            escala <- as.numeric(gsub("[(*)]","",Dataset_03$Longitud))
            
        } 
        
        
        Dataset_04 = input$Dataset_04
        req(input$Dataset_04)
        req(!rv$clear)
        if (is.null(Dataset_04)) {
            return(NULL)
        } else if (input$upload_type_04== "xls") {
            sheet <- excel_sheets(Dataset_04$datapath)
            Dataset_04 = as.data.frame(read_xlsx(Dataset_04$datapath,sheet=sheet,na="NeuN"))
            escala_0 <- as.numeric(gsub(",",".",Dataset[,6]))
            escala_01 <- as.numeric(gsub(",",".",Dataset_02[,6]))
            escala_03 <- as.numeric(gsub(",",".",Dataset_03[,6]))
            escala_04 <- as.numeric(gsub(",",".",Dataset_04[,6]))
            
            
        } else if (input$upload_type_04== "csv") {
            Dataset_04 = read.csv2(Dataset_04$datapath,header=TRUE, sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
            
            cat <- Dataset_04[,1]
            updateSelectInput(session,"Category","Focal Behavior:", choices=unique(cat),selected= tail(cat,1))
            escala_04 <- gsub("[(*)]","",Dataset_04$Radio)
            escala_04 <- as.numeric(gsub(",",".", escala_04))
            escala_04 <- as.numeric(gsub("[(*)]","",Dataset_04$Longitud))
            
        } 
        
        updateSliderInput(session, "Scale_04", min = 0, max=round(max(c(escala_04, escala_03,escala_01,escala_0))+0.5))
        
        
        polar_03 <- structure(list(degree = as.numeric(gsub(",",".",Dataset_04$Angulo)), value = escala_04,cat=as.factor(Dataset_04$Categoria)), .Names = c("degree","value", "Categoría"), class = "data.frame", row.names = attr(Dataset_04, "row.names"))
        rbPal <- colorRampPalette(c('blue',"purple",'red'))
        ybreaks = pretty(x = polar_03$value, n = 3)
        rbPal <- colorRampPalette(c('blue',"purple",'red'))
        polar_03$Col <- rbPal(3)[cut(escala_04, breaks = c(0,1.96,2.58,Inf),right = FALSE)]
        data_sub_03=as.data.frame(subset(polar_03, value!=0))
        
        
        theme_polar <- function(yvals, xgeo = 133, ygeo = 0, 
                                color = "grey50", size = 1, 
                                ylab = "y",
                                textsize = 3,
                                ticks = 10,
                                ylimit = max(abs(yvals))
        ){
            
            theme(panel.border = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),axis.text.x = element_text(size=11),panel.grid  = element_blank())
            #Add ticks programatically
            ticks_y <- ybreaks
            ticks_y_sig <- c(1.96,2.58)
            
            
            #Add axis
            theme.list <- 
                list(
                    ylim(0, ylimit),
                    annotate("text", x = 150, y = ticks_y_sig, color=c("purple","red"),size = textsize,label = ticks_y_sig)
                )
            
            #Add ticks of x axis
            nlist <- length(theme.list)
            
            #Add labels to the y-ticks
            theme.list[[nlist+2]] <- annotate("text", size = textsize,x = xgeo,y = ticks_y,color=color,label = paste(ticks_y))
            
            
            return(theme.list)
        }	
        
        
        ggplot(data_sub_03, aes(x=degree, y=value,color = data_sub_03$Col))+
            scale_x_continuous(breaks = seq(45, 360, 45), limits = c(0, 360),expand = c(0, 0))+
            coord_polar(theta = "x", start = 1.5 * pi, direction = -1) +
            geom_segment(aes(y=0, xend=degree, yend=value), arrow=arrow(length=unit(0.5,"cm")),colour=data_sub_03$Col,size=input$vector.width)+
            geom_text_repel(label = data_sub_03$Categoría,vjust = ifelse(data_sub_03$degree >= 180, 0.25, -0.25), hjust = ifelse(data_sub_03$degree >= 180, 0.25, -0.50), family=input$font.family,point.padding = NA,size = input$font.size,colour=data_sub_03$Col) +
            geom_text(x = 45,  y = input$Scale_04[2]* 1.45, label = "I"  , color="black",size = 6) +
            geom_text(x = -45, y = input$Scale_04[2]* 1.45, label = "IV" , color="black",size = 6)+
            geom_text(x = -45, y = input$Scale_04[2]*-1.45, label = "II" , color="black",size = 6)+
            geom_text(x = 45,  y = input$Scale_04[2]*-1.45, label = "III", color="black",size = 6) +
            scale_color_manual(data_sub_03$Col,labels = c('p < .005', 'p < .001'))+
            geom_hline(aes(yintercept=1.96), colour="purple",size=0.1,linetype = 2)+
            geom_hline(aes(yintercept=2.58), colour="red",size=0.1,linetype = 2)+	
            geom_hline(yintercept = as.integer(ybreaks), colour = "black", size = 0.1,linetype = 2) +
            geom_hline(yintercept = input$Scale_04[2], colour = "black", size = 0.2,linetype = 1) +
            geom_vline(xintercept = seq(0, 360, by = 45), colour = "black", size = 0.1) +
            theme_bw(base_size = 8)+
            theme(panel.grid.major = element_blank())+
            theme_polar(yvals=input$Scale_04)+
            theme(panel.border = element_blank(),axis.title = element_blank(),axis.ticks = element_blank(),axis.text.y = element_blank(),axis.text.x = element_text(size=11),panel.grid  = element_blank())
        
    })
    
    
    figure <- reactive({
        ptlist <- list(p(),p01(),p02(),p03())
        to_delete <- !sapply(ptlist,is.null)
        ptlist <- ptlist[to_delete] 
        if (length(ptlist)==0) return(NULL)
        figure <- grid.arrange(grobs=ptlist)
        print(figure)
        return(figure)
    })
    
    observeEvent(input$Dataset, {
        rv$clear <- FALSE
    }, priority = 1000)
    
    observeEvent(input$Dataset_02, {
        rv$clear <- FALSE
    }, priority = 1000)
    
    observeEvent(input$reset, {
        rv$data <- NULL
        rv$clear <- TRUE
        reset('inFile')
    }, priority = 1000)
    
    observeEvent(input$reset_02, {
        rv$data <- NULL
        rv$clear <- TRUE
        reset('inFile')
    }, priority = 1000)
    
    
    output$plot <- renderPlot({p()} ,height = function(){session$clientData$output_plot_width})
    output$plot01 <- renderPlot({p01()} ,height = function(){session$clientData$output_plot01_width})
    output$plot02 <- renderPlot({p02()} ,height = function(){session$clientData$output_plot02_width})
    output$plot03 <- renderPlot({p03()} ,height = function(){session$clientData$output_plot03_width})
    
    
    
    data <- reactive({
        
        tabla = input$SDIS
        req(tabla)
        if (is.null(data)) {
            return(NULL)
        } else if (input$upload_type== "xls") {
            sheet <- excel_sheets(tabla$datapath)
            data = as.data.frame(read_xlsx(tabla$datapath,sheet= sheet,na="NeuN"))
            
        } else if (input$upload_type== "csv") {
            data = as.data.frame(read.csv2(tabla$datapath,header=TRUE,sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE))
        }
        
    })
    
    output$data <- renderTable({
        data()
    })
    
    ###################### LAG SEQUENTIAL ANALYSIS#################################  
    
    # Create the outputs dynamically
    
observe({
        
        x <- input$minlag:input$maxlag
        tabla_jf = input$SDIS
        my_list <- list()               # Create empty list
        req(tabla_jf)
        
        if (is.null(tabla_jf)) {
            return(NULL)
            
        } else if (input$upload_type== "csv") {
            data = read.csv2(tabla_jf$datapath,header=TRUE,sep=input$sep,na.strings='NA', dec=input$dec, strip.white=TRUE)
            
            my_list <- lapply(x, function(x) sdis_sequential(data[,2], adjacent = F, lag = x))
            
            cfreq    <- lapply(my_list, '[[', 1)
            names(cfreq) <- c(paste("Lag ", x))
            
            freq     <- lapply(my_list, '[[', 2)
            names(freq) <- c(paste("Lag ", x))
            
            expfreqs <- lapply(my_list, '[[', 3)
            names(expfreqs) <- c(paste("Lag ", x))
            
            cond     <- lapply(my_list, '[[', 4)
            names(cond) <- c(paste("Lag ", x))
            
            adj      <- lapply(my_list, '[[', 6)
            names(adj) <- c(paste("Lag ", x))
            
            p_val    <- lapply(my_list, '[[', 7)
            names(p_val) <- c(paste("Lag ", x))
            
            ################################################################################
            #generar una tabla con los retardos para categoría
            bind.ith.rows <- function(i) do.call(rbind, lapply(adj, "[", i, TRUE))
            nr <- nrow(adj[[1]])
            adj_categorias <- lapply(1:nr, bind.ith.rows)
            
            names(adj_categorias) <- c(paste("Given ", colnames(adj_categorias[[1]])))
        }
        
        ############ OUTPUTS LSA######################
        
        ####### observed frequencies #######
        output$jf <- renderUI({
            
            tableList <- imap(cfreq, ~ {
                tagList(
                    h4(.y), # Note we can sprinkle in other UI elements
                    tableOutput(outputId = paste0("table_", .y))
                )
            })
            
            tagList(tableList)
        })
        
        
        # Now render each output
        tables <- iwalk(cfreq, ~{
            output_name <- paste0("table_", .y)
            output[[output_name]] <- renderTable(.x, digits = 0, rownames = TRUE)
        })
        print(tables)
        
        ############# observed frequencies plot ##################
        output$jf_plot <- renderUI({
            plot_output_list <- imap(freq, ~ {
                tagList(
                    h4(.y), # Note we can sprinkle in other UI elements
                    plotOutput(outputId = paste0("plot_", .y),  height = 200, width = 200)
                )
            })
            
            tagList(plot_output_list)
        })
        
        # Now render each output
        plots_jf <- iwalk(freq, ~{
            output_name_plots <- paste0("plot_", .y)
            output[[output_name_plots]] <- renderPlot({
                mosaic(.x, ,shade=T, margins = c(left = 1.75, top = -3.5, 0), xlab ="", ylab = "")
            })
            
        })
        
        ##############################
        
        ####### expected frequencies #######
        output$exp <- renderUI({
            
            tableList_exp <- imap(expfreqs, ~ {
                tagList(
                    h4(.y), # Note we can sprinkle in other UI elements
                    tableOutput(outputId = paste0("table_exp", .y))
                )
            })
            
            tagList(tableList_exp)
        })
        
        
        # Now render each output
        iwalk(expfreqs, ~{
            output_name_exp <- paste0("table_exp", .y)
            output[[output_name_exp]] <- renderTable(.x,digits = 3,rownames = TRUE)
        })
        
        ############# expected frequencies plot ##################
        
        output$exp_plot <- renderUI({
            plot_output_list_exp <- imap(cfreq, ~ {
                tagList(
                    h4(.y), # Note we can sprinkle in other UI elements
                    plotOutput(outputId = paste0("plot_exp", .y),  height = 200, width = 200)
                )
            })
            
            tagList(plot_output_list_exp)
        })
        
        # Now render each output
        plots_exp <- iwalk(cfreq, ~{
            output_name_plots_exp <- paste0("plot_exp", .y)
            output[[output_name_plots_exp]] <- renderPlot({
                mosaic(.x, ,shade=T, margins = c(left = 1.75, top = -3.5, 0), xlab ="", ylab = "")
            })
            
        })
        ####### conditional probabilities #######
        
        output$cond <- renderUI({
            
            tableList_cond <- imap(cond, ~ {
                tagList(
                    h4(.y), # Note we can sprinkle in other UI elements
                    tableOutput(outputId = paste0("table_cond", .y))
                )
            })
            
            tagList(tableList_cond)
        })
        
        
        # Now render each output
        iwalk(cond, ~{
            output_name_cond <- paste0("table_cond", .y)
            output[[output_name_cond]] <- renderTable(.x,digits = 2,rownames = TRUE)
        })
        
        ############# conditional probabilities plot ##################
        
        output$cond_plot <- renderUI({
            plot_output_list_cond <- imap(cond, ~ {
                tagList(
                    h4(.y), # Note we can sprinkle in other UI elements
                    plotOutput(outputId = paste0("plot_cond", .y),  height = 165, width = 200)
                )
            })
            
            tagList(plot_output_list_cond)
        })
        
        # Now render each output
        plots_cond <- iwalk(cond, ~{
            output_name_plots_cond <- paste0("plot_cond", .y)
            output[[output_name_plots_cond]] <- renderPlot({
                lag_eventos.cond <- as.matrix(.x)
                DatasetGraph <- qgraph(lag_eventos.cond,legend = FALSE,palette = 'pastel', layout = "circle",
                                       edge.labels=TRUE, node.width = 2.3, asize = 6, edge.label.cex= 3,edge.label.position = .35)
                DatasetGraph
            })
            
        })
        
        ####### adjusted residuals #######
        
        output$adj <- renderUI({
            
            tableList_adj <- imap(adj, ~ {
                tagList(
                    h4(.y), # Note we can sprinkle in other UI elements
                    tableOutput(outputId = paste0("table_adj", .y))
                )
            })
            
            tagList(tableList_adj)
        })
        
        
        # Now render each output
        iwalk(adj, ~{
            output_name_adj <- paste0("table_adj", .y)
            output[[output_name_adj]] <- renderTable(.x,digits = 3,rownames = TRUE)
        })
        
        ############# adjusted residuals plot ##################
        
        output$adj_plot <- renderUI({
            plot_output_list_adj <- imap(adj, ~ {
                tagList(
                    h4(.y), # Note we can sprinkle in other UI elements
                    plotOutput(outputId = paste0("plot_adj", .y),  height = 165, width = 200)
                )
            })
            
            tagList(plot_output_list_adj)
        })
        
        # Now render each output
        plots_adj <- iwalk(adj, ~{
            output_name_plots_adj <- paste0("plot_adj", .y)
            output[[output_name_plots_adj]] <- renderPlot({
                lag_eventos.cond <- as.matrix(.x)
                DatasetGraph <- qgraph(lag_eventos.cond,legend = FALSE,palette = 'pastel', layout = "circle",
                                       edge.labels=TRUE, node.width = 2.3, asize = 6, edge.label.cex= 3,edge.label.position = .35,
                                       cut=1.96, negDashed=TRUE)
            })
            
        })
        
        ####### p_vals adjusted residuals #######
        
        output$p_val <- renderUI({
            
            tableList_p_val <- imap(p_val, ~ {
                tagList(
                    h4(.y), # Note we can sprinkle in other UI elements
                    tableOutput(outputId = paste0("table_p_val", .y))
                )
            })
            
            tagList(tableList_p_val)
        })
        
        
        # Now render each output
        iwalk(p_val, ~{
            output_name_p_val <- paste0("table_p_val", .y)
            output[[output_name_p_val]] <- renderTable(.x,digits = 3,rownames = TRUE)
        })
        
        ####### Pattern adjusted residuals #######
        
        output$ptt <- renderUI({
            
            tableList_ptt <- imap(adj_categorias, ~ {
                tagList(
                    h4(.y), # Note we can sprinkle in other UI elements
                    tableOutput(outputId = paste0("table_ptt", .y))
                )
            })
            
            tagList(tableList_ptt)
        })
        
        
        # Now render each output
        iwalk(adj_categorias, ~{
            output_name_ptt <- paste0("table_ptt", .y)
            output[[output_name_ptt]] <- renderTable(.x,digits = 3,rownames = TRUE)
        })
        
        ############# Pattern adjusted residuals plot ##################
        
        output$ptt_plot <- renderGrViz({
            # obtener los residuos ajustados pra cada una de las categorías dadas en todos los retardos
            # y generalizar a todas las categorías 
            adj_categorias <- list()
            
            ################################################################################
            #generar una tabla con los retardos para categoría
            bind.ith.rows <- function(i) do.call(rbind, lapply(adj, "[", i, TRUE))
            nr <- nrow(adj[[1]])
            adj_categorias <- lapply(1:nr, bind.ith.rows)
            
            for(i in 1:length(adj_categorias)){
                names(adj_categorias) <- c(paste("Given ", colnames(adj_categorias[[i]])))}
            
            ################################################################################
            
            #transponer todos los elementos de las tablas
            adj_categorias <- lapply(adj_categorias, t)
            
            #identificar los valores superiores a 1.96 en cada elemento de la lista
            idx <- lapply(adj_categorias, function(y) which(y > 1.96, arr.ind = T))
            
            adj_categorias_t <- lapply(adj_categorias, t)
            
            idx_lags <- lapply(adj_categorias_t, function(y) which(y > 1.96, arr.ind = T))
            
            ################################################################
            
            # Convert to data.frame
            for(i in 1:length(idx_lags)){
                idx_lags[[i]] <-as.data.frame(idx_lags[[i]]) }
            
            idx_lags <- lapply(idx_lags, function(df){df[order(df$row),]})
            
            ################################################################
            # Dar formato a los patrones
            patrones <- list()
            
            for(i in 1:length(idx)){
                patrones[[i]] <- paste(rownames(idx_lags[[i]]),rownames(idx[[i]]))}
            
            names(patrones) <- names(idx)
            
            ######################################################
            
            library (plyr)
            df <- ldply (patrones, data.frame)
            
            uniquenodes <- unique(c(df$.id, df$X..i..))
            
            ## obtener los residuos ajustados de cada relación significativa
            cond <- round(Filter(function(x) (x) > 1.96, unlist(adj_categorias)), digits = 2)
            
            #crear los nodos
            nodes <- create_node_df(n=length(uniquenodes),
                                    fixedsize = FALSE,
                                    type="number", 
                                    label=uniquenodes)
            
            #crear los vínculos
            edges <- create_edge_df(from=match(df$.id, uniquenodes), 
                                    to=match(df$X..i.., uniquenodes), 
                                    rel="related",
                                    label = paste("z = ", cond))
            
            g <- create_graph(nodes_df=nodes, 
                              edges_df=edges)
            render_graph(g)
        })
        ############# Pattern adjusted residuals plot _2 ##################
        
        output$ptt_plot_2 <- renderGrViz({
            # obtener los residuos ajustados pra cada una de las categorías dadas en todos los retardos
            # y generalizar a todas las categorías 
            adj_categorias <- list()
            
            ################################################################################
            #generar una tabla con los retardos para categoría
            bind.ith.rows <- function(i) do.call(rbind, lapply(adj, "[", i, TRUE))
            nr <- nrow(adj[[1]])
            adj_categorias <- lapply(1:nr, bind.ith.rows)
            
            for(i in 1:length(adj_categorias)){
                names(adj_categorias) <- c(paste("Given ", colnames(adj_categorias[[i]])))}
            
            ################################################################################
            
            #transponer todos los elementos de las tablas
            adj_categorias <- lapply(adj_categorias, t)
            
            #identificar los valores superiores a 1.96 en cada elemento de la lista
            idx <- lapply(adj_categorias, function(y) which(y > 1.96, arr.ind = T))
            
            adj_categorias_t <- lapply(adj_categorias, t)
            
            idx_lags <- lapply(adj_categorias_t, function(y) which(y > 1.96, arr.ind = T))
            
            #####################################################
            
            #Obtener las tablas con los patrones
            result = lapply(idx, "[",, "col")
            
            #####################################################
            
            # Convert to data.frame
            for(i in 1:length(idx_lags)){
                idx_lags[[i]] <-as.data.frame(idx_lags[[i]]) }
            
            idx_lags <- lapply(idx_lags, function(df){df[order(df$row),]})
            ################################################################
            
            # Dar formato a los patrones
            patrones <- list()
            
            for(i in 1:length(idx)){
                patrones[[i]] <- paste(rownames(idx_lags[[i]]),rownames(idx[[i]]))}
            
            names(patrones) <- names(idx)
            
            ######################################################
            
            library (plyr)
            df <- ldply (patrones, data.frame)
            
            uniquenodes <- unique(c(df$.id, df$X..i..))
            
            ## obtener los residuos ajustados de cada relación significativa
            cond <- round(Filter(function(x) (x) > 1.96, unlist(adj_categorias)), digits = 2)
            
            #crear los nodos
            nodes <- create_node_df(n=length(uniquenodes),
                                    fixedsize = FALSE,
                                    type="number", 
                                    label=uniquenodes)
            
            #crear los vínculos
            edges <- create_edge_df(from=match(df$.id, uniquenodes), 
                                    to=match(df$X..i.., uniquenodes), 
                                    rel="related",
                                    label = paste("z = ", cond))
            
            g <- create_graph(nodes_df=nodes, 
                              edges_df=edges)
            render_graph(g)
        })
        
    })
    
    ###############################################################################
    
    
    ###############################################################################
    observe({
        Dataset = input$Dataset
        req(input$Dataset)
        req(!rv$clear)
        if (is.null(Dataset)) {
            return(NULL)
        }
        else if (is.null(input$Dataset_02)) {
            output$plotgraph = renderPlot({p()},height = function(){session$clientData$output_plot01_width})
        } else if (input$multiple=='yes') {
            
            output$plotgraph = renderPlot({
                ptlist <- list(p(),p01())
                to_delete <- !sapply(ptlist,is.null)
                ptlist <- ptlist[to_delete] 
                if (length(ptlist)==0) return(NULL)
                figure <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
            },height = function(){session$clientData$output_plot01_width})
            
        } else if (input$multiple2=='yes')
            
            output$plotgraph = renderPlot({
                ptlist <- list(p(),p01(),p02())
                to_delete <- !sapply(ptlist,is.null)
                ptlist <- ptlist[to_delete] 
                if (length(ptlist)==0) return(NULL)
                figure <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
                print(figure)
                return(figure)
            },height = function(){session$clientData$output_plot01_width})
    })
    
    observe({
        Dataset = input$Dataset
        req(input$Dataset)
        req(!rv$clear)
        if (is.null(Dataset)) {
            return(NULL)
        }
        else if (is.null(input$Dataset_03)) {
            output$plotgraph = renderPlot({p()},height = function(){session$clientData$output_plot01_width})
        } else if (input$multiple2=='yes') {
            
            output$plotgraph = renderPlot({
                ptlist <- list(p(),p01(),p02())
                to_delete <- !sapply(ptlist,is.null)
                ptlist <- ptlist[to_delete] 
                if (length(ptlist)==0) return(NULL)
                figure <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
                print(figure)
                return(figure)
            },height = function(){session$clientData$output_plot01_width})
            
        } else if (input$multiple3=='yes')
            
            output$plotgraph = renderPlot({
                ptlist <- list(p(),p01(),p02(),p03())
                to_delete <- !sapply(ptlist,is.null)
                ptlist <- ptlist[to_delete] 
                if (length(ptlist)==0) return(NULL)
                figure <- grid.arrange(grobs=ptlist,ncol=length(ptlist))
                print(figure)
                return(figure)
            },height = function(){session$clientData$output_plot01_width})
    })
    
    observe({
        Dataset = input$Dataset
        req(input$Dataset)
        req(!rv$clear)
        if (is.null(Dataset)) {
            return(NULL)
        }
        else if (is.null(input$Dataset_04)) {
            output$plotgraph = renderPlot({p()},height = function(){session$clientData$output_plot01_width})
        } else if (input$multiple2=='yes') {
            
            output$plotgraph = renderPlot({
                ptlist <- list(p(),p01(),p02(),p03())
                to_delete <- !sapply(ptlist,is.null)
                ptlist <- ptlist[to_delete] 
                if (length(ptlist)==0) return(NULL)
                figure <- grid.arrange(grobs=ptlist)
                print(figure)
                return(figure)
            },height = function(){session$clientData$output_plot01_width})
            
        } else if (input$multiple3=='yes')
            
            output$plotgraph = renderPlot({
                ptlist <- list(p(),p01(),p02(),p03())
                to_delete <- !sapply(ptlist,is.null)
                ptlist <- ptlist[to_delete] 
                if (length(ptlist)==0) return(NULL)
                figure <- grid.arrange(grobs=ptlist)
                print(figure)
                return(figure)
            },height = function(){session$clientData$output_plot01_width})
    })
    
    output$figura <- downloadHandler(
        filename = function(){
            paste("polar_figura",input$downloadType,sep=".")
        },
        content = function(file){
            if (input$downloadType == "svg"){
                ggsave(file,plot=figure(),width=7,height = 7, units="in")
            } else if (input$downloadType == "pdf") {
                ggsave(file,plot=figure(),width=7, height = 7, units="in")
            } else if (input$downloadType == "png") {
                ggsave(file,plot=figure(),width=7,height = 7, units="in")
            }
        })
    
    output$save <- downloadHandler(
        filename = function(){
            paste("polar",input$downloadType,sep=".")
        },
        content = function(file){
            if (input$downloadType == "svg"){
                ggsave(file,width=7,height = 7, units="in")
            } else if (input$downloadType == "pdf") {
                ggsave(file,width=7, height = 7, units="in")
            } else if (input$downloadType == "png") {
                ggsave(file,width=7,height = 7, units="in")
            }
        })
    
    output$save <- downloadHandler(
        filename = function(){
            paste("polar",input$downloadType,sep=".")
        },
        content = function(file){
            if (input$downloadType == "svg"){
                ggsave(file,width=7,height = 7, units="in")
            } else if (input$downloadType == "pdf") {
                ggsave(file,width=7, height = 7, units="in")
            } else if (input$downloadType == "png") {
                ggsave(file,width=7,height = 7, units="in")
            }
        })
    
########### Bangdiwala Agreement Chart ########### 
bangd <- observe({
        
    bangd = input$table_agreement
        req(bangd)
        if (is.null(bangd)) {
            
            return(NULL)
          
          if (is.numeric(bangd[,1]) == TRUE){
            bangd <- lapply(bangd, factor ,
                            ordered = TRUE,
                            levels = c(min(bangd):max(bangd)))
            bangd <- as.data.frame(bangd)
                                            }
          
        } else if (input$table_type == "xls" && input$data_type == "Wide Data") {
            sheet <- excel_sheets(bangd$datapath)
            bangd = as.data.frame(read_xlsx(bangd$datapath,sheet= sheet,na="NeuN"))
            agreement <-  as.data.frame.matrix(table(bangd[,1],bangd[,2]))
            bang_chart <-  table(bangd[,1],bangd[,2])
            names(dimnames(bang_chart)) = colnames(bangd)
            
            
        } else if (input$table_type == "csv") {
          bangd = as.data.frame(read.csv2(bangd$datapath,header=TRUE,sep=",",na.strings='NA', dec=".", strip.white=TRUE))
          agreement <-  as.data.frame.matrix(table(bangd[,1],bangd[,2]))
          bang_chart <-  table(bangd[,1],bangd[,2])
          names(dimnames(bang_chart)) = colnames(bangd)
          
        
        } else if (input$table_type == "xls" && input$data_type == "Contingency Table") {
          sheet <- excel_sheets(bangd$datapath)
          bangd = as.data.frame(read_xlsx(bangd$datapath,sheet= sheet,na="NeuN"))
          agreement <- NULL
          bang_chart <- as.matrix(bangd)
          rownames(bang_chart) <- colnames(bang_chart)
          names(dimnames(bang_chart)) = c("Obs 1", "Obs 2")
        
        }
        
### Datos Originales ###       
output$bang_data <- renderDT({
                                print(bangd)
                                })

### Matriz de Acuerdo ### 
output$bang_table <- renderDT({
                                print(agreement)
                                })

### Bangdiwala Agreement Chart ###
output$bang_plot <- renderPlot({
                                agree <- agreementplot(bang_chart)
                                print(agree)
                                })

### Bangdiwala Results ###
output$bang_res <- renderDT({
                                agree <- agreementplot(bang_chart)
                                all.results <- matrix(NA, nrow = 2, ncol = 4)
                                colnames(all.results) <- c("B", "Bw", "Weights 0", "Weights 1")
                                all.results[1,] <- c("B", "Bw", "Weights 0", "Weights 1")
                                all.results[2,] <- round(c( agree$Bangdiwala,agree$Bangdiwala_Weighted,agree$weights),2)
                                datatable(all.results, caption = htmltools::tags$caption("Bangdiwala Agreement B", style="color:blue"), 
                                          options = list(searching = FALSE, lengthChange = FALSE,info = FALSE, paging = FALSE,
                                                         columnDefs = list(list(className = 'dt-center', targets = 0:3))
                                                         )
                                          )
                            })

### Kappa Results ###                                
output$kappa_res <- renderDT({  k     <- kappa2.table(bang_chart)
                                all.results <- matrix(NA, nrow = 2, ncol = 4)
                                colnames(all.results) <-c("Cohen's Kappa", "coeff.se", "coeff.ci", "coeff.pval")
                                all.results[1,] <- c("Cohen's Kappa", "se", "95 % CI", "p val")
                                k[1,2:3] <- round(k[1,2:3],2)
                                all.results[2,] <-c(k[1,2],k[1,3],k[1,4],k[1,5])
                                datatable(all.results, caption = htmltools::tags$caption("Cohen's Kappa", style="color:blue"),
                                          options = list(searching = FALSE, lengthChange = FALSE, info = FALSE, paging = FALSE,
                                                         columnDefs = list(list(className = 'dt-center', targets = 0:3))
                                                        )
                                          )
                              })

### Krippendorff Results ###                                
output$kripp_res <-  renderDT({  if (is.numeric(bangd[,1]) == TRUE){
                                escala <- "ordinal"
                              } else if (is.numeric(bangd[,1]) == FALSE){
                                escala <- "unweighted"
                              }
                              
                                kripp       <- krippen.alpha.raw(bangd, weights = escala)
                                all.results <- matrix(NA, nrow = 2, ncol = 4)
                                colnames(all.results) <-c("\U03B1", "coeff.se", "coeff.ci", "coeff.pval")
                                all.results[1,] <- c("\U03B1", "se", "95 % CI", "p val")
                                kripp$est[1,4:5] <- round(kripp$est[1,4:5],2)
                                kripp$est[1,7] <- round(kripp$est[1,7],2)
                                all.results[2,] <- c(kripp$est[1,4],kripp$est[1,5],kripp$est[1,6],kripp$est[1,7])
                                datatable(all.results, caption = htmltools::tags$caption("Krippendorff´s \U03B1", style="color:blue"),
                                          options = list(searching = FALSE, lengthChange = FALSE, info = FALSE, paging = FALSE,
                                                          columnDefs = list(list(className = 'dt-center', targets = 0:3))
                                                          )
                                          )
                              })

### AC1_2 Gwet Results ###                                
output$gwet_res <-  renderDT({  if (is.numeric(bangd[,1]) == TRUE){
                                escala <- "ordinal"
                              } else if (is.numeric(bangd[,1]) == FALSE){
                                escala <- "unweighted"
                              }
  
                              gwet       <- gwet.ac1.raw(bangd, weights = escala)
                              all.results <- matrix(NA, nrow = 2, ncol = 4)
                              colnames(all.results) <-c("Gwet´s AC1/AC2" , "coeff.se", "coeff.ci", "coeff.pval")
                              all.results[1,] <- c("Gwet´s", "se", "95 % CI", "p val")
                              gwet$est[1,4:5] <- round(gwet$est[1,4:5],2)
                              gwet$est[1,7] <- round(gwet$est[1,7],2)
                              all.results[2,] <- c(gwet$est[1,4],gwet$est[1,5],gwet$est[1,6],gwet$est[1,7])
                              datatable(all.results, caption = htmltools::tags$caption("Gwet´s AC1/AC2", style="color:blue"),
                              options = list(searching = FALSE, lengthChange = FALSE, info = FALSE, paging = FALSE,
                              columnDefs = list(list(className = 'dt-center', targets = 0:3))
                                            )
                                        )
                              })

############Fin de resultados de Observers Agreement ############
    })



################################################################################
})


