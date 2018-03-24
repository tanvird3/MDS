shinyServer(function(input, output) {
  ma <- function(installment, maturity, int, com) {
    int <- int / 100
    int <-
      (1 + int / com) ^ (com / 12) -
      1
    
    f <- matrix(nrow = length(installment), ncol = length(maturity))
    for (i in 1:length(installment)) {
      for (j in 1:length(maturity)) {
        f[i, j] <- abs(fv(int,
                          maturity[j] * 12,
                          pmt = installment[i],
                          type = 1))
      }
    }
    f <- as.data.frame(f)
    colnames(f) <- paste(maturity, "Years", sep = "_")
    f$Installment_Size <- installment
    f <- f[c(ncol(f), 1:(ncol(f) - 1))]
    ff <- f
    library(reshape2)
    f <- melt(f, id = "Installment_Size")
    pl <-
      ggplot(f, aes(
        x = as.factor(Installment_Size),
        y = value,
        fill = variable
      )) + geom_bar(stat = "identity", position = "dodge") + labs(
        title = "Amount Receivable",
        x = "Installment Size",
        y = "Amount",
        fill =
          "Tenure"
      ) + scale_x_discrete(labels = as.factor(ff[, 1])) + theme(
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size =
                                      16),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size =
                                      16),
        plot.title = element_text(
          size = 20,
          face = "bold",
          color = "darkgreen"
        ),
        legend.title = element_text(size = 16),
        legend.text = element_text(size =
                                     14)
      )
    
    list(f = ff, pl = pl)
  }
  mv <- function(Tenure, fo, pm) {
    Tenure <- Tenure * 12
    fo <- fo * 1
    if (fo < Tenure * pm) {
      dc <-
        "Error::The Recivable Amount Can Not Be Less Than The Total Deposited Amount"
    } else {
      dc <-
        discount.rate(
          n = Tenure,
          pv = 0,
          fv = -fo,
          pmt = pm,
          type = 1,
          lower = 0.0001,
          upper = 1
        ) * 12
    }
    dc <-
      data.frame(
        Total_Deposit = Tenure * pm,
        Recivable_Amount = fo,
        Underline_Interest_Rate =
          dc
      )
    dc
  }
  output$X <-
    renderTable({
      mv(Tenure = input$Tenure,
         fo = input$MatAmt,
         pm = input$IS)
    })
  output$P <-
    renderTable({
      ma(
        as.numeric(unlist(strsplit(input$inst, ","))),
        as.numeric(unlist(strsplit(
          input$maturity, ","
        ))),
        input$interest,
        as.numeric(input$compounding)
      )$f
    }, include.rownames = F)
  output$C <- renderPlot({
    ma(
      as.numeric(unlist(strsplit(input$inst, ","))),
      as.numeric(unlist(strsplit(
        input$maturity, ","
      ))),
      input$interest,
      as.numeric(input$compounding)
    )$pl
  })
})