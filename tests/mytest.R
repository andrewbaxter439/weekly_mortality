app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$snapshot()
app$setInputs(autocorr_show = TRUE,wait_=FALSE, values_=FALSE)
app$snapshot()
