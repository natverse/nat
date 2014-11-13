context("Fiji Simple Neurite Tracer input")

test_that("read Simple Neurite Tracer files", {
  expect_is(st<-read.neuron("testdata/neuron/SinglePath.traces"), 'neuron')
  
  stbase=structure(list(NeuronName = "SinglePath", 
    NumPoints = 11L, StartPoint = 1L, BranchPoints = integer(0), 
    EndPoints = c(1L, 11L), nTrees = 1, NumSegs = 1L, SegList = structure(list(
        1:11), class = c("seglist", "list")), d = structure(list(
        PointNo = 1:11, Label = rep(2L, 11), X = c(228.337419509888, 228.337419509888, 
        228.337419509888, 227.78853148222, 227.78853148222, 227.239643454552, 
        227.239643454552, 227.239643454552, 226.690755426884, 
        226.690755426884, 226.690755426884), Y = c(92.2131886482239, 
        92.2131886482239, 92.2131886482239, 92.2131886482239, 
        92.2131886482239, 91.6643006205559, 91.1154125928879, 
        91.1154125928879, 90.5665245652199, 90.0176365375519, 
        89.4687485098839), Z = c(39, 40, 41, 42, 43, 44, 45, 
        46, 47, 48, 49), W = c(NA_real_, NA_real_, NA_real_, 
        NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, 
        NA_real_, NA_real_), Parent = c(-1L, 1L, 2L, 3L, 4L, 
        5L, 6L, 7L, 8L, 9L, 10L)), .Names = c("PointNo", "Label", 
    "X", "Y", "Z", "W", "Parent"), class = "data.frame", row.names = c(NA, 
    -11L))), .Names = c("NeuronName", "NumPoints", 
"StartPoint", "BranchPoints", "EndPoints", "nTrees", "NumSegs", 
"SegList", "d"), class = c("neuron", "list"))

  expect_equal(st, stbase)

  expect_is(mpt<-read.neuron("testdata/neuron/MultiplePathsJoinedToMainPath.traces"), 'neuron')
  mpt.seg=structure(list(1:149, 149:150, 150:568, c(150L, 644L, 645L, 646L, 
    647L, 648L, 649L, 650L, 651L, 652L, 653L, 654L, 655L, 656L, 657L, 
    658L, 659L, 660L, 661L, 662L, 663L, 664L, 665L, 666L, 667L, 668L, 
    669L, 670L, 671L, 672L, 673L, 674L, 675L, 676L, 677L, 678L, 679L, 
    680L, 681L, 682L, 683L, 684L, 685L, 686L, 687L, 688L, 689L, 690L, 
    691L, 692L, 693L, 694L, 695L, 696L, 697L, 698L, 699L, 700L, 701L
    ), c(149L, 569L, 570L, 571L, 572L, 573L, 574L, 575L, 576L, 577L, 
    578L, 579L, 580L, 581L, 582L, 583L, 584L, 585L, 586L, 587L, 588L, 
    589L, 590L, 591L, 592L, 593L, 594L, 595L, 596L, 597L, 598L, 599L, 
    600L, 601L, 602L, 603L, 604L, 605L, 606L, 607L, 608L, 609L, 610L, 
    611L, 612L, 613L, 614L, 615L, 616L, 617L, 618L, 619L, 620L, 621L, 
    622L, 623L, 624L, 625L, 626L, 627L, 628L, 629L, 630L, 631L, 632L, 
    633L, 634L, 635L, 636L, 637L, 638L, 639L, 640L, 641L, 642L, 643L
    )), class = c("seglist", "list"))
  expect_equal(as.seglist(mpt), mpt.seg)
  
  expect_is(sbt<-read.neuron("testdata/neuron/SequentiallyBranchingTrace.traces"), 'neuron')
  sbt.seg=structure(list(1:78, 78:262, c(78L, 263L, 264L, 265L, 266L, 267L, 
    268L, 269L, 270L, 271L, 272L, 273L, 274L, 275L), 275:388, c(275L, 
    389L, 390L, 391L, 392L, 393L, 394L, 395L, 396L, 397L, 398L, 399L, 
    400L, 401L, 402L, 403L, 404L, 405L, 406L, 407L, 408L, 409L, 410L, 
    411L, 412L, 413L, 414L, 415L, 416L, 417L, 418L, 419L, 420L, 421L, 
    422L, 423L, 424L, 425L, 426L, 427L, 428L, 429L, 430L, 431L, 432L, 
    433L, 434L, 435L, 436L, 437L, 438L, 439L, 440L, 441L, 442L, 443L, 
    444L, 445L, 446L, 447L, 448L, 449L, 450L, 451L, 452L, 453L, 454L, 
    455L, 456L, 457L, 458L, 459L, 460L, 461L, 462L, 463L, 464L, 465L, 
    466L, 467L, 468L, 469L, 470L, 471L, 472L, 473L, 474L, 475L, 476L, 
    477L, 478L, 479L, 480L, 481L, 482L, 483L, 484L, 485L, 486L, 487L, 
    488L, 489L, 490L, 491L, 492L, 493L, 494L, 495L, 496L, 497L, 498L, 
    499L, 500L, 501L, 502L, 503L, 504L, 505L, 506L, 507L, 508L, 509L, 
    510L, 511L, 512L, 513L, 514L, 515L, 516L, 517L, 518L, 519L, 520L, 
    521L, 522L, 523L, 524L, 525L, 526L)), class = c("seglist", "list"
    ))
  expect_equal(as.seglist(sbt), sbt.seg)
})

test_that("Cross check Simple Neurite Tracer SWC export", {
  expect_is(nl<-read.neuron("testdata/neuron/fitted.traces"), 'neuronlist')
  n=read.neuron("testdata/neuron/unfitted.swc")
  # Fiji writes width as 0 when undefined
  n$d$W=NA_real_
  expect_equal(nl[[1]], n, fieldsToExclude="NeuronName")
})

test_that("Recognise Simple Neurite Tracer files",{
  ff=dir(c("testdata/neuroml", "testdata/neuron"), full.names = T, recursive = T)
  expect_equivalent(is.fijitraces(ff), grepl(".traces$",ff))
})
