library(ggflags)

# data(lflags)
# set.seed(1234)
#
# # 15% opacity gives 2B in the alpha channel
# .flaglist[["lv"]]@content[[1]]@content[[2]]@content[[1]]@gp$fill <- "#C28FEF2B"
#   # 33% opacity gives 54 in the alpha channel
# .flaglist[["qa"]]@content[[1]]@content[[2]]@content[[1]]@gp$fill <- "#9450E054"
#   d <- data.frame(x=rnorm(10), y=rnorm(10),
#                   country=sample(c("ar","fr"), 10, TRUE),
#                   country=sample(c("lv","qa"), 10, TRUE),
#                   stringsAsFactors = FALSE)
#   ggplot(d, aes(x=x, y=y, country=country, size=x)) +
#     geom_flag() +
#     scale_country()



    # modify flags
    # attempt to load the RDA, edit out problematic data, and save it again

    # Looking at the upstream svg, bth the Latvian and Qatari flags appear
    # to have a semi-transparent overlay over the coloured portions of their
    # flags. In both cases the fractional opacity is not reflected in the
    # converted version in the rda.

    data(lflags)

  # 15% opacity gives 2B in the alpha channel
  .flaglist[["lv"]]@content[[1]]@content[[2]]@content[[1]]@gp$fill <- "#9E3039"
    # 33% opacity gives 54 in the alpha channel
  .flaglist[["qa"]]@content[[1]]@content[[2]]@content[[1]]@gp$fill <- "#9450E054"

  save(.flaglist, file="data/lflags.rda")
