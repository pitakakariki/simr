#context('fixef')


### simpler test model. kiwifruit e.g. too slow?


#a <- lmer(Carbon ~ Year + (Year | Cluster), kiwifruit)

#test_that('single fixef coefs can be replaced', {

#    .a <- a
    
    # test replacement
#    fixef(a)['Year'] <- -0.13
#    expect_that(fixef(a)['Year'], equals( c(Year=-0.13) ))

    # test reversibility
#    fixef(a)['Year'] <- fixef(.a)['Year']
#    expect_that(a, is_identical_to(.a))    
#})