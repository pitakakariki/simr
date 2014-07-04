## test that noopt works to copy a merMod

## test that new frame has the right size

## test that fixefs are the same after extend




##
## Things that need to be the same after `extend`, and things that need to be different.
##

# (Based on the `simulate` function.)



# X [getME(obj, 'X')] and n [nrow(X)] should change as appropriate.
# [ncol(X)] should be the same.

# fixef(obj), getME(obj, 'beta')

# Z and lambda?

if(FALSE) {

context('extend')

  
  ## BAD MODEL GIVES WARNINGS???
a1 <- lmer(Carbon ~ Position * Mgmt + Year + (Year | Cluster/Mgmt/Block/Position), kiwifruit)
ax <- extend(a1, 'Year', values=seq(2004, 2014, 2))

test_that('extended model preserves original parameters', {
    
    # Should still be a model object
    expect_is(ax, 'merMod')
    
    expect_equal(sigma(ax), sigma(a1))
    
    expect_equal(getME(ax, 'beta'), getME(a1, 'beta'))
    expect_equal(fixef(ax), fixef(a1))
    
    expect_equal(getME(ax, 'Lambda'), getME(a1, 'Lambda'))
    
    expect_equal(ranef(ax), ranef(a1)) 
    
})

test_that('extended model has correct dimensions', {
    
    
    
    
    
})
  
  
  
  
}