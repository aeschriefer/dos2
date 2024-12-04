library(tidyverse)

##### Function definitions ######

attack <- function(stats, high.ground=TRUE, crit.calc=c('avg', 'yes','no'), weapon=TRUE, base.dmg=1){
  # base.dmg is the damage on the weapon description or from a spell table
  # defaults to 1 but usually is a range of damage consisting of two integers

  # only one weapon skill (ie ranged, twohanded, singlehanded, dualwielding) should be passed 
  # because they effect different weapon types and cannot be used in combo during an attack

  # If weapon is TRUE the weapon formula is used
  # If weapon is FALSE the spell formula is used

  if(sum(stats < 0)){
    # Check that no stats are negative
    return(NA)
  }

  # element is warfare, aero, geo, etc...
  skills = select(stats, c('scoundrel', 'element', 'huntsman'))
  weapon.skills = select(stats, c('ranged', 'twohanded', 'singlehanded', 'dualwielding'))
  # convert integer percent values to decimals
  percents = select(stats, c('gear.crit', 'misc', 'extra.crit.multi', 'extra.crit.chance')) / 100

  if(sum(weapon.skills > 0) > 1){
    # Check that only one weapon skill is non-zero
    return(NA)
  }

  # attribute is strength, finesse, or intelligence
  # under 10 attribute reduces dmg
  # under 10 wits has no effect
  # only values above 10 add effects so we subtract 10 off for formula
  attrs = c(
    attribute=pluck(stats, 'attribute', .default=10) - 10,
    wits=max(0, pluck(stats, 'wits', .default=10) - 10)
  )

  high.ground.bonus=ifelse(high.ground, 0. + 0.05*skills[['huntsman']], 0)
  crit.multiplier=1.5 + percents[['extra.crit.multi']] + 0.05*skills[['scoundrel']] + 0.05*weapon.skills[['twohanded']]
  crit.chance = min(
    1,
    percents[['extra.crit.chance']] + percents[['gear.crit']] + 0.01*(attrs[['wits']] + weapon.skills[['ranged']])
  )
  
  if(weapon){
    dmg = (1 + 0.05*skills[['element']])*(1 + 0.05*(attrs[['attribute']] + max(weapon.skills) + percents[['misc']]))
  } else {
    dmg = (1 + 0.05*skills[['element']])*(1 + 0.05*(attrs[['attribute']]))*(1 + percents[['misc']])
  }
  crit=1+high.ground.bonus+crit.multiplier
  non.crit=1+high.ground.bonus
  
  crit.calc = match.arg(crit.calc)
  base.dmg * switch(
    crit.calc,
    yes = dmg*crit,
    no = dmg*non.crit,
    avg = dmg*crit*crit.chance + dmg*non.crit*(1-crit.chance)
  )
}

select <- function(stats, names){
  # missing skills iin stats vector default to 0
  skills = replace_na(stats[names], 0)
  names(skills) = names
  return(skills)
}

upgrade <- function(stats, change, base.dmg.ratio=1, ...){
  # Calculates the % damage change by replacing gear causing a change
  # in stats represented by the change vector
  
  # base.dmg.ratio should be:
  # (new weapon base damage) / (old weapon base damage)
  baseline=attack(stats, ...)
  combined = c(stats, change)
  summed=tapply(combined, names(combined), sum)
  mod=attack(summed, base.dmg=base.dmg.ratio, ...)
  mean((mod - baseline) / abs(baseline))
}

gradient <- function(stats, ...){
  # This function calculates the marginal % damage change caused
  # by increasing each main stat by 1
  sort(sapply(get.names(stats), single.gradient, stats, ...), decreasing=T)
}

single.gradient <- function(name, stats, ...){
  change = numeric()
  change[[name]] = 1
  upgrade(stats, change, ...)
}

get.names <- function(stats) {
  # Do not consider misc, or extra.crit.* stats in gradient calculations
  # because these stats cannot be affected by gear
  # Do not consider gear.crit because it is the same as wits
  grep('misc|crit', names(stats), invert=T, perl=T, value=T)
}


##### Unit tests #####
test.stats = c(
  scoundrel=2,
  element=13,
  huntsman=5,
  ranged=10,
  gear.crit=25,
  attribute=70,
  wits=32,
  misc=15, # from weapon rune
  extra.crit.chance=5 + 10, # human bonus and hothead
  extra.crit.multi=20 + 10 # bow and human bonus
)

stopifnot(is.na(attack(c(ranged=1, twohanded=1))))
stopifnot(is.na(attack(c(ranged=-1))))

stopifnot(floor(attack(test.stats, crit.calc='no', high.ground=F, base.dmg=c(103, 127))) == c(766, 944))

##### analysis ######
# Current stats with no buffs
ranger.stats = c(
  scoundrel=2,
  element=13,
  huntsman=5,
  ranged=10,
  gear.crit=25,
  attribute=70,
  wits=32,
  misc=15, # from weapon rune
  extra.crit.chance=5 + 10, # human bonus and hothead
  extra.crit.multi=20 + 10 # bow and human bonus
)

attack(ranger.stats, crit.calc='no', high.ground=F)
gradient(ranger.stats)
gradient(ranger.stats, high.ground=F)
gradient(ranger.stats, crit.calc='no')
gradient(ranger.stats, crit.calc='yes')


upgrade(ranger.stats, c(element=-1, attribute=3, wits=1))

upgrade(ranger.stats, c(misc=-15, wits=-3, gear.crit=-15, base.dmg.ratio=c(87,107) / c(61,75)))


mage.stats = c(
  scoundrel=13,
  element=7,
  huntsman=2,
  gear.crit=26,
  attribute=53,
  wits=33,
  misc=10 # flesh sacrifice
)

gradient(mage.stats, weapon=F)
