library(tidyverse)

##### Function definitions ######

attack <- function(stats, high.ground=TRUE, crit.calc=c('avg', 'yes','no'), weapon=TRUE){
  # base.dmg is the damage on the weapon description or from a spell table
  # defaults to 1 but usually is a range of damage consisting of two integers
  # extra.dmg is damage from elemental arrowheads

  # only one weapon skill (ie ranged, twohanded, singlehanded, dualwielding) should be passed 
  # because they effect different weapon types and cannot be used in combo during an attack

  # If weapon is TRUE the weapon formula is used
  # If weapon is FALSE the spell formula is used

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

  base.dmg=pluck(stats, 'base.dmg', .default=1)
  extra.dmg=pluck(stats, 'extra.dmg', .default=0)

  # base high ground bonus is 20%
  high.ground.bonus=ifelse(high.ground, 0.2 + 0.05*skills[['huntsman']], 0)
  crit.multiplier=0.5 + percents[['extra.crit.multi']] + 0.05*skills[['scoundrel']] + 0.05*weapon.skills[['twohanded']]
  crit.chance = min(
    1,
    percents[['extra.crit.chance']] + percents[['gear.crit']] + 0.01*(attrs[['wits']] + weapon.skills[['ranged']])
  )
  
  if(weapon){
    dmg = (1 + 0.05*skills[['element']])*(1 + percents[['misc']] + 0.05*(attrs[['attribute']] + max(weapon.skills)))
  } else {
    dmg = (1 + 0.05*skills[['element']])*(1 + 0.05*(attrs[['attribute']]))*(1 + percents[['misc']])
  }
  crit=1+high.ground.bonus+crit.multiplier
  non.crit=1+high.ground.bonus
  
  crit.calc = match.arg(crit.calc)
  (base.dmg * dmg + extra.dmg) * switch(
    crit.calc,
    yes = crit,
    no = non.crit,
    avg = crit*crit.chance + non.crit*(1-crit.chance)
  )
}

select <- function(lst, names) {
  # returns a vector
  map(names, \(x) pluck(lst, x, .default=0)) %>%
    flatten_dbl %>%
    set_names(names)
}

select.list <- function(lst, names) {
  # returns a list
  map(names, \(x) pluck(lst, x, .default=0)) %>%
    set_names(names)
}

sum.lists <- function(x, y){
  nms=union(names(x), names(y))
  map2(select.list(x, nms), select.list(y, nms), \(x, y) x + y)
}

upgrade <- function(stats, change, ...){
  # Calculates the % damage change by replacing gear causing a change
  # in stats represented by the change vector
  
  # base.dmg.ratio should be:
  # (new weapon base damage) / (old weapon base damage)
  baseline=attack(stats, ...)
  summed=sum.lists(stats, change)
  mod=attack(summed, ...)
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
test.stats = list(
  scoundrel=2,
  element=13,
  huntsman=5,
  ranged=10,
  gear.crit=25,
  attribute=70,
  wits=32,
  #misc=15, # from weapon rune
  extra.crit.chance=5 + 10, # human bonus and hothead
  extra.crit.multi=20 + 10 # bow and human bonus,
)

#stopifnot(is.na(attack(c(ranged=1, twohanded=1))))
#stopifnot(is.na(attack(c(ranged=-1))))

  # stopifnot(floor(attack(test.stats, crit.calc='no', high.ground=F, base.dmg=c(103, 127))) == c(764, 942))

# crit chance is recorded in the character panel so we check that we match it here
# crit = 1/(1.5 + 0.3 + 0.1) * (attack(test.stats, high.ground=F, crit.calc='avg') / attack(test.stats, high.ground=F, crit.calc='no') - 1)
# stopifnot(signif(crit, digits=2) == 0.72)

##### analysis ######
# Current stats with no buffs
ranger.stats = list(
  scoundrel=7,
  element=15,
  huntsman=8,
  ranged=12,
  gear.crit=11,
  attribute=76,
  wits=41,
  misc=0, # runes do not count as misc, they improve base damage of a weapon
  extra.crit.chance=10, # hothead
  extra.crit.multi=0
)
bow.stats=c(ranger.stats, list(base.dmg=c(152, 161)))
bloody.stats=c(bow.stats, list(extra.dmg=c(305, 321))) # extra damage from elemental arrowheads
  

attack(ranger.stats, crit.calc='no', high.ground=F)
attack(ranger.stats, crit.calc='yes', high.ground=F)
gradient(ranger.stats)
gradient(bloody.stats)
gradient(ranger.stats, high.ground=F)
gradient(ranger.stats, crit.calc='no')
gradient(ranger.stats, crit.calc='yes')

# consumable effects
upgrade(ranger.stats, c(wits=22))
upgrade(ranger.stats, c(attribute=22))
upgrade(ranger.stats, c(attribute=5, wits=11))

# using challenge
upgrade(ranger.stats, c(misc=20))

# change from elf to human
upgrade(bloody.stats, list(misc=-10, extra.crit.multi=10, extra.crit.chance=5, extra.dmg=-bloody.stats[['extra.dmg']]))

# effect of using bloody arrows. Bow and level dependent
upgrade(bow.stats, bloody.stats['extra.dmg'])

attack(bow.stats, high.ground=T, crit.calc='yes')
attack(bow.stats, high.ground=T, crit.calc='no', )
attack(bloody.stats, high.ground=T, crit.calc='yes', )
attack(bloody.stats, high.ground=T, crit.calc='no', )

attack(bow.stats, high.ground=F, crit.calc='yes', )
attack(bow.stats, high.ground=F, crit.calc='no', )
