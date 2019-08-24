module nat_test

open util/natural as nat

let twoViaAdd = nat/add[nat/One, nat/One]
let twoViaInc = nat/inc[nat/One]

run {
 some x: nat/Natural | nat/lte[x, twoViaAdd]
 some y: nat/Natural | nat/lt[y, twoViaInc]
 some y: nat/Natural | nat/gte[y,nat/mul[nat/One, nat/dec[nat/One]]]
 some y: nat/Natural | nat/gt[y,nat/sub[nat/inc[nat/One], nat/One]]
 some y: nat/Natural | nat/gt[y,nat/add[nat/inc[nat/Zero], nat/Zero]]
} for 7 Int