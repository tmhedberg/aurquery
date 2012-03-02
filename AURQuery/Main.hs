import Control.Arrow

import AURQuery.Local

main = fmap (map $ name &&& version) installedPkgs >>= print
