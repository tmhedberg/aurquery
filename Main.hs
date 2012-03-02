import Control.Arrow

import Local

main = fmap (map $ name &&& version) installedPkgs >>= print
