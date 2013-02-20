maintainer        "Ian Duncan"
maintainer_email  "ian@iankduncan.com"
license           "BSD"
description       "Installs the latest version of GHC"
version           "7.6.2"
recipe            "ghc", "Installs GHC"

%w{ubuntu debian}.each do |os|
  supports os
end
