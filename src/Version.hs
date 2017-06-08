module Version where

data Version = Version { majorVersion :: Int
                       , minorVersion :: Int
                       }

instance Show Version where
  show (Version major minor) = show major ++ "." ++ show minor

data Release = Stable Version
             | Unstable Version
             | Development

instance Show Release where
  show (Stable ver)   = show ver ++ "-stable"
  show (Unstable ver) = show ver ++ "-unstable"
  show Development    = "development"
