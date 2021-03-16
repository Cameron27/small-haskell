module Classes where

class Pretty a where
  pretty :: a -> String

class Typeable a where
  typeStr :: a -> String