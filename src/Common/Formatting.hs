module Common.Formatting where

-- | The `Pretty` class defines `pretty`.
class Pretty a where
  -- | @pretty a@ returns a string pretty printed string of `a`.
  pretty :: a -> String

-- | The `Pretty` class defines `typeStr`.
class Typeable a where
  -- | @typeStr a@ returns a string representing the type of `a`.
  typeStr :: a -> String