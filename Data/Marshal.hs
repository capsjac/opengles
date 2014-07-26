-- | 'fromEnum' alternative
module Data.Marshal where
	class Marshal a where
		marshal :: (Num n) => a -> n
