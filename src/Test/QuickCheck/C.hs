module Test.QuickCheck.C
  ( module Test.QuickCheck,
    module Test.QuickCheck.Modifiers,
    module Foreign.C,
    module Foreign.C.Types,
    module Foreign.Ptr,
    module Foreign.Marshal.Array,
    --
    showArray,
    showsArray
  ) where

import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array

import Data.List (intersperse)

-- | show lists with C-style array literal syntax
showArray :: Show a => [a] -> String
showArray xs = showsArray xs ""

showsArray :: Show a => [a] -> ShowS
showsArray xs
  = ('{':) . (foldr (.) id $ intersperse (',':) $ map shows xs) . ('}':)



-- | Quickcheck generator & shrinking for C types
-- * signed numeric types
instance Arbitrary CInt where
  arbitrary = CInt <$> arbitrary
  shrink (CInt i) = map CInt (shrink i)

instance Arbitrary CLong where
  arbitrary = CLong <$> arbitrary
  shrink (CLong i) = map CLong (shrink i)

instance Arbitrary CLLong where
  arbitrary = CLLong <$> arbitrary
  shrink (CLLong i) = map CLLong (shrink i)

instance Arbitrary CShort where
  arbitrary = CShort <$> arbitrary
  shrink (CShort i) = map CShort (shrink i)

instance Arbitrary CChar where
  arbitrary = CChar <$> arbitrary
  shrink (CChar i) = map CChar (shrink i)

-- * unsigned numeric types
instance Arbitrary CUInt where
  arbitrary = CUInt <$> arbitrary
  shrink (CUInt i) = map CUInt (shrink i)

instance Arbitrary CULong where
  arbitrary = CULong <$> arbitrary
  shrink (CULong i) = map CULong (shrink i)

instance Arbitrary CULLong where
  arbitrary = CULLong <$> arbitrary
  shrink (CULLong i) = map CULLong (shrink i)

instance Arbitrary CUChar where
  arbitrary = CUChar <$> arbitrary
  shrink (CUChar i) = map CUChar (shrink i)

-- * floating point numbers
instance Arbitrary CFloat where
  arbitrary = CFloat <$> arbitrary
  shrink (CFloat d) = map CFloat (shrink d)

instance Arbitrary CDouble where
  arbitrary = CDouble <$> arbitrary
  shrink (CDouble d) = map CDouble (shrink d)


