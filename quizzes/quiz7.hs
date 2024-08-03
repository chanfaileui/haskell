{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Quiz7 where

-- data Zero
-- data NonZero
-- data CheckedInt t = Checked Int

-- safeDiv :: Int -> CheckedInt NonZero -> Int
-- safeDiv x (Checked y) = x `div` y

-- add :: CheckedInt a -> CheckedInt b -> CheckedInt c
-- add (Checked x) (Checked y) = Checked (x + y)

-- check :: Int -> Either (CheckedInt Zero) (CheckedInt NonZero)
-- check 0 = error "Zero"

data Size = OneD  | TwoD | ThreeD
newtype Vector (s :: Size) a = V [a]

vec1D :: a -> Vector OneD a
vec1D a = V [a]

vec2D :: (a, a) -> Vector TwoD a
vec2D (a,b) = V [a,b]

vec3D :: (a, a, a) -> Vector ThreeD a
vec3D (a,b,c) = V [a,b,c]

-- addV :: (Num n) => Vector a n -> Vector b n -> Vector c n
-- addV (V xs) (V ys) = V (zipWith (+) xs ys)

-- addV :: (Num n) => Vector a n -> Vector a n -> Vector a n
-- addV (V xs) (V ys) = V (zipWith (+) xs ys)

-- addV :: (Num n) => Vector TwoD n -> Vector TwoD n -> Vector TwoD n
-- addV (V xs) (V ys) = V (zipWith (+) xs ys)

-- addV :: (Num n) => Vector a n -> Vector a n -> Vector b n
-- addV (V xs) (V ys) = V (zipWith (+) xs ys)