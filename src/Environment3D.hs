module Environment3D where
  import Data.Functor
  import Data.Foldable
  import Control.Applicative
  import Control.Monad
  import GHC.Float
  import Data.Ord

  --                         X Y Z
  data Vector3D a = Vector3D {x::a, y::a, z::a} deriving (Show)

  instance Foldable Vector3D where
    foldr f z (Vector3D a b c) = f c $ f b $ f a z

  instance Functor Vector3D where
    fmap f (Vector3D a b c) = Vector3D (f a) (f b) (f c)
  
  instance Applicative Vector3D where
    pure a = Vector3D a a a
    (<*>) (Vector3D f1 f2 f3) (Vector3D a b c) = Vector3D (f1 a) (f2 b) (f3 c)

  instance Monad Vector3D where
    (>>=) (Vector3D a b c) func = Vector3D (x $ func a) (y $ func b) (z $ func c)

  type DVector3D = Vector3D Double
  type FVector3D = Vector3D Float

  --                X   Y 
  type Vector2D = (Int,Int)

  add :: Num a => Vector3D a -> Vector3D a -> Vector3D a
  add = liftA2 (+)

  sub :: Num a => Vector3D a -> Vector3D a -> Vector3D a
  sub = liftA2 (subtract)

  updateVector :: (a->a->a) -> Vector3D a -> Vector3D a -> Vector3D a
  updateVector = liftA2

  floatToDoubleVector :: Vector3D Float -> Vector3D Double
  floatToDoubleVector = fmap float2Double

  doubleToFloatVector :: Vector3D Double -> Vector3D Float
  doubleToFloatVector = fmap double2Float

  realToFracVector :: (Real a, Fractional b) => Vector3D a -> Vector3D b
  realToFracVector = fmap realToFrac

  zipVector3D :: (a -> b -> c) -> Vector3D a -> [b] -> Vector3D c
  zipVector3D func vec list = liftA2 func vec $ vector3DFromList list 

  vector3DToList :: Vector3D a -> [a]
  vector3DToList (Vector3D a b c) = [a,b,c]

  vector3DFromList :: [a] -> Vector3D a
  vector3DFromList (a:b:c:d) = Vector3D a b c