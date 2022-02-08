module Object3D where

  import Environment3D
  import Matrix
  import Data.Array

  data Object3D a = Object3D [[Int]] (Array Int (Vector3D a))


  type Element2D = [Vector2D]

  type Object2D = [Element2D]

  translateBy :: Num a => Vector3D a -> Object3D a -> Object3D a
  translateBy vec (Object3D edges vert) = Object3D edges $ fmap (add vec) vert

  findCenterOfGravity :: Fractional a => Object3D a -> Vector3D a
  findCenterOfGravity (Object3D edges vert) = fmap ((/(fromIntegral $ length vert)) . sum) $ sequence $ vert


  rotateAroundPoint :: Floating a => Vector3D a -> Rotation a -> Object3D a -> Object3D a
  rotateAroundPoint point rot (Object3D edges vert) = Object3D edges $ fmap (rotatePointAroundPoint point rot) vert


  scaleObject :: Num a => a -> Object3D a-> Object3D a
  scaleObject f (Object3D edges vert) = Object3D edges $ (fmap.fmap) (*f) vert

  createObject :: Object3D a -> Object3D a
  createObject (Object3D edges vert) = Object3D (map (\elem->take ((length elem)+1) (cycle elem)) edges) vert