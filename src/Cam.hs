module Cam where

    import Environment3D
    import Object3D
    import Matrix
    import GUILib
    import Data.Array
    import Data.List

    type FocalLenght = Int 
    type Size = (Int,Int)
    type Zoom = Float
    data Cam = Cam Size FocalLenght (Rotation Float) (Vector3D Float) deriving (Show)

    --depth :: [(Vector2D,Float)] -> [(Vector2D,Float)] -> Ordering
    --depth a b = compare (((realToFrac $ length a)/) $ snd $ unzip a) (((realToFrac $ length b)/) $ snd $ unzip b)

    --Converts 3D coordinates to 2D and removes Points that are not Visible by the Camera
    --object3DtoObject2D :: Cam -> Object3D Float -> Object2D
    --object3DtoObject2D cam (Object3D edges vert) = map (lookup $ fmap (vector3DToVector2D cam) vert) edges -- map (fst.unzip) $ sortBy depth $ 
    --  where
    --    lookup :: Array Int (Vector2D) -> [Int] -> [Vector2D]
    --    lookup vert edges = map (vert Data.Array.! ) edges

    object3DtoObject2D :: Cam -> Object3D Float -> Object2D
    object3DtoObject2D cam (Object3D edges vert) = (map.map) (vector3DToVector2D cam) $ sortBy depth$ map (lookup vert) edges -- map (fst.unzip) $ sortBy depth $ 
      where
        lookup :: Array Int (Vector3D Float) -> [Int] -> [Vector3D Float]
        lookup vert edges = map (vert Data.Array.! ) edges
    
    depth :: [Vector3D Float] ->  [Vector3D Float] -> Ordering
    depth v1 v2 = compare (((realToFrac $ length v1)/) $ sum $ z $ sequence v1) (((realToFrac $ length v2)/) $ sum $ z $ sequence v2) 

    vector3DToVector2D :: Cam -> Vector3D Float -> Vector2D
    vector3DToVector2D (Cam _ f _ _) vec = (calc f (x vec) (z vec),calc f (y vec) (z vec))
      where calc c a b = round (1* (fromIntegral c)* a/b)

    render3DImage:: Cam -> [Object3D Float] -> Image
    render3DImage cam objects = concat $ map (object3DtoObject2D cam) $ prepObjs cam objects
      where 
        prepObjs (Cam size f rot pos) objects = rotateAndMoveCam pos rot objects


    --convert3Dto2D :: Cam -> [Vector3D a] -> [Vector2D]
    --convert3Dto2D _ [] = []
    --convert3Dto2D (Cam (h,w) f rot pos) ((x,y,z):t)
    --  | z > 0 =  (calc f x z,calc f y z) : (convert3Dto2D(Cam (h,w) f rot pos) t)
    --  | otherwise = (convert3Dto2D (Cam (h,w) f rot pos) t)
    --  where calc c a b= round (1* (fromIntegral c)* a/b)

    rotateCam :: Floating a => Rotation a -> [Object3D a] -> [Object3D a]
    rotateCam rot objects = map (rotateCamObj rot) objects
      where rotateCamObj rot (Object3D edges vert) = Object3D edges $ fmap (rotateAroundCenter $ fmap (*(-1)) rot) vert

    rotateAndMoveCam :: Floating a => Vector3D a -> Rotation a -> [Object3D a] -> [Object3D a]
    rotateAndMoveCam vec rot objects = map (rotateCamObj rot) objects
      where rotateCamObj rot (Object3D edges vert) = Object3D edges $ fmap ((rotateAroundCenter $ fmap (*(-1)) rot).(vec `sub` )) vert --

    moveCam :: Num a => Vector3D a -> [Object3D a] -> [Object3D a]
    moveCam vec objs = map (moveCamObj vec) objs
      where moveCamObj vec (Object3D edges vert) = Object3D edges $ fmap (vec `sub` ) vert
--
   