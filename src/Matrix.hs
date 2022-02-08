module Matrix where

    import Environment3D
    import Data.Matrix
    

    type Matrix3D = [[Float]]

    type Rotation a = Vector3D a

    rotationToMatrix :: Floating a => Rotation a -> Matrix a
    rotationToMatrix (Vector3D x y z) = multStd2 (zRotationMatrix z) $ multStd2 (xRotationMatrix x) (yRotationMatrix y) 
    
    xRotationMatrix :: Floating a => a -> Matrix a
    xRotationMatrix r = fromLists [[1,0,0],[0,cos r,-sin r],[0,sin r,cos r]]
  
    zRotationMatrix :: Floating a => a -> Matrix a
    zRotationMatrix r = fromLists [[cos r,-sin r,0],[sin r,cos r,0],[0,0,1]]
  
    yRotationMatrix :: Floating a => a -> Matrix a
    yRotationMatrix r = fromLists [[cos r,0,sin r],[0,1,0],[-sin r,0,cos r]]

    rotateAroundCenter :: Floating a => Rotation a -> Vector3D a -> Vector3D a
    rotateAroundCenter = matrixMultiplication . rotationToMatrix
   
    rotatePointAroundPoint :: Floating a => Vector3D a -> Rotation a -> Vector3D a -> Vector3D a
    rotatePointAroundPoint point rot vec = add point $ rotateAroundCenter rot $  sub vec point

    matrixMultiplication :: Floating a => Matrix a -> Vector3D a -> Vector3D a
    matrixMultiplication matrix vector  = vector3DFromList $ map (sum . (zipVector3D (*) vector)) $ toLists matrix

    rotateBack :: (Floating a, Eq a) => Vector3D a -> Rotation a -> Vector3D a
    rotateBack vec rot = case inverse $ rotationToMatrix rot of
        Right matrix -> matrixMultiplication matrix vec
        Left str -> matrixMultiplication (zero 3 3) vec

