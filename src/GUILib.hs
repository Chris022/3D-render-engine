module GUILib where

  import qualified Graphics.Gloss as Gloss

  type Point = (Int,Int)

  type Image = [[Point]]

  window :: Gloss.Display
  window = Gloss.InWindow "3D Viewer" (1000, 1000) (2000, 500)

  background :: Gloss.Color
  background = Gloss.white

  drawImageHaskell :: Image -> Gloss.Picture
  drawImageHaskell img = Gloss.Pictures $ (map fillShapeHaskell img) 
    
  drawShapeHaskell :: [Point] -> Gloss.Picture
  drawShapeHaskell pol = Gloss.line $ map (\(x,y)->(fromIntegral x, fromIntegral y)) pol --,Gloss.color (Gloss.makeColor 255 0 0 255) $ Gloss.polygon $ map (\(x,y)->(fromIntegral x, fromIntegral y)) pol

  fillShapeHaskell :: [Point] -> Gloss.Picture
  fillShapeHaskell pol = Gloss.pictures [Gloss.color (Gloss.makeColor 255 0 0 255) $ Gloss.polygon $ map (\(x,y)->(fromIntegral x, fromIntegral y)) pol,Gloss.line $ map (\(x,y)->(fromIntegral x, fromIntegral y)) pol]