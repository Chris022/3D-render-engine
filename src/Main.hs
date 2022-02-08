module Main where

  import Control.Concurrent
  import Control.Applicative (liftA2)
  import qualified Graphics.Gloss as Gloss
  import qualified Graphics.Gloss.Interface.IO.Game as Gloss
  import Graphics.Gloss.Interface.Pure.Game(Event(..),Key(..),SpecialKey(..),KeyState(..),MouseButton(..),Modifiers(..))


  import GUILib
  import Object3D
  import Environment3D
  import Cam
  import Matrix
  import Criterion.Main

  import ObjInterpreter


  data ClickEvent = MouseWheel | MouseWheelShift | None
  type Click = (ClickEvent,Float,Float)


  --                                    Word-Coordinate-Pos
  type World = (Click,Rotation Float,Zoom,Vector3D Float,Object3D Float)

  cubeIO = interpretFile "MonkeyObj.obj"

  main :: IO()
  main = do
    cube <- cubeIO
    Gloss.play window background simulationRate ((None,0,0),Vector3D 0 0 0,150,Vector3D 0 0 0,scaleObject 100 $ cube) drawingFunc inputHandler updateFunc
    where
      simulationRate :: Int
      simulationRate = 0


  drawingFunc :: World -> Gloss.Picture
  drawingFunc (_,rot,zoom, wcm,obj) = drawImageHaskell $ render3DImage (Cam (1080,720) 1000 (fmap (*(-1)) rot) (calcPositonOfCam wcm zoom rot)) [obj]


  calcPositonOfCam :: Vector3D Float -> Float-> Rotation Float -> Vector3D Float
  calcPositonOfCam offset radius (Vector3D x y z) = offset `add` fmap (*radius) (Vector3D ( cos(y-pi/2) * cos(x) ) ( sin(-x) ) ( sin(y-pi/2) * cos(-x) ))

  updateFunc :: Float -> World -> World
  updateFunc dt (c,r,z, wcm,obj) = (c,r,z, wcm,obj)

  inputHandler :: Event -> World -> World
  inputHandler (EventKey (MouseButton MiddleButton) Down (Modifiers Up Up Up) (x,y)) (_,rot,zoom, wcm,obj)            
                        =   ((MouseWheel,x,y),rot,zoom, wcm,obj)
  inputHandler (EventKey (MouseButton MiddleButton) Down (Modifiers Down Up Up) (x,y)) (_,rot,zoom, wcm,obj)            
                        =   ((MouseWheelShift,x,y),rot,zoom, wcm,obj)
  inputHandler (EventKey (MouseButton MiddleButton) Up _ _) (_,rot,zoom, wcm,obj)           
                        =    ((None,0,0), rot,zoom, wcm,obj)
  -- Rotation                      
  inputHandler (EventMotion (x2,y2)) ((MouseWheel,x,y), vec,zoom, wcm,obj) 
                        =   ((MouseWheel,x2,y2), liftA2 (\i j->i - j/100) vec (Vector3D (y-y2) (-x+x2) 0),zoom, wcm,obj)
  -- Movement
  inputHandler (EventMotion (x2,y2)) ((MouseWheelShift,x,y), rot,zoom, wcm,obj) 
                        =   ((MouseWheelShift,x2,y2), rot ,zoom, liftA2 (\i j->i - j) wcm (rotateBack (Vector3D (-x+x2) (-y+y2) 0) rot),obj)
  
  inputHandler (EventKey (MouseButton WheelUp) Down _ _) (c,rot,zoom, wcm,obj)        
                        =   (c, rot,zoom-20, wcm,obj)
  inputHandler (EventKey (MouseButton WheelDown) Down _ _) (c,rot,zoom, wcm,obj)            
                        =   (c, rot,zoom+20, wcm,obj)       
             
  inputHandler _ w = w
  
