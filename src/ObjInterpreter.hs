module ObjInterpreter where

import Environment3D
import Object3D

import Text.Parsec
import Control.Applicative ((<$), (<*), (*>), (<$>), (<*>),liftA2)
import Data.Char
import Data.Either.Combinators
import Data.Array

interpretFile :: String -> IO (Object3D Float)
interpretFile file = do
    f <- readFile file
    pure $ toObject3D $ fromRight (Obj [] []) $ parse parseObj "" f

data ObjElement = Vector FVector3D | Face [Int] | Other String deriving (Show)


data Obj = Obj [Vector3D Float] [[Int]] deriving (Show)

toObject3D :: Obj -> Object3D Float
toObject3D (Obj vec faces) = createObject $ Object3D faces $ listArray (1,length vec) vec--(map.map) ((vec!!).(subtract 1)) faces


createObj :: [ObjElement] -> Obj
createObj elem = foldl1 combineObj $ map singleConv elem
    where
        singleConv (Vector v) = Obj [v] []
        singleConv (Face   f) = Obj [] [f]
        singleConv (Other  s) = Obj [] []

        combineObj (Obj v1 f1) (Obj v2 f2) = Obj (v1++v2) (f1++f2)

parseObj :: Parsec String st Obj
parseObj = createObj <$> (many $ parseLine)

parseLine :: Parsec String st ObjElement
parseLine = try (Vector <$> parseVector) <|> try (Face <$> parseFace) <|> (Other <$> parseOther) <* (optional $ char '\n')

parseOther :: Parsec String st String
parseOther = many1 $ noneOf "\n"

parseVector :: Parsec String st FVector3D
parseVector = string "v " *> (vector3DFromList <$> (many (parseFloat <* (optional space))))

parseFace :: Parsec String st [Int]
parseFace = string "f " *> (many $ fmap (read) (num <* (optional space)))
    where num = (many $ oneOf "0123456789") <* (many1 $ string "/") <* (many  $ oneOf "0123456789")


parseFloat :: Parsec String st Float
parseFloat = r <$> (minus <|> number )
    where 
        r = read ::String -> Float
        minus = (:) <$> char '-' <*> number
        number = many1 $ choice [oneOf "0123456789",char '.']

test :: Parsec String () a -> (String -> Either ParseError a)
test p = parse p ""