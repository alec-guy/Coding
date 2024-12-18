module Types where 

data Position = Position 
              { x :: Float 
              , y :: Float
              } deriving (Eq, Show)
data Square = Square 
          {topLeftCorner     :: Position
          ,topRightCorner    :: Position
          ,bottomLeftCorner  :: Position
          ,bottomRightCorner :: Position
          } deriving (Show , Eq)

data Step = StepRight 
          | StepLeft 
          | StepUp 
          | StepDown 
          deriving (Eq)

mySquare :: Square 
mySquare = Square 
         { topLeftCorner = Position 1 2
         , topRightCorner = Position 3 2
         , bottomLeftCorner = Position 1 1 
         , bottomRightCorner = Position 3 1
         }

makeSquare :: Position -> Float -> Square 
makeSquare bottomleftcorner  sideLength = 
    let xBttmL = x bottomleftcorner
        yBttmL = y bottomleftcorner
    in Square 
       { topLeftCorner = Position (xBttmL) (yBttmL + sideLength)
       , topRightCorner = Position (xBttmL + sideLength)  (yBttmL + sideLength)
       , bottomLeftCorner = bottomleftcorner
       ,bottomRightCorner = Position (xBttmL + sideLength) (yBttmL)
       }

moveSquareUp :: Float -> Square -> Square
moveSquareUp i square = 
        Square 
        { topLeftCorner = Position (x $ topLeftCorner square) ((y $ topLeftCorner square) + i )
        , topRightCorner = Position (x $ topRightCorner square) ((y $ topRightCorner square) + i )
        , bottomLeftCorner = Position (x $ bottomLeftCorner square)  ((y $ bottomLeftCorner square) + i )
        , bottomRightCorner = Position (x $ bottomRightCorner square) ((y $ topRightCorner square) + i )
        }
moveSquareDown i square = moveSquareUp (- i) square
moveSquareLeft i square = moveSquareRight (-i) square
moveSquareRight :: Float -> Square -> Square
moveSquareRight i square = 
        Square 
        { topLeftCorner = Position ((x $ topLeftCorner square) + i)  (y $ topLeftCorner square)
        , topRightCorner = Position ((x $ topRightCorner square) + i) (y $ topRightCorner square)
        , bottomLeftCorner = Position ((x $ bottomLeftCorner square) + i ) (y $ bottomLeftCorner square)
        , bottomRightCorner = Position ((x $ bottomRightCorner square) + i) (y $ topRightCorner square)
        }

