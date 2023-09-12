import Graphics.Gloss
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace


data Card = Card
  { visina :: Float
  , sirina :: Float
  , x :: Int
  , y :: Int
  , randomNumber :: Int
  , isFlip :: Bool
  , isFound :: Bool
  }


data GameState = GameState
  { cards :: [Card]
  , selectedCardIndices :: [Int]
  , moveCount :: Int
  }


windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600


boardWidth, boardHeight :: Float
boardWidth = 500
boardHeight = 500


cardWidth, cardHeight :: Float
cardWidth = (boardWidth - cardSpacing * 4) / 5
cardHeight = (boardHeight - cardSpacing * 4) / 5


cardSpacing :: Float
cardSpacing = 10


numbers :: [Int]
numbers = [1..15] ++ [1..15]


shuffle :: [a] -> IO [a]
shuffle xs = do
    gen <- newStdGen
    return $ shuffle' gen xs


shuffle' :: RandomGen g => g -> [a] -> [a]
shuffle' _ [] = []
shuffle' gen xs = x : shuffle' gen' xs'
  where
    (index, gen') = randomR (0, length xs - 1) gen
    (x, xs') = removeAt index xs


removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! n, take n xs ++ drop (n + 1) xs)


getShuffledNumbers :: [Int]
getShuffledNumbers = unsafePerformIO $ shuffle numbers


drawCard :: Float -> Float -> Int -> Int -> Int -> Bool -> Bool -> Picture
drawCard x y corx cory memoryNumber isFlip isFound= translate (x - cardWidth / 2) (y - cardHeight / 2) $
  pictures [ cardShape, cardText ]
  where
    cardShape = if isFlip || isFound
      then color (makeColor 0.9 0.3 0.5 1.0) $ rectangleSolid cardWidth cardHeight 
      else color (makeColor 0.7 0.1 0.3 1.0) $ rectangleSolid cardWidth cardHeight

    cardText = if isFlip || isFound
      then translate (-cardWidth / 4) (-cardHeight / 4) $ scale 0.4 0.4 $ color black $ text (show memoryNumber)
      else blank
  

drawBoard :: [Card] -> Int -> Picture
drawBoard cardList moveCount=
  pictures
    [ translate (-300) 250 $ scale 0.3 0.3 $ color black $ text gameTitle
    , pictures [ drawCard
                    (fromIntegral (x card) * (cardWidth + cardSpacing) + cardWidth / 2 -250)
                    (fromIntegral (y card) * (cardHeight + cardSpacing) + cardHeight / 2 -230)
                    (x card)
                    (y card)
                    (randomNumber card)
                    (isFlip card)
                    (isFound card)
               | card <- cardList
               ]
    ]
    where
    gameTitle = if areAllCardsFlip cardList
                then "Pobeda! Broj poteza: "++ show (moveCount)
                else "Broj poteza: " ++ show (moveCount)


areAllCardsFlip :: [Card] -> Bool
areAllCardsFlip cards =
  let numFound = length (filter isFlip cards)
  in numFound == length cards


initial :: [Card]
initial = [Card
                (fromIntegral x * (cardWidth + cardSpacing) + cardWidth / 2 -250)
                (fromIntegral y * (cardHeight + cardSpacing) + cardHeight / 2 -230)
                x 
                y
                (shuffledNumbers !! (x * 5 + y))
                False
                False
           | x <- [0 .. 5], y <- [0 .. 4]]
 where
  shuffledNumbers = getShuffledNumbers


compareCards :: GameState -> GameState
compareCards gameState =
  let selectedIndices = selectedCardIndices gameState
  in if length selectedIndices == 2
     then
       let idx1 : idx2 : _ = selectedIndices
           card1 = cards gameState !! idx1
           card2 = cards gameState !! idx2
           areEqual = randomNumber card1 == randomNumber card2
           updatedCard1 = card1 { isFlip = areEqual, isFound = areEqual }
           updatedCard2 = card2 { isFlip = areEqual, isFound = areEqual }
           updatedCards = updateCardAtIndex idx1 updatedCard1 $ updateCardAtIndex idx2 updatedCard2 (cards gameState)
           newGameState = gameState { cards = updatedCards }
           updatedGameState = if null selectedIndices
                              then closeFlippedUnfound newGameState
                              else newGameState
       in updatedGameState
     else if null selectedIndices
          then closeFlippedUnfound gameState
          else gameState


closeFlippedUnfound :: GameState -> GameState
closeFlippedUnfound gameState =
  let cardsToClose = [idx | (idx, card) <- zip [0..] (cards gameState), isFlip card && not (isFound card)]
      updatedCards = foldr (\idx acc -> updateCardAtIndex idx (cards gameState !! idx) acc) (cards gameState) cardsToClose
  in gameState { cards = updatedCards }


isMouseOnCard :: Float -> Float -> Card -> Bool
isMouseOnCard mouseX mouseY card =
  let cardX = visina card
      cardY = sirina card
  in mouseX >= cardX - cardWidth && mouseX <= cardX
     && mouseY >= cardY - cardHeight && mouseY <= cardY


handleCardClick :: Int -> GameState -> GameState
handleCardClick idx gameState =
  let clickedCard = cards gameState !! idx
  in if isFound clickedCard || isFlip clickedCard
     then gameState
     else
       let gameStateAfterCompare = compareCards gameState
           updatedCard = clickedCard { isFlip = True }
           updatedCards = updateCardAtIndex idx updatedCard (cards gameStateAfterCompare)
           selectedIndices = selectedCardIndices gameStateAfterCompare
           newSelectedIndices = if length selectedIndices == 2
                                then [idx]
                                else if not (isFlip clickedCard) && length selectedIndices < 2
                                     then idx : selectedIndices
                                     else selectedIndices
           updatedMoveCount = moveCount gameStateAfterCompare + 1
           updatedGameState = gameStateAfterCompare { cards = updatedCards, selectedCardIndices = newSelectedIndices, moveCount = updatedMoveCount }
       in updatedGameState


updateCardAtIndex :: Int -> Card -> [Card] -> [Card]
updateCardAtIndex idx newCard cards = take idx cards ++ [newCard] ++ drop (idx + 1) cards


handleOtherClick :: GameState -> GameState
handleOtherClick gameState = trace "Klik izvan kartica!" gameState


handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (mouseX, mouseY)) gameState =
  case cardIndices of
    [idx] -> handleCardClick idx gameState
    _     -> handleOtherClick gameState
  where
    cardIndices = [i | (i, card) <- zip [0..] (cards gameState), isMouseOnCard mouseX mouseY card]
handleEvent _ gameState = gameState


main :: IO ()
main = play displayMode backgroundColor frameRate initialState render handleEvent update
  where
    prom = initial
    displayMode = InWindow "Memory Game" (windowWidth, windowHeight) (100, 100)
    backgroundColor = white
    frameRate = 60
    initialState = GameState prom [] 0   
    render gameState = drawBoard (cards gameState) (moveCount gameState)
    update _ = id
    

