{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Random
import Control.Monad.Writer
import Data.Maybe
import Data.List
import           Data.Text (Text,pack)
import Control.Concurrent
import Control.Concurrent.STM

import           Graphics.Blank


type Cell  = Maybe Int
type Row   = [Cell]
type Board = [Row]
--Above 3 types collectively define complete board [[Maybe Int]]


--Datatype used to indicate the user move Up, Down, Right, Left
data Direction = R | L | U | D deriving (Show, Eq)

--Datatype used to define result after each move
data Outcome = Lose | Win | Active | Invalid

--Used to represent the state of the board and the result of each move
data MoveResult = MoveResult Outcome Board

main :: IO ()
main = do
  --Creates a 4X4 board of mostly nothing and very less random cells with value 2 and 4
   boardRan <- makeStartBoard 4
   
   --TVar objects of Board and Outcome created to share between controller and viewer
   --The controller writes to them and viewer just reads
   startBoard <- newTVarIO boardRan
   startOutcome <- newTVarIO Active
   
   --Using the view controller methodolody as in the Tic Tac Toe taught in the class
   blankCanvas 3000 { events = ["keydown"] } $ \ context -> do
        forkIO $ viewer context startBoard startOutcome
        controller context startBoard startOutcome (Just 2048) 
        

viewer :: DeviceContext -> TVar Board -> TVar Outcome-> IO ()
viewer context board_var outcome_var= do
  
	-- Reads the board and outcome after each move
        board <- readTVarIO board_var
        outcome <- readTVarIO outcome_var
        send context $ do
                let (w,h) = (width context, height context)
                clearRect (0,0,w,h)
                beginPath()

                let sz = min w h
                save()
                --Creates the board on the browser screen
                translate (w / 2, h / 2)
                sequence_ [ do bigLine (-sz * 0.5,n) (sz * 0.5,n)
                               bigLine (n,-sz * 0.5) (n,sz * 0.5)
                          | n <- [-sz * 0.25,sz * 0.25,0]
                          ]
                restore()

                save()
                --Draws the current state of board on to the screen
                translate (w / 4,0)
                sequence_ [ do save()
                               translate (fromIntegral x * sz * 0.25,fromIntegral y * sz * 0.25)
                               case getelement (x,y) board of
                                Just n -> drawN n sz
                                Nothing -> return ()
                               restore()
                          | x <- [0,1,2,3]
                          , y <- [0,1,2,3]
                          ]
                restore()
--According to the outcome the game outcome is shown to the user.
        case outcome of
	     Lose ->send context $ do
	        save()
	        translate ((width context)/2,(height context)/2)
	        draw "You Lost!!"
	        restore()
	     Win ->send context $ do
	        save()
	        translate ((width context)/2,(height context)/2)
	        draw "You Win!!"
	        restore()
	     _ -> viewer context board_var outcome_var
        
--According to the key press of the user it is mapped to the right Direction Data type.        
getdirection :: Event -> Maybe Direction
getdirection event = case eWhich event of
	  Just 38 -> Just U
	  Just 37 -> Just L
	  Just 40 -> Just D
	  Just 39 -> Just R
	  _ -> Nothing
	  

--Controller that performs and manages the logic and is responsible for writing the
-- new state to the viewer
controller :: DeviceContext -> TVar Board ->TVar Outcome-> Cell -> IO ()
controller context board_var outcome_var goal= do
        board <- readTVarIO board_var
        --putStrLn $ show board
        event <- wait context
	let direction = getdirection event
        case direction of
	  Nothing -> controller context board_var outcome_var goal
	  Just dir -> do
            MoveResult moveOutcome newBoard <- liftIO $ gameRound goal dir board
            atomically $ do
				writeTVar board_var newBoard
				writeTVar outcome_var moveOutcome
            case moveOutcome of
				Lose -> return ()
				Win -> return ()
				_ -> controller context board_var outcome_var goal
        

--Helps in adjusting the font according to the number in the cell
getsize :: Int -> Int
getsize 2 = 75
getsize 4 = 75
getsize 8 = 75
getsize 16 = 55
getsize 32 = 55
getsize 64 = 55
getsize 128 = 45
getsize 256 = 45
getsize 512 = 45
getsize 1024 = 35
getsize 2048 = 35
getsize _ =0


--Used to show "Lost" or "Win" message to user
draw :: String -> Canvas()
draw message = do
	font (pack ((show 100)++"pt Calibri"))
	textAlign "center"
        textBaseline "middle"
	lineWidth 5
	fillStyle "red"
	fillText((pack message),0,0)

--Used to send numbers to cells in the UI
drawN :: Int ->Double-> Canvas ()        
drawN num size = do
	font (pack ((show (getsize num))++"pt Calibri"))
	textAlign "center"
        textBaseline "middle"
	lineWidth 5
	fillStyle "blue"
	fillText((pack (show num)),size/8,size/8)

--Displays the board lines to the user as in TicTacToe	
bigLine :: (Double, Double) -> (Double, Double) -> Canvas ()
bigLine (x,y) (x',y') = do
        beginPath()
        moveTo(x,y)
        lineTo(x',y')
        lineWidth 20
        strokeStyle "grey"
        lineCap "round"
        stroke()

-- Gets the positions of all the cells which are "Nothing"
available :: Board -> [(Int, Int)]
available = concat . zipWith (zip . repeat) [0..] . fmap (elemIndices Nothing)                   


-- Updates the board at a particular position with the new value
update :: Board -> (Int, Int) -> Cell -> Board
update board (x, y) val = newBoard
    where (rs, r:rs') = splitAt x board
          (cs, _:cs') = splitAt y r
          newRow = cs ++ (val : cs')
          newBoard = rs ++ (newRow : rs')
          
--Main function the helps in moving rows and adding numbers and accumulating to one side     
moveRow :: Row -> Row
moveRow row = (++ nothings) $ sumPairs justs
    where (justs, nothings) = partition isJust row
          sumPairs (Just x:Just y:zs) | x == y =
            let total = x + y
		rest = sumPairs zs
            in Just total : rest ++ [Nothing]
          sumPairs (x:xs) = (x :) $ sumPairs xs
          sumPairs [] = []

-- the moveRow function is called in different ways according to the action of the user
-- controls the moveRow function
performMove :: Direction -> Board -> Board
performMove direction = case direction of
    L  -> goLeft
    R  -> goRight
    U -> transpose . goLeft . transpose
    D -> transpose . goRight . transpose
    where goLeft = map moveRow
          goRight = map $ reverse . moveRow . reverse
          

-- gets the element present at (x,y) co-ordinates in the board
getelement :: (Int,Int) -> Board -> Cell
getelement (x,y) board = retrieve
  where (rs, r:rs') = splitAt y board
        (cs, target:cs') = splitAt x r
	retrieve = target

--Creates an empty Board
emptyBoard :: Int -> Board
emptyBoard n = replicate n $ replicate n Nothing

--Creates a initial board with some random values
makeStartBoard :: MonadRandom m => Int -> m Board
makeStartBoard size = do
    Just board  <- insertRandom (emptyBoard size)
    Just board' <- insertRandom board
    return board'

--Function that inserts random values into the Board after each move
--Inserts 2 with probability 0.9 and 4 with probability 0.1
insertRandom :: MonadRandom m => Board -> m (Maybe Board)
insertRandom board
    | null holes = return Nothing
    | otherwise = do
        pos <- liftM (holes !!) $ getRandomR (0, length holes - 1)
        coin <- getRandomR (0 :: Float, 1)
        let newCell = Just $ if coin < 0.9 then 2 else 4
        return . Just $ update board pos newCell
    where holes = available board


winner :: Cell -> Board -> Bool
winner winning = elem winning . concat

--Calls the other functions and produces the outcome and Board state after each user action.
--2nd level controller function that performs all the tasks.
gameRound :: MonadRandom m => Cell -> Direction -> Board -> m MoveResult
gameRound goal direction board =
    let newBoard = performMove direction board
        change = board /= newBoard
    in if not change 
        then return $ if null $ available newBoard
            then MoveResult Lose newBoard
	    else MoveResult Invalid board
        else if winner goal newBoard
            then return $ MoveResult Win newBoard
            else do
                randoBoard <- insertRandom newBoard
                case randoBoard of
                    Nothing -> return $ MoveResult Lose newBoard
                    Just b  -> return $ MoveResult Active b	  