import Data.List
import Data.Maybe
import Lecture5

isMinimal' :: Sudoku -> [(Row, Column)] -> (Bool, Maybe Sudoku)
isMinimal' s [] = (True, Nothing)
isMinimal' s (p:ps) = if (possibleSolutions /= 1) then
                        isMinimal' s ps
                      else
                        (False, Just s')
    where s' = extend s (p,0)
          possibleSolutions = length $ take 2 $ solveNs [(s',constraints s')]

-- returns whether a given grid is in minimal form
-- if not, returns false and the solution which is more-minimal, but not necessarily minimal.
isMinimal :: Grid -> (Bool, Maybe Sudoku)
isMinimal g = if solvable then
                (isMinimal' s $ filledPositions s)
              else
                (False, Nothing)
            where s = grid2sud g
                  solvable = (length $ take 2 $ solveNs [(s,constraints s)]) == 1

testMinimal :: Grid -> IO Bool
testMinimal g = if not (fst res)
                    then do
                        print "original: "
                        showGrid g
                        let s = snd res
                        if (isJust s)
                            then do 
                                print "improvement: "
                                (showSudoku (fromJust s))
                                return False
                            else do
                                print "Non-solvable sudoku"
                                return False
                else
                    return True
                where res = isMinimal g
    
minimalSudoku = [[0,0,0,0,0,0,0,1,0],
              [0,0,0,0,0,2,0,0,3],
              [0,0,0,4,0,0,0,0,0],
              [0,0,0,0,0,0,5,0,0],
              [4,0,1,6,0,0,0,0,0],
              [0,0,7,1,0,0,0,0,0],
              [0,5,0,0,0,0,2,0,0],
              [0,0,0,0,8,0,0,4,0],
              [0,3,0,9,1,0,0,0,0]]
    
main = do
    {-print "example 1"
    testMinimal example1
    print "example 2"
    testMinimal example2
    print "example 3"
    testMinimal example3
    print "example 4"
    testMinimal example4
    print "example 5"
    testMinimal example5-}
    print "minimalSudoku"
    testMinimal minimalSudoku