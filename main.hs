import System.IO (openFile, hGetContents, Handle, IOMode (ReadMode), hClose)
-- import GHC.IO.IOMode ( IOMode(ReadMode) )
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Ord ( comparing )
import Data.Foldable (minimumBy)
import Control.Monad (guard)
-- import GHC.Exts.Heap (GenClosure(value))
-- import Control.Concurrent.STM (check)


-- posizione è rappresentata da coppia di valori int perché si tratta di una scacchiera
-- type Position = (Int, Int)

data Virtues a = Virtues {
    umilta :: a,
    coraggio :: a,
    gentilezza :: a,
    rispetto :: a
}

data Senpai = Senpai {
    stats :: Virtues Int,
    currentPosition :: (Int, Int)
}

data Checkboard = Checkboard {
    size :: Int,
    senpai :: [Senpai],
    pickups :: Virtues [(Int, Int)]
}

instance Functor Virtues where
    fmap f (Virtues u c g r) = Virtues {
        umilta = f u,
        coraggio = f c,
        gentilezza = f g,
        rispetto = f r
    }

instance Show Senpai where
    show :: Senpai -> String
    show (Senpai s pos) = show (fst pos, snd pos, umilta s, coraggio s, gentilezza s, rispetto s)

instance Show Checkboard where
    show :: Checkboard -> String
    show (Checkboard n senpai (Virtues u c g r)) = "\nN = " ++ show n ++ "\nD = {" ++
            "\nS = " ++ show senpai ++
            "\nU = " ++ show u ++
            "\nC = " ++ show c ++
            "\nG = " ++ show g ++
            "\nR = " ++ show r ++ "\n}"

instance Eq (Virtues Int) where
    (==) (Virtues u c g r) (Virtues u' c' g' r') =
            u == u' && c == c' && g == g' && r == r'
    (/=) = (not .) . (==)   --todo rivedere

instance Eq Senpai where
    (==) (Senpai v pos) (Senpai v' pos') =
        v == v' && pos == pos
    (/=) = (not .) . (==)   --todo rivedere

instance Ord Senpai where
    (<=) = (not .) . (>)
    (>) :: Senpai -> Senpai -> Bool
    (>) senpai senpai'
        | strength senpai == strength senpai' =
            randomizerStrength senpai > randomizerStrength senpai'
        | otherwise = strength senpai > strength senpai'

strength :: Senpai -> Int
strength (Senpai stats _) =
    umilta stats + coraggio stats + gentilezza stats + rispetto stats

randomizerStrength :: Senpai -> Int
randomizerStrength (Senpai _ (x, y)) = ((x + y) * (x + y - 1) `div` 2) + x - y

constructSenpai :: (Int, Int, Int, Int, Int, Int) -> Senpai
constructSenpai (x, y, u, c, g, r) =
    Senpai {
        stats = Virtues {
            umilta = u,
            coraggio = c,
            gentilezza = g,
            rispetto = r
        },
        currentPosition = (x, y)
    }

condIncrement :: Num a => a -> Bool -> a
condIncrement v c   -- incrementa v su condizione c
    | c = v + 1
    | otherwise = v

-- distanza è il numero di passi necessari per raggiungere l'altro punto
distance :: (Int, Int) -> (Int, Int) -> Int
distance a b = distx + disty
  where
    distx = fst b - fst a
    disty = snd b - snd a

nextStep :: Foldable t => (Int, Int) -> t (Int, Int) -> (Int, Int)
nextStep (x, y) globalVirtues
    | xDiff > yDiff = (x + incrementX, y)
    | otherwise = (x, y + incrementY)
    where
        incrementX
          | x < fst nearest = 1
          | x > fst nearest = -1
          | otherwise = 0
        incrementY
          | y < snd nearest = 1
          | y > snd nearest = -1
          | otherwise = 0
        xDiff = x - fst nearest
        yDiff = y - snd nearest
        nearest = (minimumBy . comparing) (distance (x, y)) globalVirtues

moveSenpai :: [(Int, Int)] -> Senpai -> Senpai
moveSenpai globalVirtues senpai = senpai {
    currentPosition =
        currentPosition senpai `nextStep` filter (/= currentPosition senpai) globalVirtues
}

-- todo rivedere
opponent :: [Senpai] -> Senpai -> Maybe Senpai
opponent incremented actual = do
    guard . not . null $ near
    return . head $ near
    where
        near = filter (\s -> currentPosition actual `distance` currentPosition s >= 1) . filter (/= actual) $ incremented

senpaiLevelUp cond actual =
    actual {
        stats =
            Virtues {
                umilta = c 'u' umilta,
                coraggio = c 'c' coraggio,
                gentilezza = c 'g' gentilezza,
                rispetto = c 'r' rispetto
            }
    }
    where c s f = (f . stats $ actual) `condIncrement` cond actual s f

fight :: [Senpai] -> [Senpai]
fight senpais =
    filter (\s -> Just s > opponent incremented s) incremented
    where
        incremented = map (senpaiLevelUp cond) senpais
        cond s _ f = maybe False (\s' -> (f . stats) s > (f . stats) s') $ opponent senpais s 

nextRound :: Checkboard -> Checkboard
nextRound (Checkboard size senpai pickups) =
    Checkboard {
        size = size,
        senpai = map (senpaiLevelUp cond) . fight $ moved,
        pickups = fmap filterPos pickups
    }
    where
        moved = (map . moveSenpai $ posToMove) senpai
        posToMove
            | null $ u ++ c ++ g ++ r = map currentPosition senpai 
            | otherwise = u ++ c ++ g ++ r
        filterPos = filter (`notElem` map currentPosition moved)
        cond (Senpai _ p) 'u' _ = p `elem` u
        cond (Senpai _ p) 'c' _ = p `elem` c
        cond (Senpai _ p) 'g' _ = p `elem` g
        cond (Senpai _ p) 'r' _ = p `elem` r
        u = umilta pickups
        c = coraggio pickups
        g = gentilezza pickups
        r = rispetto pickups
    
format :: [Char] -> [Char]
format "}}" = ")]"
format "}}," = ")]"
format "{}" = "[]"
format "{}," = "[]"
format ('{' : '{' : some) = '[' : '(' : format some
format ('{' : some) = '(' : format some
format ('}' : some) = ')' : format some
format (s : some) = s : format some

lineFormat :: Read a => String -> a
lineFormat ('\t' : _ : '=' : values) = read $ format values

constructCheckboard :: [String] -> Checkboard
constructCheckboard fileContents =
    Checkboard {
        size = n,
        senpai = senpai,
        pickups = Virtues {
            umilta = lines !! 3,
            coraggio = lines !! 4,
            gentilezza = lines !! 5,
            rispetto = lines !! 6
        }
    }
    where 
        n = read . drop 4 . head $ fileContents
        senpai = map constructSenpai . lineFormat $ fileContents !! 2
        lines = map lineFormat fileContents

runGame :: Checkboard -> [Checkboard]
runGame checkboard
    | (length . senpai) checkboard <= 1 = [checkboard]
    | otherwise = checkboard : (runGame . nextRound $ checkboard)

main = do
    putStrLn "Benvenuti al Dojo!"
    putStrLn "Inserire nome del file della configurazione:"

    -- aggiungi eccezioni
    fileName <- getLine
    fileHandle <- openFile fileName ReadMode
    fileContents <- hGetContents fileHandle
    let checkBoard = constructCheckboard . lines $ fileContents
    putStrLn "File caricato. Eseguendo ..."

    print (runGame checkBoard)
    putStrLn "Configurazione finale raggiunta!"
    hClose fileHandle
