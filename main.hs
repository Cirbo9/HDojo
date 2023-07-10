import System.IO (openFile, hGetContents, IOMode (ReadMode), hClose)
import GHC.IO.IOMode


-- posizione è rappresentata da valori int perché si tratta di una scacchiera
type Position = (Int, Int)

-- Virtu può rappresentare la qualità del senpai o l'oggetto raccoglibile
data Virtu a = U a
    | C a
    | G a
    | R a


data Stats = Stats {
    umilta :: Int,
    coraggio :: Int,
    gentilezza :: Int,
    rispetto :: Int
}

data Senpai = Senpai {
    currentPosition :: Position,
    stats :: Stats
}

-- i raccoglibili sono rappresentati solamente dalla posizione che occupano nella scacchiera
data Checkboard = Checkboard {
    size :: Int,
    senpai :: [Senpai],
    u :: [Virtu Position],
    c :: [Virtu Position],
    g :: [Virtu Position],
    r :: [Virtu Position]
}
    
constructCheckboard fileContents = 
    Checkboard {
        size = n,
        senpai = senpai,
        u = Virtu pos,
        c = Virtu pos,
        g = Virtu pos,
        r = Virtu pos,
    }
    where
        n = read . drop 4 . head $ fileContents
        senpai = 
        pos =
            
constructDefaultSenpai x y = constructSenpai x y 0 0 0 0
constructSenpai x y u c g r = Senpai {
    currentPosition = (x, y),
    stats = Stats {
        umilta = u,
        coraggio = c,
        gentilezza = g,
        rispetto = r
    }
}

moveRight senpai = 
    Senpai {
        currentPosition = (x + 1, y),
        stats = stats
    }
    where x = fst . posizione senpai
        y = snd . posizione senpai
        stats = stats senpai

-- distanza è il numero di passi necessari per raggiungere l'altro punto
distance :: Position -> Position -> Int
distance a b = distx + disty
    where distx = fst b - fst a
          disty = snd b - snd a

-- ricava posizione da una lista di int
position :: [Int] -> Position
position [] = (0, 0)
position (x:y:xs) = (x, y)


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
