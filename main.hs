
-- posizione è rappresentata da valori int perché si tratta di una scacchiera
type Position = (Int, Int)

-- type Virtu = Int
-- data Virtu = U | C | G | R

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
    senpai :: [Senpai],
    u :: [Virtu Position],
    c :: [Virtu Position],
    g :: [Virtu Position],
    r :: [Virtu Position]
}

constructDefaultSenpai (x, y) = constructSenpai (x, y, 0, 0, 0, 0)
constructSenpai (x, y, u, c, g, r) = Senpai {
    currentPosition = (x, y),
    stats = Stats {
        umilta = u,
        coraggio = c,
        gentilezza = g,
        rispetto = r
    }
}

-- distanza è il numero di passi necessari per raggiungere l'altro punto
-- esempio chiamata: distance ((0,0), (1,1))
distance :: (Position, Position) -> Int
distance (a, b) = distx + disty
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

