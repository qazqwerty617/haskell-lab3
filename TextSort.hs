-- | Лабораторна робота №3, варіант 13.
-- Обробка тексту підручника з програмування.
-- Завдання: відсортувати слова тексту за зменшенням кількості входжень
-- заданого символу, а в разі рівності - за абеткою.
--
-- Компіляція: ghc -O2 TextSort.hs
-- Запуск:     ./TextSort [файл]   (за замовчуванням textbook.txt)
module Main (main) where

import Prelude hiding (Word)

import Control.Monad      (forM_, unless)
import Data.Char          (isAlphaNum, isSpace, isUpper, toLower)
import Data.Function      (on)
import Data.List          (elemIndex, groupBy, nubBy, sortBy)
import Data.Ord           (Down (..), comparing)
import System.Environment (getArgs)
import System.IO

------------------------------------------------------------------------
-- Типи даних тексту

-- | Символ: будь-який знак тексту
newtype Symbol = Symbol Char

-- | Розділовий знак
newtype Punctuation = Punctuation Char

-- | Слово: послідовність символів (літери, цифри, апостроф і дефіс усередині)
newtype Word = Word [Symbol]

-- | Елемент речення: слово або розділовий знак
data Token = TWord Word | TPunct Punctuation

-- | Речення (пропозиція): послідовність слів і розділових знаків
newtype Sentence = Sentence [Token]

-- | Увесь текст: послідовність речень
newtype Text = Text [Sentence]

------------------------------------------------------------------------
-- Клас для перетворення елементів тексту назад у рядок

class Render a where
  render :: a -> String

instance Render Symbol where
  render (Symbol c) = [c]

instance Render Punctuation where
  render (Punctuation c) = [c]

instance Render Word where
  render (Word s) = concatMap render s

instance Render Token where
  render (TWord w)  = render w
  render (TPunct p) = render p

-- | Збираємо речення з правильними пробілами: перед комою чи крапкою
-- пробілу немає, після відкривальної дужки теж, а тире відокремлюється з обох боків
instance Render Sentence where
  render (Sentence ts) = concat (zipWith glue (Nothing : map Just ts) ts)
    where
      glue Nothing t = render t
      glue (Just prev) t
        | noSpaceBefore t || noSpaceAfter prev = render t
        | otherwise                            = ' ' : render t
      noSpaceBefore (TPunct (Punctuation c)) = c `elem` ",.;:!?)]»…"
      noSpaceBefore _                        = False
      noSpaceAfter (TPunct (Punctuation c))  = c `elem` "([«"
      noSpaceAfter _                         = False

instance Render Text where
  render (Text ss) = unwords (map render ss)

------------------------------------------------------------------------
-- Розбір тексту

-- | Табуляції, переходи рядка і послідовності пробілів замінюємо одним пробілом
normalizeSpaces :: String -> String
normalizeSpaces = unwords . words

-- | Апостроф і дефіс належать слову, лише якщо стоять між літерами:
-- пам'ять, будь-який, введенням-виведенням
isJoiner :: Char -> Bool
isJoiner c = c `elem` "'’ʼ-"

tokenize :: String -> [Token]
tokenize [] = []
tokenize s@(c : cs)
  | isSpace c    = tokenize cs
  | isAlphaNum c = let (w, rest) = spanWord s in TWord (Word (map Symbol w)) : tokenize rest
  | otherwise    = TPunct (Punctuation c) : tokenize cs
  where
    spanWord = go []
    go acc (x : rest) | isAlphaNum x = go (x : acc) rest
    go acc (j : x : rest)
      | isJoiner j, isAlphaNum x, not (null acc) = go (x : j : acc) rest
    go acc rest = (reverse acc, rest)

-- | Речення закінчується на . ! ? або … (кілька знаків поспіль, як "?!", теж належать йому)
splitSentences :: [Token] -> [Sentence]
splitSentences [] = []
splitSentences ts =
  let (body, rest)  = break isEnd ts
      (ends, rest') = span isEnd rest
  in Sentence (body ++ ends) : splitSentences rest'
  where
    isEnd (TPunct (Punctuation c)) = c `elem` ".!?…"
    isEnd _                        = False

parseText :: String -> Text
parseText = Text . splitSentences . tokenize . normalizeSpaces

------------------------------------------------------------------------
-- Робота зі словами

sentenceWords :: Sentence -> [Word]
sentenceWords (Sentence ts) = [w | TWord w <- ts]

textWords :: Text -> [Word]
textWords (Text ss) = concatMap sentenceWords ss

lowerWord :: Word -> String
lowerWord = map toLower . render

-- | Слова без повторів (без урахування регістру), записані малими літерами.
-- Велику літеру залишаємо лише власним назвам: тим словам, що пишуться
-- з великої не на початку речення (як Haskell)
distinctWords :: Text -> [String]
distinctWords text@(Text ss) = map shape (nubBy ((==) `on` lowerWord) (textWords text))
  where
    properNames = [ render w | s <- ss, w <- drop 1 (sentenceWords s), startsUpper w ]
    startsUpper w = case render w of
      (c : _) -> isUpper c
      _       -> False
    shape w = let l = lowerWord w
              in head ([ p | p <- properNames, map toLower p == l ] ++ [l])

-- | Скільки разів символ c трапляється в слові (великі й малі літери не розрізняємо)
countSymbol :: Char -> String -> Int
countSymbol c = length . filter ((== toLower c) . toLower)

-- | Ключ для порівняння за українською абеткою. Звичайне порівняння рядків
-- у Haskell йде за кодами Unicode, і тоді "є", "і", "ї", "ґ" опиняються не на своїх місцях
alphabet :: String
alphabet = "0123456789abcdefghijklmnopqrstuvwxyzабвгґдеєжзиіїйклмнопрстуфхцчшщьюя"

alphaKey :: String -> [Int]
alphaKey = map pos . filter (not . isJoiner) . map toLower
  where
    pos ch = maybe (length alphabet + fromEnum ch) id (elemIndex ch alphabet)

-- | Головна функція варіанта: за спаданням кількості символу, при рівності за абеткою
sortBySymbol :: Char -> [String] -> [(String, Int)]
sortBySymbol c ws =
  sortBy (comparing (Down . snd) <> comparing (alphaKey . fst))
         [(w, countSymbol c w) | w <- ws]

------------------------------------------------------------------------
-- Вивід

-- | Правильна форма слова: 1 входження, 2 входження, 5 входжень
occurrences :: Int -> String
occurrences n
  | n `mod` 100 `elem` [11 .. 14] = show n ++ " входжень"
  | n `mod` 10 `elem` [1 .. 4]    = show n ++ " входження"
  | otherwise                     = show n ++ " входжень"

-- | Перенесення довгого списку слів по рядках, щоб не виходив за ширину екрана
wrap :: Int -> String -> [String] -> [String]
wrap width indent = go indent
  where
    go line []       = [line | line /= indent]
    go line (w : ws)
      | line == indent                       = go (line ++ w) ws
      | length line + 2 + length w <= width  = go (line ++ ", " ++ w) ws
      | otherwise                            = (line ++ ",") : go (indent ++ w) ws

printSorted :: Char -> [(String, Int)] -> IO ()
printSorted c sorted = do
  putStrLn ("\nСлова, відсортовані за кількістю символу '" ++ [c] ++ "' (великі й малі літери однакові):")
  let groups = groupBy ((==) `on` snd) sorted
  forM_ groups $ \g -> do
    let n = snd (head g)
    putStrLn ("\n  " ++ occurrences n ++ " (" ++ show (length g) ++ " сл.):")
    mapM_ putStrLn (wrap 92 "    " (map fst g))

askSymbol :: IO (Maybe Char)
askSymbol = do
  putStr "\nВведіть символ (Enter без символу - вихід): "
  hFlush stdout
  eof <- isEOF
  if eof then pure Nothing else do
    s <- filter (not . isSpace) <$> getLine
    case s of
      []  -> pure Nothing
      [c] -> pure (Just c)
      _   -> putStrLn "  Потрібен рівно один символ, спробуйте ще раз." >> askSymbol

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout]
  hSetBuffering stdout NoBuffering
  args <- getArgs
  let file = case args of
        (f : _) -> f
        []      -> "textbook.txt"
  raw <- withFile file ReadMode $ \h -> hSetEncoding h utf8 >> hGetContents' h

  let text@(Text sentences) = parseText raw
      allWords = textWords text
      unique   = distinctWords text

  putStrLn ("Файл: " ++ file)
  putStrLn ("Символів у файлі: " ++ show (length raw)
         ++ ", після заміни табуляцій і зайвих пробілів: " ++ show (length (render text)))
  putStrLn ("Речень: " ++ show (length sentences) ++ ", слів: " ++ show (length allWords)
         ++ ", різних слів: " ++ show (length unique))

  putStrLn "\nТекст, розбитий на речення:"
  forM_ (zip [1 :: Int ..] sentences) $ \(i, s) ->
    putStrLn ("  " ++ show i ++ ". " ++ render s)

  let loop = do
        mc <- askSymbol
        case mc of
          Nothing -> putStrLn "Роботу завершено."
          Just c  -> do
            let sorted = sortBySymbol c unique
            unless (any ((> 0) . snd) sorted) $
              putStrLn ("  Символ '" ++ [c] ++ "' у словах тексту не зустрічається, тому всі слова просто йдуть за абеткою.")
            printSorted c sorted
            loop
  loop
