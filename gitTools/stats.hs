import Control.Monad (join, liftM, liftM2)
import Data.List (nub, sort)
import System.Directory (getDirectoryContents, removeFile)
import System.IO (readFile)
import System.Process (runCommand, waitForProcess)

-- This is a quick and dirty tool to print time-series stats about the project

getStatsFiles = liftM (filter ((==) "stats.txt." . take 10)) . getDirectoryContents

getNumberedFilenames :: Monad m => m [[Char]] -> m [(Integer, [Char])]
getNumberedFilenames = liftM (map (\x -> ((\y -> read y :: Integer) . take 3 . drop 10 $ x, x)))

parseStatsFile (num, name) = liftM (map (\x -> (num, x)) . concat . map (points . concatUntilNum . words) . dropSecondToLast . init . drop 3 . lines . dropWhile (== '-') . dropWhile (/= '-')) . readFile $ name

dropSecondToLast = liftM2 (++) (init . init) ((:[]) . last)

points x = [(x!!0, "files", read (x!!1) :: Integer), (x!!0, "blank", read (x!!2) :: Integer), (x!!0, "comment", read (x!!3) :: Integer), (x!!0, "code", read (x!!4) :: Integer)]

concatUntilNum x = (concat . takeWhile (not . flip elem "0123456789" . head) $ x) : (dropWhile (not . flip elem "0123456789" . head) x)

getAllPoints :: FilePath -> IO [[(Integer, (String, [Char], Integer))]]
getAllPoints = join . liftM (sequence . map parseStatsFile) . getNumberedFilenames . getStatsFiles

-- (Integer, (String, [Char], Integer))
-- (commit , (lang  , attr  , value  ))

-- Want lang[ attr[ (commit, value) ] ]

attributes = ["files","blank","comment","code"]

t1 (a,b,c) = a
t3 (a,b,c) = c

reformat (a, (b,c,d)) = ((b,c),(a,d))

allLangs :: [[(Integer, (String, [Char], Integer))]] -> [String]
allLangs = sort . nub . map (t1 . snd) . concat

allStats x = [(lang, [(attr, map snd . sort . nub . map snd . filter ((== (lang, attr)) . fst) . map reformat . concat $ x) | attr <- attributes]) | lang <- allLangs x]

printOne (lang, attrs) = do
  a <- putStrLn $ lang ++ ":"
  b <- mapM_ (\(attr,vals) -> putStrLn . ("    "++) $ attr ++ " : " ++ show vals) attrs
  seq a (seq b (return ()))


-- | `main` generates stats files, prints the time series', then deletes the stats files
main = do
  process <- runCommand "./dump_all_versions.sh"
  waiter <- waitForProcess process
  printStats <- join . liftM (mapM_ printOne . allStats) . getAllPoints $ "."
  removeFiles <- join $ liftM (mapM_ removeFile) (getStatsFiles ".")
  seq waiter (seq printStats (seq removeFiles (putStrLn "done")))
