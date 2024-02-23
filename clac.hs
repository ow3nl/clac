module Main where
import Control.Monad.State ( modify, State, StateT (runStateT), runStateT, get, MonadIO (liftIO),
    lift )
import Control.Monad.Identity ( Identity )
import System.Environment ( getArgs )
import System.IO
    ( hGetContents, hClose, IOMode(ReadMode), openFile )
import Data.List (null, intersperse)
import GHC.Base (returnIO)
import Distribution.Compat.Prelude (exitSuccess, Binary (get))

type Commands = [(String, [String])]

data Env = Env {
    commands :: [(String, [String])],
    stack :: [Int],
    trace :: Bool }

tokIsInt :: String -> Bool
tokIsInt s = case reads s :: [(Int, String)] of
    [(num, "")] -> True
    _           -> False

-- line -> current word -> list of words
recParse :: String -> String -> [String]
recParse [] w = [w]
recParse (c:s) w = case c of
    ' '  -> case w of
        "" -> recParse s ""
        x  -> x : recParse s ""
    '\n' -> case w of
        "" -> recParse s ""
        x  -> x : recParse s ""
    c    -> recParse s (w++[c])

parse :: String -> [String]
parse s = recParse s ""

assignCommand :: [String] -> StateT Env IO ()
assignCommand [] = error "\":\" never closed with \";\""
assignCommand (t:ts) = do
    readCommandDef ts t []

-- tokens -> varName -> varActions -> State
readCommandDef :: [String] -> String -> [String] -> StateT Env IO ()
readCommandDef [] _ _ = error "\":\" never closed with \";\""
readCommandDef (t:ts) name actions = case t of
    ";" -> do
        env <- Control.Monad.State.get
        let c = commands env
        modify (\e -> e {commands = (name, actions):c})
        lift (putStrLn ("(defined " ++ name ++ ")"))
        env <- Control.Monad.State.get
        lift (printState env ts)
        readClacTokens ts
    _   -> do
        readCommandDef ts name (actions++[t])

printState :: Env -> [String] -> IO ()
printState e ts = do
    let st = stack e
    case st of
        [] -> do
            putStr (concat (replicate 38 " "))
        _ -> do
            putStr (concat (replicate (39 - length st * 2) " "))
    
    let tsOut = unwords ts
    if length tsOut <= 38 then
        putStrLn (unwords (map show st) ++ " || " ++ tsOut)
    else
        putStrLn (
            unwords (map show st) ++ " || " ++ take 35 tsOut ++ "...")

printHeader :: IO ()
printHeader = do
    let space = concat (replicate 33 " ")
    putStrLn (space ++ "stack || queue")

readClacTokens :: [String] -> StateT Env IO ()
readClacTokens [] = return ()
readClacTokens (t:ts)
    | t == ":"   = do
        assignCommand ts
    | t == "+"   = do
        env <- Control.Monad.State.get
        case stack env of
            (x:(y:ys)) -> do
                modify (\e -> e {stack = (y + x):ys})
                if trace env then do
                    env <- Control.Monad.State.get
                    lift (printState env ts)
                    readClacTokens ts
                else do
                    readClacTokens ts
            _          -> error "not enough elements on stack"
    | t == "-"   = do
        env <- Control.Monad.State.get
        case stack env of
            (x:(y:ys)) -> do
                modify (\e -> e {stack = (y - x):ys})
                if trace env then do
                    env <- Control.Monad.State.get
                    lift (printState env ts)
                    readClacTokens ts
                else do
                    readClacTokens ts
            _          -> error "not enough elements on stack"
    | t == "*"   = do
        env <- Control.Monad.State.get
        case stack env of
            (x:(y:ys)) -> do
                modify (\e -> e {stack = (y * x):ys})
                if trace env then do
                    env <- Control.Monad.State.get
                    lift (printState env ts)
                    readClacTokens ts
                else do
                    readClacTokens ts
            _          -> error "not enough elements on stack"
    | t == "/"   = do
        env <- Control.Monad.State.get
        case stack env of
            (x:(y:ys)) -> do
                modify (\e -> e {stack = (y `div` x):ys})
                if trace env then do
                    env <- Control.Monad.State.get
                    lift (printState env ts)
                    readClacTokens ts
                else do
                    readClacTokens ts
            _          -> error "not enough elements on stack"
    | t == "%"   = do
        env <- Control.Monad.State.get
        case stack env of
            (x:(y:ys)) -> do
                modify (\e -> e {stack = (y `mod` x):ys})
                if trace env then do
                    env <- Control.Monad.State.get
                    lift (printState env ts)
                    readClacTokens ts
                else do
                    readClacTokens ts
            _          -> error "not enough elements on stack"
    | t == "**"   = do
        env <- Control.Monad.State.get
        case stack env of
            (x:(y:ys)) -> do
                modify (\e -> e {stack = (y ^ x):ys})
                if trace env then do
                    env <- Control.Monad.State.get
                    lift (printState env ts)
                    readClacTokens ts
                else do
                    readClacTokens ts
            _          -> error "not enough elements on stack"
    | t == "<"   = do
        env <- Control.Monad.State.get
        case stack env of
            (x:(y:ys)) -> do
                if y < x then
                    modify (\e -> e {stack = 1:ys})
                else
                    modify (\e -> e {stack = 0:ys})
                if trace env then do
                    env <- Control.Monad.State.get
                    lift (printState env ts)
                    readClacTokens ts
                else do
                    readClacTokens ts
            _          -> error "not enough elements on stack"
    | t == "drop"   = do
        env <- Control.Monad.State.get
        case stack env of
            (x:xs) -> do
                modify (\e -> e {stack = xs})
                if trace env then do
                    env <- Control.Monad.State.get
                    lift (printState env ts)
                    readClacTokens ts
                else do
                    readClacTokens ts
            _      -> error "not enough elements on stack"
    | t == "swap"   = do
        env <- Control.Monad.State.get
        case stack env of
            (x:(y:ys)) -> do
                modify (\e -> e {stack = y:(x:ys)})
                if trace env then do
                    env <- Control.Monad.State.get
                    lift (printState env ts)
                    readClacTokens ts
                else do
                    readClacTokens ts
            _          -> error "not enough elements on stack"
    | t == "rot"   = do
        env <- Control.Monad.State.get
        case stack env of
            (x:(y:(z:zs))) -> do
                modify (\e -> e {stack = x:(z:(y:zs))})
                if trace env then do
                    env <- Control.Monad.State.get
                    lift (printState env ts)
                    readClacTokens ts
                else do
                    readClacTokens ts
            _              -> error "not enough elements on stack"
    | t == "pick"  = do
        env <- Control.Monad.State.get
        let (x:xs) = stack env
        if length (x:xs) > x then do
            modify (\e -> e {stack = (xs !! (x - 1)):xs})
            if trace env then do
                env <- Control.Monad.State.get
                lift (printState env ts)
                readClacTokens ts
            else do
                readClacTokens ts
        else
            error "not enough elements on stack"
    | t == "nop"   = pure ()
    | t == "print" = do
        env <- Control.Monad.State.get
        case stack env of
            (x:xs) -> do
                lift (print x)
                modify (\e -> e {stack = xs})
                if trace env then do
                    env <- Control.Monad.State.get
                    lift (printState env ts)
                    readClacTokens ts
                else do
                    readClacTokens ts
            _      -> error "not enough elements on stack"
    | t == "if" = do
        env <- Control.Monad.State.get
        case stack env of
            (x:xs) -> do
                if x == 0 then
                    if length ts >= 3 then do
                        modify (\e -> e {stack = xs})
                        if trace env then do
                            env <- Control.Monad.State.get
                            lift (printState env (drop 3 ts))
                            readClacTokens (drop 3 ts)
                        else do
                            readClacTokens (drop 3 ts)
                    else
                        error "not enough tokens in queue"
                else do
                    modify (\e -> e {stack = xs})
                    readClacTokens ts
            _      -> error "not enough elements on stack"
    | t == "skip" = do
        env <- Control.Monad.State.get
        case stack env of
            (x:xs) -> do
                if length ts >= x then do
                    modify (\e -> e {stack = xs})
                    if trace env then do
                        env <- Control.Monad.State.get
                        lift (printState env (drop x ts))
                        readClacTokens (drop x ts)
                    else do
                        readClacTokens (drop x ts)
                else
                    error "not enough tokens in queue"
            _      -> error "not enough elements on stack"
    | tokIsInt t   = do
        env <- Control.Monad.State.get
        let [(x, "")] = reads t :: [(Int, String)]
        modify (\e -> e {stack = x:stack env})
        if trace env then do
            env <- Control.Monad.State.get
            lift (printState env ts)
            readClacTokens ts
        else do
            readClacTokens ts
    | otherwise    = do
        env <- Control.Monad.State.get
        let c = commands env
        case lookup t c of
            Just val -> do readClacTokens (val ++ ts)
            Nothing  -> error ("undefined token " ++ t)

-- file contents -> Commands -> Stack -> State
readClacString :: String -> StateT Env IO ()
readClacString s = let ps = parse s in do
    env <- Control.Monad.State.get
    if trace env then do
        lift printHeader
        lift (printState env ps)
        readClacTokens ps
    else do
        readClacTokens ps

-- file names -> State
runFiles :: [String] -> StateT Env IO ()
runFiles [] = return ()
runFiles (f:fs) = do
    lift (putStrLn ("Loading file " ++ f))
    env <- Control.Monad.State.get
    handle <- lift (openFile f ReadMode)
    contents <- lift (hGetContents handle)
    readClacString contents
    lift (hClose handle)
    runFiles fs

recMain :: StateT Env IO ()
recMain = do
    lift (putStr "=>> ")
    input <- lift getLine
    case input of
        "quit" -> lift exitSuccess
        other  -> do
            readClacString other
            env <- Control.Monad.State.get
            case stack env of
                [] -> lift (putStrLn "(stack empty)")
                _  -> lift (print (head (stack env)))
            recMain

parseFlags :: [String] -> StateT Env IO [String]
parseFlags [] = return []
parseFlags (flag:args) = case head flag of
    '-' -> case tail flag of
        "trace" -> do
            modify (\e -> e {trace = True})
            parseFlags args
        -- other flags
    _   -> return (flag:args)

main :: IO ()
main = do
    let env = Env { stack = [], commands = [], trace = False }
    args <- getArgs
    (files, env) <- runStateT (parseFlags args) env
    ((), env) <- runStateT (runFiles files) env
    case stack env of
        [] -> putStrLn "(stack empty)"
        _  -> print (head (stack env))
    ((), env) <- runStateT recMain env
    exitSuccess
