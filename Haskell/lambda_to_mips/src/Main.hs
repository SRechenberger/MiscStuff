module Main where

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except

import Control.Lens

import Data.List

import qualified Data.Map as M

--------------------------------------------------------------------------------
-- Language Definition ---------------------------------------------------------
--------------------------------------------------------------------------------

data Lambda
  = Atom String
  | App Lambda Lambda
  | Abs String Lambda
  deriving (Eq, Read)

free :: Lambda -> [String]
free (Atom s) = [s]
free (App f a) = nub $ free f ++ free a
free (Abs s e) = filter (/= s) (free e)

instance Show Lambda where
  show (Atom s) = s
  show (App f a) = "(" ++ show f ++ " " ++ show a ++ ")"
  show (Abs x e) = "(\\" ++ x ++ " . " ++ show e ++ ")"


--------------------------------------------------------------------------------
-- Code Generation -------------------------------------------------------------
--------------------------------------------------------------------------------

-- Auxiliaries -----------------------------------------------------------------
type Codegen a = WriterT [String] (StateT (Int, Int) (Except String)) a

type Register = String
type Address = String
type Label = String

dataSegment :: Codegen ()
dataSegment = tell [".data"]

asciiz :: Label -> String -> Codegen ()
asciiz n s = tell ["\t" ++ n ++ ": .asciiz " ++ show s]

textSegment :: Codegen ()
textSegment = tell [".text"]

cmd :: String -> Codegen ()
cmd s = tell ['\t':s]

cmd1 :: String -> String -> Codegen ()
cmd1 c a1 = cmd $ c ++ " " ++ a1

cmd2 :: String -> String -> String -> Codegen ()
cmd2 c a1 a2 = cmd $ c ++ " " ++ a1 ++ ", " ++ a2

cmd3 :: String -> String -> String -> String -> Codegen ()
cmd3 c a1 a2 a3 = cmd $ c ++ " " ++ a1 ++ ", " ++ a2 ++ ", " ++ a3

comment :: String -> Codegen ()
comment c = tell ["# " ++ c]

label :: Label -> Codegen ()
label l = tell [l ++ ":"]

addr :: Int -> Maybe Register -> String
addr i Nothing = show i
addr i (Just r) = show i ++ "(" ++ r ++ ")"

li :: Register -> Int -> Codegen ()
li r i = cmd2 "li" r (show i)

la :: Register -> String -> Codegen ()
la r a = cmd2 "la" r a

syscall :: Codegen ()
syscall = cmd "syscall"

jalr :: Register -> Codegen ()
jalr r = cmd1 "jalr" r

jr :: Register -> Codegen ()
jr r = cmd1 "jr" r

move :: Register -> Register -> Codegen ()
move rd rs = cmd2 "move" rd rs

j :: Label -> Codegen ()
j l = cmd1 "j" l

lw :: Register -> Address -> Codegen ()
lw r a = cmd2 "lw" r a

sw :: Register -> Address -> Codegen ()
sw r a = cmd2 "sw" r a

addi :: Register -> Register -> Int -> Codegen ()
addi rd rs i = cmd3 "addi" rd rs (show i)


newLabel :: Label -> Codegen Label
newLabel l = do
  i <- use _1
  _1 += 1
  return $ l ++ show i

newGlobal :: Codegen Int
newGlobal = do
  i <- use _2
  _2 += 1
  return i

-- Code Generation Functions ---------------------------------------------------

type Depth = Int
type Offset = Int

data Place
  = Register String
  | Local Depth
  | Label String
  | Global Offset
  deriving (Show, Eq)

type Scope = M.Map String Place

code :: Lambda -> Codegen ()
code l = do
  let frees = free l
  dataSegment
  forM_ frees $ \x -> do
    asciiz x x
  textSegment
  comment $ "BEGIN " ++ show l
  move "$fp" "$sp"
  codeP (M.fromList $ frees `zip` (map Label frees)) l
  comment $ "END " ++ show l
  li "$v0" 10
  syscall

rho :: Scope -> String -> Codegen Place
rho scope var = case scope ^. at var of
                  Nothing -> throwError $ "Variable " ++ var ++ " not in scope."
                  Just l  -> return l

codeP :: Scope -> Lambda -> Codegen ()
codeP scope expr@(Atom s) = do
  comment $ "BEGIN codeP " ++ show expr
  l <- rho scope s
  case l of
    Register r -> move "$a0" r
    Local d    -> throwError "There cannot be any local vars yet."
    Label s    -> la "$a0" s
    Global i   -> lw "$a0" (addr ((-i) * 4) (Just "$gp"))
  li "$v0" 4
  syscall
  comment $ "END codeP " ++ show expr
codeP scope expr@(App f a) = do
  comment $ "BEGIN codeP " ++ show expr
  codeE scope f 0
  move "$v0" "$v1"
  codeE scope a 0
  move "$a0" "$v1"
  jalr "$v0"
  move "$a0" "$v1"
  li "$v0" 4
  syscall
  comment $ "END codeP " ++ show expr
codeP _ (Abs _ _) = throwError $ "Cannot print an abstraction."

codeE :: Scope -> Lambda -> Depth -> Codegen ()
codeE scope expr@(Atom s) d = do
  comment $ "BEGIN codeE " ++ show expr
  a <- rho scope s
  case a of
    Register s -> move "$v1" s
    Local d'   -> lw "$v1" (addr ((d-d')*12) (Just "$sp"))
    Label s    -> la "$v1" s
    Global i   -> do
      a <- use _2
      addi "$t0" "$gp" ((-a) * 4)
      addi "$t0" "$t0" (i*4)
      lw "$v1" (addr 0 (Just "$t0"))

  comment $ "END codeE " ++ show expr
codeE scope expr@(App f a) d = do
  comment $ "BEGIN codeE " ++ show expr
  codeE scope f d
  move "$v0" "$v1"
  codeE scope a d
  move "$a0" "$v1"
  jalr "$v0"
  comment $ "END codeE " ++ show expr

codeE scope expr@(Abs v e) d = do
  comment $ "BEGIN codeE " ++ show expr
  begin <- newLabel "lambda"
  let end = begin ++ "_end"

  j end

  label begin
  -- Building Stack Frame
  comment "Building Stack Frame"
  addi "$sp" "$sp" (-12)
  sw "$fp" (addr 8 (Just "$sp"))
  move "$fp" "$sp"
  sw "$ra" (addr 4 (Just "$sp"))
  sw "$a0" (addr 0 (Just "$sp"))

  sw "$a0" (addr 0 (Just "$gp"))
  addi "$gp" "$gp" 4
  i <- newGlobal
  codeE (scope & at v ?~ Global i) e (d+1)

  -- Removing Stack Frame
  comment "Removing Stack Frame"
  lw "$fp" (addr 8 (Just "$sp"))
  lw "$ra" (addr 4 (Just "$sp"))
  lw "$a0" (addr 0 (Just "$sp"))
  addi "$sp" "$sp" 12
  -- Returning
  jr "$ra"
  label end
  la "$v1" begin
  comment $ "END codeE " ++ show expr




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


main :: IO ()
main = do
  expr <- read <$> getContents
  let s = runExcept . flip evalStateT (0,0) . execWriterT $ code expr
    --    (App (App (Abs "x" (Abs "y" (Atom "x"))) (Atom "a")) (Atom "b"))
    {-    (App
          (App
            (App
                (Abs "f" (Abs "b" (Abs "a"
                  (App
                    (App
                      (Atom "f")
                      (Atom "a"))
                    (Atom "b")))))
            (Abs "a" (Abs "b"
              (Atom "a"))))
            (Atom "x"))
          (Atom "y"))-}
  case s of
    Left e -> putStrLn e
    Right cs -> putStrLn $ unlines $ cs
