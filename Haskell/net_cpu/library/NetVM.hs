{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module NetVM where

import Data.Word (Word, Word8)
import qualified Data.ByteString as BS (ByteString)
import Data.Binary (Binary (get, put), Get, Put)
import qualified Data.Text as T (Text)

import Control.Comonad (Comonad (..))
import Control.Lens

data Instruction
    -- Pushes a word on the stack.
    = Push Word
    -- Pops (deletes) a word from the stack.
    | Pop
    -- Binary operations.
    | Add | Sub | Mul | Div | Mod
    -- Sends the top value the stack to the master and stops execution.
    | Return
  deriving Show

put8 :: Word8 -> Put
put8 = put

get8 :: Get Word8
get8 = get

instance Binary Instruction where
    put (Push w) = put8 0 >> put w
    put Pop      = put8 1
    put Add      = put8 2
    put Sub      = put8 3
    put Mul      = put8 4
    put Div      = put8 5
    put Mod      = put8 6
    put Return   = put8 7

    get = do
        c <- get8
        case c of
            0 -> Push <$> get
            1 -> return Pop
            2 -> return Add
            3 -> return Sub
            4 -> return Mul
            5 -> return Div
            6 -> return Mod
            7 -> return Return

data PacketType
    = Result
    | Work
  deriving Show

instance Binary PacketType where
    put Result = put8 0
    put Work   = put8 1

    get = do
        c <- get8
        case c of
            0 -> return Result
            1 -> return Work

data Packet payload = PKT
    { _pktType    :: PacketType
    , _pktJobNo   :: Word
    , _pktPayload :: payload
    }
  deriving Show

makeLenses ''Packet


instance Binary payload => Binary (Packet payload) where
    put pkt = do
        put (pkt ^. pktType)
        put (pkt ^. pktJobNo)
        put (pkt ^. pktPayload)

    get = PKT <$> get <*> get <*> get


instance Functor Packet where
    fmap f = (pktPayload %~ f)


instance Comonad Packet where
    extract       = (^. pktPayload)
    duplicate pkt = (pktPayload .~ pkt) pkt


type Packet' = Packet BS.ByteString

data ExecState
    = Running
    | Error T.Text
    | Finished Word
  deriving (Show, Eq)

data MachineState = MS
    { _msStack     :: [Word]
    , _msProgram   :: [Instruction]
    , _msExecState :: ExecState
    }
  deriving Show

makeLenses ''MachineState


stepMachineState :: MachineState -> MachineState
stepMachineState ms = case (ms ^. msExecState, ms ^. msProgram) of
    (Running, [])   -> msExecState .~ Error "Unexpected end of program" $ ms
    (Running, i:_)  -> runInstr i $ msProgram %~ tail $ ms
    (Finished _, _) -> ms
    (Error _, _)    -> ms
  where
      opOnStack :: (Word -> Word -> Word) -> [Word] -> [Word]
      opOnStack f (x:y:xs) = f x y:xs
      opOnStack _ _ = error "low stack"

      mkOp :: Instruction -> (Word -> Word -> Word)
      mkOp Add = (+)
      mkOp Sub = (-)
      mkOp Div = div
      mkOp Mul = (*)
      mkOp Mod = mod
      mkOp _ = error "no operator"

      runInstr :: Instruction -> MachineState -> MachineState
      runInstr (Push w) ms = ms & msStack %~ (w:)
      runInstr Pop ms
          | ms ^. msStack . to null = ms & msExecState .~ Error "Empty Stack"
          | otherwise               = ms & msStack %~ tail
      runInstr Return ms
          | ms ^. msStack . to null = ms & msExecState .~ Error "Empty Stack"
          | otherwise               = ms & msExecState .~ Finished (ms ^. msStack . to head)
      runInstr op ms
          | ms ^. msStack . to length < 2 = ms & msExecState .~ Error "Not enough arguments"
          | otherwise                     = ms & msStack %~ opOnStack (mkOp op)


isRunning = msExecState . to (== Running)


runMachine :: MachineState -> MachineState
runMachine ms
    | ms ^. isRunning = runMachine . stepMachineState $ ms
    | otherwise       = ms


fetchProgramm :: Monad m => [Instruction] -> Consumer Instruction m [Instruction]
fetchProgramm acc = do
    mi <- await
    case mi of
        Nothing -> yield $ reverse acc
        Just p  -> fetchProgramm (p:acc)

buildMachine :: Monad m => Conduit Instruction m MachineState
buildMachine = do

