module VM where

--import Data.Array
import Data.Array.IO
import Data.Bits

import CodeGen

data State = Run | Halt

data MachineState =
    MachineState {
        state  :: State,
        pc     :: Integer,
        mar    :: Integer,
        stack  :: [Integer],
        ret_stack :: [Integer],
        memory :: IOArray Integer Integer
    }

load :: [Instruction] -> IO MachineState
load inst = do
    mem <- newListArray (0, 2048) inst
    return $ MachineState {
        state = Run,
        pc = 0,
        mar = 0,
        stack = [],
        ret_stack = [],
        memory = mem
    }

run :: MachineState -> IO ()
run st =
    let ms = state st in
    case ms of
        Halt -> return ()
        Run  -> do cell <- readArray (memory st) (pc st)
                   let st' = st { pc = succ (pc st) }
                   let instruction = cell .&. 0xFF000000
                   case instruction of
                       -- JMP
                       0x01000000 -> do let address = cell .&. 0x00FFFFFF
                                        let st'' = st' { pc = address }
                                        run st''
                       -- JMZ
                       0x02000000 -> do let address = cell .&. 0x00FFFFFF
                                        let (x:xs) = stack st'
                                        if x == 0
                                            then do let st'' = st' { pc = address,
                                                                     stack = xs }
                                                    run st''
                                            else do let st'' = st' { stack = xs }
                                                    run st''
                       -- LOAD
                       0x03000000 -> do let address = cell .&. 0x00FFFFFF
                                        let mar' = address
                                        val <- readArray (memory st') mar'
                                        let st'' = st' { mar = mar',
                                                         stack = val:(stack st') }
                                        run st''

                       -- STORE
                       0x04000000 -> do let address = cell .&. 0x00FFFFFF
                                        let mar' = address
                                        let (x:xs) = stack st'
                                        writeArray (memory st') mar' x
                                        let st'' = st' { mar = mar',
                                                         stack = xs }
                                        run st''
                       -- WRITE
                       0x05000000 -> do let (x:xs) = stack st'
                                        print x
                                        let st'' = st' { stack = xs }
                                        run st''

                       -- ADD
                       0x06000000 -> do let (x:y:xs) = stack st'
                                        let st'' = st' { stack = (x + y):xs }
                                        run st''

                       -- SUB
                       0x07000000 -> do let (x:y:xs) = stack st'
                                        let st'' = st' { stack = (y - x):xs }
                                        run st''

                       -- DIV
                       0x08000000 -> do let (x:y:xs) = stack st'
                                        let st'' = st' { stack = (y `div` x):xs }
                                        run st''

                       -- MUL
                       0x09000000 -> do let (x:y:xs) = stack st'
                                        let st'' = st' { stack = (x * y):xs }
                                        run st''

                       -- LT
                       0x0A000000 -> do let (x:y:xs) = stack st'
                                        let val = if y < x then 1 else 0
                                        let st'' = st' { stack = val:xs }
                                        run st''

                       -- LTE
                       0x0B000000 -> do let (x:y:xs) = stack st'
                                        let val = if y <= x then 1 else 0
                                        let st'' = st' { stack = val:xs }
                                        run st''

                       -- GT
                       0x0C000000 -> do let (x:y:xs) = stack st'
                                        let val = if y > x then 1 else 0
                                        let st'' = st' { stack = val:xs }
                                        run st''

                       -- GTE
                       0x0D000000 -> do let (x:y:xs) = stack st'
                                        let val = if y >= x then 1 else 0
                                        let st'' = st' { stack = val:xs }
                                        run st''

                       -- EQ
                       0x0E000000 -> do let (x:y:xs) = stack st'
                                        let val = if y == x then 1 else 0
                                        let st'' = st' { stack = val:xs }
                                        run st''

                       -- NEQ
                       0x0F000000 -> do let (x:y:xs) = stack st'
                                        let val = if y /= x then 1 else 0
                                        let st'' = st' { stack = val:xs }
                                        run st''

                       -- HALT
                       0x1F000000 -> do let st'' = st' { state = Halt }
                                        run st''

                       -- NOOP | STARTFUNC
                       0x2F000000 -> do run st'

                       -- RET
                       0x3F000000 -> do let (x:xs) = ret_stack st'
                                        let st'' = st' { ret_stack = xs, pc = x }
                                        run st''

                       -- CALL
                       0x4F000000 -> do let address = cell .&. 0x00FFFFFF
                                        let ret = (pc st'):(ret_stack st')
                                        let st'' = st' { pc = address,
                                                         ret_stack = ret }
                                        run st''
