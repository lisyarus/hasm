{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

--module Assembler (runAssembler, getMemory, getRegister, eax, ebx, ecx, edx, inc, dec, mov, swap, add, sub, test, gt, mult, ifnz, loopnz, store, load) where
module Assembler where

import qualified Data.Int
import Data.Array
import Data.Bits
import Control.Monad.State.Lazy

type RegisterType = Data.Int.Int32

data RegistersContext = RegistersContext
	{
		_eax :: RegisterType,
		_ebx :: RegisterType,
		_ecx :: RegisterType,
		_edx :: RegisterType,
		_zflag :: Bool
	}
	deriving (Show)

data Context = Context
	{
		_registers :: RegistersContext,
		_memory :: Array RegisterType RegisterType
	}
	deriving (Show)

type Register = (Context -> RegisterType, Context -> RegisterType -> Context)

eax = (_eax . _registers, \ctx value -> ctx { _registers = (_registers ctx) { _eax = value } })
ebx = (_ebx . _registers, \ctx value -> ctx { _registers = (_registers ctx) { _ebx = value } })
ecx = (_ecx . _registers, \ctx value -> ctx { _registers = (_registers ctx) { _ecx = value } })
edx = (_edx . _registers, \ctx value -> ctx { _registers = (_registers ctx) { _edx = value } })

zflag :: Bool -> State Context ()
zflag value = do
	ctx <- get
	let ctx1 = ctx { _registers = (_registers ctx) { _zflag = value } }
	put ctx1

reg_fn_1 :: (RegisterType -> RegisterType) -> Register -> State Context ()
reg_fn_1 f1 (g1,s1) = do
	ctx <- get
	let r = f1 (g1 ctx)
	let ctx1 = s1 ctx r
	put ctx1
	zflag (r /= 0)

inc :: Register -> State Context ()
inc = reg_fn_1 (+ 1)

dec :: Register -> State Context ()
dec = reg_fn_1 (\x -> x - 1)

mov :: Register -> RegisterType -> State Context ()
mov reg value = reg_fn_1 (const value) reg

reg_fn_2 :: (RegisterType -> RegisterType -> RegisterType) -> (RegisterType -> RegisterType -> RegisterType) -> Register -> Register -> State Context ()
reg_fn_2 f1 f2 (g1,s1) (g2,s2) = do
	ctx <- get
	let r = (f1 (g1 ctx) (g2 ctx))
	let ctx1 = s1 ctx r
	let ctx2 = s2 ctx1 (f2 (g1 ctx) (g2 ctx))
	put ctx2
	zflag (r /= 0)

add = reg_fn_2 (+) (\x y -> y)
sub = reg_fn_2 (-) (\x y -> y)
mult = reg_fn_2 (*) (\x y -> y)

test :: Register -> Register -> State Context ()
test (g1,_) (g2,_) = do
	ctx <- get
	zflag (((g1 ctx) .&. (g2 ctx)) /= 0)

gt :: Register -> Register -> State Context ()
gt (g1,_) (g2,_) = do
	ctx <- get
	zflag ((g1 ctx) > (g2 ctx))

swap = reg_fn_2 (\x y -> y) (\x y -> x)

ifnz :: State Context () -> State Context ()
ifnz f = do
	ctx <- get
	if (_zflag $ _registers $ ctx) then
		f
	else
		return ()

loopnz :: State Context () -> State Context ()
loopnz f = ifnz $ f >> loopnz f

store :: Register -> Register -> State Context ()
store (value,_) (addr,_) = do
	ctx <- get
	put $ ctx { _memory = (_memory ctx) // [(addr ctx, value ctx)] }

load :: Register -> Register -> State Context ()
load (_,value) (addr,_) = do
	ctx <- get
	put $ value ctx (_memory ctx ! (addr ctx))

initContext memory = Context
	{
		_registers = RegistersContext { _eax = 0, _ebx = 0, _ecx = 0, _edx = 0, _zflag = False },
		_memory = array (0, memorySize - 1) (zip [0, 1 ..] memory)
	}
	where memorySize = fromIntegral $ length memory

runAssembler :: [RegisterType] -> (State Context ()) -> Context
runAssembler memory f = execState (do
	mov eax (fromIntegral $ length memory)
	f) (initContext memory)

getMemory :: Context -> [RegisterType]
getMemory = elems . _memory

getRegister :: Register -> Context -> RegisterType
getRegister (g,_) ctx = g ctx
