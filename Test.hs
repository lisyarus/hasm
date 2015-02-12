module Test where

import Assembler

fact n = getRegister ebx $ runAssembler [n] $ do
	mov eax 0
	load eax eax -- load n to counter
	mov ebx 1
	test eax eax
	loopnz $ do
		mult ebx eax
		dec eax

sum lst = getRegister ebx $ runAssembler lst $ do
	mov ebx 0
	dec eax
	loopnz $ do
		load ecx eax
		add ebx ecx
		dec eax

max lst = getRegister ebx $ runAssembler lst $ do
	mov ebx 0
	dec eax
	loopnz $ do
		load ecx eax
		gt ecx ebx
		ifnz $ do
			swap ecx ebx
		dec eax
	
