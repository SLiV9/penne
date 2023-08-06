#include "stdint.h"

uint32_t square(uint32_t x)
{
	return x * x;
}

uint32_t triple(uint32_t x)
{
	return x + x + x;
}

uint32_t VALUE = 77;
uint32_t DATA[1000];

uint32_t (*SQUARE)(uint32_t) = square;
uint32_t (*TRIPLE)(uint32_t) = triple;
uint32_t *VALUE_PTR = &VALUE;
uint32_t *DATA_PTR = DATA;

//uintptr_t SQUARE_PLUS_ONE = 1 + (uintptr_t) (void*) SQUARE;
//uintptr_t TRIPLE_PLUS_ONE = 1 + (uintptr_t) (void*) TRIPLE;
//uintptr_t VALUE_PTR_PLUS_ONE = 1 + (uintptr_t) (void*) VALUE_PTR;
//uintptr_t DATA_PTR_PLUS_ONE = 1 + (uintptr_t) (void*) DATA_PTR;
