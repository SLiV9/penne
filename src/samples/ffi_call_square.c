#include "stdint.h"

extern uint32_t square(uint32_t x);

int main()
{
	uint32_t x = 8;
	uint32_t y = square(x);
	if (y == 64)
	{
		return 0;
	}
	else
	{
		return -1;
	}
}
