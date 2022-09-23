#include "stdint.h"

uint8_t foo()
{
	uint8_t buffer[3] = {20, 32, 0x00};
	uint8_t* x = buffer;
	buffer[2] = 200;
	return x[2];
}

int main()
{
	uint8_t y = foo();
	return (int) y;
}
