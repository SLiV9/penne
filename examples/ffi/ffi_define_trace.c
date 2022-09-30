#include "stdio.h"

extern void trace(const char* text)
{
	printf("%s\n", text);
}
