#include "foo.hpp"

int Foo::getBar()
{
  return bar;
}

int Foo::isBarWithinRange(int min, int max)
{	
  return bar > min && bar < max;	
}
