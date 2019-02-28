#ifndef FOO_H
#define FOO_H

class Foo {	
public:
  Foo(int _bar) : bar(_bar) {}	
  int getBar();	
  int isBarWithinRange(int min, int max);	
private:	
  int bar;	
};

#endif
