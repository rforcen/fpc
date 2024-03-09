/*
    128 bit integer support

    functions are not used as return values are not compatible

    g++ -D__USE_MINGW_ANSI_STDIO -O3 -static -shared  -o fp128.dll fp128.cpp

*/

#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <math.h>

typedef long double fp128;

extern "C" {
	
	// assign
	void putf128(fp128&c, double a) { c=a; }
	
	// arithmetic
	void addf128(fp128&c, fp128 a, fp128 b) { c=a+b; }
	void subf128(fp128&c, fp128 a, fp128 b) { c=a-b; }
	void mulf128(fp128&c, fp128 a, fp128 b) { c=a*b; }
	void divf128(fp128&c, fp128 a, fp128 b) { c=a/b; }
        void negf128(fp128&c, fp128 a) { c=-a; }
        void powf128(fp128&c, fp128 a, fp128 b) { c=pow(a,b); }
	
	// compare
	void gtf128(int&c, fp128 a, fp128 b) { c=a>b; }
	void gef128(int&c, fp128 a, fp128 b) { c=a>=b; }
	void ltf128(int&c, fp128 a, fp128 b) { c=a<b; }
	void lef128(int&c, fp128 a, fp128 b) { c=a<=b; }
	void eqf128(int&c, fp128 a, fp128 b) { c=a==b; }
	void nef128(int&c, fp128 a, fp128 b) { c=a!=b; }
	
	
	// misc
	void incf128(fp128 &a) { a++; }
	void decf128(fp128 &a) { a--; }

        // funcs
        void _truncf128(fp128&c, fp128 a) {c=trunc(a);}
        void _sqrtf128(fp128&c, fp128 a) {c=sqrt(a);}
        void _sqrf128(fp128&c, fp128 a) {c=a*a;}

        // string conversion
	void f128toa(char*ds, fp128 x)	{ sprintf(ds, "%Le", x); }
	void atof128(fp128*a, char *s)	{ char *e; *a = std::strtold(s, &e); }
}
