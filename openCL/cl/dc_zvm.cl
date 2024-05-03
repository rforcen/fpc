// Domain Coloring unsing zvm
// code is a 64 bit for both pcodes & real real
// dc_zvm.cl
//
//  sudo ln -s /usr/lib/x86_64-linux-gnu/libnvidia-compiler.so.465.24.02
//  /usr/lib/libnvidia-compiler.so clcc "-cl-nv-cstd=CL2.0" cl/dc_zvm.cl
//  cl/dc_zvm.ptx

#pragma OPENCL EXTENSION cl_khr_fp64 : enable

// typedef float real;
// typedef float2 real2;
// typedef float3 real3;

// complex arithmetics: +,-, neg direct real2 support
real2 mul(real2 a, real2 b) {
  return (real2)(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}
real2 div(real2 a, real2 b) {
  real _div = (b.x * b.x) + (b.y * b.y);

  return _div != 0 ? (real2)(((a.x * b.x) + (a.y * b.y)) / _div,
                             ((a.y * b.x) - (a.x * b.y)) / _div)
                   : (real2)(0, 0);
}

real cabs(real2 a) { return dot(a, a); }
real sqmag(real2 a) { return dot(a, a); }
real arg(real2 a) { return atan2(a.y, a.x); }

real2 cnpow(real2 a, real n) {
  real rn = pow(length(a), n), na = n * arg(a);
  return (real2)(rn * cos(na), rn * sin(na));
}

real2 cpow(real2 a, real2 z) {
  real c = z.x, d = z.y;
  real m = pow(sqmag(a), c / 2) * exp(-d * arg(a));
  real _re = m * cos(c * arg(a) + 1 / 2 * d * log(sqmag(a))),
       _im = m * sin(c * arg(a) + 1 / 2 * d * log(sqmag(a)));
  return (real2)(_re, _im);
}

real2 csqrt(real2 z) {
  real a = length(z);
  return (real2)(sqrt((a + z.x) / 2), sign(z.y) * sqrt((a - z.x) / 2));
}

real2 clog(real2 z) { return (real2)(log(length(z)), arg(z)); }

real2 ccosh(real2 z) {
  return (real2)(cosh(z.x) * cos(z.y), sinh(z.x) * sin(z.y));
}
real2 csinh(real2 z) {
  return (real2)(sinh(z.x) * cos(z.y), cosh(z.x) * sin(z.y));
}
real2 csin(real2 z) {
  return (real2)(sin(z.x) * cosh(z.y), cos(z.x) * sinh(z.y));
}
real2 ccos(real2 z) {
  return (real2)(cos(z.x) * cosh(z.y), -sin(z.x) * sinh(z.y));
}
real2 ctan(real2 z) { return div(sin(z), cos(z)); }

real2 casinh(real2 z) {
  real2 t = (real2)((z.x - z.y) * (z.x + z.y) + 1, 2 * z.x * z.y);
  return log(sqrt(t) + z);
}

real2 casin(real2 z) {
  real2 t = asinh((real2)(-z.y, z.x));
  return (real2)(t.y, -t.x);
}
real2 cacos(real2 z) {
  real2 t = asin(z);
  return (real2)(1.7514f - t.x, -t.y);
}

////////////////////////

uint argbf2uint(uint alpha, real r, real g, real b) {
  return (alpha << 24) | ((uint)(255 * r) & 0xffu) |
         (((uint)(255 * g) & 0xffu) << 8) | (((uint)(255 * b) & 0xffu) << 16);
}

uint rgbf2uint(real r, real g, real b) {
  return 0xff000000u | // alpha 0xff
         ((uint)(r * 255) & 0xffu) | (((uint)(g * 255) & 0xffu) << 8) |
         (((uint)(b * 255) & 0xffu) << 16);
}

uint HSV2RGB(real h, real s, real v) { // convert hsv to rgb,1
  real3 res;

  if (s == 0) {
    res = (real3)(v, v, v);
  } else {
    if (h == 1)
      h = 0;

    real z = floor(h * 6), f = h * 6 - z, p = v * (1 - s), q = v * (1 - s * f),
         t = v * (1 - s * (1 - f));

    switch ((int)(z) % 6) {
    case 0:
      res = (real3)(v, t, p);
      break;
    case 1:
      res = (real3)(q, v, p);
      break;
    case 2:
      res = (real3)(p, v, t);
      break;
    case 3:
      res = (real3)(p, q, v);
      break;
    case 4:
      res = (real3)(t, p, v);
      break;
    case 5:
      res = (real3)(v, p, q);
      break;
    }
  }
  return rgbf2uint(res.x, res.y, res.z);
}

real2 domain_color_func(real2); // domain coloring func

// zvm evaluator
real2 eval_zvn(real2 z, global ulong *code) {
  enum Symbols {
    SNULL = 0,
    NUMBER = 1,
    IDENTi = 2,
    IDENTz = 3,
    PLUS = 5,
    MINUS = 6,
    MULT = 7,
    DIV = 8,
    OPAREN = 9,
    CPAREN = 10,
    POWER = 12,
    PERIOD = 13,
    COMMA = 14,

    // function names
    FSIN = 90,
    FCOS = 91,
    FTAN = 92,
    FEXP = 93,
    FLOG = 94,
    FLOG10 = 95,
    FINT = 96,
    FSQRT = 97,
    FASIN = 98,
    FACOS = 99,
    FATAN = 100,
    FABS = 101,
    FC = 102,
    SPI = 103,
    SPHI = 104,
    PUSHC = 112,
    PUSHZ = 113,
    PUSHI = 114,
    PUSHCC = 115,
    NEG = 116,

    END = 200,
  };
  int pc = 0, sp = 0;
#define MAX_STACK 128
  real2 stack[MAX_STACK];

  for (;;) {
    switch (code[pc]) {
    case PUSHC:
      stack[sp++] = (real2)(*(global double *)(code + (++pc)), 0);
      break;
    case PUSHZ:
      stack[sp++] = z;
      break;
    case PUSHI:
      stack[sp++] = (real2)(1, 0);
      break;
    case PLUS:
      sp--;
      stack[sp - 1] += stack[sp];
      break;
    case MINUS:
      sp--;
      stack[sp - 1] -= stack[sp];
      break;
    case MULT:
      sp--;
      stack[sp - 1] = mul(stack[sp - 1], stack[sp]);
      break;
    case DIV:
      sp--;
      stack[sp - 1] = div(stack[sp - 1], stack[sp]);
      break;

    case POWER:
      sp--;
      stack[sp - 1] = stack[sp].y == 0 ? cnpow(stack[sp - 1], stack[sp].x)
                                       : cpow(stack[sp - 1], stack[sp]);
      break;

    case NEG:
      stack[sp - 1] = -stack[sp - 1];
      break;

    case FSQRT:
      stack[sp - 1] = csqrt(stack[sp - 1]);
      break;

    case FSIN:
      stack[sp - 1] = csin(stack[sp - 1]);
      break;
    case FCOS:
      stack[sp - 1] = ccos(stack[sp - 1]);
      break;
    case FTAN:
      stack[sp - 1] = ctan(stack[sp - 1]);
      break;
    case FASIN:
      stack[sp - 1] = casin(stack[sp - 1]);
      break;
    case FACOS:
      stack[sp - 1] = cacos(stack[sp - 1]);
      break;

    case FLOG:
      stack[sp - 1] = clog(stack[sp - 1]);
      break;
    case FLOG10:
      break;
    case FEXP:
      break;

    case FC:
      sp--;
      stack[sp - 1] = (real2)(stack[sp - 1].x, stack[sp].x);
      break;

    case END:
      goto finished;
      break;
    }
    pc++;
  }

finished:
  return sp != 0 ? stack[sp - 1] : (real2)(0, 0);
}

uint dc_get_color(int x, int y, int w, int h, global ulong *code) {

  real E = 2.7182818284f, PI = 3.141592653f, PI2 = PI * 2;
  real limit = PI, rmi = -limit, rma = limit, imi = -limit, ima = limit;

  real2 z =
      (real2)(ima - (ima - imi) * y / (h - 1), rma - (rma - rmi) * x / (w - 1));

  // real2 v = domain_color_func(z); // fixed evaluate domain coloring func
  real2 v = eval_zvn(z, code); // evaluate ZVM domain coloring func

  real m, ranges, rangee; //  prop. e^n < m < e^(n-1)
  for (m = length(v), ranges = 0, rangee = 1; m > rangee; rangee *= E)
    ranges = rangee;

  real k = (m - ranges) / (rangee - ranges),
       kk = (k < 0.5f ? k * 2 : 1 - (k - 0.5f) * 2);

  real ang = fmod(fabs(arg(v)), PI2) / PI2, // -> hsv
      sat = 0.4f + (1 - pow(1 - kk, 3)) * 0.6f,
       val = 0.6f + (1 - pow(1 - (1 - kk), 3)) * 0.4f;

  return HSV2RGB(ang, sat, val);
}

/////////////////////// example of fixed domain coloring func

real2 domain_color_func(real2 z) { // f(z)
  // return mul(z, z);
  real2 z1 = div(cnpow(z, 4) + 1, cnpow(z, 3) - 1);
  // real2 z1 = mul(cnpow(z, 4), cos(z)) + cnpow(z, 4);
  z1 += mul(z / 5, csin(z));
  return z1;
}

kernel void domain_coloring(
    global uint *image, // 0: image
    global ulong *code  // 1: code (END), int in nim is 64 bit -> ulong
) {
  size_t index = get_global_id(0);
  int width = (int)sqrt((real)get_global_size(0)); // w x w = n

  int x = (int)(index / width),
      y = (int)(index % width); // point(x,y)

  image[index] = dc_get_color(x, y, width, width, code);
}