#pragma once
#include <string>
#include <vector>
#include <complex>
#include <algorithm>
#include <cstring>

using std::string;
using std::vector;
using Complex = std::complex<double>;
using byte = unsigned char;

class zCompiler
{
public:
  enum
  {
    SNULL = 0,
    NUMBER = 1,
    IDENT_i = 2,
    IDENT_z = 3,
    PLUS = 5,
    MINUS = 6,
    MULT = 7,
    DIV = 8,
    OPAREN = 9,
    CPAREN = 10,
    POWER = 12,
    PERIOD = 13,

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
    NEG = 115
  };

  vector<string> fname = {
      "SIN", "COS", "TAN", "EXP", "LOG", "LOG10", "INT", "SQRT", "ASIN",
      "ACOS", "ATAN", "ABS", "C", "PI", "PHI"};

  const double PHI = 0.6180339887;

  string s; // expression to evaluate;
  int is;

  int sym;     // actual sym
  char ch;     // actual ch
  double nval; // actual numerical value

  string id; // actual id,

  vector<double> consts;

  bool err;

  string errMessage;

  // compiler
  byte Code[4096 * 4];
  int pc, CodeSize;

public:
  zCompiler() {}

  void toUpper(string &s)
  {
    for (auto &c : s)
      c = toupper(c);
  }
  int indexOf(vector<string> ids, string id)
  {
    auto ix = std::find(ids.begin(), ids.end(), id);
    if (ix == ids.end())
      return -1;
    else
      return ix - ids.begin();
  }
  string getErrorMessage()
  {
    return errMessage;
  }

  bool compile(string expr)
  {
    pc = is = 0;
    id = "";
    s = expr;
    getch();
    err = false;
    getsym();
    Ce0();
    CodeSize = pc;
    return err;
  }

  Complex execute(Complex z)
  {
    // run time
    Complex stack[50];
    Complex CI = Complex(0., 1.);

    int sp = 0;

    for (int pc = 0; pc < CodeSize;)
    {
      switch (Code[pc])
      {
      case PUSHC:
        pc++;
        stack[sp] = Complex(consts[Code[pc]], 0.);
        sp++;
        pc++;
        break;
      case PUSHZ:
        pc++;
        stack[sp] = z;
        sp++;
        break;
      case PUSHI:
        pc++;
        stack[sp] = CI;
        sp++;
        break;
      case PLUS:
        sp--;
        stack[sp - 1] += stack[sp];
        pc++;
        break;
      case MINUS:
        sp--;
        stack[sp - 1] -= stack[sp];
        pc++;
        break;
      case MULT:
        sp--;
        stack[sp - 1] *= stack[sp];
        pc++;
        break;
      case DIV:
        sp--;
        stack[sp - 1] /= stack[sp];
        pc++;
        break;
      case POWER:
        sp--;
        stack[sp - 1] = pow(stack[sp - 1], stack[sp]);
        pc++;
        break;
      case NEG:
        stack[sp - 1] = -stack[sp - 1];
        pc++;
        break;

      case FSIN:
        stack[sp - 1] = sin(stack[sp - 1]);
        pc++;
        break;
      case FCOS:
        stack[sp - 1] = cos(stack[sp - 1]);
        pc++;
        break;
      case FTAN:
        stack[sp - 1] = tan(stack[sp - 1]);
        pc++;
        break;
      case FASIN:
        stack[sp - 1] = asin(stack[sp - 1]);
        pc++;
        break;
      case FACOS:
        stack[sp - 1] = acos(stack[sp - 1]);
        pc++;
        break;
      case FATAN:
        stack[sp - 1] = atan(stack[sp - 1]);
        pc++;
        break;
      case FEXP:
        stack[sp - 1] = exp(stack[sp - 1]);
        pc++;
        break;
      case FLOG:
        stack[sp - 1] = log(stack[sp - 1]);
        pc++;
        break;
      case FSQRT:
        stack[sp - 1] = sqrt(stack[sp - 1]);
        pc++;
        break;
      case FC:
        sp--;
        pc++;
        stack[sp - 1] = Complex(stack[sp - 1].real(), stack[sp].real());
        break;
      default:
        err = true;
        break;
      }
    }
    if (sp != 0)
      return stack[sp - 1];
    else
      return Complex(0, 0);
  }

private:
  bool islower(char c)
  {
    return (c >= 'a' && c <= 'z');
  }
  bool isupper(char c)
  {
    return (c >= 'A' && c <= 'Z');
  }
  bool isalpha(char c)
  {
    return (islower(c) || isupper(c));
  }
  bool isdigit(char c)
  {
    return (c >= '0' && c <= '9');
  }
  bool isalnum(char c)
  {
    return (isalpha(c) || isdigit(c));
  }
  // get next char from *s
  char getch()
  {
    ch = 0;

    if (is < s.size())
    {
      ch = s[is];
      is++;
    }

    return ch;
  }
  void ungetch()
  {
    is--;
  }

  // get next symbol
  int getsym()
  {
    int i;

    sym = SNULL;
    id = "";

    // skip blanks
    while (ch != 0 && ch <= ' ')
      getch();
    // detect symbol
    if (isalpha(ch))
    { // ident
      id = "";
      for (i = 0; isalnum(ch) || ch == '_'; i++)
      {
        id += ch;
        getch();
      }
      sym = IDENT_i;
      toUpper(id); // look up for 'x' or 't'

      if (id == "Z")
        sym = IDENT_z;
      else if (id == "I")
        sym = IDENT_i;
      else
      {
        // is a func ?
        int ix = 0;
        if ((ix = indexOf(fname, id)) != -1)
        {
          sym = ix + FSIN; // first symbol offset
        }
        else
        {
          sym = 0;
          error("unknown symbol:" + id);
        }
      }
    }
    else
    {
      if (isdigit(ch))
      { // number (double) take care of dddd.ddde-dd
        for (i = 0; isdigit(ch) || ch == '.' || ch == 'e' ||
                    ch == 'E';
             i++)
        {
          id += ch;
          getch();
        }
        sym = NUMBER;
        try
        {
          nval = std::stof(id);
        }
        catch (const std::exception &e)
        {
          nval = 0;
          error("malformed number:" + id);
        }
      }
      else
      {
        switch (ch)
        {
        case '+':
          sym = PLUS;
          break;
        case '-':
          sym = MINUS;
          break;
        case '*':
          sym = MULT;
          break;
        case '/':
          sym = DIV;
          break;
        case '(':
          sym = OPAREN;
          break;
        case ')':
          sym = CPAREN;
          break;
        case '^':
          sym = POWER;
          break;
        case ',':
          sym = PERIOD;
          break;
        case 0:
          sym = SNULL;
          break;

        default:
          sym = SNULL;
          error(string("character not recognized: ") + ch);
          break;
        }
        getch();
      }
    }
    return sym;
  }
  void error(string text)
  {
    errMessage = text;
    err = true;
  }
  void gen(int token, double f)
  { // code Generation
    Code[pc++] = (byte)token;
    Code[pc++] = (byte)consts.size();
    consts.push_back(f);
  }
  void gen(int token, byte i)
  {
    Code[pc++] = (byte)token;
    Code[pc++] = (byte)i;
  }
  void gen(int token)
  {
    Code[pc++] = (byte)token;
  }
  void Ce0()
  {
    if (!err)
    {
      Ce1();
      do
      {
        switch (sym)
        {
        case PLUS:
          getsym();
          Ce1();
          gen(PLUS);
          break;
        case MINUS:
          getsym();
          Ce1();
          gen(MINUS);
          break;
        default:
          break;
        }
      } while (sym == PLUS || sym == MINUS);
    }
  }
  void Ce1()
  {
    if (!err)
    {
      Ce2();
      do
      {
        switch (sym)
        {
        case MULT:
          getsym();
          Ce2();
          gen(MULT);
          break;
        case DIV:
          getsym();
          Ce2();
          gen(DIV);
          break;
        }
      } while (sym == MULT || sym == DIV);
    }
  }
  void Ce2()
  {
    if (!err)
    {
      Ce3();
      do
      {
        if (sym == POWER)
        {
          getsym();
          Ce3();
          gen(POWER);
        }
      } while (sym == POWER);
    }
  }
  void Ce3()
  {
    if (!err)
    {
      switch (sym)
      {
      case OPAREN:
        getsym();
        Ce0();
        getsym();
        break;
      case NUMBER:
        gen(PUSHC, nval);
        getsym();
        break;
      case IDENT_i:
        gen(PUSHI);
        getsym();
        break;
      case IDENT_z:
        gen(PUSHZ);
        getsym();
        break;
      case MINUS:
        getsym();
        Ce3();
        gen(NEG);
        break;
      case PLUS:
        getsym();
        Ce3();
        break;

      case FSIN:
      case FCOS:
      case FTAN:
      case FASIN:
      case FACOS:
      case FATAN:
      case FEXP:
      case FINT:
      case FABS:
      case FLOG:
      case FLOG10:
      case FSQRT:
      {
        int tsym = sym;
        getsym();
        Ce3();
        gen(tsym);
      }
      break;

      case FC:
        getsym();
        getsym();
        Ce3();
        getsym();
        Ce3();
        getsym();
        gen(FC);
        break;

      case SPI:
        getsym();
        gen(PUSHC, M_PI);
        break;
      case SPHI:
        getsym();
        gen(PUSHC, PHI);
        break;

      case SNULL:
        break;
      default:
        error("unknown symbol: " + id);
        break; // syntax error
      }
    }
  }
};
