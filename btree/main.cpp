#include <charconv>
#include <assert.h>
#include "btreedx.h"

const int n = 10000;

void find_all()
{
  BtreeDX bt;
  if (bt.open("test.ndx"))
  {
    // find keys
    puts("\nfinding keys...");
    char key[9];
    int recno;
    bool ok = true;

    for (int i = 0; i < n; i++)
    {
      sprintf(key, "%05d", i);
      if (!bt.find(key, recno))
      {
        ok = false;
        printf("%s,%d, ", key, recno);
      }
      else
        assert(i == recno && "recno not match");
    }
    if (ok)
      puts("ok, end");
    else
      puts("found errors");
  }
  else
    puts("error opening file");
}
void create_ins()
{
  BtreeDX bt;
  if (bt.create("test.ndx", 8))
  {

    puts("created ok!");

    // insert keys
    puts("inserting keys...");
    char key[9];
    for (int i = 0; i < n; i++)
    {
      sprintf(key, "%05d", i);
      bt.add(key, i);
      if (i % 1000 == 0)
        printf("\r%d", i);
    }

    // find keys
    puts("\nfinding keys...");
    int recno;
    bool ok = true;
    for (int i = 0; i < n; i++)
    {
      sprintf(key, "%05d", i);
      if (!bt.find(key, recno))
      {
        ok = false;
        printf("%s,%d, ", key, recno);
      }
      assert(i == recno && "recno not match");
    }

    if (ok)
      puts("ok, end");
    else
      puts("found errors");

    bt.close();
  }
  else
    puts("error creating file");
}

void traverse() {
  BtreeDX bt;
  char key[8];
  int recno;

  if (bt.open("test.ndx"))
  {
   do {
      printf("%s %d, ",key, recno);
  } while (bt.next(key, recno));
  }
  bt.close();
}

int main()
{

   //create_ins();
   traverse();
  // find_all();
}
