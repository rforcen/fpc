/*
	btree.cpp c wrapper
	g++ -static -shared -O3 -o btree.dll btreeWrap.cpp btreedx.cpp
*/

#include "btreedx.h"

extern "C" {
	void *btCreate(char*fileName, int keyLen, int unique){
		BtreeDX *bt=new BtreeDX;
		bt->create((string)fileName,keyLen, unique);
		
		return (void*)bt;
	}
	
	void btClose(BtreeDX *bt){
		if (bt!=NULL) bt->close();
	}
	
	void *btOpen(char*fileName) {
		BtreeDX *bt=new BtreeDX;
		return bt->open((string)fileName) ? (void*)bt:NULL;
	}

        bool btFind(BtreeDX *bt, const char*key, int&recno) {
            return bt->find(key, recno);
        }

        bool btFindEQ(BtreeDX *bt, const char *key, int &recNo) {  // exact match
            return bt->findEQ(key, recNo);
        }

        bool btNext(BtreeDX *bt, char*key, int&recno) {
            return bt->next(key, recno);
        }

        bool btAdd(BtreeDX *bt, const char*key, const int recno) {
            return bt->add(key, recno);
        }

        bool btEraseEQ(BtreeDX *bt, const char *key) {  // exact match key erase
             return bt->eraseEQ(key);
        }

        int btEraseMatch(BtreeDX *bt, const char *key) {  //  erase ALL partial match -> return # keys deleted
            return bt->eraseMatch(key);
        }

        char *btGetKey(BtreeDX *bt) { return bt->getKey(); }

        int btGetRecNo(BtreeDX *bt) { return bt->getRecNo(); }  // after key search

        int btGetNnodes(BtreeDX *bt) { return bt->getNnodes(); }

        int btGetKeyLength(BtreeDX *bt) { return bt->getKeyLen(); }

        void printSizes() {
             printf("sizeof bool:%d\n",sizeof(bool));
             printf("sizeof int:%d\n",sizeof(int));
             printf("sizeof char:%d\n",sizeof(char));
             printf("sizeof pointer:%d\n",sizeof(void*));
        }

}
