# lzma wrap including bulk source c 

{.emit:"""
  /* lzma complete */

  /* Alloc.h -- Memory allocation functions
  2008-03-13
  Igor Pavlov
  Public domain */

  #include <stddef.h>
  #include <string.h>
  #include <stdlib.h>


  void *MyAlloc(size_t size);
  void MyFree(void *address);

  #define MidAlloc(size) MyAlloc(size)
  #define MidFree(address) MyFree(address)
  #define BigAlloc(size) MyAlloc(size)
  #define BigFree(address) MyFree(address)

  /* LzFind.h -- Match finder for LZ algorithms
  2008-10-04 : Igor Pavlov : Public domain */

  #ifndef __LZFIND_H
  #define __LZFIND_H

  /* Types.h -- Basic types
  2008-11-23 : Igor Pavlov : Public domain */

  #ifndef __7Z_TYPES_H
  #define __7Z_TYPES_H

  #define SZ_OK 0

  #define SZ_ERROR_DATA 1
  #define SZ_ERROR_MEM 2
  #define SZ_ERROR_CRC 3
  #define SZ_ERROR_UNSUPPORTED 4
  #define SZ_ERROR_PARAM 5
  #define SZ_ERROR_INPUT_EOF 6
  #define SZ_ERROR_OUTPUT_EOF 7
  #define SZ_ERROR_READ 8
  #define SZ_ERROR_WRITE 9
  #define SZ_ERROR_PROGRESS 10
  #define SZ_ERROR_FAIL 11
  #define SZ_ERROR_THREAD 12

  #define SZ_ERROR_ARCHIVE 16
  #define SZ_ERROR_NO_ARCHIVE 17

  typedef int SRes;
  typedef int WRes;

  #ifndef RINOK
  #define RINOK(x)          \
    {                       \
      int __result__ = (x); \
      if (__result__ != 0)  \
        return __result__;  \
    }
  #endif

  typedef unsigned char Byte;
  typedef short Int16;
  typedef unsigned short UInt16;

  #ifdef _LZMA_UINT32_IS_ULONG
  typedef long Int32;
  typedef unsigned long UInt32;
  #else
  typedef int Int32;
  typedef unsigned int UInt32;
  #endif

  #if defined(_MSC_VER) || defined(__BORLANDC__)
  typedef __int64 Int64;
  typedef unsigned __int64 UInt64;
  #else
  typedef long long int Int64;
  typedef unsigned long long int UInt64;
  #endif

  typedef size_t SizeT;

  typedef int Bool;
  #define True 1
  #define False 0

  #ifdef _MSC_VER

  #if _MSC_VER >= 1300
  #define MY_NO_INLINE __declspec(noinline)
  #else
  #define MY_NO_INLINE
  #endif

  #define MY_CDECL __cdecl
  #define MY_STD_CALL __stdcall
  #define MY_FAST_CALL MY_NO_INLINE __fastcall

  #else

  #define MY_CDECL
  #define MY_STD_CALL
  #define MY_FAST_CALL

  #endif

  /* The following interfaces use first parameter as pointer to structure */

  typedef struct
  {
    SRes (*Read)(void *p, void *buf, size_t *size);
    /* if (input(*size) != 0 && output(*size) == 0) means end_of_stream.
      (output(*size) < input(*size)) is allowed */
  } ISeqInStream;

  /* it can return SZ_ERROR_INPUT_EOF */
  SRes SeqInStream_Read(ISeqInStream *stream, void *buf, size_t size);
  SRes SeqInStream_Read2(ISeqInStream *stream, void *buf, size_t size, SRes errorType);
  SRes SeqInStream_ReadByte(ISeqInStream *stream, Byte *buf);

  typedef struct
  {
    size_t (*Write)(void *p, const void *buf, size_t size);
    /* Returns: result - the number of actually written bytes.
      (result < size) means error */
  } ISeqOutStream;

  typedef enum
  {
    SZ_SEEK_SET = 0,
    SZ_SEEK_CUR = 1,
    SZ_SEEK_END = 2
  } ESzSeek;

  typedef struct
  {
    SRes (*Read)(void *p, void *buf, size_t *size); /* same as ISeqInStream::Read */
    SRes (*Seek)(void *p, Int64 *pos, ESzSeek origin);
  } ISeekInStream;

  typedef struct
  {
    SRes (*Look)(void *p, void **buf, size_t *size);
    /* if (input(*size) != 0 && output(*size) == 0) means end_of_stream.
      (output(*size) > input(*size)) is not allowed
      (output(*size) < input(*size)) is allowed */
    SRes (*Skip)(void *p, size_t offset);
    /* offset must be <= output(*size) of Look */

    SRes (*Read)(void *p, void *buf, size_t *size);
    /* reads directly (without buffer). It is same as ISeqInStream::Read */
    SRes (*Seek)(void *p, Int64 *pos, ESzSeek origin);
  } ILookInStream;

  SRes LookInStream_LookRead(ILookInStream *stream, void *buf, size_t *size);
  SRes LookInStream_SeekTo(ILookInStream *stream, UInt64 offset);

  /* reads via ILookInStream::Read */
  SRes LookInStream_Read2(ILookInStream *stream, void *buf, size_t size, SRes errorType);
  SRes LookInStream_Read(ILookInStream *stream, void *buf, size_t size);

  #define LookToRead_BUF_SIZE (1 << 14)

  typedef struct
  {
    ILookInStream s;
    ISeekInStream *realStream;
    size_t pos;
    size_t size;
    Byte buf[LookToRead_BUF_SIZE];
  } CLookToRead;

  void LookToRead_CreateVTable(CLookToRead *p, int lookahead);
  void LookToRead_Init(CLookToRead *p);

  typedef struct
  {
    ISeqInStream s;
    ILookInStream *realStream;
  } CSecToLook;

  void SecToLook_CreateVTable(CSecToLook *p);

  typedef struct
  {
    ISeqInStream s;
    ILookInStream *realStream;
  } CSecToRead;

  void SecToRead_CreateVTable(CSecToRead *p);

  typedef struct
  {
    SRes (*Progress)(void *p, UInt64 inSize, UInt64 outSize);
    /* Returns: result. (result != SZ_OK) means break.
      Value (UInt64)(Int64)-1 for size means unknown value. */
  } ICompressProgress;

  typedef struct
  {
    void *(*Alloc)(void *p, size_t size);
    void (*Free)(void *p, void *address); /* address can be 0 */
  } ISzAlloc;

  #define IAlloc_Alloc(p, size) (p)->Alloc((p), size)
  #define IAlloc_Free(p, a) (p)->Free((p), a)

  #endif

  typedef UInt32 CLzRef;

  typedef struct _CMatchFinder
  {
    Byte *buffer;
    UInt32 pos;
    UInt32 posLimit;
    UInt32 streamPos;
    UInt32 lenLimit;

    UInt32 cyclicBufferPos;
    UInt32 cyclicBufferSize; /* it must be = (historySize + 1) */

    UInt32 matchMaxLen;
    CLzRef *hash;
    CLzRef *son;
    UInt32 hashMask;
    UInt32 cutValue;

    Byte *bufferBase;
    ISeqInStream *stream;
    int streamEndWasReached;

    UInt32 blockSize;
    UInt32 keepSizeBefore;
    UInt32 keepSizeAfter;

    UInt32 numHashBytes;
    int directInput;
    int btMode;
    /* int skipModeBits; */
    int bigHash;
    UInt32 historySize;
    UInt32 fixedHashSize;
    UInt32 hashSizeSum;
    UInt32 numSons;
    SRes result;
    UInt32 crc[256];
  } CMatchFinder;

  #define Inline_MatchFinder_GetPointerToCurrentPos(p) ((p)->buffer)
  #define Inline_MatchFinder_GetIndexByte(p, index) ((p)->buffer[(Int32)(index)])

  #define Inline_MatchFinder_GetNumAvailableBytes(p) ((p)->streamPos - (p)->pos)

  int MatchFinder_NeedMove(CMatchFinder *p);
  Byte *MatchFinder_GetPointerToCurrentPos(CMatchFinder *p);
  void MatchFinder_MoveBlock(CMatchFinder *p);
  void MatchFinder_ReadIfRequired(CMatchFinder *p);

  void MatchFinder_Construct(CMatchFinder *p);

  /* Conditions:
      historySize <= 3 GB
      keepAddBufferBefore + matchMaxLen + keepAddBufferAfter < 511MB
  */
  int MatchFinder_Create(CMatchFinder *p, UInt32 historySize,
                        UInt32 keepAddBufferBefore, UInt32 matchMaxLen, UInt32 keepAddBufferAfter,
                        ISzAlloc *alloc);
  void MatchFinder_Free(CMatchFinder *p, ISzAlloc *alloc);
  void MatchFinder_Normalize3(UInt32 subValue, CLzRef *items, UInt32 numItems);
  void MatchFinder_ReduceOffsets(CMatchFinder *p, UInt32 subValue);

  UInt32 *GetMatchesSpec1(UInt32 lenLimit, UInt32 curMatch, UInt32 pos, const Byte *buffer, CLzRef *son,
                          UInt32 _cyclicBufferPos, UInt32 _cyclicBufferSize, UInt32 _cutValue,
                          UInt32 *distances, UInt32 maxLen);

  /*
  Conditions:
    Mf_GetNumAvailableBytes_Func must be called before each Mf_GetMatchLen_Func.
    Mf_GetPointerToCurrentPos_Func's result must be used only before any other function
  */

  typedef void (*Mf_Init_Func)(void *object);
  typedef Byte (*Mf_GetIndexByte_Func)(void *object, Int32 index);
  typedef UInt32 (*Mf_GetNumAvailableBytes_Func)(void *object);
  typedef const Byte *(*Mf_GetPointerToCurrentPos_Func)(void *object);
  typedef UInt32 (*Mf_GetMatches_Func)(void *object, UInt32 *distances);
  typedef void (*Mf_Skip_Func)(void *object, UInt32);

  typedef struct _IMatchFinder
  {
    Mf_Init_Func Init;
    Mf_GetIndexByte_Func GetIndexByte;
    Mf_GetNumAvailableBytes_Func GetNumAvailableBytes;
    Mf_GetPointerToCurrentPos_Func GetPointerToCurrentPos;
    Mf_GetMatches_Func GetMatches;
    Mf_Skip_Func Skip;
  } IMatchFinder;

  void MatchFinder_CreateVTable(CMatchFinder *p, IMatchFinder *vTable);

  void MatchFinder_Init(CMatchFinder *p);
  UInt32 Bt3Zip_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances);
  UInt32 Hc3Zip_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances);
  void Bt3Zip_MatchFinder_Skip(CMatchFinder *p, UInt32 num);
  void Hc3Zip_MatchFinder_Skip(CMatchFinder *p, UInt32 num);

  #endif

  /* LzHash.h -- HASH functions for LZ algorithms
  2008-10-04 : Igor Pavlov : Public domain */

  #ifndef __LZHASH_H
  #define __LZHASH_H

  #define kHash2Size (1 << 10)
  #define kHash3Size (1 << 16)
  #define kHash4Size (1 << 20)

  #define kFix3HashSize (kHash2Size)
  #define kFix4HashSize (kHash2Size + kHash3Size)
  #define kFix5HashSize (kHash2Size + kHash3Size + kHash4Size)

  #define HASH2_CALC hashValue = cur[0] | ((UInt32)cur[1] << 8);

  #define HASH3_CALC                                            \
    {                                                           \
      UInt32 temp = p->crc[cur[0]] ^ cur[1];                    \
      hash2Value = temp & (kHash2Size - 1);                     \
      hashValue = (temp ^ ((UInt32)cur[2] << 8)) & p->hashMask; \
    }

  #define HASH4_CALC                                                                    \
    {                                                                                   \
      UInt32 temp = p->crc[cur[0]] ^ cur[1];                                            \
      hash2Value = temp & (kHash2Size - 1);                                             \
      hash3Value = (temp ^ ((UInt32)cur[2] << 8)) & (kHash3Size - 1);                   \
      hashValue = (temp ^ ((UInt32)cur[2] << 8) ^ (p->crc[cur[3]] << 5)) & p->hashMask; \
    }

  #define HASH5_CALC                                                       \
    {                                                                      \
      UInt32 temp = p->crc[cur[0]] ^ cur[1];                               \
      hash2Value = temp & (kHash2Size - 1);                                \
      hash3Value = (temp ^ ((UInt32)cur[2] << 8)) & (kHash3Size - 1);      \
      hash4Value = (temp ^ ((UInt32)cur[2] << 8) ^ (p->crc[cur[3]] << 5)); \
      hashValue = (hash4Value ^ (p->crc[cur[4]] << 3)) & p->hashMask;      \
      hash4Value &= (kHash4Size - 1);                                      \
    }

  /* #define HASH_ZIP_CALC hashValue = ((cur[0] | ((UInt32)cur[1] << 8)) ^ p->crc[cur[2]]) & 0xFFFF; */
  #define HASH_ZIP_CALC hashValue = ((cur[2] | ((UInt32)cur[0] << 8)) ^ p->crc[cur[1]]) & 0xFFFF;

  #define MT_HASH2_CALC \
    hash2Value = (p->crc[cur[0]] ^ cur[1]) & (kHash2Size - 1);

  #define MT_HASH3_CALC                                               \
    {                                                                 \
      UInt32 temp = p->crc[cur[0]] ^ cur[1];                          \
      hash2Value = temp & (kHash2Size - 1);                           \
      hash3Value = (temp ^ ((UInt32)cur[2] << 8)) & (kHash3Size - 1); \
    }

  #define MT_HASH4_CALC                                                                       \
    {                                                                                         \
      UInt32 temp = p->crc[cur[0]] ^ cur[1];                                                  \
      hash2Value = temp & (kHash2Size - 1);                                                   \
      hash3Value = (temp ^ ((UInt32)cur[2] << 8)) & (kHash3Size - 1);                         \
      hash4Value = (temp ^ ((UInt32)cur[2] << 8) ^ (p->crc[cur[3]] << 5)) & (kHash4Size - 1); \
    }

  #endif

  /* LzmaDec.h -- LZMA Decoder
  2008-10-04 : Igor Pavlov : Public domain */

  #ifndef __LZMADEC_H
  #define __LZMADEC_H

  /* #define _LZMA_PROB32 */
  /* _LZMA_PROB32 can increase the speed on some CPUs,
    but memory usage for CLzmaDec::probs will be doubled in that case */

  #ifdef _LZMA_PROB32
  #define CLzmaProb UInt32
  #else
  #define CLzmaProb UInt16
  #endif

  /* ---------- LZMA Properties ---------- */

  #define LZMA_PROPS_SIZE 5

  typedef struct _CLzmaProps
  {
    unsigned lc, lp, pb;
    UInt32 dicSize;
  } CLzmaProps;

  /* LzmaProps_Decode - decodes properties
  Returns:
    SZ_OK
    SZ_ERROR_UNSUPPORTED - Unsupported properties
  */

  SRes LzmaProps_Decode(CLzmaProps *p, const Byte *data, unsigned size);

  /* ---------- LZMA Decoder state ---------- */

  /* LZMA_REQUIRED_INPUT_MAX = number of required input bytes for worst case.
    Num bits = log2((2^11 / 31) ^ 22) + 26 < 134 + 26 = 160; */

  #define LZMA_REQUIRED_INPUT_MAX 20

  typedef struct
  {
    CLzmaProps prop;
    CLzmaProb *probs;
    Byte *dic;
    const Byte *buf;
    UInt32 range, code;
    SizeT dicPos;
    SizeT dicBufSize;
    UInt32 processedPos;
    UInt32 checkDicSize;
    unsigned state;
    UInt32 reps[4];
    unsigned remainLen;
    int needFlush;
    int needInitState;
    UInt32 numProbs;
    unsigned tempBufSize;
    Byte tempBuf[LZMA_REQUIRED_INPUT_MAX];
  } CLzmaDec;

  #define LzmaDec_Construct(p) \
    {                          \
      (p)->dic = 0;            \
      (p)->probs = 0;          \
    }

  void LzmaDec_Init(CLzmaDec *p);

  /* There are two types of LZMA streams:
      0) Stream with end mark. That end mark adds about 6 bytes to compressed size.
      1) Stream without end mark. You must know exact uncompressed size to decompress such stream. */

  typedef enum
  {
    LZMA_FINISH_ANY, /* finish at any point */
    LZMA_FINISH_END  /* block must be finished at the end */
  } ELzmaFinishMode;

  /* ELzmaFinishMode has meaning only if the decoding reaches output limit !!!

    You must use LZMA_FINISH_END, when you know that current output buffer
    covers last bytes of block. In other cases you must use LZMA_FINISH_ANY.

    If LZMA decoder sees end marker before reaching output limit, it returns SZ_OK,
    and output value of destLen will be less than output buffer size limit.
    You can check status result also.

    You can use multiple checks to test data integrity after full decompression:
      1) Check Result and "status" variable.
      2) Check that output(destLen) = uncompressedSize, if you know real uncompressedSize.
      3) Check that output(srcLen) = compressedSize, if you know real compressedSize.
          You must use correct finish mode in that case. */

  typedef enum
  {
    LZMA_STATUS_NOT_SPECIFIED,              /* use main error code instead */
    LZMA_STATUS_FINISHED_WITH_MARK,         /* stream was finished with end mark. */
    LZMA_STATUS_NOT_FINISHED,               /* stream was not finished */
    LZMA_STATUS_NEEDS_MORE_INPUT,           /* you must provide more input bytes */
    LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK /* there is probability that stream was finished without end mark */
  } ELzmaStatus;

  /* ELzmaStatus is used only as output value for function call */

  /* ---------- Interfaces ---------- */

  /* There are 3 levels of interfaces:
      1) Dictionary Interface
      2) Buffer Interface
      3) One Call Interface
    You can select any of these interfaces, but don't mix functions from different
    groups for same object. */

  /* There are two variants to allocate state for Dictionary Interface:
      1) LzmaDec_Allocate / LzmaDec_Free
      2) LzmaDec_AllocateProbs / LzmaDec_FreeProbs
    You can use variant 2, if you set dictionary buffer manually.
    For Buffer Interface you must always use variant 1.

  LzmaDec_Allocate* can return:
    SZ_OK
    SZ_ERROR_MEM         - Memory allocation error
    SZ_ERROR_UNSUPPORTED - Unsupported properties
  */

  SRes LzmaDec_AllocateProbs(CLzmaDec *p, const Byte *props, unsigned propsSize, ISzAlloc *alloc);
  void LzmaDec_FreeProbs(CLzmaDec *p, ISzAlloc *alloc);

  SRes LzmaDec_Allocate(CLzmaDec *state, const Byte *prop, unsigned propsSize, ISzAlloc *alloc);
  void LzmaDec_Free(CLzmaDec *state, ISzAlloc *alloc);

  /* ---------- Dictionary Interface ---------- */

  /* You can use it, if you want to eliminate the overhead for data copying from
    dictionary to some other external buffer.
    You must work with CLzmaDec variables directly in this interface.

    STEPS:
      LzmaDec_Constr()
      LzmaDec_Allocate()
      for (each new stream)
      {
        LzmaDec_Init()
        while (it needs more decompression)
        {
          LzmaDec_DecodeToDic()
          use data from CLzmaDec::dic and update CLzmaDec::dicPos
        }
      }
      LzmaDec_Free()
  */

  /* LzmaDec_DecodeToDic

    The decoding to internal dictionary buffer (CLzmaDec::dic).
    You must manually update CLzmaDec::dicPos, if it reaches CLzmaDec::dicBufSize !!!

  finishMode:
    It has meaning only if the decoding reaches output limit (dicLimit).
    LZMA_FINISH_ANY - Decode just dicLimit bytes.
    LZMA_FINISH_END - Stream must be finished after dicLimit.

  Returns:
    SZ_OK
      status:
        LZMA_STATUS_FINISHED_WITH_MARK
        LZMA_STATUS_NOT_FINISHED
        LZMA_STATUS_NEEDS_MORE_INPUT
        LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK
    SZ_ERROR_DATA - Data error
  */

  SRes LzmaDec_DecodeToDic(CLzmaDec *p, SizeT dicLimit,
                          const Byte *src, SizeT *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status);

  /* ---------- Buffer Interface ---------- */

  /* It's zlib-like interface.
    See LzmaDec_DecodeToDic description for information about STEPS and return results,
    but you must use LzmaDec_DecodeToBuf instead of LzmaDec_DecodeToDic and you don't need
    to work with CLzmaDec variables manually.

  finishMode:
    It has meaning only if the decoding reaches output limit (*destLen).
    LZMA_FINISH_ANY - Decode just destLen bytes.
    LZMA_FINISH_END - Stream must be finished after (*destLen).
  */

  SRes LzmaDec_DecodeToBuf(CLzmaDec *p, Byte *dest, SizeT *destLen,
                          const Byte *src, SizeT *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status);

  /* ---------- One Call Interface ---------- */

  /* LzmaDecode

  finishMode:
    It has meaning only if the decoding reaches output limit (*destLen).
    LZMA_FINISH_ANY - Decode just destLen bytes.
    LZMA_FINISH_END - Stream must be finished after (*destLen).

  Returns:
    SZ_OK
      status:
        LZMA_STATUS_FINISHED_WITH_MARK
        LZMA_STATUS_NOT_FINISHED
        LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK
    SZ_ERROR_DATA - Data error
    SZ_ERROR_MEM  - Memory allocation error
    SZ_ERROR_UNSUPPORTED - Unsupported properties
    SZ_ERROR_INPUT_EOF - It needs more bytes in input buffer (src).
  */

  SRes LzmaDecode(Byte *dest, SizeT *destLen, const Byte *src, SizeT *srcLen,
                  const Byte *propData, unsigned propSize, ELzmaFinishMode finishMode,
                  ELzmaStatus *status, ISzAlloc *alloc);

  #endif

  /*  LzmaEnc.h -- LZMA Encoder
  2008-10-04 : Igor Pavlov : Public domain */

  #ifndef __LZMAENC_H
  #define __LZMAENC_H

  #define LZMA_PROPS_SIZE 5

  typedef struct _CLzmaEncProps
  {
    int level;             /*  0 <= level <= 9 */
    UInt32 dictSize;       /* (1 << 12) <= dictSize <= (1 << 27) for 32-bit version
                              (1 << 12) <= dictSize <= (1 << 30) for 64-bit version
                              default = (1 << 24) */
    int lc;                /* 0 <= lc <= 8, default = 3 */
    int lp;                /* 0 <= lp <= 4, default = 0 */
    int pb;                /* 0 <= pb <= 4, default = 2 */
    int algo;              /* 0 - fast, 1 - normal, default = 1 */
    int fb;                /* 5 <= fb <= 273, default = 32 */
    int btMode;            /* 0 - hashChain Mode, 1 - binTree mode - normal, default = 1 */
    int numHashBytes;      /* 2, 3 or 4, default = 4 */
    UInt32 mc;             /* 1 <= mc <= (1 << 30), default = 32 */
    unsigned writeEndMark; /* 0 - do not write EOPM, 1 - write EOPM, default = 0 */
    int numThreads;        /* 1 or 2, default = 2 */
  } CLzmaEncProps;

  void LzmaEncProps_Init(CLzmaEncProps *p);
  void LzmaEncProps_Normalize(CLzmaEncProps *p);
  UInt32 LzmaEncProps_GetDictSize(const CLzmaEncProps *props2);

  /* ---------- CLzmaEncHandle Interface ---------- */

  /* LzmaEnc_* functions can return the following exit codes:
  Returns:
    SZ_OK           - OK
    SZ_ERROR_MEM    - Memory allocation error
    SZ_ERROR_PARAM  - Incorrect paramater in props
    SZ_ERROR_WRITE  - Write callback error.
    SZ_ERROR_PROGRESS - some break from progress callback
    SZ_ERROR_THREAD - errors in multithreading functions (only for Mt version)
  */

  typedef void *CLzmaEncHandle;

  CLzmaEncHandle LzmaEnc_Create(ISzAlloc *alloc);
  void LzmaEnc_Destroy(CLzmaEncHandle p, ISzAlloc *alloc, ISzAlloc *allocBig);
  SRes LzmaEnc_SetProps(CLzmaEncHandle p, const CLzmaEncProps *props);
  SRes LzmaEnc_WriteProperties(CLzmaEncHandle p, Byte *properties, SizeT *size);
  SRes LzmaEnc_Encode(CLzmaEncHandle p, ISeqOutStream *outStream, ISeqInStream *inStream,
                      ICompressProgress *progress, ISzAlloc *alloc, ISzAlloc *allocBig);
  SRes LzmaEnc_MemEncode(CLzmaEncHandle p, Byte *dest, SizeT *destLen, const Byte *src, SizeT srcLen,
                        int writeEndMark, ICompressProgress *progress, ISzAlloc *alloc, ISzAlloc *allocBig);

  /* ---------- One Call Interface ---------- */

  /* LzmaEncode
  Return code:
    SZ_OK               - OK
    SZ_ERROR_MEM        - Memory allocation error
    SZ_ERROR_PARAM      - Incorrect paramater
    SZ_ERROR_OUTPUT_EOF - output buffer overflow
    SZ_ERROR_THREAD     - errors in multithreading functions (only for Mt version)
  */

  SRes LzmaEncode(Byte *dest, SizeT *destLen, const Byte *src, SizeT srcLen,
                  const CLzmaEncProps *props, Byte *propsEncoded, SizeT *propsSize, int writeEndMark,
                  ICompressProgress *progress, ISzAlloc *alloc, ISzAlloc *allocBig);

  #endif

  /* LzmaLib.h -- LZMA library interface
  2008-08-05
  Igor Pavlov
  Public domain */

  #ifndef __LZMALIB_H
  #define __LZMALIB_H

  #ifdef __cplusplus
  #define MY_EXTERN_C extern "C"
  #else
  #define MY_EXTERN_C extern
  #endif

  #define MY_STDAPI MY_EXTERN_C int MY_STD_CALL

  #define LZMA_PROPS_SIZE 5

  /*
  RAM requirements for LZMA:
    for compression:   (dictSize * 11.5 + 6 MB) + state_size
    for decompression: dictSize + state_size
      state_size = (4 + (1.5 << (lc + lp))) KB
      by default (lc=3, lp=0), state_size = 16 KB.

  LZMA properties (5 bytes) format
      Offset Size  Description
        0     1    lc, lp and pb in encoded form.
        1     4    dictSize (little endian).
  */

  /*
  LzmaCompress
  ------------

  outPropsSize -
      In:  the pointer to the size of outProps buffer; *outPropsSize = LZMA_PROPS_SIZE = 5.
      Out: the pointer to the size of written properties in outProps buffer; *outPropsSize = LZMA_PROPS_SIZE = 5.

    LZMA Encoder will use defult values for any parameter, if it is
    -1  for any from: level, loc, lp, pb, fb, numThreads
    0  for dictSize

  level - compression level: 0 <= level <= 9;

    level dictSize algo  fb
      0:    16 KB   0    32
      1:    64 KB   0    32
      2:   256 KB   0    32
      3:     1 MB   0    32
      4:     4 MB   0    32
      5:    16 MB   1    32
      6:    32 MB   1    32
      7+:   64 MB   1    64

    The default value for "level" is 5.

    algo = 0 means fast method
    algo = 1 means normal method

  dictSize - The dictionary size in bytes. The maximum value is
          128 MB = (1 << 27) bytes for 32-bit version
            1 GB = (1 << 30) bytes for 64-bit version
      The default value is 16 MB = (1 << 24) bytes.
      It's recommended to use the dictionary that is larger than 4 KB and
      that can be calculated as (1 << N) or (3 << N) sizes.

  lc - The number of literal context bits (high bits of previous literal).
      It can be in the range from 0 to 8. The default value is 3.
      Sometimes lc=4 gives the gain for big files.

  lp - The number of literal pos bits (low bits of current position for literals).
      It can be in the range from 0 to 4. The default value is 0.
      The lp switch is intended for periodical data when the period is equal to 2^lp.
      For example, for 32-bit (4 bytes) periodical data you can use lp=2. Often it's
      better to set lc=0, if you change lp switch.

  pb - The number of pos bits (low bits of current position).
      It can be in the range from 0 to 4. The default value is 2.
      The pb switch is intended for periodical data when the period is equal 2^pb.

  fb - Word size (the number of fast bytes).
      It can be in the range from 5 to 273. The default value is 32.
      Usually, a big number gives a little bit better compression ratio and
      slower compression process.

  numThreads - The number of thereads. 1 or 2. The default value is 2.
      Fast mode (algo = 0) can use only 1 thread.

  Out:
    destLen  - processed output size
  Returns:
    SZ_OK               - OK
    SZ_ERROR_MEM        - Memory allocation error
    SZ_ERROR_PARAM      - Incorrect paramater
    SZ_ERROR_OUTPUT_EOF - output buffer overflow
    SZ_ERROR_THREAD     - errors in multithreading functions (only for Mt version)
  */

  
  /*
  LzmaUncompress
  --------------
  In:
    dest     - output data
    destLen  - output data size
    src      - input data
    srcLen   - input data size
  Out:
    destLen  - processed output size
    srcLen   - processed input size
  Returns:
    SZ_OK                - OK
    SZ_ERROR_DATA        - Data error
    SZ_ERROR_MEM         - Memory allocation arror
    SZ_ERROR_UNSUPPORTED - Unsupported properties
    SZ_ERROR_INPUT_EOF   - it needs more bytes in input buffer (src)
  */

  

  #endif

  /* NameMangle.h -- Name mangling to avoid linking conflicts
  2009-04-15 : Marcus Geelnard : Public domain */

  #ifndef __7Z_NAMEMANGLE_H
  #define __7Z_NAMEMANGLE_H

  #ifdef LZMA_PREFIX_CTM

  /* Alloc.c */
  #define MyAlloc _ctm_MyAlloc
  #define MyFree _ctm_MyFree
  #ifdef _WIN32
  #define MidAlloc _ctm_MidAlloc
  #define MidFree _ctm_MidFree
  #define SetLargePageSize _ctm_SetLargePageSize
  #define BigAlloc _ctm_BigAlloc
  #define BigFree _ctm_BigFree
  #endif /* _WIN32 */

  /* LzFind.c */
  #define MatchFinder_GetPointerToCurrentPos _ctm_MatchFinder_GetPointerToCurrentPos
  #define MatchFinder_GetIndexByte _ctm_MatchFinder_GetIndexByte
  #define MatchFinder_GetNumAvailableBytes _ctm_MatchFinder_GetNumAvailableBytes
  #define MatchFinder_ReduceOffsets _ctm_MatchFinder_ReduceOffsets
  #define MatchFinder_MoveBlock _ctm_MatchFinder_MoveBlock
  #define MatchFinder_NeedMove _ctm_MatchFinder_NeedMove
  #define MatchFinder_ReadIfRequired _ctm_MatchFinder_ReadIfRequired
  #define MatchFinder_Construct _ctm_MatchFinder_Construct
  #define MatchFinder_Free _ctm_MatchFinder_Free
  #define MatchFinder_Create _ctm_MatchFinder_Create
  #define MatchFinder_Init _ctm_MatchFinder_Init
  #define MatchFinder_Normalize3 _ctm_MatchFinder_Normalize3
  #define GetMatchesSpec1 _ctm_GetMatchesSpec1
  #define Bt3Zip_MatchFinder_GetMatches _ctm_Bt3Zip_MatchFinder_GetMatches
  #define Hc3Zip_MatchFinder_GetMatches _ctm_Hc3Zip_MatchFinder_GetMatches
  #define Bt3Zip_MatchFinder_Skip _ctm_Bt3Zip_MatchFinder_Skip
  #define Hc3Zip_MatchFinder_Skip _ctm_Hc3Zip_MatchFinder_Skip
  #define MatchFinder_CreateVTable _ctm_MatchFinder_CreateVTable

  /* LzmaDec.c */
  #define LzmaDec_InitDicAndState _ctm_LzmaDec_InitDicAndState
  #define LzmaDec_Init _ctm_LzmaDec_Init
  #define LzmaDec_DecodeToDic _ctm_LzmaDec_DecodeToDic
  #define LzmaDec_DecodeToBuf _ctm_LzmaDec_DecodeToBuf
  #define LzmaDec_FreeProbs _ctm_LzmaDec_FreeProbs
  #define LzmaDec_Free _ctm_LzmaDec_Free
  #define LzmaProps_Decode _ctm_LzmaProps_Decode
  #define LzmaDec_AllocateProbs _ctm_LzmaDec_AllocateProbs
  #define LzmaDec_Allocate _ctm_LzmaDec_Allocate
  #define LzmaDecode _ctm_LzmaDecode

  /* LzmaEnc.c */
  #define LzmaEncProps_Init _ctm_LzmaEncProps_Init
  #define LzmaEncProps_Normalize _ctm_LzmaEncProps_Normalize
  #define LzmaEncProps_GetDictSize _ctm_LzmaEncProps_GetDictSize
  #define LzmaEnc_FastPosInit _ctm_LzmaEnc_FastPosInit
  #define LzmaEnc_SaveState _ctm_LzmaEnc_SaveState
  #define LzmaEnc_RestoreState _ctm_LzmaEnc_RestoreState
  #define LzmaEnc_SetProps _ctm_LzmaEnc_SetProps
  #define LzmaEnc_InitPriceTables _ctm_LzmaEnc_InitPriceTables
  #define LzmaEnc_Construct _ctm_LzmaEnc_Construct
  #define LzmaEnc_Create _ctm_LzmaEnc_Create
  #define LzmaEnc_FreeLits _ctm_LzmaEnc_FreeLits
  #define LzmaEnc_Destruct _ctm_LzmaEnc_Destruct
  #define LzmaEnc_Destroy _ctm_LzmaEnc_Destroy
  #define LzmaEnc_Init _ctm_LzmaEnc_Init
  #define LzmaEnc_InitPrices _ctm_LzmaEnc_InitPrices
  #define LzmaEnc_PrepareForLzma2 _ctm_LzmaEnc_PrepareForLzma2
  #define LzmaEnc_MemPrepare _ctm_LzmaEnc_MemPrepare
  #define LzmaEnc_Finish _ctm_LzmaEnc_Finish
  #define LzmaEnc_GetNumAvailableBytes _ctm_LzmaEnc_GetNumAvailableBytes
  #define LzmaEnc_GetCurBuf _ctm_LzmaEnc_GetCurBuf
  #define LzmaEnc_CodeOneMemBlock _ctm_LzmaEnc_CodeOneMemBlock
  #define LzmaEnc_Encode _ctm_LzmaEnc_Encode
  #define LzmaEnc_WriteProperties _ctm_LzmaEnc_WriteProperties
  #define LzmaEnc_MemEncode _ctm_LzmaEnc_MemEncode

  /* LzmaLib.c */
  #define LzmaCompress _ctm_LzmaCompress
  #define LzmaUncompress _ctm_LzmaUncompress

  #endif /* LZMA_PREFIX_CTM */

  #endif /* __7Z_NAMEMANGLE_H */

  /* Alloc.c -- Memory allocation functions
  2008-09-24
  Igor Pavlov
  Public domain */

  void *MyAlloc(size_t size)
  {
    if (size == 0)
      return 0;
  #ifdef _SZ_ALLOC_DEBUG
    {
      void *p = malloc(size);
      fprintf(stderr, "\nAlloc %10d bytes, count = %10d,  addr = %8X", size, g_allocCount++, (unsigned)p);
      return p;
    }
  #else
    return malloc(size);
  #endif
  }

  void MyFree(void *address)
  {
  #ifdef _SZ_ALLOC_DEBUG
    if (address != 0)
      fprintf(stderr, "\nFree; count = %10d,  addr = %8X", --g_allocCount, (unsigned)address);
  #endif
    free(address);
  }

  #ifdef _WIN32

  void *MidAlloc(size_t size)
  {
    if (size == 0)
      return 0;
  #ifdef _SZ_ALLOC_DEBUG
    fprintf(stderr, "\nAlloc_Mid %10d bytes;  count = %10d", size, g_allocCountMid++);
  #endif
    return VirtualAlloc(0, size, MEM_COMMIT, PAGE_READWRITE);
  }

  void MidFree(void *address)
  {
  #ifdef _SZ_ALLOC_DEBUG
    if (address != 0)
      fprintf(stderr, "\nFree_Mid; count = %10d", --g_allocCountMid);
  #endif
    if (address == 0)
      return;
    VirtualFree(address, 0, MEM_RELEASE);
  }

  #ifndef MEM_LARGE_PAGES
  #undef _7ZIP_LARGE_PAGES
  #endif

  #ifdef _7ZIP_LARGE_PAGES
  SIZE_T g_LargePageSize = 0;
  typedef SIZE_T(WINAPI *GetLargePageMinimumP)();
  #endif

  void SetLargePageSize()
  {
  #ifdef _7ZIP_LARGE_PAGES
    SIZE_T size = 0;
    GetLargePageMinimumP largePageMinimum = (GetLargePageMinimumP)
        GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")), "GetLargePageMinimum");
    if (largePageMinimum == 0)
      return;
    size = largePageMinimum();
    if (size == 0 || (size & (size - 1)) != 0)
      return;
    g_LargePageSize = size;
  #endif
  }

  void *BigAlloc(size_t size)
  {
    if (size == 0)
      return 0;
  #ifdef _SZ_ALLOC_DEBUG
    fprintf(stderr, "\nAlloc_Big %10d bytes;  count = %10d", size, g_allocCountBig++);
  #endif

  #ifdef _7ZIP_LARGE_PAGES
    if (g_LargePageSize != 0 && g_LargePageSize <= (1 << 30) && size >= (1 << 18))
    {
      void *res = VirtualAlloc(0, (size + g_LargePageSize - 1) & (~(g_LargePageSize - 1)),
                              MEM_COMMIT | MEM_LARGE_PAGES, PAGE_READWRITE);
      if (res != 0)
        return res;
    }
  #endif
    return VirtualAlloc(0, size, MEM_COMMIT, PAGE_READWRITE);
  }

  void BigFree(void *address)
  {
  #ifdef _SZ_ALLOC_DEBUG
    if (address != 0)
      fprintf(stderr, "\nFree_Big; count = %10d", --g_allocCountBig);
  #endif

    if (address == 0)
      return;
    VirtualFree(address, 0, MEM_RELEASE);
  }

  #endif

  /* LzFind.c -- Match finder for LZ algorithms
  2008-10-04 : Igor Pavlov : Public domain */

  #define kEmptyHashValue 0
  #define kMaxValForNormalize ((UInt32)0xFFFFFFFF)
  #define kNormalizeStepMin (1 << 10) /* it must be power of 2 */
  #define kNormalizeMask (~(kNormalizeStepMin - 1))
  #define kMaxHistorySize ((UInt32)3 << 30)

  #define kStartMaxLen 3

  static void LzInWindow_Free(CMatchFinder *p, ISzAlloc *alloc)
  {
    if (!p->directInput)
    {
      alloc->Free(alloc, p->bufferBase);
      p->bufferBase = 0;
    }
  }

  /* keepSizeBefore + keepSizeAfter + keepSizeReserv must be < 4G) */

  static int LzInWindow_Create(CMatchFinder *p, UInt32 keepSizeReserv, ISzAlloc *alloc)
  {
    UInt32 blockSize = p->keepSizeBefore + p->keepSizeAfter + keepSizeReserv;
    if (p->directInput)
    {
      p->blockSize = blockSize;
      return 1;
    }
    if (p->bufferBase == 0 || p->blockSize != blockSize)
    {
      LzInWindow_Free(p, alloc);
      p->blockSize = blockSize;
      p->bufferBase = (Byte *)alloc->Alloc(alloc, (size_t)blockSize);
    }
    return (p->bufferBase != 0);
  }

  Byte *MatchFinder_GetPointerToCurrentPos(CMatchFinder *p) { return p->buffer; }
  Byte MatchFinder_GetIndexByte(CMatchFinder *p, Int32 index) { return p->buffer[index]; }

  UInt32 MatchFinder_GetNumAvailableBytes(CMatchFinder *p) { return p->streamPos - p->pos; }

  void MatchFinder_ReduceOffsets(CMatchFinder *p, UInt32 subValue)
  {
    p->posLimit -= subValue;
    p->pos -= subValue;
    p->streamPos -= subValue;
  }

  static void MatchFinder_ReadBlock(CMatchFinder *p)
  {
    if (p->streamEndWasReached || p->result != SZ_OK)
      return;
    for (;;)
    {
      Byte *dest = p->buffer + (p->streamPos - p->pos);
      size_t size = (p->bufferBase + p->blockSize - dest);
      if (size == 0)
        return;
      p->result = p->stream->Read(p->stream, dest, &size);
      if (p->result != SZ_OK)
        return;
      if (size == 0)
      {
        p->streamEndWasReached = 1;
        return;
      }
      p->streamPos += (UInt32)size;
      if (p->streamPos - p->pos > p->keepSizeAfter)
        return;
    }
  }

  void MatchFinder_MoveBlock(CMatchFinder *p)
  {
    memmove(p->bufferBase,
            p->buffer - p->keepSizeBefore,
            (size_t)(p->streamPos - p->pos + p->keepSizeBefore));
    p->buffer = p->bufferBase + p->keepSizeBefore;
  }

  int MatchFinder_NeedMove(CMatchFinder *p)
  {
    /* if (p->streamEndWasReached) return 0; */
    return ((size_t)(p->bufferBase + p->blockSize - p->buffer) <= p->keepSizeAfter);
  }

  void MatchFinder_ReadIfRequired(CMatchFinder *p)
  {
    if (p->streamEndWasReached)
      return;
    if (p->keepSizeAfter >= p->streamPos - p->pos)
      MatchFinder_ReadBlock(p);
  }

  static void MatchFinder_CheckAndMoveAndRead(CMatchFinder *p)
  {
    if (MatchFinder_NeedMove(p))
      MatchFinder_MoveBlock(p);
    MatchFinder_ReadBlock(p);
  }

  static void MatchFinder_SetDefaultSettings(CMatchFinder *p)
  {
    p->cutValue = 32;
    p->btMode = 1;
    p->numHashBytes = 4;
    /* p->skipModeBits = 0; */
    p->directInput = 0;
    p->bigHash = 0;
  }

  #define kCrcPoly 0xEDB88320

  void MatchFinder_Construct(CMatchFinder *p)
  {
    UInt32 i;
    p->bufferBase = 0;
    p->directInput = 0;
    p->hash = 0;
    MatchFinder_SetDefaultSettings(p);

    for (i = 0; i < 256; i++)
    {
      UInt32 r = i;
      int j;
      for (j = 0; j < 8; j++)
        r = (r >> 1) ^ (kCrcPoly & ~((r & 1) - 1));
      p->crc[i] = r;
    }
  }

  static void MatchFinder_FreeThisClassMemory(CMatchFinder *p, ISzAlloc *alloc)
  {
    alloc->Free(alloc, p->hash);
    p->hash = 0;
  }

  void MatchFinder_Free(CMatchFinder *p, ISzAlloc *alloc)
  {
    MatchFinder_FreeThisClassMemory(p, alloc);
    LzInWindow_Free(p, alloc);
  }

  static CLzRef *AllocRefs(UInt32 num, ISzAlloc *alloc)
  {
    size_t sizeInBytes = (size_t)num * sizeof(CLzRef);
    if (sizeInBytes / sizeof(CLzRef) != num)
      return 0;
    return (CLzRef *)alloc->Alloc(alloc, sizeInBytes);
  }

  int MatchFinder_Create(CMatchFinder *p, UInt32 historySize,
                        UInt32 keepAddBufferBefore, UInt32 matchMaxLen, UInt32 keepAddBufferAfter,
                        ISzAlloc *alloc)
  {
    UInt32 sizeReserv;
    if (historySize > kMaxHistorySize)
    {
      MatchFinder_Free(p, alloc);
      return 0;
    }
    sizeReserv = historySize >> 1;
    if (historySize > ((UInt32)2 << 30))
      sizeReserv = historySize >> 2;
    sizeReserv += (keepAddBufferBefore + matchMaxLen + keepAddBufferAfter) / 2 + (1 << 19);

    p->keepSizeBefore = historySize + keepAddBufferBefore + 1;
    p->keepSizeAfter = matchMaxLen + keepAddBufferAfter;
    /* we need one additional byte, since we use MoveBlock after pos++ and before dictionary using */
    if (LzInWindow_Create(p, sizeReserv, alloc))
    {
      UInt32 newCyclicBufferSize = (historySize /* >> p->skipModeBits */) + 1;
      UInt32 hs;
      p->matchMaxLen = matchMaxLen;
      {
        p->fixedHashSize = 0;
        if (p->numHashBytes == 2)
          hs = (1 << 16) - 1;
        else
        {
          hs = historySize - 1;
          hs |= (hs >> 1);
          hs |= (hs >> 2);
          hs |= (hs >> 4);
          hs |= (hs >> 8);
          hs >>= 1;
          /* hs >>= p->skipModeBits; */
          hs |= 0xFFFF; /* don't change it! It's required for Deflate */
          if (hs > (1 << 24))
          {
            if (p->numHashBytes == 3)
              hs = (1 << 24) - 1;
            else
              hs >>= 1;
          }
        }
        p->hashMask = hs;
        hs++;
        if (p->numHashBytes > 2)
          p->fixedHashSize += kHash2Size;
        if (p->numHashBytes > 3)
          p->fixedHashSize += kHash3Size;
        if (p->numHashBytes > 4)
          p->fixedHashSize += kHash4Size;
        hs += p->fixedHashSize;
      }

      {
        UInt32 prevSize = p->hashSizeSum + p->numSons;
        UInt32 newSize;
        p->historySize = historySize;
        p->hashSizeSum = hs;
        p->cyclicBufferSize = newCyclicBufferSize;
        p->numSons = (p->btMode ? newCyclicBufferSize * 2 : newCyclicBufferSize);
        newSize = p->hashSizeSum + p->numSons;
        if (p->hash != 0 && prevSize == newSize)
          return 1;
        MatchFinder_FreeThisClassMemory(p, alloc);
        p->hash = AllocRefs(newSize, alloc);
        if (p->hash != 0)
        {
          p->son = p->hash + p->hashSizeSum;
          return 1;
        }
      }
    }
    MatchFinder_Free(p, alloc);
    return 0;
  }

  static void MatchFinder_SetLimits(CMatchFinder *p)
  {
    UInt32 limit = kMaxValForNormalize - p->pos;
    UInt32 limit2 = p->cyclicBufferSize - p->cyclicBufferPos;
    if (limit2 < limit)
      limit = limit2;
    limit2 = p->streamPos - p->pos;
    if (limit2 <= p->keepSizeAfter)
    {
      if (limit2 > 0)
        limit2 = 1;
    }
    else
      limit2 -= p->keepSizeAfter;
    if (limit2 < limit)
      limit = limit2;
    {
      UInt32 lenLimit = p->streamPos - p->pos;
      if (lenLimit > p->matchMaxLen)
        lenLimit = p->matchMaxLen;
      p->lenLimit = lenLimit;
    }
    p->posLimit = p->pos + limit;
  }

  void MatchFinder_Init(CMatchFinder *p)
  {
    UInt32 i;
    for (i = 0; i < p->hashSizeSum; i++)
      p->hash[i] = kEmptyHashValue;
    p->cyclicBufferPos = 0;
    p->buffer = p->bufferBase;
    p->pos = p->streamPos = p->cyclicBufferSize;
    p->result = SZ_OK;
    p->streamEndWasReached = 0;
    MatchFinder_ReadBlock(p);
    MatchFinder_SetLimits(p);
  }

  static UInt32 MatchFinder_GetSubValue(CMatchFinder *p)
  {
    return (p->pos - p->historySize - 1) & kNormalizeMask;
  }

  void MatchFinder_Normalize3(UInt32 subValue, CLzRef *items, UInt32 numItems)
  {
    UInt32 i;
    for (i = 0; i < numItems; i++)
    {
      UInt32 value = items[i];
      if (value <= subValue)
        value = kEmptyHashValue;
      else
        value -= subValue;
      items[i] = value;
    }
  }

  static void MatchFinder_Normalize(CMatchFinder *p)
  {
    UInt32 subValue = MatchFinder_GetSubValue(p);
    MatchFinder_Normalize3(subValue, p->hash, p->hashSizeSum + p->numSons);
    MatchFinder_ReduceOffsets(p, subValue);
  }

  static void MatchFinder_CheckLimits(CMatchFinder *p)
  {
    if (p->pos == kMaxValForNormalize)
      MatchFinder_Normalize(p);
    if (!p->streamEndWasReached && p->keepSizeAfter == p->streamPos - p->pos)
      MatchFinder_CheckAndMoveAndRead(p);
    if (p->cyclicBufferPos == p->cyclicBufferSize)
      p->cyclicBufferPos = 0;
    MatchFinder_SetLimits(p);
  }

  static UInt32 *Hc_GetMatchesSpec(UInt32 lenLimit, UInt32 curMatch, UInt32 pos, const Byte *cur, CLzRef *son,
                                  UInt32 _cyclicBufferPos, UInt32 _cyclicBufferSize, UInt32 cutValue,
                                  UInt32 *distances, UInt32 maxLen)
  {
    son[_cyclicBufferPos] = curMatch;
    for (;;)
    {
      UInt32 delta = pos - curMatch;
      if (cutValue-- == 0 || delta >= _cyclicBufferSize)
        return distances;
      {
        const Byte *pb = cur - delta;
        curMatch = son[_cyclicBufferPos - delta + ((delta > _cyclicBufferPos) ? _cyclicBufferSize : 0)];
        if (pb[maxLen] == cur[maxLen] && *pb == *cur)
        {
          UInt32 len = 0;
          while (++len != lenLimit)
            if (pb[len] != cur[len])
              break;
          if (maxLen < len)
          {
            *distances++ = maxLen = len;
            *distances++ = delta - 1;
            if (len == lenLimit)
              return distances;
          }
        }
      }
    }
  }

  UInt32 *GetMatchesSpec1(UInt32 lenLimit, UInt32 curMatch, UInt32 pos, const Byte *cur, CLzRef *son,
                          UInt32 _cyclicBufferPos, UInt32 _cyclicBufferSize, UInt32 cutValue,
                          UInt32 *distances, UInt32 maxLen)
  {
    CLzRef *ptr0 = son + (_cyclicBufferPos << 1) + 1;
    CLzRef *ptr1 = son + (_cyclicBufferPos << 1);
    UInt32 len0 = 0, len1 = 0;
    for (;;)
    {
      UInt32 delta = pos - curMatch;
      if (cutValue-- == 0 || delta >= _cyclicBufferSize)
      {
        *ptr0 = *ptr1 = kEmptyHashValue;
        return distances;
      }
      {
        CLzRef *pair = son + ((_cyclicBufferPos - delta + ((delta > _cyclicBufferPos) ? _cyclicBufferSize : 0)) << 1);
        const Byte *pb = cur - delta;
        UInt32 len = (len0 < len1 ? len0 : len1);
        if (pb[len] == cur[len])
        {
          if (++len != lenLimit && pb[len] == cur[len])
            while (++len != lenLimit)
              if (pb[len] != cur[len])
                break;
          if (maxLen < len)
          {
            *distances++ = maxLen = len;
            *distances++ = delta - 1;
            if (len == lenLimit)
            {
              *ptr1 = pair[0];
              *ptr0 = pair[1];
              return distances;
            }
          }
        }
        if (pb[len] < cur[len])
        {
          *ptr1 = curMatch;
          ptr1 = pair + 1;
          curMatch = *ptr1;
          len1 = len;
        }
        else
        {
          *ptr0 = curMatch;
          ptr0 = pair;
          curMatch = *ptr0;
          len0 = len;
        }
      }
    }
  }

  static void SkipMatchesSpec(UInt32 lenLimit, UInt32 curMatch, UInt32 pos, const Byte *cur, CLzRef *son,
                              UInt32 _cyclicBufferPos, UInt32 _cyclicBufferSize, UInt32 cutValue)
  {
    CLzRef *ptr0 = son + (_cyclicBufferPos << 1) + 1;
    CLzRef *ptr1 = son + (_cyclicBufferPos << 1);
    UInt32 len0 = 0, len1 = 0;
    for (;;)
    {
      UInt32 delta = pos - curMatch;
      if (cutValue-- == 0 || delta >= _cyclicBufferSize)
      {
        *ptr0 = *ptr1 = kEmptyHashValue;
        return;
      }
      {
        CLzRef *pair = son + ((_cyclicBufferPos - delta + ((delta > _cyclicBufferPos) ? _cyclicBufferSize : 0)) << 1);
        const Byte *pb = cur - delta;
        UInt32 len = (len0 < len1 ? len0 : len1);
        if (pb[len] == cur[len])
        {
          while (++len != lenLimit)
            if (pb[len] != cur[len])
              break;
          {
            if (len == lenLimit)
            {
              *ptr1 = pair[0];
              *ptr0 = pair[1];
              return;
            }
          }
        }
        if (pb[len] < cur[len])
        {
          *ptr1 = curMatch;
          ptr1 = pair + 1;
          curMatch = *ptr1;
          len1 = len;
        }
        else
        {
          *ptr0 = curMatch;
          ptr0 = pair;
          curMatch = *ptr0;
          len0 = len;
        }
      }
    }
  }

  #define MOVE_POS               \
    ++p->cyclicBufferPos;        \
    p->buffer++;                 \
    if (++p->pos == p->posLimit) \
      MatchFinder_CheckLimits(p);

  #define MOVE_POS_RET MOVE_POS return offset;

  static void MatchFinder_MovePos(CMatchFinder *p)
  {
    MOVE_POS;
  }

  #define GET_MATCHES_HEADER2(minLen, ret_op) \
    UInt32 lenLimit;                          \
    UInt32 hashValue;                         \
    const Byte *cur;                          \
    UInt32 curMatch;                          \
    lenLimit = p->lenLimit;                   \
    {                                         \
      if (lenLimit < minLen)                  \
      {                                       \
        MatchFinder_MovePos(p);               \
        ret_op;                               \
      }                                       \
    }                                         \
    cur = p->buffer;

  #define GET_MATCHES_HEADER(minLen) GET_MATCHES_HEADER2(minLen, return 0)
  #define SKIP_HEADER(minLen) GET_MATCHES_HEADER2(minLen, continue)

  #define MF_PARAMS(p) p->pos, p->buffer, p->son, p->cyclicBufferPos, p->cyclicBufferSize, p->cutValue

  #define GET_MATCHES_FOOTER(offset, maxLen)                            \
    offset = (UInt32)(GetMatchesSpec1(lenLimit, curMatch, MF_PARAMS(p), \
                                      distances + offset, maxLen) -     \
                      distances);                                       \
    MOVE_POS_RET;

  #define SKIP_FOOTER                                  \
    SkipMatchesSpec(lenLimit, curMatch, MF_PARAMS(p)); \
    MOVE_POS;

  static UInt32 Bt2_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
  {
    UInt32 offset;
    GET_MATCHES_HEADER(2)
    HASH2_CALC;
    curMatch = p->hash[hashValue];
    p->hash[hashValue] = p->pos;
    offset = 0;
    GET_MATCHES_FOOTER(offset, 1)
  }

  UInt32 Bt3Zip_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
  {
    UInt32 offset;
    GET_MATCHES_HEADER(3)
    HASH_ZIP_CALC;
    curMatch = p->hash[hashValue];
    p->hash[hashValue] = p->pos;
    offset = 0;
    GET_MATCHES_FOOTER(offset, 2)
  }

  static UInt32 Bt3_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
  {
    UInt32 hash2Value, delta2, maxLen, offset;
    GET_MATCHES_HEADER(3)

    HASH3_CALC;

    delta2 = p->pos - p->hash[hash2Value];
    curMatch = p->hash[kFix3HashSize + hashValue];

    p->hash[hash2Value] =
        p->hash[kFix3HashSize + hashValue] = p->pos;

    maxLen = 2;
    offset = 0;
    if (delta2 < p->cyclicBufferSize && *(cur - delta2) == *cur)
    {
      for (; maxLen != lenLimit; maxLen++)
        if (cur[(ptrdiff_t)maxLen - delta2] != cur[maxLen])
          break;
      distances[0] = maxLen;
      distances[1] = delta2 - 1;
      offset = 2;
      if (maxLen == lenLimit)
      {
        SkipMatchesSpec(lenLimit, curMatch, MF_PARAMS(p));
        MOVE_POS_RET;
      }
    }
    GET_MATCHES_FOOTER(offset, maxLen)
  }

  static UInt32 Bt4_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
  {
    UInt32 hash2Value, hash3Value, delta2, delta3, maxLen, offset;
    GET_MATCHES_HEADER(4)

    HASH4_CALC;

    delta2 = p->pos - p->hash[hash2Value];
    delta3 = p->pos - p->hash[kFix3HashSize + hash3Value];
    curMatch = p->hash[kFix4HashSize + hashValue];

    p->hash[hash2Value] =
        p->hash[kFix3HashSize + hash3Value] =
            p->hash[kFix4HashSize + hashValue] = p->pos;

    maxLen = 1;
    offset = 0;
    if (delta2 < p->cyclicBufferSize && *(cur - delta2) == *cur)
    {
      distances[0] = maxLen = 2;
      distances[1] = delta2 - 1;
      offset = 2;
    }
    if (delta2 != delta3 && delta3 < p->cyclicBufferSize && *(cur - delta3) == *cur)
    {
      maxLen = 3;
      distances[offset + 1] = delta3 - 1;
      offset += 2;
      delta2 = delta3;
    }
    if (offset != 0)
    {
      for (; maxLen != lenLimit; maxLen++)
        if (cur[(ptrdiff_t)maxLen - delta2] != cur[maxLen])
          break;
      distances[offset - 2] = maxLen;
      if (maxLen == lenLimit)
      {
        SkipMatchesSpec(lenLimit, curMatch, MF_PARAMS(p));
        MOVE_POS_RET;
      }
    }
    if (maxLen < 3)
      maxLen = 3;
    GET_MATCHES_FOOTER(offset, maxLen)
  }

  static UInt32 Hc4_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
  {
    UInt32 hash2Value, hash3Value, delta2, delta3, maxLen, offset;
    GET_MATCHES_HEADER(4)

    HASH4_CALC;

    delta2 = p->pos - p->hash[hash2Value];
    delta3 = p->pos - p->hash[kFix3HashSize + hash3Value];
    curMatch = p->hash[kFix4HashSize + hashValue];

    p->hash[hash2Value] =
        p->hash[kFix3HashSize + hash3Value] =
            p->hash[kFix4HashSize + hashValue] = p->pos;

    maxLen = 1;
    offset = 0;
    if (delta2 < p->cyclicBufferSize && *(cur - delta2) == *cur)
    {
      distances[0] = maxLen = 2;
      distances[1] = delta2 - 1;
      offset = 2;
    }
    if (delta2 != delta3 && delta3 < p->cyclicBufferSize && *(cur - delta3) == *cur)
    {
      maxLen = 3;
      distances[offset + 1] = delta3 - 1;
      offset += 2;
      delta2 = delta3;
    }
    if (offset != 0)
    {
      for (; maxLen != lenLimit; maxLen++)
        if (cur[(ptrdiff_t)maxLen - delta2] != cur[maxLen])
          break;
      distances[offset - 2] = maxLen;
      if (maxLen == lenLimit)
      {
        p->son[p->cyclicBufferPos] = curMatch;
        MOVE_POS_RET;
      }
    }
    if (maxLen < 3)
      maxLen = 3;
    offset = (UInt32)(Hc_GetMatchesSpec(lenLimit, curMatch, MF_PARAMS(p),
                                        distances + offset, maxLen) -
                      (distances));
    MOVE_POS_RET
  }

  UInt32 Hc3Zip_MatchFinder_GetMatches(CMatchFinder *p, UInt32 *distances)
  {
    UInt32 offset;
    GET_MATCHES_HEADER(3)
    HASH_ZIP_CALC;
    curMatch = p->hash[hashValue];
    p->hash[hashValue] = p->pos;
    offset = (UInt32)(Hc_GetMatchesSpec(lenLimit, curMatch, MF_PARAMS(p),
                                        distances, 2) -
                      (distances));
    MOVE_POS_RET
  }

  static void Bt2_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
  {
    do
    {
      SKIP_HEADER(2)
      HASH2_CALC;
      curMatch = p->hash[hashValue];
      p->hash[hashValue] = p->pos;
      SKIP_FOOTER
    } while (--num != 0);
  }

  void Bt3Zip_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
  {
    do
    {
      SKIP_HEADER(3)
      HASH_ZIP_CALC;
      curMatch = p->hash[hashValue];
      p->hash[hashValue] = p->pos;
      SKIP_FOOTER
    } while (--num != 0);
  }

  static void Bt3_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
  {
    do
    {
      UInt32 hash2Value;
      SKIP_HEADER(3)
      HASH3_CALC;
      curMatch = p->hash[kFix3HashSize + hashValue];
      p->hash[hash2Value] =
          p->hash[kFix3HashSize + hashValue] = p->pos;
      SKIP_FOOTER
    } while (--num != 0);
  }

  static void Bt4_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
  {
    do
    {
      UInt32 hash2Value, hash3Value;
      SKIP_HEADER(4)
      HASH4_CALC;
      curMatch = p->hash[kFix4HashSize + hashValue];
      p->hash[hash2Value] =
          p->hash[kFix3HashSize + hash3Value] = p->pos;
      p->hash[kFix4HashSize + hashValue] = p->pos;
      SKIP_FOOTER
    } while (--num != 0);
  }

  static void Hc4_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
  {
    do
    {
      UInt32 hash2Value, hash3Value;
      SKIP_HEADER(4)
      HASH4_CALC;
      curMatch = p->hash[kFix4HashSize + hashValue];
      p->hash[hash2Value] =
          p->hash[kFix3HashSize + hash3Value] =
              p->hash[kFix4HashSize + hashValue] = p->pos;
      p->son[p->cyclicBufferPos] = curMatch;
      MOVE_POS
    } while (--num != 0);
  }

  void Hc3Zip_MatchFinder_Skip(CMatchFinder *p, UInt32 num)
  {
    do
    {
      SKIP_HEADER(3)
      HASH_ZIP_CALC;
      curMatch = p->hash[hashValue];
      p->hash[hashValue] = p->pos;
      p->son[p->cyclicBufferPos] = curMatch;
      MOVE_POS
    } while (--num != 0);
  }

  void MatchFinder_CreateVTable(CMatchFinder *p, IMatchFinder *vTable)
  {
    vTable->Init = (Mf_Init_Func)MatchFinder_Init;
    vTable->GetIndexByte = (Mf_GetIndexByte_Func)MatchFinder_GetIndexByte;
    vTable->GetNumAvailableBytes = (Mf_GetNumAvailableBytes_Func)MatchFinder_GetNumAvailableBytes;
    vTable->GetPointerToCurrentPos = (Mf_GetPointerToCurrentPos_Func)MatchFinder_GetPointerToCurrentPos;
    if (!p->btMode)
    {
      vTable->GetMatches = (Mf_GetMatches_Func)Hc4_MatchFinder_GetMatches;
      vTable->Skip = (Mf_Skip_Func)Hc4_MatchFinder_Skip;
    }
    else if (p->numHashBytes == 2)
    {
      vTable->GetMatches = (Mf_GetMatches_Func)Bt2_MatchFinder_GetMatches;
      vTable->Skip = (Mf_Skip_Func)Bt2_MatchFinder_Skip;
    }
    else if (p->numHashBytes == 3)
    {
      vTable->GetMatches = (Mf_GetMatches_Func)Bt3_MatchFinder_GetMatches;
      vTable->Skip = (Mf_Skip_Func)Bt3_MatchFinder_Skip;
    }
    else
    {
      vTable->GetMatches = (Mf_GetMatches_Func)Bt4_MatchFinder_GetMatches;
      vTable->Skip = (Mf_Skip_Func)Bt4_MatchFinder_Skip;
    }
  }

  /* LzmaDec.c -- LZMA Decoder
  2008-11-06 : Igor Pavlov : Public domain */

  #define kNumTopBits 24
  #define kTopValue ((UInt32)1 << kNumTopBits)

  #define kNumBitModelTotalBits 11
  #define kBitModelTotal (1 << kNumBitModelTotalBits)
  #define kNumMoveBits 5

  #define RC_INIT_SIZE 5

  #define NORMALIZE                  \
    if (range < kTopValue)           \
    {                                \
      range <<= 8;                   \
      code = (code << 8) | (*buf++); \
    }

  #define IF_BIT_0(p)                               \
    ttt = *(p);                                     \
    NORMALIZE;                                      \
    bound = (range >> kNumBitModelTotalBits) * ttt; \
    if (code < bound)
  #define UPDATE_0(p) \
    range = bound;    \
    *(p) = (CLzmaProb)(ttt + ((kBitModelTotal - ttt) >> kNumMoveBits));
  #define UPDATE_1(p) \
    range -= bound;   \
    code -= bound;    \
    *(p) = (CLzmaProb)(ttt - (ttt >> kNumMoveBits));
  #define GET_BIT2(p, i, A0, A1) \
    IF_BIT_0(p)                  \
    {                            \
      UPDATE_0(p);               \
      i = (i + i);               \
      A0;                        \
    }                            \
    else                         \
    {                            \
      UPDATE_1(p);               \
      i = (i + i) + 1;           \
      A1;                        \
    }
  #define GET_BIT(p, i) GET_BIT2(p, i, ;, ;)

  #define TREE_GET_BIT(probs, i) \
    {                            \
      GET_BIT((probs + i), i);   \
    }
  #define TREE_DECODE(probs, limit, i) \
    {                                  \
      i = 1;                           \
      do                               \
      {                                \
        TREE_GET_BIT(probs, i);        \
      } while (i < limit);             \
      i -= limit;                      \
    }

  /* #define _LZMA_SIZE_OPT */

  #ifdef _LZMA_SIZE_OPT
  #define TREE_6_DECODE(probs, i) TREE_DECODE(probs, (1 << 6), i)
  #else
  #define TREE_6_DECODE(probs, i) \
    {                             \
      i = 1;                      \
      TREE_GET_BIT(probs, i);     \
      TREE_GET_BIT(probs, i);     \
      TREE_GET_BIT(probs, i);     \
      TREE_GET_BIT(probs, i);     \
      TREE_GET_BIT(probs, i);     \
      TREE_GET_BIT(probs, i);     \
      i -= 0x40;                  \
    }
  #endif

  #define NORMALIZE_CHECK            \
    if (range < kTopValue)           \
    {                                \
      if (buf >= bufLimit)           \
        return DUMMY_ERROR;          \
      range <<= 8;                   \
      code = (code << 8) | (*buf++); \
    }

  #define IF_BIT_0_CHECK(p)                         \
    ttt = *(p);                                     \
    NORMALIZE_CHECK;                                \
    bound = (range >> kNumBitModelTotalBits) * ttt; \
    if (code < bound)
  #define UPDATE_0_CHECK range = bound;
  #define UPDATE_1_CHECK \
    range -= bound;      \
    code -= bound;
  #define GET_BIT2_CHECK(p, i, A0, A1) \
    IF_BIT_0_CHECK(p)                  \
    {                                  \
      UPDATE_0_CHECK;                  \
      i = (i + i);                     \
      A0;                              \
    }                                  \
    else                               \
    {                                  \
      UPDATE_1_CHECK;                  \
      i = (i + i) + 1;                 \
      A1;                              \
    }
  #define GET_BIT_CHECK(p, i) GET_BIT2_CHECK(p, i, ;, ;)
  #define TREE_DECODE_CHECK(probs, limit, i) \
    {                                        \
      i = 1;                                 \
      do                                     \
      {                                      \
        GET_BIT_CHECK(probs + i, i)          \
      } while (i < limit);                   \
      i -= limit;                            \
    }

  #define kNumPosBitsMax 4
  #define kNumPosStatesMax (1 << kNumPosBitsMax)

  #define kLenNumLowBits 3
  #define kLenNumLowSymbols (1 << kLenNumLowBits)
  #define kLenNumMidBits 3
  #define kLenNumMidSymbols (1 << kLenNumMidBits)
  #define kLenNumHighBits 8
  #define kLenNumHighSymbols (1 << kLenNumHighBits)

  #define LenChoice 0
  #define LenChoice2 (LenChoice + 1)
  #define LenLow (LenChoice2 + 1)
  #define LenMid (LenLow + (kNumPosStatesMax << kLenNumLowBits))
  #define LenHigh (LenMid + (kNumPosStatesMax << kLenNumMidBits))
  #define kNumLenProbs (LenHigh + kLenNumHighSymbols)

  #define kNumStates 12
  #define kNumLitStates 7

  #define kStartPosModelIndex 4
  #define kEndPosModelIndex 14
  #define kNumFullDistances (1 << (kEndPosModelIndex >> 1))

  #define kNumPosSlotBits 6
  #define kNumLenToPosStates 4

  #define kNumAlignBits 4
  #define kAlignTableSize (1 << kNumAlignBits)

  #define kMatchMinLen 2
  #define kMatchSpecLenStart (kMatchMinLen + kLenNumLowSymbols + kLenNumMidSymbols + kLenNumHighSymbols)

  #define IsMatch 0
  #define IsRep (IsMatch + (kNumStates << kNumPosBitsMax))
  #define IsRepG0 (IsRep + kNumStates)
  #define IsRepG1 (IsRepG0 + kNumStates)
  #define IsRepG2 (IsRepG1 + kNumStates)
  #define IsRep0Long (IsRepG2 + kNumStates)
  #define PosSlot (IsRep0Long + (kNumStates << kNumPosBitsMax))
  #define SpecPos (PosSlot + (kNumLenToPosStates << kNumPosSlotBits))
  #define Align (SpecPos + kNumFullDistances - kEndPosModelIndex)
  #define LenCoder (Align + kAlignTableSize)
  #define RepLenCoder (LenCoder + kNumLenProbs)
  #define Literal (RepLenCoder + kNumLenProbs)

  #define LZMA_BASE_SIZE 1846
  #define LZMA_LIT_SIZE 768

  #define LzmaProps_GetNumProbs(p) ((UInt32)LZMA_BASE_SIZE + (LZMA_LIT_SIZE << ((p)->lc + (p)->lp)))

  #if Literal != LZMA_BASE_SIZE
  StopCompilingDueBUG
  #endif

      static const Byte kLiteralNextStates_dec[kNumStates * 2] =
          {
              0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 4, 5,
              7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10};

  #define LZMA_DIC_MIN (1 << 12)

  /* First LZMA-symbol is always decoded.
  And it decodes new LZMA-symbols while (buf < bufLimit), but "buf" is without last normalization
  Out:
    Result:
      SZ_OK - OK
      SZ_ERROR_DATA - Error
    p->remainLen:
      < kMatchSpecLenStart : normal remain
      = kMatchSpecLenStart : finished
      = kMatchSpecLenStart + 1 : Flush marker
      = kMatchSpecLenStart + 2 : State Init Marker
  */

  static int MY_FAST_CALL LzmaDec_DecodeReal(CLzmaDec *p, SizeT limit, const Byte *bufLimit)
  {
    CLzmaProb *probs = p->probs;

    unsigned state = p->state;
    UInt32 rep0 = p->reps[0], rep1 = p->reps[1], rep2 = p->reps[2], rep3 = p->reps[3];
    unsigned pbMask = ((unsigned)1 << (p->prop.pb)) - 1;
    unsigned lpMask = ((unsigned)1 << (p->prop.lp)) - 1;
    unsigned lc = p->prop.lc;

    Byte *dic = p->dic;
    SizeT dicBufSize = p->dicBufSize;
    SizeT dicPos = p->dicPos;

    UInt32 processedPos = p->processedPos;
    UInt32 checkDicSize = p->checkDicSize;
    unsigned len = 0;

    const Byte *buf = p->buf;
    UInt32 range = p->range;
    UInt32 code = p->code;

    do
    {
      CLzmaProb *prob;
      UInt32 bound;
      unsigned ttt;
      unsigned posState = processedPos & pbMask;

      prob = probs + IsMatch + (state << kNumPosBitsMax) + posState;
      IF_BIT_0(prob)
      {
        unsigned symbol;
        UPDATE_0(prob);
        prob = probs + Literal;
        if (checkDicSize != 0 || processedPos != 0)
          prob += (LZMA_LIT_SIZE * (((processedPos & lpMask) << lc) +
                                    (dic[(dicPos == 0 ? dicBufSize : dicPos) - 1] >> (8 - lc))));

        if (state < kNumLitStates)
        {
          symbol = 1;
          do
          {
            GET_BIT(prob + symbol, symbol)
          } while (symbol < 0x100);
        }
        else
        {
          unsigned matchByte = p->dic[(dicPos - rep0) + ((dicPos < rep0) ? dicBufSize : 0)];
          unsigned offs = 0x100;
          symbol = 1;
          do
          {
            unsigned bit;
            CLzmaProb *probLit;
            matchByte <<= 1;
            bit = (matchByte & offs);
            probLit = prob + offs + bit + symbol;
            GET_BIT2(probLit, symbol, offs &= ~bit, offs &= bit)
          } while (symbol < 0x100);
        }
        dic[dicPos++] = (Byte)symbol;
        processedPos++;

        state = kLiteralNextStates_dec[state];
        /* if (state < 4) state = 0; else if (state < 10) state -= 3; else state -= 6; */
        continue;
      }
      else
      {
        UPDATE_1(prob);
        prob = probs + IsRep + state;
        IF_BIT_0(prob)
        {
          UPDATE_0(prob);
          state += kNumStates;
          prob = probs + LenCoder;
        }
        else
        {
          UPDATE_1(prob);
          if (checkDicSize == 0 && processedPos == 0)
            return SZ_ERROR_DATA;
          prob = probs + IsRepG0 + state;
          IF_BIT_0(prob)
          {
            UPDATE_0(prob);
            prob = probs + IsRep0Long + (state << kNumPosBitsMax) + posState;
            IF_BIT_0(prob)
            {
              UPDATE_0(prob);
              dic[dicPos] = dic[(dicPos - rep0) + ((dicPos < rep0) ? dicBufSize : 0)];
              dicPos++;
              processedPos++;
              state = state < kNumLitStates ? 9 : 11;
              continue;
            }
            UPDATE_1(prob);
          }
          else
          {
            UInt32 distance;
            UPDATE_1(prob);
            prob = probs + IsRepG1 + state;
            IF_BIT_0(prob)
            {
              UPDATE_0(prob);
              distance = rep1;
            }
            else
            {
              UPDATE_1(prob);
              prob = probs + IsRepG2 + state;
              IF_BIT_0(prob)
              {
                UPDATE_0(prob);
                distance = rep2;
              }
              else
              {
                UPDATE_1(prob);
                distance = rep3;
                rep3 = rep2;
              }
              rep2 = rep1;
            }
            rep1 = rep0;
            rep0 = distance;
          }
          state = state < kNumLitStates ? 8 : 11;
          prob = probs + RepLenCoder;
        }
        {
          unsigned limit, offset;
          CLzmaProb *probLen = prob + LenChoice;
          IF_BIT_0(probLen)
          {
            UPDATE_0(probLen);
            probLen = prob + LenLow + (posState << kLenNumLowBits);
            offset = 0;
            limit = (1 << kLenNumLowBits);
          }
          else
          {
            UPDATE_1(probLen);
            probLen = prob + LenChoice2;
            IF_BIT_0(probLen)
            {
              UPDATE_0(probLen);
              probLen = prob + LenMid + (posState << kLenNumMidBits);
              offset = kLenNumLowSymbols;
              limit = (1 << kLenNumMidBits);
            }
            else
            {
              UPDATE_1(probLen);
              probLen = prob + LenHigh;
              offset = kLenNumLowSymbols + kLenNumMidSymbols;
              limit = (1 << kLenNumHighBits);
            }
          }
          TREE_DECODE(probLen, limit, len);
          len += offset;
        }

        if (state >= kNumStates)
        {
          UInt32 distance;
          prob = probs + PosSlot +
                ((len < kNumLenToPosStates ? len : kNumLenToPosStates - 1) << kNumPosSlotBits);
          TREE_6_DECODE(prob, distance);
          if (distance >= kStartPosModelIndex)
          {
            unsigned posSlot = (unsigned)distance;
            int numDirectBits = (int)(((distance >> 1) - 1));
            distance = (2 | (distance & 1));
            if (posSlot < kEndPosModelIndex)
            {
              distance <<= numDirectBits;
              prob = probs + SpecPos + distance - posSlot - 1;
              {
                UInt32 mask = 1;
                unsigned i = 1;
                do
                {
                  GET_BIT2(prob + i, i, ;, distance |= mask);
                  mask <<= 1;
                } while (--numDirectBits != 0);
              }
            }
            else
            {
              numDirectBits -= kNumAlignBits;
              do
              {
                NORMALIZE
                range >>= 1;

                {
                  UInt32 t;
                  code -= range;
                  t = (0 - ((UInt32)code >> 31)); /* (UInt32)((Int32)code >> 31) */
                  distance = (distance << 1) + (t + 1);
                  code += range & t;
                }
                /*
                distance <<= 1;
                if (code >= range)
                {
                  code -= range;
                  distance |= 1;
                }
                */
              } while (--numDirectBits != 0);
              prob = probs + Align;
              distance <<= kNumAlignBits;
              {
                unsigned i = 1;
                GET_BIT2(prob + i, i, ;, distance |= 1);
                GET_BIT2(prob + i, i, ;, distance |= 2);
                GET_BIT2(prob + i, i, ;, distance |= 4);
                GET_BIT2(prob + i, i, ;, distance |= 8);
              }
              if (distance == (UInt32)0xFFFFFFFF)
              {
                len += kMatchSpecLenStart;
                state -= kNumStates;
                break;
              }
            }
          }
          rep3 = rep2;
          rep2 = rep1;
          rep1 = rep0;
          rep0 = distance + 1;
          if (checkDicSize == 0)
          {
            if (distance >= processedPos)
              return SZ_ERROR_DATA;
          }
          else if (distance >= checkDicSize)
            return SZ_ERROR_DATA;
          state = (state < kNumStates + kNumLitStates) ? kNumLitStates : kNumLitStates + 3;
          /* state = kLiteralNextStates[state]; */
        }

        len += kMatchMinLen;

        if (limit == dicPos)
          return SZ_ERROR_DATA;
        {
          SizeT rem = limit - dicPos;
          unsigned curLen = ((rem < len) ? (unsigned)rem : len);
          SizeT pos = (dicPos - rep0) + ((dicPos < rep0) ? dicBufSize : 0);

          processedPos += curLen;

          len -= curLen;
          if (pos + curLen <= dicBufSize)
          {
            Byte *dest = dic + dicPos;
            ptrdiff_t src = (ptrdiff_t)pos - (ptrdiff_t)dicPos;
            const Byte *lim = dest + curLen;
            dicPos += curLen;
            do
              *(dest) = (Byte) * (dest + src);
            while (++dest != lim);
          }
          else
          {
            do
            {
              dic[dicPos++] = dic[pos];
              if (++pos == dicBufSize)
                pos = 0;
            } while (--curLen != 0);
          }
        }
      }
    } while (dicPos < limit && buf < bufLimit);
    NORMALIZE;
    p->buf = buf;
    p->range = range;
    p->code = code;
    p->remainLen = len;
    p->dicPos = dicPos;
    p->processedPos = processedPos;
    p->reps[0] = rep0;
    p->reps[1] = rep1;
    p->reps[2] = rep2;
    p->reps[3] = rep3;
    p->state = state;

    return SZ_OK;
  }

  static void MY_FAST_CALL LzmaDec_WriteRem(CLzmaDec *p, SizeT limit)
  {
    if (p->remainLen != 0 && p->remainLen < kMatchSpecLenStart)
    {
      Byte *dic = p->dic;
      SizeT dicPos = p->dicPos;
      SizeT dicBufSize = p->dicBufSize;
      unsigned len = p->remainLen;
      UInt32 rep0 = p->reps[0];
      if (limit - dicPos < len)
        len = (unsigned)(limit - dicPos);

      if (p->checkDicSize == 0 && p->prop.dicSize - p->processedPos <= len)
        p->checkDicSize = p->prop.dicSize;

      p->processedPos += len;
      p->remainLen -= len;
      while (len-- != 0)
      {
        dic[dicPos] = dic[(dicPos - rep0) + ((dicPos < rep0) ? dicBufSize : 0)];
        dicPos++;
      }
      p->dicPos = dicPos;
    }
  }

  static int MY_FAST_CALL LzmaDec_DecodeReal2(CLzmaDec *p, SizeT limit, const Byte *bufLimit)
  {
    do
    {
      SizeT limit2 = limit;
      if (p->checkDicSize == 0)
      {
        UInt32 rem = p->prop.dicSize - p->processedPos;
        if (limit - p->dicPos > rem)
          limit2 = p->dicPos + rem;
      }
      RINOK(LzmaDec_DecodeReal(p, limit2, bufLimit));
      if (p->processedPos >= p->prop.dicSize)
        p->checkDicSize = p->prop.dicSize;
      LzmaDec_WriteRem(p, limit);
    } while (p->dicPos < limit && p->buf < bufLimit && p->remainLen < kMatchSpecLenStart);

    if (p->remainLen > kMatchSpecLenStart)
    {
      p->remainLen = kMatchSpecLenStart;
    }
    return 0;
  }

  typedef enum
  {
    DUMMY_ERROR, /* unexpected end of input stream */
    DUMMY_LIT,
    DUMMY_MATCH,
    DUMMY_REP
  } ELzmaDummy;

  static ELzmaDummy LzmaDec_TryDummy(const CLzmaDec *p, const Byte *buf, SizeT inSize)
  {
    UInt32 range = p->range;
    UInt32 code = p->code;
    const Byte *bufLimit = buf + inSize;
    CLzmaProb *probs = p->probs;
    unsigned state = p->state;
    ELzmaDummy res;

    {
      CLzmaProb *prob;
      UInt32 bound;
      unsigned ttt;
      unsigned posState = (p->processedPos) & ((1 << p->prop.pb) - 1);

      prob = probs + IsMatch + (state << kNumPosBitsMax) + posState;
      IF_BIT_0_CHECK(prob)
      {
        UPDATE_0_CHECK

        /* if (bufLimit - buf >= 7) return DUMMY_LIT; */

        prob = probs + Literal;
        if (p->checkDicSize != 0 || p->processedPos != 0)
          prob += (LZMA_LIT_SIZE *
                  ((((p->processedPos) & ((1 << (p->prop.lp)) - 1)) << p->prop.lc) +
                    (p->dic[(p->dicPos == 0 ? p->dicBufSize : p->dicPos) - 1] >> (8 - p->prop.lc))));

        if (state < kNumLitStates)
        {
          unsigned symbol = 1;
          do
          {
            GET_BIT_CHECK(prob + symbol, symbol)
          } while (symbol < 0x100);
        }
        else
        {
          unsigned matchByte = p->dic[p->dicPos - p->reps[0] +
                                      ((p->dicPos < p->reps[0]) ? p->dicBufSize : 0)];
          unsigned offs = 0x100;
          unsigned symbol = 1;
          do
          {
            unsigned bit;
            CLzmaProb *probLit;
            matchByte <<= 1;
            bit = (matchByte & offs);
            probLit = prob + offs + bit + symbol;
            GET_BIT2_CHECK(probLit, symbol, offs &= ~bit, offs &= bit)
          } while (symbol < 0x100);
        }
        res = DUMMY_LIT;
      }
      else
      {
        unsigned len;
        UPDATE_1_CHECK;

        prob = probs + IsRep + state;
        IF_BIT_0_CHECK(prob)
        {
          UPDATE_0_CHECK;
          state = 0;
          prob = probs + LenCoder;
          res = DUMMY_MATCH;
        }
        else
        {
          UPDATE_1_CHECK;
          res = DUMMY_REP;
          prob = probs + IsRepG0 + state;
          IF_BIT_0_CHECK(prob)
          {
            UPDATE_0_CHECK;
            prob = probs + IsRep0Long + (state << kNumPosBitsMax) + posState;
            IF_BIT_0_CHECK(prob)
            {
              UPDATE_0_CHECK;
              NORMALIZE_CHECK;
              return DUMMY_REP;
            }
            else
            {
              UPDATE_1_CHECK;
            }
          }
          else
          {
            UPDATE_1_CHECK;
            prob = probs + IsRepG1 + state;
            IF_BIT_0_CHECK(prob)
            {
              UPDATE_0_CHECK;
            }
            else
            {
              UPDATE_1_CHECK;
              prob = probs + IsRepG2 + state;
              IF_BIT_0_CHECK(prob)
              {
                UPDATE_0_CHECK;
              }
              else
              {
                UPDATE_1_CHECK;
              }
            }
          }
          state = kNumStates;
          prob = probs + RepLenCoder;
        }
        {
          unsigned limit, offset;
          CLzmaProb *probLen = prob + LenChoice;
          IF_BIT_0_CHECK(probLen)
          {
            UPDATE_0_CHECK;
            probLen = prob + LenLow + (posState << kLenNumLowBits);
            offset = 0;
            limit = 1 << kLenNumLowBits;
          }
          else
          {
            UPDATE_1_CHECK;
            probLen = prob + LenChoice2;
            IF_BIT_0_CHECK(probLen)
            {
              UPDATE_0_CHECK;
              probLen = prob + LenMid + (posState << kLenNumMidBits);
              offset = kLenNumLowSymbols;
              limit = 1 << kLenNumMidBits;
            }
            else
            {
              UPDATE_1_CHECK;
              probLen = prob + LenHigh;
              offset = kLenNumLowSymbols + kLenNumMidSymbols;
              limit = 1 << kLenNumHighBits;
            }
          }
          TREE_DECODE_CHECK(probLen, limit, len);
          len += offset;
        }

        if (state < 4)
        {
          unsigned posSlot;
          prob = probs + PosSlot +
                ((len < kNumLenToPosStates ? len : kNumLenToPosStates - 1) << kNumPosSlotBits);
          TREE_DECODE_CHECK(prob, 1 << kNumPosSlotBits, posSlot);
          if (posSlot >= kStartPosModelIndex)
          {
            int numDirectBits = ((posSlot >> 1) - 1);

            /* if (bufLimit - buf >= 8) return DUMMY_MATCH; */

            if (posSlot < kEndPosModelIndex)
            {
              prob = probs + SpecPos + ((2 | (posSlot & 1)) << numDirectBits) - posSlot - 1;
            }
            else
            {
              numDirectBits -= kNumAlignBits;
              do
              {
                NORMALIZE_CHECK
                range >>= 1;
                code -= range & (((code - range) >> 31) - 1);
                /* if (code >= range) code -= range; */
              } while (--numDirectBits != 0);
              prob = probs + Align;
              numDirectBits = kNumAlignBits;
            }
            {
              unsigned i = 1;
              do
              {
                GET_BIT_CHECK(prob + i, i);
              } while (--numDirectBits != 0);
            }
          }
        }
      }
    }
    NORMALIZE_CHECK;
    return res;
  }

  static void LzmaDec_InitRc(CLzmaDec *p, const Byte *data)
  {
    p->code = ((UInt32)data[1] << 24) | ((UInt32)data[2] << 16) | ((UInt32)data[3] << 8) | ((UInt32)data[4]);
    p->range = 0xFFFFFFFF;
    p->needFlush = 0;
  }

  void LzmaDec_InitDicAndState(CLzmaDec *p, Bool initDic, Bool initState)
  {
    p->needFlush = 1;
    p->remainLen = 0;
    p->tempBufSize = 0;

    if (initDic)
    {
      p->processedPos = 0;
      p->checkDicSize = 0;
      p->needInitState = 1;
    }
    if (initState)
      p->needInitState = 1;
  }

  void LzmaDec_Init(CLzmaDec *p)
  {
    p->dicPos = 0;
    LzmaDec_InitDicAndState(p, True, True);
  }

  static void LzmaDec_InitStateReal(CLzmaDec *p)
  {
    UInt32 numProbs = Literal + ((UInt32)LZMA_LIT_SIZE << (p->prop.lc + p->prop.lp));
    UInt32 i;
    CLzmaProb *probs = p->probs;
    for (i = 0; i < numProbs; i++)
      probs[i] = kBitModelTotal >> 1;
    p->reps[0] = p->reps[1] = p->reps[2] = p->reps[3] = 1;
    p->state = 0;
    p->needInitState = 0;
  }

  SRes LzmaDec_DecodeToDic(CLzmaDec *p, SizeT dicLimit, const Byte *src, SizeT *srcLen,
                          ELzmaFinishMode finishMode, ELzmaStatus *status)
  {
    SizeT inSize = *srcLen;
    (*srcLen) = 0;
    LzmaDec_WriteRem(p, dicLimit);

    *status = LZMA_STATUS_NOT_SPECIFIED;

    while (p->remainLen != kMatchSpecLenStart)
    {
      int checkEndMarkNow;

      if (p->needFlush != 0)
      {
        for (; inSize > 0 && p->tempBufSize < RC_INIT_SIZE; (*srcLen)++, inSize--)
          p->tempBuf[p->tempBufSize++] = *src++;
        if (p->tempBufSize < RC_INIT_SIZE)
        {
          *status = LZMA_STATUS_NEEDS_MORE_INPUT;
          return SZ_OK;
        }
        if (p->tempBuf[0] != 0)
          return SZ_ERROR_DATA;

        LzmaDec_InitRc(p, p->tempBuf);
        p->tempBufSize = 0;
      }

      checkEndMarkNow = 0;
      if (p->dicPos >= dicLimit)
      {
        if (p->remainLen == 0 && p->code == 0)
        {
          *status = LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK;
          return SZ_OK;
        }
        if (finishMode == LZMA_FINISH_ANY)
        {
          *status = LZMA_STATUS_NOT_FINISHED;
          return SZ_OK;
        }
        if (p->remainLen != 0)
        {
          *status = LZMA_STATUS_NOT_FINISHED;
          return SZ_ERROR_DATA;
        }
        checkEndMarkNow = 1;
      }

      if (p->needInitState)
        LzmaDec_InitStateReal(p);

      if (p->tempBufSize == 0)
      {
        SizeT processed;
        const Byte *bufLimit;
        if (inSize < LZMA_REQUIRED_INPUT_MAX || checkEndMarkNow)
        {
          int dummyRes = LzmaDec_TryDummy(p, src, inSize);
          if (dummyRes == DUMMY_ERROR)
          {
            memcpy(p->tempBuf, src, inSize);
            p->tempBufSize = (unsigned)inSize;
            (*srcLen) += inSize;
            *status = LZMA_STATUS_NEEDS_MORE_INPUT;
            return SZ_OK;
          }
          if (checkEndMarkNow && dummyRes != DUMMY_MATCH)
          {
            *status = LZMA_STATUS_NOT_FINISHED;
            return SZ_ERROR_DATA;
          }
          bufLimit = src;
        }
        else
          bufLimit = src + inSize - LZMA_REQUIRED_INPUT_MAX;
        p->buf = src;
        if (LzmaDec_DecodeReal2(p, dicLimit, bufLimit) != 0)
          return SZ_ERROR_DATA;
        processed = (SizeT)(p->buf - src);
        (*srcLen) += processed;
        src += processed;
        inSize -= processed;
      }
      else
      {
        unsigned rem = p->tempBufSize, lookAhead = 0;
        while (rem < LZMA_REQUIRED_INPUT_MAX && lookAhead < inSize)
          p->tempBuf[rem++] = src[lookAhead++];
        p->tempBufSize = rem;
        if (rem < LZMA_REQUIRED_INPUT_MAX || checkEndMarkNow)
        {
          int dummyRes = LzmaDec_TryDummy(p, p->tempBuf, rem);
          if (dummyRes == DUMMY_ERROR)
          {
            (*srcLen) += lookAhead;
            *status = LZMA_STATUS_NEEDS_MORE_INPUT;
            return SZ_OK;
          }
          if (checkEndMarkNow && dummyRes != DUMMY_MATCH)
          {
            *status = LZMA_STATUS_NOT_FINISHED;
            return SZ_ERROR_DATA;
          }
        }
        p->buf = p->tempBuf;
        if (LzmaDec_DecodeReal2(p, dicLimit, p->buf) != 0)
          return SZ_ERROR_DATA;
        lookAhead -= (rem - (unsigned)(p->buf - p->tempBuf));
        (*srcLen) += lookAhead;
        src += lookAhead;
        inSize -= lookAhead;
        p->tempBufSize = 0;
      }
    }
    if (p->code == 0)
      *status = LZMA_STATUS_FINISHED_WITH_MARK;
    return (p->code == 0) ? SZ_OK : SZ_ERROR_DATA;
  }

  SRes LzmaDec_DecodeToBuf(CLzmaDec *p, Byte *dest, SizeT *destLen, const Byte *src, SizeT *srcLen, ELzmaFinishMode finishMode, ELzmaStatus *status)
  {
    SizeT outSize = *destLen;
    SizeT inSize = *srcLen;
    *srcLen = *destLen = 0;
    for (;;)
    {
      SizeT inSizeCur = inSize, outSizeCur, dicPos;
      ELzmaFinishMode curFinishMode;
      SRes res;
      if (p->dicPos == p->dicBufSize)
        p->dicPos = 0;
      dicPos = p->dicPos;
      if (outSize > p->dicBufSize - dicPos)
      {
        outSizeCur = p->dicBufSize;
        curFinishMode = LZMA_FINISH_ANY;
      }
      else
      {
        outSizeCur = dicPos + outSize;
        curFinishMode = finishMode;
      }

      res = LzmaDec_DecodeToDic(p, outSizeCur, src, &inSizeCur, curFinishMode, status);
      src += inSizeCur;
      inSize -= inSizeCur;
      *srcLen += inSizeCur;
      outSizeCur = p->dicPos - dicPos;
      memcpy(dest, p->dic + dicPos, outSizeCur);
      dest += outSizeCur;
      outSize -= outSizeCur;
      *destLen += outSizeCur;
      if (res != 0)
        return res;
      if (outSizeCur == 0 || outSize == 0)
        return SZ_OK;
    }
  }

  void LzmaDec_FreeProbs(CLzmaDec *p, ISzAlloc *alloc)
  {
    alloc->Free(alloc, p->probs);
    p->probs = 0;
  }

  static void LzmaDec_FreeDict(CLzmaDec *p, ISzAlloc *alloc)
  {
    alloc->Free(alloc, p->dic);
    p->dic = 0;
  }

  void LzmaDec_Free(CLzmaDec *p, ISzAlloc *alloc)
  {
    LzmaDec_FreeProbs(p, alloc);
    LzmaDec_FreeDict(p, alloc);
  }

  SRes LzmaProps_Decode(CLzmaProps *p, const Byte *data, unsigned size)
  {
    UInt32 dicSize;
    Byte d;

    if (size < LZMA_PROPS_SIZE)
      return SZ_ERROR_UNSUPPORTED;
    else
      dicSize = data[1] | ((UInt32)data[2] << 8) | ((UInt32)data[3] << 16) | ((UInt32)data[4] << 24);

    if (dicSize < LZMA_DIC_MIN)
      dicSize = LZMA_DIC_MIN;
    p->dicSize = dicSize;

    d = data[0];
    if (d >= (9 * 5 * 5))
      return SZ_ERROR_UNSUPPORTED;

    p->lc = d % 9;
    d /= 9;
    p->pb = d / 5;
    p->lp = d % 5;

    return SZ_OK;
  }

  static SRes LzmaDec_AllocateProbs2(CLzmaDec *p, const CLzmaProps *propNew, ISzAlloc *alloc)
  {
    UInt32 numProbs = LzmaProps_GetNumProbs(propNew);
    if (p->probs == 0 || numProbs != p->numProbs)
    {
      LzmaDec_FreeProbs(p, alloc);
      p->probs = (CLzmaProb *)alloc->Alloc(alloc, numProbs * sizeof(CLzmaProb));
      p->numProbs = numProbs;
      if (p->probs == 0)
        return SZ_ERROR_MEM;
    }
    return SZ_OK;
  }

  SRes LzmaDec_AllocateProbs(CLzmaDec *p, const Byte *props, unsigned propsSize, ISzAlloc *alloc)
  {
    CLzmaProps propNew;
    RINOK(LzmaProps_Decode(&propNew, props, propsSize));
    RINOK(LzmaDec_AllocateProbs2(p, &propNew, alloc));
    p->prop = propNew;
    return SZ_OK;
  }

  SRes LzmaDec_Allocate(CLzmaDec *p, const Byte *props, unsigned propsSize, ISzAlloc *alloc)
  {
    CLzmaProps propNew;
    SizeT dicBufSize;
    RINOK(LzmaProps_Decode(&propNew, props, propsSize));
    RINOK(LzmaDec_AllocateProbs2(p, &propNew, alloc));
    dicBufSize = propNew.dicSize;
    if (p->dic == 0 || dicBufSize != p->dicBufSize)
    {
      LzmaDec_FreeDict(p, alloc);
      p->dic = (Byte *)alloc->Alloc(alloc, dicBufSize);
      if (p->dic == 0)
      {
        LzmaDec_FreeProbs(p, alloc);
        return SZ_ERROR_MEM;
      }
    }
    p->dicBufSize = dicBufSize;
    p->prop = propNew;
    return SZ_OK;
  }

  SRes LzmaDecode(Byte *dest, SizeT *destLen, const Byte *src, SizeT *srcLen,
                  const Byte *propData, unsigned propSize, ELzmaFinishMode finishMode,
                  ELzmaStatus *status, ISzAlloc *alloc)
  {
    CLzmaDec p;
    SRes res;
    SizeT inSize = *srcLen;
    SizeT outSize = *destLen;
    *srcLen = *destLen = 0;
    if (inSize < RC_INIT_SIZE)
      return SZ_ERROR_INPUT_EOF;

    LzmaDec_Construct(&p);
    res = LzmaDec_AllocateProbs(&p, propData, propSize, alloc);
    if (res != 0)
      return res;
    p.dic = dest;
    p.dicBufSize = outSize;

    LzmaDec_Init(&p);

    *srcLen = inSize;
    res = LzmaDec_DecodeToDic(&p, outSize, src, srcLen, finishMode, status);

    if (res == SZ_OK && *status == LZMA_STATUS_NEEDS_MORE_INPUT)
      res = SZ_ERROR_INPUT_EOF;

    (*destLen) = p.dicPos;
    LzmaDec_FreeProbs(&p, alloc);
    return res;
  }

  /* LzmaEnc.c -- LZMA Encoder
  2009-02-02 : Igor Pavlov : Public domain */

  /* #define SHOW_STAT */
  /* #define SHOW_STAT2 */

  #ifdef SHOW_STAT
  static int ttt = 0;
  #endif

  #define kBlockSizeMax ((1 << LZMA_NUM_BLOCK_SIZE_BITS) - 1)

  #define kBlockSize (9 << 10)
  #define kUnpackBlockSize (1 << 18)
  #define kMatchArraySize (1 << 21)
  #define kMatchRecordMaxSize ((LZMA_MATCH_LEN_MAX * 2 + 3) * LZMA_MATCH_LEN_MAX)

  #define kNumMaxDirectBits (31)

  #define kNumTopBits 24
  #define kTopValue ((UInt32)1 << kNumTopBits)

  #define kNumBitModelTotalBits 11
  #define kBitModelTotal (1 << kNumBitModelTotalBits)
  #define kNumMoveBits 5
  #define kProbInitValue (kBitModelTotal >> 1)

  #define kNumMoveReducingBits 4
  #define kNumBitPriceShiftBits 4
  #define kBitPrice (1 << kNumBitPriceShiftBits)

  void LzmaEncProps_Init(CLzmaEncProps *p)
  {
    p->level = 5;
    p->dictSize = p->mc = 0;
    p->lc = p->lp = p->pb = p->algo = p->fb = p->btMode = p->numHashBytes = p->numThreads = -1;
    p->writeEndMark = 0;
  }

  void LzmaEncProps_Normalize(CLzmaEncProps *p)
  {
    int level = p->level;
    if (level < 0)
      level = 5;
    p->level = level;
    if (p->dictSize == 0)
      p->dictSize = (level <= 5 ? (1 << (level * 2 + 14)) : (level == 6 ? (1 << 25) : (1 << 26)));
    if (p->lc < 0)
      p->lc = 3;
    if (p->lp < 0)
      p->lp = 0;
    if (p->pb < 0)
      p->pb = 2;
    if (p->algo < 0)
      p->algo = (level < 5 ? 0 : 1);
    if (p->fb < 0)
      p->fb = (level < 7 ? 32 : 64);
    if (p->btMode < 0)
      p->btMode = (p->algo == 0 ? 0 : 1);
    if (p->numHashBytes < 0)
      p->numHashBytes = 4;
    if (p->mc == 0)
      p->mc = (16 + (p->fb >> 1)) >> (p->btMode ? 0 : 1);
    if (p->numThreads < 0)
      p->numThreads =
  #ifdef COMPRESS_MF_MT
          ((p->btMode && p->algo) ? 2 : 1);
  #else
          1;
  #endif
  }

  UInt32 LzmaEncProps_GetDictSize(const CLzmaEncProps *props2)
  {
    CLzmaEncProps props = *props2;
    LzmaEncProps_Normalize(&props);
    return props.dictSize;
  }

  /* #define LZMA_LOG_BSR */
  /* Define it for Intel's CPU */

  #ifdef LZMA_LOG_BSR

  #define kDicLogSizeMaxCompress 30

  #define BSR2_RET(pos, res)                  \
    {                                         \
      unsigned long i;                        \
      _BitScanReverse(&i, (pos));             \
      res = (i + i) + ((pos >> (i - 1)) & 1); \
    }

  static UInt32 GetPosSlot1(UInt32 pos)
  {
    UInt32 res;
    BSR2_RET(pos, res);
    return res;
  }
  #define GetPosSlot2(pos, res) \
    {                           \
      BSR2_RET(pos, res);       \
    }
  #define GetPosSlot(pos, res) \
    {                          \
      if (pos < 2)             \
        res = pos;             \
      else                     \
        BSR2_RET(pos, res);    \
    }

  #else

  #define kNumLogBits (9 + (int)sizeof(size_t) / 2)
  #define kDicLogSizeMaxCompress ((kNumLogBits - 1) * 2 + 7)

  void LzmaEnc_FastPosInit(Byte *g_FastPos)
  {
    int c = 2, slotFast;
    g_FastPos[0] = 0;
    g_FastPos[1] = 1;

    for (slotFast = 2; slotFast < kNumLogBits * 2; slotFast++)
    {
      UInt32 k = (1 << ((slotFast >> 1) - 1));
      UInt32 j;
      for (j = 0; j < k; j++, c++)
        g_FastPos[c] = (Byte)slotFast;
    }
  }

  #define BSR2_RET(pos, res)                                                         \
    {                                                                                \
      UInt32 i = 6 + ((kNumLogBits - 1) &                                            \
                      (0 - (((((UInt32)1 << (kNumLogBits + 6)) - 1) - pos) >> 31))); \
      res = p->g_FastPos[pos >> i] + (i * 2);                                        \
    }
  /*
  #define BSR2_RET(pos, res) { res = (pos < (1 << (kNumLogBits + 6))) ? \
    p->g_FastPos[pos >> 6] + 12 : \
    p->g_FastPos[pos >> (6 + kNumLogBits - 1)] + (6 + (kNumLogBits - 1)) * 2; }
  */

  #define GetPosSlot1(pos) p->g_FastPos[pos]
  #define GetPosSlot2(pos, res) \
    {                           \
      BSR2_RET(pos, res);       \
    }
  #define GetPosSlot(pos, res)     \
    {                              \
      if (pos < kNumFullDistances) \
        res = p->g_FastPos[pos];   \
      else                         \
        BSR2_RET(pos, res);        \
    }

  #endif

  #define LZMA_NUM_REPS 4

  typedef unsigned CState;

  typedef struct _COptimal
  {
    UInt32 price;

    CState state;
    int prev1IsChar;
    int prev2;

    UInt32 posPrev2;
    UInt32 backPrev2;

    UInt32 posPrev;
    UInt32 backPrev;
    UInt32 backs[LZMA_NUM_REPS];
  } COptimal;

  #define kNumOpts (1 << 12)

  #define kNumLenToPosStates 4
  #define kNumPosSlotBits 6
  #define kDicLogSizeMin 0
  #define kDicLogSizeMax 32
  #define kDistTableSizeMax (kDicLogSizeMax * 2)

  #define kNumAlignBits 4
  #define kAlignTableSize (1 << kNumAlignBits)
  #define kAlignMask (kAlignTableSize - 1)

  #define kStartPosModelIndex 4
  #define kEndPosModelIndex 14
  #define kNumPosModels (kEndPosModelIndex - kStartPosModelIndex)

  // #define kNumFullDistances (1 << (kEndPosModelIndex / 2))

  #ifdef _LZMA_PROB32
  #define CLzmaProb UInt32
  #else
  #define CLzmaProb UInt16
  #endif

  #define LZMA_PB_MAX 4
  #define LZMA_LC_MAX 8
  #define LZMA_LP_MAX 4

  #define LZMA_NUM_PB_STATES_MAX (1 << LZMA_PB_MAX)

  #define kLenNumLowBits 3
  #define kLenNumLowSymbols (1 << kLenNumLowBits)
  #define kLenNumMidBits 3
  #define kLenNumMidSymbols (1 << kLenNumMidBits)
  #define kLenNumHighBits 8
  #define kLenNumHighSymbols (1 << kLenNumHighBits)

  #define kLenNumSymbolsTotal (kLenNumLowSymbols + kLenNumMidSymbols + kLenNumHighSymbols)

  #define LZMA_MATCH_LEN_MIN 2
  #define LZMA_MATCH_LEN_MAX (LZMA_MATCH_LEN_MIN + kLenNumSymbolsTotal - 1)

  #define kNumStates 12

  typedef struct
  {
    CLzmaProb choice;
    CLzmaProb choice2;
    CLzmaProb low[LZMA_NUM_PB_STATES_MAX << kLenNumLowBits];
    CLzmaProb mid[LZMA_NUM_PB_STATES_MAX << kLenNumMidBits];
    CLzmaProb high[kLenNumHighSymbols];
  } CLenEnc;

  typedef struct
  {
    CLenEnc p;
    UInt32 prices[LZMA_NUM_PB_STATES_MAX][kLenNumSymbolsTotal];
    UInt32 tableSize;
    UInt32 counters[LZMA_NUM_PB_STATES_MAX];
  } CLenPriceEnc;

  typedef struct _CRangeEnc
  {
    UInt32 range;
    Byte cache;
    UInt64 low;
    UInt64 cacheSize;
    Byte *buf;
    Byte *bufLim;
    Byte *bufBase;
    ISeqOutStream *outStream;
    UInt64 processed;
    SRes res;
  } CRangeEnc;

  typedef struct _CSeqInStreamBuf
  {
    ISeqInStream funcTable;
    const Byte *data;
    SizeT rem;
  } CSeqInStreamBuf;

  static SRes MyRead(void *pp, void *data, size_t *size)
  {
    size_t curSize = *size;
    CSeqInStreamBuf *p = (CSeqInStreamBuf *)pp;
    if (p->rem < curSize)
      curSize = p->rem;
    memcpy(data, p->data, curSize);
    p->rem -= curSize;
    p->data += curSize;
    *size = curSize;
    return SZ_OK;
  }

  typedef struct
  {
    CLzmaProb *litProbs;

    CLzmaProb isMatch[kNumStates][LZMA_NUM_PB_STATES_MAX];
    CLzmaProb isRep[kNumStates];
    CLzmaProb isRepG0[kNumStates];
    CLzmaProb isRepG1[kNumStates];
    CLzmaProb isRepG2[kNumStates];
    CLzmaProb isRep0Long[kNumStates][LZMA_NUM_PB_STATES_MAX];

    CLzmaProb posSlotEncoder[kNumLenToPosStates][1 << kNumPosSlotBits];
    CLzmaProb posEncoders[kNumFullDistances - kEndPosModelIndex];
    CLzmaProb posAlignEncoder[1 << kNumAlignBits];

    CLenPriceEnc lenEnc;
    CLenPriceEnc repLenEnc;

    UInt32 reps[LZMA_NUM_REPS];
    UInt32 state;
  } CSaveState;

  typedef struct _CLzmaEnc
  {
    IMatchFinder matchFinder;
    void *matchFinderObj;

  #ifdef COMPRESS_MF_MT
    Bool mtMode;
    CMatchFinderMt matchFinderMt;
  #endif

    CMatchFinder matchFinderBase;

  #ifdef COMPRESS_MF_MT
    Byte pad[128];
  #endif

    UInt32 optimumEndIndex;
    UInt32 optimumCurrentIndex;

    UInt32 longestMatchLength;
    UInt32 numPairs;
    UInt32 numAvail;
    COptimal opt[kNumOpts];

  #ifndef LZMA_LOG_BSR
    Byte g_FastPos[1 << kNumLogBits];
  #endif

    UInt32 ProbPrices[kBitModelTotal >> kNumMoveReducingBits];
    UInt32 matches[LZMA_MATCH_LEN_MAX * 2 + 2 + 1];
    UInt32 numFastBytes;
    UInt32 additionalOffset;
    UInt32 reps[LZMA_NUM_REPS];
    UInt32 state;

    UInt32 posSlotPrices[kNumLenToPosStates][kDistTableSizeMax];
    UInt32 distancesPrices[kNumLenToPosStates][kNumFullDistances];
    UInt32 alignPrices[kAlignTableSize];
    UInt32 alignPriceCount;

    UInt32 distTableSize;

    unsigned lc, lp, pb;
    unsigned lpMask, pbMask;

    CLzmaProb *litProbs;

    CLzmaProb isMatch[kNumStates][LZMA_NUM_PB_STATES_MAX];
    CLzmaProb isRep[kNumStates];
    CLzmaProb isRepG0[kNumStates];
    CLzmaProb isRepG1[kNumStates];
    CLzmaProb isRepG2[kNumStates];
    CLzmaProb isRep0Long[kNumStates][LZMA_NUM_PB_STATES_MAX];

    CLzmaProb posSlotEncoder[kNumLenToPosStates][1 << kNumPosSlotBits];
    CLzmaProb posEncoders[kNumFullDistances - kEndPosModelIndex];
    CLzmaProb posAlignEncoder[1 << kNumAlignBits];

    CLenPriceEnc lenEnc;
    CLenPriceEnc repLenEnc;

    unsigned lclp;

    Bool fastMode;

    CRangeEnc rc;

    Bool writeEndMark;
    UInt64 nowPos64;
    UInt32 matchPriceCount;
    Bool finished;
    Bool multiThread;

    SRes result;
    UInt32 dictSize;
    UInt32 matchFinderCycles;

    ISeqInStream *inStream;
    CSeqInStreamBuf seqBufInStream;

    CSaveState saveState;
  } CLzmaEnc;

  void LzmaEnc_SaveState(CLzmaEncHandle pp)
  {
    CLzmaEnc *p = (CLzmaEnc *)pp;
    CSaveState *dest = &p->saveState;
    int i;
    dest->lenEnc = p->lenEnc;
    dest->repLenEnc = p->repLenEnc;
    dest->state = p->state;

    for (i = 0; i < kNumStates; i++)
    {
      memcpy(dest->isMatch[i], p->isMatch[i], sizeof(p->isMatch[i]));
      memcpy(dest->isRep0Long[i], p->isRep0Long[i], sizeof(p->isRep0Long[i]));
    }
    for (i = 0; i < kNumLenToPosStates; i++)
      memcpy(dest->posSlotEncoder[i], p->posSlotEncoder[i], sizeof(p->posSlotEncoder[i]));
    memcpy(dest->isRep, p->isRep, sizeof(p->isRep));
    memcpy(dest->isRepG0, p->isRepG0, sizeof(p->isRepG0));
    memcpy(dest->isRepG1, p->isRepG1, sizeof(p->isRepG1));
    memcpy(dest->isRepG2, p->isRepG2, sizeof(p->isRepG2));
    memcpy(dest->posEncoders, p->posEncoders, sizeof(p->posEncoders));
    memcpy(dest->posAlignEncoder, p->posAlignEncoder, sizeof(p->posAlignEncoder));
    memcpy(dest->reps, p->reps, sizeof(p->reps));
    memcpy(dest->litProbs, p->litProbs, (0x300 << p->lclp) * sizeof(CLzmaProb));
  }

  void LzmaEnc_RestoreState(CLzmaEncHandle pp)
  {
    CLzmaEnc *dest = (CLzmaEnc *)pp;
    const CSaveState *p = &dest->saveState;
    int i;
    dest->lenEnc = p->lenEnc;
    dest->repLenEnc = p->repLenEnc;
    dest->state = p->state;

    for (i = 0; i < kNumStates; i++)
    {
      memcpy(dest->isMatch[i], p->isMatch[i], sizeof(p->isMatch[i]));
      memcpy(dest->isRep0Long[i], p->isRep0Long[i], sizeof(p->isRep0Long[i]));
    }
    for (i = 0; i < kNumLenToPosStates; i++)
      memcpy(dest->posSlotEncoder[i], p->posSlotEncoder[i], sizeof(p->posSlotEncoder[i]));
    memcpy(dest->isRep, p->isRep, sizeof(p->isRep));
    memcpy(dest->isRepG0, p->isRepG0, sizeof(p->isRepG0));
    memcpy(dest->isRepG1, p->isRepG1, sizeof(p->isRepG1));
    memcpy(dest->isRepG2, p->isRepG2, sizeof(p->isRepG2));
    memcpy(dest->posEncoders, p->posEncoders, sizeof(p->posEncoders));
    memcpy(dest->posAlignEncoder, p->posAlignEncoder, sizeof(p->posAlignEncoder));
    memcpy(dest->reps, p->reps, sizeof(p->reps));
    memcpy(dest->litProbs, p->litProbs, (0x300 << dest->lclp) * sizeof(CLzmaProb));
  }

  SRes LzmaEnc_SetProps(CLzmaEncHandle pp, const CLzmaEncProps *props2)
  {
    CLzmaEnc *p = (CLzmaEnc *)pp;
    CLzmaEncProps props = *props2;
    LzmaEncProps_Normalize(&props);

    if (props.lc > LZMA_LC_MAX || props.lp > LZMA_LP_MAX || props.pb > LZMA_PB_MAX ||
        props.dictSize > (1U << kDicLogSizeMaxCompress) || props.dictSize > (1U << 30))
      return SZ_ERROR_PARAM;
    p->dictSize = props.dictSize;
    p->matchFinderCycles = props.mc;
    {
      unsigned fb = props.fb;
      if (fb < 5)
        fb = 5;
      if (fb > LZMA_MATCH_LEN_MAX)
        fb = LZMA_MATCH_LEN_MAX;
      p->numFastBytes = fb;
    }
    p->lc = props.lc;
    p->lp = props.lp;
    p->pb = props.pb;
    p->fastMode = (props.algo == 0);
    p->matchFinderBase.btMode = props.btMode;
    {
      UInt32 numHashBytes = 4;
      if (props.btMode)
      {
        if (props.numHashBytes < 2)
          numHashBytes = 2;
        else if (props.numHashBytes < 4)
          numHashBytes = props.numHashBytes;
      }
      p->matchFinderBase.numHashBytes = numHashBytes;
    }

    p->matchFinderBase.cutValue = props.mc;

    p->writeEndMark = props.writeEndMark;

  #ifdef COMPRESS_MF_MT
    /*
    if (newMultiThread != _multiThread)
    {
      ReleaseMatchFinder();
      _multiThread = newMultiThread;
    }
    */
    p->multiThread = (props.numThreads > 1);
  #endif

    return SZ_OK;
  }

  static const int kLiteralNextStates[kNumStates] = {0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 4, 5};
  static const int kMatchNextStates[kNumStates] = {7, 7, 7, 7, 7, 7, 7, 10, 10, 10, 10, 10};
  static const int kRepNextStates[kNumStates] = {8, 8, 8, 8, 8, 8, 8, 11, 11, 11, 11, 11};
  static const int kShortRepNextStates[kNumStates] = {9, 9, 9, 9, 9, 9, 9, 11, 11, 11, 11, 11};

  #define IsCharState(s) ((s) < 7)

  #define GetLenToPosState(len) (((len) < kNumLenToPosStates + 1) ? (len)-2 : kNumLenToPosStates - 1)

  #define kInfinityPrice (1 << 30)

  static void RangeEnc_Construct(CRangeEnc *p)
  {
    p->outStream = 0;
    p->bufBase = 0;
  }

  #define RangeEnc_GetProcessed(p) ((p)->processed + ((p)->buf - (p)->bufBase) + (p)->cacheSize)

  #define RC_BUF_SIZE (1 << 16)
  static int RangeEnc_Alloc(CRangeEnc *p, ISzAlloc *alloc)
  {
    if (p->bufBase == 0)
    {
      p->bufBase = (Byte *)alloc->Alloc(alloc, RC_BUF_SIZE);
      if (p->bufBase == 0)
        return 0;
      p->bufLim = p->bufBase + RC_BUF_SIZE;
    }
    return 1;
  }

  static void RangeEnc_Free(CRangeEnc *p, ISzAlloc *alloc)
  {
    alloc->Free(alloc, p->bufBase);
    p->bufBase = 0;
  }

  static void RangeEnc_Init(CRangeEnc *p)
  {
    /* Stream.Init(); */
    p->low = 0;
    p->range = 0xFFFFFFFF;
    p->cacheSize = 1;
    p->cache = 0;

    p->buf = p->bufBase;

    p->processed = 0;
    p->res = SZ_OK;
  }

  static void RangeEnc_FlushStream(CRangeEnc *p)
  {
    size_t num;
    if (p->res != SZ_OK)
      return;
    num = p->buf - p->bufBase;
    if (num != p->outStream->Write(p->outStream, p->bufBase, num))
      p->res = SZ_ERROR_WRITE;
    p->processed += num;
    p->buf = p->bufBase;
  }

  static void MY_FAST_CALL RangeEnc_ShiftLow(CRangeEnc *p)
  {
    if ((UInt32)p->low < (UInt32)0xFF000000 || (int)(p->low >> 32) != 0)
    {
      Byte temp = p->cache;
      do
      {
        Byte *buf = p->buf;
        *buf++ = (Byte)(temp + (Byte)(p->low >> 32));
        p->buf = buf;
        if (buf == p->bufLim)
          RangeEnc_FlushStream(p);
        temp = 0xFF;
      } while (--p->cacheSize != 0);
      p->cache = (Byte)((UInt32)p->low >> 24);
    }
    p->cacheSize++;
    p->low = (UInt32)p->low << 8;
  }

  static void RangeEnc_FlushData(CRangeEnc *p)
  {
    int i;
    for (i = 0; i < 5; i++)
      RangeEnc_ShiftLow(p);
  }

  static void RangeEnc_EncodeDirectBits(CRangeEnc *p, UInt32 value, int numBits)
  {
    do
    {
      p->range >>= 1;
      p->low += p->range & (0 - ((value >> --numBits) & 1));
      if (p->range < kTopValue)
      {
        p->range <<= 8;
        RangeEnc_ShiftLow(p);
      }
    } while (numBits != 0);
  }

  static void RangeEnc_EncodeBit(CRangeEnc *p, CLzmaProb *prob, UInt32 symbol)
  {
    UInt32 ttt = *prob;
    UInt32 newBound = (p->range >> kNumBitModelTotalBits) * ttt;
    if (symbol == 0)
    {
      p->range = newBound;
      ttt += (kBitModelTotal - ttt) >> kNumMoveBits;
    }
    else
    {
      p->low += newBound;
      p->range -= newBound;
      ttt -= ttt >> kNumMoveBits;
    }
    *prob = (CLzmaProb)ttt;
    if (p->range < kTopValue)
    {
      p->range <<= 8;
      RangeEnc_ShiftLow(p);
    }
  }

  static void LitEnc_Encode(CRangeEnc *p, CLzmaProb *probs, UInt32 symbol)
  {
    symbol |= 0x100;
    do
    {
      RangeEnc_EncodeBit(p, probs + (symbol >> 8), (symbol >> 7) & 1);
      symbol <<= 1;
    } while (symbol < 0x10000);
  }

  static void LitEnc_EncodeMatched(CRangeEnc *p, CLzmaProb *probs, UInt32 symbol, UInt32 matchByte)
  {
    UInt32 offs = 0x100;
    symbol |= 0x100;
    do
    {
      matchByte <<= 1;
      RangeEnc_EncodeBit(p, probs + (offs + (matchByte & offs) + (symbol >> 8)), (symbol >> 7) & 1);
      symbol <<= 1;
      offs &= ~(matchByte ^ symbol);
    } while (symbol < 0x10000);
  }

  void LzmaEnc_InitPriceTables(UInt32 *ProbPrices)
  {
    UInt32 i;
    for (i = (1 << kNumMoveReducingBits) / 2; i < kBitModelTotal; i += (1 << kNumMoveReducingBits))
    {
      const int kCyclesBits = kNumBitPriceShiftBits;
      UInt32 w = i;
      UInt32 bitCount = 0;
      int j;
      for (j = 0; j < kCyclesBits; j++)
      {
        w = w * w;
        bitCount <<= 1;
        while (w >= ((UInt32)1 << 16))
        {
          w >>= 1;
          bitCount++;
        }
      }
      ProbPrices[i >> kNumMoveReducingBits] = ((kNumBitModelTotalBits << kCyclesBits) - 15 - bitCount);
    }
  }

  #define GET_PRICE(prob, symbol) \
    p->ProbPrices[((prob) ^ (((-(int)(symbol))) & (kBitModelTotal - 1))) >> kNumMoveReducingBits];

  #define GET_PRICEa(prob, symbol) \
    ProbPrices[((prob) ^ ((-((int)(symbol))) & (kBitModelTotal - 1))) >> kNumMoveReducingBits];

  #define GET_PRICE_0(prob) p->ProbPrices[(prob) >> kNumMoveReducingBits]
  #define GET_PRICE_1(prob) p->ProbPrices[((prob) ^ (kBitModelTotal - 1)) >> kNumMoveReducingBits]

  #define GET_PRICE_0a(prob) ProbPrices[(prob) >> kNumMoveReducingBits]
  #define GET_PRICE_1a(prob) ProbPrices[((prob) ^ (kBitModelTotal - 1)) >> kNumMoveReducingBits]

  static UInt32 LitEnc_GetPrice(const CLzmaProb *probs, UInt32 symbol, UInt32 *ProbPrices)
  {
    UInt32 price = 0;
    symbol |= 0x100;
    do
    {
      price += GET_PRICEa(probs[symbol >> 8], (symbol >> 7) & 1);
      symbol <<= 1;
    } while (symbol < 0x10000);
    return price;
  }

  static UInt32 LitEnc_GetPriceMatched(const CLzmaProb *probs, UInt32 symbol, UInt32 matchByte, UInt32 *ProbPrices)
  {
    UInt32 price = 0;
    UInt32 offs = 0x100;
    symbol |= 0x100;
    do
    {
      matchByte <<= 1;
      price += GET_PRICEa(probs[offs + (matchByte & offs) + (symbol >> 8)], (symbol >> 7) & 1);
      symbol <<= 1;
      offs &= ~(matchByte ^ symbol);
    } while (symbol < 0x10000);
    return price;
  }

  static void RcTree_Encode(CRangeEnc *rc, CLzmaProb *probs, int numBitLevels, UInt32 symbol)
  {
    UInt32 m = 1;
    int i;
    for (i = numBitLevels; i != 0;)
    {
      UInt32 bit;
      i--;
      bit = (symbol >> i) & 1;
      RangeEnc_EncodeBit(rc, probs + m, bit);
      m = (m << 1) | bit;
    }
  }

  static void RcTree_ReverseEncode(CRangeEnc *rc, CLzmaProb *probs, int numBitLevels, UInt32 symbol)
  {
    UInt32 m = 1;
    int i;
    for (i = 0; i < numBitLevels; i++)
    {
      UInt32 bit = symbol & 1;
      RangeEnc_EncodeBit(rc, probs + m, bit);
      m = (m << 1) | bit;
      symbol >>= 1;
    }
  }

  static UInt32 RcTree_GetPrice(const CLzmaProb *probs, int numBitLevels, UInt32 symbol, UInt32 *ProbPrices)
  {
    UInt32 price = 0;
    symbol |= (1 << numBitLevels);
    while (symbol != 1)
    {
      price += GET_PRICEa(probs[symbol >> 1], symbol & 1);
      symbol >>= 1;
    }
    return price;
  }

  static UInt32 RcTree_ReverseGetPrice(const CLzmaProb *probs, int numBitLevels, UInt32 symbol, UInt32 *ProbPrices)
  {
    UInt32 price = 0;
    UInt32 m = 1;
    int i;
    for (i = numBitLevels; i != 0; i--)
    {
      UInt32 bit = symbol & 1;
      symbol >>= 1;
      price += GET_PRICEa(probs[m], bit);
      m = (m << 1) | bit;
    }
    return price;
  }

  static void LenEnc_Init(CLenEnc *p)
  {
    unsigned i;
    p->choice = p->choice2 = kProbInitValue;
    for (i = 0; i < (LZMA_NUM_PB_STATES_MAX << kLenNumLowBits); i++)
      p->low[i] = kProbInitValue;
    for (i = 0; i < (LZMA_NUM_PB_STATES_MAX << kLenNumMidBits); i++)
      p->mid[i] = kProbInitValue;
    for (i = 0; i < kLenNumHighSymbols; i++)
      p->high[i] = kProbInitValue;
  }

  static void LenEnc_Encode(CLenEnc *p, CRangeEnc *rc, UInt32 symbol, UInt32 posState)
  {
    if (symbol < kLenNumLowSymbols)
    {
      RangeEnc_EncodeBit(rc, &p->choice, 0);
      RcTree_Encode(rc, p->low + (posState << kLenNumLowBits), kLenNumLowBits, symbol);
    }
    else
    {
      RangeEnc_EncodeBit(rc, &p->choice, 1);
      if (symbol < kLenNumLowSymbols + kLenNumMidSymbols)
      {
        RangeEnc_EncodeBit(rc, &p->choice2, 0);
        RcTree_Encode(rc, p->mid + (posState << kLenNumMidBits), kLenNumMidBits, symbol - kLenNumLowSymbols);
      }
      else
      {
        RangeEnc_EncodeBit(rc, &p->choice2, 1);
        RcTree_Encode(rc, p->high, kLenNumHighBits, symbol - kLenNumLowSymbols - kLenNumMidSymbols);
      }
    }
  }

  static void LenEnc_SetPrices(CLenEnc *p, UInt32 posState, UInt32 numSymbols, UInt32 *prices, UInt32 *ProbPrices)
  {
    UInt32 a0 = GET_PRICE_0a(p->choice);
    UInt32 a1 = GET_PRICE_1a(p->choice);
    UInt32 b0 = a1 + GET_PRICE_0a(p->choice2);
    UInt32 b1 = a1 + GET_PRICE_1a(p->choice2);
    UInt32 i = 0;
    for (i = 0; i < kLenNumLowSymbols; i++)
    {
      if (i >= numSymbols)
        return;
      prices[i] = a0 + RcTree_GetPrice(p->low + (posState << kLenNumLowBits), kLenNumLowBits, i, ProbPrices);
    }
    for (; i < kLenNumLowSymbols + kLenNumMidSymbols; i++)
    {
      if (i >= numSymbols)
        return;
      prices[i] = b0 + RcTree_GetPrice(p->mid + (posState << kLenNumMidBits), kLenNumMidBits, i - kLenNumLowSymbols, ProbPrices);
    }
    for (; i < numSymbols; i++)
      prices[i] = b1 + RcTree_GetPrice(p->high, kLenNumHighBits, i - kLenNumLowSymbols - kLenNumMidSymbols, ProbPrices);
  }

  static void MY_FAST_CALL LenPriceEnc_UpdateTable(CLenPriceEnc *p, UInt32 posState, UInt32 *ProbPrices)
  {
    LenEnc_SetPrices(&p->p, posState, p->tableSize, p->prices[posState], ProbPrices);
    p->counters[posState] = p->tableSize;
  }

  static void LenPriceEnc_UpdateTables(CLenPriceEnc *p, UInt32 numPosStates, UInt32 *ProbPrices)
  {
    UInt32 posState;
    for (posState = 0; posState < numPosStates; posState++)
      LenPriceEnc_UpdateTable(p, posState, ProbPrices);
  }

  static void LenEnc_Encode2(CLenPriceEnc *p, CRangeEnc *rc, UInt32 symbol, UInt32 posState, Bool updatePrice, UInt32 *ProbPrices)
  {
    LenEnc_Encode(&p->p, rc, symbol, posState);
    if (updatePrice)
      if (--p->counters[posState] == 0)
        LenPriceEnc_UpdateTable(p, posState, ProbPrices);
  }

  static void MovePos(CLzmaEnc *p, UInt32 num)
  {
  #ifdef SHOW_STAT
    ttt += num;
    printf("\n MovePos %d", num);
  #endif
    if (num != 0)
    {
      p->additionalOffset += num;
      p->matchFinder.Skip(p->matchFinderObj, num);
    }
  }

  static UInt32 ReadMatchDistances(CLzmaEnc *p, UInt32 *numDistancePairsRes)
  {
    UInt32 lenRes = 0, numPairs;
    p->numAvail = p->matchFinder.GetNumAvailableBytes(p->matchFinderObj);
    numPairs = p->matchFinder.GetMatches(p->matchFinderObj, p->matches);
  #ifdef SHOW_STAT
    printf("\n i = %d numPairs = %d    ", ttt, numPairs / 2);
    ttt++;
    {
      UInt32 i;
      for (i = 0; i < numPairs; i += 2)
        printf("%2d %6d   | ", p->matches[i], p->matches[i + 1]);
    }
  #endif
    if (numPairs > 0)
    {
      lenRes = p->matches[numPairs - 2];
      if (lenRes == p->numFastBytes)
      {
        const Byte *pby = p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - 1;
        UInt32 distance = p->matches[numPairs - 1] + 1;
        UInt32 numAvail = p->numAvail;
        if (numAvail > LZMA_MATCH_LEN_MAX)
          numAvail = LZMA_MATCH_LEN_MAX;
        {
          const Byte *pby2 = pby - distance;
          for (; lenRes < numAvail && pby[lenRes] == pby2[lenRes]; lenRes++)
            ;
        }
      }
    }
    p->additionalOffset++;
    *numDistancePairsRes = numPairs;
    return lenRes;
  }

  #define MakeAsChar(p)           \
    (p)->backPrev = (UInt32)(-1); \
    (p)->prev1IsChar = False;
  #define MakeAsShortRep(p) \
    (p)->backPrev = 0;      \
    (p)->prev1IsChar = False;
  #define IsShortRep(p) ((p)->backPrev == 0)

  static UInt32 GetRepLen1Price(CLzmaEnc *p, UInt32 state, UInt32 posState)
  {
    return GET_PRICE_0(p->isRepG0[state]) +
          GET_PRICE_0(p->isRep0Long[state][posState]);
  }

  static UInt32 GetPureRepPrice(CLzmaEnc *p, UInt32 repIndex, UInt32 state, UInt32 posState)
  {
    UInt32 price;
    if (repIndex == 0)
    {
      price = GET_PRICE_0(p->isRepG0[state]);
      price += GET_PRICE_1(p->isRep0Long[state][posState]);
    }
    else
    {
      price = GET_PRICE_1(p->isRepG0[state]);
      if (repIndex == 1)
        price += GET_PRICE_0(p->isRepG1[state]);
      else
      {
        price += GET_PRICE_1(p->isRepG1[state]);
        price += GET_PRICE(p->isRepG2[state], repIndex - 2);
      }
    }
    return price;
  }

  static UInt32 GetRepPrice(CLzmaEnc *p, UInt32 repIndex, UInt32 len, UInt32 state, UInt32 posState)
  {
    return p->repLenEnc.prices[posState][len - LZMA_MATCH_LEN_MIN] +
          GetPureRepPrice(p, repIndex, state, posState);
  }

  static UInt32 Backward(CLzmaEnc *p, UInt32 *backRes, UInt32 cur)
  {
    UInt32 posMem = p->opt[cur].posPrev;
    UInt32 backMem = p->opt[cur].backPrev;
    p->optimumEndIndex = cur;
    do
    {
      if (p->opt[cur].prev1IsChar)
      {
        MakeAsChar(&p->opt[posMem])
            p->opt[posMem]
                .posPrev = posMem - 1;
        if (p->opt[cur].prev2)
        {
          p->opt[posMem - 1].prev1IsChar = False;
          p->opt[posMem - 1].posPrev = p->opt[cur].posPrev2;
          p->opt[posMem - 1].backPrev = p->opt[cur].backPrev2;
        }
      }
      {
        UInt32 posPrev = posMem;
        UInt32 backCur = backMem;

        backMem = p->opt[posPrev].backPrev;
        posMem = p->opt[posPrev].posPrev;

        p->opt[posPrev].backPrev = backCur;
        p->opt[posPrev].posPrev = cur;
        cur = posPrev;
      }
    } while (cur != 0);
    *backRes = p->opt[0].backPrev;
    p->optimumCurrentIndex = p->opt[0].posPrev;
    return p->optimumCurrentIndex;
  }

  #define LIT_PROBS(pos, prevByte) (p->litProbs + ((((pos)&p->lpMask) << p->lc) + ((prevByte) >> (8 - p->lc))) * 0x300)

  static UInt32 GetOptimum(CLzmaEnc *p, UInt32 position, UInt32 *backRes)
  {
    UInt32 numAvail, mainLen, numPairs, repMaxIndex, i, posState, lenEnd, len, cur;
    UInt32 matchPrice, repMatchPrice, normalMatchPrice;
    UInt32 reps[LZMA_NUM_REPS], repLens[LZMA_NUM_REPS];
    UInt32 *matches;
    const Byte *data;
    Byte curByte, matchByte;
    if (p->optimumEndIndex != p->optimumCurrentIndex)
    {
      const COptimal *opt = &p->opt[p->optimumCurrentIndex];
      UInt32 lenRes = opt->posPrev - p->optimumCurrentIndex;
      *backRes = opt->backPrev;
      p->optimumCurrentIndex = opt->posPrev;
      return lenRes;
    }
    p->optimumCurrentIndex = p->optimumEndIndex = 0;

    if (p->additionalOffset == 0)
      mainLen = ReadMatchDistances(p, &numPairs);
    else
    {
      mainLen = p->longestMatchLength;
      numPairs = p->numPairs;
    }

    numAvail = p->numAvail;
    if (numAvail < 2)
    {
      *backRes = (UInt32)(-1);
      return 1;
    }
    if (numAvail > LZMA_MATCH_LEN_MAX)
      numAvail = LZMA_MATCH_LEN_MAX;

    data = p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - 1;
    repMaxIndex = 0;
    for (i = 0; i < LZMA_NUM_REPS; i++)
    {
      UInt32 lenTest;
      const Byte *data2;
      reps[i] = p->reps[i];
      data2 = data - (reps[i] + 1);
      if (data[0] != data2[0] || data[1] != data2[1])
      {
        repLens[i] = 0;
        continue;
      }
      for (lenTest = 2; lenTest < numAvail && data[lenTest] == data2[lenTest]; lenTest++)
        ;
      repLens[i] = lenTest;
      if (lenTest > repLens[repMaxIndex])
        repMaxIndex = i;
    }
    if (repLens[repMaxIndex] >= p->numFastBytes)
    {
      UInt32 lenRes;
      *backRes = repMaxIndex;
      lenRes = repLens[repMaxIndex];
      MovePos(p, lenRes - 1);
      return lenRes;
    }

    matches = p->matches;
    if (mainLen >= p->numFastBytes)
    {
      *backRes = matches[numPairs - 1] + LZMA_NUM_REPS;
      MovePos(p, mainLen - 1);
      return mainLen;
    }
    curByte = *data;
    matchByte = *(data - (reps[0] + 1));

    if (mainLen < 2 && curByte != matchByte && repLens[repMaxIndex] < 2)
    {
      *backRes = (UInt32)-1;
      return 1;
    }

    p->opt[0].state = (CState)p->state;

    posState = (position & p->pbMask);

    {
      const CLzmaProb *probs = LIT_PROBS(position, *(data - 1));
      p->opt[1].price = GET_PRICE_0(p->isMatch[p->state][posState]) +
                        (!IsCharState(p->state) ? LitEnc_GetPriceMatched(probs, curByte, matchByte, p->ProbPrices) : LitEnc_GetPrice(probs, curByte, p->ProbPrices));
    }

    MakeAsChar(&p->opt[1]);

    matchPrice = GET_PRICE_1(p->isMatch[p->state][posState]);
    repMatchPrice = matchPrice + GET_PRICE_1(p->isRep[p->state]);

    if (matchByte == curByte)
    {
      UInt32 shortRepPrice = repMatchPrice + GetRepLen1Price(p, p->state, posState);
      if (shortRepPrice < p->opt[1].price)
      {
        p->opt[1].price = shortRepPrice;
        MakeAsShortRep(&p->opt[1]);
      }
    }
    lenEnd = ((mainLen >= repLens[repMaxIndex]) ? mainLen : repLens[repMaxIndex]);

    if (lenEnd < 2)
    {
      *backRes = p->opt[1].backPrev;
      return 1;
    }

    p->opt[1].posPrev = 0;
    for (i = 0; i < LZMA_NUM_REPS; i++)
      p->opt[0].backs[i] = reps[i];

    len = lenEnd;
    do
      p->opt[len--].price = kInfinityPrice;
    while (len >= 2);

    for (i = 0; i < LZMA_NUM_REPS; i++)
    {
      UInt32 repLen = repLens[i];
      UInt32 price;
      if (repLen < 2)
        continue;
      price = repMatchPrice + GetPureRepPrice(p, i, p->state, posState);
      do
      {
        UInt32 curAndLenPrice = price + p->repLenEnc.prices[posState][repLen - 2];
        COptimal *opt = &p->opt[repLen];
        if (curAndLenPrice < opt->price)
        {
          opt->price = curAndLenPrice;
          opt->posPrev = 0;
          opt->backPrev = i;
          opt->prev1IsChar = False;
        }
      } while (--repLen >= 2);
    }

    normalMatchPrice = matchPrice + GET_PRICE_0(p->isRep[p->state]);

    len = ((repLens[0] >= 2) ? repLens[0] + 1 : 2);
    if (len <= mainLen)
    {
      UInt32 offs = 0;
      while (len > matches[offs])
        offs += 2;
      for (;; len++)
      {
        COptimal *opt;
        UInt32 distance = matches[offs + 1];

        UInt32 curAndLenPrice = normalMatchPrice + p->lenEnc.prices[posState][len - LZMA_MATCH_LEN_MIN];
        UInt32 lenToPosState = GetLenToPosState(len);
        if (distance < kNumFullDistances)
          curAndLenPrice += p->distancesPrices[lenToPosState][distance];
        else
        {
          UInt32 slot;
          GetPosSlot2(distance, slot);
          curAndLenPrice += p->alignPrices[distance & kAlignMask] + p->posSlotPrices[lenToPosState][slot];
        }
        opt = &p->opt[len];
        if (curAndLenPrice < opt->price)
        {
          opt->price = curAndLenPrice;
          opt->posPrev = 0;
          opt->backPrev = distance + LZMA_NUM_REPS;
          opt->prev1IsChar = False;
        }
        if (len == matches[offs])
        {
          offs += 2;
          if (offs == numPairs)
            break;
        }
      }
    }

    cur = 0;

  #ifdef SHOW_STAT2
    if (position >= 0)
    {
      unsigned i;
      printf("\n pos = %4X", position);
      for (i = cur; i <= lenEnd; i++)
        printf("\nprice[%4X] = %d", position - cur + i, p->opt[i].price);
    }
  #endif

    for (;;)
    {
      UInt32 numAvailFull, newLen, numPairs, posPrev, state, posState, startLen;
      UInt32 curPrice, curAnd1Price, matchPrice, repMatchPrice;
      Bool nextIsChar;
      Byte curByte, matchByte;
      const Byte *data;
      COptimal *curOpt;
      COptimal *nextOpt;

      cur++;
      if (cur == lenEnd)
        return Backward(p, backRes, cur);

      newLen = ReadMatchDistances(p, &numPairs);
      if (newLen >= p->numFastBytes)
      {
        p->numPairs = numPairs;
        p->longestMatchLength = newLen;
        return Backward(p, backRes, cur);
      }
      position++;
      curOpt = &p->opt[cur];
      posPrev = curOpt->posPrev;
      if (curOpt->prev1IsChar)
      {
        posPrev--;
        if (curOpt->prev2)
        {
          state = p->opt[curOpt->posPrev2].state;
          if (curOpt->backPrev2 < LZMA_NUM_REPS)
            state = kRepNextStates[state];
          else
            state = kMatchNextStates[state];
        }
        else
          state = p->opt[posPrev].state;
        state = kLiteralNextStates[state];
      }
      else
        state = p->opt[posPrev].state;
      if (posPrev == cur - 1)
      {
        if (IsShortRep(curOpt))
          state = kShortRepNextStates[state];
        else
          state = kLiteralNextStates[state];
      }
      else
      {
        UInt32 pos;
        const COptimal *prevOpt;
        if (curOpt->prev1IsChar && curOpt->prev2)
        {
          posPrev = curOpt->posPrev2;
          pos = curOpt->backPrev2;
          state = kRepNextStates[state];
        }
        else
        {
          pos = curOpt->backPrev;
          if (pos < LZMA_NUM_REPS)
            state = kRepNextStates[state];
          else
            state = kMatchNextStates[state];
        }
        prevOpt = &p->opt[posPrev];
        if (pos < LZMA_NUM_REPS)
        {
          UInt32 i;
          reps[0] = prevOpt->backs[pos];
          for (i = 1; i <= pos; i++)
            reps[i] = prevOpt->backs[i - 1];
          for (; i < LZMA_NUM_REPS; i++)
            reps[i] = prevOpt->backs[i];
        }
        else
        {
          UInt32 i;
          reps[0] = (pos - LZMA_NUM_REPS);
          for (i = 1; i < LZMA_NUM_REPS; i++)
            reps[i] = prevOpt->backs[i - 1];
        }
      }
      curOpt->state = (CState)state;

      curOpt->backs[0] = reps[0];
      curOpt->backs[1] = reps[1];
      curOpt->backs[2] = reps[2];
      curOpt->backs[3] = reps[3];

      curPrice = curOpt->price;
      nextIsChar = False;
      data = p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - 1;
      curByte = *data;
      matchByte = *(data - (reps[0] + 1));

      posState = (position & p->pbMask);

      curAnd1Price = curPrice + GET_PRICE_0(p->isMatch[state][posState]);
      {
        const CLzmaProb *probs = LIT_PROBS(position, *(data - 1));
        curAnd1Price +=
            (!IsCharState(state) ? LitEnc_GetPriceMatched(probs, curByte, matchByte, p->ProbPrices) : LitEnc_GetPrice(probs, curByte, p->ProbPrices));
      }

      nextOpt = &p->opt[cur + 1];

      if (curAnd1Price < nextOpt->price)
      {
        nextOpt->price = curAnd1Price;
        nextOpt->posPrev = cur;
        MakeAsChar(nextOpt);
        nextIsChar = True;
      }

      matchPrice = curPrice + GET_PRICE_1(p->isMatch[state][posState]);
      repMatchPrice = matchPrice + GET_PRICE_1(p->isRep[state]);

      if (matchByte == curByte && !(nextOpt->posPrev < cur && nextOpt->backPrev == 0))
      {
        UInt32 shortRepPrice = repMatchPrice + GetRepLen1Price(p, state, posState);
        if (shortRepPrice <= nextOpt->price)
        {
          nextOpt->price = shortRepPrice;
          nextOpt->posPrev = cur;
          MakeAsShortRep(nextOpt);
          nextIsChar = True;
        }
      }
      numAvailFull = p->numAvail;
      {
        UInt32 temp = kNumOpts - 1 - cur;
        if (temp < numAvailFull)
          numAvailFull = temp;
      }

      if (numAvailFull < 2)
        continue;
      numAvail = (numAvailFull <= p->numFastBytes ? numAvailFull : p->numFastBytes);

      if (!nextIsChar && matchByte != curByte) /* speed optimization */
      {
        /* try Literal + rep0 */
        UInt32 temp;
        UInt32 lenTest2;
        const Byte *data2 = data - (reps[0] + 1);
        UInt32 limit = p->numFastBytes + 1;
        if (limit > numAvailFull)
          limit = numAvailFull;

        for (temp = 1; temp < limit && data[temp] == data2[temp]; temp++)
          ;
        lenTest2 = temp - 1;
        if (lenTest2 >= 2)
        {
          UInt32 state2 = kLiteralNextStates[state];
          UInt32 posStateNext = (position + 1) & p->pbMask;
          UInt32 nextRepMatchPrice = curAnd1Price +
                                    GET_PRICE_1(p->isMatch[state2][posStateNext]) +
                                    GET_PRICE_1(p->isRep[state2]);
          /* for (; lenTest2 >= 2; lenTest2--) */
          {
            UInt32 curAndLenPrice;
            COptimal *opt;
            UInt32 offset = cur + 1 + lenTest2;
            while (lenEnd < offset)
              p->opt[++lenEnd].price = kInfinityPrice;
            curAndLenPrice = nextRepMatchPrice + GetRepPrice(p, 0, lenTest2, state2, posStateNext);
            opt = &p->opt[offset];
            if (curAndLenPrice < opt->price)
            {
              opt->price = curAndLenPrice;
              opt->posPrev = cur + 1;
              opt->backPrev = 0;
              opt->prev1IsChar = True;
              opt->prev2 = False;
            }
          }
        }
      }

      startLen = 2; /* speed optimization */
      {
        UInt32 repIndex;
        for (repIndex = 0; repIndex < LZMA_NUM_REPS; repIndex++)
        {
          UInt32 lenTest;
          UInt32 lenTestTemp;
          UInt32 price;
          const Byte *data2 = data - (reps[repIndex] + 1);
          if (data[0] != data2[0] || data[1] != data2[1])
            continue;
          for (lenTest = 2; lenTest < numAvail && data[lenTest] == data2[lenTest]; lenTest++)
            ;
          while (lenEnd < cur + lenTest)
            p->opt[++lenEnd].price = kInfinityPrice;
          lenTestTemp = lenTest;
          price = repMatchPrice + GetPureRepPrice(p, repIndex, state, posState);
          do
          {
            UInt32 curAndLenPrice = price + p->repLenEnc.prices[posState][lenTest - 2];
            COptimal *opt = &p->opt[cur + lenTest];
            if (curAndLenPrice < opt->price)
            {
              opt->price = curAndLenPrice;
              opt->posPrev = cur;
              opt->backPrev = repIndex;
              opt->prev1IsChar = False;
            }
          } while (--lenTest >= 2);
          lenTest = lenTestTemp;

          if (repIndex == 0)
            startLen = lenTest + 1;

          /* if (_maxMode) */
          {
            UInt32 lenTest2 = lenTest + 1;
            UInt32 limit = lenTest2 + p->numFastBytes;
            UInt32 nextRepMatchPrice;
            if (limit > numAvailFull)
              limit = numAvailFull;
            for (; lenTest2 < limit && data[lenTest2] == data2[lenTest2]; lenTest2++)
              ;
            lenTest2 -= lenTest + 1;
            if (lenTest2 >= 2)
            {
              UInt32 state2 = kRepNextStates[state];
              UInt32 posStateNext = (position + lenTest) & p->pbMask;
              UInt32 curAndLenCharPrice =
                  price + p->repLenEnc.prices[posState][lenTest - 2] +
                  GET_PRICE_0(p->isMatch[state2][posStateNext]) +
                  LitEnc_GetPriceMatched(LIT_PROBS(position + lenTest, data[lenTest - 1]),
                                        data[lenTest], data2[lenTest], p->ProbPrices);
              state2 = kLiteralNextStates[state2];
              posStateNext = (position + lenTest + 1) & p->pbMask;
              nextRepMatchPrice = curAndLenCharPrice +
                                  GET_PRICE_1(p->isMatch[state2][posStateNext]) +
                                  GET_PRICE_1(p->isRep[state2]);

              /* for (; lenTest2 >= 2; lenTest2--) */
              {
                UInt32 curAndLenPrice;
                COptimal *opt;
                UInt32 offset = cur + lenTest + 1 + lenTest2;
                while (lenEnd < offset)
                  p->opt[++lenEnd].price = kInfinityPrice;
                curAndLenPrice = nextRepMatchPrice + GetRepPrice(p, 0, lenTest2, state2, posStateNext);
                opt = &p->opt[offset];
                if (curAndLenPrice < opt->price)
                {
                  opt->price = curAndLenPrice;
                  opt->posPrev = cur + lenTest + 1;
                  opt->backPrev = 0;
                  opt->prev1IsChar = True;
                  opt->prev2 = True;
                  opt->posPrev2 = cur;
                  opt->backPrev2 = repIndex;
                }
              }
            }
          }
        }
      }
      /* for (UInt32 lenTest = 2; lenTest <= newLen; lenTest++) */
      if (newLen > numAvail)
      {
        newLen = numAvail;
        for (numPairs = 0; newLen > matches[numPairs]; numPairs += 2)
          ;
        matches[numPairs] = newLen;
        numPairs += 2;
      }
      if (newLen >= startLen)
      {
        UInt32 normalMatchPrice = matchPrice + GET_PRICE_0(p->isRep[state]);
        UInt32 offs, curBack, posSlot;
        UInt32 lenTest;
        while (lenEnd < cur + newLen)
          p->opt[++lenEnd].price = kInfinityPrice;

        offs = 0;
        while (startLen > matches[offs])
          offs += 2;
        curBack = matches[offs + 1];
        GetPosSlot2(curBack, posSlot);
        for (lenTest = /*2*/ startLen;; lenTest++)
        {
          UInt32 curAndLenPrice = normalMatchPrice + p->lenEnc.prices[posState][lenTest - LZMA_MATCH_LEN_MIN];
          UInt32 lenToPosState = GetLenToPosState(lenTest);
          COptimal *opt;
          if (curBack < kNumFullDistances)
            curAndLenPrice += p->distancesPrices[lenToPosState][curBack];
          else
            curAndLenPrice += p->posSlotPrices[lenToPosState][posSlot] + p->alignPrices[curBack & kAlignMask];

          opt = &p->opt[cur + lenTest];
          if (curAndLenPrice < opt->price)
          {
            opt->price = curAndLenPrice;
            opt->posPrev = cur;
            opt->backPrev = curBack + LZMA_NUM_REPS;
            opt->prev1IsChar = False;
          }

          if (/*_maxMode && */ lenTest == matches[offs])
          {
            /* Try Match + Literal + Rep0 */
            const Byte *data2 = data - (curBack + 1);
            UInt32 lenTest2 = lenTest + 1;
            UInt32 limit = lenTest2 + p->numFastBytes;
            UInt32 nextRepMatchPrice;
            if (limit > numAvailFull)
              limit = numAvailFull;
            for (; lenTest2 < limit && data[lenTest2] == data2[lenTest2]; lenTest2++)
              ;
            lenTest2 -= lenTest + 1;
            if (lenTest2 >= 2)
            {
              UInt32 state2 = kMatchNextStates[state];
              UInt32 posStateNext = (position + lenTest) & p->pbMask;
              UInt32 curAndLenCharPrice = curAndLenPrice +
                                          GET_PRICE_0(p->isMatch[state2][posStateNext]) +
                                          LitEnc_GetPriceMatched(LIT_PROBS(position + lenTest, data[lenTest - 1]),
                                                                data[lenTest], data2[lenTest], p->ProbPrices);
              state2 = kLiteralNextStates[state2];
              posStateNext = (posStateNext + 1) & p->pbMask;
              nextRepMatchPrice = curAndLenCharPrice +
                                  GET_PRICE_1(p->isMatch[state2][posStateNext]) +
                                  GET_PRICE_1(p->isRep[state2]);

              /* for (; lenTest2 >= 2; lenTest2--) */
              {
                UInt32 offset = cur + lenTest + 1 + lenTest2;
                UInt32 curAndLenPrice;
                COptimal *opt;
                while (lenEnd < offset)
                  p->opt[++lenEnd].price = kInfinityPrice;
                curAndLenPrice = nextRepMatchPrice + GetRepPrice(p, 0, lenTest2, state2, posStateNext);
                opt = &p->opt[offset];
                if (curAndLenPrice < opt->price)
                {
                  opt->price = curAndLenPrice;
                  opt->posPrev = cur + lenTest + 1;
                  opt->backPrev = 0;
                  opt->prev1IsChar = True;
                  opt->prev2 = True;
                  opt->posPrev2 = cur;
                  opt->backPrev2 = curBack + LZMA_NUM_REPS;
                }
              }
            }
            offs += 2;
            if (offs == numPairs)
              break;
            curBack = matches[offs + 1];
            if (curBack >= kNumFullDistances)
              GetPosSlot2(curBack, posSlot);
          }
        }
      }
    }
  }

  #define ChangePair(smallDist, bigDist) (((bigDist) >> 7) > (smallDist))

  static UInt32 GetOptimumFast(CLzmaEnc *p, UInt32 *backRes)
  {
    UInt32 numAvail, mainLen, mainDist, numPairs, repIndex, repLen, i;
    const Byte *data;
    const UInt32 *matches;

    if (p->additionalOffset == 0)
      mainLen = ReadMatchDistances(p, &numPairs);
    else
    {
      mainLen = p->longestMatchLength;
      numPairs = p->numPairs;
    }

    numAvail = p->numAvail;
    *backRes = (UInt32)-1;
    if (numAvail < 2)
      return 1;
    if (numAvail > LZMA_MATCH_LEN_MAX)
      numAvail = LZMA_MATCH_LEN_MAX;
    data = p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - 1;

    repLen = repIndex = 0;
    for (i = 0; i < LZMA_NUM_REPS; i++)
    {
      UInt32 len;
      const Byte *data2 = data - (p->reps[i] + 1);
      if (data[0] != data2[0] || data[1] != data2[1])
        continue;
      for (len = 2; len < numAvail && data[len] == data2[len]; len++)
        ;
      if (len >= p->numFastBytes)
      {
        *backRes = i;
        MovePos(p, len - 1);
        return len;
      }
      if (len > repLen)
      {
        repIndex = i;
        repLen = len;
      }
    }

    matches = p->matches;
    if (mainLen >= p->numFastBytes)
    {
      *backRes = matches[numPairs - 1] + LZMA_NUM_REPS;
      MovePos(p, mainLen - 1);
      return mainLen;
    }

    mainDist = 0; /* for GCC */
    if (mainLen >= 2)
    {
      mainDist = matches[numPairs - 1];
      while (numPairs > 2 && mainLen == matches[numPairs - 4] + 1)
      {
        if (!ChangePair(matches[numPairs - 3], mainDist))
          break;
        numPairs -= 2;
        mainLen = matches[numPairs - 2];
        mainDist = matches[numPairs - 1];
      }
      if (mainLen == 2 && mainDist >= 0x80)
        mainLen = 1;
    }

    if (repLen >= 2 && ((repLen + 1 >= mainLen) ||
                        (repLen + 2 >= mainLen && mainDist >= (1 << 9)) ||
                        (repLen + 3 >= mainLen && mainDist >= (1 << 15))))
    {
      *backRes = repIndex;
      MovePos(p, repLen - 1);
      return repLen;
    }

    if (mainLen < 2 || numAvail <= 2)
      return 1;

    p->longestMatchLength = ReadMatchDistances(p, &p->numPairs);
    if (p->longestMatchLength >= 2)
    {
      UInt32 newDistance = matches[p->numPairs - 1];
      if ((p->longestMatchLength >= mainLen && newDistance < mainDist) ||
          (p->longestMatchLength == mainLen + 1 && !ChangePair(mainDist, newDistance)) ||
          (p->longestMatchLength > mainLen + 1) ||
          (p->longestMatchLength + 1 >= mainLen && mainLen >= 3 && ChangePair(newDistance, mainDist)))
        return 1;
    }

    data = p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - 1;
    for (i = 0; i < LZMA_NUM_REPS; i++)
    {
      UInt32 len, limit;
      const Byte *data2 = data - (p->reps[i] + 1);
      if (data[0] != data2[0] || data[1] != data2[1])
        continue;
      limit = mainLen - 1;
      for (len = 2; len < limit && data[len] == data2[len]; len++)
        ;
      if (len >= limit)
        return 1;
    }
    *backRes = mainDist + LZMA_NUM_REPS;
    MovePos(p, mainLen - 2);
    return mainLen;
  }

  static void WriteEndMarker(CLzmaEnc *p, UInt32 posState)
  {
    UInt32 len;
    RangeEnc_EncodeBit(&p->rc, &p->isMatch[p->state][posState], 1);
    RangeEnc_EncodeBit(&p->rc, &p->isRep[p->state], 0);
    p->state = kMatchNextStates[p->state];
    len = LZMA_MATCH_LEN_MIN;
    LenEnc_Encode2(&p->lenEnc, &p->rc, len - LZMA_MATCH_LEN_MIN, posState, !p->fastMode, p->ProbPrices);
    RcTree_Encode(&p->rc, p->posSlotEncoder[GetLenToPosState(len)], kNumPosSlotBits, (1 << kNumPosSlotBits) - 1);
    RangeEnc_EncodeDirectBits(&p->rc, (((UInt32)1 << 30) - 1) >> kNumAlignBits, 30 - kNumAlignBits);
    RcTree_ReverseEncode(&p->rc, p->posAlignEncoder, kNumAlignBits, kAlignMask);
  }

  static SRes CheckErrors(CLzmaEnc *p)
  {
    if (p->result != SZ_OK)
      return p->result;
    if (p->rc.res != SZ_OK)
      p->result = SZ_ERROR_WRITE;
    if (p->matchFinderBase.result != SZ_OK)
      p->result = SZ_ERROR_READ;
    if (p->result != SZ_OK)
      p->finished = True;
    return p->result;
  }

  static SRes Flush(CLzmaEnc *p, UInt32 nowPos)
  {
    /* ReleaseMFStream(); */
    p->finished = True;
    if (p->writeEndMark)
      WriteEndMarker(p, nowPos & p->pbMask);
    RangeEnc_FlushData(&p->rc);
    RangeEnc_FlushStream(&p->rc);
    return CheckErrors(p);
  }

  static void FillAlignPrices(CLzmaEnc *p)
  {
    UInt32 i;
    for (i = 0; i < kAlignTableSize; i++)
      p->alignPrices[i] = RcTree_ReverseGetPrice(p->posAlignEncoder, kNumAlignBits, i, p->ProbPrices);
    p->alignPriceCount = 0;
  }

  static void FillDistancesPrices(CLzmaEnc *p)
  {
    UInt32 tempPrices[kNumFullDistances];
    UInt32 i, lenToPosState;
    for (i = kStartPosModelIndex; i < kNumFullDistances; i++)
    {
      UInt32 posSlot = GetPosSlot1(i);
      UInt32 footerBits = ((posSlot >> 1) - 1);
      UInt32 base = ((2 | (posSlot & 1)) << footerBits);
      tempPrices[i] = RcTree_ReverseGetPrice(p->posEncoders + base - posSlot - 1, footerBits, i - base, p->ProbPrices);
    }

    for (lenToPosState = 0; lenToPosState < kNumLenToPosStates; lenToPosState++)
    {
      UInt32 posSlot;
      const CLzmaProb *encoder = p->posSlotEncoder[lenToPosState];
      UInt32 *posSlotPrices = p->posSlotPrices[lenToPosState];
      for (posSlot = 0; posSlot < p->distTableSize; posSlot++)
        posSlotPrices[posSlot] = RcTree_GetPrice(encoder, kNumPosSlotBits, posSlot, p->ProbPrices);
      for (posSlot = kEndPosModelIndex; posSlot < p->distTableSize; posSlot++)
        posSlotPrices[posSlot] += ((((posSlot >> 1) - 1) - kNumAlignBits) << kNumBitPriceShiftBits);

      {
        UInt32 *distancesPrices = p->distancesPrices[lenToPosState];
        UInt32 i;
        for (i = 0; i < kStartPosModelIndex; i++)
          distancesPrices[i] = posSlotPrices[i];
        for (; i < kNumFullDistances; i++)
          distancesPrices[i] = posSlotPrices[GetPosSlot1(i)] + tempPrices[i];
      }
    }
    p->matchPriceCount = 0;
  }

  void LzmaEnc_Construct(CLzmaEnc *p)
  {
    RangeEnc_Construct(&p->rc);
    MatchFinder_Construct(&p->matchFinderBase);
  #ifdef COMPRESS_MF_MT
    MatchFinderMt_Construct(&p->matchFinderMt);
    p->matchFinderMt.MatchFinder = &p->matchFinderBase;
  #endif

    {
      CLzmaEncProps props;
      LzmaEncProps_Init(&props);
      LzmaEnc_SetProps(p, &props);
    }

  #ifndef LZMA_LOG_BSR
    LzmaEnc_FastPosInit(p->g_FastPos);
  #endif

    LzmaEnc_InitPriceTables(p->ProbPrices);
    p->litProbs = 0;
    p->saveState.litProbs = 0;
  }

  CLzmaEncHandle LzmaEnc_Create(ISzAlloc *alloc)
  {
    void *p;
    p = alloc->Alloc(alloc, sizeof(CLzmaEnc));
    if (p != 0)
      LzmaEnc_Construct((CLzmaEnc *)p);
    return p;
  }

  void LzmaEnc_FreeLits(CLzmaEnc *p, ISzAlloc *alloc)
  {
    alloc->Free(alloc, p->litProbs);
    alloc->Free(alloc, p->saveState.litProbs);
    p->litProbs = 0;
    p->saveState.litProbs = 0;
  }

  void LzmaEnc_Destruct(CLzmaEnc *p, ISzAlloc *alloc, ISzAlloc *allocBig)
  {
  #ifdef COMPRESS_MF_MT
    MatchFinderMt_Destruct(&p->matchFinderMt, allocBig);
  #endif
    MatchFinder_Free(&p->matchFinderBase, allocBig);
    LzmaEnc_FreeLits(p, alloc);
    RangeEnc_Free(&p->rc, alloc);
  }

  void LzmaEnc_Destroy(CLzmaEncHandle p, ISzAlloc *alloc, ISzAlloc *allocBig)
  {
    LzmaEnc_Destruct((CLzmaEnc *)p, alloc, allocBig);
    alloc->Free(alloc, p);
  }

  static SRes LzmaEnc_CodeOneBlock(CLzmaEnc *p, Bool useLimits, UInt32 maxPackSize, UInt32 maxUnpackSize)
  {
    UInt32 nowPos32, startPos32;
    if (p->inStream != 0)
    {
      p->matchFinderBase.stream = p->inStream;
      p->matchFinder.Init(p->matchFinderObj);
      p->inStream = 0;
    }

    if (p->finished)
      return p->result;
    RINOK(CheckErrors(p));

    nowPos32 = (UInt32)p->nowPos64;
    startPos32 = nowPos32;

    if (p->nowPos64 == 0)
    {
      UInt32 numPairs;
      Byte curByte;
      if (p->matchFinder.GetNumAvailableBytes(p->matchFinderObj) == 0)
        return Flush(p, nowPos32);
      ReadMatchDistances(p, &numPairs);
      RangeEnc_EncodeBit(&p->rc, &p->isMatch[p->state][0], 0);
      p->state = kLiteralNextStates[p->state];
      curByte = p->matchFinder.GetIndexByte(p->matchFinderObj, 0 - p->additionalOffset);
      LitEnc_Encode(&p->rc, p->litProbs, curByte);
      p->additionalOffset--;
      nowPos32++;
    }

    if (p->matchFinder.GetNumAvailableBytes(p->matchFinderObj) != 0)
      for (;;)
      {
        UInt32 pos, len, posState;

        if (p->fastMode)
          len = GetOptimumFast(p, &pos);
        else
          len = GetOptimum(p, nowPos32, &pos);

  #ifdef SHOW_STAT2
        printf("\n pos = %4X,   len = %d   pos = %d", nowPos32, len, pos);
  #endif

        posState = nowPos32 & p->pbMask;
        if (len == 1 && pos == (UInt32)-1)
        {
          Byte curByte;
          CLzmaProb *probs;
          const Byte *data;

          RangeEnc_EncodeBit(&p->rc, &p->isMatch[p->state][posState], 0);
          data = p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - p->additionalOffset;
          curByte = *data;
          probs = LIT_PROBS(nowPos32, *(data - 1));
          if (IsCharState(p->state))
            LitEnc_Encode(&p->rc, probs, curByte);
          else
            LitEnc_EncodeMatched(&p->rc, probs, curByte, *(data - p->reps[0] - 1));
          p->state = kLiteralNextStates[p->state];
        }
        else
        {
          RangeEnc_EncodeBit(&p->rc, &p->isMatch[p->state][posState], 1);
          if (pos < LZMA_NUM_REPS)
          {
            RangeEnc_EncodeBit(&p->rc, &p->isRep[p->state], 1);
            if (pos == 0)
            {
              RangeEnc_EncodeBit(&p->rc, &p->isRepG0[p->state], 0);
              RangeEnc_EncodeBit(&p->rc, &p->isRep0Long[p->state][posState], ((len == 1) ? 0 : 1));
            }
            else
            {
              UInt32 distance = p->reps[pos];
              RangeEnc_EncodeBit(&p->rc, &p->isRepG0[p->state], 1);
              if (pos == 1)
                RangeEnc_EncodeBit(&p->rc, &p->isRepG1[p->state], 0);
              else
              {
                RangeEnc_EncodeBit(&p->rc, &p->isRepG1[p->state], 1);
                RangeEnc_EncodeBit(&p->rc, &p->isRepG2[p->state], pos - 2);
                if (pos == 3)
                  p->reps[3] = p->reps[2];
                p->reps[2] = p->reps[1];
              }
              p->reps[1] = p->reps[0];
              p->reps[0] = distance;
            }
            if (len == 1)
              p->state = kShortRepNextStates[p->state];
            else
            {
              LenEnc_Encode2(&p->repLenEnc, &p->rc, len - LZMA_MATCH_LEN_MIN, posState, !p->fastMode, p->ProbPrices);
              p->state = kRepNextStates[p->state];
            }
          }
          else
          {
            UInt32 posSlot;
            RangeEnc_EncodeBit(&p->rc, &p->isRep[p->state], 0);
            p->state = kMatchNextStates[p->state];
            LenEnc_Encode2(&p->lenEnc, &p->rc, len - LZMA_MATCH_LEN_MIN, posState, !p->fastMode, p->ProbPrices);
            pos -= LZMA_NUM_REPS;
            GetPosSlot(pos, posSlot);
            RcTree_Encode(&p->rc, p->posSlotEncoder[GetLenToPosState(len)], kNumPosSlotBits, posSlot);

            if (posSlot >= kStartPosModelIndex)
            {
              UInt32 footerBits = ((posSlot >> 1) - 1);
              UInt32 base = ((2 | (posSlot & 1)) << footerBits);
              UInt32 posReduced = pos - base;

              if (posSlot < kEndPosModelIndex)
                RcTree_ReverseEncode(&p->rc, p->posEncoders + base - posSlot - 1, footerBits, posReduced);
              else
              {
                RangeEnc_EncodeDirectBits(&p->rc, posReduced >> kNumAlignBits, footerBits - kNumAlignBits);
                RcTree_ReverseEncode(&p->rc, p->posAlignEncoder, kNumAlignBits, posReduced & kAlignMask);
                p->alignPriceCount++;
              }
            }
            p->reps[3] = p->reps[2];
            p->reps[2] = p->reps[1];
            p->reps[1] = p->reps[0];
            p->reps[0] = pos;
            p->matchPriceCount++;
          }
        }
        p->additionalOffset -= len;
        nowPos32 += len;
        if (p->additionalOffset == 0)
        {
          UInt32 processed;
          if (!p->fastMode)
          {
            if (p->matchPriceCount >= (1 << 7))
              FillDistancesPrices(p);
            if (p->alignPriceCount >= kAlignTableSize)
              FillAlignPrices(p);
          }
          if (p->matchFinder.GetNumAvailableBytes(p->matchFinderObj) == 0)
            break;
          processed = nowPos32 - startPos32;
          if (useLimits)
          {
            if (processed + kNumOpts + 300 >= maxUnpackSize ||
                RangeEnc_GetProcessed(&p->rc) + kNumOpts * 2 >= maxPackSize)
              break;
          }
          else if (processed >= (1 << 15))
          {
            p->nowPos64 += nowPos32 - startPos32;
            return CheckErrors(p);
          }
        }
      }
    p->nowPos64 += nowPos32 - startPos32;
    return Flush(p, nowPos32);
  }

  #define kBigHashDicLimit ((UInt32)1 << 24)

  static SRes LzmaEnc_Alloc(CLzmaEnc *p, UInt32 keepWindowSize, ISzAlloc *alloc, ISzAlloc *allocBig)
  {
    UInt32 beforeSize = kNumOpts;
    Bool btMode;
    if (!RangeEnc_Alloc(&p->rc, alloc))
      return SZ_ERROR_MEM;
    btMode = (p->matchFinderBase.btMode != 0);
  #ifdef COMPRESS_MF_MT
    p->mtMode = (p->multiThread && !p->fastMode && btMode);
  #endif

    {
      unsigned lclp = p->lc + p->lp;
      if (p->litProbs == 0 || p->saveState.litProbs == 0 || p->lclp != lclp)
      {
        LzmaEnc_FreeLits(p, alloc);
        p->litProbs = (CLzmaProb *)alloc->Alloc(alloc, (0x300 << lclp) * sizeof(CLzmaProb));
        p->saveState.litProbs = (CLzmaProb *)alloc->Alloc(alloc, (0x300 << lclp) * sizeof(CLzmaProb));
        if (p->litProbs == 0 || p->saveState.litProbs == 0)
        {
          LzmaEnc_FreeLits(p, alloc);
          return SZ_ERROR_MEM;
        }
        p->lclp = lclp;
      }
    }

    p->matchFinderBase.bigHash = (p->dictSize > kBigHashDicLimit);

    if (beforeSize + p->dictSize < keepWindowSize)
      beforeSize = keepWindowSize - p->dictSize;

  #ifdef COMPRESS_MF_MT
    if (p->mtMode)
    {
      RINOK(MatchFinderMt_Create(&p->matchFinderMt, p->dictSize, beforeSize, p->numFastBytes, LZMA_MATCH_LEN_MAX, allocBig));
      p->matchFinderObj = &p->matchFinderMt;
      MatchFinderMt_CreateVTable(&p->matchFinderMt, &p->matchFinder);
    }
    else
  #endif
    {
      if (!MatchFinder_Create(&p->matchFinderBase, p->dictSize, beforeSize, p->numFastBytes, LZMA_MATCH_LEN_MAX, allocBig))
        return SZ_ERROR_MEM;
      p->matchFinderObj = &p->matchFinderBase;
      MatchFinder_CreateVTable(&p->matchFinderBase, &p->matchFinder);
    }
    return SZ_OK;
  }

  void LzmaEnc_Init(CLzmaEnc *p)
  {
    UInt32 i;
    p->state = 0;
    for (i = 0; i < LZMA_NUM_REPS; i++)
      p->reps[i] = 0;

    RangeEnc_Init(&p->rc);

    for (i = 0; i < kNumStates; i++)
    {
      UInt32 j;
      for (j = 0; j < LZMA_NUM_PB_STATES_MAX; j++)
      {
        p->isMatch[i][j] = kProbInitValue;
        p->isRep0Long[i][j] = kProbInitValue;
      }
      p->isRep[i] = kProbInitValue;
      p->isRepG0[i] = kProbInitValue;
      p->isRepG1[i] = kProbInitValue;
      p->isRepG2[i] = kProbInitValue;
    }

    {
      UInt32 num = 0x300 << (p->lp + p->lc);
      for (i = 0; i < num; i++)
        p->litProbs[i] = kProbInitValue;
    }

    {
      for (i = 0; i < kNumLenToPosStates; i++)
      {
        CLzmaProb *probs = p->posSlotEncoder[i];
        UInt32 j;
        for (j = 0; j < (1 << kNumPosSlotBits); j++)
          probs[j] = kProbInitValue;
      }
    }
    {
      for (i = 0; i < kNumFullDistances - kEndPosModelIndex; i++)
        p->posEncoders[i] = kProbInitValue;
    }

    LenEnc_Init(&p->lenEnc.p);
    LenEnc_Init(&p->repLenEnc.p);

    for (i = 0; i < (1 << kNumAlignBits); i++)
      p->posAlignEncoder[i] = kProbInitValue;

    p->optimumEndIndex = 0;
    p->optimumCurrentIndex = 0;
    p->additionalOffset = 0;

    p->pbMask = (1 << p->pb) - 1;
    p->lpMask = (1 << p->lp) - 1;
  }

  void LzmaEnc_InitPrices(CLzmaEnc *p)
  {
    if (!p->fastMode)
    {
      FillDistancesPrices(p);
      FillAlignPrices(p);
    }

    p->lenEnc.tableSize =
        p->repLenEnc.tableSize =
            p->numFastBytes + 1 - LZMA_MATCH_LEN_MIN;
    LenPriceEnc_UpdateTables(&p->lenEnc, 1 << p->pb, p->ProbPrices);
    LenPriceEnc_UpdateTables(&p->repLenEnc, 1 << p->pb, p->ProbPrices);
  }

  static SRes LzmaEnc_AllocAndInit(CLzmaEnc *p, UInt32 keepWindowSize, ISzAlloc *alloc, ISzAlloc *allocBig)
  {
    UInt32 i;
    for (i = 0; i < (UInt32)kDicLogSizeMaxCompress; i++)
      if (p->dictSize <= ((UInt32)1 << i))
        break;
    p->distTableSize = i * 2;

    p->finished = False;
    p->result = SZ_OK;
    RINOK(LzmaEnc_Alloc(p, keepWindowSize, alloc, allocBig));
    LzmaEnc_Init(p);
    LzmaEnc_InitPrices(p);
    p->nowPos64 = 0;
    return SZ_OK;
  }

  static SRes LzmaEnc_Prepare(CLzmaEncHandle pp, ISeqInStream *inStream, ISeqOutStream *outStream,
                              ISzAlloc *alloc, ISzAlloc *allocBig)
  {
    CLzmaEnc *p = (CLzmaEnc *)pp;
    p->inStream = inStream;
    p->rc.outStream = outStream;
    return LzmaEnc_AllocAndInit(p, 0, alloc, allocBig);
  }

  SRes LzmaEnc_PrepareForLzma2(CLzmaEncHandle pp,
                              ISeqInStream *inStream, UInt32 keepWindowSize,
                              ISzAlloc *alloc, ISzAlloc *allocBig)
  {
    CLzmaEnc *p = (CLzmaEnc *)pp;
    p->inStream = inStream;
    return LzmaEnc_AllocAndInit(p, keepWindowSize, alloc, allocBig);
  }

  static void LzmaEnc_SetInputBuf(CLzmaEnc *p, const Byte *src, SizeT srcLen)
  {
    p->seqBufInStream.funcTable.Read = MyRead;
    p->seqBufInStream.data = src;
    p->seqBufInStream.rem = srcLen;
  }

  SRes LzmaEnc_MemPrepare(CLzmaEncHandle pp, const Byte *src, SizeT srcLen,
                          UInt32 keepWindowSize, ISzAlloc *alloc, ISzAlloc *allocBig)
  {
    CLzmaEnc *p = (CLzmaEnc *)pp;
    LzmaEnc_SetInputBuf(p, src, srcLen);
    p->inStream = &p->seqBufInStream.funcTable;
    return LzmaEnc_AllocAndInit(p, keepWindowSize, alloc, allocBig);
  }

  void LzmaEnc_Finish(CLzmaEncHandle pp)
  {
  #ifdef COMPRESS_MF_MT
    CLzmaEnc *p = (CLzmaEnc *)pp;
    if (p->mtMode)
      MatchFinderMt_ReleaseStream(&p->matchFinderMt);
  #else
    pp = pp;
  #endif
  }

  typedef struct _CSeqOutStreamBuf
  {
    ISeqOutStream funcTable;
    Byte *data;
    SizeT rem;
    Bool overflow;
  } CSeqOutStreamBuf;

  static size_t MyWrite(void *pp, const void *data, size_t size)
  {
    CSeqOutStreamBuf *p = (CSeqOutStreamBuf *)pp;
    if (p->rem < size)
    {
      size = p->rem;
      p->overflow = True;
    }
    memcpy(p->data, data, size);
    p->rem -= size;
    p->data += size;
    return size;
  }

  UInt32 LzmaEnc_GetNumAvailableBytes(CLzmaEncHandle pp)
  {
    const CLzmaEnc *p = (CLzmaEnc *)pp;
    return p->matchFinder.GetNumAvailableBytes(p->matchFinderObj);
  }

  const Byte *LzmaEnc_GetCurBuf(CLzmaEncHandle pp)
  {
    const CLzmaEnc *p = (CLzmaEnc *)pp;
    return p->matchFinder.GetPointerToCurrentPos(p->matchFinderObj) - p->additionalOffset;
  }

  SRes LzmaEnc_CodeOneMemBlock(CLzmaEncHandle pp, Bool reInit,
                              Byte *dest, size_t *destLen, UInt32 desiredPackSize, UInt32 *unpackSize)
  {
    CLzmaEnc *p = (CLzmaEnc *)pp;
    UInt64 nowPos64;
    SRes res;
    CSeqOutStreamBuf outStream;

    outStream.funcTable.Write = MyWrite;
    outStream.data = dest;
    outStream.rem = *destLen;
    outStream.overflow = False;

    p->writeEndMark = False;
    p->finished = False;
    p->result = SZ_OK;

    if (reInit)
      LzmaEnc_Init(p);
    LzmaEnc_InitPrices(p);
    nowPos64 = p->nowPos64;
    RangeEnc_Init(&p->rc);
    p->rc.outStream = &outStream.funcTable;

    res = LzmaEnc_CodeOneBlock(p, True, desiredPackSize, *unpackSize);

    *unpackSize = (UInt32)(p->nowPos64 - nowPos64);
    *destLen -= outStream.rem;
    if (outStream.overflow)
      return SZ_ERROR_OUTPUT_EOF;

    return res;
  }

  SRes LzmaEnc_Encode(CLzmaEncHandle pp, ISeqOutStream *outStream, ISeqInStream *inStream, ICompressProgress *progress,
                      ISzAlloc *alloc, ISzAlloc *allocBig)
  {
    CLzmaEnc *p = (CLzmaEnc *)pp;
    SRes res = SZ_OK;

  #ifdef COMPRESS_MF_MT
    Byte allocaDummy[0x300];
    int i = 0;
    for (i = 0; i < 16; i++)
      allocaDummy[i] = (Byte)i;
  #endif

    RINOK(LzmaEnc_Prepare(pp, inStream, outStream, alloc, allocBig));

    for (;;)
    {
      res = LzmaEnc_CodeOneBlock(p, False, 0, 0);
      if (res != SZ_OK || p->finished != 0)
        break;
      if (progress != 0)
      {
        res = progress->Progress(progress, p->nowPos64, RangeEnc_GetProcessed(&p->rc));
        if (res != SZ_OK)
        {
          res = SZ_ERROR_PROGRESS;
          break;
        }
      }
    }
    LzmaEnc_Finish(pp);
    return res;
  }

  SRes LzmaEnc_WriteProperties(CLzmaEncHandle pp, Byte *props, SizeT *size)
  {
    CLzmaEnc *p = (CLzmaEnc *)pp;
    int i;
    UInt32 dictSize = p->dictSize;
    if (*size < LZMA_PROPS_SIZE)
      return SZ_ERROR_PARAM;
    *size = LZMA_PROPS_SIZE;
    props[0] = (Byte)((p->pb * 5 + p->lp) * 9 + p->lc);

    for (i = 11; i <= 30; i++)
    {
      if (dictSize <= ((UInt32)2 << i))
      {
        dictSize = (2 << i);
        break;
      }
      if (dictSize <= ((UInt32)3 << i))
      {
        dictSize = (3 << i);
        break;
      }
    }

    for (i = 0; i < 4; i++)
      props[1 + i] = (Byte)(dictSize >> (8 * i));
    return SZ_OK;
  }

  SRes LzmaEnc_MemEncode(CLzmaEncHandle pp, Byte *dest, SizeT *destLen, const Byte *src, SizeT srcLen,
                        int writeEndMark, ICompressProgress *progress, ISzAlloc *alloc, ISzAlloc *allocBig)
  {
    SRes res;
    CLzmaEnc *p = (CLzmaEnc *)pp;

    CSeqOutStreamBuf outStream;

    LzmaEnc_SetInputBuf(p, src, srcLen);

    outStream.funcTable.Write = MyWrite;
    outStream.data = dest;
    outStream.rem = *destLen;
    outStream.overflow = False;

    p->writeEndMark = writeEndMark;
    res = LzmaEnc_Encode(pp, &outStream.funcTable, &p->seqBufInStream.funcTable,
                        progress, alloc, allocBig);

    *destLen -= outStream.rem;
    if (outStream.overflow)
      return SZ_ERROR_OUTPUT_EOF;
    return res;
  }

  SRes LzmaEncode(Byte *dest, SizeT *destLen, const Byte *src, SizeT srcLen,
                  const CLzmaEncProps *props, Byte *propsEncoded, SizeT *propsSize, int writeEndMark,
                  ICompressProgress *progress, ISzAlloc *alloc, ISzAlloc *allocBig)
  {
    CLzmaEnc *p = (CLzmaEnc *)LzmaEnc_Create(alloc);
    SRes res;
    if (p == 0)
      return SZ_ERROR_MEM;

    res = LzmaEnc_SetProps(p, props);
    if (res == SZ_OK)
    {
      res = LzmaEnc_WriteProperties(p, propsEncoded, propsSize);
      if (res == SZ_OK)
        res = LzmaEnc_MemEncode(p, dest, destLen, src, srcLen,
                                writeEndMark, progress, alloc, allocBig);
    }

    LzmaEnc_Destroy(p, alloc, allocBig);
    return res;
  }

  /* LzmaLib.c -- LZMA library wrapper
  2008-08-05
  Igor Pavlov
  Public domain */

  static void *SzAlloc(void *p, size_t size)
  {
    p = p;
    return MyAlloc(size);
  }
  static void SzFree(void *p, void *address)
  {
    p = p;
    MyFree(address);
  }
  static ISzAlloc g_Alloc = {SzAlloc, SzFree};

  int LzmaCompress(char *dest, size_t *destLen, char *src, size_t srcLen,
                    char *outProps, size_t *outPropsSize,
                    int level,         /* 0 <= level <= 9, default = 5 */
                    unsigned dictSize, /* use (1 << N) or (3 << N). 4 KB < dictSize <= 128 MB */
                    int lc,            /* 0 <= lc <= 8, default = 3  */
                    int lp,            /* 0 <= lp <= 4, default = 0  */
                    int pb,            /* 0 <= pb <= 4, default = 2  */
                    int fb,            /* 5 <= fb <= 273, default = 32 */
                    int numThreads,    /* 1 or 2, default = 2 */
                    int algo           /* 0 = fast, 1 = normal */
  )
  {
    CLzmaEncProps props;
    LzmaEncProps_Init(&props);
    props.level = level;
    props.dictSize = dictSize;
    props.lc = lc;
    props.lp = lp;
    props.pb = pb;
    props.fb = fb;
    props.numThreads = numThreads;
    props.algo = algo;

    return LzmaEncode(dest, destLen, src, srcLen, &props, outProps, outPropsSize, 0,
                      NULL, &g_Alloc, &g_Alloc);
  }

  int LzmaUncompress(char *dest, size_t *destLen, char *src, size_t *srcLen,
                     char *props, size_t propsSize)
  {
    ELzmaStatus status;
    return LzmaDecode(dest, destLen, src, srcLen, props, (unsigned)propsSize, LZMA_FINISH_ANY, &status, &g_Alloc);
  }
""".}

# nim wrapper
{.push importc.}
proc LzmaCompress*( dest:cstring, destLen : ptr csize_t, src:cstring, srcLen:csize_t, 
  outProps:cstring, outPropsSize : ptr csize_t, level:cint, dictsize:cuint,
  lc, lp, pb, fb, numThreads, algo : cint ) : cint 

proc LzmaUncompress*(dest:cstring, destLen:ptr csize_t, src:cstring, srcLen:ptr csize_t, props:cstring, propsSize:csize_t) : cint 
{.pop.}

# compress / uncompress

const outPropsSize : csize_t = 5

type CompressDef = object
  outProps : array[5, char]
  org_size : csize_t

proc compress*( src:string, level : cint = 9 ) : string =
  var
    cd = CompressDef(org_size : src.len.csize_t)
    outPropsSize = outPropsSize
    destLen :csize_t = (1000 + src.len + CompressDef.sizeof).csize_t
  
  result = newString(destLen)
  
  if LzmaCompress(dest = result[CompressDef.sizeof].addr, destLen = destLen.unsafeAddr, src = src[0].unsafeAddr, srcLen = src.len.csize_t,
    outProps =  cd.outProps.unsafeAddr, outPropsSize = outPropsSize.unsafeAddr, level = level, 
    0.cuint, -1, -1, -1, -1, -1, 1) != 0:
    raise newException(IOError, "LZMA: error compressing data")

  result.setLen destLen.int + CompressDef.sizeof
  
  cast[ptr CompressDef](result[0].addr)[]=cd # set the compressed def in result

proc uncompress*(src:string):string=
  let 
    cd : CompressDef = cast[ptr CompressDef](src[0].unsafeAddr)[]
    srcLen : csize_t = src.len.csize_t

  result = newString(cd.org_size.int)

  if LzmaUncompress(result[0].addr, cd.org_size.unsafeAddr, src[CompressDef.sizeof].unsafeAddr, srcLen.unsafeAddr, cd.outProps.unsafeAddr, outPropsSize) != 0:
    raise newException(IOError, "LZMA: error uncompressing data")

##################
when isMainModule:

  let s = readFile("pisc.ply")
  let cs = s.compress
  echo "file len:", s.len, ", compressed len:", cs.len
  let us = cs.uncompress
  echo "uncompressed len:", us.len
  echo "result:", if s == us: "ok" else: "fail"

