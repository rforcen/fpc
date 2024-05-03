unit OpenCL;
(* Abstract: Pascal OpenCL bindings for FreePascal *)

{******************************************************************************
 * Copyright (c) 2008-2009 The Khronos Group Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and/or associated documentation files (the
 * "Materials"), to deal in the Materials without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Materials, and to
 * permit persons to whom the Materials are furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Materials.
 *
 * THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.
 ***************************************************************************** }
{ cl.h details:
$Revision: 9283 $ on $Date: 2009-10-14 10:18:57 -0700 (Wed, 14 Oct 2009) $  }
{**************************************************************************** }

(* OpenCL homepage: http://www.khronos.org/opencl/

   Translated from "cl.h" by AlmightFirebat of TMPG - steven123456789@hotmail.co.uk

   Date created: 10/02/2010. (10th Feburary 2010)
   Date modfied: 21/02/2010. (21st Feburary 2010)
     - Changed to be compatable with the existing "cl" package by Dmitry 'skalogryz' Boyarintsev
     - Thanks to Dmitry 'skalogryz' Boyarintsev for suggestions on how to improve this.

   With thanks to the Free Pascal Team for the "h2pas" tool.
   Further thanks to the developers of Lazarus for making such an easy-to-use IDE.

   For all functions in this unit, a return value of TRUE indicates SUCCESS, and vice versa.

   Symbols within this unit:
   CL_AUTOLOADING - if defined, initialization and finalization sections take care of loading the library, using the system's default library.
   CL_SAFELOADING - if defined and CL_AUTOLOADING is also defined, exceptions are raised during initialization/finalization
                  - if defined and CL_AUTOLOADING is not defined, will cause LoadOpenCL(...) to return false if a function is not found.

   By default, CL_AUTOLOADING is defined to make this compatible with "cl.pp".
   CL_AUTOLOADING is provided for ease-of-use, but currently won't work on Linux or MacOSX
     - The fix is to provide a valid library name, see line 712 and below.
   CL_SAFELOADING is provided if you want to debug an error by eliminating this unit as the cause
   (CL_SAFELOADING prevents nil pointers going unnoticed at load time)

   Extra features:
   === NOTICE ===
   FPC currently forbids overloading the the assignment operator (at least) for boolean so the following feature is currently unavailable.
   Use:
   Ord( *Boolean Expression* ) * -1
   where ever you wished to place a Pascal boolean expression and a Tcl_bool value was expected.

   Desired feature, if FPC will permit overloading boolean assignment operators:
   Operator overloading - assignment operators are overloaded for the cl_bool type with the native Pascal boolean type
   											  so "True" and "False" and any boolean expression can be assigned to the "Tcl_bool" type and vice versa
   ==============

   Supported platforms:
   Windows, Linux, MacOSX.

   NB: Linux and MACOSX support is untested due to lack of appropiate hardware/software combinations.
   I also do not know what the default OpenCL library name on Linux or MacOSX would be.
   Calling conventions are definately compatible with:
     - ATi/AMD implementations of OpenCL for Win32/64 and Linux.
     - Apple's implementation of OpenCL for MacOSX.
     - NVidia's implementation of OpenCL for Win32.

   but may not be compatible with the calling conventions for other implementations.
*)


(* The following notice is reproduced from original "cl.pp" *)
(* Some constants renamed as they conflict with type names *)
// Original C name           Ported_name
// CL_DEVICE_TYPE            CL_DEVICE_TYPE_INFO
// CL_DEVICE_LOCAL_MEM_TYPE  CL_DEVICE_LOCAL_MEM_TYPE_INFO
// CL_CONTEXT_PROPERTIES     CL_CONTEXT_PROPERTIES_INFO
// CL_CONTEXT_PLATFORM       CL_CONTEXT_PLATFORM_INFO (Changed even though no conflict in this unit, for code compatability)
// CL_FLOAT                  CL_FLOAT_TYPE
// CL_MEM_FLAGS              CL_MEM_FLAGS_INFO
// CL_IMAGE_FORMAT           CL_IMAGE_FORMAT_INFO

interface

Uses DynLibs;

// For compatibility with "cl.pp", defining "CL_AUTOLOADING"
{$DEFINE CL_AUTOLOADING}
// Uncomment if you suspect a nil-function pointer, or want to guard against nil function pointers:
// {$DEFINE CL_SAFELOADING}

type
   (* Manual type translations from cl_platform.h *)
   cl_char = ShortInt; // Signed 1 byte
   cl_uchar = Byte;
   cl_short = SmallInt; // Signed 2 byte
   cl_ushort = Word;
   cl_int = LongInt;
   cl_uint = LongWord;
   cl_long = Int64;
   {$IFDEF CPU64}
   cl_ulong = QWord;
   {$ELSE}
   cl_ulong = Int64; // Only construct that will at least be large enough. If I'm feeling nice, I'll try to overload the operators so mistakes can't happen
   {$ENDIF}
   cl_half = Word; // = cl_ushort;
   // Floating point
   cl_float = Single;
   cl_double = Double;
   // declarations to aid translation between C and Pascal
   cl_bool = cl_uint; // WARNING!  Unlike cl_ types in cl_platform.h, cl_bool is not guaranteed to be the same size as the bool in kernels.
   cl_bitfield = cl_ulong;
   intptr_t = PtrInt;
   size_t = PtrUint;
   csize_t = size_t;
   size_t3 = packed array[0..2] of size_t;

   (* C Vector types *)
   cl_char2 = packed array[0..1] of cl_char;
   cl_char4 = packed array[0..3] of cl_char;
   cl_char8 = packed array[0..7] of cl_char;
   cl_char16 = packed array[0..15] of cl_char;

   cl_uchar2 = packed array[0..1] of cl_uchar;
   cl_uchar4 = packed array[0..3] of cl_uchar;
   cl_uchar8 = packed array[0..7] of cl_uchar;
   cl_uchar16 = packed array[0..15] of cl_uchar;

   cl_short2 = packed array[0..1] of cl_short;
   cl_short4 = packed array[0..3] of cl_short;
   cl_short8 = packed array[0..7] of cl_short;
   cl_short16 = packed array[0..15] of cl_short;

   cl_ushort2 = packed array[0..1] of cl_ushort;
   cl_ushort4 = packed array[0..3] of cl_ushort;
   cl_ushort8 = packed array[0..7] of cl_ushort;
   cl_ushort16 = packed array[0..15] of cl_ushort;

   cl_int2 = packed array[0..1] of cl_int;
   cl_int4 = packed array[0..3] of cl_int;
   cl_int8 = packed array[0..7] of cl_int;
   cl_int16 = packed array[0..15] of cl_int;

   cl_uint2 = packed array[0..1] of cl_uint;
   cl_uint4 = packed array[0..3] of cl_uint;
   cl_uint8 = packed array[0..7] of cl_uint;
   cl_uint16 = packed array[0..15] of cl_uint;

   cl_long2 = packed array[0..1] of cl_long;
   cl_long4 = packed array[0..3] of cl_long;
   cl_long8 = packed array[0..7] of cl_long;
   cl_long16 = packed array[0..15] of cl_long;

   cl_ulong2 = packed array[0..1] of cl_ulong;
   cl_ulong4 = packed array[0..3] of cl_ulong;
   cl_ulong8 = packed array[0..7] of cl_ulong;
   cl_ulong16 = packed array[0..15] of cl_ulong;

   cl_float2 = packed array[0..1] of cl_float;
   cl_float4 = packed array[0..3] of cl_float;
   cl_float8 = packed array[0..7] of cl_float;
   cl_float16 = packed array[0..15] of cl_float;

   cl_double2 = packed array[0..1] of cl_double;
   cl_double4 = packed array[0..3] of cl_double;
   cl_double8 = packed array[0..7] of cl_double;
   cl_double16 = packed array[0..15] of cl_double;

   { There are no vector types for half }

   (* The following are defined in cl.h as "typedef struct _Name * Name" but because they are all pointers to
      opaque structures (thus allowing the structures to be implementation defined),
      we will simply call them untyped pointers as the contents are opqaue anyway *)
   cl_platform_id = Pointer;
   cl_device_id = Pointer;
   cl_context = Pointer;
   cl_command_queue = Pointer;
   cl_mem = Pointer;
   cl_program = Pointer;
   cl_kernel = Pointer;
   cl_event = Pointer;
   cl_sampler = Pointer;
   // Thanks to Dmitry 'skalogryz' Boyarintsev for the following declarations
   Pcl_platform_id = ^cl_platform_id;
   Pcl_device_id = ^cl_device_id;
   Pcl_context = ^cl_context;
   Pcl_command_queue = ^cl_command_queue;
   Pcl_mem = ^cl_mem;
   Pcl_program = ^cl_program;
   Pcl_kernel = ^cl_kernel;
   Pcl_event = ^cl_event;
   Pcl_sampler = ^cl_sampler;

   cl_device_type = cl_bitfield;
   cl_platform_info = cl_uint;
   cl_device_info = cl_uint;
   cl_device_address_info = cl_bitfield;
   cl_device_fp_config = cl_bitfield;
   cl_device_mem_cache_type = cl_uint;
   cl_device_local_mem_type = cl_uint;
   cl_device_exec_capabilities = cl_bitfield;
   cl_command_queue_properties = cl_bitfield;
   Pcl_command_queue_properties = ^cl_command_queue_properties;
   cl_context_properties = intptr_t;
   cl_context_info = cl_uint;
   cl_command_queue_info = cl_uint;
   cl_channel_order = cl_uint;
   cl_channel_type = cl_uint;
   cl_mem_flags = cl_bitfield;
   cl_mem_object_type = cl_uint;
   cl_mem_info = cl_uint;
   cl_image_info = cl_uint;
   cl_addressing_mode = cl_uint;
   cl_filter_mode = cl_uint;
   cl_sampler_info = cl_uint;
   cl_map_flags = cl_bitfield;
   cl_program_info = cl_uint;
   cl_program_build_info = cl_uint;
   cl_build_status = cl_int;
   cl_kernel_info = cl_uint;
   cl_kernel_work_group_info = cl_uint;
   cl_event_info = cl_uint;
   cl_command_type = cl_uint;
   cl_profiling_info = cl_uint;

   cl_image_format = packed record
   	image_channel_order : cl_channel_order;
    image_channel_data_type : cl_channel_type;
   end;

   {$MACRO ON}
   {$IFDEF MSWINDOWS}
   {$DEFINE ExtDecl := StdCall}
   {$ENDIF}
   {$IFDEF LINUX}
   {$DEFINE ExtDecl := Cdecl}
   {$ENDIF}
   {$IFDEF DARWIN}
   {$DEFINE ExtDecl := Cdecl}
   {$ENDIF}
   // OpenCL defined callback types
   TclErrorFunction = Procedure(ErrorString : PChar; PrivateInfo : Pointer; PrivateInfoSize : size_t; UserData : Pointer); ExtDecl;
   TclProgramErrorFunction = Procedure(aProgram : cl_program; UserData : pointer); ExtDecl;
   TclNativeKernel = Procedure(Data : Pointer); ExtDecl;

   // API function types
   TclGetPlatformIDs = Function(NumEntries : cl_uint; PlatformIDs : Pointer; var NumPlatforms : cl_uint) : cl_int; ExtDecl;
   TclGetPlatformInfo = Function(PlatformID : cl_platform_id; ParamName : cl_platform_info; ParamValueSize : size_t; ParamDest : Pointer; var ReturnedParamSize : size_t) : cl_int; ExtDecl;

   TclGetDeviceIDs = Function(PlatformID : cl_platform_id; DeviceType : cl_device_type; NumEntries : cl_uint; DeviceIDs : Pointer; var NumDevices : cl_uint) : cl_int; ExtDecl;
   TclGetDeviceInfo = Function(DeviceID : cl_device_id; ParamName : cl_device_info; ParamValueSize : size_t; ParamDest : Pointer; var ReturnedParamSize : size_t) : cl_int; ExtDecl;

   TclCreateContext = Function(Properties : Pointer; NumDevices : cl_uint; DeviceIDs : Pointer; ErrorCallback : TclErrorFunction; UserData : Pointer; var ErrCode : cl_int) : cl_context; ExtDecl;
   TclCreateContextFromType = Function(Properties : Pointer; DeviceType : cl_device_type; ErrorCallback : TclErrorFunction; UserData : Pointer; var ErrCode : cl_int) : cl_context; ExtDecl;
   TclRetainContext = Function(Context : cl_context) : cl_int; ExtDecl;
   TclReleaseContext = Function(Context : cl_context) : cl_int; ExtDecl;
   TclGetContextInfo = Function(Context : cl_context; ParamName : cl_context_info; ParamValueSize : size_t; ParamDest : Pointer; var ReturnedParamSize : size_t) : cl_int; ExtDecl;

   TclCreateCommandQueue = Function(Context : cl_context; DeviceID : cl_device_id; Properties : cl_command_queue_properties; var ErrCode : cl_int) : cl_command_queue; ExtDecl;
   TclRetainCommandQueue = Function(Queue : cl_command_queue) : cl_int; ExtDecl;
   TclReleaseCommandQueue = Function(Queue : cl_command_queue) : cl_int; ExtDecl;
   TclGetCommmandQueueInfo = Function(Queue : cl_command_queue; ParamName : cl_command_queue_info; ParamValueSize : size_t; ParamDest : Pointer; var ReturnedParamSize : size_t) : cl_int; ExtDecl;
   TclSetCommandQueueProperty = Function(Queue : cl_command_queue; Properties : cl_command_queue_properties; Enable : cl_bool; OldProperties : Pcl_command_queue_properties) : cl_int; ExtDecl;

   TclRetainMemObject = Function(MemObj : cl_mem) : cl_int; ExtDecl;
   TclReleaseMemObject = Function(MemObj : cl_mem) : cl_int; ExtDecl;
   TclGetMemObjectInfo = Function(MemObj : cl_mem; ParamName : cl_mem_info; ParamValueSize : size_t; ParamDest : Pointer; var ReturnedParamSize : size_t) : cl_int; ExtDecl;

   TclCreateBuffer = Function(Context : cl_context; Flags : cl_mem_flags; Size : size_t; HostPtr : Pointer; var ErrCode : cl_int) : cl_mem; ExtDecl;
   TclEnqueueReadBuffer = Function(Queue : cl_command_queue; Buffer : cl_mem; BlockingRead : cl_bool; Offset, Count : size_t; SrcPtr : Pointer; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event) : cl_int; ExtDecl;
   TclEnqueueWriteBuffer = Function(Queue : cl_command_queue; Buffer : cl_mem; BlockingWrite : cl_bool; Offset, Count : size_t; DestPtr : Pointer; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event) : cl_int; ExtDecl;
   TclEnqueueCopyBuffer = Function(Queue : cl_command_queue; SrcBuffer, DestBuffer : cl_mem; SrcOffset, DestOffset, Count : size_t; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event) : cl_int; ExtDecl;

   TclCreateImage2D = Function(Context : cl_context; Flags : cl_mem_flags; var ImageFormat : cl_image_format; Width, Height, RowPitch : size_t; Src : Pointer; var ErrCode : cl_int) : cl_mem; ExtDecl;
   TclCreateImage3D = Function(Context : cl_context; Flags : cl_mem_flags; var ImageFormat : cl_image_format; Width, Height, Depth, RowPitch, SlicePitch : size_t; Src : Pointer; var ErrCode : cl_int) : cl_mem; ExtDecl;
   TclGetSupportedImageFormats = Function(Context : cl_context; Flags : cl_mem_flags; ImageType : cl_mem_object_type; NumEntries : cl_uint; ImageFormats : Pointer; var NumImageFormats : cl_uint) : cl_int; ExtDecl;
   TclEnqueueReadImage = Function(Queue : cl_command_queue; Image : cl_mem; BlockingRead : cl_bool; var Origin : size_t3; var Region : size_t3; RowPitch, SlicePitch : size_t; Src : Pointer; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event) : cl_int; ExtDecl;
   TclEnqueueWriteImage = Function(Queue : cl_command_queue; Image : cl_mem; BlockingWrite : cl_bool; var Origin : size_t3; var Region : size_t3; RowPitch, SlicePitch : size_t; Dest : Pointer; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event) : cl_int; ExtDecl;
   TclEnqueueCopyImage = Function(Queue : cl_command_queue; Src, Dest : cl_mem; var SrcOrigin : size_t3; var DestOrigin : size_t3; var Region : size_t3; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event) : cl_int; ExtDecl;
   TclGetImageInfo = Function(Image : cl_mem; ParamName : cl_image_info; ParamValueSize : size_t; ParamDest : Pointer; var ReturnedParamSize : size_t) : cl_int; ExtDecl;

   TclEnqueueCopyImageToBuffer = Function(Queue : cl_command_queue; SrcImage, DestBuffer : cl_mem; var SrcOrigin : size_t3; var Region : size_t3; DestOffset : size_t; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event) : cl_int; ExtDecl;
   TclEnqueueCopyBufferToImage = Function(Queue : cl_command_queue; SrcBuffer, DestImage : cl_mem; SrcOffset : size_t; var DestOrigin : size_t3; var Region : size_t3; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event) : cl_int; ExtDecl;

   TclEnqueueMapBuffer = Function(Queue : cl_command_queue; Buffer : cl_mem; BlockingMap : cl_bool; Flags : cl_map_flags; Offset, Count : size_t; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event; var ErrCode : cl_int) : Pointer; ExtDecl;
   TclEnqueueMapImage = Function(Queue : cl_command_queue; Image : cl_mem; BlockingMap : cl_bool; Flags : cl_map_flags; var Origin : size_t3; var Region : size_t3; var RowPitch : size_t; var SlicePitch : size_t; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event; var ErrCode : cl_int) : Pointer; ExtDecl;
   TclEnqueueUnmapMemObject = Function(Queue : cl_command_queue; MemObj : cl_mem; MappedPtr : Pointer; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event) : cl_int; ExtDecl;

   TclCreateSampler = Function(Context : cl_context; NormalizedCoords : cl_bool; AddressingMode : cl_addressing_mode; FilterMode : cl_filter_mode; var ErrCode : cl_int) : cl_sampler; ExtDecl;
   TclRetainSampler = Function(Sampler : cl_sampler) : cl_int; ExtDecl;
   TclReleaseSampler = Function(Sampler : cl_sampler) : cl_int; ExtDecl;
   TclGetSamplerInfo = Function(Sampler : cl_sampler; ParamName : cl_sampler_info; ParamValueSize : size_t; ParamDest : Pointer; var ReturnedParamSize : size_t) : cl_int; ExtDecl;

   TclCreateProgramWithSource = Function(Context : cl_context; LineCount : cl_uint; Lines : Pointer; LineLengths : Pointer; Var ErrCode : cl_int) : cl_program; ExtDecl;
   TclCreateProgramWithBinary = Function(Context : cl_context; NumDevices : cl_uint; DeviceIDs : Pointer; BinarySizes : Pointer; Binaries : Pointer; var BinaryStatus : cl_int; var ErrCode : cl_int) : cl_program; ExtDecl;
   TclRetainProgram = Function(aProgram : cl_program) : cl_int; ExtDecl;
   TclReleaseProgram = Function(aProgram : cl_program) : cl_int; ExtDecl;
   TclBuildProgram = Function(aProgram : cl_program; NumDevices : cl_uint; DeviceList : Pointer; Options : PChar; BuildCallback : TclProgramErrorFunction; UserData : Pointer) : cl_int; ExtDecl;
   TclUnloadCompiler = Function() : cl_int; ExtDecl;
   TclGetProgramInfo = Function(aProgram : cl_program; ParamName : cl_program_info; ParamValueSize : size_t; ParamValue : Pointer; var ReturnedParamSize : size_t) : cl_int; ExtDecl;
   TclGetProgramBuildInfo = Function(aProgram : cl_program; DeviceID : cl_device_id; ParamName : cl_program_build_info; ParamSize : size_t; ParamValue : Pointer; var ReturnedParamSize : size_t) : cl_int; ExtDecl;

   TclCreateKernel = Function(aProgram : cl_program; KernelName : PChar; var ErrCode : cl_int) : cl_kernel; ExtDecl;
   TclCreateKernelsInProgram = Function(aProgram : cl_program; KernelCount : cl_uint; Kernels : Pointer; var ReturnedKernels : cl_uint) : cl_int; ExtDecl;
   TclRetainKernel = Function(Kernel : cl_kernel) : cl_int; ExtDecl;
   TclReleaseKernel = Function(Kernel : cl_kernel) : cl_int; ExtDecl;
   TclSetKernelArg = Function(Kernel : cl_kernel; ArgIndex : cl_uint; ArgSize : size_t; Arg : Pointer) : cl_int; ExtDecl;
   TclGetKernelInfo = Function(Kernel : cl_kernel; ParamName : cl_kernel_info; ParamValueSize : size_t; ParamDest : Pointer; Var ReturnedParamSize : size_t) : cl_int; ExtDecl;
   TclGetKernelWorkGroupInfo = Function(Kernel : cl_kernel; DeviceID : cl_device_id; ParamName : cl_kernel_work_group_info; ParamValueSize : size_t; ParamDest : Pointer; Var ReturnedParamSize : size_t) : cl_int; ExtDecl;
   TclEnqueueNDRangeKernel = Function(Queue : cl_command_queue; Kernel : cl_kernel; WorkDimensions : cl_uint; Reserved {GlobalWorkOffset, must currently be null - OpenCL V1.0.48} : Pointer; GlobalWorkSize, LocalWorkSize : Pointer; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event) : cl_int; ExtDecl;
   TclEnqueueTask = Function(Queue : cl_command_queue; Kernel : cl_kernel; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event) : cl_int; ExtDecl;
   TclEnqueueNativeKernel = Function(Queue : cl_command_queue; NativeKernel : TclNativeKernel; Args : Pointer; ArgSize : size_t; MemObjCount : cl_uint; MemObjList : Pointer; MemLocList : Pointer; WaitEventCount : cl_uint; WaitEventList : Pointer; FinishedEvent : cl_event) : cl_int; ExtDecl;

   TclWaitForEvents = Function(NumEvents : cl_uint; EventList : Pointer) : cl_int; ExtDecl;
   TclGetEventInfo = Function(Event : cl_event; ParamName : cl_event_info; ParamValueSize : size_t; ParamDest : Pointer; Var ReturnedParamSize : size_t) : cl_int; ExtDecl;
   TclRetainEvent = Function(Event : cl_event) : cl_int; ExtDecl;
   TclReleaseEvent = Function(Event : cl_event) : cl_int; ExtDecl;
   TclEnqueueMarker = Function(Queue : cl_command_queue; Event : cl_event) : cl_int; ExtDecl;
   TclEnqueueWaitForEvents = Function(Queue : cl_command_queue; EventCount : cl_uint; EventList : Pointer) : cl_int; ExtDecl;
   TclEnqueueBarrier = Function(Queue : cl_command_queue) : cl_int; ExtDecl;

   TclGetEventProfilingInfo = Function(Event : cl_event; ParamName : cl_profiling_info; ParamValueSize : size_t; ParamDest : Pointer; Var ReturnedParamSize : size_t) : cl_int; ExtDecl;

   TclFlush = Function(Queue : cl_command_queue) : cl_int; ExtDecl; // Non blocking execute
   TclFinish = Function(Queue : cl_command_queue) : cl_int; ExtDecl; // Blocking execute

   TclAPI = packed record
   	clGetPlatformIDs : TclGetPlatformIDs;
    clGetPlatformInfo : TclGetPlatformInfo;
    clGetDeviceIDs : TclGetDeviceIDs;
    clGetDeviceInfo : TclGetDeviceInfo;
    clCreateContext : TclCreateContext;
    clCreateContextFromType : TclCreateContextFromType;
    clRetainContext : TclRetainContext;
    clReleaseContext : TclReleaseContext;
    clGetContextInfo : TclGetContextInfo;
    clCreateCommandQueue : TclCreateCommandQueue;
    clRetainCommandQueue : TclRetainCommandQueue;
    clReleaseCommandQueue : TclReleaseCommandQueue;
    clGetCommmandQueueInfo : TclGetCommmandQueueInfo;
    clSetCommandQueueProperty : TclSetCommandQueueProperty;
    clRetainMemObject : TclRetainMemObject;
    clReleaseMemObject : TclReleaseMemObject;
    clGetMemObjectInfo : TclGetMemObjectInfo;
    clCreateBuffer : TclCreateBuffer;
    clEnqueueReadBuffer : TclEnqueueReadBuffer;
    clEnqueueWriteBuffer : TclEnqueueWriteBuffer;
    clEnqueueCopyBuffer : TclEnqueueCopyBuffer;
    clCreateImage2D : TclCreateImage2D;
    clCreateImage3D : TclCreateImage3D;
    clGetSupportedImageFormats : TclGetSupportedImageFormats;
    clEnqueueReadImage : TclEnqueueReadImage;
    clEnqueueWriteImage : TclEnqueueWriteImage;
    clEnqueueCopyImage : TclEnqueueCopyImage;
    clGetImageInfo : TclGetImageInfo;
    clEnqueueCopyImageToBuffer : TclEnqueueCopyImageToBuffer;
    clEnqueueCopyBufferToImage : TclEnqueueCopyBufferToImage;
    clEnqueueMapBuffer : TclEnqueueMapBuffer;
    clEnqueueMapImage : TclEnqueueMapImage;
    clEnqueueUnmapMemObject : TclEnqueueUnmapMemObject;
    clCreateSampler : TclCreateSampler;
    clRetainSampler : TclRetainSampler;
    clReleaseSampler : TclReleaseSampler;
    clGetSamplerInfo : TclGetSamplerInfo;
    clCreateProgramWithSource : TclCreateProgramWithSource;
    clCreateProgramWithBinary : TclCreateProgramWithBinary;
    clRetainProgram : TclRetainProgram;
    clReleaseProgram : TclReleaseProgram;
    clBuildProgram : TclBuildProgram;
    clUnloadCompiler : TclUnloadCompiler;
    clGetProgramInfo : TclGetProgramInfo;
    clGetProgramBuildInfo : TclGetProgramBuildInfo;
    clCreateKernel : TclCreateKernel;
    clCreateKernelsInProgram : TclCreateKernelsInProgram;
    clRetainKernel : TclRetainKernel;
    clReleaseKernel : TclReleaseKernel;
    clSetKernelArg : TclSetKernelArg;
    clGetKernelInfo : TclGetKernelInfo;
    clGetKernelWorkGroupInfo : TclGetKernelWorkGroupInfo;
    clEnqueueNDRangeKernel : TclEnqueueNDRangeKernel;
    clEnqueueTask : TclEnqueueTask;
    clEnqueueNativeKernel : TclEnqueueNativeKernel;
    clWaitForEvents : TclWaitForEvents;
    clGetEventInfo : TclGetEventInfo;
    clRetainEvent : TclRetainEvent;
    clReleaseEvent : TclReleaseEvent;
    clEnqueueMarker : TclEnqueueMarker;
    clEnqueueWaitForEvents : TclEnqueueWaitForEvents;
    clEnqueueBarrier : TclEnqueueBarrier;
    clGetEventProfilingInfo : TclGetEventProfilingInfo;
    clFlush : TclFlush;
    clFinish : TclFinish;
   End;

const
  { Max / Min / other special values relating to the cl_ types }
  CL_CHAR_BIT = SizeOf(cl_char) * 8;
  CL_SCHAR_MAX = High(cl_char);
  CL_SCHAR_MIN = Low(cl_char);
  CL_CHAR_MAX = CL_SCHAR_MAX;
  CL_CHAR_MIN = CL_SCHAR_MIN;
  CL_UCHAR_MAX = High(cl_uchar);
  CL_SHRT_MAX = High(cl_short);
  CL_SHRT_MIN = Low(cl_short);
  CL_USHRT_MAX = High(cl_ushort);
  CL_INT_MAX = High(cl_int);
  CL_INT_MIN = Low(cl_int);
  CL_UINT_MAX = High(cl_uint);
  CL_LONG_MAX = High(cl_long);
  CL_LONG_MIN = Low(cl_long);
  CL_ULONG_MAX = High(cl_ulong);
  // Not sure what some of these are supposed to be:
  CL_FLT_DIG = 6;
  CL_FLT_MANT_DIG = 24;
  CL_FLT_MAX_10_EXP = +38;
  CL_FLT_MAX_EXP = +128;
  CL_FLT_MIN_10_EXP = -37;
  CL_FLT_MIN_EXP = -125;
  CL_FLT_RADIX = 2;
  CL_FLT_MAX = 340282346638528859811704183484516925440.0;
  // CL_FLT_MIN =  1.175494350822287507969 ** -38;
  // CL_FLT_EPSILON = 0x1.0p-23; // Not sure what "p" does in a C constant declaration.

  CL_DBL_DIG = 15;
  CL_DBL_MANT_DIG = 53;
  CL_DBL_MAX_10_EXP = +308;
  CL_DBL_MAX_EXP = +1024;
  CL_DBL_MIN_10_EXP = -307;
  CL_DBL_MIN_EXP = -1021;
  CL_DBL_RADIX = 2;
  CL_DBL_MAX = 179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.0;
  // CL_DBL_MIN = 2.225073858507201383090 ** -308;
  // CL_DBL_EPSILON = 2.220446049250313080847 ** -16;

  { Error Codes }
  CL_SUCCESS = 0;
  CL_DEVICE_NOT_FOUND = -(1);
  CL_DEVICE_NOT_AVAILABLE = -(2);
  CL_COMPILER_NOT_AVAILABLE = -(3);
  CL_MEM_OBJECT_ALLOCATION_FAILURE = -(4);
  CL_OUT_OF_RESOURCES = -(5);
  CL_OUT_OF_HOST_MEMORY = -(6);
  CL_PROFILING_INFO_NOT_AVAILABLE = -(7);
  CL_MEM_COPY_OVERLAP = -(8);
  CL_IMAGE_FORMAT_MISMATCH = -(9);
  CL_IMAGE_FORMAT_NOT_SUPPORTED = -(10);
  CL_BUILD_PROGRAM_FAILURE = -(11);
  CL_MAP_FAILURE = -(12);
  CL_INVALID_VALUE = -(30);
  CL_INVALID_DEVICE_TYPE = -(31);
  CL_INVALID_PLATFORM = -(32);
  CL_INVALID_DEVICE = -(33);
  CL_INVALID_CONTEXT = -(34);
  CL_INVALID_QUEUE_PROPERTIES = -(35);
  CL_INVALID_COMMAND_QUEUE = -(36);
  CL_INVALID_HOST_PTR = -(37);
  CL_INVALID_MEM_OBJECT = -(38);
  CL_INVALID_IMAGE_FORMAT_DESCRIPTOR = -(39);
  CL_INVALID_IMAGE_SIZE = -(40);
  CL_INVALID_SAMPLER = -(41);
  CL_INVALID_BINARY = -(42);
  CL_INVALID_BUILD_OPTIONS = -(43);
  CL_INVALID_PROGRAM = -(44);
  CL_INVALID_PROGRAM_EXECUTABLE = -(45);
  CL_INVALID_KERNEL_NAME = -(46);
  CL_INVALID_KERNEL_DEFINITION = -(47);
  CL_INVALID_KERNEL = -(48);
  CL_INVALID_ARG_INDEX = -(49);
  CL_INVALID_ARG_VALUE = -(50);
  CL_INVALID_ARG_SIZE = -(51);
  CL_INVALID_KERNEL_ARGS = -(52);
  CL_INVALID_WORK_DIMENSION = -(53);
  CL_INVALID_WORK_GROUP_SIZE = -(54);
  CL_INVALID_WORK_ITEM_SIZE = -(55);
  CL_INVALID_GLOBAL_OFFSET = -(56);
  CL_INVALID_EVENT_WAIT_LIST = -(57);
  CL_INVALID_EVENT = -(58);
  CL_INVALID_OPERATION = -(59);
  CL_INVALID_GL_OBJECT = -(60);
  CL_INVALID_BUFFER_SIZE = -(61);
  CL_INVALID_MIP_LEVEL = -(62);
  CL_INVALID_GLOBAL_WORK_SIZE = -(63);
  { OpenCL Version }
  CL_VERSION_1_0 = 1;
  { cl_bool }
  CL_FALSE = 0;
  CL_TRUE = 1;
  { cl_platform_info }
  CL_PLATFORM_PROFILE = $0900;
  CL_PLATFORM_VERSION = $0901;
  CL_PLATFORM_NAME = $0902;
  CL_PLATFORM_VENDOR = $0903;
  CL_PLATFORM_EXTENSIONS = $0904;
  { cl_device_type - bitfield }
  CL_DEVICE_TYPE_DEFAULT = 1 shl 0;
  CL_DEVICE_TYPE_CPU = 1 shl 1;
  CL_DEVICE_TYPE_GPU = 1 shl 2;
  CL_DEVICE_TYPE_ACCELERATOR = 1 shl 3;
  CL_DEVICE_TYPE_ALL = $FFFFFFFF;
  { cl_device_info }
  CL_DEVICE_TYPE_INFO = $1000;
  CL_DEVICE_VENDOR_ID = $1001;
  CL_DEVICE_MAX_COMPUTE_UNITS = $1002;
  CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS = $1003;
  CL_DEVICE_MAX_WORK_GROUP_SIZE = $1004;
  CL_DEVICE_MAX_WORK_ITEM_SIZES = $1005;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR = $1006;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT = $1007;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT = $1008;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG = $1009;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT = $100A;
  CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE = $100B;
  CL_DEVICE_MAX_CLOCK_FREQUENCY = $100C;
  CL_DEVICE_ADDRESS_BITS = $100D;
  CL_DEVICE_MAX_READ_IMAGE_ARGS = $100E;
  CL_DEVICE_MAX_WRITE_IMAGE_ARGS = $100F;
  CL_DEVICE_MAX_MEM_ALLOC_SIZE = $1010;
  CL_DEVICE_IMAGE2D_MAX_WIDTH = $1011;
  CL_DEVICE_IMAGE2D_MAX_HEIGHT = $1012;
  CL_DEVICE_IMAGE3D_MAX_WIDTH = $1013;
  CL_DEVICE_IMAGE3D_MAX_HEIGHT = $1014;
  CL_DEVICE_IMAGE3D_MAX_DEPTH = $1015;
  CL_DEVICE_IMAGE_SUPPORT = $1016;
  CL_DEVICE_MAX_PARAMETER_SIZE = $1017;
  CL_DEVICE_MAX_SAMPLERS = $1018;
  CL_DEVICE_MEM_BASE_ADDR_ALIGN = $1019;
  CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE = $101A;
  CL_DEVICE_SINGLE_FP_CONFIG = $101B;
  CL_DEVICE_GLOBAL_MEM_CACHE_TYPE = $101C;
  CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE = $101D;
  CL_DEVICE_GLOBAL_MEM_CACHE_SIZE = $101E;
  CL_DEVICE_GLOBAL_MEM_SIZE = $101F;
  CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE = $1020;
  CL_DEVICE_MAX_CONSTANT_ARGS = $1021;
  CL_DEVICE_LOCAL_MEM_TYPE_INFO = $1022;
  CL_DEVICE_LOCAL_MEM_SIZE = $1023;
  CL_DEVICE_ERROR_CORRECTION_SUPPORT = $1024;
  CL_DEVICE_PROFILING_TIMER_RESOLUTION = $1025;
  CL_DEVICE_ENDIAN_LITTLE = $1026;
  CL_DEVICE_AVAILABLE = $1027;
  CL_DEVICE_COMPILER_AVAILABLE = $1028;
  CL_DEVICE_EXECUTION_CAPABILITIES = $1029;
  CL_DEVICE_QUEUE_PROPERTIES = $102A;
  CL_DEVICE_NAME = $102B;
  CL_DEVICE_VENDOR = $102C;
  CL_DRIVER_VERSION = $102D;
  CL_DEVICE_PROFILE = $102E;
  CL_DEVICE_VERSION = $102F;
  CL_DEVICE_EXTENSIONS = $1030;
  CL_DEVICE_PLATFORM = $1031;
  { cl_device_fp_config - bitfield }
  CL_FP_DENORM = 1 shl 0;
  CL_FP_INF_NAN = 1 shl 1;
  CL_FP_ROUND_TO_NEAREST = 1 shl 2;
  CL_FP_ROUND_TO_ZERO = 1 shl 3;
  CL_FP_ROUND_TO_INF = 1 shl 4;
  CL_FP_FMA = 1 shl 5;
  { cl_device_mem_cache_type }
  CL_NONE = $0;
  CL_READ_ONLY_CACHE = $1;
  CL_READ_WRITE_CACHE = $2;
  { cl_device_local_mem_type }
  CL_LOCAL = $1;
  CL_GLOBAL = $2;
  { cl_device_exec_capabilities - bitfield }
  CL_EXEC_KERNEL = 1 shl 0;
  CL_EXEC_NATIVE_KERNEL = 1 shl 1;
  { cl_command_queue_properties - bitfield }
  CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE = 1 shl 0;
  CL_QUEUE_PROFILING_ENABLE = 1 shl 1;
  { cl_context_info }
  CL_CONTEXT_REFERENCE_COUNT = $1080;
  CL_CONTEXT_DEVICES = $1081;
  CL_CONTEXT_PROPERTIES_INFO = $1082;
  { cl_context_properties }
  CL_CONTEXT_PLATFORM_INFO = $1084;
  { cl_command_queue_info }
  CL_QUEUE_CONTEXT = $1090;
  CL_QUEUE_DEVICE = $1091;
  CL_QUEUE_REFERENCE_COUNT = $1092;
  CL_QUEUE_PROPERTIES = $1093;
  { cl_mem_flags - bitfield }
  CL_MEM_READ_WRITE = 1 shl 0;
  CL_MEM_WRITE_ONLY = 1 shl 1;
  CL_MEM_READ_ONLY = 1 shl 2;
  CL_MEM_USE_HOST_PTR = 1 shl 3;
  CL_MEM_ALLOC_HOST_PTR = 1 shl 4;
  CL_MEM_COPY_HOST_PTR = 1 shl 5;
  { cl_channel_order }
  CL_R = $10B0;
  CL_A = $10B1;
  CL_RG = $10B2;
  CL_RA = $10B3;
  CL_RGB = $10B4;
  CL_RGBA = $10B5;
  CL_BGRA = $10B6;
  CL_ARGB = $10B7;
  CL_INTENSITY = $10B8;
  CL_LUMINANCE = $10B9;
  { cl_channel_type }
  CL_SNORM_INT8 = $10D0;
  CL_SNORM_INT16 = $10D1;
  CL_UNORM_INT8 = $10D2;
  CL_UNORM_INT16 = $10D3;
  CL_UNORM_SHORT_565 = $10D4;
  CL_UNORM_SHORT_555 = $10D5;
  CL_UNORM_INT_101010 = $10D6;
  CL_SIGNED_INT8 = $10D7;
  CL_SIGNED_INT16 = $10D8;
  CL_SIGNED_INT32 = $10D9;
  CL_UNSIGNED_INT8 = $10DA;
  CL_UNSIGNED_INT16 = $10DB;
  CL_UNSIGNED_INT32 = $10DC;
  CL_HALF_FLOAT = $10DD;
  CL_FLOAT_TYPE = $10DE;
  { cl_mem_object_type }
  CL_MEM_OBJECT_BUFFER = $10F0;
  CL_MEM_OBJECT_IMAGE2D = $10F1;
  CL_MEM_OBJECT_IMAGE3D = $10F2;
  { cl_mem_info }
  CL_MEM_TYPE = $1100;
  CL_MEM_FLAGS_INFO = $1101;
  CL_MEM_SIZE = $1102;
  CL_MEM_HOST_PTR = $1103;
  CL_MEM_MAP_COUNT = $1104;
  CL_MEM_REFERENCE_COUNT = $1105;
  CL_MEM_CONTEXT = $1106;
  { cl_image_info }
  CL_IMAGE_FORMAT_INFO = $1110;
  CL_IMAGE_ELEMENT_SIZE = $1111;
  CL_IMAGE_ROW_PITCH = $1112;
  CL_IMAGE_SLICE_PITCH = $1113;
  CL_IMAGE_WIDTH = $1114;
  CL_IMAGE_HEIGHT = $1115;
  CL_IMAGE_DEPTH = $1116;
  { cl_addressing_mode }
  CL_ADDRESS_NONE = $1130;
  CL_ADDRESS_CLAMP_TO_EDGE = $1131;
  CL_ADDRESS_CLAMP = $1132;
  CL_ADDRESS_REPEAT = $1133;
  { cl_filter_mode }
  CL_FILTER_NEAREST = $1140;
  CL_FILTER_LINEAR = $1141;
  { cl_sampler_info }
  CL_SAMPLER_REFERENCE_COUNT = $1150;
  CL_SAMPLER_CONTEXT = $1151;
  CL_SAMPLER_NORMALIZED_COORDS = $1152;
  CL_SAMPLER_ADDRESSING_MODE = $1153;
  CL_SAMPLER_FILTER_MODE = $1154;
  { cl_map_flags - bitfield }
  CL_MAP_READ = 1 shl 0;
  CL_MAP_WRITE = 1 shl 1;
  { cl_program_info }
  CL_PROGRAM_REFERENCE_COUNT = $1160;
  CL_PROGRAM_CONTEXT = $1161;
  CL_PROGRAM_NUM_DEVICES = $1162;
  CL_PROGRAM_DEVICES = $1163;
  CL_PROGRAM_SOURCE = $1164;
  CL_PROGRAM_BINARY_SIZES = $1165;
  CL_PROGRAM_BINARIES = $1166;
  { cl_program_build_info }
  CL_PROGRAM_BUILD_STATUS = $1181;
  CL_PROGRAM_BUILD_OPTIONS = $1182;
  CL_PROGRAM_BUILD_LOG = $1183;
  { cl_build_status }
  CL_BUILD_SUCCESS = 0;
  CL_BUILD_NONE = -(1);
  CL_BUILD_ERROR = -(2);
  CL_BUILD_IN_PROGRESS = -(3);
  { cl_kernel_info }
  CL_KERNEL_FUNCTION_NAME = $1190;
  CL_KERNEL_NUM_ARGS = $1191;
  CL_KERNEL_REFERENCE_COUNT = $1192;
  CL_KERNEL_CONTEXT = $1193;
  CL_KERNEL_PROGRAM = $1194;
  { cl_kernel_work_group_info }
  CL_KERNEL_WORK_GROUP_SIZE = $11B0;
  CL_KERNEL_COMPILE_WORK_GROUP_SIZE = $11B1;
  CL_KERNEL_LOCAL_MEM_SIZE = $11B2;
  { cl_event_info }
  CL_EVENT_COMMAND_QUEUE = $11D0;
  CL_EVENT_COMMAND_TYPE = $11D1;
  CL_EVENT_REFERENCE_COUNT = $11D2;
  CL_EVENT_COMMAND_EXECUTION_STATUS = $11D3;
  { cl_command_type }
  CL_COMMAND_NDRANGE_KERNEL = $11F0;
  CL_COMMAND_TASK = $11F1;
  CL_COMMAND_NATIVE_KERNEL = $11F2;
  CL_COMMAND_READ_BUFFER = $11F3;
  CL_COMMAND_WRITE_BUFFER = $11F4;
  CL_COMMAND_COPY_BUFFER = $11F5;
  CL_COMMAND_READ_IMAGE = $11F6;
  CL_COMMAND_WRITE_IMAGE = $11F7;
  CL_COMMAND_COPY_IMAGE = $11F8;
  CL_COMMAND_COPY_IMAGE_TO_BUFFER = $11F9;
  CL_COMMAND_COPY_BUFFER_TO_IMAGE = $11FA;
  CL_COMMAND_MAP_BUFFER = $11FB;
  CL_COMMAND_MAP_IMAGE = $11FC;
  CL_COMMAND_UNMAP_MEM_OBJECT = $11FD;
  CL_COMMAND_MARKER = $11FE;
  CL_COMMAND_ACQUIRE_GL_OBJECTS = $11FF;
  CL_COMMAND_RELEASE_GL_OBJECTS = $1200;
  { command execution status }
  CL_COMPLETE = $0;
  CL_RUNNING = $1;
  CL_SUBMITTED = $2;
  CL_QUEUED = $3;
  { cl_profiling_info }
  CL_PROFILING_COMMAND_QUEUED = $1280;
  CL_PROFILING_COMMAND_SUBMIT = $1281;
  CL_PROFILING_COMMAND_START = $1282;
  CL_PROFILING_COMMAND_END = $1283;

  // DLL loading:
  {$IFDEF MSWINDOWS}
  DEFAULT_OPENCL_LIBRARY = 'OpenCL.DLL';
  {$ENDIF}
  {$IFDEF LINUX}
  DEFAULT_OPENCL_LIBRARY = ''; // Currently unknown.
  {$ENDIF}
  {$IFDEF DARWIN}
  DEFAULT_OPENCL_LIBRARY = ''; // Also currently unknown.
  {$ENDIF}
  CL_FUNCTION_NAMES : array[0..64] of string =(
    'clGetPlatformIDs', 'clGetPlatformInfo', 'clGetDeviceIDs',
    'clGetDeviceInfo', 'clCreateContext', 'clCreateContextFromType',
    'clRetainContext', 'clReleaseContext', 'clGetContextInfo',
    'clCreateCommandQueue', 'clRetainCommandQueue', 'clReleaseCommandQueue',
    'clGetCommmandQueueInfo', 'clSetCommandQueueProperty', 'clRetainMemObject',
    'clReleaseMemObject', 'clGetMemObjectInfo', 'clCreateBuffer',
    'clEnqueueReadBuffer', 'clEnqueueWriteBuffer', 'clEnqueueCopyBuffer',
    'clCreateImage2D', 'clCreateImage3D', 'clGetSupportedImageFormats',
    'clEnqueueReadImage', 'clEnqueueWriteImage', 'clEnqueueCopyImage',
    'clGetImageInfo', 'clEnqueueCopyImageToBuffer', 'clEnqueueCopyBufferToImage',
    'clEnqueueMapBuffer', 'clEnqueueMapImage', 'clEnqueueUnmapMemObject',
    'clCreateSampler', 'clRetainSampler', 'clReleaseSampler',
    'clGetSamplerInfo', 'clCreateProgramWithSource', 'clCreateProgramWithBinary',
    'clRetainProgram', 'clReleaseProgram', 'clBuildProgram', 'clUnloadCompiler',
    'clGetProgramInfo', 'clGetProgramBuildInfo', 'clCreateKernel',
    'clCreateKernelsInProgram', 'clRetainKernel', 'clReleaseKernel',
    'clSetKernelArg', 'clGetKernelInfo', 'clGetKernelWorkGroupInfo',
    'clEnqueueNDRangeKernel', 'clEnqueueTask', 'clEnqueueNativeKernel',
    'clWaitForEvents', 'clGetEventInfo', 'clRetainEvent',
    'clReleaseEvent', 'clEnqueueMarker', 'clEnqueueWaitForEvents',
    'clEnqueueBarrier', 'clGetEventProfilingInfo', 'clFlush', 'clFinish');

var
  clAPI : TclAPI;

  clGetPlatformIDs : TclGetPlatformIDs absolute clAPI.clGetPlatformIDs;
  clGetPlatformInfo : TclGetPlatformInfo absolute clAPI.clGetPlatformInfo;
  clGetDeviceIDs : TclGetDeviceIDs absolute clAPI.clGetDeviceIDs;
  clGetDeviceInfo : TclGetDeviceInfo absolute clAPI.clGetDeviceInfo;
  clCreateContext : TclCreateContext absolute clAPI.clCreateContext;
  clCreateContextFromType : TclCreateContextFromType absolute clAPI.clCreateContextFromType;
  clRetainContext : TclRetainContext absolute clAPI.clRetainContext;
  clReleaseContext : TclReleaseContext absolute clAPI.clReleaseContext;
  clGetContextInfo : TclGetContextInfo absolute clAPI.clGetContextInfo;
  clCreateCommandQueue : TclCreateCommandQueue absolute clAPI.clCreateCommandQueue;
  clRetainCommandQueue : TclRetainCommandQueue absolute clAPI.clRetainCommandQueue;
  clReleaseCommandQueue : TclReleaseCommandQueue absolute clAPI.clReleaseCommandQueue;
  clGetCommmandQueueInfo : TclGetCommmandQueueInfo absolute clAPI.clGetCommmandQueueInfo;
  clSetCommandQueueProperty : TclSetCommandQueueProperty absolute clAPI.clSetCommandQueueProperty;
  clRetainMemObject : TclRetainMemObject absolute clAPI.clRetainMemObject;
  clReleaseMemObject : TclReleaseMemObject absolute clAPI.clReleaseMemObject;
  clGetMemObjectInfo : TclGetMemObjectInfo absolute clAPI.clGetMemObjectInfo;
  clCreateBuffer : TclCreateBuffer absolute clAPI.clCreateBuffer;
  clEnqueueReadBuffer : TclEnqueueReadBuffer absolute clAPI.clEnqueueReadBuffer;
  clEnqueueWriteBuffer : TclEnqueueWriteBuffer absolute clAPI.clEnqueueWriteBuffer;
  clEnqueueCopyBuffer : TclEnqueueCopyBuffer absolute clAPI.clEnqueueCopyBuffer;
  clCreateImage2D : TclCreateImage2D absolute clAPI.clCreateImage2D;
  clCreateImage3D : TclCreateImage3D absolute clAPI.clCreateImage3D;
  clGetSupportedImageFormats : TclGetSupportedImageFormats absolute clAPI.clGetSupportedImageFormats;
  clEnqueueReadImage : TclEnqueueReadImage absolute clAPI.clEnqueueReadImage;
  clEnqueueWriteImage : TclEnqueueWriteImage absolute clAPI.clEnqueueWriteImage;
  clEnqueueCopyImage : TclEnqueueCopyImage absolute clAPI.clEnqueueCopyImage;
  clGetImageInfo : TclGetImageInfo absolute clAPI.clGetImageInfo;
  clEnqueueCopyImageToBuffer : TclEnqueueCopyImageToBuffer absolute clAPI.clEnqueueCopyImageToBuffer;
  clEnqueueCopyBufferToImage : TclEnqueueCopyBufferToImage absolute clAPI.clEnqueueCopyBufferToImage;
  clEnqueueMapBuffer : TclEnqueueMapBuffer absolute clAPI.clEnqueueMapBuffer;
  clEnqueueMapImage : TclEnqueueMapImage absolute clAPI.clEnqueueMapImage;
  clEnqueueUnmapMemObject : TclEnqueueUnmapMemObject absolute clAPI.clEnqueueUnmapMemObject;
  clCreateSampler : TclCreateSampler absolute clAPI.clCreateSampler;
  clRetainSampler : TclRetainSampler absolute clAPI.clRetainSampler;
  clReleaseSampler : TclReleaseSampler absolute clAPI.clReleaseSampler;
  clGetSamplerInfo : TclGetSamplerInfo absolute clAPI.clGetSamplerInfo;
  clCreateProgramWithSource : TclCreateProgramWithSource absolute clAPI.clCreateProgramWithSource;
  clCreateProgramWithBinary : TclCreateProgramWithBinary absolute clAPI.clCreateProgramWithBinary;
  clRetainProgram : TclRetainProgram absolute clAPI.clRetainProgram;
  clReleaseProgram : TclReleaseProgram absolute clAPI.clReleaseProgram;
  clBuildProgram : TclBuildProgram absolute clAPI.clBuildProgram;
  clUnloadCompiler : TclUnloadCompiler absolute clAPI.clUnloadCompiler;
  clGetProgramInfo : TclGetProgramInfo absolute clAPI.clGetProgramInfo;
  clGetProgramBuildInfo : TclGetProgramBuildInfo absolute clAPI.clGetProgramBuildInfo;
  clCreateKernel : TclCreateKernel absolute clAPI.clCreateKernel;
  clCreateKernelsInProgram : TclCreateKernelsInProgram absolute clAPI.clCreateKernelsInProgram;
  clRetainKernel : TclRetainKernel absolute clAPI.clRetainKernel;
  clReleaseKernel : TclReleaseKernel absolute clAPI.clReleaseKernel;
  clSetKernelArg : TclSetKernelArg absolute clAPI.clSetKernelArg;
  clGetKernelInfo : TclGetKernelInfo absolute clAPI.clGetKernelInfo;
  clGetKernelWorkGroupInfo : TclGetKernelWorkGroupInfo absolute clAPI.clGetKernelWorkGroupInfo;
  clEnqueueNDRangeKernel : TclEnqueueNDRangeKernel absolute clAPI.clEnqueueNDRangeKernel;
  clEnqueueTask : TclEnqueueTask absolute clAPI.clEnqueueTask;
  clEnqueueNativeKernel : TclEnqueueNativeKernel absolute clAPI.clEnqueueNativeKernel;
  clWaitForEvents : TclWaitForEvents absolute clAPI.clWaitForEvents;
  clGetEventInfo : TclGetEventInfo absolute clAPI.clGetEventInfo;
  clRetainEvent : TclRetainEvent absolute clAPI.clRetainEvent;
  clReleaseEvent : TclReleaseEvent absolute clAPI.clReleaseEvent;
  clEnqueueMarker : TclEnqueueMarker absolute clAPI.clEnqueueMarker;
  clEnqueueWaitForEvents : TclEnqueueWaitForEvents absolute clAPI.clEnqueueWaitForEvents;
  clEnqueueBarrier : TclEnqueueBarrier absolute clAPI.clEnqueueBarrier;
  clGetEventProfilingInfo : TclGetEventProfilingInfo absolute clAPI.clGetEventProfilingInfo;
  clFlush : TclFlush absolute clAPI.clFlush;
  clFinish : TclFinish absolute clAPI.clFinish;

  clLibraryHandle : TLibHandle;

  Function LoadOpenCL(Lib : string = DEFAULT_OPENCL_LIBRARY) : boolean;
  Function UnloadOpenCL() : boolean; // Used this name for consistency.
  Function FreeOpenCL() : boolean; // Alias for UnloadOpenCL(), provided for similiarity with the "GL" unit.

implementation

var
  clCoreAPI : packed array[0..64] of pointer absolute clAPI;

Function LoadOpenCL(Lib : string = DEFAULT_OPENCL_LIBRARY) : boolean;
  var
    I : integer;
  begin
    clLibraryHandle := DynLibs.LoadLibrary(Lib);
    Result := clLibraryHandle <> NilHandle;
    If Result then
    begin
      For I := Low(clCoreAPI) to High(clCoreAPI) do
      begin
        clCoreAPI[I] := DynLibs.GetProcAddress(clLibraryHandle, CL_FUNCTION_NAMES[I]);
        {$IFDEF CL_SAFELOADING}
        Result := Assigned(clCoreAPI[I]);
        If not Result then Exit;
        {$ENDIF}
      end;
    end;
end;

Function UnloadOpenCL() : boolean;
 var
   I : integer;
 begin
   For I := Low(clCoreAPI) to High(clCoreAPI) do clCoreAPI[I] := Nil;
   Result := DynLibs.UnloadLibrary(clLibraryHandle);
end;

Function FreeOpenCL() : boolean;
  begin
    Result := UnloadOpenCL();
end;

{$IFDEF CL_AUTOLOADING}
initialization
  If not LoadOpenCL('') then
  begin
    {$IFDEF CL_SAFELOADING}
    Raise Exception.Create('Unable to load OpenCL from the system''s default library!');
    {$ENDIF}
  end;

finalization
  If not UnloadOpenCL() then
  begin
    {$IFDEF CL_SAFELOADING}
    Raise Exception.Create('Failed to free OpenCL!');
    {$ENDIF}
  end;
{$ENDIF} // CL_AUTOLOADING

end.
