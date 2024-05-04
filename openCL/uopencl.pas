unit uOpenCL;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ctypes, cl;

type

  { TOpenCL }

  TOpenCL = object
  private
    numPlatforms: cl_uint;
    size: csize_t;
    PlatformIDs: array of cl_platform_id;
    _platform: cl_platform_id;
    _name: string;
    devices: array of cl_device_id;
    device: cl_device_id;
    context: cl_context;
    queue: cl_command_queue;
    err_code: cl_int;
    kernel: cl_kernel;
    _program: cl_program;

  public
    function initByName(_pname: string): cl_int;
    procedure Free;
    procedure Release(p: cl_mem);
    procedure freePrg;
    function compileFile(FileSource, kernelFunc: string): boolean;
    function compile(Source, kernelFunc: string): boolean;
    function buffer(sz: cl_uint): cl_mem;
    function buffer<T>(a: array of T): cl_mem;
    procedure setArg(ix: cl_uint; sz: csize_t; v: pointer);
    procedure setArgs(const args: array of const);
    procedure Write(p: pointer; buff: cl_mem; sz: csize_t);
    procedure Write<T>(const a: array of T; buff: cl_mem);
    procedure run(sz: csize_t);
    procedure Read(p: pointer; buff: cl_mem; sz: csize_t);
    procedure Read<T>(const a: array of T; buff: cl_mem);

    procedure AMDdevice;
    procedure NVIDIAdevice;
    procedure INTELdevice;

    function ok: boolean;
  private
  end;

  TCLBuffer = cl_mem;

function bytesOf<T>(a: array of T): integer;

implementation

function bytesOf<T>(a: array of T): integer;
begin
  Result := length(a) * sizeof(T);
end;



{ TOpenCL }

function TOpenCL.initByName(_pname: string): cl_int;
begin
  kernel := nil;
  _program := nil;

  Result := CL_DEVICE_NOT_FOUND;
  if clGetPlatformIDs(0, nil, @numPlatforms) = CL_SUCCESS then // platforms
  begin
    setLength(PlatformIDs, numPlatforms);
    if clgetPlatformIDs(numPlatforms, @PlatformIDs[0], nil) = CL_SUCCESS then
    begin
      _platform := PlatformIDs[0]; // names
      clgetPlatformInfo(_platform, CL_PLATFORM_NAME, 0, nil, size);
      setlength(_name, size);
      clgetPlatformInfo(_platform, CL_PLATFORM_NAME, size, @_name[1], size);

      if pos(_pname, _name) <> 0 then
      begin
        setLength(devices, 1);  // deivces, context, queue
        clgetDeviceIDs(_platform, CL_DEVICE_TYPE_ALL, 1, @devices[0], nil);
        device := devices[0];
        context := clCreateContext(nil, 1, @device, nil, nil, err_code);

        queue := clCreateCommandQueue(context, device, 0, err_code);
        Result := CL_SUCCESS;
      end;
    end;
  end;
end;

procedure TOpenCL.Free;
begin
  if context <> nil then clreleaseContext(context);
  if queue <> nil then clreleaseCommandQueue(queue);
  if _program <> nil then clReleaseProgram(_program);
  if kernel <> nil then  clReleaseKernel(kernel);

  context := nil;
  queue := nil;

  _program := nil;
  kernel := nil;
end;

procedure TOpenCL.Release(p: cl_mem);
begin
  clReleaseMemObject(p);
end;

procedure TOpenCL.freePrg;
begin
  if _program <> nil then clReleaseProgram(_program);
  if kernel <> nil then  clReleaseKernel(kernel);
end;

function TOpenCL.compileFile(FileSource, kernelFunc: string): boolean;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  Lines.LoadFromFile(FileSource);
  Result := compile(Lines.Text, kernelFunc);
  Lines.Free;
end;

function TOpenCL.compile(Source, kernelFunc: string): boolean;
var
  pch: PChar;
begin
  if _program <> nil then clReleaseProgram(_program); // release if used
  if kernel <> nil then  clReleaseKernel(kernel);

  pch := PChar(Source);
  _program := clcreateProgramWithSource(context, 1, @pch, nil, err_code);
  err_code := clbuildProgram(_program, 1, @device, nil, nil, nil);
  kernel := clcreateKernel(_program, PChar(kernelFunc), err_code);

  Result := err_code = CL_SUCCESS;
end;

function TOpenCL.buffer(sz: cl_uint): cl_mem;
begin
  Result := clCreateBuffer(context, CL_MEM_READ_WRITE, sz, nil, err_code);
end;

function TOpenCL.buffer<T>(a: array of T): cl_mem;
begin
  Result := clCreateBuffer(context, CL_MEM_READ_WRITE,
    cl_uint(length(a) * sizeof(T)), nil, err_code);
end;

procedure TOpenCL.setArg(ix: cl_uint; sz: csize_t; v: pointer);
begin
  err_code := clSetKernelArg(kernel, ix, sz, v);
end;

procedure TOpenCL.setArgs(const args: array of const);
var
  i: integer;
begin
  for i := 0 to high(args) do
  begin
    case args[i].vtype of
      vtPointer: setArg(i, sizeof(pointer), @args[i].vpointer);
      vtInteger: setArg(i, sizeof(integer), @args[i].vinteger);
      vtInt64: setArg(i, sizeof(int64), @args[i].vint64);
    end;
  end;

end;

procedure TOpenCL.Write(p: pointer; buff: cl_mem; sz: csize_t);
begin
  err_code := clEnqueueWriteBuffer(queue, buff, CL_FALSE, 0, sz, p, 0, nil, nil);
end;

procedure TOpenCL.Write<T>(const a: array of T; buff: cl_mem);
begin
  err_code := clEnqueueWriteBuffer(queue, buff, CL_FALSE, 0, length(a) *
    sizeof(T), @a[0], 0, nil, nil);
end;

procedure TOpenCL.run(sz: csize_t);
const
  gws: array of csize_t = [0, 0, 0];
begin
  gws[0] := sz;
  err_code := clenqueueNDRangeKernel(queue, kernel, 1, nil, @gws[0], nil, 0, nil, nil);
end;

procedure TOpenCL.Read(p: pointer; buff: cl_mem; sz: csize_t);
begin
  clenqueueReadBuffer(queue, buff, CL_TRUE, 0, sz, p, 0, nil, nil);
end;

procedure TOpenCL.Read<T>(const a: array of T; buff: cl_mem);
begin
  clenqueueReadBuffer(queue, buff, CL_TRUE, 0, length(a) * sizeof(T),
    @a[0], 0, nil, nil);
end;

procedure TOpenCL.AMDdevice;
begin
  initByName('AMD');
end;

procedure TOpenCL.NVIDIAdevice;
begin
  initByName('NVIDIA');
end;

procedure TOpenCL.INTELdevice;
begin
  initByName('INTEL');
end;

function TOpenCL.ok: boolean;
begin
  Result := err_code = CL_SUCCESS;
end;


end.
