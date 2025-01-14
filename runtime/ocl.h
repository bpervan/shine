#ifndef SHINE_OCL_H
#define SHINE_OCL_H

#define CL_TARGET_OPENCL_VERSION 120
#define CL_USE_DEPRECATED_OPENCL_1_2_APIS
#ifdef __APPLE__
  #include "OpenCL/opencl.h"
#else
  #include "CL/cl.h"
#endif

struct ContextImpl {
  cl_context inner;
  cl_platform_id platform;
  cl_device_id device;
  cl_command_queue queue;
};

struct KernelImpl {
  cl_kernel inner;
  cl_program program;
};

typedef cl_mem DeviceBuffer;

const char* oclErrorToString(cl_int error);
bool oclReportError(cl_int error, const char* msg);
void oclFatalError(cl_int error, const char* msg);

#define KARG(val) { sizeof(val), &val }

#endif