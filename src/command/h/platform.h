#ifndef LILY_PLATFORM_H
#define LILY_PLATFORM_H

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <stdio.h>

CAMLprim value get_os(value unit);
CAMLprim value get_arch(value unit);

#if defined (__linux__)
CAMLprim value get_os(value unit) {
	char *os = "linux";
	return caml_copy_string(os);
}
#elif defined(_WIN32) || defined(_WIN64)
CAMLprim value get_os(value unit) {
	char *os = "windows";
	return caml_copy_string(os);
}
#elif defined(__APPLE__) || defined(__MACH__)
CAMLprim value get_os(value unit) {
	char *os = "macos";
	return caml_copy_string(os);
}
#elif defined(__DragonFly__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
CAMLprim value get_os(value unit) {
	char *os = "bsd";
	return caml_copy_string(os);
}
#else
CAMLprim value get_os(value unit) {
	char *os = "unknown";
	return caml_copy_string(os);
}
#endif // OS

#if defined (__x86_64__)
CAMLprim value get_arch(value unit) {
	char *arch = "x86_64";
	return caml_copy_string(arch);
}
#elif defined (__i386__)
CAMLprim value get_arch(value unit) {
	char *arch = "x86";
	return caml_copy_string(arch);
}
#elif defined (__arm__)
CAMLprim value get_arch(value unit) {
	char *arch = "arm";
	return caml_copy_string(arch);
}
#elif defined(__aarch64__)
CAMLprim value get_arch(value unit) {
	char *arch = "aarch64";
	return caml_copy_string(arch);
}
#elif defined(__mips__)
CAMLprim value get_arch(value unit) {
	char *arch = "mips";
	return caml_copy_string(arch);
}
#elif defined(__powerpc__)
CAMLprim value get_arch(value unit) {
	char *arch = "powerpc";
	return caml_copy_string(arch);
}
#else 
CAMLprim value get_arch(value unit) {
	char *arch = "unknown";
	return caml_copy_string(arch);
}
#endif // ARCH

#endif // LILY_PLATFORM_H
