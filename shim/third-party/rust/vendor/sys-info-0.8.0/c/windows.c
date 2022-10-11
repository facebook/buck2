#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <windows.h>
#include <psapi.h>
#include <powerbase.h>

#include "info.h"

#define LEN 20
#define MAXPROCESSES 1024
static const char *os_type = "Windows";

/* Internal Declarations */
static double calculate_cpu_load(unsigned long long, unsigned long long);
static unsigned long long file_time_to_ull(const FILETIME);

/* Get information */
const char *get_os_type(void) {
	return os_type;
}

const char *get_os_release(void) {
	OSVERSIONINFO osvi;
	char *s = malloc(LEN);

	ZeroMemory(&osvi, sizeof(osvi));
	osvi.dwOSVersionInfoSize = sizeof(osvi);

	if (GetVersionEx(&osvi))
		snprintf(s, LEN, "%ld.%ld.%ld",
			 osvi.dwMajorVersion, osvi.dwMinorVersion, osvi.dwBuildNumber);
	else
		strncpy(s, "unknown", LEN);
	s[LEN - 1] = '\0';

	return s;
}

unsigned int get_cpu_num(void) {
	unsigned int num;
	SYSTEM_INFO sys_info;

	GetSystemInfo(&sys_info);
	num = sys_info.dwNumberOfProcessors;

	return num;
}

// https://docs.microsoft.com/en-us/windows/win32/power/processor-power-information-str#requirements
// Note that this structure definition was accidentally omitted from WinNT.h. This error will be corrected in the future.
// In the meantime, to compile your application, include the structure definition contained in this topic in your source code.
typedef struct _PROCESSOR_POWER_INFORMATION
{
	ULONG Number;
	ULONG MaxMhz;
	ULONG CurrentMhz;
	ULONG MhzLimit;
	ULONG MaxIdleState;
	ULONG CurrentIdleState;
} PROCESSOR_POWER_INFORMATION;

unsigned long get_cpu_speed(void) {
	unsigned int num = get_cpu_num();
	unsigned int power_info_len = num * sizeof(PROCESSOR_POWER_INFORMATION);
	PROCESSOR_POWER_INFORMATION *power_info = malloc(power_info_len);

	CallNtPowerInformation(ProcessorInformation, NULL, 0, power_info, power_info_len);

	unsigned int speed = 0;
	for (unsigned int i = 0; i < num; i++) {
		if (speed < power_info[i].MaxMhz) {
			speed = power_info[i].MaxMhz;
		}
	}

	free(power_info);
	return speed;
}

LoadAvg get_loadavg(void) {
	FILETIME idle_time, kernel_time, user_time;
	LoadAvg la;
	double load =  GetSystemTimes(&idle_time, &kernel_time, &user_time) ?
		calculate_cpu_load(file_time_to_ull(idle_time),
				   file_time_to_ull(kernel_time) +
				   file_time_to_ull(user_time)) :
		-1.0;
	
	la.one = load;
	la.five = load;
	la.fifteen = load;
	return la;
}

unsigned long get_proc_total(void) {
	DWORD aprocesses[MAXPROCESSES], cb_needed, cprocesses;

	if (!EnumProcesses(aprocesses, sizeof(aprocesses), &cb_needed))
		cprocesses = 0;
	else
		cprocesses = cb_needed / sizeof(unsigned long);
	return cprocesses;
}

MemInfo get_mem_info(void) {
	MEMORYSTATUSEX stat;
	/* DWORDLONG size; */
	MemInfo mi;
	
	stat.dwLength = sizeof(stat);
	if (GlobalMemoryStatusEx(&stat)) {
		mi.total = stat.ullTotalPhys / 1024;
		mi.avail = 0;
		mi.free  = stat.ullAvailPhys / 1024;
		mi.cached = 0;
		mi.buffers = 0;
		mi.swap_total = (stat.ullTotalPageFile - stat.ullTotalPhys) / 1024;
		mi.swap_free = (stat.ullAvailPageFile - stat.ullAvailPhys) / 1024;
	} else {
		memset(&mi, 0, sizeof(mi));
	}
	return mi;
}

DiskInfo get_disk_info(void) {
	DWORD cluser, sector, free, total;
	double tmp;
	DiskInfo di;
	
	if (GetDiskFreeSpace(NULL, &cluser, &sector, &free, &total)) {
		tmp = cluser * sector;
		di.total = tmp * total / 1024;
		di.free = tmp * free / 1024;
	} else {
		di.total = 0;
		di.free = 0;
	}
	return di;
}


/* Internal definitions */
double calculate_cpu_load(unsigned long long idle_ticks,
			  unsigned long long total_ticks) {
	static unsigned long long _prev_total_ticks = 0;
	static unsigned long long _prev_idle_ticks = 0;
	unsigned long long total_ticks_since_last_time = total_ticks -
		_prev_total_ticks;
	unsigned long long idle_ticks_since_last_time = idle_ticks -
		_prev_idle_ticks;
	double ret = 1.0 - ((total_ticks_since_last_time > 0) ?
			    ((double)idle_ticks_since_last_time) /
			    total_ticks_since_last_time :
			    0);
	_prev_total_ticks = total_ticks;
	_prev_idle_ticks = idle_ticks;
	return ret;
}

unsigned long long file_time_to_ull(const FILETIME ft) {
	return (((unsigned long long)(ft.dwHighDateTime)) << 32) |
		((unsigned long long)ft.dwLowDateTime);
}
	
	


