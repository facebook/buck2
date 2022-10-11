#include <stdio.h>
#include "info.h"

int main(void) {
	printf("os type: %s\n", get_os_type());
	printf("os release: %s\n", get_os_release());

	printf("\ncpu num: %u\n", get_cpu_num());
	printf("cpu speed: %lu\n", get_cpu_speed());

	LoadAvg la = get_loadavg();
	printf("\nloadavg: %f %f %f\n", la.one, la.five, la.fifteen);

	printf("proc total: %lu\n", get_proc_total());

	MemInfo mi = get_mem_info();
	printf("\nmem:\ntotal %llu, avail %llu, free %llu\n",
	     mi.total, mi.avail, mi.free);
	printf("buffers %llu, cached %llu\n", mi.buffers, mi.cached);
	printf("swap: total %llu, free %llu\n", mi.swap_total, mi.swap_free);

	DiskInfo di = get_disk_info();
	printf("\ndisk: total %llu, free %llu\n", di.total, di.free);

	return 0;
}
