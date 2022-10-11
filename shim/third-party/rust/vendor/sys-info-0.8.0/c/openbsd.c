#include <sys/param.h>
#include <sys/types.h>
#include <sys/mount.h>
#include <sys/swap.h>
#include <sys/sysctl.h>
#include <sys/utsname.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "info.h"

static const char *os_release;

static pthread_once_t once_init_openbsd;
static void init_openbsd(void) {
	struct utsname un;

	if (uname(&un) == -1)
		return;
	os_release = strdup(un.release);
}

const char *get_os_release(void) {
	pthread_once(&once_init_openbsd, init_openbsd);
	return (os_release);
}

unsigned long get_cpu_speed(void) {
	unsigned int mhz;
	int mib[2], error;
	size_t len;

	mib[0] = CTL_HW;
	mib[1] = HW_CPUSPEED;
	len = sizeof(mhz);
	error = sysctl(mib, 2, &mhz, &len, NULL, 0);
	if (error == -1)
		return 0;

	return mhz;
}

unsigned long get_proc_total(void) {
	struct kinfo_proc *kp, *kpp;
	int mib[6], count, error;
	size_t len;

	mib[0] = CTL_KERN;
	mib[1] = KERN_NPROCS;
	len = sizeof(count);
	error = sysctl(mib, 2, &count, &len, NULL, 0);
	if (error == -1)
		return (0);

	kp = calloc(count, sizeof(*kp));
	if (kp == NULL)
		return (0);

	mib[0] = CTL_KERN;
	mib[1] = KERN_PROC;
	mib[2] = KERN_PROC_ALL;
	mib[3] = 0;
	mib[4] = sizeof(*kp);
	mib[5] = count;
	len = count * sizeof(*kp);
	error = sysctl(mib, 6, kp, &len, NULL, 0);
	if (error == -1) {
		free(kp);
		return (0);
	}

	for (count = 0, kpp = kp; (char *)kpp < (char *)kp + len; kpp++) {
		if (kpp->p_pid == 0)
			continue;
		count++;
	}
	free(kp);
	return (count);
}

int32_t get_mem_info_bsd(struct MemInfo *mi) {
	struct uvmexp uv;
	struct bcachestats bc;
	struct swapent *sw;
	int mib[3], error, i, ns;
	size_t len;

	mib[0] = CTL_VM;
	mib[1] = VM_UVMEXP;
	len = sizeof(uv);
	error = sysctl(mib, 2, &uv, &len, NULL, 0);
	if (error == -1)
		goto fail;

	mib[0] = CTL_VFS;
	mib[1] = VFS_GENERIC;
	mib[2] = VFS_BCACHESTAT;
	len = sizeof(bc);
	error = sysctl(mib, 3, &bc, &len, NULL, 0);
	if (error == -1)
		goto fail;

	mi->total = (uint64_t)uv.npages * uv.pagesize / 1024;
	mi->avail = 0;
	mi->free = (uint64_t)uv.free * uv.pagesize / 1024;
	mi->cached = bc.numbufpages * uv.pagesize / 1024;
	mi->buffers = 0;
	mi->swap_total = 0;
	mi->swap_free = 0;

	ns = swapctl(SWAP_NSWAP, 0, 0);
	sw = (ns > 0) ? calloc(ns, sizeof(*sw)) : NULL;
	if (sw != NULL && (swapctl(SWAP_STATS, sw, ns) == ns)) {
		for (i = 0; i < ns; i++) {
			if (!(sw[i].se_flags & SWF_ENABLE))
				continue;
			mi->swap_total += sw[i].se_nblks / (1024 / DEV_BSIZE);
			mi->swap_free += (sw[i].se_nblks - sw[i].se_inuse) /
				(1024 / DEV_BSIZE);
		}
	}
	free(sw);
	return (0);

fail:
	return (-1);
}

int32_t get_disk_info_bsd(DiskInfo *di) {
	struct statfs *sfs, *sf;
	int i, nmounts;
	uint64_t dtotal, dfree;
	int32_t res;

	dtotal = 0;
	dfree = 0;
	sfs = NULL;
	res = -1;

	nmounts = getfsstat(NULL, 0, MNT_WAIT);
	if (nmounts == -1)
		goto fail;
	sfs = calloc(nmounts, sizeof(*sfs));
	if (sfs == NULL)
		goto fail;
	nmounts = getfsstat(sfs, nmounts * sizeof(*sfs), MNT_WAIT);
	if (nmounts == -1)
		goto fail;

	for (i = 0; i < nmounts; i++) {
		sf = &sfs[i];
		if ((sf->f_flags & MNT_LOCAL) != MNT_LOCAL)
			continue;
		dtotal += sf->f_blocks * sf->f_bsize;
		dfree += sf->f_bfree * sf->f_bsize;
	}

	di->total = dtotal / 1000;
	di->free = dfree / 1000;
	res = 0;

fail:
	free(sfs);
	return (res);
}
