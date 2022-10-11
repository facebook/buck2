#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/sysinfo.h>
#include <sys/statvfs.h>

#include "info.h"

#define DFHASHSIZE 101
#define MOUNTS "/proc/mounts"
static const char *os_type = "Linux";

/* Internal Declarations */
struct nlist {
	struct nlist *next;
	char *name;
};

unsigned int DFhash(const char*);
struct nlist *seen_before(struct nlist**, const char*);
void DFcleanup(struct nlist**);
int remote_mount(const char*, const char*);
float device_space(struct nlist**, char*, char*, double*, double*);


/* Get information */
/*
  get_os_type & get_os_release
  /proc/sys/kernel
*/

const char *get_os_type(void) {
	return os_type;
}

const char *get_os_release(void) {
	return "";
}

unsigned int get_cpu_num(void) {
	return get_nprocs();
}


/*
  get_cpu_speed
  /sys/devices/system/cpu/cpu0
*/

unsigned long get_cpu_speed(void) {
    return 0;
}

/*
  get_loadavg & get_proc_total
  /proc/loadavg
*/

LoadAvg get_loadavg(void) {
    static LoadAvg avg;
    return avg;
}

unsigned long get_proc_total(void) {
    return 0;
}

/*
  get_mem_info
  /proc/meminfo
*/

MemInfo get_mem_info(void) {
    static MemInfo info;
    return info;
}

DiskInfo get_disk_info(void) {
	FILE *mounts;
	char procline[1024];
	struct nlist *DFhashvector[DFHASHSIZE] = {0};
	char *mount, *device, *type, *mode, *other;
	float thispct, max=0.0;
	double dtotal=0.0, dfree=0.0;
	DiskInfo di;
	
	di.total = 0;
	di.free = 0;

	mounts = fopen(MOUNTS,"r");
	if (!mounts) {
		return di;
	}
	while ( fgets(procline, sizeof(procline), mounts) ) {
		device = procline;
		mount = index(procline, ' ');
		if (mount == NULL) continue;
		*mount++ = '\0';
		type = index(mount, ' ');
		if (type == NULL) continue;
		*type++ = '\0';
		mode = index(type, ' ');
		if (mode == NULL) continue;
		*mode++ = '\0';
		other = index(mode, ' ');
		if (other != NULL) *other = '\0';
		if (!strncmp(mode, "ro", 2)) continue;
		if (remote_mount(device, type)) continue;
		if (strncmp(device, "/dev/", 5) != 0 &&
		    strncmp(device, "/dev2/", 6) != 0) continue;
		thispct = device_space(DFhashvector, mount, device, &dtotal, &dfree);
		if (!max || max<thispct)
			max = thispct;
	}
	fclose(mounts);

	DFcleanup(DFhashvector);
	di.total = dtotal / 1000;
	di.free = dfree / 1000;
	
	return di;
}

/* Internal Definitions */
unsigned int DFhash(const char *s)
{
	unsigned int hashval;
	for (hashval=0; *s != '\0'; s++)
		hashval = *s + 31 * hashval;
	return hashval % DFHASHSIZE;
}

/* From K&R C book, pp. 144-145 */
struct nlist * seen_before(struct nlist **DFhashvector, const char *name)
{
	struct nlist *found=0, *np;
	unsigned int hashval;

	/* lookup */
	hashval=DFhash(name);
	for (np=DFhashvector[hashval]; np; np=np->next) {
		if (!strcmp(name,np->name)) {
			found=np;
			break;
		}
	}
	if (!found) {    /* not found */
		np = (struct nlist *) malloc(sizeof(*np));
		if (!np || !(np->name = (char *) strdup(name)))
			return NULL;
		np->next = DFhashvector[hashval];
		DFhashvector[hashval] = np;
		return NULL;
	}
	else /* found name */
		return found;
}

void DFcleanup(struct nlist **DFhashvector)
{
	struct nlist *np, *next;
	int i;
	for (i=0; i<DFHASHSIZE; i++) {
		/* Non-standard for loop. Note the last clause happens at the end of the loop. */
		for (np = DFhashvector[i]; np; np=next) {
			next=np->next;
			free(np->name);
			free(np);
		}
		DFhashvector[i] = 0;
	}
}

int remote_mount(const char *device, const char *type)
{
	/* From ME_REMOTE macro in mountlist.h:
	      A file system is `remote' if its Fs_name contains a `:'
	      or if (it is of type smbfs and its Fs_name starts with `//'). */
	return ((strchr(device,':') != 0)
		|| (!strcmp(type, "smbfs") && device[0]=='/' && device[1]=='/')
		|| (!strncmp(type, "nfs", 3)) || (!strcmp(type, "autofs"))
		|| (!strcmp(type,"gfs")) || (!strcmp(type,"none")) );
}

float device_space(struct nlist **DFhashvector, char *mount, char *device, double *total_size, double *total_free)
{
	struct statvfs svfs;
	double blocksize;
	double free;
	double size;
	/* The percent used: used/total * 100 */
	float pct=0.0;

	/* Avoid multiply-mounted disks - not done in df. */
	if (seen_before(DFhashvector, device)) return pct;

	if (statvfs(mount, &svfs)) {
		/* Ignore funky devices... */
		return pct;
	}

	free = svfs.f_bavail;
	size  = svfs.f_blocks;
	blocksize = svfs.f_bsize;
	/* Keep running sum of total used, free local disk space. */
	*total_size += size * blocksize;
	*total_free += free * blocksize;
	/* The percentage of space used on this partition. */
	pct = size ? ((size - free) / (float) size) * 100 : 0.0;
	return pct;
}



