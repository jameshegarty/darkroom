/*
 * This test application is to read/write data directly from/to the device 
 * from userspace. 
 * 
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <assert.h>

void usage(void) {
	printf("*argv[0] -g <GPIO_ADDRESS> -i|-o <VALUE>\n");
	printf("    -g <GPIO_ADDR>   GPIO physical address\n");
	printf("    -i               Input from GPIO\n");
	printf("    -o <VALUE>       Output to GPIO\n");
	return;
}

typedef struct {
    int cmd;
    int src;
    int dest;
    int len;
} Conf;

int main(int argc, char *argv[]) {

	unsigned gpio_addr = 0x70000000;
	unsigned copy_addr = atoi(argv[1]);
	unsigned len = atoi(argv[2]);
	unsigned check = atoi(argv[3]);
	assert(len % (8*16) == 0);

	unsigned page_size = sysconf(_SC_PAGESIZE);

	printf("GPIO access through /dev/mem.\n", page_size);

	if (gpio_addr == 0) {
		printf("GPIO physical address is required.\n");
		usage();
		return -1;
	}
	
	int fd = open ("/dev/mem", O_RDWR);
	if (fd < 1) {
		perror(argv[0]);
		return -1;
	}
    
    if (!check) {
        /* mmap the device into memory */
        unsigned page_addr = (gpio_addr & (~(page_size-1)));
        void * ptr = mmap(NULL, page_size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, page_addr);

        volatile Conf * conf = (Conf*) ptr;
    
        conf->src = copy_addr;
        conf->dest = copy_addr + len;
        conf->len = len;
        conf->cmd = 3;
        munmap(ptr, page_size);
    
        return 0;
    } else {
        unsigned page_addr = (copy_addr & (~(page_size-1)));
        printf("mapping %08x\n",page_addr);
	    void * ptr = mmap(NULL, 3*len, PROT_READ|PROT_WRITE, MAP_SHARED, fd, page_addr);
        volatile int * data = (int*)ptr;
        for(int i = 0; i < len/sizeof(int); i++) {
            unsigned a = data[i];
            unsigned b = data[i+len/sizeof(int)];
            if(a != b) {
                printf("%d: mismatch: %08x, %08x\n",i,a,b);
            } else {
                //printf("%d matches\n",i);
            }
        }

        return 0;
    }
}


