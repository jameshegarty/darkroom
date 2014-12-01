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
	
	int ifd = open("/dev/uio0",O_RDWR|O_SYNC);
    if (ifd < 0) {
        perror(argv[0]);
        return 1;
    }
    
    printf("mapping %08x\n",copy_addr);
    void * copy_ptr = mmap(NULL, 2*len, PROT_READ|PROT_WRITE, MAP_SHARED, fd, copy_addr);
    volatile int * data = (int*)copy_ptr;    
    
    /* mmap the device into memory */
    void * config_ptr = mmap(NULL, page_size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, gpio_addr);
    
    for(int i = 0; i < len/sizeof(int); i++) {
        data[i] = i + check;
    }
    
    unsigned idata;
    int err;
    fcntl(ifd,F_SETFL,O_RDWR|O_SYNC|O_NONBLOCK);
    do {
        err = read(ifd,&idata,4);
        if (err == 4)
            printf("found interrupt %d\n",idata);
    } while(err == 4);
    
    idata = 1; //enable? the interrupt
    err = write(ifd, &idata, 4);
    if (err != 4) {
        perror(argv[0]);
        return 1;
    }
    
    fcntl(ifd,F_SETFL,O_RDWR|O_SYNC);
    
    volatile Conf * conf = (Conf*) config_ptr;
    conf->src = copy_addr;
    conf->dest = copy_addr + len;
    conf->len = len;
    conf->cmd = 3;
       
    sleep(1);
    if(check) { 
        printf("waiting for interrupt\n");
        err = read(ifd, &idata, 4);
        if(err != 4) {
            perror(argv[0]);
            return 1;
        }
        printf("idata = %d\n",idata);
    }
    
    for(int i = len/sizeof(int) - 1; i >= 0; i--) {
        unsigned a = data[i];
        unsigned b = data[i+len/sizeof(int)];
        if(a != b || a != i + check) {
            printf("%d: mismatch: %08x, %08x\n",i,a,b);
        } else {
            //printf("%d matches\n",i);
        }
    }

    return 0;
}


