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

#define IN 0
#define OUT 1

void usage(void)
{
	printf("*argv[0] -g <GPIO_ADDRESS> -i|-o <VALUE>\n");
	printf("    -g <GPIO_ADDR>   GPIO physical address\n");
	printf("    -i               Input from GPIO\n");
	printf("    -o <VALUE>       Output to GPIO\n");
	return;
}

int main(int argc, char *argv[])
{
	int c;
	int fd;
	int direction=IN;
	unsigned gpio_addr = 0;
	int value = 0;
	
	unsigned page_addr, page_offset;
	void *ptr;
	unsigned page_size=sysconf(_SC_PAGESIZE);

	printf("GPIO access through /dev/mem.\n", page_size);

	/* Parse command line arguements */
	while((c = getopt(argc, argv, "g:io:h")) != -1) {
		switch(c) {
		case 'g':
			gpio_addr=strtoul(optarg,NULL, 0);
			break;
		case 'i':
			direction=IN;
			break;
		case 'o':
			direction=OUT;
			value=atoi(optarg);
			break;
		case 'h':
			usage();
			return 0;
		default:
			printf("invalid option: %c\n", (char)c);
			usage();
			return -1;
		}
		
	}

	if (gpio_addr == 0) {
		printf("GPIO physical address is required.\n");
		usage();
		return -1;
	}
	
	/* Open /dev/mem file */
	fd = open ("/dev/mem", O_RDWR);
	if (fd < 1) {
		perror(argv[0]);
		return -1;
	}

	/* mmap the device into memory */
	page_addr = (gpio_addr & (~(page_size-1)));
	page_offset = gpio_addr - page_addr;
	ptr = mmap(NULL, page_size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, page_addr);

	if (direction == IN) {
	/* Read value from the device register */
		value = *((unsigned *)(ptr + page_offset));
		printf("gpio dev-mem test: input: %08x\n",value);
	} else {
	/* Write value to the device register */
		*((unsigned *)(ptr + page_offset)) = value;
	}
	

	munmap(ptr, page_size);

	return 0;
}


