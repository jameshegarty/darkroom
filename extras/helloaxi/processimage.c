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

FILE* openImage(char* filename, int* numbytes){
  FILE* infile = fopen(filename, "rb");
  if(infile==NULL){
    printf("File not found %s\n",filename);
    exit(1);
  }
  fseek(infile, 0L, SEEK_END);
  *numbytes = ftell(infile);
  fseek(infile, 0L, SEEK_SET);

  return infile;
}

void loadImage(FILE* infile,  volatile void* address, int numbytes){
  int outlen = fread(address, sizeof(char), numbytes, infile);
  if(outlen!=numbytes){
    printf("ERROR READING\n");
  }

  fclose(infile);
}

int saveImage(char* filename,  volatile void* address, int numbytes){
  FILE* outfile = fopen(filename, "wb");
  if(outfile==NULL){
    printf("could not open for writing %s\n",filename);
    exit(1);
  }
  int outlen = fwrite(address,1,numbytes,outfile);
  if(outlen!=numbytes){
    printf("ERROR WRITING\n");
  }

  fclose(outfile);
}

int main(int argc, char *argv[]) {
	unsigned gpio_addr = 0x70000000;
	unsigned copy_addr = atoi(argv[1]);

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

  unsigned len;
  FILE* imfile = openImage(argv[2], &len);
  printf("file LEN %d\n",len);
  assert(len % (8*16) == 0);

  printf("mapping %08x\n",copy_addr);
  void * ptr = mmap(NULL, 2*len, PROT_READ|PROT_WRITE, MAP_SHARED, fd, copy_addr);

  loadImage( imfile, ptr, len );
  //saveImage("before.raw",ptr,len);

  // mmap the device into memory 
  void * gpioptr = mmap(NULL, page_size, PROT_READ|PROT_WRITE, MAP_SHARED, fd, gpio_addr);
  
  volatile Conf * conf = (Conf*) gpioptr;
  
  conf->src = copy_addr;
  conf->dest = copy_addr + len;
  conf->len = len;
  conf->cmd = 3;

  usleep(10000);

  saveImage(argv[3],ptr+len,len);

  return 0;
}


