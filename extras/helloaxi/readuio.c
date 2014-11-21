#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>


int main(int argc, char *argv[]) {
    __clear_cache(argv,argv+2);
    if (argc < 2) {
        printf("usage: %s uiofile\n", argv[0]); 
    }
    char * file = argv[1];
    int fd = open(file,O_RDWR|O_SYNC);
    if (fd < 0) {
        perror(argv[0]);
        return 1;
    }
    unsigned data = 1;
    int err;
    err = write(fd,&data,4);
    
    if(err != 4) {
        perror(argv[0]);
        return 1;
    }
    
    printf("waiting for interrupt\n");
    
    err = read(fd, &data, 4);
    
    if(err != 4) {
        perror(argv[0]);
        return 1;
    }
    printf("interrupt received\n");
	return 0;
}


