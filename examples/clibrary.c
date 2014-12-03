#include <stdio.h>
#include <stdlib.h>

struct EmptyTapStruct{};
struct TapStruct{unsigned char tap;};

// write function declarations with matching names and signatures for
// the pipelines you exported

void pipeline1(void* input, void* output, struct EmptyTapStruct *tapStruct);
void pipeline2(void* input, void* output, struct TapStruct *tapStruct);

unsigned char* readFile(char* filename){
  FILE *f = fopen(filename, "rb");
  fseek(f, 0, SEEK_END);
  long fsize = ftell(f);
  fseek(f, 0, SEEK_SET);
  
  unsigned char *buf = malloc(fsize);
  fread(buf, fsize, 1, f);
  fclose(f);

  return buf;
}

void writePPM(char* filename, int width, int height, unsigned char* data){
  FILE *f = fopen(filename, "wb");
  fprintf(f,"P5\n%d %d\n255\n",width,height);
  fwrite(data,width*height,1,f);
  fclose(f);
}

int main(int argc, char** argv){
  unsigned char* input = readFile(argv[1]);

  void* output1 = malloc(128*64);
  struct EmptyTapStruct emptyTapStruct;
  pipeline1(input,output1,&emptyTapStruct);
  writePPM(argv[2],128,64,output1);

  void* output2 = malloc(128*64);
  struct TapStruct tapStruct;
  tapStruct.tap=100;
  pipeline2(input,output2,&tapStruct);
  writePPM(argv[3],128,64,output2);

  return 0;
}
