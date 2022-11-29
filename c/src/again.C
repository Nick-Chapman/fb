
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/time.h> // gettimeofday()

typedef unsigned int u32;
typedef unsigned long u64;

const int physical_width = 1920;
const int physical_height = 1080;

const int portal_width = 900;
const int portal_height = 600;
const int life_scale = 1;

const int life_width = portal_width / life_scale;
const int life_height = portal_height / life_scale;

const u32 black = 0x00000000;
const u32 red = 0x00ff0000;
const u32 green = 0x0000ff00;
const u32 blue = 0x000000ff;
const u32 white = 0x00ffffff;

u64 wallclock_time() { //in micro-seconds
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return tv.tv_sec*(u64)1000000+tv.tv_usec;
}

void draw_boundary_rectangle(u32* fb) { // -->fb

  int W = life_width;
  int H = life_height;
  int S = life_scale;
  int off_y = (physical_height - S*H) / 2;
  int off_x = (physical_width - S*W) / 2;

/*
width=4; height=3

  o    o
   xxxx
   xxxx
   xxxx
  o
*/
  // 'x' prefix indicates external to the portal
  int xtl = off_x - 1 + (physical_width * (off_y - 1));
  int xtr = xtl + S*W + 1;
  int xbl = xtl + (S*H + 1) * physical_width;
/*
  -----|
  |xxxx|
  |xxxx|
  |xxxx|
  |-----
*/
  for (int i = 0; i <= S*W; i++) {
    fb[xtl+i] = blue;
    fb[xbl+i+1] = blue;
  }
  for (int i = 0; i <= S*H; i++) {
    fb[xtl + physical_width * (i+1)] = blue;
    fb[xtr + physical_width * i] = blue;
  }
}

u32 life[life_height * life_width];

void grab(u32* fb) { // fb-->life
  int W = life_width;
  int H = life_height;
  int S = life_scale;
  int off_y = (physical_height - S*H) / 2;
  int off_x = (physical_width - S*W) / 2;
  int tl = off_x + physical_width * off_y;
  // loop life array
  for (int y = 0; y < H; y++) {
    for (int x = 0; x < W; x++) {
      u32 col = fb[tl + (S*x) + (S*y) * physical_width];
      unsigned char r = (col & 0xff0000) >> 16;
      unsigned char g = (col & 0xff00) >> 8;
      unsigned char b = (col & 0xff);
      int brite = (r+g+b)/3;
      bool on = brite>127;
      life[x + y * W] = on;
    }
  }
}

void blit(u32* fb) { // life-->fb
  int W = life_width;
  int H = life_height;
  int S = life_scale;
  int off_y = (physical_height - S*H) / 2;
  int off_x = (physical_width - S*W) / 2;
  int tl = off_x + physical_width * off_y;
  // loop portal size
  for (int y = 0; y < (S*H); y++) {
    for (int x = 0; x < (S*W); x++) {
      int i = (x/S) + (y/S)*W;
      int j = tl + x + y*physical_width;
      fb[j] = life[i];
    }
  }
}

u32 neighbors[life_height * life_width] = {};

void step_gen(int g) { // life
  int W = life_width;
  int H = life_height;
  // loop life array --> neighbors
  for (int i = 0; i < H*W; i++) {
    int x = i % W;
    int y = i / W;
    u32 alive = life[i];
    if (alive) {
#define neighbor(xi,yi) neighbors [ (W+x+xi)%W + ((H+y+yi)%H) * W ]
      neighbor(-1,-1)++;
      neighbor( 0,-1)++;
      neighbor( 1,-1)++;
      neighbor(-1, 0)++;
      neighbor( 1, 0)++;
      neighbor(-1, 1)++;
      neighbor( 0, 1)++;
      neighbor( 1, 1)++;
    }
  }
  // loop neighbor --> life array
  for (int y = 0; y < H; y++) {
    for (int x = 0; x < W; x++) {
      u32 i = x + y*W;
      int n = neighbors[i];
      if ((n==3) || ((n==2) && life[i])) {
        life[i] = white;
      } else {
        life[i] = black;
      }
      neighbors[i] = 0;
    }
  }
}

typedef struct { int x; int y; } xy;
xy gliderDR[] = { {0,0}, {2,0}, {1,1}, {2,1}, {1,2}, {0,0} };
xy gliderDL[] = { {0,0}, {-2,0}, {-1,1}, {-2,1}, {-1,2}, {0,0} };

void place(xy loc, xy* elems) {
  int W = life_width;
  int H = life_height;
  for (int i = 0;; i++) {
    xy e = elems[i];
    if (i>0 && e.x == 0 && e.y == 0) break;
    life[(W+loc.x+e.x)%W + ((H+loc.y+e.y)%H) * W] = white;
  }
}

xy random_pos() {
  int x = rand() % life_width;
  int y = rand() % life_height;
  //printf("x=%d,y=%d\n",x,y);
  xy pos = {x,y};
  return pos;
}

void place_gliders(int n) {
  // place N random gliders
  int half1 = (n+1)/2;
  int half2 = n/2;
  for (int i = 0; i < half1; i++) place(random_pos(),gliderDR);
  for (int i = 0; i < half2; i++) place(random_pos(),gliderDL);
}

int population() {
  int res = 0;
  int W = life_width;
  int H = life_height;
  for (int y = 0; y < H; y++) {
    for (int x = 0; x < W; x++) {
      u32 i = x + y*W;
      if (life[i]) res++;
    }
  }
  return res;
}

const int prints_per_second = 1;

void print_stats_maybe() {
  static bool first = true;
  if (first) {
    first = false;
    printf("   n :   time :   gen# :  fps :   pop\n");
  }
  static u64 start = wallclock_time();
  u64 now = wallclock_time();
  static int frame = 0;
  static int prints = 0;
  frame++;
  const u64 million = 1000000;
  int frames_per_print = 500;
  bool do_print = (frame % frames_per_print == 0);
  if (do_print) {
    prints++;
    u32 fps = million * frames_per_print * prints / (u64)(now-start);
    float secs = (float)(now-start)/million;
    int pop = population();
    printf("%4d :%7.2f :%7d :%5d :%6d\n" , prints, secs, frame, fps, pop);
    fflush(stdout);
  }
}

int main(int argc, char* argv[]) {

  bool do_grab = false;
  int n_gliders = 0;
  for (int i = 1; i < argc; i++) {
    char* arg = argv[i];
    //printf("argv(%d) '%s'\n", i, arg);
    if (0 == strcmp(arg,"-grab")) do_grab = true;
    sscanf(arg,"%d",&n_gliders);
  }
  //printf("life: do_grab=%d, n_gliders=%d\n", do_grab, n_gliders);

  int fd = open("/dev/fb0", O_RDWR);
  u32 fbsize = physical_width * physical_height * sizeof(u32);
  u32* fb = (u32*)mmap(NULL,fbsize,PROT_WRITE,MAP_SHARED,fd,0);

  draw_boundary_rectangle(fb);
  if (do_grab) grab(fb);
  place_gliders(n_gliders);

  for (int g = 0;; g++) {
    print_stats_maybe();
    step_gen(g);
    blit(fb);
  }
}
