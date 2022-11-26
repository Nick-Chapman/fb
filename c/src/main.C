
#include <assert.h>
#include <stdio.h>
#include <sys/time.h> // gettimeofday()
#include <unistd.h> // sleep()

typedef unsigned char u8;
typedef unsigned int u32;
typedef unsigned long u64;

typedef int i32;

const u32 tick_prints_per_second = 3;
const u32 million = 1000000;
const u32 tick_print_duration_us = million / tick_prints_per_second;

static u64 wallclock_time() { //in micro-seconds
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return tv.tv_sec*(u64)1000000+tv.tv_usec;
}

static void blit(void);

static void init_life(void);
static void prepare_life(void);
static void step_world(void);

int main() {
  assert(sizeof(u8) == 1);
  assert(sizeof(u32) == 4);
  assert(sizeof(u64) == 8);
  u64 toc = wallclock_time();
  u32 tick = 0; //count frames since last print
  init_life();
  for (u32 frame = 0;; frame++) {
    u64 tic = wallclock_time();
    if (tic - toc >= tick_print_duration_us) {
      u32 fps = tick * tick_prints_per_second;
      printf("frame=%d, fps=%d\n", frame, fps); fflush(stdout);
      toc = tic;
      tick = 0;
    }
    tick++;
    prepare_life();
    blit();
    step_world();
  };
  return 0;
}

const u32 physical_width = 1920;
const u32 physical_height = 1080;
const u32 physical_size = physical_height * physical_width;

static u32 screen[physical_size] = {};

static void blit() {
  FILE* fp = fopen("/dev/fb0","w");
  fwrite(&screen,sizeof(u32),physical_size,fp);
  fclose(fp);
}

const u32 black = 0x00000000;
const u32 white = 0x00ffffff;
const u32 blue  = 0x000000ff;
const u32 grey  = 0x007f7f7f;

const u32 life_scale = 16;

const u32 life_width = 1024/life_scale;
const u32 life_height = 800/life_scale;
const u32 life_size = life_height * life_width;

const u32 life_offset_x = (physical_width - (life_width * life_scale)) /2;
const u32 life_offset_y = (physical_height - (life_height * life_scale)) /2;

u8 world[life_size] = {};

void prepare_life() {
  for (u32 y = 0; y < physical_height; y++) {
    for (u32 x = 0; x < physical_width; x++) {
      u32 el = y * physical_width + x;
      bool borderL = x < life_offset_x;
      bool borderR = x >= (life_offset_x + life_width * life_scale);
      bool borderU = y < life_offset_y;
      bool borderD = y >= (life_offset_y + life_height * life_scale);
      bool border = borderL || borderR || borderU || borderD;
      if (border) {
        screen[el] = grey;
      } else {
        u32 yy = (y - life_offset_y) / life_scale;
        u32 xx = (x - life_offset_x) / life_scale;
        u32 cell = yy * life_width + xx;
        bool alive = world[cell];
        u32 col = alive ? white : black;
        screen[el] = col;
      }
    }
  }
}

u8 neighbors[life_size] = {};

void step_world() {
  for (u32 y = 0; y < life_height; y++) {
    for (u32 x = 0; x < life_width; x++) {
      u32 cell = y * life_width + x;
      bool alive = world[cell];
      if (alive) {
        #define neighbor(x,y) \
          (neighbors[(life_size + cell + (life_width*y) + x) % life_size])
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
  }
  for (u32 y = 0; y < life_height; y++) {
    for (u32 x = 0; x < life_width; x++) {
      u32 cell = y * life_width + x;
      u32 n = neighbors[cell];
      if ((n==3) || ((n==2) && world[cell])) {
        world[cell] = 1;
      } else {
        world[cell] = 0;
      }
      neighbors[cell] = 0;
    }
  }
}

typedef struct { i32 x; i32 y; } xy;
xy gliderDR[] = { {0,0}, {2,0}, {1,1}, {2,1}, {1,2}, {0,0} };
xy gliderDL[] = { {0,0}, {-2,0}, {-1,1}, {-2,1}, {-1,2}, {0,0} };

void place(xy loc, xy* elems) {
  u32 cell = loc.y * life_width + loc.x;
  for (u32 i = 0;; i++) {
    xy e = elems[i];
    if (i>0 && e.x == 0 && e.y == 0) break;
    world[(life_size + cell + (life_width * e.y) + e.x) % life_size] = 1;
  }
}

void init_life() {
  place({5,7},gliderDR);
  place({23,15},gliderDR);
  place({10,40},gliderDL);
}
