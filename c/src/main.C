
#include <assert.h>
#include <stdio.h>
#include <sys/time.h> // gettimeofday()
#include <unistd.h> // sleep()

typedef unsigned char u8;
typedef unsigned int u32;
typedef unsigned long u64;

typedef int i32;

const u32 physical_width = 1920;
const u32 physical_height = 1080;

const u32 world_width = 1024;
const u32 world_height = 800;

const u32 border = 50;

const u32 virtual_width = world_width + border;
const u32 virtual_height = world_height + border;

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
  const u32 prints_per_second = 3;
  const u32 million = 1000000;
  const u32 tick_print_duration_us = million / prints_per_second;
  u64 toc = wallclock_time();
  u32 prints = 0; //count of prints
  u32 tick = 0; //count of frames since last print
  init_life();
  for (u32 frame = 0;; frame++) {
    u64 tic = wallclock_time();
    if (tic - toc >= tick_print_duration_us) {
      u32 fps = tick * prints_per_second;
      float secs = (float)prints/prints_per_second;
      printf("time=%.2f, frame=%d, fps=%d\n", secs, frame, fps); fflush(stdout);
      toc = tic;
      tick = 0;
      prints++;
    }
    tick++;
    prepare_life(); //most of the time here
    blit();
    step_world();
  };
  return 0;
}

static u32 screen[virtual_height * virtual_width] = {};

static void blit() {
  FILE* fp = fopen("/dev/fb0","w");
  //fwrite(&screen,sizeof(u32),physical_size,fp);
  const u32 off_v = (physical_height - virtual_height) / 2;
  const u32 off_h = (physical_width - virtual_width) / 2;
  for (u32 y = 0; y < virtual_height; y++) {
    const u32 off = (((off_v + y) * physical_width) + off_h) * sizeof(u32);
    fseek(fp,off,2);
    fwrite(&screen[y*virtual_width],sizeof(u32),virtual_width,fp);
  }
  fclose(fp);
}

const u32 black = 0x00000000;
const u32 white = 0x00ffffff;
const u32 blue  = 0x000000ff;
const u32 grey  = 0x007f7f7f;

const u32 life_scale = 8;

const u32 life_width = world_width/life_scale;
const u32 life_height = world_height/life_scale;
const u32 life_size = life_height * life_width;

const u32 life_offset_x = (virtual_width - (life_width * life_scale)) /2;
const u32 life_offset_y = (virtual_height - (life_height * life_scale)) /2;

u8 world[life_size] = {};

void prepare_life() {
  for (u32 y = 0; y < virtual_height; y++) {
    for (u32 x = 0; x < virtual_width; x++) {
      u32 el = y * virtual_width + x;
      bool borderL = x < life_offset_x;
      bool borderR = x >= (life_offset_x + life_width * life_scale);
      bool borderU = y < life_offset_y;
      bool borderD = y >= (life_offset_y + life_height * life_scale);
      bool border = borderL || borderR || borderU || borderD;
      if (border) {
        screen[el] = blue;
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

xy gosperGun[] = {{5,1},{5,2},{6,1},{6,2},{5,11},{6,11},{7,11},{4,12},{3,13},{3,14},{8,12},{9,13},{9,14},{6,15},{4,16},{5,17},{6,17},{7,17},{6,18},{8,16},{3,21},{4,21},{5,21},{3,22},{4,22},{5,22},{2,23},{6,23},{1,25},{2,25},{6,25},{7,25},{3,35},{4,35},{3,36},{4,36},{0,0}};

void place(xy loc, xy* elems) {
  u32 cell = loc.y * life_width + loc.x;
  for (u32 i = 0;; i++) {
    xy e = elems[i];
    if (i>0 && e.x == 0 && e.y == 0) break;
    world[(life_size + cell + (life_width * e.y) + e.x) % life_size] = 1;
  }
}

void init_life() {
  //place({5,7},gliderDR);
  //place({23,15},gliderDR);
  //place({10,40},gliderDL);
  place({10,10},gosperGun);
}
