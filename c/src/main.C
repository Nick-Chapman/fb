
#include <assert.h>
#include <stdio.h>
#include <sys/time.h> // gettimeofday()
#include <unistd.h> // sleep()

typedef unsigned char u8;
typedef unsigned int u32;
typedef unsigned long u64;

const u32 tick_prints_per_second = 3;
const u32 million = 1000000;
const u32 tick_print_duration_us = million / tick_prints_per_second;

u64 wallclock_time(void);

void blit(void);
void prepare_blue(void);
void prepare_red_green_gradient(void);

void init_life(void);
void prepare_life(void);
void step_world(void);

void log(const char* s) {
  if (0) {
    printf("log:%s\n",s);
    fflush(stdout);
  }
}

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

u64 wallclock_time() { //in micro-seconds
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return tv.tv_sec*(u64)1000000+tv.tv_usec;
}

const u32 physical_width = 1920;
const u32 physical_height = 1080;
const u32 physical_size = physical_height * physical_width;

u32 screen[physical_size] = {};

void blit() {
  log("blit");
  FILE* fp = fopen("/dev/fb0","w");
  fwrite(&screen,sizeof(u32),physical_size,fp);
  fclose(fp);
}

const u32 black = 0x00000000;
const u32 white = 0x00ffffff;
const u32 blue  = 0x000000ff;
const u32 grey  = 0x007f7f7f;

void prepare_blue() {
  log("prepare_blue");
  for (u32 y = 0; y < physical_height; y++) {
    for (u32 x = 0; x < physical_width; x++) {
      u32 el = y * physical_width + x;
      screen[el] = blue;
    }
  }
}

void prepare_red_green_gradient() {
  log("prepare_red_green_gradient");
  for (u32 y = 0; y < physical_height; y++) {
    for (u32 x = 0; x < physical_width; x++) {
      u32 el = y * physical_width + x;
      u8 r = 256*x/physical_width;
      u8 g = 256*y/physical_height;
      u8 b = 0;
      u32 col = (r<<16) | (g<<8) | b;
      screen[el] = col;
    }
  }
}

const u32 life_scale = 16;

const u32 life_width = 1024/life_scale;
const u32 life_height = 800/life_scale;
const u32 life_size = life_height * life_width;

const u32 life_offset_x = (physical_width - (life_width * life_scale)) /2;
const u32 life_offset_y = (physical_height - (life_height * life_scale)) /2;

u8 world[life_size] = {};

void prepare_life() {
  log("prepare_life");
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

void glider_DR(u32 x, u32 y) {
  log("glider_DR");
  u32 i = x * life_width + y;
  u32 w = life_width;
  u32 ww = 2*life_width;
  world[i] = 1;
  world[i+2] = 1;
  world[i+w+1] = 1;
  world[i+w+2] = 1;
  world[i+ww+1] = 1;
}

void glider_DL(u32 x, u32 y) {
  log("glider_DL");
  u32 i = x * life_width + y;
  u32 w = life_width;
  u32 ww = 2*life_width;
  world[i] = 1;
  world[i-2] = 1;
  world[i+w-1] = 1;
  world[i+w-2] = 1;
  world[i+ww-1] = 1;
}

void zero_world() {
  log("zero_world");
  for (u32 i = 0; i < life_size; i++) {
    world[i] = 0;
  }
}

void chess_world() {
  log("chess_world");
  for (u32 y = 0; y < life_height; y++) {
    for (u32 x = 0; x < life_width; x++) {
      u32 cell = y * life_width + x;
      bool alive = (x+y)%2;
      world[cell] = alive;
    }
  }
}

void init_life() {
  glider_DR(7,5);
  glider_DR(15,23);
  glider_DL(40,10);
}

u8 neighbors[life_size] = {};

void step_world() {
  log("step_world");
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
