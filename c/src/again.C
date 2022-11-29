
#include <stdio.h>
#include <sys/mman.h>
#include <fcntl.h>

typedef unsigned int u32;

const int physical_width = 1920;
const int physical_height = 1080;

const int portal_width = 900;
const int portal_height = 600;
const int life_scale = 2;

const int life_width = portal_width / life_scale;
const int life_height = portal_height / life_scale;

const u32 red = 0x00ff0000;
const u32 green = 0x0000ff00;
const u32 blue = 0x000000ff;

void draw_boundary_rectangle(u32* fb) {

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


/*void old_slide(u32* fb) { //slow
  int width = life_scale * life_width;
  int height = life_scale * life_height;
  int off_y = (physical_height - height) / 2;
  int off_x = (physical_width - width) / 2;
  int tl = off_x + physical_width * off_y;
  for (int y = 0; y < height; y++) {
    u32* line = fb + tl + y * physical_width;
    u32 save = line[0];
    for (int x = 1; x < width; x++) {
      line[x-1] = line[x];
    }
    line[width-1] = save;
  }
}*/

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
      life[x + y * W] = fb[tl + (S*x) + (S*y) * physical_width];
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

void slide() { // life
  int W = life_width;
  int H = life_height;
  // loop life array
  for (int y = 0; y < H; y++) {
    u32* line = life + y * W;
    u32 save = line[0];
    for (int x = 1; x < W; x++) {
      line[x-1] = line[x];
    }
    line[W-1] = save;
  }
}

int main(void) {
  int fd = open("/dev/fb0", O_RDWR);
  u32 fbsize = physical_width * physical_height * sizeof(u32);
  u32* fb = (u32*)mmap(NULL,fbsize,PROT_WRITE,MAP_SHARED,fd,0);
  draw_boundary_rectangle(fb);
  grab(fb);
  for (int i = 0;; i++) {
    slide();
    blit(fb);
    if (i % portal_width == 0) printf("i=%d\n",i);
  }
}
