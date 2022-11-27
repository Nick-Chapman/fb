
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

const u32 world_width = 1920; //1024;
const u32 world_height = 1080; //800;

const u32 border = 0; //50

const u32 virtual_width = world_width + border;
const u32 virtual_height = world_height + border;

const u32 life_scale = 5;

static u64 wallclock_time() { //in micro-seconds
  struct timeval tv;
  gettimeofday(&tv,NULL);
  return tv.tv_sec*(u64)1000000+tv.tv_usec;
}

void blit(void);

static void init_life(void);
void prepare_life(void);
void step_world(void);

const u32 million = 1000000;

void print_stats_maybe(u64 runtime) {
  const u32 prints_per_second = 1;
  static u32 frame = 0;
  static u32 prints = 0;
  static u32 frames_since_last_print = 0;
  frame++;
  frames_since_last_print++;
  bool do_print = runtime >= million*(prints+1) / prints_per_second;
  if (do_print) {
    u32 fps = frames_since_last_print * prints_per_second;
    prints++;
    frames_since_last_print = 0;
    float secs = (float)runtime/million;
    printf("%4d :%7.2f :%7d :%5d\n" , prints, secs, frame, fps);
    fflush(stdout);
  }
}

int main() {
  assert(sizeof(u8) == 1);
  assert(sizeof(u32) == 4);
  assert(sizeof(u64) == 8);
  init_life();
  u64 start_time = wallclock_time();
  for (;;) {
    u64 now = wallclock_time();
    u64 runtime = now - start_time;
    print_stats_maybe(runtime);
    prepare_life();
    blit();
    if (runtime > 10 * million) {
      step_world();
    }
  };
  return 0;
}

static u32 screen[virtual_height * virtual_width] = {};

void blit() {
  FILE* fp = fopen("/dev/fb0","w");
  fwrite(&screen,sizeof(u32),physical_width*physical_height,fp);
  fclose(fp);
}

void blitV() {
  FILE* fp = fopen("/dev/fb0","w");
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

// lowercase....

const char* alpha_a[] =
  {"                                                 ii    jj                        ",
   "       bb             dd       fff         hh    ii    jj    kk    ll            ",
   "       bb             dd       fff         hh                kk    ll            ",
   " aaaaa bbbbb ccccc ddddd eeeee ff    ggggg hhhhh ii    jj    kk kk ll    mmmmmmmm",
   " aaaaa bbbbb ccccc ddddd eeeee ff    ggggg hhhhh ii    jj    kk kk ll    mmmmmmmm",
   "    aa bb bb cc cc dd dd ee ee fff   gg gg hh hh ii    jj    kk kk ll    mm mm mm",
   " aaaaa bb bb cc    dd dd eeeee fff   gg gg hh hh ii    jj    kkkk  ll    mm mm mm",
   " aaaaa bb bb cc    dd dd eeeee ff    gg gg hh hh ii    jj    kkkk  ll    mm mm mm",
   " aa aa bb bb cc cc dd dd ee    ff    ggggg hh hh ii    jj    kk kk ll    mm mm mm",
   " aaaaa bbbbb ccccc ddddd eeeee ff    ggggg hh hh ii    jj    kk kk ll    mm mm mm",
   " aaaaa bbbbb ccccc ddddd eeeee ff       gg hh hh ii    jj    kk kk ll    mm mm mm",
   "                                     ggggg            jjj                        ",
   "                                     ggggg            jjj                        "};

const char* alpha_n[] =
  {"                                                               ",
   "                                     tt                        ",
   "                                     tt                        ",
   " nnnnn ooooo ppppp qqqqq rrr   sssss ttt   uu uu vv vv ww ww ww",
   " nnnnn ooooo ppppp qqqqq rrr   sssss ttt   uu uu vv vv ww ww ww",
   " nn nn oo oo pp pp qq qq rr    ss    tt    uu uu vv vv ww ww ww",
   " nn nn oo oo pp pp qq qq rr    sssss tt    uu uu vv vv ww ww ww",
   " nn nn oo oo pp pp qq qq rr    sssss tt    uu uu vv vv ww ww ww",
   " nn nn oo oo pp pp qq qq rr       ss tt    uu uu vvvvv ww ww ww",
   " nn nn ooooo ppppp qqqqq rr    sssss ttt   uuuuu  vvv  wwwwwwww",
   " nn nn ooooo ppppp qqqqq rr    sssss ttt   uuuuu  vvv  wwwwwwww",
   "             pp       qq                                       ",
   "             pp       qq                                       "};

const char* alpha_x[] =
  {"                  ",
   "                  ",
   "                  ",
   " xx xx yy yy zzzzz",
   " xx xx yy yy zzzzz",
   " xx xx yy yy    zz",
   "  xxx  yy yy zzzzz",
   "  xxx  yy yy zzzzz",
   " xx xx yyyyy zz   ",
   " xx xx yyyyy zzzzz",
   " xx xx    yy zzzzz",
   "       yyyyy      ",
   "       yyyyy      "};

// uppercase....

const char* alpha_A[] =
  {"                                                                                 ",
   " AAAAA BBBBB CCCCC DDDD  EEEE  FFFF  GGGGG HH HH II      JJJ KK KK LL    MMMMMMMM",
   " AAAAA BBBBB CCCCC DDDDD EEEE  FFFF  GGGGG HH HH II      JJJ KK KK LL    MMMMMMMM",
   " AA AA BB BB CC CC DD DD EE    FF    GG GG HH HH II       JJ KK KK LL    MM MM MM",
   " AA AA BB BB CC    DD DD EE    FF    GG    HH HH II       JJ KK KK LL    MM MM MM",
   " AAAAA BBBB  CC    DD DD EEEE  FFFF  GG GG HHHHH II       JJ KKKK  LL    MM MM MM",
   " AAAAA BBBB  CC    DD DD EEEE  FFFF  GG GG HHHHH II       JJ KKKK  LL    MM MM MM",
   " AA AA BB BB CC    DD DD EE    FF    GG GG HH HH II       JJ KK KK LL    MM MM MM",
   " AA AA BB BB CC CC DD DD EE    FF    GG GG HH HH II    JJ JJ KK KK LL    MM MM MM",
   " AA AA BBBBB CCCCC DDDDD EEEE  FF    GGGGG HH HH II    JJJJJ KK KK LLL   MM MM MM",
   " AA AA BBBBB CCCCC DDDD  EEEE  FF    GGGGG HH HH II    JJJJJ KK KK LLL   MM MM MM",
   "                                                                                 ",
   "                                                                                 "};

const char* alpha_N[] =
  {"                                                               ",
   " NNNNN OOOOO PPPPP QQQQQ RRRRR SSSSS TTTT  UU UU VV VV WW WW WW",
   " NNNNN OOOOO PPPPP QQQQQ RRRRR SSSSS TTTT  UU UU VV VV WW WW WW",
   " NN NN OO OO PP PP QQ QQ RR RR SS SS  TT   UU UU VV VV WW WW WW",
   " NN NN OO OO PP PP QQ QQ RR RR SS     TT   UU UU VV VV WW WW WW",
   " NN NN OO OO PPPPP QQ QQ RRRR  SSSSS  TT   UU UU VV VV WW WW WW",
   " NN NN OO OO PPPPP QQ QQ RRRR  SSSSS  TT   UU UU VV VV WW WW WW",
   " NN NN OO OO PP    QQ QQ RR RR    SS  TT   UU UU VV VV WW WW WW",
   " NN NN OO OO PP    QQ QQ RR RR SS SS  TT   UU UU VVVVV WW WW WW",
   " NN NN OOOOO PP    QQQQQ RR RR SSSSS  TT   UUUUU  VVV  WWWWWWWW",
   " NN NN OOOOO PP    QQQQQ RR RR SSSSS  TT   UUUUU  VVV  WWWWWWWW",
   "                      QQ                                       ",
   "                      QQ                                       "};

const char* alpha_X[] =
  {"                  ",
   " XX XX YY YY ZZZZZ",
   " XX XX YY YY ZZZZZ",
   " XX XX YY YY    ZZ",
   " XX XX YY YY    ZZ",
   "  XXX  YY YY  ZZZ ",
   "  XXX  YYYYY  ZZZ ",
   " XX XX YYYYY ZZ   ",
   " XX XX    YY ZZ   ",
   " XX XX YYYYY ZZZZZ",
   " XX XX YYYYY ZZZZZ",
   "                  ",
   "                  "};

const char* alpha_0[] =
  {"                                                            ",
   " 00000 1111  22222 33333 44 44 55555 66666 77777 88888 99999",
   " 00000 1111  22222 33333 44 44 55555 66666 77777 88888 99999",
   " 00 00   11  22 22 33 33 44 44 55    66 66 77 77 88 88 99 99",
   " 00 00   11     22    33 44 44 55    66       77 88 88 99 99",
   " 00 00   11  22222  3333 44444 55555 66666    77 88888 99999",
   " 00 00   11  22222  3333 44444 55555 66666    77 88888 99999",
   " 00 00   11  22       33    44    55 66 66    77 88 88    99",
   " 00 00   11  22    33 33    44 55 55 66 66    77 88 88 99 99",
   " 00000 11111 22222 33333    44 55555 66666    77 88888 99999",
   " 00000 11111 22222 33333    44 55555 66666    77 88888 99999",
   "                                                            ",
   "                                                            "};

u32 char_pixel_width(char c) { // including 1 pixel gap (for all but j descender)
  assert((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'));
  //                a b c d e f g h i j k l m n o p q r s t u v w x y z
  static u32 x[] = {6,6,6,6,6,4,6,6,3,3,6,3,9,6,6,6,6,4,6,4,6,6,9,6,6,6};
  static u32 X[] = {6,6,6,6,5,5,6,6,3,6,6,4,9,6,6,6,6,6,6,5,6,6,9,6,6,6};
  if (c>='a') {
    return x[c-'a'];
  } else if (c>='A') {
    return X[c-'A'];
  } else {
    return 6;
  }
}

void place_font_element(xy loc, char c, const char** font, u32 offset) {
  const u32 char_height = 13;
  const u32 width = char_pixel_width(c);
  const u32 char_description_width = 6;
  for (u32 j = 0; j < char_height; j++) {
    const char* line = font[j];
    for (u32 i = 0; i < width; i++) {
      char desc = line[i + offset * char_description_width] ;
      bool check = (desc == c) || (desc == ' ');
      if(!check) {
        printf("check failed: '%c' -> '%c'\n",c,desc);
        printf("i=%d, j=%d, offset=%d\n",i,j,offset);
        printf("line=%s\n",line);
      }
      assert(check);
      bool on = (desc == c);
      if (on) {
        u32 x = (loc.x + i) % life_width;
        u32 y = (loc.y + j) % life_height;
        u32 cell = y * life_width + x;
        world[cell] = 1;
      }
    }
  }
}

void place_char(xy loc, char c) {
  assert((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'));
  if (c >= 'x') {
    place_font_element(loc,c,alpha_x,c-'x');
  }
  else if (c >= 'n') {
    place_font_element(loc,c,alpha_n,c-'n');
  }
  else if (c >= 'a') {
    place_font_element(loc,c,alpha_a,c-'a');
  }
  else if (c >= 'X') {
    place_font_element(loc,c,alpha_X,c-'X');
  }
  else if (c >= 'N') {
    place_font_element(loc,c,alpha_N,c-'N');
  }
  else if (c >= 'A') {
    place_font_element(loc,c,alpha_A,c-'A');
  }
  else {
    place_font_element(loc,c,alpha_0,c-'0');
  }
}

xy addXY(xy a, xy b) {
  return { a.x + b.x, a.y + b.y };
}

void place_string(xy loc, const char* s) {
  while (char c = *s++) {
    if (c == ' ') {
      loc = addXY(loc,{4,0});
    } else {
      place_char(loc,c);
      loc = addXY(loc,{(i32)char_pixel_width(c),0});
    }
  }
}

void init_life() {
  //place({5,7},gliderDR);
  //place({23,15},gliderDR);
  //place({10,40},gliderDL);
  //place({10,10},gosperGun);
  place_string({50, 50},"In 1970 the British mathematician John Horton Conway");
  place_string({50, 75},"        devised the Game of Life");
  place_string({50,100},"A cellular automaton with simple rules");
  place_string({50,125},"Yet interesting emergent behaviour");
  //place_string({50,150},"AaBbcDdEeFfGgHhIiJjKkLlMm");
  //place_string({50,170},"NnOoPpQqRrSsTtUuVvWwXxYyZz");
  //place_string({50,190},"0123456789");
}
