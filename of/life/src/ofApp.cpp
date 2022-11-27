#include "ofApp.h"

#include <assert.h>
#include <stdio.h>

typedef unsigned char u8;
typedef unsigned int u32;
typedef int i32;

typedef struct { i32 x; i32 y; } xy;

const u32 life_size = life_height * life_width;

u8 world[life_size] = {};

void ofApp::draw() {
  //printf("step: %d\n", step);
  for (u32 y = 0; y < life_height; y++) {
    for (u32 x = 0; x < life_width; x++) {
      u32 cell = y * life_width + x;
      bool alive = world[cell];
      ofSetColor(alive?255:0);
      ofDrawRectangle({life_scale*x+1,life_scale*y+1},life_scale-1,life_scale-1);
    }
  }
}

void step_world() {
  u8 neighbors[life_size] = {};
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

void clear_world() {
  for (u32 y = 0; y < life_height; y++) {
    for (u32 x = 0; x < life_width; x++) {
      u32 cell = y * life_width + x;
      world[cell] = 0;
    }
  }
}

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


void ofApp::setup() {
  ofBackground(0,0,255); //blue

  //place({10,10},gosperGun);

}

void ofApp::update() {
  step++;;
  if (running) step_world();

  if (step==1) {
    place_string({10, 5},"In 1970");
    place_string({10,22},"John Conway");
    place_string({10,39},"devised");
    place_string({10,56},"the Game of Life");
  }
  if (step == 300) running = true;

  if (step == 700) {
    running = false;
    clear_world();
    place_string({10, 5},"A cellular automaton");
    place_string({10,22},"with simple rules");
    place_string({10,39},"but interesting");
    place_string({10,56},"emergent behaviour");
  }
  if (step == 1000) running = true;

  if (step == 1400) {
    running = false;
    clear_world();
    place_string({10, 5},"Depending on the");
    place_string({10,22},"initial state");
  }
  if (step == 1500) {
    place_string({10,39},"For example");
  }
  if (step == 1600) {
    place_string({10,56},"The Gosper Gun");
  }
  if (step == 1700) {
    clear_world();
    place({10,10},gosperGun);
    running = true;
  }
}
