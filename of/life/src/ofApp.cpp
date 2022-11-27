#include "ofApp.h"

#include <assert.h>
#include <stdio.h>

typedef unsigned char u8;
typedef unsigned int u32;
typedef int i32;

const u32 life_size = life_height * life_width;

u8 world[life_size] = {};

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

typedef struct { i32 x; i32 y; } xy;

xy gosperGun[] = {{5,1},{5,2},{6,1},{6,2},{5,11},{6,11},{7,11},{4,12},{3,13},{3,14},{8,12},{9,13},{9,14},{6,15},{4,16},{5,17},{6,17},{7,17},{6,18},{8,16},{3,21},{4,21},{5,21},{3,22},{4,22},{5,22},{2,23},{6,23},{1,25},{2,25},{6,25},{7,25},{3,35},{4,35},{3,36},{4,36},{0,0}};

void place(xy loc, xy* elems) {
  u32 cell = loc.y * life_width + loc.x;
  for (u32 i = 0;; i++) {
    xy e = elems[i];
    if (i>0 && e.x == 0 && e.y == 0) break;
    world[(life_size + cell + (life_width * e.y) + e.x) % life_size] = 1;
  }
}

void draw_life() {
  for (u32 y = 0; y < life_height; y++) {
    for (u32 x = 0; x < life_width; x++) {
      u32 cell = y * life_width + x;
      bool alive = world[cell];
      ofSetColor(alive?255:0);
      ofDrawRectangle({life_scale*x+1,life_scale*y+1},life_scale-1,life_scale-1);
    }
  }
}

void ofApp::setup(){
  ofBackground(0,0,255); //blue
  place({10,10},gosperGun);
}

void ofApp::update(){
  step++;;
  step_world();
}

void ofApp::draw(){
  //printf("step: %d\n", step);
  draw_life();
}
