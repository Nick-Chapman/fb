
#include "ofMain.h"

const int life_width = 100;
const int life_height = 88;
const int life_scale = 8;

class ofApp : public ofBaseApp {
public:
  void setup();
  void update();
  void draw();
  int step;
};
