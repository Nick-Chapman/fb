#include "ofMain.h"
#include "ofApp.h"

int main( ){
  int width = life_width * life_scale + 1;
  int height = life_height * life_scale + 1;
  ofSetupOpenGL(width,height,OF_WINDOW);
  ofRunApp(new ofApp());
}
