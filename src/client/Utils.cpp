#include "Utils.hpp"
#include <math.h>
#include <vector>

/* + Randomizing + */
sf::Color getRandomColour()
{
    return sf::Color(Random<unsigned int>(0, 255), Random<unsigned int>(0, 255), Random<unsigned int>(0, 255));
}

/* + Maths + */
float degToRad(float deg)
{
    return deg * M_PI/180;
}

float radToDeg(float rad)
{
    rad * 180/M_PI;
}

