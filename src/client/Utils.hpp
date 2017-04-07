#ifndef UTILS
#define UTILS

#include <ctime>
#include <SFML/Graphics.hpp>

/* +====== Interface =====+ */

/* + Randomizing + */
template <typename T> T Random(T min, T max);
sf::Color getRandomColour(); // Returns a Colour with random values ranging between 0 and 255

/* + Maths + */
float degToRad(float deg);
float radToDeg(float rad);




/* +====== Template Implementation =====+ */

/* + Randomizing + */
template <typename T>
T Random(T min, T max)
{
    return min + rand() % ((max + 1) - min);
}

#endif
