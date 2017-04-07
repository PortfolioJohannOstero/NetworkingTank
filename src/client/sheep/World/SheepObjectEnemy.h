#ifndef SHEEP_OBJECTENEMY_H
#define SHEEP_OBJECTENEMY_H

/* +================================+
 *  Name: ObjectEnemey
 *  Author: Johann Ostero (P4220933)
 *  Type: Object
 *  --
 *  This is a subscriber to the Object class.
 *  This class is purely for representing the other networking players.
 *  It has not controls, and is purely controlled by the World class.
 *
 *  It has one additional feature, which is rendering a label of the objects name above itself
 * +================================+
 */

#include "SheepObject.h"

namespace Sheep
{
    class ObjectEnemy : public Object
    {
    public:
        ObjectEnemy(float speed, const std::string& objectName = "Object", int spriteId = 0, Tag objectTag = Tag::ENEMY, int fontId = 0, float labelOffset = 45.0f);
        virtual ~ObjectEnemy();

        void Update() override final;
        void Render() override;

    private:
        float mSpeed;
        sf::Vector2f mPrevPos;

        sf::Text mNameLabel;
        float mLabelOffset;

    };
}

#endif // SHEEP_OBJECTENEMY_H
