#ifndef SHEEP_OBJECTPROJECTILE_H
#define SHEEP_OBJECTPROJECTILE_H

/* +================================+
 *  Name: ObjectProjectile
 *  Author: Johann Ostero (P4220933)
 *  Type: Object
 *  --
 *  This is a subscriber to the Object class.
 *  This class is inactive at start, and will be reactivated from the World class.
 *  When activated, it will simply constantly travel in a given direction from a given position,
        until it either leaves the window or hits something with the provided collision tags, resulting in active = false again
 * +================================+
 */

#include "SheepObject.h"

namespace Sheep
{
    class ObjectProjectile : public Object
    {
    public:
        ObjectProjectile(float speed, const std::string& objectName = "Object", int spriteId = 0, Tag objectTag = Tag::PROJECTILE);
        virtual ~ObjectProjectile();

        void Update() override final;

        void OnCollisionEnter(Object* otherObject) override;

    private:
        float mSpeed = 0;
    };
}

#endif // SHEEP_OBJECTPROJECTILE_H
